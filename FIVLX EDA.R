library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

#load data
FIVLX <- read.table("FIVLX.csv", 
                    header = TRUE, sep = ",")
MSCI_EAFE_Val <- readxl::read_excel("msci_eafe_val.xls",
                            skip = 6)
CAC40 <- read.csv("CAC40.csv", 
                    header = TRUE, sep = ",")

MSCI_EAFE_Val$Date <- as.Date(MSCI_EAFE_Val$Date, "%Y-%m-%d")
FIVLX$Date<- as.Date(FIVLX$Date, "%Y-%m-%d")
CAC40$Date<- as.Date(CAC40$Date, "%Y-%m-%d")
FIVLX %<>% select(Date, Close) %>% rename(FIVLX=Close)
CAC40 %<>% select(Date, Close) %>% rename(CAC40=Close)

#merge price data by date
data <- merge( FIVLX, MSCI_EAFE_Val, by = "Date")
data <- merge( data, CAC40, by = "Date")
data <- rename(data, MSCI_Val=`EAFE VALUE Standard (Large+Mid Cap) Value`)
data$CAC40 <- as.numeric(as.character(data$CAC40))#??????

#calculate log return
MSCI_Val_return <- diff(log(data$MSCI_Va), differences=1)
FIVLX_return <- diff(log(data$FIVLX), differences=1)
CAC40_return <- diff(log(data$CAC40), differences=1)
log_return <- data.frame(cbind(MSCI_Val_return,FIVLX_return, CAC40_return))
log_return <- cbind(data$Date[2:length(data$Date)],log_return)
log_return %<>% rename(Date=`data$Date[2:length(data$Date)]`)

#---------------------------------------------------
#visualization
ggplot(data)+
  geom_line(aes(x=Date, y=10000*(FIVLX/data$FIVLX[1])))+
  geom_line(aes(x=Date, y=10000*(MSCI_Val/data$MSCI_Val[1])))+
  geom_line(aes(x=Date, y=10000*(CAC40/data$CAC40[1])))
  
COR <- cor(data[-c(1,2)])
corrplot::corrplot(COR, type = "upper", order = "hclust", 
                   tl.col = "black", tl.srt = 45)

#---------------------------------------------------
#linear regression

fit1 <- lm(FIVLX_return~MSCI_Val_return+CAC40_return ,data = log_return)
summary(fit1)




# plot(FIVLX$Close, type = 'l', xlab = '', ylab = '')
# FIVLX_1year <- filter(FIVLX, Date>='2018-10-07')
# plot(FIVLX_1year$Date, FIVLX_1year$Close, type = 'l', xlab = '', ylab = '')

# a1 <- ts(data$Close)
# a2 <- SMA(a1,5)
# a3 <- ts(data$`EAFE VALUE Standard (Large+Mid Cap) Value`)
# a4 <- SMA(a3,5)
# plot(a2, type = 'l', color="red", xlab = '', ylab = '')
# par(new=TRUE)
# plot(a4, type = 'l', xlab = '', ylab = '')




#-----------------------------------------------------------------------------
data %<>% mutate(year=year(Date), month= month(Date))
rolling_mean <- rollify(mean, window = 5)
rolling_sd <- rollify(sd, window = 5)
data %<>% mutate(rollingz_FIVLX=(Close-rolling_mean(Close))/rolling_sd(Close))
data %<>% mutate(rollingz_msci=(`EAFE VALUE Standard (Large+Mid Cap) Value`-rolling_mean(`EAFE VALUE Standard (Large+Mid Cap) Value`))/rolling_sd(`EAFE VALUE Standard (Large+Mid Cap) Value`))
#moving z score
plot(data$rollingz_FIVLX[1078:1258], type = 'l', xlab="",ylab="",pch=0, col="red", ylim=c(-2,2))
par(new=TRUE)
plot(data$rollingz_msci[1078:1258], type= 'l', xlab=" ",ylab=" NAV (red) and benchmark (blue)",
     col="blue", ylim=c(-2,2))

ggplot(data)+geom_line(aes(x=1:length(rollingz_FIVLX), y=rollingz_FIVLX))

ggplot(data)+
  geom_line(aes(x=1:length(rollingz_FIVLX), y=rollingz_FIVLX),color="#CC79A7")+
  geom_line(aes(x=1:length(rollingz_FIVLX), y=rollingz_msci), color="#000000")

rollingz_FIVLX_180 <- data$rollingz_FIVLX[1079:1258]
rollingz_msci_180 <- data$rollingz_msci[1079:1258]
rollingz_180 <- data.frame(cbind(rollingz_FIVLX_180, rollingz_msci_180))
ggplot(rollingz_180)+
  geom_line(aes(x=1:180, y=rollingz_FIVLX_180),color="#CC79A7")+
  geom_line(aes(x=1:180, y=rollingz_msci_180), color="#000000")

rollingz_FIVLX_30 <- data$rollingz_FIVLX[1229:1258]
rollingz_msci_30 <- data$rollingz_msci[1229:1258]
rollingz_30 <- data.frame(cbind(rollingz_FIVLX_30, rollingz_msci_30))
ggplot(rollingz_30)+
  geom_line(aes(x=1:30, y=rollingz_FIVLX_30),color="#CC79A7")+
  geom_line(aes(x=1:30, y=rollingz_msci_30), color="#000000")



#monthly volitility
sigma <- data %>% group_by(year, month)  %>% 
  mutate(v_FIVLX=sd(Close), v_msci=sd(`EAFE VALUE Standard (Large+Mid Cap) Value`)) %>%
  select(year, month, v_FIVLX, v_msci) %>%
  distinct()

fit <- lm(formula= data$Close~ log(data$`EAFE VALUE Standard (Large+Mid Cap) Value`))
display(fit)




#----------------------------------
library(forecast)
index <- ts(data$MSCI_Val, frequency = 1)
plot(index)
acf(index)
index_diff <- diff(index, differences=1)
acf(index_diff)
pacf(index_diff)
fit1 <- Arima(index_diff,order = c(4,1,4))
plot(residuals(fit1))
acf(residuals(fit1))


auto.arima(index)
arima_index <- arima(index, order = c(4,1,2))
plot(residuals(arima_index))
nav <- ts(data$Close)
auto.arima(nav)
arima_nav <- arima(nav, order = c(0,1,1))
plot(residuals(arima_nav))



#-------------------------------------------------
index_return <- diff(log(data$MSCI_Va), differences=1)
nav_return <- diff(log(data$Adj.Close), differences=1)
index_return <- ts(index_return, frequency = 1)
nav_return <-ts(nav_return, frequency = 1)
hist(index_return)
hist(nav_return)
qqnorm(index_return)
ks.test(index_return) 
shapiro.test(index_return)
adf(index_return)
# plot(nav_return, type = "l")
# plot(index_return)
# 
# 
# ggplot()+
#   geom_line(aes(x=1:1258, y=10000*data$Close/8.59), color="gray")
#   

auto.arima(index_return)

acf(index_return)
pacf(index_return)

acf(nav_return)
pacf(nav_return)


fit2 <- arima(index_return, c(0,0,1))
auto.arima(nav_return)
arma(index_return, order = c(0,1))

#----------------------------------------------
index_return1 <- index_return[1:1008]
auto.arima(index_return1)
fit1 <- arima(index_return1, order = c(0,0,1))
#fcfgy=forecast()
y <- predict(fit1, n.ahead = 249)
pracma::Norm((nav_return[1009:1257]-y$pred), p = 2)

#------------------------------------------------
#test for weak stationarity
adf.test(nav_return)
adf.test(nav_return)

the_fit <- lm(nav_return~index_return)
plot(residuals(the_fit), type = "l")
qqnorm(residuals(the_fit))
car::qqPlot(residuals(the_fit),id=F)


#dynamic time warp
## Find the best match with the canonical recursion formula
library(dtw)
alignment<-dtw(nav_return, index_return,keep=TRUE)
## Display the warping curve, i.e. the alignment curve
plot(alignment,type="threeway")
plot(alignment$index1,alignment$index2,main="Warping function")


#----------------------------------------------------------
CAC40 <- readxl::read_excel("/Users/tsuyu/Downloads/fidelity/historyIndex.xls",
                                    skip = 6)



