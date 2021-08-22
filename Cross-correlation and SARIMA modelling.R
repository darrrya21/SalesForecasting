library(tseries)
library(readxl)
library(forecast)
library(Metrics)
library(xlsx)

df = read_excel('C://Users/Dasha/sales.xlsx', col_names = T)
head(df)

df = data.frame(df, row.names = 1)


units = ts(df$Units....Brand.X., start = c(2013, 1), frequency = 12)

# cross-correlation analysis

trp = ts(df$TRP..Brand.X., start=c(2013, 1), freq=12)


plot(ccf(x=c(trp), y=c(units)), main = '', xlab='Лаг', 
     ylab='Коэффициент корреляции', color='black')
ccf_1 = ccf(x=c(trp), y=c(units))
u = decompose(units, type='add')
v = decompose(trp, type = 'add')
ccf_2 = ccf(x=c(v$random), y=c(u$random), na.action = na.pass)
plot(ccf_2)

# SARIMA modelling
train = window(units, start=c(2013, 1), end=c(2015, 12))
test = window(units, start=c(2016, 1), end=c(2016, 8))
units_acf = Acf(c(train))
plot(Acf(diff(c(train), 1)))
units_pacf = Pacf(c(train))


length(train) + length(test)
a1 = auto.arima(train, seasonal = T, stepwise = F, parallel = T)

predictions = forecast(a1, h=8)
rmse(predicted=c(predictions$mean), actual=c(test))
mape(actual=c(test), predicted=c(predictions$mean))


plot(c(predictions$mean), c(test), xlim = c(45000, 140000), 
     ylim=c(45000, 140000))
abline(a = 0, b=1)
summary(a1)

predictions_2017 = forecast(a1, h=24)
plot(predictions_2017)

# write.xlsx(predictions_2017, '2017_pred.xlsx')
