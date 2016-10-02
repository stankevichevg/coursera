library("Ecdat")

library("lubridate") # работа с датами
library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("bstats")

library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции данными
library("broom") # еще манипуляции
library("ggplot2")

library("quantmod")
library("rusquant")
library("sophisthse")
library("Quandl")

# 11
d <- Griliches
model <- lm(data=d, lw80~age80+iq+school80+expr80)
vcov(model)
# 12
vcovHC(model, type = "HC3")
# 13
coeftest(model, vcov. = vcovHC(model, type = "HC3")) #  0.0044312
coeftest(model, vcov. = vcovHC(model, type = "HC5")) #  0.0044097
coeftest(model, vcov. = vcovHC(model, type = "HC4m")) # 0.0044409
coeftest(model, vcov. = vcovHC(model, type = "HC1")) #  0.0043926
# 14
bptest(model, data=d, varformula = ~ iq)
# 15
gqtest(model, order.by = ~iq, data = d, fraction = 0.2)
# 16
d <- Solow
model <- lm(data=d, q~k+A)
vcov(model)      # 9.204408e-05
vcovHAC(model)   # 8.759004e-05
# 17
model <- lm(data=d, q~k)
dwt(model)
# 18
bgtest(model, order = 3)
# 19
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="google")
plot(GOOG$GOOG.Close, main = "")
# ---
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="google")
plot(MSFT$MSFT.Close, main = "")
# ---
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "INTC",from="2010-01-01", to="2014-02-03",src="google")
plot(INTC$INTC.Close, main = "")
# ---
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL",from="2010-01-01", to="2014-02-03",src="google")
plot(AAPL$AAPL.Close, main = "")
# 20
getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="google")
google <- GOOG$GOOG.Close
head(google)
model <- lm(data = google, GOOG.Close~lag(GOOG.Close, 1)+lag(GOOG.Close, 2))
summary(model)
