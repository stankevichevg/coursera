library("devtools")
# install_github("dgrtwo/broom")
# install_github("cran/bstats")
# install.packages("rusquant", repos="http://R-Forge.R-project.org")
# install_github("bdemeshev/sophisthse")

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


x <- c("2012-04-15","2011-05-17")
# переводим строки в даты
y <- ymd(x)
y + days(20)
day(y)
month(y)
# подробная документация для lubridate
vignette("lubridate")

# создадим временной ряд
x <- rnorm(5)
y <- ymd("2014-01-01") + days(0:4)
ts <- zoo(x, order.by = y)
ts
help(lag)
lag(ts, n = -1)
diff(ts)

# создадим регулярный временной ряд
ts2 <- zooreg(x, start = as.yearqtr("2014-01"), freq=4)
ts3 <- zooreg(x, start = as.yearmon("2014-01"), freq=12)

# посмотрим на встроенные данные
data("Investment")
help("Investment")
start(Investment)
end(Investment)
# основная информация по ряду
time(Investment)
coredata(Investment)

# можно восстанавливать пропущенное значение
dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA
# линейная апроксимация
na.approx(dna)
# предыдущее значение
na.locf(dna)

# Загрузка данных из внешних источников
# finance.yuahoo.com
# finam.ru
# sophist.hse.ru
a <- sophisthse("POPNUM_Y")
# quandle.com
b <- Quandl("FRED/GNP")
# finance.google.com
Sys.setlocale("LC_TIME", "C")
getSymbols(Symbols = "AAPL", from="2010-01-01", to="2016-09-02", src="google")
head(AAPL)
tail(AAPL)
# finam.ru
getSymbols(Symbols = "GAZP", from="2011-01-02", to="2014-09-09", src="Finam")
chartSeries(GAZP)
autoplot(GAZP[,1:4], facets = NULL)

# построение робастных доверительных интервалов
data("Investment")
d <- as.zoo(Investment)
autoplot(d[,1:2], facets = NULL)

model <- lm(data=d, RealInv~RealInt+RealGNP)
summary(model)
confint(model)
# дополним данные результатами модели
d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data=d_aug, lag(.resid), .resid)
vcov(model)
vcovHAC <- vcovHAC(model)
conftable <- coeftest(model, vcov. = vcovHAC)
# постороим доверительные интервалы с учетом поправок
ci <- data.frame(estimate = conftable[,1], se_ac=conftable[,2])
ci <- mutate(ci, left_95 = estimate - 1.96 * se_ac, right_95 = estimate + 1.96 * se_ac)
ci
# Тест Дарбина-Вотсана
# H0: нет автокорреляции
# Ha: есть автокорреляция 1-го порядка
res <- dwt(model)
# статистика
res$dw
# p-value
res$p
# оценка параметра автокорреляции
res$r

# Тест Бройма-Годфри
# H0: нет автокорреляции
# Ha: автокорреляция k-ого порядка
res <- bgtest(model, order = 4)
res$statistic
# p-value
res$p.value
