# создание функций в R

f <- function(x) {
  res <- x^2
  return(res)
}

# функция с необязательным параметром

fs <- function(x, stepen=2) {
  res <- x^stepen
  return(res)
}

fs(2)
fs(2, stepen = 5)

# посчитаем процент данных с пропущенными наблюдениями

d <- cars
d[1,2] = NA
d[3,1] = NA
d

na_perc <- function(d) {
  if (!is.data.frame(d)) {
    stop("d should be a data.frame")
  }
  res <- sum(is.na(d))/nrow(d)/ncol(d)
  return(res)
}

na_perc(d)
#error
na_perc(c(1, 2))

# работа с циклами
for (i in 5:10) {
  k <- i^2
  cat("i=", i, "k=", k, '\n')
}

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

h <- read.table("flats_moskow.txt", header=TRUE)
head(h)
tail(h)

qplot(data=h, x=totsp, y=price)
# видно что разброс увеличивается, это говорит об условной гетероскедастичности

model <- lm(price~totsp, data=h)
summary(model)

# неверно!!! так как оценка ковариционной матрицы считается неправильно
coeftest(model)
confint(model)
vcov(model)

# добавляем данные, полученные из модели
h <- augment(model, h)
glimpse(h)

qplot(data=h, totsp, abs(.resid))

# сравним оценки обычной ковариационной матрици и устойчивых к гетерокседастичности
vcov(model)
vcovHC(model)
vcovHC(model, model="HC2")

# теперь можно протестировать и найти правильные доверительные интервалы для оценок коэффициентов
coeftest(model, vcov. = vcovHC(model))
coeftest(model)

conftable <- coeftest(model, vcov. = vcovHC(model))
ci <- data.frame(estimate=conftable[,1], se_hc=conftable[,2])
ci <- mutate(ci, left_ci=estimate - 1.96 * se_hc, right_ci = estimate + 1.96 * se_hc)
ci
confint(model)
# вывод: гетерокседастичность привела к тому, что доверительные интервалы расширились, оценки стали менее точными

# Тесты на гетерокседастичность
# Тест Уайта
bptest(model)
bptest(model, data=h, varformula = ~ totsp + I(totsp^2))

# Тест Голфельда-Квандта
gqtest(model, order.by = ~totsp, data = h, fraction = 0.2)


# ------

qplot(data=h, log(totsp), log(price))
model2 <- lm(data=h, log(price)~log(totsp))
gqtest(model2, order.by = ~totsp, data = h, fraction = 0.2)
