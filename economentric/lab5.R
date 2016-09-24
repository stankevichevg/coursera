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

