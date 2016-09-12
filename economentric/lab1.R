library("dplyr")
library("ggplot2")
library("GGally")
library("psych")

Sys.setlocale("LC_ALL", "Ru_Ru")

d <- cars

glimpse(d)
head(d)
tail(d)
describe(d)
ncol(d)
nrow(d)
str(d)

mean(d$speed)

glimpse(d2)

qplot(data=d2, dist)
qplot(data=d2, xlab="Длина тормозного пути", ylab="Количество машин", main="Данные 1920х годов", dist)

qplot(data = d2, speed, dist)

model <- lm(data=d2, dist~speed)

model

beta_hat <- coef(model)

eps_hat <- residuals(model)
eps_hat

y <- d2$dist
y_hat <- fitted(model)
y

RSS <- deviance(model)
TSS <- sum( (y - mean(y))^2 )
ESS <- TSS - RSS

R2 <- ESS/TSS

R2_cor <- cor(y, y_hat)^2

X <- model.matrix(model)
X

# прогнозирование
nd <- data.frame(speed=c(40,60))
nd

predict(model, nd)

qplot(data=d2, speed, dist, xlab="Длина тормозного пути", ylab="Количество машин", main="Данные 1920х годов") +
  stat_smooth(method = "lm")


# -------

t <- swiss
help(swiss)

glimpse(t)
describe(t)
ggpairs(t)

model2 <- lm(data=t, Fertility~Agriculture+Education+Catholic)
model2

coef(model2)
fitted(model2)
residuals(model2)
deviance(model2)
report <- summary(model2)
report$r.squared
cor(t$Fertility, fitted(model2))^2

# mtcars

data(mtcars)
models <- c(
  summary(lm(data=mtcars, mpg~disp+hp+wt))$r.squared,
  summary(lm(data=mtcars, mpg~cyl+hp+wt))$r.squared,
  summary(lm(data=mtcars, mpg~disp+cyl+wt))$r.squared,
  summary(lm(data=mtcars, mpg~disp+hp+cyl))$r.squared
)

models

cor(mtcars$mpg, fitted(model3))^2
