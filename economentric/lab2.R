library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# генерация случайных величин
# z_1, ..., z_100 ~ N(5, 9)
z <- rnorm(100, mean=5, sd=3)
qplot(z)

# построим ф-ию плотности
x <- seq(-10,15,0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x, y, geom = "line")

# P(Z < 3) = F(3)
pnorm(3, mean = 5, sd = 3)

# P(Z \in [4;9])
# P(Z < 9) - P(Z < 4)
pnorm(9, mean = 5, sd = 3) - pnorm(4, mean = 5, sd = 3)

# P(Z<a) = 0.7; a-? 
# Квантиль
qnorm(0.7, mean = 5, sd = 3)

# Кроме нормального распределения, есть еще shisq, t, f
qplot(rchisq(1000,df = 44))

# Множественная регрессия. Проверка гипотиз.

h <- swiss
glimpse(h)
help(swiss)

model <- lm(data = h, Fertility ~ Catholic + Agriculture + Examination)
summary(model)

coeftest(model)
confint(model)

# Проверка гипотизы b_Cath = b_Agri
model_aux <- lm(data = h, Fertility ~ Catholic + I(Catholic + Agriculture) + Examination)

summary(model_aux)

# Стандартизированные коэффициенты

h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st <- lm(data = h_st, Fertility ~ Catholic + Agriculture + Examination)
summary(model_st)

# Искуственный эксперимент
D <- matrix(nrow = 100, rnorm(100*41, mean = 0, sd = 1))
df <- data.frame(D)
glimpse(df)

model_fake <- lm(data=df, X1~.)
summary(model_fake)

# Получаем, что некоторые коэффициенты значимы, хотя генерировались случайно. 
# Нельзя выбирать коэффициенты при большом числе регрессоров!!! А что делать??

model2 <- lm(data = h, Fertility ~ Catholic + Agriculture)
help("mtable")
toLatex(mtable(model, model2))
compar_12 <- mtable(model, model2)
compar_12

# Сохранение результатов исследования
stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS", stuff)

# Загрузка данных
mylist <- readRDS("mydata.RDS")
summary(mylist$model)
