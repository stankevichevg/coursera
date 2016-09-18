library("HSAUR")
library("dplyr")
library("psych")
library("lmtest")
library("glmnet")
library("ggplot2")
library("car")

h <- cars

qplot(data=h, speed, dist)

model <- lm(data=h, dist~speed)
summary(model)

h <- mutate(h, speed2 = speed^2, speed3 = speed^3)

model2 <- lm(data=h, dist~speed+speed2+speed3)
summary(model2)

# Получили, что вторая регрессия значима. И кажды коэффициен по отдельности значим!

# Посмотрим на коэффициенты вздутия дисперсии. 1/(1-R^2)
vif(model2)

x0 <- model.matrix(data=h, dist ~ 0 + speed + speed2 + speed3)
head(x0)

# Посмотрим на кореляцию объясняющих переменных
cor(x0)

nd <- data.frame(speed=10, speed2 = 100, speed3=1000)

# Посмотрим на предсказанные значения и доверительные интервалы
predict(model, newdata = nd, interval = "prediction")
predict(model2, newdata = nd, interval = "prediction")
# Обратим внимание, что доверительный интервал и предсказанное значение изменились не существенно

# Посмотрим доверительные интервалы при коэффициентах линейной модели
confint(model)
confint(model2)
# В случае присутствия линейной зависимости между объясняющими переменными 
# (мультиколинеарность), доверительный интервал для оценки коэффициента регрессора выше

# Ridge и LASSO регрессии
y <- h$dist
x0 <- model.matrix(data=h, dist ~ 0 + speed + speed2 + speed3)

# LASSO
lambdas <- seq(50, 0.1, length = 30)
m_lasso <- glmnet(x0, y, alpha = 1, lambda = lambdas)

# посмотрим на зависимость размера коэффициентов от величины штрафа
plot(m_lasso, xvar = "lambda", label=TRUE)
# доля объясненной дисперсии (ESS)
plot(m_lasso, xvar = "dev", label=TRUE)

# коэффициенты для регрессий LASSO при разных значениях lambda
coef(m_lasso, s=c(0.1,1))

# для оценки Ridge регрессии необходимо установить alpha=0
m_ridge <- glmnet(x0, y, alpha = 1, lambda = lambdas)
help(glmnet)
# для подбора значения регуляризирующего параметра lambda, исспользуем кросс-валидацию

cv <- cv.glmnet(x0, y, alpha = 1)
plot(cv)
# посмотрим на коэффициенты модели для такого максимального lambda, что 
# ошибка прогнозирования не превышает стандартную ошибку  
coef(cv, s="lambda.1se")

# PCA. Метод главных компонент
h <- heptathlon
help("heptathlon")
glimpse(h)
h <- select(h, -score)
# посмотрим описательные статистики
describe(h)
cor(h)
# переменные имеют разные единицы измерения, у них разные разбросы. Для применения PCA
# необходимо стандартизировать переменные
h.pca <- prcomp(h, scale = TRUE)
pca1 <- h.pca$x[,1]
# веса, с которыми старые переменные входят в новую переменную (первая главная компонента)
v1 <- h.pca$rotation[,1]
v1

head(pca1)

# посмотрим на результат метода PCA
summary(h.pca)
# визуализируем долю объясненной дисперсии разными компонентами
plot(h.pca)

# посмотрим как выглядят наши данные в координатах двух первых главных компонент
biplot(h.pca, xlim=c(-1,1))
