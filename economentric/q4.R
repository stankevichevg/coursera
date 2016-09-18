
library("ggplot2")

h <- airquality

# 15
qplot(h$Ozone, h$Temp)
# 16
model <- lm(data=h, Ozone~Solar.R+Wind+Temp)
vif(model)
# 17
# убираем строки с пропущенными данными
h <- na.omit(h)
y <- h$Ozone
x <- model.matrix(data=h, Ozone ~ 0 + Solar.R + Wind + Temp)
lambdas <- seq(50,0.1,length=30)
m_lasso <- glmnet(x, y, alpha = 1, lambda = lambdas)
coef(m_lasso, s = 1)

# 18
m_ridge <- glmnet(x, y, alpha = 0, lambda = lambdas)
coef(m_lasso, s = 2)

# 20
h.pca <- prcomp(x, scale = TRUE)
qplot(h.pca$x[,1], h.pca$x[,2])
plot(h.pca)
