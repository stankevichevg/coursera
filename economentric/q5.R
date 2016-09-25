library("ggplot2")
library("dplyr")
library("psych")
library("Ecdat")

# 4
R_sq <- 0.4
RSS = 120
ESS <- R_sq * RSS / (1 - R_sq)
cat("Answer for the 4th question is", round(ESS, digits = 2))

# 5
# Найдите средний вес цыплёнка на 10-ый день с момента рождения 
# (вне зависимости от типа диеты). Введите ответ с точностью до 2 
# знака после запятой, не забудьте про округление!
d <- ChickWeight
glimpse(d)
describe(d)

d <- filter(d, Time == 10)
cat("Answer for the 5th question is", round(describe(d)$mean[1], digits = 2))

# 6
# Цыплята на диете какого типа в среднем весят больше всего на 21-ый день?
d <- ChickWeight
d <- filter(d, Time == 21)
by_diet <- group_by(d, Diet)
weights <- summarise(by_diet, mean = mean(weight))
best_diet <- filter(weights, mean == max(mean))$Diet
cat("Answer for the 6th question is", best_diet)

# 7
# Оцените регрессию веса цыплёнка на его возраст и тип диеты. 
# Какой коэффициент детерминации R2 Вы получили? 
# Введите ответ с точностью до 2 знака после запятой, не забудьте про округление!
d <- ChickWeight
model <- lm(data=d, weight~Time+Diet)
cat("Answer for the 7th question is", round(summary(model)$r.squared, digits = 2))

# 9
# По 5 000 наблюдениям была оценена следующая модель: y^i=20+5xi+6zi. 
# Проверяя гипотезу о том, что β2+β3=10 (β2 и β3 - это, соответственно, коэффициенты при x и z), 
# чему равно значение наблюдаемой статистики, если se(β2)=0.7, se(β3)=0.5, Covˆ(β2,β3)=0.25?

beta2_se <- 0.7
beta3_se <- 0.5
betas_cov <- 0.25
beta2_hat <- 5
beta3_hat <- 6
betas_summ <- 10
betas_summ_se <- sqrt(beta2_se^2 + beta3_se^2 + 2 * betas_cov)
statistic <- round(((beta2_hat + beta3_hat) - (betas_summ)) / betas_summ_se, digits = 3)
cat("Answer for the 9th question is", statistic)

# 12
d <- diamonds
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)

# 13
model <- lm(data=d, price~carat+table+x+y+z+depth)
summary(model)

# 14
model <- lm(data=d, price~carat+table+x+y+depth)
help("diamonds")
confint(model, level = 0.9)

# 16
round(((80 - 40)/2) / (40 / (75 - 4)), digits = 2)

# 19
help(BudgetFood)
d <- BudgetFood
glimpse(d)
model <- lm(data=d, wfood~totexp+size)
test_data <- data.frame(totexp=c(700000), size=c(4))
prediction <- predict(model, test_data, interval = "prediction", level = 0.9)
cat("Answer for the 19th question is", round(prediction[2], digits = 2))

#20 
resettest(model)$statistic

# 21
d <- na.omit(BudgetFood)
model <- lm(data=d, wfood~totexp+size)
model2 <- lm(data=d, wfood~sex*(totexp+size))
waldtest(model, model2)
# H0 отвергается: есть зависимость от пола

# 26
d <- mtcars
model <- lm(data=d, mpg~disp+hp+wt)
round(vif(model), digits = 2)

# 27
pcdata <- select(d, disp, hp, wt)
d.pca <- prcomp(pcdata, scale = TRUE)
round(sqrt(sum(d.pca$x[,1]^2)), digits = 2)

# 28
d <- mutate(d, pca1=d.pca$x[,1], pca2=d.pca$x[,2], pca3=d.pca$x[,3])
model2 <- lm(data=d, mpg~pca1+pca2)
model3 <- lm(data=d, mpg~pca1+pca2+pca3)
diff <- summary(model3)$r.squared - summary(model2)$r.squared
cat("Answer for the 19th question is", round(diff, digits = 2))


  