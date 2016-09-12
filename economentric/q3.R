library(ggplot2)
library(memisc)
library(lmtest)

d <- diamonds
glimpse(d)

model_0 <- lm(data=d, price~log(carat))
summary(model_0)

model_1 <- lm(data=d, price~log(carat)+y+x)


createDummies <- function(all_data, df, col_name) {
  for (level in unique(all_data[[col_name]])) {
    name <- paste(col_name, sub(" ", "_", tolower(level)), sep = "_")
    df[name] <- factor(ifelse(df[[col_name]] == level, 1, 0))
  }
  return(df)
}

d <- createDummies(diamonds, d, "clarity")
d <- createDummies(diamonds, d, "cut")
glimpse(d)

model_2 <- lm(data=d, price~carat)
model_3 <- lm(data=d,
              price~carat +
                clarity_si2 +
                clarity_si1 +
                clarity_vs1 +
                clarity_vs2 +
                clarity_vvs2 +
                clarity_vvs1 +
                clarity_i1
              )

summary(model_2)
summary(model_3)

mtable(model_2)

model_4 <- lm(data=d, price~carat)
model_5 <- lm(data=d, price~carat+depth+cut_ideal+cut_premium+cut_good+cut_very_good)
waldtest(model_4, model_5)

resettest(model_5)

qplot(data = d, log(price), fill=color, geom = "density", alpha = 0.5) + facet_wrap(~color)
qplot(data = d, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_wrap(~cut)

qplot(data=d, log(carat), log(price), color = clarity) + facet_wrap(~cut)
