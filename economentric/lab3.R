library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("ggplot2")
library("foreign")
library("hexbin")
library("vcd")
library("pander")
library("plyr")

h <- sample_frac(diamonds, 0.07)
glimpse(h)
help("diamonds")

qplot(data=h, carat, price)

# So, we can see non leanear relationship
# Look to log 
bg <- qplot(data=h, log(carat), log(price))
bg
# Note, now it looks more lenear

# Playing with dummy variables
train <- sample_frac(diamonds, 0.3)

createDummies <- function(all_data, df, col_name) {
  for (level in unique(all_data[[col_name]])) {
    name <- paste(col_name, sub(" ", "_", tolower(level)), sep = "_")
    df[name] <- factor(ifelse(df[[col_name]] == level, 1, 0))
  }
  return(df)
}

createFeatures <- function(all_data, data) {
  data <- createDummies(data, train, "color")
  data <- createDummies(data, train, "cut")
  data <- createDummies(data, train, "clarity")
  return(data)
}

train <- createFeatures(diamonds, train)

model_0 <- lm(data=train, log(price) ~ log(carat) )
model_1 <- lm(data=train, log(price) ~ (log(carat) *
                                      (color_h + color_e + color_f + color_g + color_j + color_d) * 
                                      (cut_ideal + cut_fair + cut_premium + cut_very_good) *
                                      (clarity_vs2 +
                                         clarity_si1 +
                                         clarity_si2 + 
                                         clarity_vs1 +
                                         clarity_vvs2 +
                                         clarity_i1 +
                                         clarity_if) *
                                      log(depth) *
                                      log(table)
                                    )
              )

summary(model_1)

test <- sample_frac(diamonds, 0.3)
test <- createFeatures(diamonds, test)
exp(predict(model_1, newdata = test, interval = "confidence"))
exp(predict(model_1, newdata = test, interval = "prediction"))

# 
gg <- qplot(data=train, log(carat), log(price))

gg + aes(col=clarity) + stat_smooth(method = "lm") + facet_wrap(~color)

# Compare models
mtable(model_0, model_1)

# Ramsey test
resettest(model_1)
