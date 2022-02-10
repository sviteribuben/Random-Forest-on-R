library("spikeslab")
library("ggplot2")
library("dplyr")
library("reshape2")
library("MCMCpack")
library("quantreg")
library("randomForest")
library("caret")
library("rpart")

f <- read.table("flats_r.txt", header=TRUE, sep="\t", dec=".")
#check data
glimpse(f)

#regression model from the quanreg, totsp - total sq

model_q01 <- rq(data=f, price~totsp, tau=c(0.1, 0.5, 0.9))
summary(model_q01)

#base plot
base <- qplot(data=f, totsp, price)
base

# add to plot smoothing
#help(stat_smooth)

#base + stat_smooth(method="rq", se=FALSE, span = 0.1) +
 # stat_smooth(method="rq", se=FALSE, span = 0.9)

#base + geom_smooth(stat = "smooth", method = "rq", se=FALSE, formula = "tau = 0.1")
base_q <- base + stat_smooth(geom = "smooth", method = "rq", se=FALSE) + 
  stat_smooth(geom = "smooth", method = "lm", se=FALSE)


#differenc material of walls
#without factor - variable will be non-stop
base_q + aes(color=factor(brick))

#Random forest for predict

#slice to 2 parts
in_sample <- createDataPartition(f$price, p=0.75, list = FALSE)
head(in_sample)

f_train <- f[in_sample,]
f_test <- f[-in_sample,]


model_lm <- lm(data = f_train, price~totsp+kitsp+livesp+brick)

model_rf <- randomForest(data = f_train, price~totsp+kitsp+livesp+brick)

y <- f_test$price
yhat_lm <- predict(model_lm, f_test)

yhat_rf <- predict(model_rf, f_test)

# sum of square errors

sum((y - yhat_lm)^2)
sum((y - yhat_rf)^2)

