### Exercise 20.1 ###
#a)
library("MASS")
View(survey)

survfit <- lm(formula = Height ~ Wr.Hnd, data = survey)
summary(survfit)

xvals <- data.frame(Wr.Hnd = c(12, 15.2, 17, 19.9))

predict(object = survfit,
        interval = "confidence",
        newdata = xvals,
        level = 0.99)

#b)
incomplete.obs <- which(is.na(survey$Height)|is.na(survey$Wr.Hnd))

incomp.Wr.Hnd <- data.frame(Index = incomplete.obs,
                            Wr.Hnd = survey$Wr.Hnd[incomplete.obs],
                            Height = survey$Height[incomplete.obs])

beta_hat.1 <- cor(x = survey$Wr.Hnd[-incomplete.obs], y = survey$Height[-incomplete.obs]) * sd(survey$Height[-incomplete.obs])/sd(survey$Wr.Hnd[-incomplete.obs])
beta_hat.0 <- mean(survey$Height[-incomplete.obs]) - beta_hat.1*mean(survey$Wr.Hnd[-incomplete.obs])

#c)
model <- lm(formula = Height ~ Pulse, data = survey)
summary(model)


plot(Height ~ Pulse, data = survey)
abline(model)


confint(object = model, level = 0.9) # 90% CI for parameters


xseq <- seq(from = 30, to = 110, by = 1)
xseq.df <- data.frame(Pulse = seq(from = 30, to = 110, by = 1))


band.conf <- predict(object = model, newdata = xseq.df,
                     interval = c("confidence"),
                     level = 0.9)
band.pred <- predict(object = model, newdata = xseq.df,
                     interval = c("prediction"),
                     level = 0.9)
lines(x = xseq, y = band.conf[ ,2])
lines(x = xseq, y = band.conf[ ,3])
lines(x = xseq, y = band.pred[ ,2], col = "grey")
lines(x = xseq, y = band.pred[ ,3], col = "grey")


incomplete.obs <- which(x = is.na(survey$Height) | is.na(survey$Pulse))
height.mean <- mean(survey$Height[-incomplete.obs])
abline(h = height.mean, col = "blue", lty = 2)

#d)
View(mtcars)
plot(formula = mpg ~ wt, data = mtcars)
model_d <- lm(formula = mpg ~ wt, data = mtcars)
summary(model_d)

#e)
abline(model_d)

#f)
text(x = 4.5, y = 30, label = "y = 37.29 + x(-5.35)\nR^2 = 0.75")

#g)
test <- data.frame(wt = 6)
predict(object = model_d, newdata = test, interval = "prediction", level = 0.95)



### Exercise 20.2 ###
#a)
MASS::survey
table(survey$Exer)
boxplot(Height ~ Exer, data = survey)

#b)
model.b <- lm(formula = Height ~ Exer, data = survey)
summary(model.b)

#d)
one_of_each <- data.frame(Exer = factor(names(table(survey$Exer))))
predict(object = model.b, newdata = one_of_each, level = 0.95, interval = "prediction")

#e)
summary(aov(formula = Height ~ Exer, data = survey))

#g)
model.g <- lm(formula = qsec ~ gear, data = mtcars)
summary(model.g)

#h)
model.h <- lm(formula = qsec ~ factor(mtcars$gear), data = mtcars)
summary(model.h)

par(mfrow = c(1,2))
boxplot(qsec ~ gear, data = mtcars)
plot(qsec ~ gear, data = mtcars)
abline(model.g)
points(x = 3:5, y = tapply(X = mtcars$qsec, INDEX = mtcars$gear, FUN = mean), col = "blue", pch = 4, cex = 1.5)