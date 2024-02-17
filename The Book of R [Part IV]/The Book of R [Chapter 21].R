### Exercise 21.1 ###
#a)
MASS::cats
View(cats)
plot(cats$Hwt ~ cats$Bwt,
     col = c("blue", "red")[as.numeric(cats$Sex)],
     pch = c(3,4)[as.numeric(cats$Sex)], ylab = "Heart weight", xlab = "Body weight")
legend("topleft", legend = c("Female","Male"), col = c("blue","red"), pch = c(3,4), cex = 0.75)

#b)
model <- lm(formula = Hwt ~ Bwt + Sex, data = cats)
summary(model) #Heart weight = -0.4149 + 4.0758(Body weight) - 0.0821(Sex)
               #Male heart weight = = -0.4149 + 4.0758(Body weight)
               #Female heart weight = -0.497 + 4.0758(Body weight)

#c)
cat.test <- data.frame(Sex = factor("F"), Bwt = 3.4)
predict(object = model, newdata = cat.test, interval = "prediction", level = 0.95)

#d)
  #names(model)
  #model$coefficients

abline(a = model$coefficients[1], b = model$coefficients[2], col = "red")
abline(a = model$coefficients[1] + model$coefficients[3], b = model$coefficients[2], col = "blue")

#e)
library("boot")
pairs(nuclear)

#f)
model.f <- lm(formula = cost ~ t1 + t2, data = nuclear)
summary(model.f)

#g)
model.g <- lm(formula = cost ~ t1 + t2 + date, data = nuclear)
summary(model.g)

#h)
model.h <- lm(formula = cost ~ cap + date + ne, data = nuclear)
summary(model.h)

#i)
data <- data.frame(Murders = c(8.6,8.9,8.52,8.89,13.07,14.57,21.36,28.03,31.49,37.39,46.26,47.24,52.33),
                   Police = c(260.35,269.8,272.04,272.96,272.51,261.26,268.89,295.99,319.87,341.43,356.59,376.69,390.19),
                   Unemployment = c(11,7,5.2,4.3,3.5,3.2,4.1,3.9,3.6,7.1,8.4,7.7,6.3),
                   Guns = c(178.15,156.41,198.02,222.10,301.92,391.22,665.56,1131.21,837.6,794.9,817.74,583.17,709.59))
pairs(data)

#j)
data_model <- lm(formula = Murders ~ Police + Unemployment + Guns, data = data)
summary(data_model)

#j)
data_model2 <- lm(formula = Murders ~ Police + Guns, data = data)
summary(data_model2)

#k)
newdata <- data.frame(Police = rep(300,2), Guns = c(500, 0))
predict(object = data_model2, newdata = newdata, interval = "confidence", level = 0.99)



### Exercise 21.2 ###
#a)
dane <- data.frame(Initial_Height = c(1000, 800, 600, 450, 300, 200, 100),
                   Distance = c(573, 534, 495, 451, 395, 337, 253))
plot(x = dane$Initial_Height, y = dane$Distance)

gal.mod2 <- lm(formula = Distance ~ Initial_Height + I(Initial_Height^2), data = dane)
gal.mod3 <- lm(formula = Distance ~ Initial_Height + I(Initial_Height^2) + I(Initial_Height^3), data = dane)
gal.mod4 <- lm(formula = Distance ~ Initial_Height + I(Initial_Height^2) + I(Initial_Height^3) + I(Initial_Height^4),
               data = dane)
summary(gal.mod2)
summary(gal.mod3)
summary(gal.mod4)

#c)
height.seq <- seq(from = 0, to = 1100, length = 1000)
predictions <- predict(object = gal.mod3, newdata = data.frame(Initial_Height = height.seq), interval = "confidence", level = 0.9)
plot(x = height.seq, y = predictions[ ,1], type = "l", ylab = "Distance", xlab = "Height")
lines(x = height.seq, y = predictions[ ,2], col = "orange2")
lines(x = height.seq, y = predictions[ ,3], col = "orange2")

#d)
View(trees)
plot(x = trees$Girth, y = trees$Volume)

#e)
model_e1 <- lm(formula = Volume ~ I(Girth^2), data = trees)
summary(model_e1)

model_e2 <- lm(formula = log(Volume) ~ log(Girth), data = trees)
summary(model_e2)

#f)
seq.dopredict <- seq(from = 5, to = 25, length = 100)
dopredict <- data.frame(Girth = seq.dopredict)
predict1 <- predict(object = model_e1, newdata = dopredict, interval = "prediction", level = 0.95)
predict2 <- predict(object = model_e2, newdata = dopredict, interval = "prediction", level = 0.95)
lines(x = dopredict[ ,1], y = predict1[ ,1], col = "red")
lines(x = dopredict[ ,1], y = predict1[ ,2], col = "red3", lty = 2)
lines(x = dopredict[ ,1], y = predict1[ ,3], col = "red3", lty = 2)
lines(x = dopredict[ ,1], y = exp(predict2[ ,1]), col = "blue")
lines(x = dopredict[ ,1], y = exp(predict2[ ,2]), col = "blue4", lty = 2)
lines(x = dopredict[ ,1], y = exp(predict2[ ,3]), col = "blue4", lty = 2)

#g)
str(mtcars)
model_g <- lm(formula = mpg ~ hp + wt + disp, data = mtcars)
summary(model_g)

#h)
mtcars$gpm <- 1/(mtcars$mpg)
model_h <- lm(formula = gpm ~ hp + wt + disp, data = mtcars) #model_h <- lm(formula = I(1/mpg) ~ hp + wt + disp, data = mtcars)
summary(model_h)



### Exercise 21.3 ###
#a)
levels(cats$Sex) <- c(1,2) # 1-Female, 2-Male
model_a <- lm(formula = Hwt ~ Bwt + Sex + Bwt:Sex, data = cats)
summary(model_a)

#b)
plot(x = cats$Bwt, y = cats$Hwt, col = c("red", "blue")[cats$Sex])
  #pred.F.data <- data.frame(Bwt = cats$Bwt[cats$Sex == 1], Sex = factor(rep(1, sum(cats$Sex == 1))))
  #pred.M.data <- data.frame(Bwt = cats$Bwt[cats$Sex == 2], Sex = factor(rep(2, sum(cats$Sex == 2))))
  #pred.F.line <- predict(object = model_a, newdata = pred.F.data)
  #pred.M.line <- predict(object = model_a, newdata = pred.M.data)
abline(a = 2.9813, b = 2.6364, col = "red")
abline(a = (2.9813 - 4.1654), b = (2.6364 + 1.6763), col = "blue")

#c)
cat.test$Sex <- factor(1)
predict(object = model_a, newdata = cat.test, interval = "prediction", level = 0.95)

#d)
model_c1 <- lm(formula = Volume ~ Girth + Height, data = trees)
model_c2 <- lm(formula = Volume ~ Girth*Height, data = trees)
summary(model_c1)
summary(model_c2)

#e)
model_c1log <- lm(formula = log(Volume) ~ log(Girth) + log(Height), data = trees)
model_c2log <- lm(formula = log(Volume) ~ log(Girth)*log(Height), data = trees)
summary(model_c1log)
summary(model_c2log)

#f)
model_f <- lm(formula = mpg ~ hp*factor(cyl) + wt, data = mtcars)
summary(model_f)

#h)
data_h <- data.frame(hp = c(100, 210, 200),
                     cyl = c(4, 8, 6),
                     wt = c(2.1, 3.9, 2.9))
predict(object = model_f, newdata = data_h, interval = "confidence", level = .95)