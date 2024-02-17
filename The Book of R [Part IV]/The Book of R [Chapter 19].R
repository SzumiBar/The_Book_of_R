### Exercise 19.1 ###
info <- data.frame("dep" = c(93,120,65,105,115,82,99,87,100,90,78,95,93,88,110,85,
                             45,80,28,75,70,65,55,50,40,100,75,65,40,73,65,50,30,
                             45,50,45,55,96,58,95,90,65,80,85,95,82),
                   "site" = c(rep("Site i",15),
                              rep("Site ii",10),
                              rep("Site iii",12),
                              rep("Site iv",9)))

#a)
boxplot(formula = dep ~ site, data = info)
means <- tapply(X = info$dep, INDEX = info$site, FUN = mean)
points(x = 1:4, y = means, pch = 4, cex = 1.5)

#b)
devs <- tapply(X = info$dep, INDEX = info$site, FUN = sd)
max(devs)/min(devs) < 2 #Check for equality of variances

qqnorm(y = info$dep)
qqline(y = info$dep)

#c)
model_1 <- aov(formula = dep ~ site, data = info)
summary(model_1)

#d)
View(iris)

qqnorm(y = iris$Sepal.Length)
qqline(y = iris$Sepal.Length) # This one

qqnorm(y = iris$Sepal.Width)
qqline(y = iris$Sepal.Width) # This one

qqnorm(y = iris$Petal.Length)
qqline(y = iris$Petal.Length)

qqnorm(y = iris$Petal.Width)
qqline(y = iris$Petal.Width)

#e)
model_2 <- aov(formula = Sepal.Length ~ Species, data = iris)
summary(model_2)

model_3 <- aov(formula = Sepal.Width ~ Species, data = iris)
summary(model_3)



### Exercise 19.2 ###
#a)
View(quakes)

cut(x = quakes$depth,
    breaks = c(0, 200, 400, 680),
    right = TRUE,
    labels = c("Low", "Medium", "High")) -> quakes$new

#b)
qqnorm(y = quakes$stations[quakes$new == "Low"])
qqline(y = quakes$stations[quakes$new == "Low"], col = "red", lwd = 2)

qqnorm(y = quakes$stations[quakes$new == "Medium"])
qqline(y = quakes$stations[quakes$new == "Medium"], col = "blue", lwd = 2)

qqnorm(y = quakes$stations[quakes$new == "High"])
qqline(y = quakes$stations[quakes$new == "High"], col = "green", lwd = 2) #Normality check

#head(x = quakes, n = 10)
tapply(X = quakes$stations,
       INDEX = quakes$new,
       FUN = sd) -> quakes_sd

max(quakes_sd) / min(quakes_sd) < 2 #Equality of variance check

#c)
kruskal.test(formula = stations ~ new, data = quakes)

#d)
library("MASS")
View(Cars93)

quest_d <- aggregate(x = Cars93$Length,
                     by = list(Cars93$AirBags, Cars93$Man.trans.avail),
                     FUN = mean)

#e)
interaction.plot(x.factor = quest_d$Group.1,
                trace.factor = quest_d$Group.2,
                response = quest_d$x,
                trace.label = "Manual transmission",
                xlab = "AirBags",
                ylab = "Length")

#f)
model <- aov(formula = Length ~ AirBags + Man.trans.avail + AirBags:Man.trans.avail, data = Cars93)
#model <- aov(formula = Length ~ AirBags*Man.trans.avail, data = Cars93)
summary(model)