### Exercise 13.1 ###
#a)
i. #numeric-discrete
ii. #categorical-ordinal
iii. #numeric-continous
iv. #categorical-ordinal
v. #categorical-nominal
vi. #numeric-continous

#b)
i. #statistic
ii. #statistic
iii. #parameter
iv. #parameter
v. #statistic



### Exercise 13.2 ###
#a)
nrow(quakes[quakes$depth >= 300, ])/nrow(quakes)

#b)
mean(quakes$mag[quakes$depth >= 300])
median(quakes$mag[quakes$depth >= 300])

#c)
for(i in levels(chickwts$feed)) {
  result <- mean(chickwts$weight[chickwts$feed == i])
  result <- round(result, 1)
  cat("Mean weight of chicks feeded with", i, "equals", result, "\n")
}

#d)
InsectSprays #count is discrete numerical, spray is categorical numinal

#e)
quest_e <- table(InsectSprays$count)
quest_e[quest_e == max(quest_e)]

#f)
tapply(X = InsectSprays$count,
       INDEX = InsectSprays$spray,
       FUN = sum)

#g)
for(i in levels(InsectSprays$spray)) {
  more_table <- InsectSprays[InsectSprays$count >= 5, ]
  result <- nrow(more_table[more_table$spray == i, ]) / nrow(InsectSprays[InsectSprays$spray == i, ])
  result <- result*100
  result <- round(result, 1)
  cat(result, "%" , " of agricultural units in ", i, " spray type group that had at least five bugs in them", "\n", sep = "")
}

#h)
tapply(X = InsectSprays$count,
       INDEX = InsectSprays$spray,
       FUN = function(X) {
         Y <- round((length(X[X >= 5]) / length(X))*100, digits = 0)
         Y
       })

tapply(InsectSprays$count, 
       InsectSprays$spray, 
       function(x){round(mean(x>=5)*100,2)}) #Lepsza wersja



### Exercise 13.3 ###
#a)
quantile(chickwts$weight, prob = c(0.1, 0.3, 0.9))

tapply(X = chickwts$weight,
       INDEX = chickwts$feed,
       FUN = var)

#b)
IQR(quakes$depth)

summary(quakes$mag[quakes$depth >= 400])

depthcut <- cut(quakes$depth,
                breaks = c(40, 200, 360, 520, 680),
                right = F,
                include.lowest = T)
levels(depthcut)

tapply(X = quakes$mag,
       INDEX = depthcut,
       mean)

tapply(X = quakes$mag,
       INDEX = depthcut,
       sd)

tapply(X = quakes$mag,
       INDEX = depthcut,
       FUN = function(x){quantile(x, prob = 0.8)})



### Exercise 13.4 ###
#a)
weight <- c(55,85,75,42,93,63,58,75,89,67)
height <- c(161,185,174,154,188,178,170,167,181,178)

cor(weight, height)

#b)
plot(mtcars$hp, mtcars$qsec)
abline(lm(qsec ~ hp, mtcars))
round(cor(mtcars$hp, mtcars$qsec), 1) -> touse
text(300, 22, labels = paste("r =", touse))

mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")

library(ggplot2)
qplot(mtcars$hp, mtcars$am)

cor(mtcars$hp[mtcars$am == "Manual"], mtcars$qsec[mtcars$am == "Manual"])
cor(mtcars$hp[mtcars$am == "Automatic"], mtcars$qsec[mtcars$am == "Automatic"])

#c)
plot(x = chickwts$weight[chickwts$feed == "sunflower"],
     y = rep(0, length(chickwts$weight[chickwts$feed == "sunflower"])),
     yaxt = "n",
     bty = "n",
     ylab = "",
     xlab = "Weight")
abline(h = 0,
       col = "grey",
       lty = 2)
arrows(275, 0.5, 230, 0.05)
text(275,
     0.6,
     label = "Outlier",
     cex = 1.5)

sd(chickwts$weight[chickwts$feed == "sunflower"])
IQR(chickwts$weight[chickwts$feed == "sunflower"])

sd(sort(chickwts$weight[chickwts$feed == "sunflower"])[-1])
IQR(sort(chickwts$weight[chickwts$feed == "sunflower"])[-1])