### Exercise 7.1 ###
#a)
plot(x = 0,
     y = 0,
     type = "n",
     xlab = "",
     ylab = "",
     ylim = c(7, 13),
     xlim = c(-3, 3))
abline(h = c(7, 13),
       v = c(-3, 3),
       lty = 2,
       lwd = 2,
       col = "grey")
text(x = 0,
     y = 10,
     labels = "SOMETHING\nPROFOUND")
arrows(x0 = -2.5, y0 = 10, x1 = -1, y1 = 10)
arrows(x0 = -2.5, y0 = 7.5, x1 = -1, y1 = 9.5)
arrows(x0 = -2.5, y0 = 12.5, x1 = -1, y1 = 10.5)
arrows(x0 = 2.5, y0 = 10, x1 = 1, y1 = 10)
arrows(x0 = 2.5, y0 = 7.5, x1 = 1, y1 = 9.5)
arrows(x0 = 2.5, y0 = 12.5, x1 = 1, y1 = 10.5)

#b)
height <- c(161,185,174,154,188,178,170,167,181,178)
weight <- c(55,85,75,42,93,63,58,75,89,67)
sex <- factor(c("f","m","m","f","m","m","f","m","m","f"))
quest_b <- data.frame(height, weight, sex)

plot(x = quest_b$weight,
     y = quest_b$height,
     type = "n")
points(x = quest_b[quest_b$sex == "f", 2],
       y = quest_b[quest_b$sex == "f", 1],
       col = "red")
points(x = quest_b[quest_b$sex == "m", 2],
       y = quest_b[quest_b$sex == "m", 1],
       col = "blue")
legend("topleft",
       legend = c("female","male"),
       pch = c(1,1),
       col = c("red","blue"))



### Exercise 7.2 ###
#a)
library(ggplot2)
ggplot(quest_b) +
  geom_point(aes(weight, height, color = sex, shape = sex))