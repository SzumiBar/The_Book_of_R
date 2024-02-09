### Exercise 23.1 ###
#a)
model_a <- lm(formula = mpg ~ cyl, data = mtcars)
par(mfrow = c(1,2))
boxplot(formula = mtcars$mpg ~ mtcars$cyl, ylab = "MPG", xlab = "Cylinders")
plot(x = mtcars$cyl, y = mtcars$mpg)
abline(a = model_a, b = model_a, lwd = 2)

#b)
mat1 <- matrix(c(2,1,1,3), byrow = T, ncol = 1)
layout(mat = mat1)
layout.show(n = 3)

mat2 <- matrix(c(1,1,1,1,2,4,3,5), byrow = F, ncol = 4)
layout(mat = mat2)
layout.show(n = 5)

mat3 <- matrix(c(2,2,2,3,3,4,3,3,5,1,1,1), byrow = T, ncol = 3)
layout(mat = mat3)
layout.show(n = 5)

#c)
layout(mat = matrix(c(1,1,2,4,1,1,3,4), byrow = T, ncol = 4))
layout.show(n = 4)

dev.new(height = 4.5, width = 9)
layout(mat = matrix(c(1,1,2,4,1,1,3,4), byrow = T, ncol = 4))
par(mar = c(4,4,2,1))

plot(x = quakes$long, y = quakes$lat, cex = 0.02*quakes$stations,
     xlab = "Longitude", ylab = "Latitude")
box(which = "figure", lty = 1, col = "grey", lwd = 3)

plot(x = quakes$mag, y = quakes$stations,
     xlab = "Magnitude", ylab = "Stations")
box(which = "figure", lty = 1, col = "grey", lwd = 3)

plot(x = quakes$depth, y = quakes$stations,
     xlab = "Depth", ylab = "Stations")
box(which = "figure", lty = 1, col = "grey", lwd = 3)

hist(x = quakes$stations, xlab = "Stations", main = NA)
abline(v = mean(quakes$stations), lty = 2)
box(which = "figure", lty = 1, col = "grey", lwd = 3)

#d)
interactive.arrow <- function(..., label = NA) {
  cords <- locator(n = 2)
  arrows(x0 = cords$x[1], y0 = cords$y[1],
         x1 = cords$x[2], y1 = cords$y[2], ...)
  if(!is.na(label)) {
    lab.cord <- locator(n = 1)
    text(x = lab.cord$x, y = lab.cord$y, labels = label)
  }
}

dev.off()
boxplot(x = quakes$mag)
interactive.arrow(xpd = NA, label = "maximum")
interactive.arrow(xpd = NA, label = "minimum")
interactive.arrow(xpd = NA, label = "outliers")
interactive.arrow(xpd = NA, label = "median")
interactive.arrow(xpd = NA, label = "1st quartile")
interactive.arrow(xpd = NA, label = "3rd quartile")



### Exercise 23.2 ###
#a)
# Proper data is not available - unable to do this exercise #
library("ggplot2")
head(diamonds)
diamonds$SGD <- round(1.34 * diamonds$price)

dev.new(height = 6, width = 6)
par(mar = c(0,4,2,0))
boxplot(diamonds$SGD ~ diamonds$cut, frame = F, yaxt = "n", xaxt = "n")
axis(side = 2, at = seq(from = 0, to = 28000, by = 2000), tcl = 1, las = 1, mgp = c(3,0.5,0))
text(locator(n = 5), labels = c("Ideal","Premium","Very Good","Good","Fair"))

#b)
# Proper data is not available - unable to do this exercise #
dev.new(width = 8, height = 7)
par(mar = c(2,5,3,5), oma = c(2,1,1,1))

plot(y = diamonds$price, x = diamonds$depth,
     col = c("red", "green", "blue", "orange", "yellow")[diamonds$cut],
     bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
box(which = "plot", lty = 1, bty = "u")
axis(side = 1,
     at = seq(from = 40, to = 80, by = 5),
     family = "sans",
     font = 3,
     cex.axis = 0.75)
axis(side = 1,
     at = seq(from = 42.5, to = 82.5, by = 5),
     tcl = -0.25,
     family = "sans",
     font = 3,
     cex.axis = 0.5)
axis(side = 2,
     at = seq(from = 0, to = 18000, by = 1000),
     las = 1,
     font = 3)
axis(side = 4,
     at = seq(from = 0, to = 18000, by = 1000),
     labels = round(seq(from = 0, to = 18000, by = 1000) / 1.37),
     las = 1,
     font = 3)