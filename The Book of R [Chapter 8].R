### Exercise 8.1 ###
#a)
View(quakes)
write.table(quakes[quakes$mag >= 5, ],
            "C:\\Users\\OMaDaym\\OneDrive\\Desktop\\q5.txt",
            row.names = F,
            sep = "!")
q5.dframe <- read.table("C:\\Users\\OMaDaym\\OneDrive\\Desktop\\q5.txt",
                       sep = "!",
                       header = T)

#b)
library(car)
plot(x = Duncan$education,
     y = Duncan$income,
     xlim = c(0,100),
     ylim = c(0,100),
     xlab = "Education",
     ylab = "Income",
     type = "n")
points(x = Duncan$education[Duncan$prestige <= 80],
       y = Duncan$income[Duncan$prestige <= 80],
       col = "black",
       pch = 1)
points(x = Duncan$education[Duncan$prestige > 80],
       y = Duncan$income[Duncan$prestige > 80],
       col = "blue",
       pch = 19)
legend("bottomright",
       legend = c("Prestige <= 80", "Prestige > 80"),
       pch = c(1,19),
       col = c("black","blue"))
png("C:\\Users\\OMaDaym\\OneDrive\\Desktop\\quest_b.png",width = 500,height = 500)

#c)
exer <- list(quakes,q5.dframe, Duncan)
dput(exer, "C:\\Users\\OMaDaym\\OneDrive\\Desktop\\Exercise8-1.txt")
list.of.dataframes <- dget("C:\\Users\\OMaDaym\\OneDrive\\Desktop\\Exercise8-1.txt")