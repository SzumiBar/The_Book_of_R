### Exercise 14.1 ###
#a)
hist(InsectSprays$count)

#b)
library(tidyverse)

InsectSprays %>%
  group_by(spray) %>%
  summarize(number = sum(count))

InsectSprays %>%
  count(spray, wt = count) -> quest_b

barplot(height = quest_b$n,
        names.arg = quest_b$spray)

pie(x = quest_b$n,
    labels = quest_b$spray)

#c)
ggplot(data = InsectSprays, aes(x = spray, y = count)) +
  geom_boxplot() +
  labs(title = "Question C") #ggtitle()

#d)
vert_lines <- as.vector(quantile(USArrests$UrbanPop, c(0.25, 0.5, 0.75)))

qplot(x = USArrests$UrbanPop,
      geom = "blank",
      main = "Question D",
      xlab = "Urban Population") +
  geom_histogram(breaks = seq(0,100,10)) +
  geom_vline(aes(xintercept = vert_lines,
                 linetype = factor(c("1st IQR", "Median", "3rd IQR")),
                 color = factor(c("1st IQR", "Median", "3rd IQR"))),
             show.legend = T) +
  labs(linetype = "Legend",
       color = "Legend")

#e)
quest_e <- t(as.matrix(USArrests[,-3]))
state.abb

barplot(height = quest_e,
        names.arg = state.abb,
        horiz = T,
        cex.names = 0.5,
        las = 1,
        col = c("red","green","blue"))
legend("topright",
       legend = c("Murder","Assault","Rape"),
       pch = 20,
       col = c("red","green","blue"),
       cex = 0.8)

#f)
pop_great <- USArrests$UrbanPop > median(USArrests$UrbanPop)
urbancat <- factor(pop_great, labels = c(0,1))

#g)
quest_g <- USArrests[-3]
quest_g[4] <- urbancat

#h)
pairs(quest_g[1:3],
      col = quest_g$V4)

#i)
quest_i <- factor(cut(quakes$mag,
                      breaks = c(min(quakes$mag),
                                 quantile(quakes$mag, (1/3)),
                                 quantile(quakes$mag, (2/3)),
                                 max(quakes$mag)),
                      include.lowest = TRUE),
                  labels = c("Min", "Med", "High"))

#j)
length(quakes)
quakes[6] <- quest_i
plot(x = quakes$long,
     y = quakes$lat,
     col = quest_i,
     pch = (1:3)[quest_i])

#k)
legend("bottomleft",
       legend = c("Magnitude 4 to 4.4", "Magnitude 4.4 to 4.7", "Magnitude 4.7 to 6.4"),
       pch = 1:3,
       col = c("black", "red", "green"))