options(prompt = "Command:")
options(continue = "Dawaj dziadek, dawaj")

### Exercise 2.1 ###
#a)
a <- 2.3
(6*a+42)/3^(4.2-3.62)

#b)
(-4)^2+2

#c)
numbers <- c(25.2, 15, 16.44, 15.3, 18.6)
sqrt(mean(numbers)/2)

#d)
log(0.3) -> quest_e

#e)
exp(quest_e)

#f)
-0.00000000423546322



### Exercise 2.2 ###
#a)
3^2*4^(1/8) -> quest_a

#b)
quest_b <- quest_a/2.33

#c)
quest_c <- (-8.2)*10^(-13)

#d)
quest_b*quest_c



### Exercises 2.3 ###
#a)
seq_a <- seq(from = 5, to = (-11), by = (-0.3))

#b)
sort(x = seq_a, decreasing = FALSE)

#c)
vector <- c(-1, 3, -5, 7, 9)
rep_vector <- rep(vector, times = 2, each = 10)
sort(rep_vector, decreasing = TRUE)

#d)
i. <- seq(from = 6, to = 12)
ii. <- rep(5.3, times = 3)
iii. <- -3
iv. <- seq(from = 102, to = length(rep_vector), length.out = 9)
quest_d <- c(i., ii., iii., iv.)

#e)
length(quest_d) == 20



### Exercise 2.4 ###
#a)
quest_a <- c(seq(from = 3, to = 5, length.out = 5),
             rep(c(2,-5.1,-33), times = 2),
             7/42+2)
#b)
quest_b <- quest_a[c(1, length(quest_a))]

#c)
quest_c <- quest_a[-c(1, length(quest_a))]

#d)
quest_d <- c(quest_b[1], quest_c, quest_b[2])
sum(quest_d == quest_a) == length(quest_d)

#e)
quest_e <- sort(quest_a, decreasing = FALSE)

#f)
quest_f <- quest_e[length(quest_e):1]
quest_f == sort(quest_e, decreasing = TRUE)

#g)
quest_g <- c(rep(quest_c[3], times = 3),
             rep(quest_c[6], times = 4),
             quest_c[length(quest_c)])

#h)
quest_h <- quest_e
quest_h[c(1,5:7,length(quest_h))] <- 99:95



### Exercise 2.5 ###
#a)
vector <- c(2,0.5,1,2,0.5,1,2,0.5,1)
vector[1:length(vector)] <- rep(1, times = 3)

#b)
temperatures <- c(45,77,20,19,101,120,212)
celsius <- 5/9*(temperatures-32)

#c)
vector_1 <- c(2,4,6)
vector_2 <- c(1,2)
vector_3 <- rep(vector_1, times = 2)*rep(vector_2, each = 3)

#d)
vector_3[2:5] <- rep(c(-0.1,-100), each = 2)