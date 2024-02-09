### Exercise 16.1 ###
#a)
dbinom(x = 1:13,
       size = 13,
       prob = 0.75) -> x
round(x = x,
      digits = 3) -> x
barplot(height = x)

#b)
x[length(x)]

#c)
1 - pbinom(q = 9,
           size = 13,
           prob = 0.75)

#d)
sum(dbinom(x = 8:11,
           size = 13,
           prob = 0.75))

1 - pbinom(q = 7,
           size = 13,
           prob = 0.75) - (1 - pbinom(q = 11,
                                      size = 13,
                                      prob = 0.75))

#e)
1 - sum(dbinom(x = 9:13,
               size = 13,
               prob = 0.75))

#f)
rbinom(n = 10,
       size = 13,
       prob = 0.75) -> quest_f

#g)
mean(quest_f)
sd(quest_f)



### Exercise 16.2 ###
#a)
1 - ppois(q = 100,
          lambda = 107)

#b)
dpois(x = 0,
      lambda = 107)

#c)
barplot(height = dpois(x = 60:150,
                       lambda = 107),
        main = "Poisson distribution\n?? = 107",
        xlab = "Number of cars",
        ylab = "Pr(X = x)",
        names.arg = 60:150)

#d)
rpois(n = 260,
      lambda = 107) -> quest_d
hist(quest_d,
     xlim = c(60, 150))



### Exercise 16.3 ###
#a)
dunif(x = 5.5,
      min = 3,
      max = 70)

#b)
qunif(p = 0.85,
      min = 3,
      max = 70)

#c)
3+70/2 -> mean_c
sqrt((3-70)^2/12) -> sd_c

#d)
min_d <- mean_c - 0.5*sd_c
max_d <- mean_c + 0.5*sd_c

punif(q = max_d,
      min = 3,
      max = 70) - punif(q = min_d,
                        min = 3,
                        max = 70)

#e)
barplot(dunif(3:70,min=3,max=70),
        space=0)
max(dunif(3:70,min=3,max=70))

#f)
quantile(x = runif(n = 10,
                   min = 3,
                   max = 70),
         probs = 0.85
)
quantile(x = runif(n = 1000,
                   min = 3,
                   max = 70),
         probs = 0.85
)



### Exercise 16.4 ###
#a)
i. <- 1 - dnorm(x = 20, mean = 17, sd = 4.5)
ii. <- dnorm(x = 10, mean = 17, sd = 4.5) - dnorm(x = 5, mean = 17, sd = 4.5)  
iii. <- qnorm(p = 0.9, mean = 17, sd = 4.5)

X <- seq(from = 17-4*4.5, to = 17+4*4.5, length.out = 1000)
Y <- dnorm(X, mean = 17, sd = 4.5)
plot(x = X, y = Y, type = "l", main = "Normal distribution\nMean = 17 SD = 4.5", xlab = "x", ylab = "f(x)")
abline(h = 0, col = "grey")
abline(v = iii., col = "grey", lty = 2)

Xval <- X[X >= iii.]
Yval <- Y[X >= iii.]
polygon(rbind(
  cbind(Xval[1], 0),
  cbind(Xval, Yval),
  cbind(Xval[length(Xval)], 0)),
  col = "blue"
)

v. <- rnorm(n = 10, mean = 17, sd = 4.5)

#b)
i. <- pnorm(q = 11, mean = 10, sd = 2) - pnorm(q = 9.5, mean = 10, sd = 2)

ii.1 <- (9.5-10)/2
ii.2 <- (11-10)/2
pnorm(ii.2) - pnorm(ii.1)

iii. <- qnorm(p = 0.025, mean = 10, sd = 2)

iv. <- (iii. - 10)/2



### Exercise 16.5 ###
#a)
lambda <- 3500
lambda.e <- lambda/365.25

Xval <- seq(from = 0, to = 1, length.out = 1000)
Yval <- dexp(x = Xval, rate = lambda.e)
plot(x = Xval, y = Yval, type = "l", main = "Exponential function", xlab = "x", ylab = "f(x)")

ii. <- 1/lambda.e

lambda.e_minute <- lambda.e/24/60
iii. <- pexp(q = 60*24, rate = lambda.e_minute)

iv. <- qexp(p = 0.9, rate = lambda.e_minute*60)

#b)
lambda.e <- 1/11 #rate per year
i. <- pexp(q = 5, rate = lambda.e)

lambda.e2 <- 1/9
ii. <- pexp(q = 6, rate = lambda.e2)

iii.1 <- 1 - pexp(q = 15, rate = lambda.e)
iii.2 <- 1 - pexp(q = 15, rate = lambda.e2)