### Exercise 17.1 ###
#a)
se <- 11.3/sqrt(6)

#b)
pnorm(q = 55,
      mean = 41.1,
      sd = 11.3) - pnorm(q = 45,
                         mean = 41.1,
                         sd = 11.3)

#c)
fail <- 65/2
pnorm(q = fail, mean = 41.1, sd = 11.3)

#d)
0.35*140 >= 5 & 0.65*140 >= 5

#e)
1 - pnorm(q = 0.4, mean = 0.35, sd = sqrt((0.65*0.35)/140))

#f)
qnorm(p = 0.1, mean = 0.35, sd = sqrt((0.65*0.35)/140))
qnorm(p = 0.9, mean = 0.35, sd = sqrt((0.65*0.35)/140))

#g)
se <- 34.51/sqrt(63-1)

#h)
x.hat <- (40-37.8)/se
1 - pt(q = x.hat, df = 62) -> i.

x.hat2 <- (30-37.8)/se
pt(q = x.hat2, df = 62) -> ii.

pt(q = x.hat,df = 62) - 0.5



### Exercise 17.1 ###
#a)
14.22 + c(-1,1)*qnorm(p = 0.95)*(2.9/sqrt(34))

#b)
14.22 + c(-1,1)*qt(p = 0.95, df = 33)*(2.9/sqrt(34-1))

#c)
rhand <- 352/400
lhand <- 37/400
ambid <- 11/400
se.lhand <- sqrt((lhand*(1-lhand))/400)
37/400 + c(-1,1)*qnorm(0.995)*se.lhand

#d)
se.lhand.ambid <- sqrt(((lhand+ambid)*(1-(lhand+ambid))) / 400)
(lhand+ambid) + c(-1,1)*qnorm(0.995)*se.lhand.ambid

#e)
matrix(data = NA, nrow = 5000, ncol = 3) -> step1

for(i in 1:5000) {
  rexp(n = 300, rate = 0.1) -> x
  mean(x) -> srednia
  sd(x) -> odchylenie
  CI95 <- srednia + c(-1,1)*qt(p = 0.975, df = 300-1)*(odchylenie/sqrt(300))
  step1[i,1] <- CI95[1]
  step1[i,2] <- CI95[2]
  if(1/0.1 >= CI95[1] & 1/0.1 <= CI95[2]) {
    step1[i,3] <- TRUE
    } else {
      step1[i,3] <- FALSE
    }
}

View(step1)
mean(step1[ ,3])

#f)
plot(x = NA, y = NA, xlim = c(7,13), ylim = c(0,100), xlab = "", ylab = "", main = "95% CI")
for(i in 1:100) {
  lines(x = c(step1[i,1], step1[i,2]), y = c(i,i))
}
abline(v = 10, col = "red")