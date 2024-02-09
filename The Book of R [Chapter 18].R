### Exercise 18.1 ###
#a)
(3.97-3.5)/(2.21/sqrt(73)) #1.82
qt(p = 0.975, df = 73) #1.99, H0 not rejected

#b)
t.test(x = quakes$mag, mu = 4.3, alternative = "greater", conf.level = 0.01)

#c)
mean(quakes$mag) + c(-1,1)*qt(p = 0.975, df = 999)*sd(quakes$mag)



### Exercise 18.2 ###
#a)
library(MASS)
t.test(x = anorexia$Postwt, y = anorexia$Prewt, paired = TRUE, conf.level = 0.95)

#b)
t.test(x = anorexia$Postwt[anorexia$Treat == "Cont"], y = anorexia$Prewt[anorexia$Treat == "Cont"], paired = TRUE, conf.level = 0.95)
t.test(x = anorexia$Postwt[anorexia$Treat == "CBT"], y = anorexia$Prewt[anorexia$Treat == "CBT"], paired = TRUE, conf.level = 0.95)
t.test(x = anorexia$Postwt[anorexia$Treat == "FT"], y = anorexia$Prewt[anorexia$Treat == "FT"], paired = TRUE, conf.level = 0.95)

#c) & d)
sd(PlantGrowth$weight[PlantGrowth$group == "ctrl"])/sd(PlantGrowth$weight[PlantGrowth$group == "trt1"]) # sd1/sd2 < 2
sd(PlantGrowth$weight[PlantGrowth$group == "ctrl"])/sd(PlantGrowth$weight[PlantGrowth$group == "trt2"]) # sd1/sd2 < 2

t.test(x = PlantGrowth$weight[PlantGrowth$group == "ctrl"], y = PlantGrowth$weight[PlantGrowth$group == "trt1"], var.equal = T)
t.test(x = PlantGrowth$weight[PlantGrowth$group == "ctrl"], y = PlantGrowth$weight[PlantGrowth$group == "trt2"], var.equal = T)

#e)
quest_e <- function(x, y, var.equal = F, paired = F, ...) {
  if(paired == F) {
    if(max(c(sd(x), sd(y))) / min(c(sd(x), sd(y))) < 2) {
      var.equal = T
    } else {
      var.equal = F
    }
  }
  return(t.test(x = x, y = y, var.equal = var.equal, paired = paired, ...))
}

#f)
snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,
            75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,77.18,
            79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,78.54,79.7,
            82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)
snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,79.21,
             81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,79.08,79.47,
             78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)
quest_e(snacks2,snacks,alternative="greater", conf.level = 0.9)

men <- c(102,87,101,96,107,101,91,85,108,67,85,82)
women <- c(73,81,111,109,143,95,92,120,93,89,119,79,90,126,62,92,77,106,105,111)
quest_e(men,women,alternative="two.sided")

rate.before <- c(52,66,89,87,89,72,66,65,49,62,70,52,75,63,65,61) 
rate.after <- c(51,66,71,73,70,68,60,51,40,57,65,53,64,56,60,59) 
quest_e(rate.after,rate.before,alternative="less",paired=TRUE)



### Exercise 18.3 ###
#a) & b)
prop.test(x = 71, n = 89, p = 0.9, conf.level = 0.9, correct = FALSE)

#c)
71/89 + c(-1,1)*qnorm(p = 0.95)*sqrt((71/89)*(1-71/89)/89)

#d) & e)
prop.test(x = c(97, 90), n = c(445, 419), correct = F)

#f)
Z.test <- function(p1, n1, p2 = NULL, n2 = NULL, p0, alternative = "two.sided", conf.level = 0.95) {
  Z <- NULL
  P <- NULL
  CI <- NULL
  #Two-sample: Z, CI, warning
  if(!is.null(p2) | !is.null(n2)) {
    cat("Two-sample Z test for proportion\n")
    pooled.p <- ((n1*p1)+(n2*p2))/(n1+n2)
    Z <- (p1-p2)/sqrt((pooled.p*(1-pooled.p))*(1/n1 + 1/n2))
    CI <- (p1-p2) + c(-1,1)*qnorm(p = conf.level + (1-conf.level)/2)*sqrt(pooled.p*(1-pooled.p)*(1/n1 + 1/n2))
    if(p1*n1 < 5 || (1-p1)*n1 < 5 || p2*n2 < 5 || (1-p2)*n2 < 5) {
      warning("Yates continuity correction should be applied\n")
    }} else {
      #One-sample: Z, CI, warning
       cat("One-sample Z test for proportion\n")
       Z <- (p1-p0)/sqrt((p0*(1-p0))/n1)
       CI <- p1 + c(-1,1)*qnorm(p = conf.level + (1-conf.level)/2)*sqrt(p0*(1-p0)/n1)
       if(p1*n1 < 5 | (1-p1)*n1 < 5) {
         warning("Yates continuity correction should be applied\n")
       }
    }
  #P-value
  P.area <- pnorm(q = Z)
  if(alternative == "greater") {
    P <- 1-P.area
  }
  if(alternative == "less") {
    P <- P.area
  }
  if(alternative == "two.sided") {
    if(Z > 0) {
      P <- 2*(1-P.area)
    } else {
      P <- 2*P.area
    }
  }
  cat("Z score: ", Z, "\nConfidence interval: ", CI, "\nP-value: ", P, "\nList of test objects:\n")
  return(list(Z = Z, CI = CI, P = P))
}

#g)
sick <- c(0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1)
x1 <- sum(sick)
n1 <- length(sick)
p1 <- x1/n1
prop.test(x = x1, n = n1, p = 0.2, correct = FALSE)
Z.test(p1 = p1, n1 = n1, p0 = 0.2)

x1 <- 180
n1 <- 233
p1 <- x1/n1
x2 <- 175
n2 <- 197
p2 <- x2/n2
prop.test(x = c(x2,x1), n = c(n2,n1), alternative = "greater", correct = FALSE)
Z.test(p1 = p2, n1 = n2, p2 = p1, n2 = n1, alternative = "greater")

#h)
Z.test(p1 = 0.11, n1 = 10, p0 = 0.1)



### Mno¿enie matryc - wykorzystywane w teœcie niezale¿noœci Chi-kwadrat ###
skin <- matrix(c(20,32,8,52,9,72,8,32,16,64,30,12),4,3,
               dimnames=list(c("Injection","Tablet","Laser","Herbal"),
                             c("None","Partial","Full")))

matrixA <- matrix(data = rowSums(skin), nrow = 4, dimnames = list(c("Injection","Tablet","Laser","Herbal")))
matrixB <- matrix(data = colSums(skin), nrow = 1, dimnames = list(NULL, c("None","Partial","Full")))

matrixC <- matrixA%*%matrixB
skin.expected <- matrixC/sum(skin)

skin
matrixA
matrixB
matrixC
skin.expected # 1-szy rz¹d: 45*(112/(112+121+122)) = 14.19, 45*(121/(112+121+122)) = 15.33 ,45*(122/(112+121+122)) = 15.46



### Exercise 18.4 ###
#a)
HairEyeColor[ , ,1] + HairEyeColor[ , ,2] -> quest_a
chisq.test(x = quest_a)

#b)
library(car)
library(tidyverse)

Duncan %>%
  count(type) -> quest_b # Duncan %>% group_by(type) %>% summarize(n = n())

quest_b <- as.matrix(x = quest_b[ ,2])
dimnames(quest_b) <- list(c("Blue collar", "Proffesional", "White collar"), "Type")

chisq.test(quest_b) # 0.05 > p > 0.01



### Exercise 18.5 ###
#a)
typeI.mean <- function(mu0, sigma, n, alpha, ITERATIONS = 10000, test){
  statistics <- rep(NA, ITERATIONS)
  pvals <- rep(NA, ITERATIONS)
  for(i in 1:ITERATIONS){
    temporary.sample <- rnorm(n = n, mean = mu0, sd = sigma)
    temporary.mean <- mean(temporary.sample)
    temporary.sd <- sd(temporary.sample)
    statistics[i] <- (temporary.mean-mu0)/(temporary.sd/sqrt(n))
    if(test == "two.sided") {
      if(statistics[i] > 0) {
        pvals[i] <- 2*(1-pt(q = statistics[i], df = n-1))
      } else {
        pvals[i] <- 2*pt(q = statistics[i], df = n-1)
      }
    }
    if(test == "less") {
      pvals[i] <- pt(statistics[i], df = n-1)
    }
    if(test == "greater") {
      pvals[i] <- 1-pt(statistics[i], df = n-1)
    }
  }
  if(test != "two.sided" && test != "less" && test != "greater") {
    stop("Argument 'test' needs appropriate value")
  }
  return(mean(pvals<alpha))
}

typeI.mean(mu0 = 0, sigma = 1, n = 40, alpha = 0.05, test = "less")
typeI.mean(mu0 = 0, sigma = 1, n = 40, alpha = 0.05, test = "greater")
typeI.mean(mu0 = 0, sigma = 1, n = 40, alpha = 0.05, test = "two.sided")

typeI.mean(mu0 = -4, sigma = 0.3, n = 60, alpha = 0.01, test = "less")
typeI.mean(mu0 = -4, sigma = 0.3, n = 60, alpha = 0.01, test = "greater")
typeI.mean(mu0 = -4, sigma = 0.3, n = 60, alpha = 0.01, test = "two.sided")

#b)
typeII.mean <- function(mu0, muA, sigma, n, alpha, ITERATIONS=10000, test){
  statistics <- rep(NA, ITERATIONS)
  pvals <- rep(NA,ITERATIONS)
  for(i in 1:ITERATIONS){
    temporary.sample <- rnorm(n = n, mean = muA, sd = sigma)
    temporary.mean <- mean(temporary.sample)
    temporary.sd <- sd(temporary.sample)
    statistics[i] <- (temporary.mean-mu0)/(temporary.sd/sqrt(n))
    #pvals[i] <- 1-pt((temporary.mean-mu0)/(temporary.sd/sqrt(n)),df=n-1)
    if(test == "two.sided") {
      if(statistics[i] > 0) {
        pvals[i] <- 2*(1-pt(q = statistics[i], df = n-1))
      } else {
        pvals[i] <- 2*pt(q = statistics[i], df = n-1)
      }
    }
    if(test == "less") {
      pvals[i] <- pt(statistics[i], df = n-1)
    }
    if(test == "greater") {
      pvals[i] <- 1-pt(statistics[i], df = n-1)
    }
  }
  if(test != "two.sided" && test != "less" && test != "greater") {
    stop("Argument 'test' needs appropriate value")
  }
  return(mean(pvals>=alpha))
}

typeII.mean(mu0 = -3.2, muA = -3.3, sigma = 0.1, alpha = 0.05, n = 25, test = "two.sided")
typeII.mean(mu0 = 8994, muA = 5600, sigma = 3888, alpha = 0.01, n = 9, test = "less")
typeII.mean(mu0 = 0.44, muA = 0.4, sigma = 2.4, alpha = 0.05, n = 68, test = "greater")



### Exercise 18.6 ###
#a)
power.mean <- function(nvec,...){
  nlen <- length(nvec)
  result <- rep(NA,nlen)
  pbar <- txtProgressBar(min=0,max=nlen,style=3)
  for(i in 1:nlen){
    result[i] <- 1-typeII.mean(n=nvec[i],...)
    setTxtProgressBar(pbar,i)
  }
  close(pbar)
  return(result)
}

i. <- 1-typeII.mean(mu0 = 10, muA = 10.5, sigma = 0.9, alpha = 0.01, n = 50, test = "two.sided")
ii.1 <- power.mean(nvec = 44, mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.05, test = "less")
ii.2 <- power.mean(nvec = 44, mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.01, test = "less")

#b)
sample.sizes <- 5:100

power.mean(nvec = sample.sizes, mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.05, test = "less") -> A
sample.sizes[min(which(A > 0.8))] -> A.2

power.mean(nvec = sample.sizes, mu0 = 80, muA = 78.5, sigma = 3.1, alpha = 0.01, test = "less") -> B
sample.sizes[min(which(B > 0.8))] -> B.2

plot(x = sample.sizes, y = A, ylim = c(0,1), main = "Power curves", xlab = "N", ylab = "Power", pch = 20, col = "red")
points(x = sample.sizes, y = B, pch = 20, col = "blue")
abline(h = 0.801, v = A.2, lty = 3, col = "red")
abline(h = 0.799, v = B.2, lty = 3, col = "blue")
legend(x = "bottomright", legend = c("alpha = 0.05", "alpha = 0.01"), col = c("red", "blue"), pch = 20, title = "Legend")