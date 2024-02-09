### Exercise 15.1 ###
#a)
4/52 #ace
1/52 #4 of spades

#b)
(13/52)*(26/51) #Not independent

#c)
(13/52)*(26/52) #Independent

#d)
(4/52)*(26/52) #Not mutually exclusive



### Exercise 15.2 ###
#a)
i. #realization of random variable, discrete
ii. #random variable, discrete/continous
iii. #random variable, discrete
iv. #realization of random variable, continous
v. #realization of random variable, discrete
vi. #realization of random variable, continous

#b)
i. <- 1 - (0.1 + 0.13 + 0.21 + 0.15)

ii_1 <- 0.1
ii_2 <- ii_1 0.13
ii_3 <- ii_2 + 0.21
ii_4 <- ii_3 + i.
ii_5 <- ii_4 + 0.15

ii_vector <- 1:5
ii_vector_2 <- c(0.1, 0.13, 0.21, 0.41, 0.15)
iii. <- sum(ii_vector*ii_vector_2)

var_iv. <- sum((ii_vector_2 - iii.)^2)/5
sd_iv. <- sqrt(var_iv.)

#c)
i. <- function(w) {
  result <- 0
  for(i in 1:length(w)) {
    if(w[i] >= 40 & w[i] <= 65) {
      result[i] <- (w[i]-40)/625
    } else {
      if(w[i] >= 65 & w[i] <= 90) {
        result[i] <- (90-w[i])/625
      } else {
        result[i] <- 0
      }
    }
  }
  return(result)
}
a <- seq(35,95,by=5)
i.(a)

ii. <- function(w) {
  result <- 0
  for(i in 1:length(w)) {
    if(w[i] < 40) {
      result[i] <- 0
    }
    if(w[i] >= 40 & w[i] <= 65) {
      result[i] <- (w[i]^2-80*w[i]+1600)/1250
    }
    if(w[i] >= 65 & w[i] <= 90) {
      result[i] <- (180*w[i]-w[i]^2-6850)/1250
    }
    if(w[i] > 90) {
      result[i] <- 1
    }
  }
  return(result)
}
ii.(a)

i.(55.2)
ii.(55.2)

1 - ii.(60)

ii.(76.89) - ii.(60.3)

#d)
i. #bimodal, symmetrical
ii. #trimodal, positively skewed
iii. #unimodal, symmetrical
iv. #unimodal, positively skewed