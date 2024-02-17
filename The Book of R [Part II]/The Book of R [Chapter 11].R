### Exercise 11.1 ###
#a)
myfib4 <- function(thresh, printme) {
  if(printme == TRUE) {
    fib.a <- 1
    fib.b <- 1
    cat(fib.a,", ",fib.b,", ",sep="")
    repeat{
      temp <- fib.a+fib.b
      fib.a <- fib.b
      fib.b <- temp
      cat(fib.b,", ",sep="")
      if(fib.b>thresh){
        cat("BREAK NOW...")
        break
      }
    }
  } else {
    fibseq <- c(1,1)
    counter <- 2
    repeat{
      fibseq <- c(fibseq,fibseq[counter-1]+fibseq[counter])
      counter <- counter+1
      if(fibseq[counter]>thresh){
        break
      }
    }
    return(fibseq)
  }
}

myfib4(thresh=150,printme=TRUE)
myfib4(1000000,T)
myfib4(150,FALSE)
myfib4(1000000,printme=F)

#b)
myfac <- function(int) {
  result <- vector(mode = "double", length = 1)
  condition <- int >= 0
  while(condition) {
    if(int >= 1) {
      sort(cumprod(1:int),
           decreasing = T)[1] -> result} else {
             result <- 1
           }
    condition <- FALSE
  }
  return(result)
}
myfac(4)
myfac(0)
myfac(12)

myfac2 <- function(int) {
  if(int >= 0) {
    result <- vector(mode = "double", length = 1)
    condition <- int >= 0
    while(condition) {
      if(int >= 1) {
        sort(cumprod(1:int),
             decreasing = T)[1] -> result} else {
               result <- 1
             }
      condition <- FALSE
    }
    return(result)
  } else {
   return(NaN) 
  }
}
myfac2(-5)
myfac(5)



### Exercise 11.2 ###
#a)
compound_int <- function(P, i, t = 12, y, plotit = TRUE, ...) {
  F_value <- vector("numeric", 0)
  Y <- vector("numeric", 0)
  if(plotit == T) {
    for(n in 1:y) {
      F_value <- c(F_value, P*(1+i/(100*t))^(t*n))
      Y <- c(Y, n)
    }
    plot(Y, F_value, type = "s", ...)
  } else {
    for(n in 1:y) {
      F_value <- c(F_value, P*(1+i/(100*t))^(t*n))
    }
    return(F_value)
  }
}

compound_int(P = 5000, y = 10, i = 4.4, t = 12, plotit = F)[length(compound_int(P = 5000, y = 10, i = 4.4, t = 12, plotit = F))] #i.

compound_int(P = 100, y = 20, i = 22.9, main = "Compount interest calculator", ylab = "Balance (F)", xlab = "Year (y)") #ii.

iii. <- compound_int(P = 100, y = 20, i = 22.9, t = 1, plotit = F)
lines(iii., type = "s", col = "green")
legend("topleft",
       legend = c("Compounded monthly", "Compounded annualy"),
       pch = 20,
       col = c("black", "green"))

#b)
quadratic_fun <- function(a = 1, b = 1, c = 1) {
  test <- c(missing(a), missing(b), missing(c))
  if(any(test) == T) {
    cat("No arguments for constants\nDefault values of 1 are supplied for missing constants\n\n")
  }
  imaginary_test <- b^2 - 4*a*c
  if(imaginary_test >= 0) {
    if(imaginary_test == 0) {
      solution <- (-b)/(2*a)
      return(cat("There is one real root:", solution))
    } else {
      pos_solution <- ((-b)+sqrt(b^2-4*a*c))/(2*a)
      neg_solution <- ((-b)-sqrt(b^2-4*a*c))/(2*a)
      return(cat("There are two real roots:", pos_solution, neg_solution))
    }
  } else {
    return("There are no real roots")
  }
}

quadratic_fun(a = 2, b = (-1), c = (-5))
quadratic_fun(a = 1, b = 1, c = 1)
quadratic_fun(a = 1.3, b = (-8), c = (-3.13))
quadratic_fun(a = 2.25, b = (-3), c = 1)
quadratic_fun(a = 1.4, b = (-2.2), c = (-5.1))
quadratic_fun(a = (-5), b = 10.11, c = (-9.9))
quadratic_fun()



### Exercise 11.3 ###
#a)
foo <- list("a",c("b","c","d","e"),"f",c("g","h","i"))

lapply(X = foo, FUN = function(X) {
  paste(X, "!", sep = "")
})

#b)
factorial_rec <- function(x) {
  if(x < 0) {
    return(cat("Argument can't be a negative number"))
  }
  if(x == 0) {
    return(1)
  } else {
    result <- x*factorial_rec(x-1)
    return(result)
  }
}

factorial_rec(0)
factorial_rec(5)
factorial_rec(12)
factorial_rec(-12)

#c)
geolist <- function(num_list) {
  #
  matrix_substitute <- NULL
  #
  for(i in 1:length(num_list)) {
    if(!is.numeric(num_list[[i]])) {
      return(cat("List contains non-numeric members"))
    }
  }
  #
  geo_mean <- function(y) {
    result <- cumprod(y)[length(y)]^(1/length(y))
    return(result)
  }
  #
  for(i in 1:length(num_list)) {
    if(is.vector(num_list[[i]])) {
      num_list[[i]] <- geo_mean(num_list[[i]])
    }
    if(is.matrix(num_list[[i]])) {
      for(n in 1:nrow(num_list[[i]])) {
        result <- geo_mean(num_list[[i]][n, ])
        matrix_substitute[n] <- result
      }
      names(matrix_substitute) <- paste("Geo_mean of matrix row number:", 1:length(matrix_substitute))
      num_list[[i]] <- matrix_substitute
      matrix_substitute <- NULL
    }
  }
  #
  return(num_list)
}

foo <- list(1:3,matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),4,2),
            matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),2,4))
geolist(foo)

bar <- list(1:9,matrix(1:9,1,9),matrix(1:9,9,1),matrix(1:9,3,3))
geolist(bar)