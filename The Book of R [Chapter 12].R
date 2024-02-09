### Exercise 12.1 ###
#a)
factorial_rec <- function(x) {
  if(x < 0) {
    stop("Argument can't be a negative number")
  }
  if(x == 0) {
    return(1)
  } else {
    result <- x*factorial_rec(x-1)
    return(result)
  }
}

factorial_rec(5)
factorial_rec(8)
factorial_rec(-8)

#b)
invert_mat <- function(x, noninv = NA, nonmat = "not a matrix", silent = T) {
  if(!is.list(x)) {
    stop("x is not a list")
  }
  if(length(x) < 1) {
    stop("x has no members")
  }
  if(!is.character(nonmat)) {
    warning("nonmat is not a character string\nnonmat changed to character string")
    nonmat <- as.character(nonmat)
  }
  for(i in 1:length(x)) {
    current <- x[[i]]
    if(is.matrix(current)) {
      test <- try(solve(current))
      if(class(test) != "try-error") {
        x[[i]] <- test
      } else {
        x[[i]] <- noninv
      }
    } else {
      x[[i]] <- nonmat
    }
  }
  return(x)
}

i. <- list(1:4,matrix(1:4,1,4),matrix(1:4,4,1),matrix(1:4,2,2))
invert_mat(x = i.)
invert_mat(x = i., noninv = Inf, nonmat = 666)
invert_mat(x = i., noninv = Inf, nonmat = 666, silent = F)

iv. <- list(diag(9),matrix(c(0.2,0.4,0.2,0.1,0.1,0.2),3,3),
            rbind(c(5,5,1,2),c(2,2,1,8),c(6,1,5,5),c(1,0,2,0)),
            matrix(1:6,2,3),cbind(c(3,5),c(6,5)),as.vector(diag(2)))
invert_mat(x = iv., noninv = "unsuitable matrix")
invert_mat(x = "hello")
invert_mat(x = list())



### Exercise 12.2 ###
#a)
prog_test_fancy <- function(n, ...){
  result <- 0
  progbar <- txtProgressBar(min=0,max=n, ...)
  for(i in 1:n){
    result <- result + 1
    Sys.sleep(0.5)
    setTxtProgressBar(progbar,value=i)
  }
  close(progbar)
  return(result)
}

t1 <- Sys.time()
prog_test_fancy(n = 50, style = 3, char = "r")
t2 <- Sys.time()
t2-t1

#b)
myfibvectorTRY <- function(nvec){
  myfibrec2 <- function(n) {
    if (n < 0) {
      warning("Assuming you meant 'n' to be positive -- doing that instead")
      n <- n * -1
    } else if (n == 0) {
      stop("'n' is uninterpretable at 0")
    }
    if (n == 1 || n == 2) {
      return(1)
    } else{
      return(myfibrec2(n - 1) + myfibrec2(n - 2))
    }
  }
  nterms <- length(nvec)
  result <- rep(0,nterms)
  progbar <-txtProgressBar(label = "Progress",
                           max = nterms,
                           style = 3,
                           char = "...")
  for(i in 1:nterms){
    attempt <- try(myfibrec2(nvec[i]),silent=T)
    if(class(attempt)=="try-error"){
      result[i] <- NA
    } else {
      result[i] <- attempt
    }
    #Sys.sleep(0.25)
    setTxtProgressBar(progbar, value = i)
  }
  close(progbar)
  return(result)
}

myfibvectorTRY(nvec = c(3,2,7,0,9,13))

t1 <- Sys.time()
myfibvectorTRY(1:35)
t2 <- Sys.time()
t2-t1

#c)
t1 <- Sys.time()
fibvec <- c(1,1,rep(NA,33)) 
for(i in 3:35){
  fibvec[i] <- fibvec[i-2]+fibvec[i-1]
}
fibvec
t2 <- Sys.time()
t2-t1