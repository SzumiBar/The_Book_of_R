### Exercise 10.1 ###
#a)
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)

if((vec1[1]+vec2[2])==10){ cat("Print me!") }
if(vec1[1]>=2&&vec2[1]>=2){ cat("Print me!") }
if(all((vec2-vec1)[c(2,6)]<7)){ cat("Print me!") }
if(!is.na(vec2[3])){ cat("Print me!") }

#b)
ifelse(test = vec1+vec2 >= 3,
       yes = vec1*vec2,
       no = vec1+vec2)

#c)
mymat <- matrix(as.character(1:16),4,4)
mymat <- matrix(c("DANDELION","Hyacinthus","Gerbera",
                  "MARIGOLD","geranium","ligularia",
                  "Pachysandra","SNAPDRAGON","GLADIOLUS"),3,3)
mymat <- matrix(c("GREAT","exercises","right","here"),2,2,
                byrow=T)

mat_function <- function(mymat) {
  string_vector <- substr(diag(mymat), 
                          start = 1, 
                          stop = 1)
  string_vector_2 <- diag(mymat)
  if(any(string_vector == "g" | string_vector == "G")) {
    diag(mymat)[string_vector == "g" | string_vector == "G"] <- "HERE"
  } else {
    mymat <- diag(dim(mymat)[1])
  }
  print(mymat)
}
newmat <- mat_function(mymat = mymat)



### Exercise 10.2 ###
#a)
mynum <- 3
mynum <- 0

if (mynum == 1) {
  mynum <- 12 } else if (mynum == 2) {
    mynum <- 34} else if (mynum == 3) {
      mynum <- 56} else if (mynum == 4) {
        mynum <- 78} else {
          mynum <- NA
        }

#b)
if (any(doselevel == "High")) {
  if(lowdose >= 10) {
    lowdose <- 10} else {
      lowdose <- lowdose/2
    }
  if(meddose >= 26) {
    meddose <- 26}
  if(highdose < 60) {
    highdose <- 60} else {
      highdose <- highdose*1.5}
  dosage <- rep(lowdose, length(doselevel))
  dosage[doselevel == "Med"] <- meddose
  dosage[doselevel == "High"] <- highdose
} else {
      doselevel <- factor(doselevel, levels = c("Low","Med"), labels = c("Small","Large"))
      if(lowdose < 15 && meddose < 35) {
        lowdose <- lowdose*2
        meddose <- meddose+highdose} 
      dosage <- rep(lowdose, length(doselevel))
      dosage[doselevel == "Large"] <- meddose
}

lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","High","High","High","Low","Med",
                      "Med"),levels=c("Low","Med","High"))
dosage == c(10,60,60,60,10,25.3,25.3)

doselevel <- factor(c("Low","Low","Low","Med","Low","Med",
                      "Med"),levels=c("Low","Med","High"))
dosage == c(25, 25, 25, 83.4, 25, 83.4, 83.4)
doselevel == c("Small", "Small", "Small", "Large", "Small", "Large", "Large")

lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","Med","Med"),
                    levels=c("Low","Med","High"))
dosage == c(9, 49, 49)
doselevel == c("Small", "Large", "Large")

lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","High","High","High","Low","Med",
                      "Med"),levels=c("Low","Med","High"))
dosage == c(4.5, 91.5, 91.5, 91.5, 4.5, 26, 26)

#c)
ifelse(mynum > 0,
       switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),
       "zero")
mynum <- 7



### Exercise 10.3 ###
#a)
loopvec1 <- 5:7
loopvec2 <- 9:6
mymat <- matrix(NaN, nrow = length(loopvec1), ncol = length(loopvec2))
for(i in 1:length(loopvec1)) {
  mymat[i, ] <- loopvec2*loopvec1[i]
  print(mymat)
}

#b)
mystring <- c("Peter","Homer","Lois","Stewie","Maggie","Bart")
for(i in 1:length(mystring)) {
  mystring[i] <- switch(EXPR = mystring[i], Homer=12, Marge=34, Bart=56, Lisa=78, Maggie=90, NA)
}

#c)
answer <- 0
for(i in 1:length(mylist)) {
  if(is.matrix(mylist[[i]])) {
    answer <- answer + 1} else {
    for(j in 1:length(mylist[i][[1]])) {
      if(is.matrix(mylist[i][[1]][[j]])) {
        answer <- answer + 1}
    }
  }
}
print(answer)

mylist <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),
               cc=matrix(c(T,T,F,T,F,F),3,2),dd="string here",
               ee=list(c("hello","you"),matrix(c("hello",
                                                 "there"))),
               ff=matrix(c("red","green","blue","yellow")))

mylist <- list("tricked you",as.vector(matrix(1:6,3,2)))

mylist <- list(list(1,2,3),list(c(3,2),2),
               list(c(1,2),matrix(c(1,2))),
               rbind(1:10,100:91))



### Exercise 10.4 ###
#a)
mylist <- list()
counter <- 1
mycondition <- mynumbers[counter]<=5
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist

mynumbers <- c(2,2,2,2,5,2)
mynumbers <- 2:20
mynumbers <- c(10,1,10,1,2)

#b)
result <- vector(mode = "integer", length = 1)
condition <- is.integer(mynum) && mynum >= 0
while(condition) {
  if(mynum >= 1) {
  sort(cumprod(1:mynum),
       decreasing = T)[1] -> result} else {
         result <- 1
       }
  condition <- FALSE
}
result
condition

mynum <- 5L
mynum <- 12L
mynum <- 0L

#c)
mystring <- "R fever"
index <- 1
ecount <- 0
result <- mystring
while(ecount<2 && index<=nchar(mystring)){
  character <- substr(mystring, index, index)
  if(character == "e" | character == "E") {
    ecount <- ecount + 1
    }
  if(ecount == 2) {
    result <- substr(result, 1, index-1)
    }
  index <- index + 1
  }
result

mystring <- "beautiful"
mystring <- "ECCENTRIC"
mystring <- "ElAbOrAte"
mystring <- "eeeeek!"



### Exercise 10.5 ###
#a)

foo <- matrix(1:12,4,3)
apply(X = apply(X = foo,
                MARGIN = 1,
                FUN = sort, decreasing=TRUE),
      MARGIN = 2,
      FUN = prod)

#b)
matlist <- list(matrix(c(T,F,T,T),2,2),
                matrix(c("a","c","b","z","p","q"),3,2),
                matrix(1:8,2,4))

lapply(X = matlist,
       FUN = t)

#c)
qux <- array(96:1,dim=c(4,4,2,3))

apply(X = qux[ , ,2, ],
      MARGIN = 3,
      FUN = diag)

apply(X = apply(X = qux[ ,4, , ],
                MARGIN = 3,
                FUN = dim),
      MARGIN = 1,
      FUN = sum)



### Exercise 10.6 ###
#a)
foo <- 5
bar <- c(2,3,1.1,4,0,4.1,3)
loop2.result <- rep(NA,length(bar))
x <- 1

while(x <= length(bar)) {
  if(bar[x] != 0) {
    loop2.result[x] <- foo/bar[x]
  } else {
    loop2.result[x] <- NA
  }
  x <- x + 1
}
loop2.result

#b)
mynumbers <- c(4,5,1,2,6,2,4,6,6,2)
mylist <- list()

for(i in 1:length(mynumbers)) {
  if(mynumbers[i] <= 5 ) {
    mylist[[i]] <- diag(mynumbers[i])
  } else {
    break
  }
}
mylist

counter <- 1
repeat{if(mynumbers[counter] <= 5) {
  mylist[[counter]] <- diag(mynumbers[counter])} else {
    break}
  counter <- counter + 1
}
mylist

#c)
matlist1 <- list(matrix(1:4,2,2),matrix(1:4),matrix(1:8,4,2))
matlist2 <- matlist1

matlist1 <- list(matrix(1:4,2,2),matrix(2:5,2,2),
                 matrix(1:16,4,2))
matlist2 <- list(matrix(1:8,2,4),matrix(10:7,2,2),
                 matrix(9:2,4,2))

reslist <- list()
counter <- 1
for(i in 1:length(matlist1)) {
  for(j in 1:length(matlist2)) {
    if(ncol(matlist1[[i]]) == nrow(matlist2[[j]])) {
    reslist[[counter]] <- matlist1[[i]] %*% matlist2[[j]]} else {
      reslist[[counter]] <- "not possible"
    }
    counter <- counter + 1
  }
}
reslist