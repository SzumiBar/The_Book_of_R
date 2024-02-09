### Exercise 6.1 ###
#a)
foo <- c(13563,-14156,-14319,16981,12921,11979,9568,8833,-12968,8133)
i. <- foo[!is.infinite(foo^75)]
ii. <- foo[-which(foo^75 == -Inf)]

#b)
matrix_vector <- c(77875.40, 27551.45, 23764.30, -36478.88, -35466.25, -73333.85, 36599.69, -70585.69, -39803.81, 55976.34, 76694.82, 47032.00)
bar <- matrix(matrix_vector,
              ncol = 4,
              byrow = TRUE)

i. <- which(is.nan(bar^65/Inf),
            arr.ind = TRUE)

ii. <- bar[which(!is.nan(bar^67+Inf))]
all(ii. == bar[which(bar^67 != -Inf)])

iii. <- bar[which(bar^67 == -Inf | is.finite(bar^67))]



### Exercise 6.2 ###
#a)
foo <- c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)
length(foo) == 8
which(is.na(foo)) 
is.null(foo)
is.na(foo[8])+4/NULL

#b)
quest_b <- list(c(7,7,NA,3,NA,1,1,5,NA))
names(quest_b) <- "alpha"
is.null(quest_b$beta)
quest_b$beta <- which(is.na(quest_b$alpha))



### Exercise 6.3 ###
#a)
foo <- array(data=1:36,dim=c(3,3,4))
class(foo)

bar <- as.vector(foo)
class(bar)

baz <- as.character(bar)
class(baz)

qux <- as.factor(baz)
class(qux)

quux <- bar+c(-0.1,0.1)
class(quux)

#b)
i. <- is.numeric(foo)+is.integer(foo)
ii. <- is.numeric(bar)+is.integer(bar)
iii. <- is.numeric(baz)+is.integer(baz)
iv. <- is.numeric(qux)+is.integer(qux)
v. <- is.numeric(quux)+is.integer(quux)

quest_b <- factor(c(i.,
                    ii.,
                    iii.,
                    iv.,
                    v.))
quest_b2 <- as.numeric(quest_b)

#c)
quest_c <- matrix(2:13, nrow = 3, byrow = F)
as.character(t(quest_c))

#d)
quest_d <- matrix(c(34,0,1,23,1,2,33,1,1,42,0,1,41,0,2),
                  ncol = 3,
                  byrow = T)
i. <- as.data.frame(quest_d)
ii. <- i.
ii.[ ,2] <- as.logical(ii.[ ,2])
iii. <- ii.
iii.[ ,3] <- as.factor(iii.[ ,3])