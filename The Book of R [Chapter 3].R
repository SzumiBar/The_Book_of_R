### Exercise 3.1 ###
#a)
quest_a <- matrix(c(4.3, 3.1, 8.2, 8.2, 3.2, 0.9, 1.6, 6.5),
                  nrow = 4,
                  ncol = 2,
                  byrow = TRUE)

#b)
dim(quest_a[-1, ])

#c)
quest_c <- quest_a
quest_c[ ,2] <- sort(quest_a[ ,2], decreasing = FALSE)

#d)
quest_d <- quest_c[-4,-1]
as.matrix(quest_d)

#e)
quest_e <- quest_c[-c(1,2), ]

#f)
quest_f <- quest_c
quest_f[c(4,1), c(2,1)] <- diag(quest_c)*(-0.5)



### Exercise 3.2 ###
#a)
matrix_A <- cbind(c(1,2,7), c(2,4,6))
matrix_B <- matrix(seq(from = 10, to = 60, by = 10),
                   ncol = 2,
                   nrow = 3,
                   byrow = TRUE)
matrix_C <- 2/7*(matrix_A - matrix_B)

#b)
matrix_A <- matrix(c(1,2,7))
matrix_B <- matrix(c(3,4,8))

i. matrix_A%*%matrix_B #Impossible
ii. <- t(matrix_A)%*%matrix_B
iii. <- t(matrix_B)%*%(matrix_A%*%t(matrix_A))
iv. <- (matrix_A%*%t(matrix_A))%*%t(matrix_B) #Impossible
v. <- ((matrix_B%*%t(matrix_B))+(matrix_A%*%t(matrix_A))-100*diag(3))^(-1)

#c)
quest_c <- cbind(c(2,0,0,0), c(0,3,0,0), c(0,0,5,0), c(0,0,0,-1))
solve(quest_c)%*%quest_c-diag(4)



### Exercise 3.3 ###
#a)
quest_a <- array(1:48, dim = c(4,2,6))

#b)
quest_b <- quest_a[c(4,1),2, ]

#c)
quest_c <- array(rep(quest_b[2, ], times = 4), dim = c(2,2,2,3))

#d)
quest_d <- quest_a[ , ,-6]

#e)
quest_e <- quest_d
quest_e[c(2,4),2,c(1,3,5)] <- -99