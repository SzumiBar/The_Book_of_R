### Exercise 4.1 ###
#a)
quest_a <- c(6,9,7,3,6,7,9,6,3,6,6,7,1,9,1)
i. <- quest_a == 6
ii. <- quest_a >= 6
iii. <- quest_a < 6+2
iv. <- quest_a != 6

#b)
quest_b <- quest_a[-(1:3)]
quest_b <- array(quest_b, dim = c(2,2,3))
i. <- quest_b <= 6/2+4
ii. <- 2*quest_b <= 6/2+4

#c)
quest_c <- diag(10)
quest_c == 0

#d)
any(i.)
all(i.)
any(ii.)
all(ii.)

#e)
any(diag(quest_c == 0))



### Exercise 4.2 ###
#a)
quest_a <- c(7,1,7,10,5,9,10,3,10,8)
quest_a2 <- quest_a > 5 | quest_a == 2

#b)
quest_b <- c(8,8,4,4,5,1,5,6,6,8)
quest_b2 <- quest_b <= 6 & quest_b != 4

#c)
quest_c <- quest_a2 & quest_b2

#d)
quest_d <- quest_a + quest_b
i. <- quest_d >= 14 & quest_d != 15
ii. <- quest_a/quest_b > 4 | quest_a/quest_b <=2

#e)
(quest_a > 5 || quest_a == 2) == quest_a2[1]
(quest_b <= 6 && quest_b != 4) == quest_b2[1]
(quest_a2 && quest_b2) == quest_c[1]
(quest_d >= 14 && quest_d != 15) == i.[1]
(quest_a/quest_b > 4 || quest_a/quest_b <=2) == ii.[1]



### Exercise 4.3 ###
#a)
quest_a <- c(7,5,6,1,2,10,8,3,8,2)
i. <- quest_a[quest_a >= 5]
ii. <- quest_a[-(which(quest_a >= 5))]

#b)
quest_b <- matrix(i., nrow = 3, ncol = 2)
i. <- quest_b
i.[i. == 8] <- i.[1,2]^2
ii. <- all(i.[i. <= 25 & i. > 4])

#c)
quest_c <- array(c(10,5,1,4,7,4,3,3,1,3,4,3,1,7,8,3,7,3),
                 dim = c(3,2,3))
i. <- which(quest_c == 3 | quest_c == 4, arr.ind = TRUE)
ii. <- quest_c
ii.[ii. < 3 | ii. >= 7] <- 100

#d)
vector <- c(F, T)
quest_d <- quest_a[vector]
vector_2 <- c(0,1)
quest_d <- quest_a[vector_2] #does not recycle through vector



### Exercise 4.4 ###
#a)
quest_a <- "The quick brown fox\n\tjumped over\n\t\tthe lazy dogs"
cat(quest_a)

#b)
num1 <- 4
num2 <- 0.75

quest_b <- paste("The result of multiplying",
                 num1,
                 "by",
                 num2,
                 "is",
                 num1*num2,
                 sep = " ")
cat(quest_b)

#d)
bar <- "How much wood could a woodchuck chuck"
i. <- paste(bar, "if a woodchuck could chuck wood")
ii. <- gsub(i., pattern = "wood", replacement = "metal")

#e)
quest_e <- "Two 6-packs for $12.99"
i. <- substr(quest_e, start = 5, stop = 10) == "6-pack"
ii. <- sub(quest_e, pattern = "12.99", replacement = "10.99")



### Exercise 4.5 ###
#a)
sex <- vector(mode = "character", length = 20)
sex[c(1,5:7,12,14:16)] <- "F"
sex[-c(1,5:7,12,14:16)] <- "M"

party <- vector(mode = "character", length = 20)
party[c(1,4,12,15,16,19)] <- "Labour"
party[c(6,9,11)] <- "Greens"
party[c(10,20)] <- "Other"
party[party == ""] <- "National"

#b)
factor(sex, levels = c("M", "F"))
factor(party, levels = c("National","Labour", "Greens", "Maori", "Other"))

#c)
i. <- party[sex == "M"]
ii. <- sex[party == "National"]

#d)
new_party <- c("National","Maori","Maori","Labour","Greens","Labour") 
new_sex <-  c("M","M","F","F","F","M")

quest_d_party <- c(party,new_party)
quest_d_sex <- c(sex,new_sex)

#e)
confidence <- c(93, 55, 29, 100, 52, 84, 56, 0, 33, 52, 35, 53, 55, 46, 40, 40, 56, 45, 64, 31, 10, 29, 40, 95, 18, 61)
cut(confidence,
    breaks = c(0,30,70,100),
    labels = c("Low","Moderate","High"),
    right = T,
    include.lowest = T) -> confidence_factor

#f)
quest_f1 <- confidence_factor[quest_d_party == "Labour"]
quest_f2 <- confidence_factor[quest_d_party == "National"]