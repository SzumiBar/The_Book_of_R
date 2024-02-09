### Exercise 5.1 ###
#a)
quest_a <- list(seq(from = -4, to = 4, length.out = 20),
                matrix(c(F,T,T,T,F,T,T,F,F), ncol = 3, byrow = FALSE),
                c("don","quixote"),
                factor(c("LOW","MED","LOW","MED","MED","HIGH"), levels = c("LOW", "MED","HIGH")))

i. <- quest_a[[2]][c(2,1), c(2,3)]

ii. <- quest_a
ii.[[3]] <- sub(x = ii.[[3]],
                pattern = "don",
                replacement = "Don")
ii.[[3]] <- sub(x = ii.[[3]],
                pattern = "quixote",
                replacement = "Quixote")

cat("Windmills! ATTACK!\n\t-\\",
    ii.[[3]][1],
    " ",
    ii.[[3]][2],
    "/-")

iii. <- quest_a[[1]][quest_a[[1]] > 1]

#b)
quest_b <- list(quest_a[[4]],
                c(3,2.1,3.3,4,1.5,4.9),
                list(quest_a[[1]],
                     quest_a[[2]],
                     quest_a[[3]]))
names(quest_b) <- c("facs", "nums", "oldlist")
i. <- quest_b$facs[quest_b$nums >= 3]
quest_b$flags <- rep(quest_b$oldlist[[2]][ ,3], times = 2)
iii. <- quest_b$num[which(quest_b$flags == FALSE)]
quest_b$oldlist[[3]] <- "Don Quixote"



### Exercise 5.2 ###
#a)
quest_a <- data.frame(person = c("Stan","Francine", "Steve","Roger","Hayley","Klaus"),
                      sex = factor(c("M","F","M","M","F","M"),
                                   levels = c("M","F")),
                      funny = factor(c("High","Med","Low","High","Med","Med"),
                                     levels = c("Low","Med","High")),
                      stringsAsFactors = FALSE)

#b)
age <- c(41,41,15,1600,21,60)
quest_b <- quest_a
quest_b$age <- age

#c)
quest_c <- quest_b[c(1,4,2,3)]

#d)
newrecord <- data.frame(person="Brian",age=7,
                        sex=factor("M",levels=levels(mydata$sex)))
mydata <- data.frame(person=c("Peter","Lois","Meg","Chris","Stewie"),
                     age=c(42,40,17,14,1),
                     sex=factor(c("M","F","F","M","M")))
mydata <- rbind(mydata,newrecord)
mydata$age.mon <- mydata$age*12
quest_d <- mydata[ ,-4]

#e)
quest_e <- cbind(quest_d, quest_a)
quest_e <- quest_e[ ,unique(colnames(quest_e))]

#f)
quest_f <- quest_e[quest_e$funny == "Med" | quest_e$funny == "High",1:2]

#g)
quest_g <- quest_e[substring(quest_e$person,
                   first = 1,
                   last = 1) == "S", ]