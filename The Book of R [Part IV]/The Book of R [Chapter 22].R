### Exercise 22.1 ###
#a)
library("boot")
View(nuclear)

model_a1 <- lm(formula = cost ~ 1, data = nuclear)
step_a1 <- step(object = model_a1, scope = . ~ . + date + t1 + t2 + cap + pr + ne + ct + bw + cum.n + pt)
summary(step_a1)

#c)
Galileo <- data.frame(Init.height = c(1000,800,600,450,300,200,100),
                      Dist = c(573,534,495,451,395,337,257))


mod_c0 <- lm(formula = Dist ~ 1, data = Galileo)
mod_c1 <- lm(formula = Dist ~ Init.height, data = Galileo)
mod_c2 <- lm(formula = Dist ~ Init.height + I(Init.height^2), data = Galileo)
mod_c3 <- lm(formula = Dist ~ Init.height + I(Init.height^2) + I(Init.height^3), data = Galileo)
mod_c4 <- lm(formula = Dist ~ Init.height + I(Init.height^2) + I(Init.height^3) + I(Init.height^4), data = Galileo)
summary(mod_c0)
summary(mod_c1)
summary(mod_c2)
summary(mod_c3)
summary(mod_c4)

#d)
anova(mod_c0, mod_c1, mod_c2, mod_c3, mod_c4)

#e)
install.packages("faraway")
library("faraway")
head(diabetes)

diab_new <- diabetes[ - which(is.na(diabetes$chol) | 
                                is.na(diabetes$age) | 
                                is.na(diabetes$gender) | 
                                is.na(diabetes$height) | 
                                is.na(diabetes$weight) | 
                                is.na(diabetes$frame) | 
                                is.na(diabetes$waist) | 
                                is.na(diabetes$hip) | 
                                is.na(diabetes$location)), ]

#f)
dia.null <- lm(formula = chol ~ 1, data = diab_new)
dia.full <- lm(formula = chol ~ age*gender*weight*frame + waist*height*hip + location, data = diab_new)
summary(dia.null)
summary(dia.full)

#g)
step_g <- step(object = dia.null, scope = . ~ . + age*gender*weight*frame + waist*height*hip + location)
summary(step_g)

#h)
forw_sel0 <- add1(object = dia.null, scope = . ~ . + age*gender*weight*frame + waist*height*hip + location, test = "F")
forw_sel0
dia.1 <- update(object = dia.null, formula. = . ~ . + age)

forw_sel1 <- add1(object = dia.1, scope = . ~ . + age*gender*weight*frame + waist*height*hip + location, test = "F")
forw_sel1
dia.2 <- update(object = dia.1, formula. = . ~ . + frame)

forw_sel2 <- add1(object = dia.2, scope = . ~ . + age*gender*weight*frame + waist*height*hip + location, test = "F")
forw_sel2
summary(dia.2)

#i)
back_sel0 <- drop1(object = dia.full, test = "F")
back_sel0
back_1 <- update(object = dia.full, formula. = . ~ . - location)

back_sel1 <- drop1(object = back_1, test = "F")
back_sel1
back_2 <- update(object = back_1, formula. = . ~ . - age:gender:weight:frame)

back_sel2 <- drop1(object = back_2, test = "F")
back_sel2 #...

model_i <- step(object = dia.full, test = "F")
summary(model_i)

#j)
test_j0 <- lm(formula = mpg ~ 1, data = mtcars)
model_j0 <- step(object = test_j0, scope = . ~ . + wt*hp*factor(cyl)*disp + am + factor(gear) + drat + vs + qsec + carb)
summary(model_j0)

test_j1 <- lm(formula = I(1/mpg) ~ 1, data = mtcars)
model_j1 <- step(object = test_j1, scope = . ~ . + wt*hp*factor(cyl)*disp + am + factor(gear) + drat + vs + qsec + carb)
summary(model_j1)



### Exercise 22.2 ###
#a)
head(boot::nuclear)
mod22.2a <- lm(formula = cost ~ date + cap + pt + ne, data = nuclear)
summary(mod22.2a)

#b)
plot(mod22.2a, which = 1)
plot(mod22.2a, which = 2)

#c)
plot(mod22.2a, which = 4)
abline(h = 4/nrow(nuclear), lty = 2)

#d)
plot(mod22.2a, which = 5, cook.levels = c(4/nrow(nuclear),0.5,1))

#e)
nuclear_2 <- nuclear[-19, ]
mod22.2e <- lm(formula = cost ~ date + cap + pt + ne, data = nuclear_2)
summary(mod22.2e)

#f)
library("faraway")
diabetes_mod <- lm(chol ~ age*frame + waist, data = diabetes)
summary(diabetes_mod)

#g)
plot(diabetes_mod, which = 1)
plot(diabetes_mod, which = 2)

#h)
n <- nrow(diabetes) - 16
plot(diabetes_mod, which = 4, cook.levels = 4/n)
abline(h = 4/n, lty = 2)
plot(diabetes_mod, which = 5, cook.levels = c(4/n,4*2/n,4*3/n))
plot(diabetes_mod, which = 6, cook.levels = 4/n)

#i, j, k, l ... Proper data is not available - unable to do those exercises