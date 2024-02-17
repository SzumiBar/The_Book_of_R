### Exercise 9.1 ###
#a)
ls(package:methods)[1:20]
length(ls(package:methods))

#b)
environment(read.table)
environment(data)
environment(matrix)
environment(jpeg)

#c)
any(ls(package:graphics) == "smoothScatter")



### Exercise 9.2 ###
#a)
seq(-4,4,0.2)

#b)
i. #mixed
ii. #positional
iii. #exact
vi. #exact
v. #positional
vi. #partial, mixed

#c)
?plot.default #col, lwd, lty, pch