#Nyholt -> forstimlocked 98.96
library("tidyverse")
tbl <- read.table('100bcstimlockedmyData.csv', header = TRUE, sep = ",")
head(tbl)


tbl$dir = factor(tbl$dir)
tbl$cond = factor(tbl$cond)
tbl$age= factor(tbl$age)


A = data.matrix(tbl, rownames.force = NA)
A = A[,-1]
A = A[,-603]
A = A[,-602]
A = A[,-601]



cor_matrix = cor(A)
es = eigen(cor_matrix)
var_eig = var(es$values)
var_eig  #obtained value = 24.662

#the effective number od variables (M eff) may be calculaated as follows:
#Meff = 1+ (M-1)(1 - (var_eig/M))
#M = 600 (all timepoints)
#var_eig/600 = 24.662/600 = 0.041

#finally, Meff = 1+599*(1 - 0.041) = 1+599* 0.959 = 575.441

#601/575.441 -> 1.0444 -> instead of 95 CI I report 100-1.05 = 98.96 CI