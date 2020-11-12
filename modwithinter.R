library(lme4)
library(rlist)
library(ggplot2)
library(gridExtra)

data <- read.csv('myData.csv')
data$cond = as.factor(data$cond)
data$age = as.factor(data$age)
data$dir = as.factor(data$dir)

model = list()
for (i in 1:600)
{#300 ms before and 100 after the saccade
  model[[i]] <- eval(parse(text=paste("lmer(X",i," ~ 1 + cond*age*dir + (1|Var1), data=data)", sep="")))
  summary(model[[i]])
}


#STAT SUMMARIES
 allsummary = list()
 for (i in 1:600){
  allsummary[[i]] = summary(model[[i]])
 }
# 
 for (i in 1:600){
   print(allsummary[[i]])
 }
