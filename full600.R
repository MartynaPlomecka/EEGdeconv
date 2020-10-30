# summary(mod300)
library(lme4)
library(rlist)
library(ggplot2)
library(gridExtra)
data <- read.csv('myData.csv')

model = list()
for (i in 1:600)
{#300 ms before and 100 after the saccade
  model[[i]] <- eval(parse(text=paste("lmer(X",i," ~ 1 + cond*age+ (1|Var1), data=data)", sep="")))
  summary(model[[i]])
}

#STAT SUMMARIES
allsummary = list()
for (i in 1:600){
  allsummary[[i]] = summary(model[[i]])
}

for (i in 1:600){
  print(allsummary[[i]])
}

list.save(allsummary, 'lmerallsummaries.rds')
list.save(allsummary, 'lmerallsummaries.rdata')


### bootstrap to get reliable and stable estimates
boot = list()
for (i in 1:600){
  boot[[i]] = confint( model[[i]], nsim=100, method='boot') 
}


####COND
cond = data.frame(A=numeric(0),B=numeric(0))
for (i in 1:600){
  cond[i,] = boot[[i]][4,]
}
write.csv(cond,'cond.csv')

####AGE
age = data.frame(A=numeric(0),B=numeric(0))
for (i in 1:600){
  age[i,] = boot[[i]][5,]
}
write.csv(age,'age.csv')

inter = data.frame(A=numeric(0),B=numeric(0))
for (i in 1:600){
  inter[i,] = boot[[i]][6,]
}
write.csv(inter,'inter.csv')
###########################################


#############
#############
#############
grid.arrange(a, c,i,  nrow = 1)