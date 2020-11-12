# summary(mod300)
library(lme4)
library(rlist)
library(ggplot2)


setwd("~/dev/EEGdeconv/data")
data <- read.csv('100bcsacclockedmyData.csv')
data$cond = as.factor(data$cond)
data$age = as.factor(data$age)
data$dir = as.factor(data$dir)

model = list()
for (i in 1:600)
{#600 ms before and 600 after the saccade
  model[[i]] <- eval(parse(text=paste("lmer(X",i," ~ 1 + cond*age+ (1|Var1), data=data)", sep="")))
}

modelinter = list()
for (i in 1:600)
{#600 ms before and 600 after the saccade
  modelinter[[i]] <- eval(parse(text=paste("lmer(X",i," ~ 1 + cond*age*dir + (1|Var1), data=data)", sep="")))
}

BICcomp = list()
for (i in 1:600)
{#300 ms before and 100 after the sacculus
  BICcomp[[i]] <- BIC(model[[i]],modelinter[[i]])
}
