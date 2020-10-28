# summary(mod300)
library(lme4)
library(rlist)
library(ggplot2)
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
  boot[[i]] = confint( model[[i]], nsim=1000, method='boot') 
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
age <- read.csv('age.csv')

names(age) <- c("X", "A", "A")
age_binned <- rbind(age[, c(1,2)], age[, c(1,3)])

ggplot(age_binned, aes(X, A)) +
  geom_point(size = .8, color = "royalblue3") +
  geom_line(aes(group = X), alpha = 0.35) +
  theme_light()


#############
#############
#############
cond <- read.csv('cond.csv')

names(cond) <- c("X", "A", "A")
cond_binned <- rbind(cond[, c(1,2)], cond[, c(1,3)])

ggplot(cond_binned, aes(X, A)) +
  geom_point(size = .8, color = "royalblue3") +
  geom_line(aes(group = X), alpha = 0.35) +
  theme_light()

#############
#############
#############

inter <- read.csv('inter.csv')

names(inter) <- c("X", "A", "A")
inter_binned <- rbind(inter[, c(1,2)], inter[, c(1,3)])

ggplot(inter_binned, aes(X, A)) +
  geom_point(size = .8, color = "royalblue3") +
  geom_line(aes(group = X), alpha = 0.35) +
  theme_light()