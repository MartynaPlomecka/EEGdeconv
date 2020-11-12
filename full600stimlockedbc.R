# summary(mod300)
library(lme4)
library(rlist)
library(ggplot2)
library(gridExtra)

setwd("~/dev/EEGdeconv/data")
data <- read.csv('100bcstimlockedmyData.csv')
data$cond = as.factor(data$cond)
data$age = as.factor(data$age)
model = list()
for (i in 1:600)
{#300 ms before and 100 after the stimulus
  model[[i]] <- eval(parse(text=paste("lmer(X",i," ~ 1 + cond*age+ (1|Var1), data=data)", sep="")))
  summary(model[[i]])
}

# #STAT SUMMARIES
# allsummary = list()
# for (i in 1:600){
#   allsummary[[i]] = summary(model[[i]])
# }
# 
# for (i in 1:600){
#   print(allsummary[[i]])
# }




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
write.csv(cond,'bcstimcond.csv')

####AGE
age = data.frame(A=numeric(0),B=numeric(0))
for (i in 1:600){
  age[i,] = boot[[i]][5,]
}
write.csv(age,'bcstimage.csv')

inter = data.frame(A=numeric(0),B=numeric(0))
for (i in 1:600){
  inter[i,] = boot[[i]][6,]
}
write.csv(inter,'bcstiminter.csv')
###########################################


#############
#############
#############
age <- read.csv('bcstimage.csv')

names(age) <- c("X", "A", "A")
age_binned <- rbind(age[, c(1,2)], age[, c(1,3)])

a = ggplot(age_binned, aes(X, A)) +
  geom_point(size = .8, color = "royalblue3", alpha = 0.7) +
  geom_line(aes(group = X), alpha = 0.15) +
  scale_x_continuous(name="time points", limits=c(0, 600)) +
  scale_y_continuous(name="confidence interval", limits=c(-1.5, 2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  ggtitle("bc stimloc Fixed effect age") +
  geom_vline(xintercept=300)+
  
  
  theme_light()


#############
#############
#############
cond <- read.csv('bcstimcond.csv')

names(cond) <- c("X", "A", "A")
cond_binned <- rbind(cond[, c(1,2)], cond[, c(1,3)])

c = ggplot(cond_binned, aes(X, A)) +
  geom_point(size = .8, color = "royalblue3",  alpha = 0.7) +
  geom_line(aes(group = X), alpha = 0.15) +
  scale_x_continuous(name="time points", limits=c(0, 600)) +
  scale_y_continuous(name="confidence interval", limits=c(-1.5, 1.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=300)+
  
  ggtitle("bc stimloc Fixed effect cond (pro/anti)") +
  
  
  theme_light()

###############################
###############################
###############################
###############################

inter <- read.csv('bcstiminter.csv')

names(inter) <- c("X", "A", "A")
inter_binned <- rbind(inter[, c(1,2)], inter[, c(1,3)])

i = ggplot(inter_binned, aes(X, A)) +
  geom_point(size = .8, color = "royalblue3",  alpha = 0.7) +
  geom_line(aes(group = X), alpha = 0.15) +
  scale_x_continuous(name="time points", limits=c(0, 600)) +
  scale_y_continuous(name="confidence interval", limits=c(-1.5, 1.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=300)+
  
  ggtitle("bc stimloc Inter age*cond") +
  theme_light()



####

#############
#############
#############
grid.arrange(a, c,i,  nrow = 1)
