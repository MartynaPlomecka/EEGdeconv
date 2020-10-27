library(brms)
# Upload the csv file
data <- read.csv('myData.csv')
colnames(data)
#model
data$Var1   #120 levels, 60 old, 60 yng





beta <- brm(X300 ~ 1 + cond + dir + age+ (1|Var1), data = data)

mod300 <- brm(X300 ~ 1 + cond * age+ (1|Var1), data = data)
summary(mod300)

mod = list()
for (cidx in 150:160)#350)
  {#300 ms before and 100 after the saccade
mod[[i]] <- eval(parse(text=paste("brm(X",i," ~ 1 + cond*age+ (1|Var1), data=data)", sep="")))
summary(mod[[i]])
}



pp_check(beta)
fitted(beta)
summary(beta)
predict(beta)


# Function fitter predict mean values of the response distribution (i.e., the 'regression line') for a fitted model. Can be performed for the data used to fit the model (posterior predictive checks) or for new data. By definition, these predictions have smaller variance than the response predictions performed by the predict method. This is because the measurement error is not incorporated. The estimated means of both methods should, however, be very similar.












###############
dir =  data$dir
cond = data$cond
age = data$age
id = data$Var1
t1 = data$X1
t2 = data$X2
t100 = data$X100
t300 = data$X300
t400 = data$X400

plot(t1)
hist(t400,breaks = 10)




 for (cidx in 2:601){
    val = data[,cidx]
    data$val 
 }


val