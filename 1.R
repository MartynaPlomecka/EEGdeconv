library(brms)
install.packages("rlist")
library(rlist)
# Upload the csv file
data <- read.csv('myData.csv')
data$Var1   #120 levels, 60 old, 60 yng


# mod300 <- brm(X300 ~ 1 + cond * age+ (1|Var1), data = data)
# summary(mod300)

mod = list()
for (i in 150:350)
  {#300 ms before and 100 after the saccade
mod[[i]] <- eval(parse(text=paste("brm(X",i," ~ 1 + cond*age+ (1|Var1), data=data)", sep="")))
summary(mod[[i]])
}


for (i in 150:350){
  print(allsum[[i]])
}
allsum = list()
for (i in 150:350){
  allsum[[i]] = summary(mod[[i]])
}



list.save(allsum, 'allsummaries.rds')
list.save(allsum, 'allsummaries.rdata')


#allsum[[160]][14]

##############################################################################
# pp_check(beta)
# fitted(beta)
# summary(beta)
# predict(beta)
# Function fitter predict mean values of the response distribution 
#(i.e., the 'regression line') for a fitted model. 
#Can be performed for the data used to fit the model (posterior predictive checks) 
#or for new data. 
#By definition, these predictions have smaller variance than 
#the response predictions performed by the predict method.
#This is because the measurement error is not incorporated. 
#The estimated means of both methods should, however, be very similar.


