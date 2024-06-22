rm(list=ls())# removes al variables stored previously
install.packages("Hmisc")
library(Hmisc)
data <- read.csv("C:/Users/user/Downloads/COVID 19/COVID 19.csv")
describe(data)# Hmisc command
# in death, 14 values are distinct because death is reported as 0,1 or the date itself hence it's inconsistent
# Create a new column
#Cleaned up death column
data$death_dummy<-as.integer(data$death != 0) #if the deaths column is 0, no deaths
unique(data$death_dummy)

#death rate
sum(data$death_dummy)/nrow(data) #5.8%

#AGE
#Claim; People who died are older
dead= subset(data,death_dummy==1) #63
alive=subset(data,death_dummy==0) #1022

mean(dead$age, na.rm=TRUE) #68.5862
mean(alive$age,na.rm=TRUE) #48.0723
#Difference between dead and alive is 20 yrs
#Is this statistically significant?

t.test(alive$age, dead$age,alternative = "two.sided",conf.level = 0.95 )
#Difference between a person who's alive and dead is between 24 to 16yrs
#One's that alive is much younger
2.2e-16 #=0, ages in this pop are not equal

#if p value<0.05, reject H0
#Here, pvalue~0, hence reject H0 and conclude that this is statistically significant
#The people who die from the virus are much older

#GENDER
#Claim; Gender has no effect
men= subset(data,gender=="male")
women=subset(data, gender=="female") 
mean(men$death_dummy) #8.5%
mean(women$death_dummy)#3.7%
t.test(men$death_dummy,women$death_dummy ,alternative = "two.sided",conf.level = 0.95 )
# pvalue~0, reject H0 and conlcude that gender has an effect




    
  



 
