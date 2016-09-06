#Team- SeeRs, Survival Analysis for customer churn for Telecom company


#The data used for this project is taken from Source :  http://www.dataminingconsultant.com/data/churn.txt
#this is a test data which has been used only for study purpose

#loading the data
survival=read.csv("D:\\Classes\\R programming\\Project 2_sectionB12\\Team SeeRs- Survival Analysis\\churn.csv")

#finding the no of NAs in the data by running summary on whole dataset
summary(survival)
#No NAs in the dataframe

#Install packages required 
#uncomment it if it is there in your R studio
#survival package for survival analysis
#GGally package for plotting survival analysis


# install.packages("survival")
install.packages("GGally")

#loading library

library(GGally)
library(survival)
library(ggplot2)
library(plyr)

#Data Cleaning

# names of the columns in our dataset
# using unique for finding no of levels in each Columns
names(survival)
unique( survival$Area.Code)
unique(survival$State)
#Although we observe that the State column has 51 levels but Area.Code column has only 3 levels 
#this seems spurious so we have not considered these two columns anywhere in our analysis

#renaming Account.Length to Time of the survival analysis which is in Days
#renaming Churn to Event of the survival analysis
#changing yes/no to 1/0 because survival model run in binary better then categorical
survival=rename(survival,c("Account.Length"="Time"))
survival$Churn.=ifelse(survival$Churn.=="False.",0,1)
survival=rename(survival,c("Churn."="Event"))
survival$VMail.Plan=ifelse(survival$VMail.Plan=="no",0,1)
survival$Int.l.Plan=ifelse(survival$Int.l.Plan=="no",0,1)

# subsetting data which can be of more importance for the model
#Phone numbers because of many levels cannot play much role in the model and also increases dimenstionality
# Same logic applies to State as well
survival = subset( survival,select = -Phone)
survival = subset( survival,select = -State)

#attaching Data so we dont have to refer to it everytime
attach(survival)

#kaplan-Meier~non parametric 
km=survfit(Surv(Time,Event)~1)
summary(km)
ggsurv(km,plot.cens = FALSE, surv.col = "red",main = "Kaplan-Meier Plot")
# from the summary and the plot we conclude that 
# Till time 100 days =9% customers left and from 100-200 days 34 % customers left

plot(km)
#kaplan-Meier~non parametric by Int.l.Plan
kmintl=survfit(Surv(Time,Event)~Int.l.Plan)
summary(kmintl)
plot(kmintl)
ggsurv(kmintl,main = "Kaplan-Meier Plot by International plan",plot.cens = FALSE)
survival$int_chrg_per_min=survival$Intl.Charge/survival$Intl.Mins
boxplot(int_chrg_per_min~as.factor(Int.l.Plan),data = survival )
title("Call charges vs International Plan")
#from the summary ,plot and boxplot we conclude that
# based on the boxplot as there is no difference in the int call charge of the customers having 
#intl plan and with no intl plan,the customers having the plan survival rate is low(low  customer satisfaction )


#kaplan-Meier~non parametric by VMail.Plan
kmvm=survfit(Surv(Time,Event)~VMail.Plan)
summary(kmvm)
plot(kmvm)
ggsurv(kmvm,main = "Kaplan-Meier Plot by Voice Mail Plan",plot.cens = FALSE)
#Customers having voice mail plan are satisfied and hence we see the survival rate is pretty high

#LogRank Test
#to test if there is no difference in the survival probability for both groups we will conduct a logRank 
#test, using survdiff function
survdiff(Surv(Time,Event)~VMail.Plan)
#The expectancy of the event happening for customers with voice mail is less comapred to customers those who 
#don't have voice mail

#Restricted Mean Survival Time (done for recommendation)
#Calculate the total call charges for each customer
survival$total.Charge = survival$Day.Charge + survival$Eve.Charge + survival$Night.Charge + survival$Intl.Charge

#Assuming that the usage for every customer is almost same we have taken mean(total.Charge)/mean(Time)
#to denote the average usage per day for each customer
mean(survival$total.Charge)/mean(survival$Time)
#which is 0.588234, we can say that on an average every customer is giving us .588$ each day

#now we will do restricted Mean survival Time for 180 days so see which group of customers have higher survival rate
#As we already have survfit object kmvm from Kaplan Meier run we can directly use it to display RMST
print(kmvm,print.rmean= getOption("survfit.print.rmean"),rmean = 180)
#We observe that the customer with Vmail plan have higher mean survival time, this information is used in the 
#recommendation section

#Nelson-Aalen~non-parametric
nesurvival=survfit(coxph(Surv(Time,Event)~1),type="aalen")
summary(nesurvival)
plot(nesurvival)
ggsurv(nesurvival,main = "Nelson-Aalen Plot",plot.cens = FALSE,surv.col = "red")

#semi parametric methods
#Cox proportional method with number of Calls
#The proportional hazard model is the most general of the regression models because it is not based on any
#assumptions concerning the nature or shape of the underlying survival distribution. The model assumes that the underlying hazard rate (rather than survival time) is a function of the independent variables (covariates); no assumptions are made about the nature or shape of the hazard function
# based on the charges we canconclude that People satisfied with their intl call services have a low hazard rate
z=subset(survival,select =c("Night.Calls","Eve.Calls","Day.Calls","Intl.Calls"))
names(survival)
coxph = coxph(Surv(Time,Event)~as.matrix(z),method = "breslow",robust= T)
summary(coxph)

#We have assumed that there is no tied event for churning so have used coxph's default method 'breslow'
#for tie handling. If no tied events are there all the methods are same.

# The results below show that only International calls affect the hazard rate, 
# for each increase in International calls, the hazard rate get lowered by (1- 0.94)= 0.06 ~ 6%
# exp(coef) exp(-coef) lower .95 upper .95
# as.matrix(z)Night.Calls    1.0016     0.9984    0.9968     1.006
# as.matrix(z)Eve.Calls      1.0014     0.9986    0.9970     1.006
# as.matrix(z)Day.Calls      1.0002     0.9998    0.9956     1.005
# as.matrix(z)Intl.Calls     0.9387     1.0653    0.8973     0.982


#diagnosing this cox prportion model
#cox.zph test the proportional hazards assumption for a Cox regression model fit
cox_proportional_hazard = cox.zph( coxph) 
cox_proportional_hazard
# from the p value we can conclude that all the pvalues are high 
# thus null hypothesis is true ie all the parameters affect hazard rate
plot(cox_proportional_hazard)
abline( h=0,  col= "red")
# From the residuala plot, we can see that there is no immediate pattern, 
# Thus model is not violating any assumptions 


#Cox proportional hazard model coefficient and hazard rate for Plan types, customer service calls and mins usage
# we ran cox proportion model other group of columns
x=subset(survival,select=c(2,3,4,6,9,12,15,18))
coxph = coxph(Surv(Time,Event)~as.matrix(x),method = "breslow",robust= T)
summary(coxph)
# we conclude that CustServ.Calls ie the no of time customer sevice calls the the hazardous increases by 33%
#similarly Int.l.Plan is also increasing hazardous rate by 324 %
# exp(coef) exp(-coef) lower .95 upper .95
# as.matrix(x)Area.Code         1.0003     0.9997    0.9981    1.0024
# as.matrix(x)Int.l.Plan        3.2481     0.3079    2.6072    4.0465
# as.matrix(x)VMail.Plan        0.4409     2.2680    0.3327    0.5843
# as.matrix(x)Day.Mins          1.0090     0.9911    1.0070    1.0109
# as.matrix(x)Eve.Mins          1.0041     0.9959    1.0023    1.0060
# as.matrix(x)Night.Mins        1.0028     0.9972    1.0010    1.0046
# as.matrix(x)Intl.Mins         1.0547     0.9481    1.0184    1.0924
# as.matrix(x)CustServ.Calls    1.3374     0.7477    1.2296    1.4547
# The pvalue of Area code tells us that its not a significant parameter 
# let us remove the Are code from  further analysis
x  = subset(x, select = -Area.Code)

#diagnosing this cox prportion model
#cox.zph test the proportional hazards assumption for a Cox regression model fit
cox_proportional_hazard = cox.zph( coxph) 
cox_proportional_hazard
# from the p value we can conclude that all the pvalues are high 
# thus null hypothesis is true ie all the parameters affect hazard rate
plot(cox_proportional_hazard)
abline( h=0,  col= "red")
# From the residuala plot, we can see that there is no immediate pattern, 
# Thus model is not violating any assumptions 

#Cox proportion by Call Charges
y=subset(survival,select =c("Intl.Charge","Night.Charge","Eve.Charge","Day.Charge"))
coxph = coxph(Surv(Time,Event)~as.matrix(y),method = "breslow",robust= T)
summary(coxph)
#we can clude from the summary that int.Charges are major cause of hazard rate.
# for every dollar increase in Intl call charge, hazard rate is increasing by 27 %
# exp(coef) exp(-coef) lower .95 upper .95
# as.matrix(y)Intl.Charge      1.278     0.7824     1.134     1.440
# as.matrix(y)Night.Charge     1.048     0.9542     1.011     1.087
# as.matrix(y)Eve.Charge       1.057     0.9463     1.035     1.079
# as.matrix(y)Day.Charge       1.056     0.9471     1.044     1.068


#diagnosing this cox prportion model
#cox.zph test the proportional hazards assumption for a Cox regression model fit
cox_proportional_hazard = cox.zph( coxph) 
cox_proportional_hazard
# from the p value we can conclude that all the pvalues are high 
# thus null hypothesis is true ie all the parameters affect hazard rate
plot(cox_proportional_hazard)
abline( h=0,  col= "red")
# From the residuala plot, we can see that there is no immediate pattern, 
# Thus model is not violating any assumptions 

#parametric
#Exponential   Analysis
#this model assumes that the survival time distribution is exponential, and contingent on the values of a set of independent variables (zi). The rate parameter of the exponential distribution can then be expressed as:
#S(z) = exp(a + b1*z1 + b2*z2 + ... + bm*zm)
expo=survreg(Surv(Time,Event)~as.matrix(x),dist = "exponential")
summary(expo)


#Weibull Analysis and model
wbl=survreg(Surv(Time,Event)~as.matrix(x),dist = "weibull")
summary(wbl)
scalwbl=wbl$scale
alpha0=wbl$coefficients[1]
alpha1=wbl$coefficients[2]
alpha2=wbl$coefficients[3]
alpha3=wbl$coefficients[4]
alpha4=wbl$coefficients[5]
alpha5=wbl$coefficients[6]
alpha6=wbl$coefficients[7]
# lets project out survival probabilty for 1 more year
time2=seq(from=1,to = 365*2 , by =1)
# Assuming weibull distribution, the survival equation is
survival1 = exp((-exp(-alpha0-alpha1-alpha2-alpha3-alpha4-alpha5-alpha6)^scalwbl)*time2^scalwbl)
newdata=cbind("Survival_probability" = survival1, "Time" = time2)
newdata=as.data.frame(newdata)
ggplot(newdata, aes(time2,survival1))+geom_line(colour="red")+ylab('Survival Probability')+xlab("Time")+ggtitle(" Weibull Prediction")

# log-logistic analysis
# is a continuous probability distribution for a non-negative random variable. It is used in survival analysis as a parametric model for events whose rate increases initially and decreases later, for example mortality rate from cancer following diagnosis or treatment
llg=survreg(Surv(Time,Event)~as.matrix(x),dist = "loglogistic")
summary(llg)
scalllg=llg$scale
alpha0=llg$coefficients[1]
alpha1=llg$coefficients[2]
alpha2=llg$coefficients[3]
alpha3=llg$coefficients[4]
alpha4=llg$coefficients[5]
alpha5=llg$coefficients[6]
alpha6=llg$coefficients[7]

# lets project out survival probabilty for 1 more year
time2=seq(from=1,to = 365*2 , by =1)
# Assuming log-logistic distribution, the survival equation is
survival2 = ((1+exp(-alpha0-alpha1-alpha2-alpha3-alpha4-alpha5-alpha6)^scalllg)*time2^scalllg)^-1
newdata2=cbind("Survival_probability" = survival2, "Time" = time2)
newdata2=as.data.frame(newdata)
ggplot(newdata2, aes(time2,survival2))+geom_line(colour="red")+ylab('Survival Probability')+xlab("Time")+ggtitle(" Loglogistic Prediction")
# This does not correcpond to our semi parameteric analysis, 
# thus we are sure that Log-logistic assumption does not apply to our model

#plotting the survival probability with respect to time for both weibull and logistic distribution
ggplot() + 
  geom_line(data = newdata, aes(x = time2, y = survival1, color = "blue")) +
  geom_line(data = newdata2, aes(x = time2, y = survival2, color = "red"))  +
  xlab('Time') +
  ylab('Survival Probability')+
  scale_color_manual("Model\n",labels = c("Weibull", "LogLogistic"), values = c("blue", "red")) +
  ggtitle(" Loglogistic Prediction (Red) Vs Weibull Prediction (Blue) ")

#This plot clearly shows that log logistic model is incorrect for our model here and we should use weibull over this.

#==========================================End of Code===================================================