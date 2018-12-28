#READING CLEANED DATA
setwd("/Users/ozgurcetinok/Desktop/Fall2018/Datathon")
data = read.csv("CleanData.csv", header = TRUE)
data_2018= read.csv("CleanData_2018.csv", header = TRUE)

#CORRELATION PLOTS
Numeric_= dplyr::select_if(data, is.numeric)
library("corrplot")
corr_ = cor(Numeric_)
quartz()
corrplot(corr_, method="circle") #pie,number,color

#DROPPING LOTS OF COLUMNS TO BUILD A BENCHMARK MODEL IN PANEL
drops = c("X","Payroll.Department","Benefits.Plan","Pay.Grade","MOU.Title","X..Over.Base.Pay","Q3.Payments","Q4.Payments","Row.ID","MOU","FMS.Department","Job.Class","Job.Class.Link","Average.Health.Cost","Average.Dental.Cost","Average.Basic.Life","Permanent.Bonus.Pay","Longevity.Bonus.Pay","Temporary.Bonus.Pay","Lump.Sum.Pay","Permanent.Bonus.Pay","Other.Pay...Adjustments","Payments.Over.Base.Pay","Base.Pay")
benchmark = data[ , !(names(data) %in% drops)]
benchmark_2018 = data_2018[ , !(names(data_2018) %in% drops)]

summary(benchmark)


#Remove some categorical for now
dropcat = c("Department.Title","Job.Class.Title")
benchmark_nocat = benchmark[ , !(names(benchmark) %in% dropcat)]
benchmark_2018_nocat = benchmark_2018[ , !(names(benchmark_2018) %in% dropcat)]


#Dealing With Left Categorical Variable
library(dummies)
df=dummy.data.frame(benchmark_nocat,names=c("Employment.Type"),sep="_")
df_2018=dummy.data.frame(benchmark_2018_nocat,names=c("Employment.Type"),sep="_")
df=df[!duplicated(df[c("Record.Number","Year")]),]
df_2018=df_2018[!duplicated(df_2018[c("Record.Number","Year")]),]
library("foreign")
library("plm")

data.p = plm.data(df,index=c("Record.Number","Year"))
data.p_2018 = plm.data(df_2018,index=c("Record.Number","Year"))

data.p = transform(data.p, H1= data.p$Q1.Payments+data.p$Q2.Payments )

# Between estimator, The model we use. We tried pooling, Fe, Re Too. Can Provide the code if it is necessary.

between =  plm(H1 ~  lag(Projected.Annual.Salary,1) + lag(Total.Payments,1) + 
                 lag(Overtime.Pay,1) + lag(Other.Pay..Payroll.Explorer.,1) + lag(Average.Benefit.Cost,1), data=data.p, model= "between")
summary(between)

# Pooled OLS estimator,not preferred model 
pooling = plm(H1 ~ lag(Projected.Annual.Salary,1) + lag(Total.Payments,1) + lag(Overtime.Pay,1) + lag(Other.Pay..Payroll.Explorer.,1) + lag(Average.Benefit.Cost,1), data=data.p, model= "pooling")
summary(pooling)


# First differences estimator
firstdiff = plm(H1 ~ lag(Projected.Annual.Salary,1) + lag(Total.Payments,1) + lag(Overtime.Pay,1) + lag(Other.Pay..Payroll.Explorer.,1) + lag(Average.Benefit.Cost,1), data=data.p, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed = plm(H1 ~ lag(Projected.Annual.Salary,1) + lag(Total.Payments,1) + lag(Overtime.Pay,1) + lag(Other.Pay..Payroll.Explorer.,1) + lag(Average.Benefit.Cost,1), data=data.p, model= "within")
summary(fixed)

# Random effects estimator
random = plm(H1 ~ lag(Projected.Annual.Salary,1) + lag(Total.Payments,1) + lag(Overtime.Pay,1) + lag(Other.Pay..Payroll.Explorer.,1) + lag(Average.Benefit.Cost,1), data=data.p, model= "random")
summary(random)

# LM test for random effects versus OLS
#plmtest(pooling) #significant effects, random effect is better

# LM test for fixed effects versus OLS
#pFtest(fixed, pooling)#more support for fixed

# Hausman test for fixed versus random effects model
#phtest(random, fixed)#one of the models is inconsistent, go with fixed effects model.

#GO WITH BETWEEN EFFECTS MODEL

#Checking Everything for 2016-2017 to see if it works
#Checking the accuracy for 2016-2017
data_17 = subset(data.p,data.p$Year=="2017")
data_16 = subset(data.p,data.p$Year=="2016")
coefficients = coef(between)

notincommon_17 = setdiff(data_17$Record.Number, data_16$Record.Number)
notincommon_16 = setdiff(data_16$Record.Number, data_17$Record.Number)

data_17 = data_17[!(data_17$Record.Number %in% notincommon_17),]
data_16 = data_16[!(data_16$Record.Number %in% notincommon_16),]

data_17$Record.Number = lapply(data_17$Record.Number, as.character)
data_16$Record.Number = lapply(data_16$Record.Number, as.character)

data17=data_17

data17 = transform(data17, Projected.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Projected.Annual.Salary))
data17 = transform(data17, Totalpay.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Total.Payments))
data17 = transform(data17, OT.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Overtime.Pay))
data17 = transform(data17, Other.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Other.Pay..Payroll.Explorer.))
data17 = transform(data17, Benefit.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Average.Benefit.Cost))
data17 = transform(data17, H1.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$H1.Payments))
data17 = transform(data17, Q1.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Q1.Payments))
data17 = transform(data17, Q2.lag= if(identical(data_16$Record.Number,data17$Record.Number)) (data_16$Q2.Payments))


data17 = transform(data17, H1.Forecast=coefficients[1]+coefficients[2]*data17$Projected.lag+coefficients[3]*data17$Totalpay.lag+coefficients[4]*data17$OT.lag+coefficients[5]*data17$Other.lag+coefficients[6]*data17$Benefit.lag)

library(forecast)
accuracy(data17$H1.Forecast,data17$H1)

#Checking the Accuracy level on 2017 data
err_= data17$H1 - data17$H1.Forecast
err__= err_[err_ <500]
err__=err__[err__>-500]
length(err__)/length(err_)

#Applying Model to 2018
notincommon_17 = setdiff(data_17$Record.Number, data.p_2018$Record.Number)
notincommon_18 = setdiff(data.p_2018$Record.Number, data_17$Record.Number)

data_17 = data_17[!(data_17$Record.Number %in% notincommon_17),]
data.p_2018 = data.p_2018[!(data.p_2018$Record.Number %in% notincommon_18),]

data_17$Record.Number = lapply(data_17$Record.Number, as.character)
data.p_2018$Record.Number = lapply(data.p_2018$Record.Number, as.character)



data.p_2018 = transform(data.p_2018, Projected.lag= if(identical(data_17$Record.Number,data.p_2018$Record.Number)) (data_17$Projected.Annual.Salary))
data.p_2018 = transform(data.p_2018, Totalpay.lag= if(identical(data_17$Record.Number,data.p_2018$Record.Number)) (data_17$Total.Payments))
data.p_2018 = transform(data.p_2018, OT.lag= if(identical(data_17$Record.Number,data.p_2018$Record.Number)) (data_17$Overtime.Pay))
data.p_2018 = transform(data.p_2018, Other.lag= if(identical(data_17$Record.Number,data.p_2018$Record.Number)) (data_17$Other.Pay..Payroll.Explorer.))
data.p_2018 = transform(data.p_2018, Benefit.lag= if(identical(data_17$Record.Number,data.p_2018$Record.Number)) (data_17$Average.Benefit.Cost))
#data.p_2018 = transform(data.p_2018, H1.lag= if(identical(data.p_2018$Record.Number,data17$Record.Number)) (data_17$H1))
#data.p_2018 = transform(data.p_2018, Q1.lag= if(identical(data.p_2018$Record.Number,data17$Record.Number)) (data_17$Q1.Payments))
#data.p_2018 = transform(data.p_2018, Q2.lag= if(identical(data.p_2018$Record.Number,data17$Record.Number)) (data_17$Q2.Payments))

coefficients = coef(between)
data.p_2018 = transform(data.p_2018, H1.Forecast=coefficients[1]+coefficients[2]*data.p_2018$Projected.lag+coefficients[3]*data.p_2018$Totalpay.lag+coefficients[4]*data.p_2018$OT.lag+coefficients[5]*data.p_2018$Other.lag+coefficients[6]*data.p_2018$Benefit.lag)



towrite = read.csv("payroll_2018.csv", header = TRUE)
drop = c("X")
towrite = towrite[ , !(names(towrite) %in% drop)]


dropp = c("Year","Employment.Type_full.time","Employment.Type_part.time","Employment.Type_per.event","Projected.lag","Totalpay.lag","OT.lag","Other.lag","Benefit.lag")
final = data.p_2018[ , !(names(data.p_2018) %in% dropp)]

final$Record.Number <- as.numeric(as.character(final$Record.Number))

aaa= merge(towrite, final, by = "Record.Number") # NA's match, so 6 rows

fdrop = c("Q1.Q2Payments.2018")
final_pred = aaa[ , !(names(aaa) %in% fdrop)]

colnames(final_pred) <- c("Record.Number", "Q1.Q2Payments.2018")

write.csv(final_pred, file = "Group6.csv", row.names=FALSE)
