setwd("/Users/ozgurcetinok/Desktop/Fall2018/Datathon")
data = read.csv("City_Employee_Payroll.csv", header = TRUE)
#Look at Department Title, no nulls
new_DF = data[is.na(data$Department.Title),]
#Look at Department Title, no nulls
new_DF = data[is.na(data$Year),]
#Look at Payroll Department, many nulls
new_DF = data[is.na(data$Payroll.Department),]
#All the null Payroll Department Values are from DWP
#We will fix that by assigning a new Payroll Dept to them.For example 8703
data$Payroll.Department[is.na(data$Payroll.Department)] = "8703"
new_DF = data[is.na(data$Payroll.Department),]
#Payroll Dept is fine right now.
#Look at Record Number, no nulls
new_DF = data[is.na(data$Record.Number),]
#Look at Job Class Title, no nulls
new_DF = data[is.na(data$Job.Class.Title),]
#Look at Employment Type, no nulls
new_DF = data[is.na(data$Employment.Type),]
#Hourly or Event Rate Missing for DWP AGAIN!
new_DF = data[is.na(data$Hourly.or.Event.Rate),]
X = subset(data,data$Year!="2018")
Y_2018 = subset(data,data$Year=="2018")
#For Full time 2088, part time same.
new_DF = data[is.na(data$Hourly.or.Event.Rate),]
X$Hourly.or.Event.Rate = ifelse(is.na(X$Hourly.or.Event.Rate),X$Projected.Annual.Salary/2088,X$Hourly.or.Event.Rate)
#Fixed the issue
#Projected Annual Salary, no Nulls
new_DF = newdata[is.na(newdata$Projected.Annual.Salary),]
#Q, no null luckily
new_DF = X[is.na(X$Q1.Payments),]
new_DF = X[is.na(X$Q2.Payments),]
new_DF = X[is.na(X$Q3.Payments),]
new_DF = X[is.na(X$Q4.Payments),]
#Payments Over Base Pay, there are nulls. They should be zero.
new_DF = X[is.na(X$Payments.Over.Base.Pay),]
X$Payments.Over.Base.Pay = ifelse(is.na(X$Payments.Over.Base.Pay),X$Total.Payments-X$Base.Pay,X$Payments.Over.Base.Pay)
new_DF = X[is.na(X$X..Over.Base.Pay),]
X$X..Over.Base.Pay = ifelse(is.na(X$X..Over.Base.Pay),0,X$X..Over.Base.Pay)
#There are nulls, will drop the column
#Total Payments, No nulls
new_DF = X[is.na(X$Total.Payments),]
#Base Pay, No nulls
new_DF = X[is.na(X$Base.Pay),]
#Permanent Bonus Pay
new_DF = X[is.na(X$Permanent.Bonus.Pay),]
new_DF = X[is.na(X$Longevity.Bonus.Pay),]
new_DF = X[is.na(X$Temporary.Bonus.Pay),]
new_DF = X[is.na(X$Lump.Sum.Pay),]
#Make it equal to zero.
X$Lump.Sum.Pay = ifelse(is.na(X$Lump.Sum.Pay),0,X$Lump.Sum.Pay)
#OT
new_DF = X[is.na(X$Overtime.Pay),]
X$Overtime.Pay = ifelse(is.na(X$Overtime.Pay),0,X$Overtime.Pay)
#Other Pay
new_DF = X[is.na(X$Other.Pay...Adjustments),]
#Other Payroll eXp
new_DF = X[is.na(X$Other.Pay..Payroll.Explorer.),]
#MOU
new_DF = X[is.na(X$MOU),]
#MOU Title
new_DF = X[is.na(X$MOU.Title),]
#FMS Dept
new_DF = X[is.na(X$FMS.Department),]
#Job Class
new_DF = X[is.na(X$Job.Class),]
#Pay Grade
new_DF = X[is.na(X$Pay.Grade),]
#Avg Health Cost
new_DF = X[is.na(X$Average.Health.Cost),]
new_DF = X[is.na(X$Average.Dental.Cost),]
new_DF = X[is.na(X$Average.Basic.Life),]
new_DF = X[is.na(X$Average.Benefit.Cost),]
#Benefits Plan
new_DF = X[is.na(X$Benefits.Plan),]
#Job Class Link
new_DF = X[is.na(X$Job.Class.Link),]
#DATA CLEANED
summary(X)
table(is.na(X))
#NAs are DONE, move to next level

#Removing negatives
X = subset(X,X$Total.Payments>=0) 
X = subset(X,X$Other.Pay..Payroll.Explorer. >=0)
X = subset(X,X$Q1.Payments >=0)
X = subset(X,X$Q2.Payments >=0)
X = subset(X,X$Overtime.Pay >=0)
X = subset(X,X$Q3.Payments >=0)
X = subset(X,X$Q4.Payments >=0)
X = subset(X,X$Payments.Over.Base.Pay >=0)
X = subset(X,X$X..Over.Base.Pay >=0)
X = subset(X,X$Base.Pay >=0)
X = subset(X,X$Permanent.Bonus.Pay >=0)
X = subset(X,X$Longevity.Bonus.Pay >=0)
X = subset(X,X$Temporary.Bonus.Pay >=0)
X = subset(X,X$Other.Pay...Adjustments >=0)
summary(X)

X$Job.Class.Title = tolower(X$Job.Class.Title)
X$Employment.Type = tolower(X$Employment.Type)
X$MOU.Title = tolower(X$MOU.Title)
X$Department.Title = tolower(X$Department.Title)

Y_2018$Job.Class.Title = tolower(Y_2018$Job.Class.Title)
Y_2018$Employment.Type = tolower(Y_2018$Employment.Type)
Y_2018$MOU.Title = tolower(Y_2018$MOU.Title)
Y_2018$Department.Title = tolower(Y_2018$Department.Title)

# Write CSV in R
write.csv(X, file = "CleanData.csv")
write.csv(Y_2018, file = "CleanData_2018.csv")

#odev baskasi icin
E1 <- function(x, y, learn_rate = 1e-2, iterations = 1) {
  
  #Create prediction list
  ypred = y
  # initializing weight vector, uniform
  weight <- runif(dim(x)[2] + 1,-0.5,0.5)
  errors <- rep(0, iterations)
  
  # loop over number of epochs
  for (j in 1:iterations) {
    
    # loop through training data set
    for (i in 1:length(y)) {
      
      # Predicting the possibility
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[i, ])) + weight[1]
      #Prediction
      if(z < 0.5) {
        ypred[i] = 0
      } else {
        ypred[i] = 1
      }
      
      # Changing weight
      weightdiff = learn_rate * (y[i] - ypred[i]) * c(1, as.numeric(x[i, ]))
      weight = weight + weightdiff
      
      # Updating error function
      if ((y[i] - ypred)[i] != 0.0) {
        errors[j] = errors[j] + 1
      }
      
    }
  }
  
  # Printing weights
  print(weight)
  #Returning predictions
  return(ypred)
}

