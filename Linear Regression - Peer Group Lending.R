#---------------------Data Overivew ---------------------------------------------
#The Lending Club is a peer-to-peer lending site where members make loans to each other. 
#The site makes anonymized data on loans and borrowers publicly available. 

#------------Data description----------------------------------------------------------------
#The data have the following variables (with data type and explanation of meaning)
#.	Amount.Requested - numeric. The amount (in dollars) requested in the loan application.
#.	Amount.Funded.By.Investors - numeric. The amount (in dollars) loaned to the individual.
#.	Interest.rate - character. The lending interest rate charged to the borrower.
#.	Loan.length - character. The length of time (in months) of the loan.
#.	Loan.Purpose - categorical variable. The purpose of the loan as stated by the applicant.
#.	Debt.to.Income.Ratio - character. The % of consumer's gross income going toward paying debts.
#.	State - character. The abbreviation for the U.S. state of residence of the loan applicant.
#.	Home.ownership - character. Indicates whether the applicant owns, rents, or has a mortgage.
#.	Monthly.income -¬ categorical. The monthly income of the applicant (in dollars).
#.	FICO.range - categorical (expressed as a string label e.g. "650-655"). A range indicating the applicants FICO score.
#.	Open.CREDIT.Lines - numeric. The number of open lines of credit at the time of application.
#.	Revolving.CREDIT.Balance - numeric. The total amount outstanding all lines of credit.
#.	Inquiries.in.the.Last.6.Months - numeric. Number of credit inquiries in the previous 6 months.
#.	Employment.Length - character. Length of time employed at current job.

#------------------------------Business Problem1--------------------------------------- 
#Using lending club loans data, the team would like to test below hypothesis on how different factors effecing each other (Hint: You may leverage hypothesis testing)

#Data importing
setwd("C:/Users/admin/Desktop/ML Project for R/1. PEER GROUP LENDING - REGRESSION/")
LoansData <- read.csv("LoansData.csv")
#Change the variable names: replace dots with underscores
names(LoansData) <- gsub(".","_", names(LoansData),fixed=T)

#Convert data types: Some of the variables ideally should be numerical however they read as factor variables
# Intrest Rate, Debt.To.Income.Ratio, FICO.Range

LoansData$Interest_Rate <- as.numeric(gsub("%","",as.character(LoansData$Interest_Rate)))
LoansData$Debt_To_Income_Ratio <- as.numeric(gsub("%","",as.character(LoansData$Debt_To_Income_Ratio)))
LoansData_new<- LoansData %>% separate(FICO_Range, c("FICO_Low", "FICO_High"), "-") 
LoansData_new<- LoansData_new %>% mutate(FICO_Low = as.numeric(FICO_Low), 
                                 FICO_High = as.numeric(FICO_High),
                                 FICO_AVG = (FICO_Low+FICO_High)/2) 
LoansData_new$FICO_High<-NULL
LoansData_new$FICO_Low<-NULL

#a.	Intrest rate is varied for different loan amounts (Less intrest charged for high loan amounts)
plot(LoansData$Interest_Rate, LoansData$Amount_Funded_By_Investors)
cor(LoansData$Interest_Rate, LoansData$Amount_Funded_By_Investors, use = "pairwise.complete.obs")


#b.	Loan length is directly effecting intrest rate.
#Method-1
t.test(LoansData$Interest_Rate~LoansData$Loan_Length, paired=FALSE, var.equal=FALSE )

#Method-2
summary(aov(LoansData$Interest_Rate~LoansData$Loan_Length)) 

#c.	Intrest rate varies for different purpose of loans

summary(aov(LoansData$Interest_Rate~LoansData$Loan_Purpose)) 


#d.	There is relationship between FICO scores and Home Ownership. 
    #It means that, People with owning home will have high FICO scores.
require(gmodels)

output<-CrossTable(LoansData$FICO_Range, LoansData$Home_Ownership)
View(output)

chisq.test(table(LoansData$FICO_Range, LoansData$Home_Ownership))



#------------------------------Business Problem2--------------------------------------- 

#How can we predict interest rates based on borrower and loan attributes?
#We would like to use lending club loans data to explore how the interest rate charged on loans depends on various factors.

#----------------------------------------------------------------------------------
#Important steps for model building:
# Pre Modeling	
  #1. Identify business problem	
  #2. Convert business problem into statistics problem	(identify Y, X1, X2,X3 ....)
  #3. What type of technique to solve - Linear Regression -Y = F(X1, X2,X3,.)	
  #   Y = B1*X1+B2*X2+.Bn*Xn +C	
  #4. Extract the data from multiple sources based on problem solve and identify variable to create	
  #5. Consolidate the data at customer level(Customer 360 degree view)
#Modeling			
  #1. Create Data Audit Report and it will be used for identifying 
    #Univariate Analysis: To understand the Distribution of data
    #Bivariate analysis: To identify the Relationships between data
    #Missing values, Outliers, constant values, zeros, Zero variance	
  #2. Data Preparation-1: The objective of step is process the data and solve the problems identified in the previous step				
    #Treating missing value	
        # If Y have missings - Drop observations    
        # Dropping columns/obs or replacing missings with some value (imputation)
    #Treating outliers - dropping the observation or capping/flooring outliers
    #Removing constant values (if required)			
    #Replacing zeros (if required)			
    #Dropping zero variance variables	
  #3. Data Preparation Step-2: Validate the Assumptions of technique and process the data as per the requirement	
    #Linear regression follows below assumptions					
      #1. Y Should follow normal(Normality) -	If Y is not following normal distribution, you need to transform Y such that, transformed variable follows normal distribution
      #Y & X should have linear relationship (Linearity) - If Y & X are not having linear relationship, then you need to transform X, such that Y & Transformed X should have linear relaionship
      #No outliers - If the outliers are existed, you need to treat them
      #No multicollinieirity -If the multicollinieirity exists, you need to drop all the variable except one
      #No hetroscedasticity - If hetroscedasticity presents, you need to transform Y
  #4. Data Preparation Step-3: Feature engineering				
      #Create new variables (derived variables - ratios, using dates)			
      #Categorical variables (Encoding)
          #Ordinal - Numerical encoding	
          #Nominal - Dummy variables (binary variables)	
      #Variable Reduction techniques	
          #Correlation metrics 
          #Factor analysis - Principal component analysis		
		      #Y & X not having correlation	
          #Bdetween X (there is collreation) - you required drop one of them other one	
          #VIF	Multicollinieirity	
          #Statistical methods		
                #Y & X	Both are numerical
                #Y & X	Both are categorical
                #Y & X	one is categorical- other one is numerical
                #F-regresison		
                #Univariate regression		
                #Stepwise regression		

       #Split the data into Dev(training) & validation (testing)				
                # 70:30: 70% of data for training & 30% data for testing		
                # 80:20	
       #Build the model usign training data				
                # Finalizing the mathematical equation (With good accuracy)			
                # Finalizing the variables
			          #Accuracy metrics			
      #Validate the model on testing data				
            # Score the testing data using final equation (from model devlopement)			
            # Check the accuracies between training & Testing by calculating the 
            # metrics like R-squre, MAPE, RMSE, MSE and correlation between actual and predicted			
#Post Modeling	
      #Model implementation code	
      #Prepare Document about the model	with details like Pros & Cons, assumptions of the model	etc
      #Model tracking team will track the model performance for Every 6 months or every 1 year
#-----------------------------------------------------------------------------------------------

#Important Libraries in R for performing linear regression
  #0. Data Understanding: Base R, dplyr, pysch, Hmisc, sqldf
  #1. Data preparation: Base R, dplyr, tidyr, data.table, sqldf
  #2. Data visualizations: Base R, ggplot, plotly etc
  #3. Variable reduction: Base R (corr, t.test(), aov(), cor.plot() etc), psych (factor analysis)
  #4. Model building: Base R (lm(), glm()), car
  #5. Model validation: dplyr, data.table, sqldf,
#-----------------------------------------------------------------------------------------------
#Importing the csv file into R
setwd("C:/Users/admin/Desktop/ML Project for R/1. PEER GROUP LENDING - REGRESSION/")
LoansData <- read.csv("LoansData.csv")

#Understand the data
str(LoansData)
View(LoansData)
head(LoansData)
tail(LoansData)
names(LoansData)

summary1 <- summary(mydata)
View(summary1)

require(psych)
describe(mydata)
summary2 <- str(mydata)

#Data Preparation:
require(dplyr)
require(tidyr)

#Change the variable names: replace dots with underscores
names(LoansData) <- gsub(".","_", names(LoansData),fixed=T)

#Convert data types: Some of the variables ideally should be numerical however they read as factor variables
# Intrest Rate, Debt.To.Income.Ratio, FICO.Range

LoansData$Interest_Rate <- as.numeric(gsub("%","",as.character(LoansData$Interest_Rate)))
LoansData$Debt_To_Income_Ratio <- as.numeric(gsub("%","",as.character(LoansData$Debt_To_Income_Ratio)))
LoansData<- LoansData %>% separate(FICO_Range, c("FICO_Low", "FICO_High"), "-") 
LoansData<- LoansData %>% mutate(FICO_Low = as.numeric(FICO_Low), 
                                 FICO_High = as.numeric(FICO_High),
                                 FICO_AVG = (FICO_Low+FICO_High)/2) 
LoansData$FICO_High<-NULL
LoansData$FICO_Low<-NULL
#str(LoansData)
#View(LoansData)

#User defined function to create Data audit report for numerical variables

mystats_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  cv = sd(x, na.rm=T)/mean(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, cv=cv, var=var, range=range, pctl=pctl))
}

#User defined function to create Data audit report for categorical variables
mystats_cat = function(x){
Var_Type=class(x)
n<-length(x)
nmiss<-sum(is.na(x))
fre<-list(table(x))
prop<-list(prop.table(table(x)))
#x[is.na(x)]<-x[which.max(prop.table(table(x)))]
return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
}

#Implementing udf on one variable at a time
mystats_num(LoansData$Interest_Rate)
mystats_cat(LoansData$Loan_Purpose)

#Implementing the function on one variable at a time
#Separating all the numeric & categorical variables to treat them
numeric_vars = names(LoansData)[sapply(LoansData, FUN=is.numeric)]
cat_vars = names(LoansData)[sapply(LoansData, FUN=is.factor)]

#Data Audit Report for numerical variables
summary_stats = t(apply(LoansData[numeric_vars], 2, FUN=mystats_num))
#View(summary_stats)
write.csv(summary_stats, file = "stats_numeric_vars.csv")

#Data Audit Report for categorical variables
summary_stats_cat = apply(LoansData[cat_vars], 2, FUN=mystats_cat)

con <- file("stats_cat_vars.txt")
sink(con, append=TRUE)
#sink(con, append=TRUE, type="message")
summary_stats_cat
sink()

#--------------------------------------------------------------------------------------
#Outlier treatment

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  #UC1 = mean(x,na.rm=T) + 3*sd(x, na.rm=T)
  #LC1 = mean(x,na.rm=T) - 3*sd(x, na.rm=T)
  
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  #x[x>UC1]=UC1
  #x[x<LC1]=LC1
  return(x)
  
}

LoansData_num = data.frame(apply(LoansData[numeric_vars], 2, FUN=outlier_treat))
#View(LoansData_num)

#Number of missings in each variable
sapply(LoansData, FUN=function(x) sum(is.na(x)))

# Missing value treatment for numerical variables
miss_treat_num = function(x){
  x[is.na(x)] = median(x,na.rm=T) # replace missings with mean
  return(x)
}

LoansData_num = data.frame(apply(LoansData_num, 2, FUN=miss_treat_num))

# Missing value treatment for categorical variables
miss_treat_cat = function(x){
  x[is.na(x)]<-x[which.max(prop.table(table(x)))] #replacing missings with mode
  return(x)
}

LoansData_cat = data.frame(apply(LoansData[cat_vars], 2, FUN=miss_treat_cat))


#Number of missings in each variable
sapply(LoansData_num, FUN=function(x) sum(is.na(x)))
sapply(LoansData_cat, FUN=function(x) sum(is.na(x)))

#-------------------------------------------------------------------------------------
#Assumptions Check

hist(LoansData$Interest_Rate)  #closer to normal

cor_mat<-data.frame(cor(LoansData_num))
write.csv(cor_mat, "cor_mat.csv")

#install.packages('corrplot')
require(corrplot)

corrplot(cor(LoansData_num), method="number", number.font = 1)


#identify which categorical variables are significant

summary(aov(LoansData$Interest_Rate~LoansData$Loan_Length))  # significant
summary(aov(LoansData$Interest_Rate~LoansData$Loan_Purpose)) # significant
summary(aov(LoansData$Interest_Rate~LoansData$Home_Ownership)) #significant
summary(aov(LoansData$Interest_Rate~LoansData$Employment_Length)) #Not significant
summary(aov(LoansData$Interest_Rate~LoansData$State)) # Not significant

#Converting categorical variables into dummy variables
#install.packages('caret')
require(caret)
#ls("package:caret")

names(LoansData_cat)

#-----------Loan_Length
LoansData_cat$Loan_Length <- factor(LoansData_cat$Loan_Length)

dv1=dummyVars(~Loan_Length , data=LoansData_cat)
dummy_Loan_Length = data.frame(predict(dv1, LoansData_cat))[-1]

#-----------Loan_Purpose
LoansData_cat$Loan_Purpose <- factor(LoansData_cat$Loan_Purpose)

dv2=dummyVars(~Loan_Purpose , data=LoansData_cat)
dummy_Loan_Purpose = data.frame(predict(dv2, LoansData_cat))[-1]

#-----------Home_Ownership
LoansData_cat$Home_Ownership <- factor(LoansData_cat$Home_Ownership)

dv3=dummyVars(~Home_Ownership , data=LoansData_cat)
dummy_Home_Ownership = data.frame(predict(dv3, LoansData_cat))[-1]


#---------combining numeric and categorical data into single data set

LoansData1=data.frame(cbind(LoansData_num, dummy_Loan_Length,dummy_Loan_Purpose,dummy_Home_Ownership))

View(LoansData1)

#Change the variable names: replace dots with underscores
names(LoansData1) <- gsub(".","_", names(LoansData1),fixed=T)
str(LoansData1)
#------------------------------------------------------------------------------------------------

#Split the data into training & Testing
samp<-sample(1:nrow(LoansData1), floor(nrow(LoansData1)*0.7))

dev<-LoansData1[samp,]
val<-LoansData1[-samp,]

nrow(LoansData1)
nrow(dev)
nrow(val)


#---------------------------------------------------------------------------------
#Develop the model (Train the model)
names(LoansData1)
fit<- lm(Interest_Rate~.,data=LoansData1 ) #Build the model with all the variables
summary(fit)

# Useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
hist(residuals(fit))

#Reducing the variables using step function
step(fit, direction="both") # Step function will help you to reduce the variables based on AIC value (if AIC is low, then its good model)


fit2<-lm(formula = Interest_Rate ~ Amount_Requested + Amount_Funded_By_Investors + 
     Monthly_Income + Open_CREDIT_Lines + Inquiries_in_the_Last_6_Months + 
     FICO_AVG + Loan_Length_60_months + Loan_Purpose_credit_card + 
     Loan_Purpose_debt_consolidation + Loan_Purpose_moving + Loan_Purpose_other + 
     Loan_Purpose_small_business + Loan_Purpose_wedding + Home_Ownership_NONE + 
     Home_Ownership_OWN + Home_Ownership_RENT, data = LoansData1)

summary(fit2)
require(car)
car::vif(fit2) #Two variables have high vif value (with >5), drop one of the variable and build the model

fit3<-lm(formula = Interest_Rate ~  Amount_Funded_By_Investors + 
           Monthly_Income + Open_CREDIT_Lines + Inquiries_in_the_Last_6_Months + 
           FICO_AVG + Loan_Length_60_months + Loan_Purpose_credit_card + 
           Loan_Purpose_debt_consolidation + Loan_Purpose_moving + Loan_Purpose_other + 
           Loan_Purpose_small_business + Loan_Purpose_wedding + Home_Ownership_NONE + 
           Home_Ownership_OWN + Home_Ownership_RENT, data = LoansData1)

summary(fit3)
car::vif(fit3) #all the variables have vif value with less than 5

#-------------------------------------------------------------------------
#Scoring the data (predicting the value using predict function)

dev1<-data.frame(cbind(dev, pred_int_rate = predict(fit3, newdata=dev)))
val1<-data.frame(cbind(val, pred_int_rate = predict(fit3, newdata=val)))

#View(dev1)

#----COmparing the metrics between Development & validation

#MAPE
dev_mape = mean(abs(dev1$Interest_Rate - dev1$pred_int_rate)/dev1$Interest_Rate)
val_mape = mean(abs(val1$Interest_Rate - val1$pred_int_rate)/val1$Interest_Rate)

print(dev_mape)
print(val_mape)

#RMSE
dev_rmse = sqrt(mean((dev1$Interest_Rate - dev1$pred_int_rate)**2))
val_rmse = sqrt(mean((val1$Interest_Rate - val1$pred_int_rate)**2))

print(dev_rmse)
print(val_rmse)

#Correlations
cor(dev1$Interest_Rate, dev1$pred_int_rate)
cor(val1$Interest_Rate, val1$pred_int_rate)


#################### Creating Deciles####################################
# find the decile locations 
decLocations <- quantile(dev1$pred_int_rate, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
dev1$decile <- findInterval(dev1$pred_int_rate,c(-Inf,decLocations, Inf))
#View(dev1)

##################################Decile Analysis Reports
#install.packages('sqldf')
require(sqldf)
dev_DA <- sqldf("select decile, count(decile) as count, avg(pred_int_rate) as avg_pre_int_rate,  
                avg(Interest_Rate) as avg_Int_Rate
                    from dev1
                    group by decile
                    order by decile desc")

View(dev_DA)
#######################################################

# find the decile locations 
decLocations <- quantile(val1$pred_int_rate, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
val1$decile <- findInterval(val1$pred_int_rate,c(-Inf,decLocations, Inf))
#View(val1)

##################################Decile Analysis Reports

val_DA <- sqldf("select decile, count(decile) as count, avg(pred_int_rate) as avg_pre_int_rate,  
                avg(Interest_Rate) as avg_Int_Rate
                from val1
                group by decile
                order by decile desc")

View(val_DA)

###################################END OF REGRESSION case study 

