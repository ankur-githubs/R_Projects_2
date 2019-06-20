setwd("E:\\R_class\\linear regresion\\Boot Camp - 27th August 2018 - Linear Regression\\Case Study - Automobile Example - Class Exercise")

mydata <- read.csv("car_sales.csv")

View(mydata)

summary1 <- summary(mydata)

View(summary1)

write.csv(summary1, file = "summary.csv")

require(psych)
describe(mydata)

names <- names(mydata)

summary2 <- str(mydata)

#write.csv(summary2 ,file = "summary2.csv")

con <- file("summary2.csv")
sink(con,append = T)
str(mydata)
sink()



mystats = function(x){
  
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
  
}

mystats(mydata$Sales_in_thousands)

sapply(mydata, FUN=is.numeric)

numeric_vars = names(mydata)[sapply(mydata, FUN=is.numeric)]

summary_stats = t(apply(mydata[numeric_vars], 2, FUN=mystats))

View(summary_stats)


write.csv(summary_stats, file = "stats.csv")



#Outlier treatment

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  
 # UC2 = mean(x,na.rm=T) + 3*sd(x, na.rm=T)
 # LC2 = mean(x,na.rm=T) - 3*sd(x, na.rm=T)
  
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  
  #x[x>UC1]=UC1
  #x[x<LC1]=LC1
  
  return(x)
  
}

mydata_num_vars = data.frame(apply(mydata[numeric_vars], 2, FUN=outlier_treat))

View(mydata_num_vars)
#x= mydata$Price_in_thousands

summary_stats1 = t(apply(mydata_num_vars, 2, FUN=mystats))

View(summary_stats1)


mydata_num_vars<- mydata_num_vars[!is.na(mydata_num_vars$Sales_in_thousands),]
mydata_num_vars$Price_in_thousands[is.na(mydata_num_vars$Price_in_thousands)] <- mean(mydata_num_vars$Price_in_thousands,na.rm=T)


miss_treat = function(x){
  x[is.na(x)] = mean(x,na.rm=T)
  return(x)
}

mydata_num_vars = data.frame(apply(mydata_num_vars, 2, FUN=miss_treat))


View(mydata_num_vars)

summary_stats2 = t(apply(mydata_num_vars, 2, FUN=mystats))

View(summary_stats2)

cor_matrics = cor(mydata_num_vars)
View(cor_matrics)

str(mydata)

mydata$Latest_Launch = as.Date(as.character(mydata$Latest_Launch), '%m/%d/%Y')



mydata$Tenure = Sys.Date()-mydata$Latest_Launch
View(mydata)



install.packages('caret')
require(caret)
ls("package:caret")

mydata$Vehicle_type <- factor(mydata$Vehicle_type)
levels(mydata$Vehicle_type) <- c("Car","Passenger")

?dummyVars()

dv=dummyVars(~Vehicle_type , data=mydata)
dummy_cat = data.frame(predict(dv, mydata))


mydata1=data.frame(cbind(mydata_num_vars, Vehicle_type_car=dummy_cat$Vehicle_type.Car,Tenure= mydata$Tenure))

View(mydata1)

#Assumptions Check

hist(mydata1$Sales_in_thousands)

mydata1$ln_sales <- log(mydata1$Sales_in_thousands)
hist(mydata1$ln_sales)

mydata1$Sales_in_thousands<-NULL
str(mydata1)

mydata1$Tenure <- as.numeric(mydata1$Tenure)

plot(mydata1$ln_sales, mydata1$Price_in_thousands)

cor_mat<-data.frame(cor(mydata1))
write.csv(cor_mat, "cor_mat.csv")

install.packages('corrplot')
require(corrplot)

ls("package:corrplot")

corrplot(cor(mydata1), method="number", number.font = 1)



######################################################
#Split the data into training & Testing
samp<-sample(1:nrow(mydata1), floor(nrow(mydata1)*0.7))

dev<-mydata1[samp,]
val<-mydata1[-samp,]

nrow(mydata1)
nrow(dev)
nrow(val)
names(mydata1)

############################################################
#Develop the model
fit<- lm(ln_sales~Price_in_thousands+Wheelbase+Vehicle_type_car,data=mydata1 )
summary(fit)



fit1<-lm(ln_sales ~., data=dev)
step(fit1, direction="both")

fit2 <- lm(formula = ln_sales ~ Price_in_thousands + Engine_size + Wheelbase + 
     Fuel_efficiency + Vehicle_type_car, data = dev)
summary(fit2)
#fit2<-lm(ln_sales ~ X__year_resale_value + Price_in_thousands + Engine_size + 
#     Horsepower + Length + Fuel_efficiency + Vehicle_type.Car, data=dev)

#summary(fit2)

dev1<-data.frame(cbind(dev,pred_ln_sales=predict(fit, newdata=dev), pred_sales = exp(predict(fit2, newdata=dev))))
View(dev1)



val1<-data.frame(cbind(val,pred_ln_sales=predict(fit, newdata=val), pred_sales = exp(predict(fit2, newdata=val))))
View(val1)

#MAPE
dev_mape = mean(abs(dev1$ln_sales - dev1$pred_ln_sales)/dev1$ln_sales)
val_mape = mean(abs(val1$ln_sales - val1$pred_ln_sales)/val1$ln_sales)

#RMSE
sqrt(mean((dev1$ln_sales - dev1$pred_ln_sales)**2))
sqrt(mean((val1$ln_sales - val1$pred_ln_sales)**2))

#Correlations
cor(dev1$ln_sales, dev1$pred_ln_sales)
cor(val1$ln_sales, val1$pred_ln_sales)

############################### SCoring Data sets/Predicting the sales using Mathematical Equation#####################################
#mydata1$Ln_pre_sales<- (-2.321593  +
#                          mydata1$Price_in_thousands* -0.054988 +
#                          mydata1$Engine_size*0.254696  +
#                          mydata1$Wheelbase*0.047546	+
#                          mydata1$Fuel_efficiency*0.068975+
#                          mydata1$VT*-0.573255)
#mydata1$Pre_sales= exp(mydata1$Ln_pre_sales);

#################### Creating Deciles####################################
# find the decile locations 
decLocations <- quantile(dev1$pred_ln_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
dev1$decile <- findInterval(dev1$pred_ln_sales,c(-Inf,decLocations, Inf))
require(dplyr)
dev1 %>% group_by(decile) %>% summarise(mean_sale=mean(ln_sales),mean_pred_sale = mean(pred_ln_sales)) %>% arrange(desc(decile))
View(dev1)

summary(dev1$decile)
xtabs(~decile,dev1)


##################################Decile Analysis Reports
install.packages('sqldf')
require(sqldf)
dev_DA <- sqldf("select decile, count(decile) as count, avg(pred_ln_sales) as avg_pre_sales,   avg(ln_sales) as sum_Actual_sales
                    from dev1
                    group by decile
                    order by decile desc")

View(dev_DA)
#######################################################

# find the decile locations 
decLocations <- quantile(val1$pred_ln_sales, probs = seq(0.1,0.9,by=0.1))
# use findInterval with -Inf and Inf as upper and lower bounds
val1$decile <- findInterval(val1$pred_ln_sales,c(-Inf,decLocations, Inf))
View(val1)

summary(val1$decile)
xtabs(~decile,val1)

val_DA <- sqldf("select decile, count(decile) as count, avg(pred_ln_sales) as avg_pre_sales,   avg(ln_sales) as sum_Actual_sales
                    from val1
                    group by decile
                    order by decile desc")

View(val_DA)

write.csv(mydata1_DA,"mydata1_DA.csv")

getwd()

###################################END OF REGRESSION case study 






fit <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type.Car, data=mydata1)
summary(fit) # show results

ls(fit)



# Other useful functions 
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

# Stepwise Regression
library(MASS)
step <- stepAIC(fit, direction="both")
step$anova # display results

View(mydata1)

fit <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands + Engine_size + 
            Wheelbase + Fuel_efficiency + Vehicle_type.Car ,data=mydata1)

summary(fit)