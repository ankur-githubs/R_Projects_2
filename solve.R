setwd("E:\\R_class\\linear regresion\\Boot Camp - 27th August 2018 - Linear Regression\\Case Study - Housing Example")
house_data <- read.csv("House_Prices.csv")
names(house_data)
str(house_data)


house_data$Bedrooms <- as.factor(house_data$Bedrooms)
house_data$Bathrooms <- as.factor(house_data$Bathrooms)
house_data$Offers<- as.factor(house_data$Offers)


summary(house_data)

require(psych)
describe(house_data)

my_stat_num <- function(x){
  var_type <- class(x)
  count <- length(x)
  nmiss <- sum(is.na(x))
  Mean <- mean(x,na.rm=T)
  Sum <- sum(x,na.rm=T)
  Sd <-sd(x,na.rm=T)
  Var <- var(x,na.rm=T)
  Min <- min(x,na.rm=T)
  Pct <- quantile(x,probs = c(0.1,0.5,0.10,0.25,0.50,0.75,0.95,0.99),na.rm = T)
  Max <- max(x,na.rm=T)
  return(c(Type=var_type,miss=nmiss,Count=count,mean=Mean,sum=Sum,sd=Sd,var=Var,min=Min,pct=Pct,max=Max))
}

my_stat_cat <- function(x){
  var_type <- class(x)
  count <- length(x)
  nmiss <- sum(is.na(x))
  Freq <- list(table(x))
  Prop <- list(prop.table(table(x)))
  return(c(var=var_type,count=count,nmiss=nmiss,freq=Freq,prop=Prop))
}

sapply(mydata, FUN=is.numeric)
sum(is.numeric(mydata))

numeric_col <- names(house_data)[sapply(house_data,is.numeric)]
catogry_col <- names(house_data)[sapply(house_data,is.factor)]

summry_num_col <- apply(X = house_data[numeric_col],2,FUN = my_stat_num)
summry_cat_col <- apply(X = house_data[catogry_col],2,FUN = my_stat_cat)


write.csv(house_data[numeric_col],"summry_num_col.csv" )

con <- file("cat_var.txt")
sink(con,append = T)
summry_cat_col
sink()

sum(is.na(summry_num_col))


describe(house_data[numeric_col])

outlier_treat <- function(x){
  UC <- quantile(x, p=0.99,na.rm=T)
  LC <- quantile(x, p=0.01,na.rm=T) 
  x <- ifelse(x>UC,UC,x)
  x <- ifelse(x<LC,LC,x)
  return(x)
}

outliar_housedata_num <- apply(house_data[numeric_col],2,FUN = outlier_treat)


sum(is.na(house_data[numeric_col]))
sum(is.na(house_data[catogry_col]))


hist(log(house_data$Price),breaks = 50)
shapiro.test(log(house_data$Price))


house_data_num <- data.frame(house_data[numeric_col])
hist(house_data_num$Price)
cor_mat <- cor(house_data_num)
write.csv(cor_mat,"cor_mat.csv")

require(corrplot)
corrplot(cor(cor_mat), method="circle", number.font = 1)

names(house_data[numeric_col])
summary(aov(house_data$Price~house_data$Home)) #significant
summary(aov(house_data$Price~house_data$SqFt)) #significant

require(caret)
house_data$Bedrooms <- factor(house_data$Bedrooms)
dv1 <- dummyVars(~Bedrooms ,data = house_data[catogry_col])

dumg_loan_len <- data.frame(predict(dv1,house_data[catogry_col]))[-1]

house_data$Bathrooms <- factor(house_data$Bathrooms)
dv2 <- dummyVars(~Bathrooms ,data = house_data[catogry_col])

dumg_loan_len2 <- data.frame(predict(dv2,house_data[catogry_col]))[-1]

house_data$Offers <- factor(house_data$Offers)
dv3 <- dummyVars(~Offers ,data = house_data[catogry_col])

dumg_loan_len3 <- data.frame(predict(dv3,house_data[catogry_col]))[-1]


house_data$Brick <- factor(house_data$Brick)
dv4 <- dummyVars(~Brick ,data = house_data[catogry_col])

dumg_loan_len4 <- data.frame(predict(dv4,house_data[catogry_col]))[-1]


house_data$Neighborhood <- factor(house_data$Neighborhood)
dv5 <- dummyVars(~Neighborhood ,data = house_data[catogry_col])

dumg_loan_len5 <- data.frame(predict(dv5,house_data[catogry_col]))[-1]


comb_cat_var <- as.data.frame(cbind(house_data_num,dumg_loan_len,dumg_loan_len2,dumg_loan_len3,dumg_loan_len4,dumg_loan_len5 ))
names(comb_cat_var) <- gsub(pattern = ".",replacement = "_",x = names(comb_cat_var) , fixed = T)
comb_cat_var$Home <- as.numeric(comb_cat_var$Home )
comb_cat_var$Price <- as.numeric(comb_cat_var$Price )
comb_cat_var$SqFt <- as.numeric(comb_cat_var$SqFt )
str(comb_cat_var)


sample <- sample(1:nrow(comb_cat_var),floor(nrow(comb_cat_var)*0.7))
train <- comb_cat_var[sample,]
names(train) <- gsub(pattern = ".",replacement = "_",x = names(train) , fixed = T)
test <- comb_cat_var[-sample,]
names(test) <- gsub(pattern = ".",replacement = "_",x = names(test) , fixed = T)

names(comb_cat_var)
fit <- lm(Price~.,data =train)
summary(fit)
residuals(fit)

step(fit,direction = "both")

fit2 <- lm(formula = Price ~ SqFt + Bedrooms_4 + Bedrooms_5 + Bathrooms_3 + Offers_2 + 
             Offers_3 + Offers_4 + Offers_5 + Brick_Yes + Neighborhood_West,data =comb_cat_var)
summary(fit2)


require(car)
car::vif(fit2)



prid_rate <- predict(fit2 , newdata = train)
prid_rate <- predict(fit2 , newdata = test)

train1 <- as.data.frame(cbind(train,prid_rate = predict(fit2 , newdata = train)))
test1 <- as.data.frame(cbind(test,prid_rate=predict(fit2,newdata = test)))


train_mape_res <- mean(abs(train1$Price-train1$prid_rate/train1$Price))
test_mape_res <- mean(abs(test1$Price-test1$prid_rate/test1$Price))



train_rmse_var <- sqrt(mean((train1$Price-train1$prid_rate)**2))
test_rmse_var <- sqrt(mean((test1$Price-test1$prid_rate)**2))



cor(train1$Price,train1$prid_rate)
cor(test1$Interest_Rate,test1$pred_intrest_rate)

plot(train1$Price,train1$prid_rate)
fitx <- lm(train1$Price~train1$prid_rate)
abline(fitx)

train_decile_loc <- quantile(train1$prid_rate,probs = seq(0.1,0.9,by = 0.1))
train1$decile_loc1 <- findInterval(train1$prid_rate,vec = c(-Inf,train_decile_loc,Inf))
require(dplyr)
train_decile_analysis <-  train1 %>% group_by(decile_loc1) %>% summarise(count=n(),avg_prid_rate = mean(prid_rate,na.rm = T), avg_rate= mean(Price,na.rm = T)) %>% arrange(desc(decile_loc1))


