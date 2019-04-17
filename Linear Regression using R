mydata<-read.csv("C:/Users/Ravinder/Desktop/Linear Regression/car_sales.csv")
View(mydata)
str(mydata)

# user function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

vars <- c( "Sales_in_thousands" , "X__year_resale_value" ,  "Price_in_thousands",   
           "Engine_size" , "Horsepower", "Wheelbase" , "Width" ,"Power_perf_factor" , "Length" , "Curb_weight" , 
           "Fuel_capacity", "Fuel_efficiency" )

diag_stats<-t(data.frame(sapply(mydata[vars],mystats)))

View(diag_stats)

# Writing Summary stats to external file

write.csv(diag_stats, file = "diag_stats.csv")

## OUTLIER TREATMENT
mydata$Sales_in_thousands[mydata$Sales_in_thousands>257.086342425636] <-257.086342425636
mydata$X__year_resale_value[mydata$X__year_resale_value>52.4331275042866] <-52.4331275042866
mydata$Price_in_thousands[mydata$Price_in_thousands>70.4457144064253] <-70.4457144064253

## Missing value treatment

require(Hmisc)
mydata1<-data.frame(sapply(mydata[vars],function(x) impute(x, mean))) #Imputing missings with mean for IV's

mydat2<-cbind(mydata1,Vehicle_type=mydata$Vehicle_type )

#R code for categorical variables(Converting as factor variable)

mydat2$Vehicle_type <- factor(mydat2$Vehicle_type)

levels(mydat2$Vehicle_type) <- c("Car","Passenger")

hist(mydat2$Sales_in_thousands)
hist(log(mydat2$Sales_in_thousands))

mydat2$ln_sales<-log(mydat2$Sales_in_thousands)


corrm <- (cor(mydat2[, vars],use="pairwise.complete.obs"))

require(corrplot)
corrplot(cor(mydat2[, vars],use="pairwise.complete.obs"), method = "circle", tl.cex = 0.7)


set.seed(12345)
#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(mydat2), size = floor(0.70 * nrow(mydat2)))
train_ind

training<-mydat2[train_ind,]
testing<-mydat2[-train_ind,]



# Multiple Linear Regression

fit <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=training)

summary(fit) 

#VIF - variance inflation factor 
library(car)
vif(fit)

require(MASS)
step3<- stepAIC(fit,direction="both")

#Akaike Information criteria - Reward and Penalty for each variable factor


fit2 <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Wheelbase
           +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=training)

summary(fit2)
#Multicollinierity Check using VIF
library(car)
vif(fit2)

require(MASS)
step3<- stepAIC(fit2,direction="both")

?stepAIC()
ls(step3)
step3$anova

fit3<-lm(ln_sales ~ Price_in_thousands + Engine_size + Wheelbase + Fuel_efficiency + Vehicle_type, data = mydat2)

summary(fit3)

#Multicollinierity Check using VIF
library(car)
vif(fit3)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit3)

hist(fit3$residuals)


#######################SCORING USING PREDICT FUNCTION
t1<-cbind(training, pred_sales = exp(predict(fit3,training)))

View(t1)

t2<-cbind(testing, pred_sales=exp(predict(fit3,testing)))

View(t2)

##################################Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_sales,c(-Inf,decLocations, Inf))

t1$decile <- as.factor(t1$decile)
require(dplyr)
t1_DA1 <- dplyr::group_by(t1,decile) %>% summarise(count=length(decile),avg_pred_sales=mean(pred_sales), avg_actual_sales =mean(Sales_in_thousands))

View(t1_DA)
write.csv(t1_DA,"mydata1_DA.csv")


##################################Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_sales,c(-Inf,decLocations, Inf))

t2$decile <- as.factor(t2$decile)
t2_DA1 <- dplyr::group_by(t2,decile) %>% summarise(count=length(decile),avg_pred_sales=mean(pred_sales), avg_actual_sales =mean(Sales_in_thousands))

View(t2_DA1)
write.csv(t2_DA,"t2_DA.csv")






