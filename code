```
loan_data = readRDS("loandata.rds")
library(gmodels)
```
#Number and Percent of defaulted and non defaulted loans
```
CrossTable(loan_data$loan_status)
```

   Cell Contents
|-------------------------|
|                       N |
|         N / Table Total |
|-------------------------|


Total Observations in Table:  29092

|         0 |         1 |
          |-----------|-----------|
          |     25865 |      3227 |
          |     0.889 |     0.111 |
          |-----------|-----------|

```
CrossTable(loan_data$grade,loan_data$loan_status,prop.r=TRUE,prop.t=FALSE,prop.c=FALSE,prop.chisq=FALSE)

```



https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img1.png

```
hist_1$breaks
```
[1]     0  2000  4000  6000  8000 10000 12000 14000 16000 18000 20000 22000 24000
[14] 26000 28000 30000 32000 34000 36000


#Increasing number of breaks
```

hist_2 <- hist(loan_data$loan_amnt, breaks = 200, xlab = "Loan amount",
main = "Histogram of the loan amount",col="beige")
```

https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img2.png

```
plot(loan_data$age,ylab="Age")
```
https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img3.png

In the above image we see that there is an outlier with age>140. In the next
step we remove that outlier.

```
index_highage=which(loan_data$age>122)
 new_data <- loan_data[-index_highage, ]
 plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual Income")
 ```

https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img4.png

```
plot(new_data$age, new_data$annual_inc, xlab = "Age", ylab = "Annual Income")
```
https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img5.png

```
loan_data=new_data
```
#replacing the missing values with the median values
```
median_ir=median(loan_data$int_rate,na.rm=TRUE)
 loan_data_replace <- loan_data
 loan_data_replace$int_rate[na_index] <- median_ir
 summary(loan_data_replace$int_rate)
 ```

 #Splitting the data set into training set and test set
 ```
 set.seed(567)
index_train=sample(1:nrow(loan_data),2/3* nrow(loan_data))
training_set <- loan_data[index_train, ]
test_set = loan_data[-index_train, ]
```

#coarse classification
```
loan_data$emp_cat <- rep(NA, length(loan_data$emp_length))
loan_data$emp_cat[which(loan_data$emp_length <= 15)] <- "0-15"
loan_data$emp_cat[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
loan_data$emp_cat[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
loan_data$emp_cat[which(loan_data$emp_length > 45)] <- "45+"
loan_data$emp_cat[which(is.na(loan_data$emp_length))] <- "Missing"
loan_data$emp_cat <- as.factor(loan_data$emp_cat)
loan_data$emp_length=NULL
```
```
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))
loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"
loan_data$ir_cat <- as.factor(loan_data$ir_cat)
loan_data$int_rate=NULL
```

```
log_model_full=glm(loan_status~.,family="binomial",data=training_set)
summary(log_model_full)
```
https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img7.png

```
range(predictions_all_full)
```
[1] 8.369797e-06 5.141640e-01

```
pred_cutoff_15=ifelse(predictions_all_full>0.15,1,0)
conf = table(test_set$loan_status,pred_cutoff_15)
conf

```
https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img8.png

```
acc <- sum(diag(conf)) / nrow(test_set)
acc
```
[1] 0.7364133   ACCURACY

#USING DECISION TREES FOR PREDICTION

We load rpart, rattle, rpart.plot packages
```
library(rpart)
library(rattle)
library(rpart.plot)
```
 #Using all the features in the set, we build the decision tree model
 ```
 tree <- rpart(loan_status ~ ., method = "class", data = training_set)
pred <- predict(tree,newdata=test_set,type="class")
conf_d=table(test_set$loan_status,pred)
acc_d<- sum(diag(conf_d)) / nrow(test_set)
```

[1] 0.8930597   ACCURACY

#Trying variations with the model
```
tree_weights <- rpart(loan_status ~ ., method = "class", data =training_set,parms = list(prior=c(0.7,0.3)), control = rpart.control(cp = 0.001659))
plotcp(tree_weights)
```
https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img9.png

```
pred_1 <- predict(tree_weights,newdata=test_set,type="class")
conf_d1=table(test_set$loan_status,pred_1)
acc_d1<- sum(diag(conf_d1)) / nrow(test_set)
acc_d1
```
[1] 0.8675879   ACCURACY

```
rpart.plot(tree_weights)
```
https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img10.png
```
fancyRpartPlot(tree_weights)

```

https://github.com/anagar20/Credit-Risk-Modeling/blob/master/images/img11.png
