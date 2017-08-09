# INTRODUCTION
# Lending Club, a San Francisco-based fintech company, works to 
# facilitate peer-to-peer loans through their online lending platform. 
# It is now the largest online loan platform
# --------------------------------------------------------------------------------------------------------------------
# DATASET EXPLANATION
# Obtained from Kaggle. Each record has complete loan information from 2007 to 2015
# 887379 records and 74 columns
# --------------------------------------------------------------------------------------------------------------------
# PREDICTION QUESTION
# Which loans will be paid back in full and which will default?
# --------------------------------------------------------------------------------------------------------------------
# LIBRARIES USED
library(gdata)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tibble)  
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(ROCR)
library(RColorBrewer)			
library(party)					
library(partykit)
library(AUC)
library(ROSE)
library(MASS)
library(ipred)

#--------------------------------------------------------------------------------------------------------------------
# DATA PREPARATION
# read the file
loanData <- read.csv("/Users/aparnamanohar/Desktop/lending-club-loan-data/loan.csv")

# making sure the class of the type of the object is data frame
class(loanData)

# Checking dimensions
dim(loanData) # 887379 records and 74 columns

ld <- loanData
colnames(loanData) # all 74 variables

#choosing only required columns related to loan amount which are 
# loan_status	        --- The final status of the loan
# loan_amnt	            --- The total principal of the loan
# term                  ---	The term length of the loan. Either 36-months (standard) or 60-months
# int_rate	            --- Interest Rate
# installment	        --- Dollar amount of montly payment isntallments
# grade	                --- The quality of the loan (as determined by Lending Club) - A, B, C, etc.
# sub-grade	            --- A further partitioning of loan quality - A1, A2, etc.
# emp_length	        --- Borrower???s length of employment.
# home_ownership        --- Borrwer???s home status (ie own, mortgage, rent)
# annual_inc	        --- Borrower???s reported annual income
# verification_status   --- Status of Lending Club???s verification of the borrower
# issue_d	            --- Loan issue date
# dti	                --- Monthly Debt Payments divided by reported Monthly Income
# earliest.cr.line	    --- Date of borrower???s earliest credit line
# open_acc	            --- Number of open credit lines on the borrower???s credit file.
# revol_bal	            --- Total credit revolving balance.
# revol_util	        --- Revolving Line Utilization Rate
# total_acc	            --- Total number of historical credit lines on the borrower???s file


ld1 = ld[,c(3, 6:10, 12:17, 25, 27, 31, 33:35)]
str(ld1)

# The main column is status which is what we are trying to predict(column 17)
levels(ld1$loan_status)
# "Charged Off"                                         "Current"                                            
# "Default"                                             "Does not meet the credit policy. Status:Charged Off"
# "Does not meet the credit policy. Status:Fully Paid"  "Fully Paid"                                         
# "In Grace Period"                                     "Issued"                                             
# "Late (16-30 days)"                                   "Late (31-120 days)"

# dropping the columns which do not meet the credit policy, late statuses and current and issued ones
datNew1 <- droplevels( ld1[-which(ld1$loan_status == "Does not meet the credit policy. Status:Charged Off"),])
datNew2 <- droplevels( datNew1[-which(datNew1$loan_status == "Does not meet the credit policy. Status:Fully Paid"),])
datNew3 <- droplevels( datNew2[-which(datNew2$loan_status == "In Grace Period"),])
datNew4 <- droplevels( datNew3[-which(datNew3$loan_status == "Issued"),])
datNew5 <- droplevels( datNew4[-which(datNew4$loan_status == "Late (31-120 days)"),])
datNew6 <- droplevels( datNew5[-which(datNew5$loan_status == "Late (16-30 days)"),])
finaldf <- droplevels( datNew6[-which(datNew6$loan_status == "Current"),])

# levels of final df
levels(finaldf$loan_status) # "Charged Off" "Default"     "Fully Paid"

# combining the levels of default and charged off
levels(finaldf$loan_status) <- c("Charged Off", "Charged Off", "Fully Paid")
levels(finaldf$loan_status) # "Charged Off" "Fully Paid"  

# tabling them we get
table(finaldf$loan_status) # Charged Off - 46467  Fully Paid - 207723

# Removing outliers considering Annual Income using box plot
boxplot(finaldf$annual_inc)
df <- subset(finaldf, finaldf$annual_inc < 1500000)
boxplot(df$annual_inc)

# Calculating credit history
head(df$issue_d)
df$issue_d <- as.character(df$issue_d)
df$issue_d <- paste(df$issue_d, "-01", sep = "")
df$issue_d <- parse_date_time(df$issue_d, "myd")
head(df$issue_d)

df$earliest_cr_line <- as.character(df$earliest_cr_line)
df$earliest_cr_line <- paste(df$earliest_cr_line, "-01", sep = "")
df$earliest_cr_line <- parse_date_time(df$earliest_cr_line, "myd")

df$time_since_first_credit <- df$issue_d - df$earliest_cr_line
df$time_since_first_credit <- as.numeric(df$time_since_first_credit)

head(df$time_since_first_credit)

df <- df %>% filter(time_since_first_credit > 0)
head(df)

# Calculation Current Account Ratio which  was calculated by dividing 
# open.acc (the number of open credit lines the borrower had at the time of the loan) 
# by total.acc (the total number of credit lines the borrower has had)

df$current_account_ratio <- df$open_acc/df$total_acc
head(df$current_account_ratio)

str(df) # 254174 obs. of  20 variables

# Removing records which have na values
df1 <- na.omit(df)
str(df1) # 253974 obs. of  20 variables

# EXPLORATORY ANALYSIS

table(df$loan_status)
# Plot of rate of interest vs the loan status, we can see that higher the interest rates, more likely that it gets
# charged off
ggplot(df1, aes(int_rate, col = loan_status)) + geom_histogram(bins = 30) + facet_grid(loan_status ~ .)

# Plot of rate of interest vs status of the loan considering the grade of the loan using histogram
table(df1$loan_status, df1$grade)
ggplot(df1, aes(x = int_rate)) + geom_histogram(aes(fill = grade), bins = 30) + facet_grid(loan_status ~ .)

# Plot of term vs status of the loan as per amount using box plot
ggplot(df1, aes(term, loan_amnt)) + geom_boxplot(aes(fill = loan_status)) + facet_grid(loan_status ~ .)

# Scatter plot of records based on interest rate and status of the loan  
a <- df1[sample(1:nrow(df1), 1000),]
ggplot(a, aes(int_rate, loan_amnt)) + geom_point(aes(color = term))

# Sampling and splitting the data into training, testing and validation
df2 <- sample(3, nrow(df1), replace = TRUE, prob = c(0.6,0.3,0.1))
train <- df1[df2 == 1,]
nrow(train)
test <- df1[df2 == 2,]
nrow(test)
table(test$loan_status)
validate <- df1[df2 == 3,]
nrow(validate)

# DECISION TREE ANALYSIS
dectree1 <- rpart(loan_status ~ ., data = train, method="class")
pred1 <- predict(dectree1, test,  type = "class")
confusionMatrix(pred1,test$loan_status) # accuracy is 81% and sensitivity is 0.02
# indicating that we cannot consider this model as we would likely want to have that value closer to one

plot(dectree1) # error
rpart.plot(dectree1) # This plot has only one root

# controlling the decision tree with more parameters
dectree2 <- rpart(loan_status ~ . , data = train, control=rpart.control(minsplit=10, minbucket = 3, cp=0.0006))
pred2 <- (predict(dectree2, test, type = "class"))
confusionMatrix(pred2,test$loan_status) # accuracy 81%, sensitivity 0.02
rpart.plot(dectree2)
# Same as before

# Considering another measure of accuracy of the model called Area of the curve from ROC
# Making transformations to calculate the Area Under the Curve
pred3 <- predict(dectree2, test, type = "prob")[,2] 
predi <- prediction(pred3, test$loan_status) 
plot(performance(predi, "tpr", "fpr"))
abline(0, 1, lty = 2)
performance(predi, measure = "auc")@y.values # The value is 0.64

# We get this because the dataset is largely imbalanced
# We look at the distribution of data and see that there are more values of Fully paid loans than of Charged Off ones
# We can Over sample the data or Under Sample the data or do both and try creating the decision tree model

# Performing Oversampling
dfoversampled <- ovun.sample(loan_status ~ ., data = train, method = "over", N = 249350)$data
table(train$loan_status) # Charged off- 27909 Fully Paid- 124673
table(dfoversampled$loan_status) # Charged off- 124299 Fully Paid- 125051

# Tuning the tree for a better structure
tune <- data.frame(0.001)
tune
colnames(tune) <- "cp"
tr_control <- trainControl(method = "cv",number = 10, verboseIter = TRUE)

# Creating the oversampled tree
oversampledTree <- train(loan_status ~., data = dfoversampled, 
                                 method = "rpart", trControl = tr_control, tuneGrid = tune, 
                                 control=rpart.control(minsplit=10, minbucket = 3))

# Predicting with the test data
pred4 = predict(oversampledTree, test, type = "prob")[,2]
confusionMatrix(predict(oversampledTree, test), test$loan_status) # accuracy 62%

# Calculating Area under the curve value
auc(test$loan_status, pred4) # Area under the curve: 0.6827 
# This is better than the decision tree with original imbalanced dataset

# Performing Undersampling
dfundersampled <- ovun.sample(loan_status ~ ., data = train, method = "under", N = 55850)$data
table(train$loan_status)
table(dfundersampled$loan_status) # Fully Paid - 28108, Charged Off- 27742 

# Creating the undersampled tree
undersampledTree <- train(loan_status ~., data = dfundersampled, 
                             method = "rpart", trControl = tr_control, tuneGrid = tune, 
                             control=rpart.control(minsplit=10, minbucket = 3))
pred5 <- predict(undersampledTree, test, type = "prob")[,2]
# Checking senstivity, specificity and accuracy values
confusionMatrix(predict(undersampledTree, test), test$loan_status) #accuracy 63%

# Calculating AUC value to measure accuracy
auc(test$loan_status, pred5) # Area under the curve: 0.6817 which is lesser than the oversampled tree

# Both oversampling and undersampling
dfbothsampled <- ovun.sample(loan_status ~ ., data = train, method = "both", N = 120000)$data
table(train$loan_status)
table(dfbothsampled$loan_status) # Fully Paid- 60068 Charged Off- 59932

# Creating the tree  
bothsampledTree <- train(loan_status ~., data = dfbothsampled, 
                             method = "rpart", trControl = tr_control, tuneGrid = tune, 
                             control=rpart.control(minsplit=10, minbucket = 3))
pred6 <- predict(bothsampledTree, test, type = "prob")[,2]
# Checking senstivity, specificity and accuracy values
confusionMatrix(predict(bothsampledTree, test), test$loan_status) #accuracy 62%
# Calculating AUC value to measure accuracy
auc(test$loan_status, pred6) # Area under the curve: 0.6877 which is the best of the three

# LOGISTIC REGRESSION
# We found that when sampled using the method "both", we get a better AUC value, I am using that for logistic regression
dfglm <- glm(loan_status ~ ., data = dfoversampled, family = "binomial")

# Converting that to caret using train function
dfglm1 <- train(loan_status ~ loan_amnt + term + int_rate + installment + sub_grade + 
                              emp_length + home_ownership + annual_inc + verification_status + 
                              dti + open_acc + revol_bal + revol_util + total_acc + time_since_first_credit + 
                              current_account_ratio, data = dfoversampled, method = "glm")

# Making a confusion matrix of the values to find accuracy
confusionMatrix(predict(dfglm1, test), test$loan_status) # accuracy is 63%

# Traansformation of data for calculating area under the curve
dfglm1prediction <- prediction(predict(dfglm1, newdata = test, type = "prob")[,"Fully Paid"], test$loan_status)
performance(dfglm1prediction, measure = "auc")@y.values # Area underthe curve is 0.70 which is the best so far

# ENSEMBLE MODELING
# Creating an bootstrap aggregation decision tree model with oversampled data using the library ipred
dfbagging <- bagging(loan_status ~ ., data = dfoversampled, coob = TRUE, nbagg = 25, 
                     control=rpart.control(minsplit=10, minbucket = 3, cp = 0.001)) 

# Transforming the data and with the predictions, calculating the area under the curve
pred <- predict(dfbagging, type = "prob", newdata = test)
baggingPred <- prediction(pred [,1], test$loan_status)
bagperf <- performance(baggingPred, "auc")
testbag <- predict(dfbagging, newdata = test)
table(testbag, test$loan_status)
bagperf@y.values # Area under the curve value - 0.67

# Taking a look at the importance of each of the factors of the bagged model
varImp(dfbagging) # grade, subgrade, interst rate, and term are the most important features

# From my analysis I can say that Logistic Regression model could be considered to predict loan outcomes
# Validating the model with the validation dataset we get the following area under the curve
dfglm <- glm(loan_status ~ ., data = validate, family = "binomial")
dfglm1 <- train(loan_status ~ loan_amnt + term + int_rate + installment + sub_grade + 
                    emp_length + home_ownership + annual_inc + verification_status + 
                    dti + open_acc + revol_bal + revol_util + total_acc + time_since_first_credit + 
                    current_account_ratio, data = validate, method = "glm")

# Making a confusion matrix of the values to find accuracy
confusionMatrix(predict(dfglm1, validate), validate$loan_status) # misleading accuracy

# Traansformation of data for calculating area under the curve
dfglm1prediction <- prediction(predict(dfglm1, newdata = validate, type = "prob")[,"Fully Paid"], validate$loan_status)
performance(dfglm1prediction, measure = "auc")@y.values # Area underthe curve is 0.70 




