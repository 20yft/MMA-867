if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","rpart", "ROCR","lift","glmnet","MASS","e1071","randomForest","xgboost") #Check, and if needed install the necessary packages

## Model without gender variable ###############

Creditdata <-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE) # Load the datafile to R

dim(Creditdata) #Verifying the dimension of the data set

anyNA(Creditdata) #Checking NAs in the data set

summary(Creditdata) #summary of the data set

head(Creditdata) #first 6 lines of the data set

str(Creditdata) #structure of the data set, See if some data types were misclassified when importing data from CSV

#Feature Engineering

table(Creditdata$MARRIAGE)
table(Creditdata$EDUCATION)
table(Creditdata$AGE)
table(Creditdata$LIMIT_BAL)
summary(Creditdata$AGE)
summary(Creditdata$LIMIT_BAL)
table(Creditdata$LIMIT_BAL_SCR)

unique(Creditdata$MARRIAGE)

# Updating Marriage 0 to 3, converting Marriage variable to factor
Creditdata$MARRIAGE[Creditdata$MARRIAGE == 0] <- 3

Creditdata$MARRIAGE[Creditdata$MARRIAGE == 1] <- "Married"
Creditdata$MARRIAGE[Creditdata$MARRIAGE == 2] <- "Single"
Creditdata$MARRIAGE[Creditdata$MARRIAGE == 3] <- "Other"
Creditdata$MARRIAGE <- as.factor(Creditdata$MARRIAGE)


# Updating Education 0 to 1, converting Education variable to factor
unique(Creditdata$EDUCATION)
Creditdata$EDUCATION[Creditdata$EDUCATION == 5] <- 4
Creditdata$EDUCATION[Creditdata$EDUCATION == 6] <- 4
# Creditdata$Education_Score <- recode(Creditdata$EDUCATION, '1'=2, '2'=2,'3'=1,'4'=1,'5'=0,'6'=0,'0'=0)
Creditdata$EDUCATION[Creditdata$EDUCATION == 0] <- 1
Creditdata$EDUCATION <- as.factor(Creditdata$EDUCATION)

# Scoring the customer based on the Delinquency Level
unique(Creditdata$PAY_1)
Creditdata$Pay_1_Score <- ifelse(Creditdata$PAY_1 <= 0,1,0)

unique(Creditdata$PAY_2)
Creditdata$Pay_2_Score <- ifelse(Creditdata$PAY_2 <= 0,1,0)

unique(Creditdata$PAY_3)
Creditdata$Pay_3_Score <- ifelse(Creditdata$PAY_3 <= 0,1,0)

unique(Creditdata$PAY_4)
Creditdata$Pay_4_Score <- ifelse(Creditdata$PAY_4 <= 0,1,0)

unique(Creditdata$PAY_5)
Creditdata$Pay_5_Score <- ifelse(Creditdata$PAY_5 <= 0,1,0)

unique(Creditdata$PAY_6)
Creditdata$Pay_6_Score <- ifelse(Creditdata$PAY_6 <= 0,1,0)

# Pay_1 and Pay_2 Comparison
Creditdata$Pay1Pay2_Score <- ifelse(Creditdata$PAY_1 > Creditdata$PAY_2, 0,1)

# Pay 1 and Pay_3 Comparison
Creditdata$Pay1Pay3_Score <- ifelse(Creditdata$PAY_1 > Creditdata$PAY_3, 0,1)

# Pay 4, Pay 5 and Pay_6 Comparison
Creditdata$Pay456_Score <- ifelse(Creditdata$PAY_4 > 0 & Creditdata$PAY_5 >0 & Creditdata$PAY_6 >0, 0,1)

# Limit_Balance and Pay_1 Comparison
Creditdata$LimPay_1_Score <- ifelse(Creditdata$LIMIT_BAL <= 50000 & Creditdata$PAY_1 > 0, 0,1)



# Scoring Customrs based on the Credit Limit Range
Limit_Bal <- Creditdata$LIMIT_BAL

Limit_Bal[Limit_Bal > 9000 & Limit_Bal <= 20000] <- 1
Limit_Bal[Limit_Bal > 20000 & Limit_Bal <= 30000] <- 2
Limit_Bal[Limit_Bal > 30000 & Limit_Bal <= 40000] <- 3
Limit_Bal[Limit_Bal > 40000 & Limit_Bal <= 50000] <- 4
Limit_Bal[Limit_Bal > 50000 & Limit_Bal <= 60000] <- 5
Limit_Bal[Limit_Bal > 60000 & Limit_Bal <= 70000] <- 6
Limit_Bal[Limit_Bal > 70000 & Limit_Bal <= 80000] <- 7
Limit_Bal[Limit_Bal > 80000 & Limit_Bal <= 90000] <- 8
Limit_Bal[Limit_Bal > 90000 & Limit_Bal <= 100000] <- 9
Limit_Bal[Limit_Bal > 100000 & Limit_Bal <= 110000] <- 10
Limit_Bal[Limit_Bal > 110000 & Limit_Bal <= 120000] <- 11
Limit_Bal[Limit_Bal > 120000 & Limit_Bal <= 130000] <- 12
Limit_Bal[Limit_Bal > 130000 & Limit_Bal <= 140000] <- 13
Limit_Bal[Limit_Bal > 140000 & Limit_Bal <= 150000] <- 14
Limit_Bal[Limit_Bal > 150000 & Limit_Bal <= 160000] <- 15
Limit_Bal[Limit_Bal > 160000 & Limit_Bal <= 170000] <- 16
Limit_Bal[Limit_Bal > 170000 & Limit_Bal <= 180000] <- 17
Limit_Bal[Limit_Bal > 180000 & Limit_Bal <= 190000] <- 18
Limit_Bal[Limit_Bal > 190000 & Limit_Bal <= 200000] <- 19
Limit_Bal[Limit_Bal > 200000 & Limit_Bal <= 210000] <- 20
Limit_Bal[Limit_Bal > 210000 & Limit_Bal <= 220000] <- 21
Limit_Bal[Limit_Bal > 220000 & Limit_Bal <= 230000] <- 22
Limit_Bal[Limit_Bal > 230000 & Limit_Bal <= 240000] <- 23
Limit_Bal[Limit_Bal > 240000 & Limit_Bal <= 250000] <- 24
Limit_Bal[Limit_Bal > 250000 & Limit_Bal <= 260000] <- 25
Limit_Bal[Limit_Bal > 260000 & Limit_Bal <= 270000] <- 26
Limit_Bal[Limit_Bal > 270000 & Limit_Bal <= 280000] <- 27
Limit_Bal[Limit_Bal > 280000 & Limit_Bal <= 290000] <- 28
Limit_Bal[Limit_Bal > 290000 & Limit_Bal <= 300000] <- 29
Limit_Bal[Limit_Bal > 300000 & Limit_Bal <= 310000] <- 30
Limit_Bal[Limit_Bal > 310000 & Limit_Bal <= 320000] <- 31
Limit_Bal[Limit_Bal > 320000 & Limit_Bal <= 330000] <- 32
Limit_Bal[Limit_Bal > 330000 & Limit_Bal <= 340000] <- 33
Limit_Bal[Limit_Bal > 340000 & Limit_Bal <= 350000] <- 34
Limit_Bal[Limit_Bal > 350000 & Limit_Bal <= 360000] <- 35
Limit_Bal[Limit_Bal > 360000 & Limit_Bal <= 370000] <- 36
Limit_Bal[Limit_Bal > 370000 & Limit_Bal <= 380000] <- 37
Limit_Bal[Limit_Bal > 380000 & Limit_Bal <= 390000] <- 38
Limit_Bal[Limit_Bal > 390000 & Limit_Bal <= 400000] <- 39
Limit_Bal[Limit_Bal > 400000 & Limit_Bal <= 410000] <- 40
Limit_Bal[Limit_Bal > 410000 & Limit_Bal <= 420000] <- 41
Limit_Bal[Limit_Bal > 420000 & Limit_Bal <= 430000] <- 42
Limit_Bal[Limit_Bal > 430000 & Limit_Bal <= 440000] <- 43
Limit_Bal[Limit_Bal > 440000 & Limit_Bal <= 450000] <- 44
Limit_Bal[Limit_Bal > 450000 & Limit_Bal <= 460000] <- 45
Limit_Bal[Limit_Bal > 460000 & Limit_Bal <= 470000] <- 46
Limit_Bal[Limit_Bal > 470000 & Limit_Bal <= 480000] <- 47
Limit_Bal[Limit_Bal > 480000 & Limit_Bal <= 490000] <- 48
Limit_Bal[Limit_Bal > 490000 & Limit_Bal <= 500000] <- 49
Limit_Bal[Limit_Bal > 500000 & Limit_Bal <= 550000] <- 50
Limit_Bal[Limit_Bal > 550000 & Limit_Bal <= 600000] <- 51
Limit_Bal[Limit_Bal > 600000 & Limit_Bal <= 700000] <- 52
Limit_Bal[Limit_Bal > 700000 & Limit_Bal <= 1000000] <- 53

Creditdata$LIMIT_BAL_SCR <- Limit_Bal

# Updating Sex Variable to Male and Female, changing the data type of Sex variable t0 factor
Creditdata$SEX[Creditdata$SEX == 1] <- "Male"
Creditdata$SEX[Creditdata$SEX == 2] <- "Female"
Creditdata$SEX <- as.factor(Creditdata$SEX)

# Calculating Total Bill Amount and Total Payment
Total_Payment <- Creditdata$PAY_AMT1 + Creditdata$PAY_AMT2 + Creditdata$PAY_AMT3 + Creditdata$PAY_AMT4 + Creditdata$PAY_AMT5 + Creditdata$PAY_AMT6 
Total_Bill_Amount <- Creditdata$BILL_AMT1 + Creditdata$BILL_AMT2 + Creditdata$BILL_AMT3 + Creditdata$BILL_AMT4 + Creditdata$BILL_AMT5 + Creditdata$BILL_AMT6

# Scoring customers based on the total bill amount and the total payment
Creditdata$Payment_Score <- ifelse(Total_Bill_Amount <= 0, 1, ifelse(Total_Bill_Amount>0 & (Total_Payment>=Total_Bill_Amount), 1, 0))

# Scoring customers based on the Balance
Creditdata$Balance_Score <- ifelse(Total_Bill_Amount <0 | Creditdata$LIMIT_BAL - Total_Bill_Amount > 0,1,0)

# Calculating the Credit Limit Utilization
Creditdata$Credit_Utilization <- ifelse(Total_Bill_Amount<=0, 0, ifelse((Total_Bill_Amount > 0 & Total_Bill_Amount<Total_Payment),0, (Total_Bill_Amount - Total_Payment / Creditdata$LIMIT_BAL)))

# Scoring customers based on the Credit Limit Utilization
Creditdata$Credit_Utilization_Score <- ifelse(Creditdata$Credit_Utilization > Creditdata$LIMIT_BAL, 0,1)

# Calculating the Percentage Credit Limit Utilization
Creditdata$P_Credit_Utilization <- ifelse(Creditdata$Credit_Utilization <= 0, 0, ((Total_Bill_Amount - Total_Payment)/Creditdata$LIMIT_BAL))

# Scoring customers based on the Percentage Credit Limit Utilization
Creditdata$Credit_Utilization_PScore <- ifelse(Creditdata$P_Credit_Utilization < 0.8, 1, 0)

# Scoring based on Monthly Payment
Creditdata$PAY_AMT1_Score <- ifelse(Creditdata$BILL_AMT1 > 0 & Creditdata$PAY_AMT1 <= 0, 0, 1)
Creditdata$PAY_AMT2_Score <- ifelse(Creditdata$BILL_AMT2 > 0 & Creditdata$PAY_AMT2 <= 0, 0, 1)
Creditdata$PAY_AMT3_Score <- ifelse(Creditdata$BILL_AMT3 > 0 & Creditdata$PAY_AMT3 <= 0, 0, 1)
Creditdata$PAY_AMT4_Score <- ifelse(Creditdata$BILL_AMT4 > 0 & Creditdata$PAY_AMT4 <= 0, 0, 1)
Creditdata$PAY_AMT5_Score <- ifelse(Creditdata$BILL_AMT5 > 0 & Creditdata$PAY_AMT5 <= 0, 0, 1)
Creditdata$PAY_AMT6_Score <- ifelse(Creditdata$BILL_AMT6 > 0 & Creditdata$PAY_AMT6 <= 0, 0, 1)


# Categorizing Customers based on the age
age <- Creditdata$AGE

age <- cut(age,6, labels = c("Between 21 and 30", "Between 31 and 40","Between 41 and 50", "Between 51 and 60",
                             "Between 61 and 70", "Between 71 and 80"))
# table(Creditdata$AGE)

Creditdata$Age_Category <- age


# Fixing incorrectly classified data types:

Creditdata$PAY_1 <- as.factor(Creditdata$PAY_1)
Creditdata$PAY_2 <- as.factor(Creditdata$PAY_2)
Creditdata$PAY_3 <- as.factor(Creditdata$PAY_3)
Creditdata$PAY_4 <- as.factor(Creditdata$PAY_4)
Creditdata$PAY_5 <- as.factor(Creditdata$PAY_5)
Creditdata$PAY_6 <- as.factor(Creditdata$PAY_6)
Creditdata$default_0 <- as.factor(Creditdata$default_0)

#test/train split

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime

Creditdata_predict<-subset(Creditdata,ID>24000)

Creditdata_test_train<-subset(Creditdata,ID<=24000)

Creditdata_no_gender <- subset(Creditdata_test_train, select=-c(SEX))

inTrain <- createDataPartition(y = Creditdata_no_gender$default_0, p = 22999/24000, list = FALSE)
training <- Creditdata_no_gender[ inTrain,]
testing <- Creditdata_no_gender[ -inTrain,]


#############xgboost###

Creditdata_matrix <- model.matrix(default_0 ~ . -ID, data = Creditdata)[,-1]

x_train <- Creditdata_matrix[ inTrain,]
x_test <- Creditdata_matrix[ -inTrain,]

y_train <-training$default_0
y_test <-testing$default_0

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.099,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.26,1,0)),y_test,positive="0") #Display confusion matrix


####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #0.772501




# male / female separately ------------------------------------------------
#Apply without gender model separately to males and females

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
Creditdata_test_train<-subset(Creditdata,ID<=24000)
#male observations
male_raw <- filter(Creditdata_test_train, Creditdata_test_train$SEX == "Male")
male <- subset(male_raw, select=-c(SEX))

# male only model ---------------------------------------------------------
m_inTrain <- createDataPartition(y = male$default_0, p =0.9, list = FALSE)
m_training <- male[ m_inTrain,]
m_testing <- male[ -m_inTrain,]

m_matrix <- model.matrix(default_0 ~ . -ID, data = male)[,-1]

m_x_train <- m_matrix[ m_inTrain,]
m_x_test <- m_matrix[ -m_inTrain,]

m_y_train <-m_training$default_0
m_y_test <-m_testing$default_0

model_XGboost<-xgboost(data = data.matrix(m_x_train), 
                       label = as.numeric(as.character(m_y_train)), 
                       eta = 0.099,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=m_x_test, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.26,1,0)),m_y_test,positive="0") #Display confusion matrix


####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, m_y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #0.764132



# female only model -------------------------------------------------------
#female observations
female_raw <- filter(Creditdata_test_train, Creditdata_test_train$SEX == "Female")
female <- subset(female_raw, select=-c(SEX))

f_inTrain <- createDataPartition(y = female$default_0, p=0.9, list = FALSE)
f_training <- female[ f_inTrain,]
f_testing <- female[ -f_inTrain,]

f_matrix <- model.matrix(default_0 ~ . -ID, data = female)[,-1]

f_x_train <- f_matrix[ f_inTrain,]
f_x_test <- f_matrix[ -f_inTrain,]

f_y_train <- f_training$default_0
f_y_test <- f_testing$default_0

model_XGboost<-xgboost(data = data.matrix(f_x_train), 
                       label = as.numeric(as.character(f_y_train)), 
                       eta = 0.099,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=f_x_test, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.26,1,0)),f_y_test,positive="0") #Display confusion matrix


####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, f_y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #0.7747757


















