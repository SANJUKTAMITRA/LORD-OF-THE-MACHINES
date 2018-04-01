#import the required libraries

#import the two input files 
campaign <- read.csv("campaign_data.csv",stringsAsFactors = FALSE)
traincampaign <- read.csv("train.csv",stringsAsFactors = FALSE)

#import the test file 

emailtest <- read.csv("test_BDIfz5B.csv",stringsAsFactors = FALSE)

#check first few rows,struture and dimension of the table 
head(campaign)
str(campaign)
dim(campaign)

head(traincampaign)
str(traincampaign)
dim(traincampaign)

head(emailtest)
str(emailtest)
dim(emailtest)

#check for data anamolies 
sum(is.na(campaign))
sum(is.na(traincampaign))
sum(is.na(emailtest))

#no missing record exist

sum(duplicated(campaign))
sum(duplicated(traincampaign))
sum(duplicated(emailtest))

#merge the tables 

emailcampaign <- merge(campaign,traincampaign,by="campaign_id")

emailcampaign <- emailcampaign[,c(10,11,1:9,12:14)]

emailtest1 <- merge(emailtest,campaign,by ="campaign_id")

#traindata : create a sample that equals number of rows for is_click value 0 and 1
x <- subset(emailcampaign,is_click==0)
y <- subset(emailcampaign,is_click==1)

i <- sample(nrow(emailcampaign),0.015*nrow(emailcampaign))
x1 <- x[i,]

emailcampaign1 <- rbind(x1,y)

#shuffle the df
emailcampaign1 <- emailcampaign1[sample(nrow(emailcampaign1)),]
nrow(emailcampaign1) - sum(duplicated(emailcampaign1$email_body))
#the above code shows that there are 25 unique emails body text and rest are duplicates 
#lets assign a number to each of unique email body and create a column that will display which particular 
#email body text it is showing 
list <- data.frame(unique(emailcampaign1$email_body))
list$id <- c(1:26)
colnames(list)[1] <- "email_body"
library(dplyr)
library(stringr)
emailcampaign1 <-   left_join(emailcampaign1,list,by="email_body")

colnames(emailcampaign1)[15] <- "emailid"


#repeat the above step in emailtest 
listt <- data.frame(unique(emailtest1$email_body))
listt$id <- c(1:24)
colnames(listt)[1] <- "email_body"
listt$email_body <- as.factor(listt$email_body)
emailtest1 <-   left_join(emailtest1,listt,by="email_body")

colnames(emailtest1)[13] <- "emailid"


#hackathon
#machine learning
#win
#prize
#money
#interview
#Data scien
#artificial Intelligence
#big data
#competition
#workshops
#conference


#shaping
#knowledge
#interactions
#datahack
#summit
#largest platform
#proud
#python
#neural network
#career
#job
#vacancies
#expert
#

emailcampaign1$hackathon <- str_count(emailcampaign1$email_body,"hackathon") + str_count(emailcampaign1$subject,"hackathon")
emailcampaign1$machinelearning <- str_count(emailcampaign1$email_body,"machine learning") + str_count(emailcampaign1$subject,"machine learning")
emailcampaign1$win <- str_count(emailcampaign1$email_body,"win") + str_count(emailcampaign1$subject,"win")
emailcampaign1$prize <- str_count(emailcampaign1$email_body,"prize") + str_count(emailcampaign1$subject,"prize")
emailcampaign1$money <- str_count(emailcampaign1$email_body,"money") + str_count(emailcampaign1$subject,"money")
emailcampaign1$interview <- str_count(emailcampaign1$email_body,"interview") + str_count(emailcampaign1$subject,"interview")
emailcampaign1$datascience <- str_count(emailcampaign1$email_body,"data scien") + str_count(emailcampaign1$subject,"data scien")
emailcampaign1$AI <- str_count(emailcampaign1$email_body,"artificial intelligence") + str_count(emailcampaign1$subject,"artificial intelligence")
emailcampaign1$bigdata <- str_count(emailcampaign1$email_body,"bigdata") + str_count(emailcampaign1$subject,"bigdata")
emailcampaign1$competition <- str_count(emailcampaign1$email_body,"competition") + str_count(emailcampaign1$subject,"competition")
emailcampaign1$workshops <- str_count(emailcampaign1$email_body,"workshops") + str_count(emailcampaign1$subject,"workshops")
emailcampaign1$conference <- str_count(emailcampaign1$email_body,"conference") + str_count(emailcampaign1$subject,"conference")



emailcampaign1$shaping <- str_count(emailcampaign1$email_body,"shaping") + str_count(emailcampaign1$subject,"shaping")
emailcampaign1$knowledge <- str_count(emailcampaign1$email_body,"knowledge") + str_count(emailcampaign1$subject,"knowledge")
emailcampaign1$interactions <- str_count(emailcampaign1$email_body,"interactions") + str_count(emailcampaign1$subject,"interactions")
emailcampaign1$datahack <- str_count(emailcampaign1$email_body,"datahack") + str_count(emailcampaign1$subject,"datahack")
emailcampaign1$summit <- str_count(emailcampaign1$email_body,"summit") + str_count(emailcampaign1$subject,"summit")
emailcampaign1$LP <- str_count(emailcampaign1$email_body,"largest platform") + str_count(emailcampaign1$subject,"largest platform")
emailcampaign1$proud <- str_count(emailcampaign1$email_body,"proud") + str_count(emailcampaign1$subject,"proud")
emailcampaign1$python <- str_count(emailcampaign1$email_body,"python") + str_count(emailcampaign1$subject,"python")
emailcampaign1$bigdata <- str_count(emailcampaign1$email_body,"neural network") + str_count(emailcampaign1$subject,"neural network")
emailcampaign1$NN <- str_count(emailcampaign1$email_body,"neural network") + str_count(emailcampaign1$subject,"neural network")
emailcampaign1$career <- str_count(emailcampaign1$email_body,"career") + str_count(emailcampaign1$subject,"career")
emailcampaign1$job <- str_count(emailcampaign1$email_body,"job") + str_count(emailcampaign1$subject,"job")
emailcampaign1$vacancies <- str_count(emailcampaign1$email_body,"vacancies") + str_count(emailcampaign1$subject,"vacancies")
emailcampaign1$expert <- str_count(emailcampaign1$email_body,"expert") + str_count(emailcampaign1$subject,"expert")

#repeat the above steps for emailtest

emailtest1$hackathon <- str_count(emailtest1$email_body,"hackathon") + str_count(emailtest1$subject,"hackathon")
emailtest1$machinelearning <- str_count(emailtest1$email_body,"machine learning") + str_count(emailtest1$subject,"machine learning")
emailtest1$win <- str_count(emailtest1$email_body,"win") + str_count(emailtest1$subject,"win")
emailtest1$prize <- str_count(emailtest1$email_body,"prize") + str_count(emailtest1$subject,"prize")
emailtest1$money <- str_count(emailtest1$email_body,"money") + str_count(emailtest1$subject,"money")
emailtest1$interview <- str_count(emailtest1$email_body,"interview") + str_count(emailtest1$subject,"interview")
emailtest1$datascience <- str_count(emailtest1$email_body,"data scien") + str_count(emailtest1$subject,"data scien")
emailtest1$AI <- str_count(emailtest1$email_body,"artificial intelligence") + str_count(emailtest1$subject,"artificial intelligence")
emailtest1$bigdata <- str_count(emailtest1$email_body,"bigdata") + str_count(emailtest1$subject,"bigdata")
emailtest1$competition <- str_count(emailtest1$email_body,"competition") + str_count(emailtest1$subject,"competition")
emailtest1$workshops <- str_count(emailtest1$email_body,"workshops") + str_count(emailtest1$subject,"workshops")
emailtest1$conference <- str_count(emailtest1$email_body,"conference") + str_count(emailtest1$subject,"conference")



emailtest1$shaping <- str_count(emailtest1$email_body,"shaping") + str_count(emailtest1$subject,"shaping")
emailtest1$knowledge <- str_count(emailtest1$email_body,"knowledge") + str_count(emailtest1$subject,"knowledge")
emailtest1$interactions <- str_count(emailtest1$email_body,"interactions") + str_count(emailtest1$subject,"interactions")
emailtest1$datahack <- str_count(emailtest1$email_body,"datahack") + str_count(emailtest1$subject,"datahack")
emailtest1$summit <- str_count(emailtest1$email_body,"summit") + str_count(emailtest1$subject,"summit")
emailtest1$LP <- str_count(emailtest1$email_body,"largest platform") + str_count(emailtest1$subject,"largest platform")
emailtest1$proud <- str_count(emailtest1$email_body,"proud") + str_count(emailtest1$subject,"proud")
emailtest1$python <- str_count(emailtest1$email_body,"python") + str_count(emailtest1$subject,"python")
emailtest1$bigdata <- str_count(emailtest1$email_body,"neural network") + str_count(emailtest1$subject,"neural network")
emailtest1$NN <- str_count(emailtest1$email_body,"neural network") + str_count(emailtest1$subject,"neural network")
emailtest1$career <- str_count(emailtest1$email_body,"career") + str_count(emailtest1$subject,"career")
emailtest1$job <- str_count(emailtest1$email_body,"job") + str_count(emailtest1$subject,"job")
emailtest1$vacancies <- str_count(emailtest1$email_body,"vacancies") + str_count(emailtest1$subject,"vacancies")
emailtest1$expert <- str_count(emailtest1$email_body,"expert") + str_count(emailtest1$subject,"expert")




#change the format of send data and extract year and month 
emailcampaign1$send_date <- as.POSIXct(emailcampaign1$send_date,format="%d-%m-%Y %H:%M")

emailcampaign1$sendweek <- format(emailcampaign1$send_date,"%w")
emailcampaign1$sendmonth <- format(emailcampaign1$send_date,"%m")

##change the format of send data and extract year and month for emailtest
emailtest1$send_date <- as.POSIXct(emailtest1$send_date,format="%d-%m-%Y %H:%M")

emailtest1$sendweek <- format(emailtest1$send_date,"%w")
emailtest1$sendmonth <- format(emailtest1$send_date,"%m")


str(emailcampaign1)
emailcampaign1$is_open <- as.factor(emailcampaign1$is_open)
emailcampaign1$is_click <- as.factor(emailcampaign1$is_click)



#check for outliers 
quantile(emailcampaign1[,5],probs=seq(0,1,0.01))
quantile(emailcampaign1[,6],probs=seq(0,1,0.01))
quantile(emailcampaign1[,7],probs=seq(0,1,0.01))
quantile(emailcampaign1[,8],probs=seq(0,1,0.01))

#no outliers detected

levels(as.factor(emailcampaign1$communication_type))

emailcampaign1$communication_type <- as.factor(emailcampaign1$communication_type)
emailtest1$communication_type <- as.factor(emailtest1$communication_type)
unique(emailcampaign1$subject)
#26 unique subjects are there 

levels(as.factor(emailcampaign1$subject))

#create a subject id for each subject and url id for each url

nrow(emailcampaign1) - sum(duplicated(emailcampaign1$email_body))
#the above code shows that there are 25 unique emails body text and rest are duplicates 
#lets assign a number to each of unique email body and create a column that will display which particular 
#email body text it is showing 
list_sub <- data.frame(unique(emailcampaign1$subject))
list_sub$id <- c(1:27)
colnames(list_sub)[1] <- "subject"
colnames(list_sub)[2] <- "subject_id"
library(dplyr)
library(stringr)
emailcampaign <-   left_join(emailcampaign,list_sub,by="subject")

#repeat the process for emailtest
listt_sub <- data.frame(unique(emailtest1$subject))
listt_sub$id <- c(1:24)
colnames(listt_sub)[1] <- "subject"
colnames(listt_sub)[2] <- "subject_id"
emailtest1 <-   left_join(emailtest1,listt_sub,by="subject")
#URL
list_url <- data.frame(unique(emailcampaign1$email_url))
list_url$url_id <- c(1:27)
colnames(list_url)[1] <- "email_url"
colnames(list_url)[2] <- "url_id"
library(dplyr)
library(stringr)
emailcampaign <-   left_join(emailcampaign,list_url,by="email_url")
#emailtest
listt_url <- data.frame(unique(emailtest1$email_url))
listt_url$url_id <- c(1:26)
colnames(listt_url)[1] <- "email_url"
colnames(listt_url)[2] <- "url_id"
emailtest1 <-   left_join(emailtest1,listt_url,by="email_url")


emailcampaign1$emailid <- as.factor(emailcampaign1$emailid)
emailcampaign1$url_id <- as.factor(emailcampaign1$url_id)
emailcampaign1$subject_id <- as.factor(emailcampaign1$subject_id)
emailcampaign1$sendmonth <- as.numeric(emailcampaign1$sendmonth)
emailcampaign1$sendweek <- as.numeric(emailcampaign1$sendweek)

emailtest1$emailid <- as.factor(emailtest1$emailid)
emailtest1$url_id <- as.factor(emailtest1$url_id)
emailtest1$subject_id <- as.factor(emailtest1$subject_id)
emailtest1$sendmonth <- as.numeric(emailtest1$sendmonth)
emailtest1$sendweek <- as.numeric(emailtest1$sendweek)


#2.2 for random forest

#reaariange the columns 
emailcampaign3 <- emailcampaign1[,-c(9:13)]
##check for duplicates

sum(duplicated(emailcampaign3))
#184

emailcampaign3 <- emailcampaign3[!duplicated(emailcampaign3),]
#MODEL BUILDING
#check the key parameters using decision trees
library(rpart.plot)
library(rpart)
dt1 <- rpart(is_click~.,data=emailcampaign3[,-c(1:3)])

prp(dt1)


set.seed(1234)
library(caTools)
indicesR2 <- sample.split(emailcampaign3,SplitRatio = 0.7)
trainemailRM <- emailcampaign3[indicesR2,]
testemailRM <- emailcampaign3[!indicesR2,]

colnames(trainemailRM)
#will use random forest model
library(randomForest)

modelRF1 <- randomForest(is_click~.,data = trainemailRM[,-c(1:3)],proximity=FALSE,
                         ntree=1000, mtry=26, do.trace=2, na.action=na.omit)


modelRF1
testPredRF <- predict(modelRF1, newdata=testemailRM)
table(testPredRF, testemailRM$is_click)

library(caret)
confusionMatrix(testPredRF, testemailRM$is_click,positive = "1")
#sensitvity 21% and accuracy as 58%

modelRF2 <- randomForest(is_click~.,data = trainemailRM[,-c(1:3)],proximity=FALSE,
                         ntree=900, mtry=20, do.trace=2, na.action=na.omit)


modelRF2
testPredRF1 <- predict(modelRF2, newdata=testemailRM)
table(testPredRF1, testemailRM$is_click)

library(caret)
confusionMatrix(testPredRF1, testemailRM$is_click,positive = "1")
#no improvement in sensitvity
#naive bayes
library(e1071)

modelNB1 <- naiveBayes(is_click~.,data = trainemailRM)
predNB1 <- predict(modelNB1,testemailRM)
predNB1
table(predNB1,testemailRM$is_click)

confusionMatrix(predNB1,testemailRM$is_click,positive = "1")
#Reference
#         Reference
#Prediction    0    1
#0 1148  679
#1 3783 3455

#Accuracy : 0.5078          
#95% CI : (0.4974, 0.5181)
#No Information Rate : 0.544           
#P-Value [Acc > NIR] : 1               

#Kappa : 0.0646          
#Mcnemar's Test P-Value : <2e-16          

#Sensitivity : 0.8358          
#Specificity : 0.2328          
#Pos Pred Value : 0.4773          
#Neg Pred Value : 0.6284          
#Prevalence : 0.4560          
#Detection Rate : 0.3811          
#Detection Prevalence : 0.7985          
#Balanced Accuracy : 0.5343          

#'Positive' Class : 1               

#use NAIVE bayes model to predict class label for emailtest

emailtest2 <- emailtest1[,-c(4,10:12)]
rm(predemail)
predemail <- predict(modelNB1,emailtest2)


emailtest1$is_clickpred <- predemail

#merge(emailtest,emailtest1[,c(1:3,43)],by.x = c("campaign_id","id","user_id"),by.y = c("campaign_id","id.x","user_id"))
write.csv(emailtest1[,c(1:3,43)],"emailtest-naivebayes1.csv")




#for KNN we need to scale the data 
testemailRM1 <- testemailRM
trainemailRM1 <- trainemailRM
trainemailRM1[,c(2:4)] <- data.frame(sapply(trainemailRM[,c(2:4)],function(x) scale(x)))
testemailRM1[,c(2:4)] <- data.frame(sapply(testemailRM[,c(2:4)],function(x) scale(x)))

#knn works with numeric variables only
#convert factor variable to numeric 
trainemailRM1$emailid <- as.numeric(trainemailRM1$emailid)
trainemailRM1$subject_id <- as.numeric(trainemailRM1$subject_id)
trainemailRM1$url_id <- as.numeric(trainemailRM1$url_id)
testemailRM1$emailid <- as.numeric(testemailRM1$emailid)
testemailRM1$subject_id <- as.numeric(testemailRM1$subject_id)
testemailRM1$url_id <- as.numeric(testemailRM1$url_id)

trainemailRM1 <- data.frame(sapply(trainemailRM1,function(x) as.numeric(x)))
testemailRM1 <- data.frame(sapply(testemailRM1,function(x) as.numeric(x)))
sum(duplicated(trainemailRM1))
trainemailRM1 <- trainemailRM1[!duplicated(trainemailRM1),]
testemailRM1 <- testemailRM1[!duplicated(testemailRM1),]


#K-NN model
#store the output of train and test in different vvariables

knn_trainout1 <- trainemailRM1$is_click
knn_testout1 <- testemailRM1$is_click

str(trainemailRM1)
trainemailRM1 <- trainemailRM1[,-c(5:6)]
testemailRM1 <- testemailRM1[,-c(5:6)]

x <- subset(trainemailRM,is_click==0)
y <- subset(trainemailRM,is_click==1)

ind1 <- sample.split(1:nrow(x),0.01*nrow(x))
ind2 <- sample.split(1:nrow(y),0.92*nrow(y))

x1 <- trainemailRM[ind1,]
y1 <- trainemailRM[ind2,]

x1 <- x1[!duplicated(x1),]
y1 <- y1[!duplicated(y1),]

trainemailRM1 <- rbind(x1,y1)

a <- subset(testemailRM,is_click==0)
b <- subset(testemailRM,is_click==1)

ind3 <- sample.split(1:nrow(a),0.01*nrow(a))
ind4 <- sample.split(1:nrow(b),0.92*nrow(b))

a1 <- testemailRM[ind3,]
b1 <- testemailRM[ind4,]

a1 <- a1[!duplicated(a1),]
b1 <- b1[!duplicated(b1),]

trainemailRM1 <- rbind(x1,y1)

rm(knnmodel2)
library(class)
knnmodel1 <- class::knn(train=trainemailRM1[,-c(1:5)],test=testemailRM1[,-c(1:5)],cl=knn_trainout1,k=1,prob = TRUE)
table(knnmodel1,knn_testout1)  

knnmodel2 <- class::knn(train=trainemailRM1,test=testemailRM1,cl=knn_trainout1,k=5,prob = TRUE)
table(knnmodel2,knn_testout1)  

