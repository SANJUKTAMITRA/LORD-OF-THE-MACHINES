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

emailtest <- merge(emailtest,campaign,by ="campaign_id")





nrow(emailcampaign) - sum(duplicated(emailcampaign$email_body))
#the above code shows that there are 25 unique emails body text and rest are duplicates 
#lets assign a number to each of unique email body and create a column that will display which particular 
#email body text it is showing 
list <- data.frame(unique(emailcampaign$email_body))
list$id <- c(1:25)
colnames(list)[1] <- "email_body"
library(dplyr)
library(stringr)
emailcampaign <-   left_join(emailcampaign,list,by="email_body")

colnames(emailcampaign)[15] <- "emailid"

#repeat the above step for emailtest
listt <- data.frame(unique(emailtest$email_body))
listt$id <- c(1:24)
colnames(listt)[1] <- "email_body"
listt$email_body <- as.factor(listt$email_body)
library(dplyr)
library(stringr)
emailtest <-   left_join(emailtest,listt,by="email_body")

colnames(emailtest)[13] <- "emailid"

#create a coulmn that count number of each of the following keywords  in the table
#from the coulmn email_body and subject
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
emailcampaign$hackathon <- str_count(emailcampaign$email_body,"hackathon") + str_count(emailcampaign$subject,"hackathon")
emailcampaign$machinelearning <- str_count(emailcampaign$email_body,"machine learning") + str_count(emailcampaign$subject,"machine learning")
emailcampaign$win <- str_count(emailcampaign$email_body,"win") + str_count(emailcampaign$subject,"win")
emailcampaign$prize <- str_count(emailcampaign$email_body,"prize") + str_count(emailcampaign$subject,"prize")
emailcampaign$money <- str_count(emailcampaign$email_body,"money") + str_count(emailcampaign$subject,"money")
emailcampaign$interview <- str_count(emailcampaign$email_body,"interview") + str_count(emailcampaign$subject,"interview")
emailcampaign$datascience <- str_count(emailcampaign$email_body,"data scien") + str_count(emailcampaign$subject,"data scien")
emailcampaign$AI <- str_count(emailcampaign$email_body,"artificial intelligence") + str_count(emailcampaign$subject,"artificial intelligence")
emailcampaign$bigdata <- str_count(emailcampaign$email_body,"bigdata") + str_count(emailcampaign$subject,"bigdata")
emailcampaign$competition <- str_count(emailcampaign$email_body,"competition") + str_count(emailcampaign$subject,"competition")
emailcampaign$workshops <- str_count(emailcampaign$email_body,"workshops") + str_count(emailcampaign$subject,"workshops")
emailcampaign$conference <- str_count(emailcampaign$email_body,"conference") + str_count(emailcampaign$subject,"conference")



emailcampaign$shaping <- str_count(emailcampaign$email_body,"shaping") + str_count(emailcampaign$subject,"shaping")
emailcampaign$knowledge <- str_count(emailcampaign$email_body,"knowledge") + str_count(emailcampaign$subject,"knowledge")
emailcampaign$interactions <- str_count(emailcampaign$email_body,"interactions") + str_count(emailcampaign$subject,"interactions")
emailcampaign$datahack <- str_count(emailcampaign$email_body,"datahack") + str_count(emailcampaign$subject,"datahack")
emailcampaign$summit <- str_count(emailcampaign$email_body,"summit") + str_count(emailcampaign$subject,"summit")
emailcampaign$LP <- str_count(emailcampaign$email_body,"largest platform") + str_count(emailcampaign$subject,"largest platform")
emailcampaign$proud <- str_count(emailcampaign$email_body,"proud") + str_count(emailcampaign$subject,"proud")
emailcampaign$python <- str_count(emailcampaign$email_body,"python") + str_count(emailcampaign$subject,"python")
emailcampaign$bigdata <- str_count(emailcampaign$email_body,"neural network") + str_count(emailcampaign$subject,"neural network")
emailcampaign$NN <- str_count(emailcampaign$email_body,"neural network") + str_count(emailcampaign$subject,"neural network")
emailcampaign$career <- str_count(emailcampaign$email_body,"career") + str_count(emailcampaign$subject,"career")
emailcampaign$job <- str_count(emailcampaign$email_body,"job") + str_count(emailcampaign$subject,"job")
emailcampaign$vacancies <- str_count(emailcampaign$email_body,"vacancies") + str_count(emailcampaign$subject,"vacancies")
emailcampaign$expert <- str_count(emailcampaign$email_body,"expert") + str_count(emailcampaign$subject,"expert")
  
  
#repeat the above steps for emailtest
emailtest$hackathon <- str_count(emailtest$email_body,"hackathon") + str_count(emailtest$subject,"hackathon")
emailtest$machinelearning <- str_count(emailtest$email_body,"machine learning") + str_count(emailtest$subject,"machine learning")
emailtest$win <- str_count(emailtest$email_body,"win") + str_count(emailtest$subject,"win")
emailtest$prize <- str_count(emailtest$email_body,"prize") + str_count(emailtest$subject,"prize")
emailtest$money <- str_count(emailtest$email_body,"money") + str_count(emailtest$subject,"money")
emailtest$interview <- str_count(emailtest$email_body,"interview") + str_count(emailtest$subject,"interview")
emailtest$datascience <- str_count(emailtest$email_body,"data scien") + str_count(emailtest$subject,"data scien")
emailtest$AI <- str_count(emailtest$email_body,"artificial intelligence") + str_count(emailtest$subject,"artificial intelligence")
emailtest$bigdata <- str_count(emailtest$email_body,"bigdata") + str_count(emailtest$subject,"bigdata")
emailtest$competition <- str_count(emailtest$email_body,"competition") + str_count(emailtest$subject,"competition")
emailtest$workshops <- str_count(emailtest$email_body,"workshops") + str_count(emailtest$subject,"workshops")
emailtest$conference <- str_count(emailtest$email_body,"conference") + str_count(emailtest$subject,"conference")



emailtest$shaping <- str_count(emailtest$email_body,"shaping") + str_count(emailtest$subject,"shaping")
emailtest$knowledge <- str_count(emailtest$email_body,"knowledge") + str_count(emailtest$subject,"knowledge")
emailtest$interactions <- str_count(emailtest$email_body,"interactions") + str_count(emailtest$subject,"interactions")
emailtest$datahack <- str_count(emailtest$email_body,"datahack") + str_count(emailtest$subject,"datahack")
emailtest$summit <- str_count(emailtest$email_body,"summit") + str_count(emailtest$subject,"summit")
emailtest$LP <- str_count(emailtest$email_body,"largest platform") + str_count(emailtest$subject,"largest platform")
emailtest$proud <- str_count(emailtest$email_body,"proud") + str_count(emailtest$subject,"proud")
emailtest$python <- str_count(emailtest$email_body,"python") + str_count(emailtest$subject,"python")
emailtest$bigdata <- str_count(emailtest$email_body,"neural network") + str_count(emailtest$subject,"neural network")
emailtest$NN <- str_count(emailtest$email_body,"neural network") + str_count(emailtest$subject,"neural network")
emailtest$career <- str_count(emailtest$email_body,"career") + str_count(emailtest$subject,"career")
emailtest$job <- str_count(emailtest$email_body,"job") + str_count(emailtest$subject,"job")
emailtest$vacancies <- str_count(emailtest$email_body,"vacancies") + str_count(emailtest$subject,"vacancies")
emailtest$expert <- str_count(emailtest$email_body,"expert") + str_count(emailtest$subject,"expert")



#change the format of send data and extract year and month 
emailcampaign$send_date <- as.POSIXct(emailcampaign$send_date,format="%d-%m-%Y %H:%M")

emailcampaign$sendweek <- format(emailcampaign$send_date,"%w")
emailcampaign$sendmonth <- format(emailcampaign$send_date,"%m")

#change the format of send data and extract year and month for emailtest
emailtest$send_date <- as.POSIXct(emailtest$send_date,format="%d-%m-%Y %H:%M")

emailtest$sendweek <- format(emailtest$send_date,"%w")
emailtest$sendmonth <- format(emailtest$send_date,"%m")


str(emailcampaign)
emailcampaign$is_open <- as.factor(emailcampaign$is_open)
emailcampaign$is_click <- as.factor(emailcampaign$is_click)


#check for outliers 
quantile(emailcampaign[,5],probs=seq(0,1,0.01))
quantile(emailcampaign[,6],probs=seq(0,1,0.01))
quantile(emailcampaign[,7],probs=seq(0,1,0.01))
quantile(emailcampaign[,8],probs=seq(0,1,0.01))

#no outliers detected

levels(as.factor(emailcampaign$communication_type))

emailcampaign$communication_type <- as.factor(emailcampaign$communication_type)

unique(emailcampaign$subject)
#26 unique subjects are there 

levels(as.factor(emailcampaign$subject))

emailcampaign$subject <- as.factor(emailcampaign$subject)

dummy2 <- data.frame(model.matrix(~subject,data = emailcampaign))
dummy2 <- dummy2[,-1]

emailcampaign$email_body <- as.factor(emailcampaign$email_body)
emailcampaign$email_body <- as.factor(emailcampaign$email_body)

dummy3 <- data.frame(model.matrix(~email_body,data = emailcampaign))
dummy3 <- dummy3[,-1]

#store the colnames of subject and email_body inside a vector
subject1 <- colnames(dummy2)
email_body1 <- colnames(dummy3)

colnames(dummy2) <- c("s1:s25")
colnames(dummy3) <- c(26:50)

#emailid column is not required

#check the key parameters using decision trees
library(rpart.plot)
library(rpart)
dt1 <- rpart(is_click~.,data=emailcampaign[,c(4:8,13:42)])
prp(dt1)
#create dummy variable for communication type 
dummy1 <- data.frame(model.matrix(~communication_type,data = emailcampaign))

dummy1 <- dummy1[,-1]
#create a secondary dataframe from emailcamapign that shall be used for model building 
#column 15 is emailid
emailcampaign2 <- emailcampaign[,-c(1:4,9:12,15)]
emailcampaign2 <- cbind(emailcampaign2,dummy1,dummy2,dummy3)


#scaling
str(emailcampaign2)
emailcampaign2$sendmonth <- as.numeric(emailcampaign2$sendmonth)
emailcampaign2$sendweek <- as.numeric(emailcampaign2$sendweek)

emailtest$sendmonth <- as.numeric(emailtest$sendmonth)
emailtest$sendweek <- as.numeric(emailtest$sendweek)

##check for duplicates

sum(duplicated(emailcampaign2))
#1023107 

emailcampaign2.1 <- emailcampaign2[!duplicated(emailcampaign2),]

#reaariange the columns 
emailcampaign2.1 <- emailcampaign2.1[,c(6,1:5,7:87)]
emailcampaign2.1[,c(2:5)] <- data.frame(sapply(emailcampaign2.1[,c(2:5)],function(x) scale(x)))

sum(is.na(emailcampaign2))

#MODEL BUILDING

set.seed(1234)
library(caTools)
#indices1 <- sample.split(1:nrow(emailcampaign2),0.05*nrow(emailcampaign2))
#DATA1 <- emailcampaign2[indices1,]
rm(trainemail)
indices2 <- sample.split(emailcampaign2.1,SplitRatio = 0.7)
trainemail <- emailcampaign2.1[indices2,]
testemail <- emailcampaign2.1[!indices2,]


#will use trainemail and testemail for model building purpose

model1 <- glm(formula=is_click~., data=trainemail,family = "binomial")
summary(model1)

  library(MASS)
model2 <-stepAIC(model1,direction = "both")

summary(model2)

model3 <- glm(formula = is_click ~ communication_typeUpcoming.Events+ no_of_sections+no_of_images+communication_typeCorporate+
                machinelearning+ sendweek + prize+total_links+ communication_typeHackathon+ summit+ expert + datascience + hackathon+ 
                communication_typeNewsletter +job +win + knowledge +shaping, family = "binomial", data = trainemail)

summary(model3)

library(car)
vif(model3)
#communication_typeCorporate 
model4 <- glm(formula = is_click ~  sendweek+expert+machinelearning +total_links+communication_typeOthers+
                communication_typeCorporate+no_of_internal_links+shaping +workshops+communication_typeNewsletter +
              datascience+ interview +no_of_sections+is_open , family = "binomial", data = trainemail)


summary(model4)
library(car)
vif(model4)
#no_of_internal_links has high vif , lets remove and check AIC
model5 <- glm(formula = is_click ~  sendweek+expert+machinelearning +total_links+communication_typeOthers+
                communication_typeCorporate+shaping +workshops+communication_typeNewsletter +
                datascience+ interview +no_of_sections+is_open , family = "binomial", data = trainemail)


summary(model5)

vif(model5)
#remove total_links which has high vif and p value
model6 <- glm(formula = is_click ~  sendweek+expert+machinelearning +communication_typeOthers+
                communication_typeCorporate+shaping +workshops+communication_typeNewsletter +
                datascience+ interview +no_of_sections+is_open , family = "binomial", data = trainemail)

summary(model6)

vif(model6)

#shaping  and sendweek 
model7 <- glm(formula = is_click ~ expert+machinelearning +communication_typeOthers+
                communication_typeCorporate+workshops+communication_typeNewsletter +
                datascience+ interview +no_of_sections+is_open , family = "binomial", data = trainemail)

summary(model7)

vif(model7)
# expert and  communication_typeNewsletter 
model8 <- glm(formula = is_click ~ expert+machinelearning +communication_typeOthers+
                communication_typeCorporate+workshops+
                datascience+ interview +no_of_sections+is_open , family = "binomial", data = trainemail)

summary(model8)

vif(model8)
#communication_typeOthers+ 
model9 <- glm(formula = is_click ~ expert+machinelearning +
                communication_typeCorporate+workshops+
                datascience+ interview +no_of_sections+is_open , family = "binomial", data = trainemail)

summary(model9)

vif(model9)
#communication_typeCorporate and interview 
model10 <- glm(formula =is_click ~ expert+machinelearning +workshops+
                 datascience+ no_of_sections+is_open , family = "binomial", data = trainemail)

summary(model10)

vif(model10)
#communication_typeUpcoming.Events 
model11 <- glm(formula =is_click ~ total_links+ communication_typeHackathon+ summit+ expert + datascience + hackathon+ 
                 job , family = "binomial", data = trainemail)

summary(model11)

vif(model11)
#summit  
model12 <- glm(formula = is_click ~  total_links+ communication_typeHackathon+ expert + datascience + hackathon+ 
                 job , family = "binomial", data = trainemail)

summary(model12)

vif(model12)
#expert   
model13 <- glm(formula = is_click ~  total_links+ communication_typeHackathon+ datascience + hackathon+ 
                 job , family = "binomial", data = trainemail)

summary(model13)

vif(model13)
#total_links 
model14 <- glm(formula =  is_click ~  communication_typeHackathon+ datascience + hackathon+ 
                 job , family = "binomial", data = trainemail)

summary(model14)

vif(model14)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 -4.34858    0.07129 -60.994  < 2e-16 ***
#  communication_typeHackathon  1.26099    0.22124   5.700 1.20e-08 ***
#  datascience                 -0.09300    0.03915  -2.375  0.01753 *  
#  hackathon                   -0.32631    0.10516  -3.103  0.00192 ** 
#  job                          0.63883    0.14192   4.501 6.76e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 4781.0  on 35810  degrees of freedom
#Residual deviance: 4739.5  on 35806  degrees of freedom
#AIC: 4749.5
final_model <- model14
#although is_open1 has high p value but upon removing it the AIC value is increasing to 7500 from 3700

#######################################################################

### Model Evaluation


### Test Data ####
#predicted probabilities of Attrition  for test data
test_pred <- predict(final_model,type="response",testemail)

#lets check summary 
summary(test_pred)
#minimum 0.0003665 and maximum 0.3095

testemail$probability <- test_pred

# Let's use the probability cutoff of 2%.
test_pred_click <- factor(ifelse(testemail$probability  >= 0.010, 1, 0))


table(test_pred_click,testemail$is_click)



# Let's use the probability cutoff of 3%.
test_pred_click1 <- factor(ifelse(testemail$probability  >= 0.02, 1, 0))

table(test_pred_click1,testemail$is_click)


install.packages("caret")
library(e1071)
library(caret)
test_conf <- confusionMatrix(test_pred_click1,testemail$is_click , positive = "1")
test_conf$byClass[1]

#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_click <- factor(ifelse(test_pred >= cutoff, 1, 0))
  conf <- confusionMatrix(test_pred_click,testemail$is_click , positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


#to check the above function for cutoff value from 30% to highest value of probability 0.8959000 i.e 89.5%
s = seq(0.001,0.04,length=100)
#creating a output matrix of size 70
OUT1 <- matrix(0,100,3)


for(i in 1:99){
  OUT1[i,] <- perform_fn(s[i])
}
OUT1
#putting cut off value,sensitivity,Specificity and Accuracy in a single dataframe
#plotting them to check the cut off value 
OUT <- data.frame(s,OUT1[,1],OUT1[,2],OUT1[,3])
colnames(OUT)[1] <- "cutoff"
colnames(OUT)[2] <- "sensitivity"
colnames(OUT)[3] <- "specificity"
colnames(OUT)[4] <- "accuracy"
plot(s, OUT1[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT1[,2],col="darkgreen",lwd=2)
lines(s,OUT1[,3],col=4,lwd=2)
box()
legend(0.7,0.2,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


#the locator shows the cut off value at 0.674 where y value is 0.8498 
cutoff <- 0.61
test_cutoff_attrition <- factor(ifelse(test$probability >= cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_actual_atrrition,test_cutoff_attrition,positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

#accuracy as 85.4%
acc
#sensitivity as 83.33%
sens
#Specificity as 85.5%
spec

View(test)
#sort the test dataframe in sorted order of probability
test <- arrange(test,desc(probability))
##################################################################################################
### 
write.csv(HR_Company,"HR_Company.xls")
write.csv(HR_Company1,"HR_Company1.xls")
write.csv(test,"test.xls")


