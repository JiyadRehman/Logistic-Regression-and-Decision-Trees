# ---------- ASSIGNMENT 3 -----------------

# --------- Libraries ---------------------
install.packages("questionr")
install.packages("ROCR") # for LIFT, GAIN, AUC chart
install.packages("pROC") # for AUC value



library(caTools)  # for sample.split
library(ggplot2)
library(caret)    # for confusion matrix
library(ROCR) # for LIFT, GAIN, AUC chart
library(pROC) # for AUC


library(questionr)


# --------- Libraries ---------------------

# ---- DATA LOADING -----------------------

library(readxl)
Churn <- read_excel("Churn(1).xlsx")
View(Churn)

# ---- DATA LOADING -----------------------

# ----- WORKING ---------------------------

# ----------- PART 1 ------------------------------

 #----------- PART (a) --------------------
Totlen <- length(Churn$Churn)
chulen <- length(which(Churn$Churn == 1))

ChurnRate <- (chulen/Totlen)*100

rm(Totlen, chulen)

# Churn Rate = 14.49145%

#----------- PART (a) --------------------

#----------- PART (b) --------------------

set.seed(12345)

split = sample.split(Churn$Churn, SplitRatio = 0.6)

training <- subset(Churn, split == TRUE)
validation <- subset(Churn, split == FALSE)


ggplot(training, aes(x=RoamMins, y=Churn))  +
 geom_point(aes(col=as.factor(ContractRenewal)), size = 4) + labs(col = 'ContrantRenewal') +
   ggtitle("Churn vs. RoamMins colored by ContractRenewal")  

#----------- PART (b) --------------------

#----------- PART (c) --------------------

model <- glm(Churn ~ AccountWeeks + DataUsage + CustServCalls + DayMins + DayCalls + 
               MonthlyCharge + OverageFee + RoamMins ,
             data=training, family=binomial)

mylogit.probs<-predict(model,validation,type="response")

lv <- length(validation$Churn)

mylogit.pred = rep(0, lv)
mylogit.pred[mylogit.probs > 0.5] = 1

confusionMatrix(as.factor(mylogit.pred), as.factor(validation$Churn), positive = NULL, dnn = c("Predicted", "Actual"))

length(which(validation$Churn == 0))

rm(lv)
#----------- PART (c) --------------------

#----------- PART (d) --------------------

model2 <- glm(Churn ~. ,
             data=training, family=binomial)


    
mylogit.probs2<-predict(model2,validation,type="response")

lv <- length(validation$Churn)

mylogit.pred2 = rep(0, lv)
mylogit.pred2[mylogit.probs2 > 0.5] = 1

confusionMatrix(as.factor(mylogit.pred2), as.factor(validation$Churn), positive = NULL, dnn = c("Predicted", "Actual"))


rm(lv)   
#----------- PART (d) --------------------

#----------- PART (e) --------------------

  # Calculation based on part(c) and part(d) 

#----------- PART (e) --------------------

#----------- PART (f) --------------------


  # *********** ROC curves and AUC values ******************************

 # for MODEL C ----------------------------------------

rocdf <- cbind(validation,mylogit.probs)
rocdf$res <- as.factor(ifelse(rocdf$mylogit.probs>0.5, 1, 0))


logit_scores <- prediction(predictions=rocdf$mylogit.probs, labels=rocdf$Churn)

#PLOT ROC CURVE
logit_perf <- performance(logit_scores, "tpr", "fpr")

plot(logit_perf,
     main="ROC Curve for Model C",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")


# AREA UNDER THE CURVE
logit_auc <- performance(logit_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value

auc(roc(rocdf$Churn,rocdf$mylogit.probs))


  # for MODEL D ------------------------------------

rocdf2 <- cbind(validation,mylogit.probs2)
rocdf2$res <- as.factor(ifelse(rocdf2$mylogit.probs2>0.5, 1, 0))


logit_scores2 <- prediction(predictions=rocdf2$mylogit.probs, labels=rocdf2$Churn)

#PLOT ROC CURVE
logit_perf2 <- performance(logit_scores2, "tpr", "fpr")

plot(logit_perf2,
     main="ROC Curve for Model D",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")


# AREA UNDER THE CURVE
logit_auc2 <- performance(logit_scores2, "auc")
as.numeric(logit_auc2@y.values)  ##AUC Value

auc(roc(rocdf2$Churn,rocdf2$mylogit.probs2))

# *********** ROC curves and AUC values ******************************

    # ********** LIFT chart ********************************************

  # for MODEL C ------------------------------------

logit_lift <- performance(logit_scores, measure="lift", x.measure="rpp")
plot(logit_lift,
     main="Lift Chart for Model c",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

  # for MODEL D ------------------------------------

logit_lift2 <- performance(logit_scores2, measure="lift", x.measure="rpp")
plot(logit_lift2,
     main="Lift Chart for Model d",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

#----------- PART (f) --------------------


# ----------- PART 1 ------------------------------

###############################################################################

# ----------- PART 2 ------------------------------


Web.Robot <- read.csv("Web Robot.csv")

#----------- PART (a) --------------------

WebRobots <- length(which(Web.Robot$Robot == 1))

WebRobots/length(Web.Robot$Robot)
# WebRobots = 449

#----------- PART (a) --------------------

#----------- PART (b) --------------------

Web.Robot$Robot = ifelse(Web.Robot$Robot==1,"Yes","No")

set.seed(12345)


split = sample.split(Web.Robot$Robot, SplitRatio = 0.6)

training2 <- subset(Web.Robot, split == TRUE)
validation2 <- subset(Web.Robot, split == FALSE)


#---------------- Libraries ---------------------------------------


install.packages("tree")
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("party")
install.packages("partykit")
install.packages("caret")

library(rpart)				        # Popular decision tree algorithm
library(rattle)				      	# Fancy tree plot
library(rpart.plot)			    	# Enhanced tree plots
library(RColorBrewer)			  	# Color selection for fancy tree plot
library(party)					      # Alternative decision tree algorithm
library(partykit)				      # Convert rpart object to BinaryTree
library(caret)					      # Just a data source for this script
library(tree)


#---------------- Libraries ---------------------------------------

# Web.Robot$Robot = ifelse(Web.Robot$Robot==1,"Yes","No")

training2$Robot <- as.factor(training2$Robot)

tree1 <- rpart(Robot~., training2, method = "class")

#tree1 <- rpart(Robot~., training2)



prp(tree1,varlen=2)				    # Shorten variable names if too many variables

rpart.plot(tree1)

# Each node shows:
# - the predicted class (Yes or No)
# - the predicted probability of Yes
# - the percentage of observations in the node

fancyRpartPlot(tree1)			  	       # A fancy plot from rattle

Act1 <- validation2$Robot

tree.pred = predict(tree1 , validation2, type="class")
table(tree.pred,Act1)


#----------- PART (b) --------------------

#----------- PART (c) --------------------

  # Calculation done on word file.

#----------- PART (c) --------------------

#----------- PART (d) --------------------

 # Random forest
library(randomForest)

training2[-13]


# trainData = data.frame( x=training2[-13], y=training2[13] )


Model3 <- randomForest(training2[,-13], as.factor(training2[,13]), ntree=500 )# ntree=500

Mod3Pre = predict(Model3,validation2[-13])

Act <- validation2$Robot

table(Mod3Pre,Act)

length(which(validation2$Robot == "No"))

#test <- importance(Model3)

#test <- as.data.frame(test)

Model3$importance

varImpPlot(Model3,type=2)

#----------- PART (d) --------------------

#----------- PART (e) --------------------

 # Compare error rate

#----------- PART (e) --------------------
