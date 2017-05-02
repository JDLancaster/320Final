#import required libraries   
require(xlsx)
require(dplyr)
require(tidyr)
require(randomForest)
require(popbio)

#import data
data <- read.csv("pimaData.csv")
#only keep observations where Plasma, BP, BMI have values greater than zero
data <- filter(data, Plasma>0, BP>0, BMI>0)
#remove index column   
data <- data[-c(1)]

#perform random forest on the dataset
rf <- randomForest(Diabetic~., data=data)
#create variable importance plot as measured by the random forest
varImpPlot(rf)

#randomly divide the observations into a training and test set with a standard 66% split
train_sample <- sample(nrow(data), size=0.66*nrow(data))
#create the training set
train <- data[train_sample,]
#create the test set
test <- data[-train_sample,]

#fit a logit model on the training set using 4 predictors
glm_fit <- glm(Diabetic ~ Plasma+BMI+Age+Pedigree, data = train, family="binomial"(link = "logit"))
#get a summary of the model
summary(glm_fit)
#obtain the exponentiated values of the logit coefficients,
#since they represent the relative risk ratios 
#which allow an easier interpretation of the logit coefficients
exp(coef(glm_fit))

#obtain predicted probabilities of diabetes in our test set
glm_probs <- predict(glm_fit, test, type="response")
#create a vector of class predictions of 247 elements of "0" 
#(0 means no diabetes; 1 means diabetes; 247 is number of observations in the test set)
glm_pred <- rep("0", 247)
#transforms to "1" all of the elements for which the predicted probability of diabetes exceeds 0.5
glm_pred[glm_probs>.5] <- "1"
#given these predictions, produce a confusion matrix 
table(glm_pred, test$Diabetic)
#compute the accuracy rate of the logistic model
mean(glm_pred==test$Diabetic)

#create logistic curves of probabilities of diabetes based on Plasma, BMI, Age, Pedigree
par(mfrow=c(2,2))
logi.hist.plot(data$Plasma, data$Diabetic, logi.mod=1, type="hist", 
               boxp=FALSE, rug=FALSE, ylabel="Probability of Diabetic",
               ylabel2="Frequency of Plasma", xlabel="Plasma", 
               mainlabel="Probability of Diabetes Based on Plasma Glucose")
logi.hist.plot(data$BMI, data$Diabetic, logi.mod=1, type="hist", 
               boxp=FALSE, rug=FALSE, ylabel="Probability of Diabetic",
               ylabel2="Frequency of BMI", xlabel="BMI", 
               mainlabel="Probability of Diabetes Based on Body Mass Index")
logi.hist.plot(data$Age, data$Diabetic, logi.mod=1, type="hist", 
               boxp=FALSE, rug=FALSE, ylabel="Probability of Diabetic",
               ylabel2="Frequency of Age", xlabel="Age", 
               mainlabel="Probability of Diabetes Based on Age")
logi.hist.plot(data$Pedigree, data$Diabetic, logi.mod=1, type="hist", 
               boxp=FALSE, rug=FALSE, ylabel="Probability of Diabetic",
               ylabel2="Frequency of Pedigree", xlabel="Pedigree", 
               mainlabel="Probability of Diabetes Based on Pedigree Function")

# ggplot(data, aes(x=Plasma, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
# ggplot(data, aes(x=BMI, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
# ggplot(data, aes(x=Age, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
# ggplot(data, aes(x=Pedigree, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
