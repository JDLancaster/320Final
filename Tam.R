require(xlsx)
require(dplyr)
require(tidyr)
require(randomForest)
require(popbio)

data <- read.csv("pimaData.csv")
data <- filter(data, Plasma>0, BP>0, BMI>0)
data <- data[-c(1)]

rf <- randomForest(Diabetic~., data=data)
varImpPlot(rf)

#Randomly divide the observations into a training and test set with a standard 66% split
train_sample <- sample(nrow(data), size=0.66*nrow(data))
train <- data[train_sample,]
test <- data[-train_sample,]

glm_fit <- glm(Diabetic ~ Plasma+BMI+Age+Pedigree, data = train, family="binomial"(link = "logit"))
summary(glm_fit)
exp(coef(glm_fit))

glm_probs <- predict(glm_fit, test, type="response")
glm_pred <- rep("0", 247)
glm_pred[glm_probs>.5] <- "1"
table(glm_pred, test$Diabetic)
mean(glm_pred==test$Diabetic)

# ggplot(data, aes(x=Plasma, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
# ggplot(data, aes(x=BMI, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
# ggplot(data, aes(x=Age, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
# ggplot(data, aes(x=Pedigree, y=Diabetic)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

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

