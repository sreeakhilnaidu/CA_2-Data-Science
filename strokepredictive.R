# Load packages
library(mice)
library(ggplot2)
library(caret)
library(dplyr)
library(GGally)
library(ROSE)
library(randomForest)
library(e1071)

# Load data
stroke<-read.csv("stroke.csv")

sapply(stroke, class)

# Data preparation
stroke$stroke<- factor(stroke$stroke, levels = c(0,1), labels = c("No", "Yes"))
stroke$gender<-as.factor(stroke$gender)
stroke$hypertension<- factor(stroke$hypertension, levels = c(0,1), labels = c("No", "Yes"))
stroke$heart_disease<- factor(stroke$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
stroke$ever_married<-as.factor(stroke$ever_married)
stroke$work_type<-as.factor(stroke$work_type)
stroke$Residence_type<-as.factor(stroke$Residence_type)
stroke$smoking_status<-as.factor(stroke$smoking_status)
stroke$bmi<-as.numeric(stroke$bmi)

summary(stroke)

# Exploratory data analysis
## BMI Vs Age
qplot(stroke$age, stroke$bmi, xlab = "Age of the Patient", ylab ="BMI of the Patient")

## Work type Vs Stroke
g1<-ggplot(stroke, aes(x = work_type, fill = stroke))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")
g1

## Ever married Vs Stroke
g2<-ggplot(stroke, aes(x = stroke, fill = ever_married))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")
g2

## Smoking Status Vs stroke for each Residence Type and Glucose levels
g3<-ggplot(stroke, aes(x = stroke, y = avg_glucose_level))+
  geom_bar(aes(fill = Residence_type),stat = "identity", position = position_dodge(0.9))+
  facet_wrap(~smoking_status, scales = "free")
g3

# Predictive modeling
## Train and test split creation
### Impute missing data or NA's
ImputedData<-mice(stroke, m = 5, method = ifelse(colnames(stroke) == "bmi", "pmm", ""), maxit = 20)
stroke_processed<-complete(ImputedData, 5)

summary(stroke_processed)

stroke_processed<-stroke_processed[2:12]
part<-createDataPartition(stroke_processed$stroke, p = 0.75, list = FALSE)
train<-stroke_processed[part,]
test<-stroke_processed[-part,]
dim(train)
dim(test)

### Re-sample using "both" method from ROSE library, control and weight functions for modeling.
both<-ovun.sample(stroke~.,
                  data = train,
                  method = "both",
                  p = 0.5,
                  N = 3833)$data

ctrl<-trainControl(method = "repeatedcv",
                   number = 10,
                   repeats = 5,
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE)

model_weights<-ifelse(train$stroke == "Yes",
                      (1/table(train$stroke)[1])*0.5,
                      (1/table(train$stroke)[2])*0.5)

## Model building
model<-randomForest(stroke~.,
                    data = both,
                    trControl = ctrl,
                    weights = model_weights,
                    proximity = TRUE)
model
plot(model)

## Model validation
confusionMatrix(predict(model, test), test$stroke)

## Model forecasting
prediction <-predict(model, test)
head(prediction)

varImp(model)
varImpPlot(model)

