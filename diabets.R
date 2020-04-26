rm(list=ls(all=T))

#Load Libraries
a = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(a)
lapply(a, require, character.only = TRUE)
rm(a)

## Read the data
day = read.csv("diabetes.csv", header = T, na.strings = c(" ", "", "NA"))
str(day)
day$Outcome= as.factor(day$Outcome)
df=day
day=df

######## Determine datatype of each variable
class(day$Glucose)
nrow(day[which(day$Glucose == 0 ),])
day = day[-which(day$Glucose == 0),]

class(day$Pregnancies)
nrow(day[which(day$Pregnancies == 0 ),])

class(day$BloodPressure)

class(day$SkinThickness)
nrow(day[which(day$SkinThickness == 0 ),])
day=day[-which(day$SkinThickness == 0 ),]

class(day$Insulin)
class(day$BMI)
nrow(day[which(day$BMI == 0 ),])
day=day[-which(day$BMI == 0 ),]

class(day$Outcome)
class(day$DiabetesPedigreeFunction)
nrow(day[which(day$DiabetesPedigreeFunction == 0 ),])

###############Missing Values Analysis####################################

value_mis = data.frame(apply(day,2,function(x){sum(is.na(x))}))
value_mis$Columns = row.names(value_mis)
names(value_mis)[1] =  "Missing_percentage"
value_mis$Missing_percentage = (value_mis$Missing_percentage/nrow(day)) * 100
value_mis = value_mis[order(-value_mis$Missing_percentage),]
row.names(value_mis) = NULL
value_mis$Missing_percentage


################Outlier Analysis##########################################

# ## BoxPlots - Distribution and Outlier Check

index_num = sapply(day,is.numeric) #selecting only numeric

num_data = day[,index_num]

c_names = colnames(num_data)


for (i in 1:length(c_names))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (c_names[i]), x = "Outcome"), data = subset(day))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=c_names[i],x="Outcome")+
           ggtitle(paste("Box plot of Outcome for",c_names[i])))
}

 
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,ncol=2)



######### IMPUTE OUTLIERS

# #Replace all outliers with NA and impute

for(i in c_names){
  value = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
  #   #print(length(val))
  day[,i][day[,i] %in% value] = NA
}

# KNN IMPUTATION
day = knnImputation(day, k = 3)


#### Visualisation of variable  distribution 

hist(day$Glucose)
hist(day$BloodPressure)
hist(day$SkinThickness)
hist(day$Insulin)
hist(day$BMI)
hist(day$Age)
hist(day$DiabetesPedigreeFunction)


################# Feature Scaling

for(i in c_names){
  print(i)
  day[,i] = (day[,i] - min(day[,i]))/(max(day[,i] - min(day[,i])))
}

###### Post normalization visualizations

hist(day$Glucose)
hist(day$BloodPressure)
hist(day$SkinThickness)
hist(day$Insulin)
hist(day$BMI)
hist(day$Age)
hist(day$DiabetesPedigreeFunction)

##############Feature Selection###################

corrgram(day[,index_num],upper.panel=panel.pie, main = "Correlation Plot")

# Dimension Reduction

day=subset(day,select=-c(SkinThickness))

######## Divide the dataset into trainin and testing data
train.index = createDataPartition(day$Outcome, p = .80, list = FALSE)
train = day[ train.index,]
test  = day[-train.index,]




#Decision tree for classification
#Develop Model on training da
set.seed(1234)
C50_model = C5.0(Outcome ~., train, trials = 50, rules = TRUE)

#Summary of DT model
summary(C50_model)

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-8], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$Outcome, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
#FNR = FN/FN+TP 
# 8/ (63+8) * 100 = 11%
#Accuracy = 83.02%


###Random Forest
RF_model = randomForest(Outcome ~ ., train, importance = TRUE, ntree = 500)
#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  
# 
# #Extract rules
exec = extractRules(treeList, train[,-8])  # R-executable conditions
# 
# #Visualize some rules
exec[1:2,]

#Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
# 
# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-8], train$Outcome)  # get rule metrics
# 
# #evaulate few rules
ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-8])

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Outcome, RF_Predictions)
confusionMatrix(ConfMatrix_RF)# 

#Accuracy=81.13%

#Naive Baye's
library(e1071)

#Develop model
NB_model = naiveBayes(Outcome ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:8], type = 'class')
mean(NB_Predictions == test$Outcome)

#Look at confusion matrix
Conf_matrix = table(observed = test[,8], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy=78.3%

##KNN Implementation
library(class)

#Predict test data
KNN_Predictions = knn(train[, 1:8], test[, 1:8], train$Outcome, k = 3)

#Confusion matrix
Conf_matrix = table(KNN_Predictions, test$Outcome)

#Accuracy
sum(diag(Conf_matrix))/nrow(test)

mean(NB_Predictions == test$Outcome)
day
#Divide the data into train and test
#set.seed(123)

train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]



#### Logistic Regression
# ##rpart for regression
logit_model= glm(Outcome ~., data=train,family=binomial)

summary(logit_model)
#Predict for new test cases
logit_predict= predict(logit_model, newdata=test, type="response")

logit_predict=ifelse(logit_predict>0.5,1,0)

#confusion matrix
confusion_matrix=table(test$Outcome,logit_predict)
#Accuracy
#76.66

