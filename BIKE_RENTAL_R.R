rm(list=ls(all=T))

#Load Libraries
a = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(a)
lapply(a, require, character.only = TRUE)
rm(a)

## Read the data
day = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))
str(day)
df=day
df=subset(day,select = -c(dteday))
day=df

##################################Missing Values Analysis###############################################
value_mis = data.frame(apply(day,2,function(x){sum(is.na(x))}))
value_mis$Columns = row.names(value_mis)
names(value_mis)[1] =  "Missing_percentage"
value_mis$Missing_percentage = (value_mis$Missing_percentage/nrow(day)) * 100
value_mis = value_mis[order(-value_mis$Missing_percentage),]
row.names(value_mis) = NULL

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
index_num = sapply(day,is.numeric) #selecting only numeric

num_data = day[,index_num]

c_names = colnames(num_data)

for (i in 1:length(c_names))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (c_names[i]), x = "cnt"), data = subset(day))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=c_names[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",c_names[i])))
}
# 
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)

# # #loop to remove from all variables
for(i in c_names){
  print(i)
  val = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
  #print(length(val))
  day = day[which(!day[,i] %in% val),]
}

# #Replace all outliers with NA and impute

for(i in c_names){
  value = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
  #   #print(length(val))
  day[,i][day[,i] %in% value] = NA
}
# 
# IMpute the NA from predictors windspeed and casual
day$holiday[is.na(day$holiday)] = mean(day$holiday,na.rm = T)
day$hum[is.na(day$hum)] = mean(day$hum,na.rm = T)
day$windspeed[is.na(day$windspeed)] = mean(day$windspeed,na.rm = T)
day$casual[is.na(day$casual)] = mean(day$casual,na.rm = T)
day1=day
day1=subset(day,select = -c(holiday))

##################################Feature Selection################################################

df1 =day1
day1=df1
index_num = sapply(day1,is.numeric) #selecting only numeric

numeric_data = day1[,index_num]

cnames = colnames(numeric_data) #column names of only numeric variables

#  get the correlation plot
corrgram(day1[,index_num], order = F ,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# Dimension Reduction

day_sub = subset(day,select = -c(instant,atemp,holiday,registered,casual))
day1=day_sub

#Divide the data into train and test
#set.seed(123)
train_index = sample(1:nrow(day1), 0.8 * nrow(day1))
train_data = day1[train_index,]
test_data = day1[-train_index,]

# Perform analysis of variance
fit = rpart(cnt ~ ., data = train_data, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test_data[,-10])

#MAPE
#calculate MAPE

MAPE = function(y, y_pred){
  mean(abs((y - y_pred)/y))
}
MAPE(test_data[,10], predictions_DT) 



# MAPE = function(y, yhat){
#   mean(abs((y - yhat)/y))
# }
# MAPE(test_data[,10], predictions_DT) 

#ERROR RATE: 23.91
#ACCURACY:76.09

#Linear Regression 
#check  for multicollearity of independent variables
library(usdm)
vif(df1[,-10])

vifcor(df1[,-10], th = 0.9)

#run regression model
reg_model = lm(cnt ~., data = train_data)

#Summary of the model
summary(reg_model)

#Predict
predictions_LR = predict(reg_model, test_data[,1:10])

#Calculate MAPE
MAPE(test_data[,10], predictions_LR)

# #Error Rate: 16.38
# #acuracy: 83.62

