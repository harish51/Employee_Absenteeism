#Clearing RAM
rm(list = ls())

#importing all the required libraries

library(plyr)
library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)
library(rpart)
library(DataExplorer)
library(ggthemes)
library(grid)
library(gridExtra)
library(factoextra)
library(FactoMineR)
library(forcats)
library(mice)
library(VIM)
library(caret)
library(caTools)
library(class)
library(MASS)
library(NbClust)
library(fossil)

#Knowing the working directory
getwd()

#setting the working directory
setwd("C:/Users/Harish/Desktop/Projects")

#Importing the data
eadata  = read_excel("Absenteeism_at_work_Project.xls")

#Understanding the data or the summary of the day data

head(eadata,5)
summary(eadata)
View(eadata)

#Changing the variable or attribute names to a simple name

names(eadata) = c("id", "reason","month","day","season","texpense",
                  "distance","service","age","work_avg","hit_target",
                  "disp_fail","education","son","drinker","smoker","pet",
                  "weight","height","bmi","absent_time")

#Knowing the data type of the variables
str(eadata)

#converting the data type of required variables to categorical form

eadata$reason     =as.factor(eadata$reason)
eadata$month      =as.factor(eadata$month)
eadata$day        =as.factor(eadata$day)
eadata$season     =as.factor(eadata$season)
eadata$disp_fail  =as.factor(eadata$disp_fail)
eadata$education  =as.factor(eadata$education)
eadata$son        =as.factor(eadata$son)
eadata$drinker    =as.factor(eadata$drinker) 
eadata$smoker     =as.factor(eadata$smoker) 
eadata$pet        =as.factor(eadata$pet)


#Cheking for any missing values in the dataset
sum(is.na(eadata))

#Finding the no of missing values for each variable

sapply(eadata, function(x) sum(is.na(x)))

#plotting missing values on a single plot using VIM package

miss_plot = aggr(eadata, col=mdc(1:2),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(eadata), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


#we have missing values in many columns with total of 135 values

#Imputing Missing Values using MICE package with max iterations of 10

mice_imputes = mice(eadata , m=5, maxit = 10)

#knowing the methods used by MICE to impute the values of each column

mice_imputes$method

#Selecting the best imputed data sets by MICE
#Lets take the 5th data set

feadata = complete(mice_imputes,5)

#checking for completeness of the imputed data

sum(is.na(feadata))

#Now we have data with all values imputed
#Data Exploration
#plotting data for better understanding

p  = ggplot(feadata, aes(x = pet, fill = pet)) + geom_bar() 
s  = ggplot(feadata, aes(x = son, fill = son)) + geom_bar()
ss = ggplot(feadata, aes(x = smoker, fill = smoker)) + geom_bar()
sd = ggplot(feadata, aes(x = drinker, fill = drinker)) + geom_bar()
d  = ggplot(feadata, aes(x = day, fill = day)) + geom_bar() 
se = ggplot(feadata, aes(x = season,fill = season)) + geom_bar()

grid.arrange(p,s, nrow = 1)
grid.arrange(ss,se, nrow = 1)
grid.arrange(sd, d, nrow = 1)

# creating a new variable 'absent' to filter absentees

absent = as.data.frame( feadata %>% filter(absent_time > 0))

#Plotting 'absent' againest all variables to understand each variable imapct on absent time
#Plotting againest seasons

season1 = as.data.frame(absent %>% group_by(season) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(season1,aes(x= reorder(season,percent), y= percent, fill = season)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('season')

#From the plot it seems every season have around same absent_time but a little 
#more in autumn and spring

#Plotting againest disciplinary failure

disciplinary = as.data.frame(absent %>% group_by(disp_fail) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(disciplinary,aes(x= reorder(disp_fail,percent), y= percent, fill = disp_fail)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Disciplinary failure')

#No dispilinary failures for absentees

#plotting againest each medical reason

med_reason = as.data.frame(absent %>% group_by(reason) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(med_reason,aes(x = reorder(reason,percent), y= percent, fill= reason)) + geom_bar(stat = 'identity') + coord_flip() + theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + xlab('Reason for absence')

#Top medical reasons are 23,28 and 27

#Plotting againest Social drinker

ggplot(absent,aes(x= age,y= absent_time,fill= drinker)) + geom_bar(stat='identity',position= position_dodge()) + 
  scale_x_continuous(breaks =c(seq(20,60,5)),limits=c(20,60))

#Middle age group people are drinking and with high absent time

#Plotting Hit target across service time

ggplot(absent,aes(x= service,y= hit_target)) + geom_point()+ geom_smooth(method = 'loess') + 
  ggtitle('Analysis of Hit target across Service time') + xlab('Service time(years)') + ylab('Hit target(%)')

#service time is showing same trend as age

#Plotting Hit target across age

ggplot(absent,aes(x= age,y= hit_target)) + geom_point()+ geom_smooth(method = 'loess') + 
  ggtitle('Analysis of Hit target across Age') + xlab('Age') + ylab('Hit target(%)')

#Analysis of travel expense across distance

ggplot(absent,aes(x= texpense,y= distance)) + geom_point()+ geom_smooth(method = 'loess') + 
  ggtitle('Analysis of distance and texpense') + xlab('Travel Expense') + ylab('Distance')

#The travel expense is more above 35 range of distance
#analysis of travel expense across absenteeism time

ggplot(absent,aes(x= texpense,y= absent_time)) + geom_point()+ geom_smooth(method = 'loess') + 
  ggtitle('Analysis of absent time and texpense') + xlab('Travel Expense') + ylab('Absent Time')

#Travel expense is not showing much variations in absent_time

#analysis of distance across absenteeism time
ggplot(absent,aes(x= distance,y= absent_time)) + geom_point()+ geom_smooth(method = 'loess') + 
  ggtitle('Analysis of absent time and distance') + xlab('Distance') + ylab('Absent Time')

#above 35 range distance have more absent hours

#we can see from the plots every variable is contributing to absenteeism
#Lets reduce the dimensionality of data set by selecting important variables only
#We use PCA for Dimensionality Reduction
#coverting variables to numeric to carry out PCA

feadata$reason     =as.numeric(feadata$reason)
feadata$month      =as.numeric(feadata$month)
feadata$day        =as.numeric(feadata$day)
feadata$season     =as.numeric(feadata$season)
feadata$disp_fail  =as.numeric(feadata$disp_fail)
feadata$education  =as.numeric(feadata$education)
feadata$son        =as.numeric(feadata$son)
feadata$drinker    =as.numeric(feadata$drinker) 
feadata$smoker     =as.numeric(feadata$smoker) 
feadata$pet        =as.numeric(feadata$pet)

#Scaling the data for PCA

peadata = feadata 
  
peadata = scale(peadata)


pcaex = PCA(peadata,graph = F)

#eigen values 
egval = get_eigenvalue(pcaex)
fviz_eig(pcaex,addlabels=T)

#correlation of variables with PCA components
fviz_pca_var(pcaex,col.var='red')

#quality of presentation of variables in correlogram
fviz_cos2(pcaex,choice='var',axes=1:2)

#contribution of variables to the respective principal components
fviz_contrib(pcaex,choice='var',axes=1)

#Feature selection of dataset based on PCA analysis
#Selecting only impacting variables 
meadata = subset(feadata, select=c("id", "reason","month","day","season",
                                   "texpense","distance","service","age",
                                   "work_avg","hit_target","disp_fail",
                                   "education","bmi","absent_time"))

#plotting histogram to see the impacts of variables

plot_histogram(meadata)

#checking for correlation among selected variables

plot_correlation(meadata)

#There is no much correlated variables in our selected data
#Converting required variables to categorical type

meadata$reason     =as.factor(meadata$reason)
meadata$month      =as.factor(meadata$month)
meadata$day        =as.factor(meadata$day)
meadata$season     =as.factor(meadata$season)
meadata$disp_fail  =as.factor(meadata$disp_fail)
meadata$education  =as.factor(meadata$education)
meadata$absent_time=as.factor(meadata$absent_time)

#Now the data is ready for feeding to the model
#Model building

#Starting with KNN

#set seed to ensure you always have same random numbers generated
set.seed(53)

#splitting the data into train and test sets

sample = sample.split(meadata,SplitRatio = 0.80) 

train_data = subset(meadata,sample ==TRUE) 

test_data = subset(meadata, sample==FALSE)

#Building the model with k_value =2

knn_pred = knn(train_data, test_data, cl = train_data$absent_time, 2)

#evaluating the model

KNN_CM = confusionMatrix(test_data$absent_time, knn_pred)

KNN_CM$overall

#Accuracy =35 with K=2

#Now with K value 4

knn_pred2 = knn(train_data, test_data, cl = train_data$absent_time, 4)

KNN_CM2 = confusionMatrix(test_data$absent_time, knn_pred2)

KNN_CM2$overall

#Accuracy =39 with k=4

#Now with K value 6

knn_pred3 = knn(train_data, test_data, cl = train_data$absent_time, 6)

KNN_CM3 = confusionMatrix(test_data$absent_time, knn_pred3)

KNN_CM3$overall

#Accuracy =32 with k = 6

#It seems K=4 is giving better accuracy in KNN


#Building Model with Decision Tree
#Multi Class Classification with Decision Tree Classifier

DTree = rpart(absent_time~., data = train_data)

DTree_pred = predict(DTree, test_data, type = "class")

#Evaluating the Model

DTree_CM = confusionMatrix(test_data$absent_time, DTree_pred)

DTree_CM$overall

#Accuracy = 54

#Building Clusttering Model with K means
#K_Means Clusttering 

#Converting the variable type
meadata$reason     =as.numeric(meadata$reason)
meadata$month      =as.numeric(meadata$month)
meadata$day        =as.numeric(meadata$day)
meadata$season     =as.numeric(meadata$season)
meadata$education  =as.numeric(meadata$education)
meadata$disp_fail  =as.numeric(meadata$disp_fail)
meadata$absent_time=as.numeric(meadata$absent_time) 


#Scaling the data
kmeadata = data.frame(scale(meadata[-15]))

#knowing the optimum K_value
nbclust_result = NbClust(kmeadata,min.nc = 2,max.nc = 15,method = "kmeans")

barplot(table(nbclust_result$Best.n[1,]),
        xlab = "No of Clusters",
        ylab = "No of Criteria",
        main = "No of Clusters Choosen")

#Building the model with K value

kmeans_model = kmeans(kmeadata,5,nstart = 25)

#Summary of the model
kmeans_model

#Evaluating the model

kcluster_accuracy = table(meadata$absent_time,kmeans_model$cluster)

rand.index(meadata$absent_time,kmeans_model$cluster)

#Accuracy =67

#Generally Clusttering Models give more accuracy than Classification
#Among Classification models, Decision tree is giving more accuracy than any of its type
