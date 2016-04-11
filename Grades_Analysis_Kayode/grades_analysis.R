setwd("/Users/aimhighfly/Documents/StonyBrookUniversity/Spring_semester/EST508/Projects/grades")

library("car")
library("caret")
library("MVA")
library("MASS")

# Intepret significant
# t value >1.96
# P(>|t|) < 0.05
# Multiple R-squared shows the amount of Y explained by X


data1 = read.csv("4.csv", stringsAsFactors = F)
# View(data1)

record1 = subset(data1, select = c(TotalGrade, 
                                  TotalTimeStudentsspentinCourseinHours, 
                                  TotalDBMFhits,
                                  DBWeek1MFhits,
                                  DBWeek2MFhits,
                                  DBWeek3MFhits,
                                  DBWeek4MFhits,
                                  DBWeek5MFhits,
                                  DBWeek6MFhits,
                                  DBWeek7MFhits,
                                  DBWeek8MFhits,
                                  DBWeek9MFhits,
                                  DBWeek10MFhits,
                                  TotalNumberofDBposts,
                                  DBWeek1Numberofposts,
                                  DBWeek2Numberofposts,
                                  DBWeek3Numberofposts,
                                  DBWeek4Numberofposts,
                                  DBWeek5Numberofposts,
                                  DBWeek6Numberofposts,
                                  DBWeek7Numberofposts,
                                  DBWeek8Numberofposts,
                                  DBWeek9Numberofposts,
                                  DBWeek10Numberofposts))
# View(record1)
record1
# delete rows with NA
record1 <- na.omit(record1)
# delelte rows with "-"
record2 = subset(record1, TotalGrade!="-" & TotalGrade!="" & 
                   TotalDBMFhits!="-" &  TotalDBMFhits!= 0 ,
                   TotalNumberofDBposts!="-"
                  )

record3 <- data.frame(TotalGrade = as.numeric(as.character(record2$TotalGrade)), 
                      TotalDBMFhits = as.numeric(as.character(record2$TotalDBMFhits)),
                      DBWeek1MFhits= as.numeric(as.character(record2$DBWeek1MFhits)),
                      DBWeek2MFhits= as.numeric(as.character(record2$DBWeek2MFhits)),
                      DBWeek3MFhits= as.numeric(as.character(record2$DBWeek3MFhits)),
                      DBWeek4MFhits= as.numeric(as.character(record2$DBWeek4MFhits)),
                      DBWeek5MFhits= as.numeric(as.character(record2$DBWeek5MFhits)),
                      DBWeek6MFhits= as.numeric(as.character(record2$DBWeek6MFhits)),
                      DBWeek7MFhits= as.numeric(as.character(record2$DBWeek7MFhits)),
                      DBWeek8MFhits= as.numeric(as.character(record2$DBWeek8MFhits)),
                      DBWeek9MFhits= as.numeric(as.character(record2$DBWeek9MFhits)),
                      DBWeek10MFhits= as.numeric(as.character(record2$DBWeek10MFhits)),
                      TotalNumberofDBposts = as.numeric(as.character(record2$TotalNumberofDBposts)),
                     DBWeek1Numberofposts= as.numeric(as.character(record2$DBWeek1Numberofposts)),
                     DBWeek2Numberofposts= as.numeric(as.character(record2$DBWeek2Numberofposts)),
                     DBWeek3Numberofposts= as.numeric(as.character(record2$DBWeek3Numberofposts)),
                     DBWeek4Numberofposts= as.numeric(as.character(record2$DBWeek4Numberofposts)),
                     DBWeek5Numberofposts= as.numeric(as.character(record2$DBWeek5Numberofposts)),
                     DBWeek6Numberofposts= as.numeric(as.character(record2$DBWeek6Numberofposts)),
                     DBWeek7Numberofposts= as.numeric(as.character(record2$DBWeek7Numberofposts)),
                     DBWeek8Numberofposts= as.numeric(as.character(record2$DBWeek8Numberofposts)),
                     DBWeek9Numberofposts= as.numeric(as.character(record2$DBWeek9Numberofposts)),
                     DBWeek10Numberofposts= as.numeric(as.character(record2$DBWeek10Numberofposts))
                     ) 
record3 <- na.omit(record3)
 View(record3)

pairs(record3)
View(record3)
record3[which(record3[,1]>110),1]<- 100
record3[which(record3[,1]>110),1]
record_matrix = as.matrix(record3)
record_scale = scale(record_matrix)
record_dataframe = as.data.frame(record_scale)

hist(record3$TotalGrade)

lm1 = lm(TotalGrade ~ TotalDBMFhits, record_dataframe)
summary(lm1)
plot(lm1)

lm2 = lm(TotalGrade ~ TotalNumberofDBposts, record_dataframe)
summary(lm2)
plot(lm2)

lm3 = lm(TotalGrade ~ DBWeek1Numberofposts+DBWeek8Numberofposts+DBWeek9Numberofposts+DBWeek10Numberofposts, record_dataframe)
summary(lm3)
plot(lm3)

lm4 = lm(TotalGrade ~ ., record_dataframe)
summary(lm4)
plot(lm4)


influenceIndexPlot(lm4)

# polynomial lm

lmp = lm(TotalGrade ~ poly(TotalNumberofDBposts,3) + poly(TotalDBMFhits,3),record_dataframe)
summary(lmp)

lmp2 = lm(TotalGrade ~ I(TotalNumberofDBposts^2) + TotalDBMFhits + I(TotalDBMFhits^3) ,record_dataframe)
summary(lmp2)

lmp3 = lm(TotalGrade ~ poly(TotalNumberofDBposts,1),record_dataframe)
summary(lmp3)

# good news
record_dataframe2 = subset(record_dataframe, select = c(TotalGrade, TotalDBMFhits))

plot(record_dataframe2$TotalDBMFhits,record_dataframe2$TotalGrade,
     col='deepskyblue4',xlab='q',main='Observed data')

lmp4 = lm(TotalGrade ~ poly(TotalDBMFhits,5),record_dataframe2)
summary(lmp4)

plot(TotalGrade ~ TotalDBMFhits , record_dataframe2)
lines(lmp4)

predicted.intervals <- predict(lmp4,record_dataframe2,interval='confidence',
                               level=0.99)
lines(record_dataframe2$TotalDBMFhits,predicted.intervals[,1],col='green',lwd=3)
lines(record_dataframe2$TotalDBMFhits,predicted.intervals[,2],col='black',lwd=1)
lines(record_dataframe2$TotalDBMFhits,predicted.intervals[,3],col='red',lwd=1)


# clustering
record.cluster = record3
hist(record.cluster$TotalGrade)
hist(record.cluster$TotalDBMFhits)
hist(record.cluster$TotalNumberofDBposts)

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

set.seed(7)
fit.lda <- train(TotalGrade ~ .,
                 data = record_dataframe2, method = "lda", metric = metric, trControl = control)
print(fit.lda)

results <- resamples(lda = fit.lda)
summary(results)

predicted.intervals <- predict(lmp,record_dataframe,interval='confidence',
                               level=0.99)


lev = hat(model.matrix(lm))
plot(lev)
subset(record_dataframe, lev>0.2)
avPlots(lm)

boxplot(record6)

featurePlot(recordc, recordd, plot = "ellipse")

library(squash)
hist2(record7)

library(hexbin)
splom(record3, panel=panel.hexbinplot)

lm1 = lm(TotalGrade ~. , record3)
summary(lm1)
layout(matrix(1:4,2,2)) 
plot(lm1)


# Outlier, Bonferonni test 
outlierTest(lm)

# High leverage (hat) points
influenceIndexPlot(lm1)
# Cook's distance measures how much an observation influences the overall model or predicted values
# Studentizided residuals are the residuals divided by their estimated standard deviation as a way tostandardized
# Bonferroni test to identify outliers
# Hat-points identify influential observations (have a high impact on the predictor variables)

# Influence plot 
influencePlot(lm1)


# Testing for normality
qqPlot(lm1)

# Testing for heteroskedasticity
ncvTest(lm1)
# Breusch-Pagan test
# At 99% we reject the null and conclude that residuals are not homogeneous.

# Testing for multicolinearity
vif(lm1)
# gvif> 4 suggests collinearity.

stalac(record3)



# Evaluate 5 algorithms
# Linear Discriminat Analysis(LDA)
# Classification and Regression Trees(CART)
# k-Nearest Neighbors(kNN)
# Support Vector Machine(SVM) with a linear kernel
# Random Forest(RF)


data11 = read.csv("11.csv")
record1 = subset(data11, select = c(LetterGrade, TotalTimeStudentsspentinCourseinHours, 
                                   TotalNumberofDBposts, TotalDBMFhits))
# data1$TotalNumberofDBposts
# delete rows with NA
record1 <- na.omit(record1)
# delelte rows with "-"
record2 = subset(record1, LetterGrade!="" & 
                   TotalTimeStudentsspentinCourseinHours!="-"&
                   TotalDBMFhits!="-" &  
                   TotalNumberofDBposts!="-"
)
record22 <- as.data.frame(record2)
record2 <- dataframe(record2$LetterGrade)
record3 <- data.frame(LetterGrade = record2$LetterGrade, 
                      TotalTimeStudentsspentinCourseinHours = as.numeric(as.character(record2$TotalTimeStudentsspentinCourseinHours)),
                      TotalDBMFhits = as.numeric(as.character(record2$TotalDBMFhits)),
                      TotalNumberofDBposts = as.numeric(as.character(record2$TotalNumberofDBposts))
) 
record4 = subset(data1, select = c(LetterGrade, TotalTimeStudentsspentinCourseinHours))
record4 <- na.omit(record4)
record4 = subset(record1, LetterGrade!="" & 
                   TotalTimeStudentsspentinCourseinHours!="-")


recorda = subset(record3, select = c(TotalTimeStudentsspentinCourseinHours, TotalDBMFhits, TotalNumberofDBposts))
recordb = subset(record3, select = LetterGrade)
recordc= as.matrix(recorda)
recorde = round(recordb, -1)
recordd= as.factor(recorde$TotalGrade)

record7 = subset(record3, select = c(LetterGrade,TotalTimeStudentsspentinCourseinHours))
# Run algorithm using 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

set.seed(7)
fit.lda <- train(LetterGrade ~., data=record3, method = "lda", metric = metric, trControl = control)
fit.cart <- train(LetterGrade ~., data=record3, method = "rpart", metric = metric, trControl = control)
fit.knn <- train(LetterGrade ~., data=record3, method = "knn", metric = metric, trControl = control)
fit.svm <- train(LetterGrade ~., data=record3, method = "svmRadial", metric = metric, trControl = control)
fit.rf <- train(LetterGrade ~., data=record3, method = "rf")

results <- resamples(list(lda = fit.lda, cart = fit.cart, svm = fit.svm))

print(fit.lda)

summary(results)

# Analysis of the second dataset
data2 = read.csv("2.csv")
# data cleaning
# choose columns "Grade" and "Courseaccess"
record = subset(data2, select = c(Grade, StudyHrs,Courseaccess, CourseExperience, GPA))
record0 = subset(data2, select = c(Grade, StudyHrs, GPA))
# delete rows with NA
record <- na.omit(record)

recorda = subset(record, select = c(StudyHrs,Courseaccess, CourseExperience, GPA))
recordb = subset(record, select = Grade)
recordc= as.matrix(recorda)
recordd= as.factor(recordb$Grade)

featurePlot(recordc, recordd, plot = "ellipse")

record0 <- na.omit(record0)
record00 = data.frame(new = (4-record0$GPA)*0.5 + record0$Grade, StudyHrs = record0$StudyHrs)

influenceIndexPlot(fit)

fit <- glm(Grade~StudyHrs+GPA,data=record0,family=quasipoisson())
summary(fit)

lm2 = lm(record0$Grade ~ record0$GPA + I(log(record0$StudyHrs)), record0)
summary(lm2)

lm1 = lm(GPA*Grade ~ StudyHrs + StudyHrs, record)
summary(lm1)

plot(record0$StudyHrs, (4+record0$GPA)*0.5 +record0$Grade, ylim=c(12, 14))

abline(12.93793,0.04680)

kmeans = kmeans(record00, centers = 2)

library(cluster)
library(HSAUR)
library(fpc)
km    <- kmeans(record00,2)

dissE <- daisy(record00) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)
# pre-analysis of Grade and Courseraccess
plotcluster(record00, km$cluster)

clusplot(record00, km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

hist(record$Grade)
plot(density(record$Grade,na.rm=TRUE))
plot(sort(record$Grade),pch=".")

hist(record$StudyHrs)
plot(density(record$StudyHrs,na.rm=TRUE))
plot(sort(record$StudyHrs),pch=".")

boxplot(record)

plot(record$Grade ~ record$StudyHrs)


# linear regression
lm1 = lm(record$Grade ~., record)
summary(lm1)
layout(matrix(1:4,2,2)) 
plot(lm1)

plot(lm1$fitted.values, record$Grade)
abline(0,1)

# polynomial regression

lm2 = lm(record$Grade ~ record$Courseaccess + I(record$Courseaccess^2))
summary(lm2)
plot(record$Courseaccess, record$Grade)
abline(lm2)

lm3 = lm(record$Grade ~ record$Courseaccess + I(record$Courseaccess^2) + I(record$Courseaccess^3))
summary(lm3)
plot(record$Courseaccess, record$Grade)
abline(lm3)

# data spliting

inTrain <- createDataPartition(y = record$Grade, p=0.7, list=FALSE)
training <- record[inTrain,]
testing <- record[-inTrain,]

# Train the model.
fit <- train(Grade ~ StudyHrs, data=training, method='glm')
# Show statistical significance of coefficients (terms).
summary(fit)
# Show accuracy on training set.
fit
plot(fit)
# Run model on test set.
results <- predict(fit, newdata = testing)
# Show accuracy on test set.
plot(testing$Grade, results)
abline(0,1)


# random forest model
library(randomForest)
rfFit <- randomForest(Grade ~ StudyHrs, data=training)
rfPredict <- predict(rfFit, testing)
plot(testing$Grade, rfPredict)
abline(0,1)

library(kernlab)

# SVM

inTrain <- createDataPartition(y = record00$new, p=0.7, list=FALSE)
training <- record00[inTrain,]
testing <- record00[-inTrain,]

fit <- train(new ~ ., data = training, method = "svmRadial")

pred <- predict(fit,testing)

plot(testing$new, pred)

abline(0,1)










