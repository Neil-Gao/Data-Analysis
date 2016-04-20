# EST530 Final Project      By Xiang Gao  ID:110682110
library(tm)

# cname <- file.path("/", "Users", "aimhighfly", "Documents", "StonyBrookUniversity", "Fall_semester", "Big_Data", "spam/Data_alltxt", "spam_txt")
cname1 <- file.path("/Users/aimhighfly/Documents/StonyBrookUniversity/Fall_semester/Big_Data/spam/Data_alltxt/spam_txt")
cname0 <- file.path("/Users/aimhighfly/Documents/StonyBrookUniversity/Fall_semester/Big_Data/spam/Data_alltxt/ham_txt")

docs1 <- Corpus(DirSource(cname1))
docs0 <- Corpus(DirSource(cname0))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs1 <- tm_map(docs1, toSpace, "/")
docs1 <- tm_map(docs1, toSpace, "@")
docs1 <- tm_map(docs1, toSpace,"\\|")
docs0 <- tm_map(docs0, toSpace, "/")
docs0 <- tm_map(docs0, toSpace, "@")
docs0 <- tm_map(docs0, toSpace,"\\|")

docs1 <- tm_map(docs1, content_transformer(tolower))
docs0 <- tm_map(docs0, content_transformer(tolower))


docs1 <- tm_map(docs1, removePunctuation)
docs0 <- tm_map(docs0, removePunctuation)

# docs1 <- tm_map(docs1, removeWords, stopwords("english"))
# docs0 <- tm_map(docs0, removeWords, stopwords("english"))
# 
# library(SnowballC)

# Stemming uses an algorithm that removes common word endings for English words, such as “es”, “ed” and “s"
# docs <- tm_map(docs, stemDocument)

dtm1 <- DocumentTermMatrix(docs1)
dtm0 <- DocumentTermMatrix(docs0)

index_list = c("address","all","any","available","change","click","deal","email",
               "emails","event","free","may","more","new","now","off","offer",
               "online","only","our","prices","privacy","products","receive",
               "shipping","shop","unsubscribe","valid","view","your","type")

dtm_spam <- DocumentTermMatrix(docs1,list(dictionary = index_list))
freq1 <- rowSums(as.matrix(dtm1))
dtm_spam_matrix <- as.matrix(dtm_spam)
dtm_spam_matrix = dtm_spam_matrix / freq1
spam_index = matrix(data="spam", nrow = nrow(dtm_spam), ncol = 1)
dtm_spam_index <- cbind( dtm_spam_matrix, spam_index)
colnames(dtm_spam_index)[ncol(dtm_spam_index)] <- "detect_index"
rownames(dtm_spam_index) <- paste("spam", 1: nrow(dtm_spam_index))

dtm_ham <- DocumentTermMatrix(docs1,list(dictionary = index_list))
freq0 <- rowSums(as.matrix(dtm0))
dtm_ham_matrix <- as.matrix(dtm_ham)
dtm_ham_matrix = dtm_ham_matrix / freq0
ham_index = matrix(data="nonspam", nrow = nrow(dtm_ham), ncol = 1)
dtm_ham_index <- cbind(dtm_ham_matrix, ham_index)
colnames(dtm_ham_index)[ncol(dtm_ham_index)] <- "detect_index"
rownames(dtm_ham_index) <- paste("nonspam", 1: nrow(dtm_ham_index))

combine <- rbind(dtm_spam_index, dtm_ham_index)

rownames(combine) <- NULL

# combine[,1:ncol(combine)-1] <- as.numeric(combine[,1:ncol(combine)-1])
apply(combine[,1:ncol(combine)-1], c(1, 2), as.numeric)
# combine[,1:ncol(combine)-1] <- as.numeric((combine[,1:ncol(combine)-1]))

combine <- data.frame(apply(combine[,1:ncol(combine)-1], c(1, 2), as.numeric) ,detect_index = combine[,ncol(combine)])

options(max.print = 100000)
# utils::View(combine)

write.csv(combine, file="detect_spam.csv")

require(caret)
require(kernlab)


# Split the data into a training/test set by 60% training/40% test.
inTrain <- createDataPartition(y = combine$detect_index, p=0.7, list=FALSE)
training <- combine[inTrain,]
testing <- combine[-inTrain,]

# # Set random seed value to ensure consistent training results each time.
# set.seed(12345)

# Train the model.
fit <- train(detect_index ~ ., data=training, method='glm')
# Show statistical significance of coefficients (terms).
summary(fit)
# Show accuracy on training set.
fit
plot(fit)
# Run model on test set.
results <- predict(fit, newdata = testing)
# Show accuracy on test set.
confusionMatrix(results, testing$detect_index)
# random forest

library(randomForest)

rfFit <- randomForest(detect_index ~. , data=training)
rfPredict <- predict(rfFit, testing)
confusionMatrix(rfPredict, testing$detect_index)


# buxingyu's data

setwd("/Users/aimhighfly/Documents/StonyBrookUniversity/Fall_semester/Big_Data/spam/")
spam_ham = read.csv('spam&ham.csv')
require(caret)
require(kernlab)



# glm model
glm_accuracy = 0
for (i in 1:50){
inTrain <- createDataPartition(y = spam_ham$type, p = 0.7, list = FALSE)
training <- spam_ham[inTrain,-1]
testing <- spam_ham[-inTrain,-1]
# Train the model.
fit <- train(type ~ ., data = training, method = 'glm')
# Show statistical significance of coefficients (terms).
# summary(fit)
# # Show accuracy on training set.
# fit
# plot(fit)
# Run model on test set.
results <- predict(fit, newdata = testing)
# Show accuracy on test set.
glm_outcome = confusionMatrix(results, testing$type)
glm_accuracy[i] = glm_outcome$overall[1]
}
plot(glm_accuracy, ylim=c(0, 1))

# random forest model
rf_accuracy = 0
library(randomForest)
for(i in 1:50){
rfFit <- randomForest(type ~. , data=training)
rfPredict <- predict(rfFit, testing)
rf_outcome = confusionMatrix(rfPredict, testing$type)
rf_accuracy[i] = rf_outcome$overall[1]
}

plot(rf_accuracy, ylim = c(0,1)) 
# SVM

require(caret)

require(kernlab)

require(doMC)

trainIndex <- createDataPartition(spam_ham$type, p = .7, list = FALSE, times = 1)

dataTrain <- spam_ham[ trainIndex,-1]

dataTest  <- spam_ham[-trainIndex,-1]

registerDoMC(cores=5)

x <- train(type ~ ., data = dataTrain, method = "svmRadial")

pred <- predict(x,dataTest)

confusionMatrix(pred,dataTest$type)


# library(ggplot2)
# subset(wf, freq>100) 
# ggplot(aes(word, freq))
# geom_bar(stat="identity")
# theme(axis.text.x=element_text(angle = 45, hjust=1))

freq[head(ord, n=30)]

freq[tail(ord, n=50)]

head(table(freq), 15)

tail(table(freq), 15)

m <- as.matrix(dtm)

library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)



# fianl presentation

setwd("/Users/aimhighfly/Documents/StonyBrookUniversity/Fall_semester/Big_Data/spam/presentation")

#Prediction
library(caret)
training <- read.csv("training_spam&ham.csv", row.names = 1)
testing <- read.csv("testing_spam&ham.csv", row.names = 1)

# Using glm model to predict
glm_accuracy = 0
glm_time <- 0
for(i in 1:20){
fit_linear <- train(type ~.,  data = training, method = "glm")
pred_linear <- predict(fit_linear, newdata = testing)
# pred_linear
table(pred_linear, testing$type)
# Start the clock
ptm <- proc.time()
glm_outcome = confusionMatrix(pred_linear, testing$type)
# Stop the clock
temp = proc.time() - ptm
glm_time[i] = sum(temp)
glm_accuracy[i] = glm_outcome$overall[1]
}

plot(glm_time, xlab = "time", ylab = "processing time / seconds", main = " ",type = "l", ylim = c(0, 100), col = "red")
title(expression("GLM" * phantom(" Random Forest SVM")), col.main="red")
title(expression(phantom("GLM Random Forest ") * "SVM"), col.main="blue")
title(expression(phantom("GLM ") * "Random Forest" * phantom(" SVM")), col.main="green") 


#Using random-forest to predict
rf_accuracy = 0
rf_time=0 
for(i in 1:20){
fit_rf <- train(type ~., method = "rf", data = training)
# Start the clock
ptm <- proc.time()
pred_rf <- predict(fit_rf, newdata = testing)
# pred_rf
table(pred_rf, testing$type)
confusionMatrix(pred_rf, testing$type)
# Stop the clock
temp <- proc.time() - ptm
rf_time[i] <- sum(temp)
}
lines(rf_time, type = "l", ylim = c(0, 100), col = "green")


# SVM
svm_time = 0
for(i in 1:20){
# Start the clock
ptm <- proc.time()
x <- train(type ~ ., data = training, method = "svmRadial")
pred <- predict(x,testing)
confusionMatrix(pred,testing$type)
# Stop the clock
temp <- proc.time() - ptm
svm_time[i] = sum(temp)
}
lines(svm_time, type = "l", ylim = c(0, 100), col = "blue")


# write.csv(m, file="dtm.csv")



# email = scan(file = filelist, what="list")
# email = paste(email, scan(file = "1.txt", what="list"))


# filelist = list.files(pattern = ".*.txt")
# 
# email = scan(file = filelist, what="list")


#  txt_files = list.files(pattern = "*.txt")
# 
# 
# #  for (f in files) {
# #   
# #    tempData = scan(file = f, what = "character", sep = "", dec =".", skip = 0, na.strings = "NA") 
# #   
# #   # data <- c(data,tempData)  
# #  }
# #  
#  data_list = lapply(txt_files, scan, what = "character")
# #  tempData = scan("0.txt", what = "character", sep = "", dec =".", skip = 0, na.strings = "NA") 
# 
# # tempData = matrix(data = NA, nrow= length(files), ncol =  100)
# # for(i in 1: length(files))
# # {
# # tempData = scan(files[i], what = "list")
# # }
# library(MASS)
# # tempData.freq = table(tempData)
# # freqtable = cbind(tempData.freq)
# # tempData.freq
# # tempData = scan("0.txt", what = "list")
# # txt <- readPlain("0.txt")
# # ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"), readerControl = list(language = "lat"))
# # tempData = scan(file = "0.txt", what = "character", sep = "", dec =".", skip = 0, na.strings = "NA") 
# # tempData.freq = table(tempData)
# # freqtable = cbind(tempData.freq)
# temp_data <- VectorSource(data_list)
# corpus <- Corpus(temp_data)
# # transformed every word to lower case
# corpus <- tm_map(corpus, content_transformer(tolower))
# # removed all punctuation
# corpus <- tm_map(corpus, removePunctuation)
# # stripped out any extra whitespace
# corpus <- tm_map(corpus, stripWhitespace)
# # corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 
# dtm <- DocumentTermMatrix(corpus)
# 
# dtm2 <- as.matrix(dtm)
# 
# frequency <- colSums(dtm2)
# 
# frequency <- sort(frequency, decreasing=TRUE)
# 
# head(frequency)
# 
# # index = c(" ")
# 
# # review_source <- VectorSource(review_text)
# # Data <- tm_map(tempData, content_transformer(tolower))
# 
# # setwd("/Users/aimhighfly/Documents/StonyBrookUniversity/Fall_semester/Big_Data/spam/ham/")

