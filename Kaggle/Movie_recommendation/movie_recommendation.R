setwd("~/Documents/StonyBrookUniversity/Spring_semester/EST508/projects/kaggle/movie_recommendation")

#____________________________________________________________________________________________
## Create users dataframe
users = read.table("users.dat", stringsAsFactors = F)
library(data.table)
users.df <- as.data.frame(tstrsplit(users$V1, '::', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(users.df) <- c("UserID","Gender","Age","Occupation","Zip.code")
users.df$Gender[which(users.df$Gender=="M")] <- 1
users.df$Gender[which(users.df$Gender=="F")] <- 0
users.df$Gender = as.numeric(users.df$Gender)
# View(users.df)
str(users.df)
# save movies.df as a csv
write.csv(users.df,"users.df.csv", row.names=F)

#____________________________________________________________________________________________
## Create movies dataframe
movies = readLines("movies.dat")
# View(movies)
library(data.table)
Sys.setlocale('LC_ALL','C')
movies.df <- as.data.frame(tstrsplit(movies, '::', fixed=TRUE), stringsAsFactors=FALSE)
colnames(movies.df) <- c("MovieID","Title","Genres")
View(movies.df)
# save movies.df as a csv
str(movies.df)
write.csv(movies.df,"movies.df.csv", row.names=F)

#____________________________________________________________________________________________
## Create genres dataframe
library(data.table)
genres <- as.data.frame(tstrsplit(movies.df$Genres, '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres) <- c(1:6)
# View(genres)

genre_list <- c("Action", "Adventure", "Animation", "Children's", "Comedy", "Crime",
                "Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical",
                "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
genre_matrix <- matrix(0,3884,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

# iterate through matrix
for (i in 1:nrow(genres)) {
  for (c in 1:ncol(genres)) {
    genmat_col = which(genre_matrix[1,] == genres[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}
#convert into dataframe
genre.data.frame <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE)#remove first row, which was the genre list
genre.df <- as.data.frame(lapply(genre.data.frame,as.integer))#convert from characters to integers
# View(genre.df)


# save genre.df as a csv
write.csv(genre.df,"genre.df.csv", row.names=F)

#_____________________________________________________________________________________________
## Combine users and ratings
ratings = read.csv("training_ratings_for_kaggle_comp.csv", stringsAsFactors = F)
# View(ratings)
str(ratings)

combine.r.u <- data.frame(ratings, subset(users.df, users.df$UserID == ratings[1,1], select = c(Gender,Age,Occupation)),
                      row.names = NULL)

for(i in 1:nrow(ratings))
{
  combine.r.u[i,5:7] <- subset(users.df, users.df$UserID == ratings[i,1], select = c(Gender,Age,Occupation,Zip.code))
  #combine = rbind.data.frame(combine,cbind.data.frame(ratings[i,] ,info))
}

# View(combine.r.u)
str(combine.r.u)
# save combine.r.u as a csv
write.csv(combine.r.u,"combine.r.u.csv")

## Combine users, genres and ratings
combine.r.u = read.csv("combine.r.u.csv", stringsAsFactors=F)
combine.r.u.g <- data.frame(combine.r.u, genre.df[1,])
for(i in 1:nrow(combine.r.u))
{
  combine.r.u.g[i,9:26] <- as.matrix(genre.df[combine.r.u.g[i,3],])
  print(i)
}
# View(combine.r.u.g)
str(combine.r.u.g)

# save combine.r.u.g as a csv
write.csv(combine.r.u.g[,-1],"combine.r.u.g.csv", row.names=F)

# delete NAs which are movies not in the movies.df
combine.r.u.g =read.csv("combine.r.u.g.csv", stringsAsFactors=F)
combine.na = na.omit(combine.r.u.g)
str(combine.na)

# standardize combine
age = combine.na$Age
age[which(age==1)] = 0
age[which(age==18)] = 1/6
age[which(age==25)] = 2/6
age[which(age==35)] = 3/6
age[which(age==45)] = 4/6
age[which(age==50)] = 5/6
age[which(age==56)] = 1
#View(age)
occupation = combine.na$Occupation/20

combine.stand = data.frame(combine.na[,1:5], "Age" = age, 
                           "Occupation" = occupation, combine.na[,8:25])
str(combine.stand)
# save combine.stand as a csv
write.csv(combine.stand,"combine.stand.csv", row.names=F)

# combine.ds = dist(combine.stand, method = "euclidean")
# # save combine.ds as a csv
# write.csv(combine.ds,"combine.ds.csv", row.names=F)


#____________________________________________________________________________________________
## training,testing

combine.r.u.g = read.csv("combine.r.u.g.csv", stringsAsFactors=F)
View(combine.r.u.g)
str(combine.r.u.g)
combine.omit <- na.omit(combine.r.u.g)
str(combine.omit)
combine.model <- combine.omit[,-c(1,2,4)]
combine.model[which(combine.model[,1]%in% c(1,2)),1]<- -1
combine.model[which(combine.model[,1]==3),1] <- 0
combine.model[which(combine.model[,1]%in% c(4,5)),1] <- 1

combine.model$rating <- as.factor(combine.model$rating)
str(combine.model)



require(caret)
require(kernlab)
# Split the data into a training/test set by 60% training/40% test.
inTrain <- createDataPartition(y = combine.model$rating, p=0.7, list=FALSE)
training <- combine.model[inTrain,]
testing <- combine.model[-inTrain,]

library(randomForest)

rfFit <- randomForest(rating ~. , data=training)
rfPredict <- predict(rfFit, testing)
confusionMatrix(rfPredict, testing$rating)


#____________________________________________________________________________________________
## Submission dataframe
submission = read.csv("sample_submission.csv", stringsAsFactors = F)
# View(submission)
str(submission)
library(data.table)
sub.movie <- as.data.frame(tstrsplit(submission$id, '_', type.convert=TRUE), stringsAsFactors=FALSE)
submission.df = data.frame("user" = submission$user, "rating" = submission$rating,
                           "movie" = sub.movie[,2])
# View(submission.df)
str(submission.df)
# save submission.df as a csv
write.csv(submission.df,"submission.df.csv", row.names=F)

#_____________________________________________________________________________________________

combine.stand = read.csv("combine.stand.csv", stringsAsFactors=F)
str(combine.stand)
combine.matrix = as.matrix(combine.stand[,-4:-4])
str(combine.matrix)

users.df =read.csv("users.df.csv", stringsAsFactors=F)
str(users.df)
users.matrix = as.matrix(users.df[,-5])

# standardize users.df 
users.matrix[which(users.matrix[,3]==1),3] = 0
users.matrix[which(users.matrix[,3]==18),3] = 1/6
users.matrix[which(users.matrix[,3]==25),3] = 2/6
users.matrix[which(users.matrix[,3]==35),3] = 3/6
users.matrix[which(users.matrix[,3]==45),3] = 4/6
users.matrix[which(users.matrix[,3]==50),3] = 5/6
users.matrix[which(users.matrix[,3]==56),3] = 1
users.matrix[,4] = users.matrix[,4]/20
# View(users.matrix)
str(users.matrix)


movies.df =read.csv("movies.df.csv", stringsAsFactors=F)
str(movies.df)

genre.df =read.csv("genre.df.csv", stringsAsFactors=F)
str(genre.df)
genre.matrix = as.matrix(genre.df)
str(genre.matrix)



submission.final.df1 =read.csv("submission.final.df1.csv", stringsAsFactors=F)
submission.matrix1 = as.matrix(submission.final.df1)


submission.final.df2 =read.csv("submission.final.df2.csv", stringsAsFactors=F)
submission.matrix2 = as.matrix(submission.final.df2)

submission.matrix = rbind(submission.matrix1[1:220873,],submission.matrix2[220874:nrow(submission.matrix2),])
# View(submission.matrix)
# s= 220873

user.first = as.matrix(rowSums((sweep(users.matrix[,-1], 2, users.matrix[1,-1]))^2))
user.dist.combine = cbind(1:nrow(users.matrix),user.first)
user.dist = user.dist.combine[order(user.first),][1:50,1]
for (u in 2:nrow(users.matrix)){
  user.temp = as.matrix(rowSums((sweep(users.matrix[,-1], 2, users.matrix[u,-1]))^2))
  user.temp.combine = cbind(1:nrow(users.matrix),user.temp)
  single.user.dist = user.temp.combine[order(user.temp),][1:50,1]
  user.dist = cbind(user.dist, single.user.dist)
  print(u)
}
user.dist[,1]
str(user.dist)

# save user.dist as a csv
write.csv(user.dist,"user.dist.csv", row.names=F)

movie.first = as.matrix(rowSums((sweep(genre.matrix, 2, genre.matrix[1,]))^2))
movie.dist.combine = cbind(1:nrow(genre.matrix), movie.first)
movie.dist <- movie.dist.combine[order(movie.first),][1:50,1]
for (m in 2:length(movieID)){
  movie.temp = as.matrix(rowSums((sweep(genre.matrix, 2, genre.matrix))^2))
  movie.temp.combine = cbind(1:nrow(genre.matrix), movie.temp)
  single.movie.dist = movie.temp.combine[order(movie.temp),][1:50,1]
  movie.dist = cbind(movie.dist, single.movie.dist)
  print(m)
}
movie.dist[,1]
str(movie.dist)
# save movie.dist as a csv
write.csv(movie.dist,"movie.dist.csv", row.names=F)

#  single.combine = c(users.matrix[submission.matrix[s,1],2:5], genre.matrix[movieID[submission.matrix[s,3]],])
#   single.user = users.matrix[submission.matrix[s,1],2:5]
#   single.movie = genre.matrix[grep(submission.matrix[s,3], movieID),]
#   user.dist = rowSums((sweep(users.matrix[,-1], 2, single.user))^2)
#   movie.dist = rowSums((sweep(genre.matrix, 2, single.movie))^2)
#   dist.matrix = cbind(c(1:nrow(combine.matrix)),dist)
#   order = dist.matrix[order(dist.matrix[,2]),]
#   submission.matrix[s,2] = ceiling(mean(combine.matrix[order[1:100,1],3]))


# user, movie matrix
user_movie = matrix(0, nrow(users.df), nrow(movies.df))
  for (r in 1:nrow(ratings.df)){
      user_movie[ratings.df[r,1],which(movieID == ratings.df[r,2])] = as.numeric(ratings.df[r,3])
      print(r)
  }
str(user_movie)
# save user_movie as a csv
write.csv(user_movie,"user_movie.csv", row.names=F)

submission.matrix[,2] <- 0
for(s in 1:nrow(submission.matrix))
{
  count = 0
  sum = 0
  for (i in user.dist[,submission.matrix[s,1]]){
    for (j in movie.dist[,which(movieID==submission.matrix[s,3])]){
      if(user_movie[i,j]>0){
        count = count + 1
        sum =  sum + user_movie[i,j]
      }
    }
  }
  if(count){
  submission.matrix[s,2] =  submission.matrix[s,2] + sum/count
  }
  else{print("NA")}
  print(s)
}
submission.matrix.na = submission.matrix
submission.matrix.na[which(is.na(submission.matrix[,2])),2] = 0
str(submission.matrix.na)
# View(submission.matrix)
submission.final.df = as.data.frame(submission.matrix.na)
# View(submission.final.df)
# save submission.final.df as a csv
write.csv(submission.final.df,"submission.final.df.csv", row.names=F)


# final combination
submission = read.csv("sample_submission.csv", stringsAsFactors = F)
final.combination = cbind.data.frame("user"=submission.final.df$user,"rating"=submission.final.df$rating, "id" = submission$id)

write.csv(final.combination,"final.combination.csv", row.names=F)


# Mar28 Second Edition
# April 8, Third Edition

final.combination = read.csv("final.combination.csv", stringsAsFactors = F)
str(final.combination)

ratings = read.csv("training_ratings_for_kaggle_comp.csv", stringsAsFactors = F)
str(ratings)
ratings.df = ratings[,-4]
str(ratings.df)
ratings.matrix = as.matrix(ratings.df[,-4])
ratings.matrix[1,]

users.df =read.csv("users.df.csv", stringsAsFactors=F)
str(users.df)
library(stringr)
users.df$Zip.code <- as.numeric(lapply(users.df$Zip.code, function(x) str_sub(x,1,1)))
users.matrix = as.matrix(users.df)
# standardize users.df 
users.matrix[which(users.matrix[,3]==1),3] = 0
users.matrix[which(users.matrix[,3]==18),3] = 1/6
users.matrix[which(users.matrix[,3]==25),3] = 2/6
users.matrix[which(users.matrix[,3]==35),3] = 3/6
users.matrix[which(users.matrix[,3]==45),3] = 4/6
users.matrix[which(users.matrix[,3]==50),3] = 5/6
users.matrix[which(users.matrix[,3]==56),3] = 1
users.matrix[,4] = users.matrix[,4]/20
users.matrix[,5] = users.matrix[,5]/10
# View(users.matrix)
str(users.matrix)
users.matrix[1:10,]


genre.df = read.csv("genre.df.csv", stringsAsFactors = F)
str(genre.df)
genre.matrix = as.matrix(genre.df)
genre.matrix[1,]

movies.df = read.csv("movies.df.csv", stringsAsFactors = F)
str(movies.df)

movieID = movies.df$MovieID



submission.df = read.csv("submission.df.csv", stringsAsFactors=F)
str(submission.df)
submission.matrix = as.matrix(submission.df)
submission.matrix[1,]


# combine.stand = read.csv("combine.stand.csv", stringsAsFactors=F)
# str(combine.stand)
# combine.matrix = as.matrix(combine.stand[,-4:-4])
# combine.matrix[1,]

# test
user2785 = subset(users.df, UserID == 2785)

# postive
ratings2785.p = subset(ratings.df, user == 3000&(rating==5|rating==4))
ratings2785.p
movie.index.p = which(movieID==ratings2785.p$movie[1])
for (i in 2:length(ratings2785.p$movie)){
  movie.index.p = cbind(movie.index.p, which(movieID==ratings2785.p$movie[i]))
}
str(movie.index.p) 
pref.p = colSums(genre.df[movie.index.p,])/length(movie.index.p)

# negative 
ratings2785.n = subset(ratings.df, user == 3000&(rating==2|rating==1))
ratings2785.n
movie.index.n = which(movieID==ratings2785.n$movie[1])
for (i in 2:length(ratings2785.n$movie)){
  movie.index.n = cbind(movie.index.n, which(movieID==ratings2785.n$movie[i]))
}
str(movie.index.n) 
pref.n = colSums(genre.df[movie.index.n,])/length(movie.index.n)

# middle 
ratings2785.m = subset(ratings.df, user == 3000&(rating==3))
ratings2785.m
movie.index.m = which(movieID==ratings2785.m$movie[1])
for (i in 2:length(ratings2785.m$movie)){
  movie.index.m = cbind(movie.index.m, which(movieID==ratings2785.m$movie[i]))
}
str(movie.index.m) 
pref.m = colSums(genre.df[movie.index.m,])/length(movie.index.m)


pref.p
pref.m
pref.n
pref.p - pref.n
final.pref = (pref.p - pref.n)*((pref.p>=pref.m) == (pref.m>=pref.n))


combine2785 = cbind.data.frame("ratings"=ratings2785$rating ,user.index[,-1], genre.df[movie.index,])
str(combine2785)

library(caret)
inTrain <- createDataPartition(y = combine2785$ratings, p=0.7, list=FALSE)
training <- combine2785[inTrain,]
testing <- combine2785[-inTrain,]

# Train the model.
fit <- train(ratings ~ ., data=training, method='glm')
# Show statistical significance of coefficients (terms).
summary(fit)
# Show accuracy on training set.
fit
plot(fit)
# Run model on test set.
results <- predict(fit, newdata = testing)
comparison = cbind(testing$ratings, results)
comparison[order(results),]


# new idea
# user.pref

user.pref = genre.df[1,]
for(u in 1:nrow(users.df)){
  if(nrow(subset(ratings.df, user == u))>0){
  # postive
      if(nrow(subset(ratings.df, user == u&(rating==5|rating==4)))>0){
          ratings.p = subset(ratings.df, user == u&(rating==5|rating==4))
          movie.index.p = which(movieID==ratings.p$movie[1])
          for (i in 2:length(ratings.p$movie)){
              movie.index.p = cbind(movie.index.p, which(movieID==ratings.p$movie[i]))
          }
          pref.p = colSums(genre.df[movie.index.p,])/length(movie.index.p)
      }
      else{pref.p = genre.df[1,]-genre.df[1,] }
  # negative 
    if(nrow(subset(ratings.df, user == u&(rating==2|rating==1)))>0){
        ratings.n = subset(ratings.df, user == u&(rating==2|rating==1))
        movie.index.n = which(movieID==ratings.n$movie[1])
        for (i in 2:length(ratings.n$movie)){
            movie.index.n = cbind(movie.index.n, which(movieID==ratings.n$movie[i]))
        }
        pref.n = colSums(genre.df[movie.index.n,])/length(movie.index.n)
    }
    else{pref.n = genre.df[1,]-genre.df[1,]}
  # middle 
    if(nrow(subset(ratings.df, user == u&(rating==3)))>0){
        ratings.m = subset(ratings.df, user == u&(rating==3))
        movie.index.m = which(movieID==ratings.m$movie[1])
        for (i in 2:length(ratings.m$movie)){
            movie.index.m = cbind(movie.index.m, which(movieID==ratings.m$movie[i]))
        }
        pref.m = colSums(genre.df[movie.index.m,])/length(movie.index.m)
    }
    else{pref.m = genre.df[1,]-genre.df[1,]}
  single.pref = (pref.p - pref.n)*((pref.p>=pref.m) == (pref.m>=pref.n))
  user.pref = rbind.data.frame(user.pref, single.pref)
  }
  else{user.pref = rbind(user.pref, 0)}
  print(u)
}
user.pref = user.pref[-1,]
str(user.pref)
user.pref[4005,]
sum(is.na(user.pref))

usertest0 = matrix(0,nrow(users.df),1)

for(i in 1:nrow(users.df))
{
  if(sum(user.pref[i,]==0)!=18){
  usertest0[i,1] = 1
  }
}

for(i in 1:nrow(users.df))
{
  if(usertest0[i,1]==0){
    count = 0
    sum = 0
    for (j in user.dist[,i]){
      if(usertest0[j,1]==1){
        count = count + 1
        sum =  sum + user.pref[j,]
      }
    }
    user.pref[i,] = sum/count
  }
  print(i)
}



submission.matrix[,2] <- 0
for(s in 1:nrow(submission.matrix))
{
  if(usertest0[submission.matrix[s,1],1]==0){
      count = 0
      sum = 0
      genre = genre.df[which(movieID==submission.matrix[s,3]),]
      for (i in user.dist[,submission.matrix[s,1]]){
        if(usertest0[i,1]==1){
        count = count + 1
        sum =  sum + sum(genre * user.pref[i,])
        }
        
      }
      submission.matrix[s,2] = sum/count
  }
  else{
      submission.matrix[s,2] = sum(genre.df[which(movieID==submission.matrix[s,3]),] * user.pref[submission.matrix[s,1],])
    }
  
  print(s)
}

# time saving try
submission.matrix[,2] <- 0
for(s in 1:nrow(submission.matrix))
{
    submission.matrix[s,2] = sum(genre.df[which(movieID==submission.matrix[s,3]),] * user.pref[submission.matrix[s,1],])
  print(s)
}

submission.final.df = as.data.frame(submission.matrix)
submission = read.csv("sample_submission.csv", stringsAsFactors = F)
final.combination = cbind.data.frame("user"=submission.final.df$user,"rating"=submission.final.df$rating, "id" = submission$id)

write.csv(final.combination,"final.combination.csv", row.names=F)



# Collaborative Filtering
user_movie =read.csv("user_movie.csv", stringsAsFactors=F)
str(user_movie)

movie1 = subset(ratings.df, movie==1)
str(movie1)
movie2 = subset(ratings.df, movie==2)
str(movie2)

pair1.2 = data.frame()
for(i in 1:nrow(users.df)){
  if(nrow(subset(movie1, user==i))>0&nrow(subset(movie2, user==i))){
    pair = cbind(subset(movie1, user==i)[,3], subset(movie2, user==i)[,3])
    pair1.2 = rbind(pair1.2, pair)
    print(i)
  }
  else{print(rep(i,10))}
}

plot(pair1.2) 









