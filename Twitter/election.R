setwd("/Users/aimhighfly/Documents/StonyBrookUniversity/Spring_semester/EST508/Projects/twitter")

library("twitteR")
library("tm")
library("wordcloud")

library("cluster")
library("FactoMineR")
library("RColorBrewer")
library("ggplot2")

setup_twitter_oauth("OsgS5oe7vJWrbR68Tvjn9SIiI", 
                    "tzliYNHEKuUsy3wMaRFQPL8uiFKmRP4dKlsGpe6sdYWIzbDKck", 
                    "2833789056-RIryMGYAVdH5at7UD5k7YfDQEqgaEOCFlQrTQhF", 	
                    "BCV8ZH590WI44orTFvQ4ukCb1zZazdNjFXkKrPz3fBtlw")

#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________
# Donald Trump
Donald = userTimeline("realDonaldTrump", n=3200)

Donald.info = getUser("realDonaldTrump")
str(Donald.info)
Donald.followers = Donald.info$getFollowerIDs()

follower_location <- data.frame()
# 75000/500 = 150
# Because of the rate limit of Twitter, try to get 500*15 data everytime.
for (i in 146:150){
    follower_temp <- lookupUsers(Donald.followers[500*(i-1)+1:500*i])
    for (j in 1:length(follower_temp)){
        follower_location_temp <- data.frame(follower_temp[[j]]$getLocation(),stringsAsFactors = F)
        follower_location <- rbind.data.frame(follower_location, follower_location_temp)
    }
}
str(follower_location)

# Save followers' ID
save(Donald.followers, file = "TrumpFollowersID.RData")
load("TrumpFollowersID.RData")
# Save follower_location
save(follower_location, file = "TrumpFollowers.RData")
load("TrumpFollowers.RData")
library("maps")
library("mapdata")

addr_state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

state_name =  c("Alabama",        "Alaska",         "Arizona",        "Arkansas",       "California",     "Colorado",      
                "Connecticut",    "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",
                "Illinois",       "Indiana",        "Iowa",           "Kansas",         "Kentucky",       "Louisiana",     
                "Maine",          "Maryland",       "Massachusetts",  "Michigan",       "Minnesota",      "Mississippi",   
                "Missouri",       "Montana",        "Nebraska",       "Nevada",         "New Hampshire",  "New Jersey",    
                "New Mexico",     "New York",       "North Carolina", "North Dakota",   "Ohio",           "Oklahoma",      
                "Oregon",         "Pennsylvania",   "Rhode Island",   "South Carolina", "South Dakota",   "Tennessee",     
                "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West Virginia", 
                "Wisconsin",      "Wyoming")

state_name1 =  c("Alabama",        "Alaska",         "Arizona",        "Arkansas",       "California",     "Colorado",      
                "Connecticut",    "Delaware",       "Florida",        "Georgia",        "Hawaii",         "Idaho",
                "Illinois",       "Indiana",        "Iowa",           "Kansas",         "Kentucky",       "Louisiana",     
                "Maine",          "Maryland",       "Massachusetts",  "Michigan",       "Minnesota",      "Mississippi",   
                "Missouri",       "Montana",        "Nebraska",       "Nevada",         "Hampshire",      "Jersey",    
                "Mexico",         "York",           "North",           "Dakota",        "Ohio",           "Oklahoma",      
                "Oregon",         "Pennsylvania",   "Rhode",          "South",          "Dakota",   "Tennessee",     
                "Texas",          "Utah",           "Vermont",        "Virginia",       "Washington",     "West", 
                "Wisconsin",      "Wyoming")

# map("usa")
# map('state', region= state_name)

# library(data.table)
# follower_location_split <- lapply(follower_location, function(x) strsplit(x,' '))

follower_location_split[[1]]
follower_corpus <- Corpus(VectorSource(follower_location))

follower_corpus <- tm_map(follower_corpus, removePunctuation)
follower_corpus <- tm_map(follower_corpus, content_transformer(tolower))
follower_corpus <- tm_map(follower_corpus, removeWords, stopwords("english"))
follower_corpus <- tm_map(follower_corpus, stripWhitespace)
inspect(follower_corpus[1])

tdm <- TermDocumentMatrix(follower_corpus)
# inspect(tdm)
state.map.lowercase <- tolower(state_name1)
location_term <- tolower(tdm$dimnames$Terms)
follower.df <- data.frame(location_term,tdm$v)

follower.df.map <- follower.df[location_term %in% state.map.lowercase,]

nrow(follower.df[location_term %in% state.map.lowercase,])
sum(follower.df[location_term %in% state.map.lowercase,2])

follower.final <- data.frame("states" = state_name, "Followers" = 0)
for (i in 1:length(state.map.lowercase)){
  follower.final[i,2] <- follower.df.map[grep(state.map.lowercase[i], follower.df.map[,1])[length(grep(state.map.lowercase[i], follower.df.map[,1]))],2]
}
  
  
l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))

Donald.plot <- plot_ly(z = follower.final[,2], text = follower.final[,1], 
        locations = addr_state, type = 'choropleth',
        locationmode = 'USA-states', color = follower.final[,2], colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "Number of followers")) %>%
        layout(title = 'Number of Twitter followers of Trump<br>(Hover for Details)', geo = g)



# search DonaldTrump's tweets
Donald = userTimeline("realDonaldTrump", n=3200)
str(Donald[1])
# convert list to vector
# get text
Donald_text <- sapply(Donald, function(x) x$getText())
# get time
# Donald_time <- sapply(Donald, function(x) x$getCreated())
# str(Donald_time)
Donald_time = data.frame()
for (i in 1:length(Donald)){
time <- as.data.frame(Donald[[i]]$getCreated())
Donald_time = rbind.data.frame(Donald_time, time) 
}
str(Donald_time)
# get retweetted count
Donald_retweetted <- sapply(Donald, function(x) x$getRetweetCount())
str(Donald_retweetted)
# get favorites count
Donald_favorite <- sapply(Donald, function(x) x$getFavoriteCount())
str(Donald_favorite)

# For mac system
# Convert Character Vector between Encodings
Donald_text <- iconv(Donald_text,to="utf-8-mac")
str(Donald_text)
Donald_text[1]
# create corpus from vector
Donald_corpus <- Corpus(VectorSource(Donald_text))
Donald_corpus
inspect(Donald_corpus[1])

# remove punctuation, lower case, remove numbers, cut out stopwords,  strip whitespace
Donald_clean <- tm_map(Donald_corpus, removePunctuation)
Donald_clean <- tm_map(Donald_clean, content_transformer(tolower))
Donald_clean <- tm_map(Donald_clean, removeWords, stopwords("english"))
Donald_clean <- tm_map(Donald_clean, stripWhitespace)
# Donald_clean <- tm_map(Donald_clean, removeWords, c())
inspect(Donald_clean[1])

wordcloud(Donald_clean, random.order = F ,max.words = 50, scale = c(4, 0.5), col = rainbow(50))
#___________________________________________________________________________________________________________________
# Sentiment analysis
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


pos.words = scan("positive-words.txt", what='character', comment.char=';') 
neg.words = scan("negative-words.txt", what='character', comment.char=';') 

Donald_sentiment = score.sentiment(Donald_text, pos.words, neg.words)
table(Donald_sentiment$score)
hist(Donald_sentiment$score)
boxplot(Donald_retweetted ~ Donald_sentiment$score, xlab = "Sentiment score", ylab = "Retweeted count of Trump")

Donald_sentiment_score <- as.factor(Donald_sentiment$score)

qplot(Donald_sentiment_score, Donald_retweetted, geom=c("boxplot"), color = Donald_sentiment_score,
      main="Retweeted count VS Sentiment score of Trump", 
      xlab = "Sentiment score", ylab = "Retweeted count")

qplot(Donald_sentiment$score, Donald_retweetted, geom=c("point", "smooth"), 
      main="Retweeted count VS Sentiment score of Trump", 
      xlab = "Sentiment score", ylab = "Retweeted count")

tdm = TermDocumentMatrix(Donald_clean)
m = as.matrix(tdm)
# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1

# distance matrix with binary distance
m1dist = dist(m1, method="binary")

# cluster with ward method
clus1 = hclust(m1dist, method="ward.D")

# plot dendrogram
plot(clus1, cex=0.7)

# correspondance analysis
Donald_ca = CA(m1, graph=FALSE)

# default plot of words
plot(Donald_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(Donald_ca$row$coord[,1], Donald_ca$row$coord[,2], labels=rownames(m1),
     col=hsv(0,0,0.6,0.5))
title(main="@realDonaldTrump Correspondence Analysis of tweet words", cex.main=1)

# partitioning around medoids iwth 6 clusters
k = 6
# To improve the correspondance analysis plot, we can apply a clustering method
# like k-means or partitioning around medoids (pam)
# pam clustering
Donald_pam = pam(Donald_ca$row$coord[,1:2], k)

# get clusters
clusters = Donald_pam$clustering

# a nicer plot
# first we need to define a color palette
gbrew = brewer.pal(8, "Dark2")

# Use hsv encoding
gpal = rgb2hsv(col2rgb(gbrew))

# colors in hsv (hue, saturation, value, transparency)
gcols = rep("", k)
for (i in 1:k) {
  gcols[i] = hsv(gpal[1,i], gpal[2,i], gpal[3,i], alpha=0.65)
}

# plot with frequencies
wcex = log10(rowSums(m1))
plot(Donald_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
title("@realDonaldTrump Correspondence Analysis of tweet words", cex.main=1)
for (i in 1:k)
{
  tmp <- clusters == i
  text(Donald_ca$row$coord[tmp,1], Donald_ca$row$coord[tmp,2],
       labels=rownames(m1)[tmp], cex=wcex[tmp],
       col=gcols[i])
}


#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________
# Hillary Clinton

# search Hillary Clinton's tweets
Clinton = userTimeline("HillaryClinton", n=3200)
str(Clinton[1])

Clinton.info = getUser("HillaryClinton")
Clinton.followers = Clinton.info$getFollowerIDs()
  
follower1_location <- data.frame()
# 75000/500 = 150
# Because of the rate limit of Twitter, try to get 500*15 data everytime.
for (i in 136:150){
  follower1_temp <- lookupUsers(Clinton.followers[500*(i-1)+1:500*i])
  for (j in 1:length(follower1_temp)){
    follower1_location_temp <- data.frame(follower1_temp[[j]]$getLocation(),stringsAsFactors = F)
    follower1_location <- rbind.data.frame(follower1_location, follower1_location_temp)
  }
}
str(follower1_location)

# Save followers' ID
save(Clinton.followers, file = "ClintonFollowersID.RData")
# Save follower1_location
save(follower1_location, file = "ClintonFollowers.RData")


follower1_corpus <- Corpus(VectorSource(follower1_location))

follower1_corpus <- tm_map(follower1_corpus, removePunctuation)
follower1_corpus <- tm_map(follower1_corpus, content_transformer(tolower))
follower1_corpus <- tm_map(follower1_corpus, removeWords, stopwords("english"))
follower1_corpus <- tm_map(follower1_corpus, stripWhitespace)
inspect(follower1_corpus[1])

tdm <- TermDocumentMatrix(follower1_corpus)
# inspect(tdm)
state.map.lowercase <- tolower(state_name1)
location_term <- tolower(tdm$dimnames$Terms)
follower1.df <- data.frame(location_term,tdm$v)

follower1.df.map <- follower1.df[location_term %in% state.map.lowercase,]

nrow(follower1.df[location_term %in% state.map.lowercase,])
sum(follower1.df[location_term %in% state.map.lowercase,2])

follower1.final <- data.frame("states" = state_name, "Followers" = 0)
for (i in 1:length(state.map.lowercase)){
  follower1.final[i,2] <- follower1.df.map[grep(state.map.lowercase[i], follower.df.map[,1])[length(grep(state.map.lowercase[i], follower1.df.map[,1]))],2]
}


l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))

Clinton.plot <- plot_ly(z = follower1.final[,2], text = follower1.final[,1], 
                       locations = addr_state, type = 'choropleth',
                       locationmode = 'USA-states', color = follower1.final[,2], 
                       marker = list(line = l), colorbar = list(title = "Number of followers")) %>%
  layout(title = 'Number of Twitter followers of Clinton<br>(Hover for Details)', geo = g)

# Comparison of Trump and Clinton
comparison.df = data.frame(follower.final$states,follower.final$Followers - follower1.final$Followers)

l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))

comparison.plot <- plot_ly(z = comparison.df[,2], text = comparison.df[,1], 
                        locations = addr_state, type = 'choropleth',
                        locationmode = 'USA-states', color = comparison.df[,2], 
                        marker = list(line = l), colorbar = list(title = "Number of followers")) %>%
  layout(title = 'Comparison of number of Twitter followers of Trump and Clinton<br>(Positive numbers means supporting Trump<br>negative numbers means supporting Hillary)', geo = g)





# Upload graphs to Plot.ly
Sys.setenv("plotly_username"="aimhighfly")
Sys.setenv("plotly_api_key"="skaekyykf2")

plotly_POST(comparision.plot, filename = "predict_election")
comparison.df[rev(order(comparison.df[,2])),1]



# convert list to vector
# get text
Clinton_text <- sapply(Clinton, function(x) x$getText())
# get time
# Clinton_time <- sapply(Clinton, function(x) x$getCreated())
# str(Clinton_time)
Clinton_time = data.frame()
for (i in 1:length(Clinton)){
  time <- as.data.frame(Clinton[[i]]$getCreated())
  Clinton_time = rbind.data.frame(Clinton_time, time) 
}
str(Clinton_time)
# get retweetted count
Clinton_retweetted <- sapply(Clinton, function(x) x$getRetweetCount())
str(Clinton_retweetted)
# get favorites count
Clinton_favorite <- sapply(Clinton, function(x) x$getFavoriteCount())
str(Clinton_favorite)

# For mac system
# Convert Character Vector between Encodings
Clinton_text <- iconv(Clinton_text,to="utf-8-mac")
str(Clinton_text)
Clinton_text[1]
# create corpus from vector
Clinton_corpus <- Corpus(VectorSource(Clinton_text))
Clinton_corpus
inspect(Clinton_corpus[1])

# remove punctuation, lower case, remove numbers, cut out stopwords,  strip whitespace
Clinton_clean <- tm_map(Clinton_corpus, removePunctuation)
Clinton_clean <- tm_map(Clinton_clean, content_transformer(tolower))
Clinton_clean <- tm_map(Clinton_clean, removeWords, stopwords("english"))
Clinton_clean <- tm_map(Clinton_clean, stripWhitespace)
# Donald_clean <- tm_map(Clinton_clean, removeWords, c())
inspect(Clinton_clean[1])

wordcloud(Clinton_clean, random.order = F ,max.words = 50, scale = c(4, 0.5), col = rainbow(50))

#___________________________________________________________________________________________________________________
# Sentiment analysis

Clinton_sentiment = score.sentiment(Clinton_text, pos.words, neg.words)
table(Clinton_sentiment$score)
hist(Clinton_sentiment$score)
boxplot(Clinton_retweetted ~ Clinton_sentiment$score, xlab = "Sentiment score", ylab = "Retweeted count of Clinton")

Clinton_sentiment_score <- as.factor(Clinton_sentiment$score)

qplot(Clinton_sentiment_score, Clinton_retweetted, geom=c("boxplot"), color = Clinton_sentiment_score,
      main="Retweeted count VS Sentiment score of Clinton", 
      xlab = "Sentiment score", ylab = "Retweeted count")

qplot(Clinton_sentiment$score, Clinton_retweetted, geom=c("point", "smooth"), 
      main="Retweeted count VS Sentiment score of Trump", 
      xlab = "Sentiment score", ylab = "Retweeted count")

tdm = TermDocumentMatrix(Clinton_clean)
m = as.matrix(tdm)
# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1

# distance matrix with binary distance
m1dist = dist(m1, method="binary")

# cluster with ward method
clus1 = hclust(m1dist, method="ward.D")

# plot dendrogram
plot(clus1, cex=0.7)

# correspondance analysis
Clinton_ca = CA(m1, graph=FALSE)

# default plot of words
plot(Clinton_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(Clinton_ca$row$coord[,1], Clinton_ca$row$coord[,2], labels=rownames(m1),
     col=hsv(0,0,0.6,0.5))
title(main="@HillaryClinton Correspondence Analysis of tweet words", cex.main=1)

# partitioning around medoids iwth 6 clusters
k = 6
# To improve the correspondance analysis plot, we can apply a clustering method
# like k-means or partitioning around medoids (pam)
# pam clustering
Clinton_pam = pam(Clinton_ca$row$coord[,1:2], k)

# get clusters
clusters = Clinton_pam$clustering

# a nicer plot
# first we need to define a color palette
gbrew = brewer.pal(8, "Dark2")

# Use hsv encoding
gpal = rgb2hsv(col2rgb(gbrew))

# colors in hsv (hue, saturation, value, transparency)
gcols = rep("", k)
for (i in 1:k) {
  gcols[i] = hsv(gpal[1,i], gpal[2,i], gpal[3,i], alpha=0.65)
}

# plot with frequencies
wcex = log10(rowSums(m1))
plot(Clinton_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
title("@HillaryClinton Correspondence Analysis of tweet words", cex.main=1)
for (i in 1:k)
{
  tmp <- clusters == i
  text(Clinton_ca$row$coord[tmp,1], Clinton_ca$row$coord[tmp,2],
       labels=rownames(m1)[tmp], cex=wcex[tmp],
       col=gcols[i])
}









