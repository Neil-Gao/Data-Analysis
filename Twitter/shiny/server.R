library(shinydashboard)
library(DT)
library(googleVis)
library(ggplot2)
library(tm)
library(wordcloud)

load("comparison.Rdata")
load("comparison.anno.Rdata")
load("trump.Rdata")
load("hillary.Rdata")
load("ted.Rdata")
load("bernie.Rdata")
load("trump_sentiment.Rdata")
load("hillary_sentiment.Rdata")
load("ted_sentiment.Rdata")
load("bernie_sentiment.Rdata")
load("trump_sentiment_score.Rdata")
load("hillary_sentiment_score.Rdata")
load("ted_sentiment_score.Rdata")
load("bernie_sentiment_score.Rdata")
load("trump_clean.Rdata")
load("hillary_clean.Rdata")
load("ted_clean.Rdata")
load("bernie_clean.Rdata")

server <- function(input, output, session) {
  output$table <- DT::renderDataTable(
    DT::datatable({
      data <- comparison[,-1]
      data
    }
    )
  )
  output$timeseries_plot <- renderGvis(comparison.anno)
  output$box1 <- renderPlot(
    qplot(trump_sentiment$score, trump$retweet, geom=c("boxplot"), color = trump_sentiment_score,
          main="Retweeted count VS Sentiment score of Trump", 
          xlab = "Sentiment score", ylab = "Retweeted count")
  )
  output$box2 <- renderPlot(
    qplot(trump_sentiment$score, trump$retweet, geom=c("point", "smooth"), 
          main="Retweeted count VS Sentiment score of Trump", 
          xlab = "Sentiment score", ylab = "Retweeted count")
  )
  output$wordcloud <- renderPlot(
    wordcloud(trump_clean, random.order = F ,max.words = 50, scale = c(4, 0.5), colors=c(1,2,3,4,5,6))
  )
  output$twitter <- renderUI({
    tags$iframe(src="https://twitter.com/realDonaldTrump", height=242, width=242)
  })
}
