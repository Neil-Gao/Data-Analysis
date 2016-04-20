library(shinydashboard)
library(DT)
library(googleVis)


ui <- dashboardPage(
  dashboardHeader(title = tagList(icon("twitter"), "Xiang Gao")),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Exploratory", tabName = "exploratory", icon = icon("database")),
      menuItem("Time series", tabName = "timeseries", icon = icon("area-chart")),
      menuItem("Sentiment", tabName = "sentiment", icon = icon("smile-o")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      br(),
      selectizeInput("selected", "Select Candidate(s)",
                     choices = list('Donald Trump'='trump', 'Ted Cruz'='cruz',
                     'Hillary Clinton'='hilary', 'Bernie Sanders'='bernie'),
                     selected = "trump", multiple=TRUE)
    )
    ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12,
                  title = "2016 Presidential Candidates' Twitter Engagement Analysis", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  h4("Using Twitter's API, I've scrapped the last 3,200 tweets from each of the 
                    top 4 presidential candidates, and explored differences in engagement and sentiment. ")
                )
              )
      ),
      
    tabItem(tabName = "exploratory",
            DT::dataTableOutput("table")
      ),
    tabItem(tabName = "timeseries",
            fluidRow(
              box(width = 12,
                  title = "Engagement Analysis", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  p("Whats being plotted: the most retweeted tweet per day.Click on a date to view each candidates tweet on that day.")
              )
            ),
            fluidRow(
            htmlOutput("timeseries_plot")
            )
    ),
    tabItem(tabName = "sentiment",
            h2("How does Sentiment Score affect Retweets?"),
            fluidRow(box(width = 12, plotOutput('box1', width=1000))),
            br(),
            fluidRow(box(width = 12, plotOutput('box2', width=1000)))
    ),
    tabItem(tabName = "wordcloud",
            fluidRow(box(width = 12, plotOutput('wordcloud')))
    )
  )
)
)