


library(shiny)
library(googlesheets)

# Import data

gs_data <- gs_key("1-17_HfRFg6guIf3lDs-Lsa6zGmOhKRU5AkNQvUcWnNc", lookup = FALSE)
data <- gs_read(gs_data)

data$day_week <- weekdays(as.Date(data$date))

data<- data[data$season != 0,]

data$DoW <- factor(data$day_week, levels= c("Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ski Lessons - Mt. Tremblant"),
   
   fluidRow(
     column(width = 2,
            numericInput('s_week', 'Start week:', 1, 1, 10, 1)),
     column(width = 2,
            numericInput('e_week', 'End week:', 1, 1, 10, 1)),
     column(width = 2,
            selectInput("season", "Season:", 
                        choices = data$season[data$season != 0],
                        selected = data$season[data$season == 1],
                        multiple = TRUE))
   ),
   
   fluidRow(
     column(width = 5,
            plotOutput("avg_lessons"),
            plotOutput("revenue"),
            tags$head(tags$style("#avg_lessons{height:25vh !important;}")),
            tags$head(tags$style("#revenue{height:25vh !important;}")))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  season <- reactive({
    c <- data[data$season == input$season,]
    t <-aggregate(c$lessons, by=list(c$DoW), FUN=mean)
    names(t) <- c('Week_Day', 'avg_lessons')
    t
  })
  
  output$avg_lessons <- renderPlot({
      barplot(season()$avg_lessons, main="Average Lessons/Day", 
              names.arg = season()$Week_Day, col = "blue")
   })
  
  output$revenue <- renderPlot({
    barplot(season()$avg_lessons * 200, main="Revenue by Day", 
            names.arg = season()$Week_Day, col = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

