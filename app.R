


library(shiny)
library(googlesheets)
library (shinydashboard)

# Import data
gs_data <- gs_key("1-17_HfRFg6guIf3lDs-Lsa6zGmOhKRU5AkNQvUcWnNc", lookup = FALSE)
data <- gs_read(gs_data)

# Modify Data
data$day_week <- weekdays(as.Date(data$date))
data<- data[data$season != 0,]
data$DoW <- factor(data$day_week, levels= c("Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(
    # Application title
    title = "Ski Lessons - Mt. Tremblant"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",icon=icon("area-chart"))
    ),  
    #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    selectInput("season", "Season:", 
                choices = data$season[data$season != 0],
                selected = data$season[data$season == 0]),
    numericInput('s_week', 'Start week:', 1, 1, 10, 1),
    numericInput('e_week', 'End week:', 1, 1, 10, 1)
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("ytdLessons"),
      valueBoxOutput("ytdProfit")
    ),
    fluidRow(
      column(width = 5,
             box(
               title = "Average Lessons Per Day", solidHeader = TRUE, width = NULL, status = "primary",
               plotOutput("avg_lessons"),
               tags$head(tags$style("#avg_lessons{height:30vh !important;}"))
               ),
             box(
               title = "Revenue per Day", solidHeader = TRUE, width = NULL, status = "primary",
               plotOutput("revenue"),
               tags$head(tags$style("#revenue{height:30vh !important;}"))
             )
      )
    )
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
  
  lessons <- reactive({
    c <- data[data$season == input$season,]
    ytdLessons <- sum(c$lessons)
    ytdLessons
  })
  
  output$ytdLessons <- renderValueBox({
    
    valueBox(
      value = toString(lessons()),
      subtitle = "YTD Lessons",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })  
  
  output$ytdProfit <- renderValueBox({
    
    valueBox(
      value = paste("$", formatC(lessons()*200, format="d", big.mark=",")),
      subtitle = "YTD Profit",
      icon = icon("usd"),
      color = "green"
    )
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

