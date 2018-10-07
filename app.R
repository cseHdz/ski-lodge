


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
ui <- 
  
  dashboardPage(
  dashboardHeader(
    # Application title
    title = "Skiing Lessons"
  ),
  dashboardSidebar(
    selectInput("season", "Season:", 
                choices = data$season[data$season != 0],
                selected = data$season[data$season == 0]),
    selectInput("view", "Dashboard View:", 
                choices = c('Day of the Week', 'by Week', 'Total'),
                selected = 'Total'),
    sidebarMenu(
      menuItem("Profit Analysis", tabName = "profit",icon=icon("usd")),
      menuItem("Capacity Management", tabName = "capacity",icon=icon("users")),
      menuItem("Scenario Builder", tabName = "scenario",icon=icon("area-chart"))
    ),  
    column(width = 4,
      style='padding:10px',
      align = 'center',
      tableOutput("metrics1"),
      tableOutput("metrics2")
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("kpi1"),
      valueBoxOutput("kpi2"),
      valueBoxOutput("kpi3")
    ),
    fluidRow(
      column(width = 6,
             box(
               title = "Average Lessons Per Day", solidHeader = TRUE, width = NULL, status = "primary",
               plotOutput("chart1"),
               tags$head(tags$style("#chart1{height:25vh !important;}"))
             )),
      column(width = 6,
             box(
               title = "Revenue per Day", solidHeader = TRUE, width = NULL, status = "primary",
               plotOutput("chart2"),
               tags$head(tags$style("#chart2{height:25vh !important;}"))
             ))),
      fluidRow(
        column(width = 6,
               box(
                 title = "Average Lessons Per Day", solidHeader = TRUE, width = NULL, status = "primary",
                 plotOutput("chart3"),
                 tags$head(tags$style("#chart3{height:25vh !important;}"))
               )),
        column(width = 6,
               box(
                 title = "Revenue per Day", solidHeader = TRUE, width = NULL, status = "primary",
                 plotOutput("chart4"),
                 tags$head(tags$style("#chart4{height:25vh !important;}"))
               )))
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
  
  output$kpi1 <- renderValueBox({
    
    valueBox(
      value = toString(lessons()),
      subtitle = "YTD Lessons",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })  
  
  output$kpi2 <- renderValueBox({
    
    valueBox(
      value = paste("$", formatC(lessons()*200, format="d", big.mark=",")),
      subtitle = "YTD Profit",
      icon = icon("usd"),
      color = "green"
    )
  })  
  
  output$kpi3 <- renderValueBox({
    
    valueBox(
      value = paste("$", formatC(lessons()*200, format="d", big.mark=",")),
      subtitle = "YTD Profit",
      icon = icon("usd"),
      color = "red"
    )
  })
  
  output$chart1 <- renderPlot({
      barplot(season()$avg_lessons, main="Average Lessons/Day", 
              names.arg = season()$Week_Day, col = "blue")
   })
  
  output$chart2 <- renderPlot({
    barplot(season()$avg_lessons * 200, main="Revenue by Day", 
            names.arg = season()$Week_Day, col = "green")
  })
  
  output$chart3 <- renderPlot({
    barplot(season()$avg_lessons, main="Average Lessons/Day", 
            names.arg = season()$Week_Day, col = "blue")
  })
  
  output$chart4 <- renderPlot({
    barplot(season()$avg_lessons * 200, main="Revenue by Day", 
            names.arg = season()$Week_Day, col = "green")
  })
  
  output$metrics1 <- renderTable({
    t[0:3,]
  })
  
  output$metrics2 <- renderTable({
    t[0:3,]
  })
  
  output$metrics3 <- renderTable({
    t[0:3,]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

