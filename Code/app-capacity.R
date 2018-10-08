


library(shiny)
library(googlesheets)
library (shinydashboard)
library(lubridate)

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
                choices = c('This Season', 'by Month', 'by Week','by Weekday'),
                selected = 'This Season'),
    sidebarMenu(
      menuItem("Profit Analysis", tabName = "profit",icon=icon("usd")),
      menuItem("Capacity Management", tabName = "capacity",icon=icon("users")),
      menuItem("Weather Assessment", tabName = "scenario",icon=icon("snowflake-o")),
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
  
  profit <- reactive({
    c <- data[data$season == input$season,]
    
    
    t <-aggregate(c$lessons, by=list(c$DoW), FUN=mean)
    names(t) <- c('Week_Day', 'avg_lessons')
    t
  })
  
  momLessonsPerInstructor <- reactive({
    # Selected Season
    c <- data[data$season == input$season,]
    c$lessonsPerInstructor <- round(c$lessons / (-20*(c$promotion-1) + 36*(c$promotion)),2)
    t <-aggregate(c$lessonsPerInstructor, by=list(c$year, c$month), FUN=mean)
    names(t) <- c('year', 'month', 'avg_lessons_instr')
    
    # Last Year Seasons
    #if (input$season > 1)
    #{
    #  lastYear <- data[data$season == input$season-1,]
    #  lastYear$lessonsPerInstructor <- round(lastYear$lessons / (-20*(lastYear$promotion-1) + 36*(lastYear$promotion)),2)
    #  lastYear_agg <-aggregate(lastYear$lessonsPerInstructor, by=list(lastYear$year, lastYear$month), FUN=mean)
    #  names(lastYear_agg) <- c('year', 'month', 'last_year_avg')
    #  lastYear_agg <- lastYear_agg[c('month','last_year_avg')]
    #  t <- merge(t,lastYear_agg, by="month")
    #}
    t
  })
  
  lessonsPerInsutrctor <- reactive({
    c <- data[data$season == input$season,]
    totalNormalDays <- length(c$lessons[c$promotion == 0])
    totalPromoDays <- length(c$lessons[c$promotion == 1])
    instructorDays <- 20*totalNormalDays + 36*totalPromoDays
    lessons <- sum(c$lessons)
    lessonsPerInsutrctor <- round(lessons / instructorDays,2)
    lessonsPerInsutrctor
  })
  
  daysOverStaffed <- reactive({
    c <- data[data$season == input$season,]
    totalPossibleDays <- length(c$lessons)
    daysOverStaffed <- length(c$lessons[((c$promotion == 0) & (c$lessons<20)) | ((c$promotion == 1) & (c$lessons<36))])
    percentOverstaffed <- round(daysOverStaffed / totalPossibleDays*100,0)
    percentOverstaffed
  })  
  
  daysAtCapacity <- reactive({
    c <- data[data$season == input$season,]
    totalPossibleDays <- length(c$lessons)
    daysAtCapacity <- length(c$lessons[((c$promotion == 0) & (c$lessons==20)) | ((c$promotion == 1) & (c$lessons==36))])
    percentAtCapacity <- round(daysAtCapacity / totalPossibleDays*100,0)
    percentAtCapacity
  }) 
  
  output$kpi1 <- renderValueBox({
    
    valueBox(
      value = toString(lessonsPerInsutrctor()),
      subtitle = "Lessons per Instructor",
      icon = icon("graduation-cap"),
      color = "aqua"
    )
  })  
  
  output$kpi2 <- renderValueBox({
    
    valueBox(
      value = paste(toString(daysOverStaffed()), "%"),
      subtitle = "% Days Overstaffed",
      icon = icon("user-plus"),
      color = "green"
    )
  })  
  
  output$kpi3 <- renderValueBox({
    
    valueBox(
      value = paste(toString(daysAtCapacity()), "%"),
      subtitle = "% Days @ Capacity",
      icon = icon("battery-full"),
      color = "green"
    )
  }) 
  
  output$chart2 <- renderPlot({
      barplot(season()$avg_lessons, main="Average Lessons/Day", 
              names.arg = season()$Week_Day, col = "blue")
   })
  
  output$chart1 <- renderPlot({
    barplot(momLessonsPerInstructor()$avg_lessons_instr, main="Average Monthly Lessons per Instructor", 
            names.arg = month.abb[momLessonsPerInstructor()$month], col = "blue")
  })
  
  output$chart3 <- renderPlot({
    barplot(season()$avg_lessons, main="Average Lessons/Day", 
            names.arg = season()$Week_Day, col = "blue")
  })
  
  output$chart4 <- renderPlot({
    barplot(season()$avg_lessons * 200, main="Revenue by Day", 
            names.arg = season()$Week_Day, col = "green")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

