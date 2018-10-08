


library(shiny)
library(googlesheets)
library (shinydashboard)
library(data.table)
suppressMessages(library(dplyr))

# Import data
gs_data <- gs_key("1-17_HfRFg6guIf3lDs-Lsa6zGmOhKRU5AkNQvUcWnNc", lookup = FALSE)
summary <- gs_read(gs_data, ws = "summary")
sales_data <- gs_read(gs_data, ws = "sales_data")

# Define UI for application 
ui <- (
  dashboardPage(
    dashboardHeader(
      # Application title
      title = "Skiing Lessons"
    ),
    dashboardSidebar(
      # Season Selector
      selectInput("season", "Season:", 
                  choices = summary$season[summary$season != 0],
                  selected = summary$season[summary$season == 0]),
      
      # Dashboard View Selector
      selectInput("view", "Dashboard View:", 
                  choices = c('by Season', 'by Month','by Weekday'),
                  selected = 'by Season'),
      sidebarMenu(
        
        # Tabs
        menuItem("Profit Analysis", tabName = "profit",icon=icon("usd")),
        menuItem("Capacity Management", tabName = "capacity",icon=icon("users")),
        menuItem("Weather Assessment", tabName = "weather",icon=icon("snowflake-o")),
        menuItem("Scenario Builder", tabName = "scenario",icon=icon("area-chart"))
      ),  
      
      # Summary Metrics
      column(width = 5,style='padding:10px',align = 'center',
             tableOutput("metrics1"))
             #tableOutput("metrics2"))
    ),
    dashboardBody(
      tabItems(
        
        # Individual sections corresponding to the tabs in the tab bar
        tabItem("profit",
                #KPI Row
                fluidRow(
                  valueBoxOutput("total_profit"),
                  valueBoxOutput("avg_profit_lesson"),
                  valueBoxOutput("avg_profit_instructor")
                ),
                
                #Charts Row
                fluidRow(
                  column(width = 6,
                         box(title = "Profit Trends", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("profit_chart"),tags$head(tags$style("#profit_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Profit per Lesson", solidHeader = TRUE, width = NULL, status = "primary",
                           plotOutput("profit_l_chart"),tags$head(tags$style("#profit_l_chart{height:25vh !important;}"))))),
                fluidRow(
                  column(width = 6,
                         box(title = "Revenue Analysis", solidHeader = TRUE, width = NULL, status = "primary",
                           plotOutput("revenue_chart"),tags$head(tags$style("#revenue_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Cost Analysis", solidHeader = TRUE, width = NULL, status = "primary",
                           plotOutput("cost_chart"),tags$head(tags$style("#cost_chart{height:25vh !important;}"))))
                  )
        ),
        tabItem("capacity",
                #KPI Row
                fluidRow(
                  valueBoxOutput("lessons_instructor"),
                  valueBoxOutput("days_overstaffed"),
                  valueBoxOutput("avg_lessons_day")
                ),
                
                #Charts Row
                fluidRow(
                  column(width = 6,
                         box(title = "Lesson Trends", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("lessons_chart"),tags$head(tags$style("#lessons_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Lessons per Instructor", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("lessons_i_chart"),tags$head(tags$style("#lessons_i_chart{height:25vh !important;}"))))),
                fluidRow(
                  column(width = 6,
                         box(title = "Overstaffed Ratio", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("overstaffed_chart"),tags$head(tags$style("#overstaffed_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Lessons Distribution", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("lessons_d_chart"),tags$head(tags$style("#lessons_d_chart{height:25vh !important;}"))))
                )
        ),
        tabItem("weather",
                #KPI Row
                fluidRow(
                  valueBoxOutput("days_open"),
                  valueBoxOutput("avg_temp"),
                  valueBoxOutput("avg_snow")
                ),
                
                #Charts Row
                fluidRow(
                  column(width = 6,
                         box(title = "% Open Days", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("open_days_chart"),tags$head(tags$style("#open_days_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Lessons to Avg. Temperature", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("lessons_t_chart"),tags$head(tags$style("#lessons_t_chart{height:25vh !important;}"))))),
                fluidRow(
                  column(width = 6,
                         box(title = "Distribution of Temperature", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("temp_chart"),tags$head(tags$style("#temp_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Distribution of Snow", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("snow_chart"),tags$head(tags$style("#snow_chart{height:25vh !important;}"))))
                )
        ),
        tabItem("scenario"
                #KPI Row
              
        )
      )
    )
  )
)

# Define server logic to render outputs
server <- function(input, output) {
  
  # Reactive Data to Season & View
  grouping <- reactive ({
    c <- summary %>%
      filter(season == input$season)
    
    if(input$view == 'by Season'){ g <- c$season
    }else if(input$view == 'by Month'){ g <- c$month
    }else{g <- c$DoW}
    g
  })
  
  season_info <- reactive({
    c <- summary %>%
      filter(season == input$season)
    
    k <- sales_data %>%
      filter(season == input$season)
    
    k <- k %>%
      group_by(season) %>%
      summarise(season_start = min(date),
                season_end = max(date))
    
    if(max(c$promotion) == 1){
      k$promotion = 'Yes'
    }else{
      k$promotion = 'No'
    }
    
    k
  })
  
  summ_data <- reactive({
    # Extract Metrics for Profit
    c <- summary %>%
      filter(season == input$season)
    
    # 'This Season', 'by Month', 'by Week', 'by Weekday'
    group <- grouping()

    c <- c %>%
      group_by(!!group) %>%
      summarise(total_cost = sum(total_cost), 
                n_cost = sum(n_cost), 
                p_cost = sum(p_cost),
                n_retCost = sum(n_retCost),
                p_retCost = sum(p_retCost),
                total_revenue = sum(total_revenue), 
                n_rev = sum(n_rev), 
                p_rev = sum(p_rev),
                profit = sum(profit), 
                total_inst = sum(total_inst), 
                n_inst = sum(n_inst), 
                p_inst = sum(p_inst), 
                lessons = sum(lessons),
                days_open = sum(days_open),
                days_season = sum(days_season),
                days_snow = sum(days_snow),
                days_ostaffed = sum(days_ostaffed),
                total_snow_cm = sum(total_snow_cm),
                snow_on_grnd_cm = sum (snow_on_grnd_cm),
                mean_temp_c_factor = sum(mean_temp_c_factor, na.rm = TRUE),
                mean_temp_c = sum(mean_temp_c_factor)/sum(days_season)/24)
    c
  })
  
  # -----------------------------------  Profit Tab ----------------------------------
  # ----------------------------------- Profit KPIs ----------------------------------
  output$total_profit <- renderValueBox({
    metric <- round(mean(summ_data()$profit),0)
    valueBox(value = paste("$", format(metric, format="d", big.mark=",")),
             subtitle = "Avg. Profit",icon = icon("usd"),color = "green")})  
  
  output$avg_profit_lesson <- renderValueBox({
    metric <- round(mean(summ_data()$profit/summ_data()$lessons),0)
    valueBox(value = paste("$", format(metric, format="d", big.mark=",")),
             subtitle = "Avg. Profit/Lesson",icon = icon("ticket"),color = "aqua")})
  
  output$avg_profit_instructor <- renderValueBox({
    metric <- round(mean(summ_data()$profit/summ_data()$total_inst),0)
    valueBox(value = paste("$", format(metric,format="d", big.mark=",")),
             subtitle = "Avg. Profit/Staff",icon = icon("street-view"),color = "purple")})  
  
  # ----------------------------------- Profit Charts ----------------------------------
  output$profit_chart<- renderPlot({
    barplot(summ_data()$profit,main="Profit", col = "blue")})
  
  output$profit_l_chart <- renderPlot({
    barplot(summ_data()$profit/summ_data()$lessons, main="Profit per Lesson", col = "green")})
  
  output$revenue_chart <- renderPlot({
    barplot(summ_data()$total_revenue,main="Revenue", col = "blue")})
  
  output$cost_chart <- renderPlot({
    barplot(summ_data()$total_cost,main="Expenses", col = "green")})
  
  
  # -----------------------------------  Capacity Tab ----------------------------------
  # ----------------------------------- Capacity KPIs ----------------------------------
  output$lessons_instructor <- renderValueBox({
    metric <- round(mean(summ_data()$lessons/summ_data()$total_inst),0)
    valueBox(value = toString(metric),subtitle = "Lessons/Instructor",
             icon = icon("graduation-cap"),color = "green")})  
  
  output$days_overstaffed <- renderValueBox({
    metric <- round(mean(summ_data()$days_ostaffed/summ_data()$days_open*100),0)
    valueBox(value = paste(toString(metric), '%'), subtitle = "% Days Overstaffed",
             icon = icon("user-plus"),color = "aqua")})
  
  output$avg_lessons_day <- renderValueBox({
    metric <- round(mean(summ_data()$lessons/summ_data()$days_open),0)
    valueBox(value = metric, subtitle = "Lessons per Open Day",icon = icon("child"),color = "purple")})  
  
  # ----------------------------------- Capacity Charts ----------------------------------
  output$lessons_chart<- renderPlot({
    barplot(summ_data()$lessons,main="Lessons", col = "blue")})
  
  output$lessons_i_chart <- renderPlot({
    barplot(summ_data()$lessons/summ_data()$total_inst, main="Profit per Lesson", col = "green")})
  
  output$overstaffed_chart <- renderPlot({
    barplot(summ_data()$days_ostaffed/summ_data()$days_open, main="Revenue", col = "blue")})
  
  output$lessons_d_chart <- renderPlot({
    hist(summ_data()$lessons, main="Distribution of Lessons", xlab="# Lesson")})

  # -----------------------------------  Weather Tab ----------------------------------
  # ----------------------------------- Weather KPIs ----------------------------------
  
  output$days_open <- renderValueBox({
    metric = round(mean(summ_data()$days_open),0)
    valueBox(value = metric, subtitle = "# Days Open",
             icon = icon("calendar-check-o"),color = "green")})  
  
  output$avg_temp<- renderValueBox({
    metric <- round(mean(summ_data()$mean_temp_c_factor/summ_data()$days_season/24),0)
    valueBox(value = paste(toString(metric), '°C'), subtitle = "Average Temperature",
             icon = icon("thermometer-half"),color = "aqua")})
  
  output$avg_snow <- renderValueBox({
    metric <- round(mean(summ_data()$snow_on_grnd_cm/summ_data()$days_season),0)
    valueBox(value = paste(toString(round(metric,0)), ' cm'),
             subtitle = "Avg. cm of Snow",icon = icon("snowflake-o"),color = "purple")})  
  
  # ----------------------------------- Weather Charts ----------------------------------
  output$open_days_chart<- renderPlot({
    barplot(summ_data()$days_open,main="Lessons", col = "blue")})
  
  output$lessons_t_chart <- renderPlot({
    plot(summ_data()$lessons, summ_data()$mean_temp_c, main="Avg. Temperature to Lessons", 
         xlab="Lessons", ylab="Avg. Temperature", pch=19)
    if (length(summ_data()$lessons) > 1){
      abline(lm(summ_data()$mean_temp_c ~ summ_data()$lessons), col="red") # regression line (y~x) 
    }
  })
  
  output$snow_chart <- renderPlot({
    hist(summ_data()$snow_on_grnd_cm, main="Distribution of Snow", xlab="Snow on Ground")})
  
  output$temp_chart <- renderPlot({
    hist(summ_data()$mean_temp_c, main="Distribution of Temperature", xlab="Temperature")})
  
  # ----------------------------------- Summary Metrics ----------------------------------
  output$metrics1 <- renderTable({
    n <- colnames(season_info())
    x <- t(season_info())
    colnames(x) <- c('Season Info')
    x
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

