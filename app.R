


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
                  selected = summary$season[summary$season == '1'],
                  multiple = TRUE),
      
      # Dashboard View Selector
      selectInput("view", "Dashboard View:", 
                  choices = c('This Season', 'by Month','by Weekday'),
                  selected = 'This Season'),
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
                  valueBoxOutput("days_snow"),
                  valueBoxOutput("corr_snow_lessons")
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
                         box(title = "% Snow Days Open", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("snow_chart"),tags$head(tags$style("#snow_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Lessons to Snow cm", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("lessons_s_chart"),tags$head(tags$style("#lessons_s_chart{height:25vh !important;}"))))
                )
        ),
        tabItem("scenario")
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
    
    if(input$view == 'This Season'){ g <- c$season
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
                mean_temp_c = sum(mean_temp_c_factor)/sum(days_season)/24)
    c
  })
  
  # -----------------------------------  Profit Tab ----------------------------------
  # ----------------------------------- Profit KPIs ----------------------------------
  output$total_profit <- renderValueBox({
    valueBox(value = paste("$", format(round(summ_data()$profit,0), format="d", big.mark=",")),
             subtitle = "Total Profit",icon = icon("usd"),color = "green")})  
  
  output$avg_profit_lesson <- renderValueBox({
    valueBox(value = paste("$", format(round(summ_data()$profit/summ_data()$lessons,0), 
                                        format="d", big.mark=",")),
             subtitle = "Avg. Profit/Lesson",icon = icon("ticket"),color = "aqua")})
  
  output$avg_profit_instructor <- renderValueBox({
    valueBox(value = paste("$", format(round(summ_data()$profit/summ_data()$total_inst,1),
                                        format="d", big.mark=",")),
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
    valueBox(value = toString(round(mean(summ_data()$lessons/summ_data()$total_inst),2)),
             subtitle = "Lessons/Instructor",icon = icon("graduation-cap"),color = "green")})  
  
  output$days_overstaffed <- renderValueBox({
    valueBox(value = paste(toString(round(mean(summ_data()$days_ostaffed/summ_data()$days_open*100),0)), '%'),
             subtitle = "% Days Overstaffed",icon = icon("user-plus"),color = "aqua")})
  
  output$avg_lessons_day <- renderValueBox({
    valueBox(value = round(summ_data()$lessons/summ_data()$days_open,1),
             subtitle = "Lessons per Open Day",icon = icon("child"),color = "purple")})  
  
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
    valueBox(value = toString(round(mean(summ_data()$days_open),2)),
             subtitle = "# Days Open",icon = icon("calendar-check-o"),color = "green")})  
  
  output$days_snow <- renderValueBox({
    valueBox(value = toString(round(mean(summ_data()$days_snow),2)),
             subtitle = "# Days with 15cm + snow ",icon = icon("snowflake-o"),color = "aqua")})
  
  output$corr_snow_lessons <- renderValueBox({
    valueBox(value = paste(toString(round(cor(summ_data()$lessons,summ_data()$snow_on_grnd_cm)*100,0)), '%'),
             subtitle = "% Lessons driven by Snow cm",icon = icon("percent"),color = "purple")})  
  
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
    plot(summ_data()$days_snow/summ_data()$days_open, main="% Snow Days Open", col = "blue")})
  
  output$lessons_s_chart <- renderPlot({
    hist(summ_data()$snow_on_grnd_cm, main="Distribution of Snow", xlab="Snow on Ground")})
  
  
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

