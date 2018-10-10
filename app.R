


library(shiny)
library(googlesheets)
library (shinydashboard)
library(data.table)
library(ggplot2)

suppressMessages(library(dplyr))

# Import data
gs_data <- gs_key("1-17_HfRFg6guIf3lDs-Lsa6zGmOhKRU5AkNQvUcWnNc", lookup = FALSE, visibility = "private")
summary <- gs_read(gs_data, ws = "summary")
sales_data <- gs_read(gs_data, ws = "sales_data")
model_dates <- gs_read(gs_data, ws = "model_dates")
p <- gs_read(gs_data, ws = "model_parameters")

# Define UI for application 
ui <- (
  dashboardPage(
    dashboardHeader(
      # Application title
      title = "Skiing Lessons"
    ),
    dashboardSidebar(
      
      uiOutput("season_control"),
      
      # Dashboard View Selector
      selectInput("view", "Dashboard View:", 
                  choices = c('by Season', 'by Month','by Weekday'),
                  selected = 'by Season'),
      
      sidebarMenu(id = 'tab',
        # Tabs
        menuItem("Profit Analysis", tabName = "profit",icon=icon("usd")),
        menuItem("Capacity Management", tabName = "capacity",icon=icon("users")),
        menuItem("Weather Assessment", tabName = "weather",icon=icon("snowflake-o")),
        menuItem("Scenario Builder", tabName = "scenario",icon=icon("area-chart"))
      ),  
      uiOutput("controls")
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
                         box(title = "Lessons to Instructor Ratio", solidHeader = TRUE, width = NULL, status = "primary",
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
                         box(title = "# Open Days", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("open_days_chart"),tags$head(tags$style("#open_days_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Lessons to Temperature Trends", solidHeader = TRUE, width = NULL, status = "primary",
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
        tabItem("scenario",
                
                #KPI Row
                fluidRow(
                  valueBoxOutput("s_profit"),
                  valueBoxOutput("s_lessons"),
                  valueBoxOutput("s_inst")
                ),
                
                #Charts Row
                fluidRow(
                  column(width = 6,
                         box(title = "Expected Profit", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("s_profit_chart"),tags$head(tags$style("#s_profit_chart{height:25vh !important;}")))),
                  column(width = 6,
                         box(title = "Expected Lessons", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("s_lessons_chart"),tags$head(tags$style("#s_lessons_chart{height:25vh !important;}"))))),
                fluidRow(
                  column(width = 6,
                         box(title = "Lessons to Instructors Ratio", solidHeader = TRUE, width = NULL, status = "primary",
                             plotOutput("s_lessons_i_chart"),tags$head(tags$style("#s_lessons_i_chart{height:25vh !important;}")))),
                  column(width =6,verbatimTextOutput("recommendation"))
                )
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
    }else if(input$view == 'by Month'){ g <- factor(month.abb[c$month], 
                                                    levels =  c("Jan","Feb","Mar",
                                                                "Apr","May","Jun",
                                                                "Jul","Aug","Sep",
                                                                "Oct","Nov","Dec"))
    }else{g <- factor(c$DoW, levels= c("Monday", "Tuesday", "Wednesday", 
                                       "Thursday", "Friday", "Saturday", "Sunday"))}
    g
  })
  
  season_info <- reactive({
    c <- summary %>%
      filter(season == input$season)
    k <- sales_data %>%
      filter(season == input$season)
    
    k <- k %>%
      group_by(season) %>%
      summarise(s_start = min(date),
                s_end = max(date))
    
    if(max(c$promotion) == 1){
      k$promotion = 'Yes'
    }else{
      k$promotion = 'No'
    }
    k
  })
  
  model <- reactive({
    start_Date <- as.Date(format(input$season_length[1]))
    end_Date <- as.Date(format(input$season_length[2]))
    
    # Filter days of interest
    x <- model_dates[model_dates$Date >= start_Date,]
    x <- x[x$Date <= end_Date,]
    x$count <- 1
    
    # Summarize all concepts
    y<- x %>%
      group_by(DoW, Month, Year, Day_Num) %>%
      summarise(count = sum(count))
    size <- nrow(y)
    
    # Calculate n = size values from normal dist
    y$snow_avg <- c(rnorm(size, input$exp_snow, p$s_snow)) 
    y$temp_avg <- c(rnorm(size, input$exp_temp, p$s_temp))
    
    # Calculate total factors
    y$snow <- y$snow_avg * y$count       
    y$temp <- y$temp_avg * y$count
    
    # Assign factors to week vector
    week <- c(0, p$DoWTuesday, p$DoWWednesday, p$DoWThursday, 
              p$DoWFriday, p$DoWSaturday, p$DoWSunday)
    
    # Calculate lessons
    y$f_int <- as.numeric(p['(Intercept)'])
    y$f_days <- p$days_season * y$Day_Num
    y$f_snow <- p$snow_on_grnd_cm * y$snow
    y$f_temp <- p$mean_temp_c_factor * y$temp
    y$f_month <- p$month * y$Month
    y$f_DoW <- week[y$Day_Num]
    y$lessons <- y$f_int + y$f_days + y$f_snow + y$f_temp +y$f_month +y$f_DoW
    inst <- ceiling(max(y$lessons/y$Day_Num))
    y$profit <- 549/2 * y$lessons - 100*(inst * y$Day_Num - y$lessons)
    y$inst <- inst * y$Day_Num
    y$season <- 1
    
    if(input$view == 'by Season'){ y$group <- y$season
    }else if(input$view == 'by Month'){ y$group <- factor(month.abb[y$Month], 
                                                          levels =  c("Jan","Feb","Mar",
                                                                      "Apr","May","Jun",
                                                                      "Jul","Aug","Sep",
                                                                      "Oct","Nov","Dec"))
    }else{y$group <- factor(y$DoW, levels=  c("Monday", "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", "Saturday", "Sunday"))}
    
    y <- y %>%
      group_by(group) %>%
      summarize(profit = sum(profit),
                lessons = sum(lessons),
                inst = sum(inst),
                days_season = sum(Day_Num))
  
    y
  })
  
  control_data <- reactive({
    if(input$tab != 'scenario'){
      c <- summary %>%
        filter(season == input$season)
    }else{
      min_temp <- min(summary$mean_temp_c_factor/summary$days_season/2)
      max_temp <- max(summary$mean_temp_c_factor/summary$days_season/2)
      mean_temp <- mean(summary$mean_temp_c_factor/summary$days_season/2)
      min_snow <- min(summary$snow_on_grnd_cm/summary$days_season)
      max_snow <- max(summary$snow_on_grnd_cm/summary$days_season)
      mean_snow <- mean(summary$snow_on_grnd_cm/summary$days_season)
      z <- list(min_temp = min_temp, max_tem = max_temp, mean_temp = mean_temp,
                min_snow = min_snow, max_snow = max_snow, mean_snow = mean_snow)
      z
    }
  })
  
  summ_data <- reactive({
    # Extract Metrics for Profit
    c <- summary %>%
      filter(season == input$season)
    
    # 'This Season', 'by Month', 'by Week', 'by Weekday'
    c$group <- grouping()

    c <- c %>%
      group_by(group) %>%
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
    #barplot(summ_data()$profit,main="Profit", col = "blue")})
    ggplot(data=summ_data(), aes(x = factor(group), y = profit))+
      geom_bar(stat = 'identity', fill = 'steelblue')+
      xlab('') +
      ylab('dollars') +
      theme_minimal()
  })
  
  output$profit_l_chart <- renderPlot({
    #barplot(summ_data()$profit/summ_data()$lessons, main="Profit per Lesson", col = "green")})
    ggplot(data=summ_data(), aes(x = factor(group), y = profit/lessons))+
      geom_bar(stat = 'identity', fill = 'lightsteelblue')+
      xlab('') +
      ylab('dollars') +
      theme_minimal()
  })
  
  output$revenue_chart <- renderPlot({
    #barplot(summ_data()$total_revenue,main="Revenue", col = "blue")})
    ggplot(data=summ_data(), aes(x = factor(group), y = total_revenue))+
      geom_bar(stat = 'identity', fill = 'forestgreen')+
      xlab('') +
      ylab('dollars') +
      theme_minimal()
  })
  
  
  output$cost_chart <- renderPlot({
    #barplot(summ_data()$total_cost,main="Expenses", col = "green")})
    ggplot(data=summ_data(), aes(x = factor(group), y = total_cost))+
      geom_bar(stat = 'identity', fill = 'red4')+
      xlab('') +
      ylab('dollars') +
      theme_minimal()
  })
  
  # -----------------------------------  Capacity Tab ----------------------------------
  # ----------------------------------- Capacity KPIs ----------------------------------
  output$lessons_instructor <- renderValueBox({
    metric <- round(mean(summ_data()$lessons/summ_data()$total_inst),2)
    valueBox(value = toString(metric),subtitle = "Lessons/Instructor",
             icon = icon("graduation-cap"),color = "green")})  
  
  output$days_overstaffed <- renderValueBox({
    metric <- round(mean(summ_data()$days_ostaffed/summ_data()$days_open*100),0)
    valueBox(value = paste(toString(metric), '%'), subtitle = "% Days Overstaffed",
             icon = icon("user-plus"),color = "aqua")})
  
  output$avg_lessons_day <- renderValueBox({
    metric <- round(mean(summ_data()$lessons/summ_data()$days_open),2)
    valueBox(value = metric, subtitle = "Lessons per Open Day",icon = icon("child"),color = "purple")})  
  
  # ----------------------------------- Capacity Charts ----------------------------------
  output$lessons_chart<- renderPlot({
    #barplot(summ_data()$lessons,main="Lessons", col = "blue")})
    ggplot(data=summ_data(), aes(x = factor(group), y = lessons))+
      geom_bar(stat = 'identity', fill = 'forestgreen', width=0.7)+
      xlab('') +
      ylab('# of lessons') +
      theme_minimal()
    })
  
  output$lessons_i_chart <- renderPlot({
    ggplot(data = summ_data()) +
      geom_bar(aes(x = factor(group), y = lessons/total_inst), width=0.7, stat = 'identity', alpha = 0.7, fill = 'purple4') +
      xlab('') +
      ylab('%') +
      theme_minimal()
    
    # ggplot(data = summ_data(), aes(lessons, total_inst)) + 
    #   geom_point() + geom_smooth(method = "lm") +
    #   theme_minimal() +
    #   ylab('instructors')
  })
  
  output$overstaffed_chart <- renderPlot({
    ggplot(data = summ_data()) + 
      geom_bar(aes(x = factor(group), y = days_ostaffed/days_open),width=0.7, stat = 'identity', alpha = 0.8, fill = "steelblue") +
      xlab('') +
      ylab('%') +
      theme_minimal()
  })
  
  output$lessons_d_chart <- renderPlot({
    #hist(summ_data()$lessons, main="Distribution of Lessons", xlab="# Lesson")
    ggplot(data=summ_data(), aes(x = factor(group) ,y = lessons))+
      geom_bar(stat = 'identity', width=0.7, fill="steelblue")+
      theme_minimal() +
      ylab('lessons') +
      xlab('')
  })

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
    ggplot(data=summ_data(), aes(x = factor(group) ,y = days_open))+
      geom_bar(stat = 'identity', width=0.7, fill="forestgreen")+
      theme_minimal() +
      ylab('# of days') +
      xlab('')
    })
  
  output$lessons_t_chart <- renderPlot({
    
    ggplot(data = summ_data(), aes(lessons, mean_temp_c)) + 
      geom_point() + geom_smooth(method = "lm") +
      theme_minimal() +
      ylab('°C')
  
  })
  
  output$snow_chart <- renderPlot({
    ggplot(data=summ_data(), aes(x = factor(group) ,y = snow_on_grnd_cm/days_season))+
      geom_bar(stat = 'identity', width=0.7, fill="steelblue")+
      theme_minimal() +
      ylab('cm.') +
      xlab('')
    })
  
  output$temp_chart <- renderPlot({
    ggplot(data=summ_data(), aes(x = factor(group) , y = mean_temp_c)) +
      geom_bar(stat = 'identity', width=0.7) +
      theme_minimal() +
      ylab('°C') +
      xlab('')
  })
  
  # ----------------------------------- Scenario Tab ----------------------------------
  # ----------------------------------- Scenario KPIs ---------------------------------
  
  output$s_profit <- renderValueBox({
    metric <- round(mean(model()$profit),0)
    valueBox(value = paste("$", format(metric, format="d", big.mark=",")),
             subtitle = "Avg. Profit",icon = icon("usd"),color = "green")})  
  
  output$s_lessons<- renderValueBox({
    metric <- round(mean(model()$lessons/model()$inst),2)
    valueBox(value = toString(metric),subtitle = "Lessons/Instructor",
             icon = icon("graduation-cap"),color = "green")})  
  
  output$s_inst <- renderValueBox({
    metric = round(mean(model()$inst/model()$days_season),0)
    valueBox(value = metric, subtitle = "# Instructors",
             icon = icon("street-view"),color = "green")})  
  
  # ----------------------------------- Scenario Charts ----------------------------------
  
  output$s_profit_chart<- renderPlot({
    ggplot(data=model(), aes(x = factor(group), y = profit))+
      geom_bar(stat = 'identity', fill = "steelblue", width=0.7)+
      xlab('') +
      ylab('$') +
      theme_minimal()
  })
  
  output$s_lessons_chart<- renderPlot({
    ggplot(data=model(), aes(x = factor(group), y = lessons))+
      geom_bar(stat = 'identity', fill = 'forestgreen', width=0.7)+
      xlab('') +
      ylab('# of lessons') +
      theme_minimal()})
  
  output$s_lessons_i_chart<- renderPlot({
    ggplot(data = model()) + 
      geom_bar(aes(x = factor(group), y = lessons/inst), stat = 'identity', width=0.7, alpha = 0.7, fill = 'purple4') +
      xlab('') +
      ylab('%') +
      theme_minimal()
    })
  
  output$recommendation <- renderText({ 
    metric = round(mean(model()$inst/model()$days_season),0)
    r1 <- paste("The chosen season length is ",sum(model()$days_season)," days.")
    r2 <- paste("You can expect a profit of $", format(sum(model()$profit), format="d", big.mark=","))
    r3 <- paste("There will be a total of ", round(sum(model()$lessons),0), "lessons.")
    r4 <- paste("You should hire ", metric, " instructors.")
    paste(r1, r2, r3, r4, sep="\n")
  })
  
  # ----------------------------------- Summary Metrics ----------------------------------
  output$metrics1 <- renderTable({
    if (input$tab != 'scenario'){
      m <- t(data.frame(season_info()[2:4]))
      rownames(m) <-c('Season Start', 'Season End', 'Promotion')
      colnames(m) <- c('Season Info')
      m
    }
  },  rownames = TRUE)
  
  
  # ----------------------------------- Dynamic Sidebar ----------------------------------
  # ---------------------------------- Scenario Controls ---------------------------------
  output$controls <- renderUI({
    if (input$tab != 'scenario'){
      column(width = 5,style='padding:10px',align = 'center',
             tableOutput("metrics1"))
    }else{
      list(sliderInput("exp_temp", "Avg. Temperature (in °C):",
                  min = control_data()$min_temp, max = 10,
                  value = control_data()$mean_temp, step = 2.5),
      
      sliderInput("exp_snow", "Avg. Snow (in cm):",
                  min = control_data()$min_snow, max =  100,
                  value = control_data()$mean_snow, step = 2.5),
      
      dateRangeInput("season_length", "Season Start:", start = Sys.Date(), end = Sys.Date()+30,
                     min = Sys.Date(), max = Sys.Date() + 365,
                     format = "yyyy-mm-dd", startview = "day", weekstart = 1,
                     separator = " to ", language = "en", width = NULL))
      
    #   selectInput("promo", "Promotion?", 
    #               choices = c('Yes', 'No'),
    #               selected = 'No'))
     }
  })
  
  # -------------------------------- Descriptive Controls -------------------------------
  output$season_control <- renderUI({
    # Season Selector
    if (input$tab != 'scenario'){
      selectInput("season", "Season:", 
                  choices = summary$season[summary$season != 0],
                  selected = summary$season[summary$season == 0])
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

