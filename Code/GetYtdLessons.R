
#The function asumes that there is a sales_weather_data file as a DataFrame with a particular
#structure

#GetYtdLessons returns an int with the number of lessons until "date" ("date" day not included)

GetYtdLessons<-function(date){
 
  my_row<-sales_weather_data[sales_weather_data$date==date,] #capture the row that matches with the date in "date"
  
  my_season_number<-my_row$season #put the season number of "date" in "my_season_number
  
  my_season<-sales_weather_data[sales_weather_data$season==my_season_number,] #create a DataFrame with all the info for the season "my_season_number"
  
  my_query<-my_season[my_season$date<date,] #create a DataFrame with the info of the dates before "date"
  
  my_lessons<-0
  
  for (i in my_query$lessons) { #runs over the DataFrame "my_query" and add the number of lessons in each row
    my_lessons<-my_lessons+i 
  }

return(my_lessons) #return int with the number of lessons

}

GetYtdLessons("2013-12-05") #the date should be passed in this format: chr "%Y-%m-%d"

