gmap_pickup_points <- function(df_data, mode, sel_item1 = NULL, sel_item2 = NULL, sel_item3 = NULL){
  
  # ggplot map + shp shapes
  map_base <- get_map(c(-74.05, 40.54, -73.70001, 40.91553), 
                      maptype = "roadmap"
                      )
  gmap_base <- ggmap(map_base)
  
  if(mode == 1){
    # slicing by specific day: mode 1
    # get mode 1 variables
    sel_month <- sel_item1
    sel_day <- seq(sel_item2[1], sel_item2[2])
    sel_hour <- seq(sel_item3[1], sel_item3[2])
    
    # cutting time: type 1
    # sel_month <- c(6)
    # sel_day <- c(1:5)
    # sel_hour <- c(7, 8)
    
    # get filtered data frame
    yellow_cleaned_type1 <- df_data %>% 
      filter(pickup_month %in% sel_month, 
             pickup_day %in% sel_day, 
             pickup_hour %in% sel_hour)
    
    # plot points on ggmap layer
    gmap_type1_dots <- gmap_base + 
      geom_point(aes(x = pickup_longitude, y = pickup_latitude), 
                 data = yellow_cleaned_type1, 
                 colour = "black", 
                 size = 0.8) + 
      geom_polygon(data = df_sp, 
                   aes(x = long, y = lat, group = group), 
                   colour = "orange", # df_sp_borough$borough, 
                   alpha = 0.4, 
                   fill = df_sp_borough$color) + 
      geom_text(data = df_borough, 
                aes(x = long, y = lat, label = borough), 
                size = 10, 
                colour = "darkgreen")
    gmap_type1_dots # test run
  }
  else{
    # slicing by time in a day: mode 2
    # get mode 2 variables
    sel_isWeekend <- as.numeric(sel_item1)
    sel_weekday <- sel_item2
    sel_timeInDay <- sel_item3
    
    # cutting time: type 2
    # sel_isWeekend <- c(FALSE)
    # sel_weekday <- c("mon", "tue")
    # sel_timeInDay <- c("work_midnight", "work_on_work")
    
    # get filtered data frame
    yellow_cleaned_type2 <- 
      if(sel_isWeekend == 1){
        filter(df_data, 
               isWeekend == TRUE, 
               weekendTime %in% sel_timeInDay
               )} 
    else if(sel_isWeekend == 0){
      filter(df_data, 
             isWeekend == FALSE, 
             pickup_weekday %in% sel_weekday,  
             workTime %in% sel_timeInDay
             )}
    else{
      filter(df_data, 
             pickup_weekday %in% sel_weekday)}
    
    # plot points on ggmap layer
    gmap_type2_dots <- gmap_base + 
      geom_point(aes(x = pickup_longitude, y = pickup_latitude), 
                 data = yellow_cleaned_type2, 
                 colour = "black", 
                 size = 0.8) + 
      geom_polygon(data = df_sp, 
                   aes(x = long, y = lat, group = group), 
                   colour = "orange", # df_sp_borough$borough, 
                   alpha = 0.4, 
                   fill = df_sp_borough$color) + 
      geom_text(data = df_borough, 
                aes(x = long, y = lat, label = borough), 
                size = 10, 
                colour = "darkgreen")
    gmap_type2_dots # test run
  }
  
}

pickup_count <- function(df_data, mode, sel_item1 = NULL, sel_item2 = NULL, sel_item3 = NULL, sel_item4 = NULL){
  if(mode == 1){
    # slicing by specific day: mode 1
    sel_month <- sel_item1
    sel_day <- seq(sel_item2[1], sel_item2[2])
    sel_hour <- seq(sel_item3[1], sel_item3[2])
    
    # filter by restricting conditions
    yellow_cleaned_type1 <- df_data %>% 
      filter(pickup_month %in% sel_month, 
             pickup_day %in% sel_day, 
             pickup_hour %in% sel_hour)
    
    # plot
    if(length(sel_day) == 1){
      # summarise frequency by hours in a day
      cnt_df <- yellow_cleaned_type1$pickup_hour %>% 
        table() %>% as.data.frame()
      colnames(cnt_df) <- c("hour", "freq")
      
      # plot barplot
      ggplot(cnt_df, aes(x = hour, y = freq, fill = "#FA8072", alpha = 0.5)) + 
        ggtitle("Frequency of Pickup Times (by hours)") + 
        geom_bar(stat="identity") + 
        theme(panel.background = element_rect(fill = "#E0F5FF",
                                              colour = "#E0F5FF",
                                              size = 0.5, linetype = "solid"), 
              plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
              legend.position = "none")
    }else{
      # summarise frequency by days
      cnt_df <- yellow_cleaned_type1 %>% 
        group_by(pickup_month, pickup_day, pickup_hour) %>% 
        count(pickup_hour)
      colnames(cnt_df)[4] <- "freq"
      
      # plot barplot
      ggplot(cnt_df, aes(x = pickup_day, y = freq, fill = "#FA8072", alpha = 0.5)) + 
        ggtitle("Frequency of Pickup Times (by days)") + 
        geom_bar(stat="identity") + 
        theme(panel.background = element_rect(fill = "#E0F5FF",
                                              colour = "#E0F5FF",
                                              size = 0.5, linetype = "solid"), 
              plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
              legend.position = "none")
    }
    
    
  }else{
    # slicing by time in a day: mode 2
    sel_isWeekend <- as.numeric(sel_item1)
    sel_weekday <- sel_item2
    sel_timeInDay <- sel_item3
    sel_viewByHour <- sel_item4
    
    # filter by restricting conditions
    yellow_cleaned_type2 <- if(sel_isWeekend == 2){ # mixed
      filter(df_data,
             pickup_weekday %in% sel_weekday)
      }else if(sel_isWeekend == 0){ # weekday
        filter(df_data, 
              isWeekend == FALSE, 
              pickup_weekday %in% sel_weekday, 
              workTime %in% sel_timeInDay)
      }else{#(sel_isWeekend == 1){ # weekend
        filter(df_data, #df_data, 
              isWeekend == TRUE, 
              pickup_weekday %in% sel_weekday,  
              weekendTime %in% sel_timeInDay)
      }
    
    # plot
    if(sel_isWeekend == 2){ # mixed
      # summarise frequency by logical time in a day
      cnt_df <- yellow_cleaned_type2$pickup_weekday %>% 
        table() %>% as.data.frame()
      colnames(cnt_df) <- c("weekday", "freq")
      
      # plot barplot
      ggplot(cnt_df, aes(x = weekday, y = freq, fill = "#FA8072", alpha = 0.5)) + 
        ggtitle("Frequency of Pickup Times in a Week (by logical time)") + 
        geom_bar(stat="identity") + 
        theme(panel.background = element_rect(fill = "#E0F5FF",
                                              colour = "#E0F5FF",
                                              size = 0.5, linetype = "solid"), 
              plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
              legend.position = "none")
      
    }else if(sel_isWeekend == 1){ # weekend
      # summarise frequency by logical time in a day(weekend)
      ## summarise by logical time
      cnt_df <- yellow_cleaned_type2 %>% 
        group_by(pickup_weekday, weekendTime) %>% 
        count(weekendTime)
      colnames(cnt_df)[3] <- "freq"
      cnt_df <- cnt_df %>% left_join(dayTimeToHour, by = c("weekendTime" = "dayTime"))
      cnt_df <- cbind(combined_time = paste(cnt_df$pickup_weekday, cnt_df$order, cnt_df$weekendTime, cnt_df$hour, sep = "_"), 
                      cnt_df, 
                      freq_per_hour = cnt_df$freq / cnt_df$length)
      
      ## summarise by hours
      cnt_df_hour <- yellow_cleaned_type2 %>% 
        group_by(pickup_weekday, pickup_hour) %>% 
        count(pickup_hour) %>% 
        arrange(pickup_weekday, pickup_hour)
      colnames(cnt_df_hour)[3] <- "freq"
      # cnt_df_hour <- cbind(combined_time = paste(cnt_df_hour$pickup_weekday, cnt_df_hour$pickup_hour, sep = "_"), cnt_df_hour)
      
      # plot barplot
      if(sel_viewByHour == FALSE){
        ggplot(cnt_df, aes(x = combined_time, y = freq_per_hour, fill = cnt_df$pickup_weekday, alpha = 0.5)) + 
          ggtitle("Frequency of Pickup Times on Weekends (by logical time)") + 
          geom_bar(stat="identity") + 
          theme(panel.background = element_rect(fill = "#E0F5FF",
                                                colour = "#E0F5FF",
                                                size = 0.5, linetype = "solid"), 
                plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
                axis.text.x = element_text(angle = 45, hjust = 1), 
                legend.title = element_blank()) + 
          guides(alpha=FALSE) # remove specific legend
                # legend.position = "none")
      }else{
        ggplot(cnt_df_hour, aes(x = pickup_hour, y = freq, fill = cnt_df_hour$pickup_weekday, alpha = 0.5)) + 
          ggtitle("Frequency of Pickup Times on Workdays (by logical time)") + 
          geom_bar(stat="identity") + 
          theme(panel.background = element_rect(fill = "#E0F5FF",
                                                colour = "#E0F5FF",
                                                size = 0.5, linetype = "solid"), 
                plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
                # axis.text.x = element_text(angle = 45, hjust = 1), 
                legend.title = element_blank()) + 
          guides(alpha=FALSE) # remove specific legend
        # legend.position = "none")
      }
      
    }else{ # weekday
      # summarise frequency by logical time in a day(weekend)
      ## summarise by logical time
      cnt_df <- yellow_cleaned_type2 %>% 
        group_by(pickup_weekday, workTime) %>% 
        count(workTime)
      colnames(cnt_df)[3] <- "freq"
      cnt_df <- cnt_df %>% left_join(dayTimeToHour, by = c("workTime" = "dayTime"))
      cnt_df <- cbind(combined_time = paste(cnt_df$pickup_weekday, cnt_df$order, cnt_df$workTime, cnt_df$hour, sep = "_"), 
                      cnt_df, 
                      freq_per_hour = cnt_df$freq / cnt_df$length)
      
      ## summarise by hours
      cnt_df_hour <- yellow_cleaned_type2 %>% 
        group_by(pickup_weekday, pickup_hour) %>% 
        count(pickup_hour)
      colnames(cnt_df_hour)[3] <- "freq"
      # cnt_df_hour <- cbind(combined_time = paste(cnt_df_hour$pickup_weekday, cnt_df_hour$pickup_hour, sep = "_"), cnt_df_hour)
      
      # plot barplot
      if(sel_viewByHour == FALSE){
        ggplot(cnt_df, aes(x = combined_time, y = freq_per_hour, fill = cnt_df$pickup_weekday, alpha = 0.5)) + 
          ggtitle("Frequency of Pickup Times on Workdays (by logical time)") + 
          geom_bar(stat="identity") + 
          theme(panel.background = element_rect(fill = "#E0F5FF",
                                                colour = "#E0F5FF",
                                                size = 0.5, linetype = "solid"), 
                plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
                axis.text.x = element_text(angle = 45, hjust = 1), 
                legend.title = element_blank()) + 
          guides(alpha=FALSE) # remove specific legend
                # legend.position = "none")
      }else{
        ggplot(cnt_df_hour, aes(x = pickup_hour, y = freq, fill = cnt_df_hour$pickup_weekday, alpha = 0.5)) + 
          ggtitle("Frequency of Pickup Times on Workdays (by logical time)") + 
          geom_bar(stat="identity") + 
          theme(panel.background = element_rect(fill = "#E0F5FF",
                                                colour = "#E0F5FF",
                                                size = 0.5, linetype = "solid"), 
                plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5), 
                # axis.text.x = element_text(angle = 45, hjust = 1), 
                legend.title = element_blank()) + 
          guides(alpha=FALSE) # remove specific legend
                # legend.position = "none")
      }
    }
  }
}

corr_plot <- function(df_data, mode, sel_item1, sel_item2, sel_item3){
  if(mode == 1){
    # slicing by specific day: mode 1
    # get mode 1 variables
    sel_month <- sel_item1
    sel_day <- seq(sel_item2[1], sel_item2[2])
    sel_hour <- seq(sel_item3[1], sel_item3[2])
    
    # cutting time: type 1
    # sel_month <- c(6)
    # sel_day <- c(1:5)
    # sel_hour <- c(7, 8)
    
    # get filtered data frame
    yellow_cleaned_type <- df_data %>% 
      filter(pickup_month %in% sel_month, 
             pickup_day %in% sel_day, 
             pickup_hour %in% sel_hour)
    
  }else{
    # slicing by time in a day: mode 2
    # get mode 2 variables
    sel_isWeekend <- as.numeric(sel_item1)
    sel_weekday <- sel_item2
    sel_timeInDay <- sel_item3
    
    # cutting time: type 2
    # sel_isWeekend <- c(FALSE)
    # sel_weekday <- c("mon", "tue")
    # sel_timeInDay <- c("work_midnight", "work_on_work")
    
    # get filtered data frame
    yellow_cleaned_type <- 
      if(sel_isWeekend == 1){
        filter(df_data, 
               isWeekend == TRUE, 
               weekendTime %in% sel_timeInDay
        )} 
    else if(sel_isWeekend == 0){
      filter(df_data, 
             isWeekend == FALSE, 
             pickup_weekday %in% sel_weekday,  
             workTime %in% sel_timeInDay
      )}
    else{
      filter(df_data, 
             pickup_weekday %in% sel_weekday)}
  }
  
  corrplot(cor(yellow_cleaned_type %>% select(passenger_count, travel_time_sec, trip_distance, total_amount) %>% data.frame()),
           method = "pie", 
           type = "upper", 
           order = "hclust", 
           title = "Correlation of Passenger number, Travel time, \nTrip distance & Total fare amount", 
           col = brewer.pal(n = 10, name = "PuOr"), 
           tl.col = "black", tl.srt = 45, #Text label color and rotation
           addCoef.col = "turquoise",  # Add coefficient of correlation
           mar = c(0,0,3,0) # http://stackoverflow.com/a/14754408/54964
           )
}

attr_summ_tbl <- function(df_data, mode, sel_item1 = NULL, sel_item2 = NULL, sel_item3 = NULL){
  if(mode == 1){
    # slicing by specific day: mode 1
    # get mode   1 variables
    sel_month <- sel_item1
    sel_day <- seq(sel_item2[1], sel_item2[2])
    sel_hour <- seq(sel_item3[1], sel_item3[2])
    
    # filter by restricting conditions
    yellow_cleaned_type1 <- df_data %>% 
      filter(pickup_month %in% sel_month, 
             pickup_day %in% sel_day, 
             pickup_hour %in% sel_hour)
    
    # get summarised table
    if(length(sel_day) == 1){
      df_summ <- yellow_cleaned_type1 %>% 
        select(pickup_month, pickup_day, pickup_hour, passenger_count, trip_distance, travel_time_sec, tip_amount, total_amount) %>% 
        group_by(pickup_hour) %>% 
        summarise(passenger_num = mean(passenger_count), 
                  trip_distance = mean(trip_distance), 
                  travel_time = mean(travel_time_sec), 
                  total_fare_amount = mean(total_amount))
      df_summ
    }else{
      # summarise frequency by days
      df_summ <- yellow_cleaned_type1 %>% 
        select(pickup_month, pickup_day, pickup_hour, passenger_count, trip_distance, travel_time_sec, tip_amount, total_amount) %>% 
        group_by(pickup_month, pickup_day) %>% 
        summarise(passenger_num = mean(passenger_count), 
                  trip_distance = mean(trip_distance), 
                  travel_time = mean(travel_time_sec), 
                  total_fare_amount = mean(total_amount))
      df_summ
    }
    
  }else{
    # slicing by time in a day: mode 2
    # get mode 2 variables
    sel_isWeekend <- as.numeric(sel_item1)
    sel_weekday <- sel_item2
    sel_timeInDay <- sel_item3
    
    # filter by restricting conditions
    yellow_cleaned_type2 <- if(sel_isWeekend == 2){ # mixed
      filter(df_data,
             pickup_weekday %in% sel_weekday)
    }else if(sel_isWeekend == 0){ # weekday
      filter(df_data, 
             isWeekend == FALSE, 
             pickup_weekday %in% sel_weekday, 
             workTime %in% sel_timeInDay)
    }else{#(sel_isWeekend == 1){ # weekend
      filter(df_data, #df_data, 
             isWeekend == TRUE, 
             pickup_weekday %in% sel_weekday,  
             weekendTime %in% sel_timeInDay)
    }
    
    # get summarised table
    if(sel_isWeekend == 2){ # mixed
      df_summ <- yellow_cleaned_type2 %>% 
        select(pickup_weekday, passenger_count, trip_distance, travel_time_sec, tip_amount, total_amount) %>% 
        group_by(pickup_weekday) %>% 
        summarise(passenger_num = mean(passenger_count), 
                  trip_distance = mean(trip_distance), 
                  travel_time = mean(travel_time_sec), 
                  total_fare_amount = mean(total_amount))
      df_summ
    }else if(sel_isWeekend == 1){ # weekend
      df_summ <- yellow_cleaned_type2 %>% 
        select(pickup_weekday, pickup_weekday, weekendTime, passenger_count, trip_distance, travel_time_sec, tip_amount, total_amount) %>% 
        group_by(pickup_weekday, weekendTime) %>% 
        summarise(passenger_num = mean(passenger_count), 
                  trip_distance = mean(trip_distance), 
                  travel_time = mean(travel_time_sec), 
                  total_fare_amount = mean(total_amount))
      df_summ
    }else{ # weekday
      df_summ <- yellow_cleaned_type2 %>% 
        select(pickup_weekday, pickup_weekday, workTime, passenger_count, trip_distance, travel_time_sec, tip_amount, total_amount) %>% 
        group_by(pickup_weekday, workTime) %>% 
        summarise(passenger_num = mean(passenger_count), 
                  trip_distance = mean(trip_distance), 
                  travel_time = mean(travel_time_sec), 
                  total_fare_amount = mean(total_amount))
      df_summ
    }
  }
}