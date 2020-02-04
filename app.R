library(shiny)
library(shinyjs)
library(rgdal)
library(broom)
library(gpclib)
library(ggplot2)
library(dplyr)
library(ggmap)
library(corrplot)
library(RColorBrewer)
# setwd("D:/finalProjectData/RShiny/NYCTaxiVis")
source("files/dataPreprocessing.R")
source("files/helpers.R")
df_data <- rbind(yellow_june_cleaned, yellow_july_cleaned)

# Define UI ----
ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  titlePanel("DataMining Course Project with analysis of New York City Taxi data"), 
  
  sidebarLayout(
    
    sidebarPanel(
      helpText(h4("Time are defined by pick up time.")),
      
      radioButtons("radio_mode", 
                   h4("Time slicing mode"),
                   choices = list("Specific time (Month, day, hour)" = 1, 
                                  "Logical time (Weekday)" = 2), 
                   selected = 1),
      br(),
      helpText(h3("Time slicing mode 1: (by Specific time)")),
      
      checkboxGroupInput("checkGr_month", 
                         h4("Month"), 
                         choices = list("June" = 6, 
                                        "July" = 7),
                         selected = c(6)),
      sliderInput("sli_ty1_day", h4("Day"),
                  min = 1, max = 31, value = c(1, 5)),
      sliderInput("sli_ty1_hour", h4("Hour"),
                  min = 0, max = 23, value = c(5, 9)),
      br(),
      helpText(h3("Time slicing mode 2: (by Logical time)")),
      
      radioButtons("radio_isWeekend", 
                   h4("Workday or weekend"),
                   choices = list("Mixed" = 2, "Workday" = 0, "Weekend" = 1), 
                   selected = 2),
      
      helpText("Suitable when cheking Workday & Weekend"), 
      checkboxInput("check_viewHour", "View by hours", value = FALSE),
      
      checkboxGroupInput("checkGr_weekday", 
                         h4("Weekday"), 
                         choices = list("Monday" = "mon", 
                                        "Tuesday" = "tue", 
                                        "Wednsday" = "wed",
                                        "Thursday" = "thu", 
                                        "Friday" = "fri", 
                                        "Saturday" = "sat", 
                                        "Sunday" = "sun"),
                         selected = c("mon", "tue", "wed")),
      # checkboxInput("check_weekday", "Select all", value = FALSE),
      checkboxGroupInput("checkGr_timeInDay_work", 
                         h4("Time in a day(workday)"), 
                         choices = list("Midnight" = "work_midnight", 
                                        "Go to work" = "work_on_work", 
                                        "Daytime" = "work_whiteday",
                                        "Off work" = "work_off_work", 
                                        "Night" = "work_night"),
                         selected = NULL),
      # checkboxInput("check_workday", "Select all", value = FALSE),
      checkboxGroupInput("checkGr_timeInDay_weekend", 
                         h4("Time in a day(weekend)"), 
                         choices = list("Midnight" = "week_midnight", 
                                        "Morning" = "week_morning", 
                                        "Afternoon" = "week_afternoon",
                                        "Night" = "week_night"),
                         selected = NULL),
      # checkboxInput("check_weekend", "Select all", value = FALSE)
      
    ),
    
    mainPanel(
      plotOutput("gmap_NYC", height = "800px"),
      plotOutput("pickup_count"), 
      plotOutput("time_fare_dist_Corr", height = "600px"), 
      tableOutput("attr_summarise")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  observe({
    if(input$radio_mode == 1){
      disable("radio_isWeekend")
      disable("checkGr_weekday")
      disable("checkGr_timeInDay_work")
      disable("checkGr_timeInDay_weekend")
      enable("checkGr_month")
      enable("sli_ty1_day")
      enable("sli_ty1_hour")
    }else{
      enable("radio_isWeekend")
      enable("checkGr_weekday")
      enable("checkGr_timeInDay_work")
      enable("checkGr_timeInDay_weekend")
      disable("checkGr_month")
      disable("sli_ty1_day")
      disable("sli_ty1_hour")
    }
    
    if(input$radio_mode == 2 & input$radio_isWeekend == 0){
      enable(selector = "#checkGr_weekday input[value='mon']")
      enable(selector = "#checkGr_weekday input[value='tue']")
      enable(selector = "#checkGr_weekday input[value='wed']")
      enable(selector = "#checkGr_weekday input[value='thu']")
      enable(selector = "#checkGr_weekday input[value='fri']")
      disable(selector = "#checkGr_weekday input[value='sat']")
      disable(selector = "#checkGr_weekday input[value='sun']")
      enable("checkGr_timeInDay_work")
      disable("checkGr_timeInDay_weekend")
    }else if(input$radio_mode == 2 & input$radio_isWeekend == 1){
      disable(selector = "#checkGr_weekday input[value='mon']")
      disable(selector = "#checkGr_weekday input[value='tue']")
      disable(selector = "#checkGr_weekday input[value='wed']")
      disable(selector = "#checkGr_weekday input[value='thu']")
      disable(selector = "#checkGr_weekday input[value='fri']")
      enable(selector = "#checkGr_weekday input[value='sat']")
      enable(selector = "#checkGr_weekday input[value='sun']")
      disable("checkGr_timeInDay_work")
      enable("checkGr_timeInDay_weekend")
    }else if(input$radio_mode == 2 & input$radio_isWeekend == 2){
      enable(selector = "#checkGr_weekday input[value='mon']")
      enable(selector = "#checkGr_weekday input[value='tue']")
      enable(selector = "#checkGr_weekday input[value='wed']")
      enable(selector = "#checkGr_weekday input[value='thu']")
      enable(selector = "#checkGr_weekday input[value='fri']")
      enable(selector = "#checkGr_weekday input[value='sat']")
      enable(selector = "#checkGr_weekday input[value='sun']")
      disable("checkGr_timeInDay_work")
      disable("checkGr_timeInDay_weekend")
    }else{
      NULL
    }
    
    # Select all function
    # if(input$check_weekday == TRUE){
    #   updateCheckboxGroupInput(session, 
    #                            "checkGr_weekday", 
    #                            "Weekday", 
    #                            choices = checkGr_weekday_list, 
    #                            selected = checkGr_weekday_list)
    # }
    # 
    # if(input$check_workday == TRUE){
    #   updateCheckboxGroupInput(session, 
    #                            "checkGr_timeInDay_work", 
    #                            "Time in a day(workday)", 
    #                            choices = checkGr_timeInDay_work_list, 
    #                            selected = checkGr_timeInDay_work_list)
    # }
    # 
    # if(input$check_weekend == TRUE){
    #   updateCheckboxGroupInput(session, 
    #                            "checkGr_timeInDay_weekend", 
    #                            "Time in a day(weekend)", 
    #                            choices = checkGr_timeInDay_weekend_list, 
    #                            selected = checkGr_timeInDay_weekend_list)
    # }
    
  })
  
  output$gmap_NYC <- renderPlot({
    mode <- input$radio_mode
    
    if(mode == 1){
      month <- input$checkGr_month
      day <- input$sli_ty1_day
      hour <- input$sli_ty1_hour
      
      sel_item1 <- month
      sel_item2 <- day
      sel_item3 <- hour
    }else{
      isWeekend <- input$radio_isWeekend
      weekday <- input$checkGr_weekday
      
      if(isWeekend == 0){
        timeInDay <- input$checkGr_timeInDay_work
        sel_item3 <- timeInDay
      }else if(isWeekend == 1){
        timeInDay <- input$checkGr_timeInDay_weekend
        sel_item3 <- timeInDay
      }else{
        sel_item3 <- NULL
      }
      
      sel_item1 <- isWeekend
      sel_item2 <- weekday
      # sel_item3 <- timeInDay
    }
    gmap_pickup_points(df_data, mode, sel_item1, sel_item2, sel_item3)
  }, height = 800)
  
  output$pickup_count <- renderPlot({
    mode <- input$radio_mode
    
    if(mode == 1){
      month <- input$checkGr_month
      day <- input$sli_ty1_day
      hour <- input$sli_ty1_hour
      
      sel_item1 <- month
      sel_item2 <- day
      sel_item3 <- hour
    }else{
      viewByHour <- input$check_viewHour
      isWeekend <- input$radio_isWeekend
      weekday <- input$checkGr_weekday
      
      if(isWeekend == 0){
        timeInDay <- input$checkGr_timeInDay_work
        sel_item3 <- timeInDay
      }else if(isWeekend == 1){
        timeInDay <- input$checkGr_timeInDay_weekend
        sel_item3 <- timeInDay
      }else{
        sel_item3 <- NULL
      }
      
      sel_item1 <- isWeekend
      sel_item2 <- weekday
      # sel_item3 <- timeInDay
      sel_item4 <- viewByHour
    }
    
    pickup_count(df_data, mode, sel_item1, sel_item2, sel_item3, sel_item4)
  })
  
  output$time_fare_dist_Corr <- renderPlot({
    mode <- input$radio_mode
    
    if(mode == 1){
      month <- input$checkGr_month
      day <- input$sli_ty1_day
      hour <- input$sli_ty1_hour
      
      sel_item1 <- month
      sel_item2 <- day
      sel_item3 <- hour
    }else{
      isWeekend <- input$radio_isWeekend
      weekday <- input$checkGr_weekday
      
      if(isWeekend == 0){
        timeInDay <- input$checkGr_timeInDay_work
        sel_item3 <- timeInDay
      }else if(isWeekend == 1){
        timeInDay <- input$checkGr_timeInDay_weekend
        sel_item3 <- timeInDay
      }else{
        sel_item3 <- NULL
      }
      
      sel_item1 <- isWeekend
      sel_item2 <- weekday
      # sel_item3 <- timeInDay
    }
    
    corr_plot(df_data, mode, sel_item1, sel_item2, sel_item3)
  }, height = 500)
  
  output$attr_summarise <- renderTable({
    mode <- input$radio_mode
    
    if(mode == 1){
      month <- input$checkGr_month
      day <- input$sli_ty1_day
      hour <- input$sli_ty1_hour
      
      sel_item1 <- month
      sel_item2 <- day
      sel_item3 <- hour
    }else{
      isWeekend <- input$radio_isWeekend
      weekday <- input$checkGr_weekday
      
      if(isWeekend == 0){
        timeInDay <- input$checkGr_timeInDay_work
        sel_item3 <- timeInDay
      }else if(isWeekend == 1){
        timeInDay <- input$checkGr_timeInDay_weekend
        sel_item3 <- timeInDay
      }else{
        sel_item3 <- NULL
      }
      
      sel_item1 <- isWeekend
      sel_item2 <- weekday
      # sel_item3 <- timeInDay
    }
    
    data.frame(mean = isolate(attr_summ_tbl(df_data, mode, sel_item1, sel_item2, sel_item3)))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)