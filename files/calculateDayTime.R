# setwd("D:/finalProjectData/RShiny/NYCTaxiVis")

# Functions to calculate day time

# deprecated
custom_day_time <- function(row){
  row <- trimws(row)
  if(row[1] %in% c("mon", "tue", "wed", "thu", "fri") & row[2] %in% c(0:6))
  {"work_midnight"}
  else if(row[1] %in% c("mon", "tue", "wed", "thu", "fri") & row[2] %in% c(7:8))
  {"work_on_work"}
  else if(row[1] %in% c("mon", "tue", "wed", "thu", "fri") & row[2] %in% c(9:15))
  {"work_whiteday"}
  else if(row[1] %in% c("mon", "tue", "wed", "thu", "fri") & row[2] %in% c(16:17))
  {"work_off_work"}
  else if(row[1] %in% c("mon", "tue", "wed", "thu", "fri") & row[2] %in% c(18:23))
  {"work_night"}
  else if(row[1] %in% c("sat", "sun") & row[2] %in% c(0:4))
  {"week_midnight"}
  else if(row[1] %in% c("sat", "sun") & row[2] %in% c(5:11))
  {"week_morning"}
  else if(row[1] %in% c("sat", "sun") & row[2] %in% c(12:16))
  {"week_afternoon"}
  else if(row[1] %in% c("sat", "sun") & row[2] %in% c(17:23))
  {"week_night"}
  else{NA}
}

custom_weekend <- function(row){
  row <- trimws(row)
  if(row[2] %in% c(0:4))
  {"week_midnight"}
  else if(row[2] %in% c(5:11))
  {"week_morning"}
  else if(row[2] %in% c(12:16))
  {"week_afternoon"}
  else if(row[2] %in% c(17:23))
  {"week_night"}
  else{NA}
}

custom_work_day <- function(row){
  row <- trimws(row)
  if(row[2] %in% c(0:6))
  {"work_midnight"}
  else if(row[2] %in% c(7:8))
  {"work_on_work"}
  else if(row[2] %in% c(9:15))
  {"work_whiteday"}
  else if(row[2] %in% c(16:17))
  {"work_off_work"}
  else if(row[2] %in% c(18:23))
  {"work_night"}
  else{NA}
}
