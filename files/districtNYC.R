# setwd("D:/finalProjectData/RShiny/NYCTaxiVis")

library(rgdal)
# library(rgeos) # failed
library(broom)
# library(maptools)
library(gpclib)
library(ggplot2)
library(dplyr)
library(ggmap)
sp_shape <- readOGR(dsn = "data/taxi_zones", layer = "taxi_zones")
sp_shape2 <- spTransform(sp_shape, CRS("+proj=longlat +datum=WGS84"))
## Annotated
# yellow_june_map <- yellow_june_cleaned %>% select(ID, pickup_longitude, pickup_latitude)
# yellow_july_map <- yellow_july_cleaned %>% select(ID, pickup_longitude, pickup_latitude)

## Annotated
# coordinates(yellow_june_map)=~pickup_longitude+pickup_latitude
# coordinates(yellow_july_map)=~pickup_longitude+pickup_latitude

## Annotated
# proj4string(yellow_june_map) = proj4string(sp_shape2)
# proj4string(yellow_july_map) = proj4string(sp_shape2)

## Annotated
# yellow_june_dist <- over(yellow_june_map, sp_shape2)
# yellow_july_dist <- over(yellow_july_map, sp_shape2)

## Annotated
# Map area
# par(mar=c(0,0,0,0))
# plot(sp_shape, col="#d8d8d8", bg="skyblue", lwd=0.25, border=0)

# shp shapes to data frame
df_sp <- tidy(sp_shape2)#, region = "borough")

# shp shapes preparations
join_table <- sp_shape@data %>% select(LocationID, borough)
join_table$LocationID <- 0:262 %>% as.character()# (join_table$LocationID - 1)

# marking airport in join_table
# JFK airport: 132 zone
# LGA airport: 138 zone
levels(join_table$borough) <- c(levels(join_table$borough), c("JFK", "LGA"))
join_table$borough[which(join_table$LocationID %in% c("131", "137"))] <- c("JFK", "LGA")

df_sp_borough <- left_join(df_sp, join_table, by = c("id" = "LocationID"))
# df_sp_borough$group <- paste0(df_sp_borough$borough, ".1")

# produce borough & zone coordinates
df_borough <- df_sp_borough %>% group_by(borough) %>% summarise(long = mean(long), lat = mean(lat))
df_borough$lat[7:8] <- df_borough$lat[7:8] + c(0.018, 0.015) # modify airport location
# df_zone <-  df_sp %>% group_by(id) %>% summarise(long = mean(long), lat = mean(lat))

# modify df_sp_borough: mutate color column
df_borough <- df_borough %>% mutate(color = c("#CFF1FF", "#DDC2FF", 
                                              "#000000", "#EFAFBF", 
                                              "#C2F2B5", "#000000", 
                                              "#FFE0F5", "#FFE0F5"))
df_sp_borough <- df_sp_borough %>% 
  left_join(data.frame(borough = df_borough$borough, color = df_borough$color))

# necessary variables
# df_borough
# df_sp
# df_sp_borough

# remove useless variables
rm(sp_shape, 
   sp_shape2, 
   join_table)
