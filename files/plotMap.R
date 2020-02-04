source("D:/finalProjectData/RShiny/NYCTaxiVis/districtNYC.R")

library(ggmap)

# ggplot map + shp shapes
map_base <- get_map(c(-74.05, 40.54, -73.70001, 40.91553), 
                    maptype = "roadmap"
)
gmap_base <- ggmap(map_base)

################ plot boroughs & zones ################
df_zone <-  df_sp %>% group_by(id) %>% summarise(long = mean(long), lat = mean(lat))

gmap_boroughs <- 
  gmap_base + 
  geom_polygon(data = df_sp, 
               mapping = aes(x = long, y = lat, group = group), 
               colour = "orange", # df_sp_borough$borough, 
               alpha = 0.4, 
               fill = df_sp_borough$color) + 
  geom_label(data = df_borough, 
             mapping = aes(x = long, y = lat, label = borough, fill = "black", alpha = 0.5),
             size = 10, 
             colour = "white", #"darkgreen", 
             label.size = 0) + 
  scale_fill_manual(values = rep("#1E1E1E", 8)) + 
  theme(legend.position = "none")
gmap_boroughs

gmap_zones <- gmap_base + 
  geom_polygon(data = df_sp, 
               aes(x = long, y = lat, group = group), 
               colour = "orange", # df_sp_borough$borough, 
               alpha = 0.4, 
               fill = df_sp_borough$color) + 
  geom_text(data = df_zone, 
            aes(x = long, y = lat, label = id), 
            size = 3, 
            colour = "darkgreen")
gmap_zones

################ plot map test ################
# plot pickup points on map
gmap_dots <- 
  gmap_base + 
  geom_point(aes(x = pickup_longitude, y = pickup_latitude), 
             data = yellow_june_cleaned, 
             colour = "black", 
             size = 0.8)
gmap_dots # test run

# plot district bg color & bounding box
gmap_dist <- 
  gmap_dots + 
  geom_polygon(data = df_sp, 
               aes(x = long, y = lat, group = group), 
               colour = "orange", # df_sp_borough$borough, 
               alpha = 0.4, 
               fill = df_sp_borough$color)
# scale_fill_gradient(low="blue", high="red")
# scale_color_gradientn(colours = rainbow(5))
gmap_dist # test run

# add borougn name
gmap_dist_title <- 
  gmap_dist + 
  geom_text(data = df_borough, 
            aes(x = long, y = lat, label = borough), 
            size = 10, 
            colour = "darkgreen")
# gmap_dist_title # test run

################ plot by different cutting time ################
# cutting time: type 1
sel_month <- c(6)
sel_day <- c(1:5)
sel_hour <- c(7, 8)

yellow_cleaned_type1 <- yellow_june_cleaned %>% 
  filter(pickup_month %in% sel_month, 
         pickup_day %in% sel_day, 
         pickup_hour %in% sel_hour)

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

# cutting time: type 2
sel_isWeekend <- c(FALSE)
sel_weekday <- c("mon", "tue")
sel_timeInDay <- c("work_midnight", "work_on_work")

yellow_cleaned_type2 <- yellow_june_cleaned %>% 
  filter(isWeekend %in% sel_isWeekend, 
         pickup_weekday %in% sel_weekday, 
         if(sel_isWeekend == TRUE){
           weekendTime
         }
         else{
           workTime
         } %in% sel_timeInDay)

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

