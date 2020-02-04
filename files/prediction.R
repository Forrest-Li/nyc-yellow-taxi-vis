setwd("D:/finalProjectData/RShiny/NYCTaxiVis")

# library(e1071)
# library(randomForest)
# install.packages("Metrics")
library(Metrics)
predict_set <- yellow_june_cleaned[, c(4:10, 13:14, 26)]
predict_set <- apply(predict_set, 2, as.numeric)
# predict_set <- apply(predict_set, 2, normalize)
predict_set <- as.data.frame(predict_set)

random <- sample(1:nrow(predict_set), 0.8*nrow(predict_set))
train <- predict_set[random, ]
test <- predict_set[-random, ]
mod <- lm(total_amount ~ ., train)
summary(mod)

predict_vec <- mod$coefficients[1] + test$trip_distance * mod$coefficients[2] + as.numeric(test$travel_time_sec) * mod$coefficients[3]
actual_vec <- test$total_amount
  mse(actual_vec, predict_vec)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# result_vec <- mod$coefficients[1] + test$trip_distance * mod$coefficients[2] + as.numeric(test$travel_time_sec) * mod$coefficients[3]
# result_diff <- result_vec - test$total_amount
# result_res <- c()
# # max_val <- max(test$total_amount)*0.5
# for (i in result_diff){
#   if(abs(result_diff) <= 0.3)
#     result_res <- c(result_res, 1)
#   else
#     result_res <- c(result_res, 0)
# }
# table(result_res)
# sum(result_res == 1) / length(result_res)
# # confusionMatrix(as.numeric(result_vec), reference = test$total_amount)

########### Cut dataset ########### 
June <- sample(c(1:nrow(yellow_june)), 60000)
July <- sample(c(1:nrow(yellow_july)), 60000)
June <- yellow_june[June, ]
July <- yellow_july[July, ]
june_july <- rbind(June, July)
write.csv(june_july, "sample2.csv", row.names = F)

