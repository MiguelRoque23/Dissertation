# Decision trees

set.seed(155)
train_kfold <- trainControl(method = 'cv', number = 5)
grid <- expand.grid(maxdepth = (2:30))

tree_d1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_d1)

tree_d2 <- train(crimes ~ factor(year) + factor(month) + black + latino + pop + hsol + nilf + male + log(area_kmsq) + poverty, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_d2)

tree_log_d1 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_log_d1)

rmse(tree_log_d1$trainingData$crimes, exp(predict(tree_log_d1, tree_log_d1$trainingData)))
mae(tree_log_d1$trainingData$crimes, exp(predict(tree_log_d1, tree_log_d1$trainingData)))

tree_log_d2 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + log(area_kmsq) + poverty, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_log_d2)

rmse(tree_log_d2$trainingData$crimes, exp(predict(tree_log_d2, tree_log_d2$trainingData)))
mae(tree_log_d2$trainingData$crimes, exp(predict(tree_log_d2, tree_log_d2$trainingData)))

tree_u1 <- train(crimes ~ factor(year) + factor(month) + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_u1)

tree_u2 <- train(crimes ~ factor(year) + factor(month) + food + university + school + library + parking_lot + bank + social_center + club + law_enforcement + church + cemetery + indoor_sport + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_u2)

tree_g1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_g1)

tree_g2 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + pop + hsol + pop_density + poverty + food + university + school + library + parking_lot + bank + hospital + pharmacy + social_center + club + law_enforcement + church + cemetery + park + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + electronics + car_related_shops + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_g2)


# Gradient boosting

xgb_d1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'xgbTree', 
               trControl = train_kfold)

print(xgb_d1)

xgb_d2 <- train(crimes ~ factor(year) + factor(month) + black + latino + pop + hsol + nilf + male + log(area_kmsq) + poverty, 
               data = data, 
               method = 'xgbTree', 
               trControl = train_kfold)

print(xgb_d2)

xgb_u1 <- train(crimes ~ factor(year) + factor(month) + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'xgbTree', 
               trControl = train_kfold)

print(xgb_u1)

xgb_u2 <- train(crimes ~ factor(year) + factor(month) + food + university + school + library + parking_lot + bank + social_center + club + law_enforcement + church + cemetery + indoor_sport + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'xgbTree', 
               trControl = train_kfold)

print(xgb_u2)

xgb_g1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'xgbTree', 
               trControl = train_kfold)

print(xgb_g1)

xgb_g2 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + pop + hsol + pop_density + poverty + food + university + school + library + parking_lot + bank + hospital + pharmacy + social_center + club + law_enforcement + church + cemetery + park + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + electronics + car_related_shops + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'xgbTree', 
               trControl = train_kfold)

print(xgb_g2)


# Random forests

rf_d1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'ranger', 
               trControl = train_kfold)

print(rf_d1)

rf_d2 <- train(crimes ~ factor(year) + factor(month) + black + latino + pop + hsol + nilf + male + log(area_kmsq) + poverty, 
               data = data, 
               method = 'ranger', 
               trControl = train_kfold)

print(rf_d2)

rf_u1 <- train(crimes ~ factor(year) + factor(month) + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'ranger', 
               trControl = train_kfold)

print(rf_u1)

rf_u2 <- train(crimes ~ factor(year) + factor(month) + food + university + school + library + parking_lot + bank + social_center + club + law_enforcement + church + cemetery + indoor_sport + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'ranger', 
               trControl = train_kfold)

print(rf_u2)

rf_g1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'ranger', 
               trControl = train_kfold)

print(rf_g1)

rf_g2 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + pop + hsol + pop_density + poverty + food + university + school + library + parking_lot + bank + hospital + pharmacy + social_center + club + law_enforcement + church + cemetery + park + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + electronics + car_related_shops + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'ranger', 
               trControl = train_kfold)

print(rf_g2)
