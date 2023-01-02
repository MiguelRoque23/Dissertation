# Theoretical examples

x1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
x2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
y1 <- c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11)
y2 <- c(2, 1, 4, 3, 12, 10, 11, 10, 2, 4, 3, 4)

sim_data_1 <- data.frame(x1, y1)
sim_data_2 <- data.frame(x2, y2)

ggplot(data = sim_data_1, aes(x = x1, y = y1)) + 
  geom_point(size = 3, colour = 'dodgerblue3') + 
  labs(title = 'Figure 20: Relation between x and y', x = 'x', y = 'y') + 
  stat_smooth(method = lm, colour = 'black', se = F) + 
  scale_x_continuous(breaks = seq(0, 12, 3)) + 
  scale_y_continuous(breaks = seq(0, 12, 4)) + 
  theme_few()

ggplot(data = sim_data_2, aes(x = x2, y = y2)) + 
  geom_point(size = 3, colour = 'dodgerblue3') + 
  labs(title = 'Figure 21: Relation between x and y', x = 'x', y = 'y') + 
  stat_smooth(method = lm, colour = 'black', se = F) + 
  scale_x_continuous(breaks = seq(0, 12, 3)) + 
  scale_y_continuous(breaks = seq(0, 12, 4)) + 
  theme_few()


# Feature selection

set.seed(155)

train_kfold <- trainControl(method = 'repeatedcv', number = 5, repeats = 1)

lm_d1 <- train(crimes ~ factor(year) + factor(month) + white + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_d2 <- train(crimes ~ factor(year) + factor(month) + white + latino + asian + age + pop + nilf + male + log(area_kmsq) + pop_density + income, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_d3 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_d4 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + nilf + male + log(area_kmsq) + pop_density + income, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_log_d1 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + white + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_log_d2 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + white + latino + asian + age + pop + nilf + male + log(area_kmsq) + pop_density + income, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_log_d3 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_log_d4 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + nilf + male + log(area_kmsq) + pop_density + income, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

rmse(lm_log_d4$trainingData$crimes, exp(lm_log_d4$finalModel$fitted.values))
mae(lm_log_d4$trainingData$crimes, exp(lm_log_d4$finalModel$fitted.values))

lm_u <- train(crimes ~ factor(year) + factor(month) + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_log_u <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

rmse(lm_log_u$trainingData$crimes, exp(lm_log_u$finalModel$fitted.values))
mae(lm_log_u$trainingData$crimes, exp(lm_log_u$finalModel$fitted.values))

lm_g <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

lm_log_g <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
            data = data, 
            method = 'lm', 
            trControl = train_kfold)

rmse(lm_log_g$trainingData$crimes, exp(lm_log_g$finalModel$fitted.values))
mae(lm_log_g$trainingData$crimes, exp(lm_log_g$finalModel$fitted.values))


# Linear regressions

set.seed(155)

train_kfold <- trainControl(method = 'repeatedcv', number = 5, repeats = 1)

model_d1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_d1)

summary(lm_d3)

model_d2 <- train(crimes ~ factor(year) + factor(month) + black + latino + pop + hsol + nilf + male + log(area_kmsq) + poverty, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_d2)

model_log_d1 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_log_d1)

rmse(model_log_d1$trainingData$crimes, exp(model_log_d1$finalModel$fitted.values))
mae(model_log_d1$trainingData$crimes, exp(model_log_d1$finalModel$fitted.values))

summary(lm_log_d3)

model_log_d2 <- train(log(crimes + 0.1) ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + log(area_kmsq) + poverty, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_log_d2)

rmse(model_log_d2$trainingData$crimes, exp(model_log_d2$finalModel$fitted.values))
mae(model_log_d2$trainingData$crimes, exp(model_log_d2$finalModel$fitted.values))

model_u1 <- train(crimes ~ factor(year) + factor(month) + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_u1)

summary(lm_u1)

model_u2 <- train(crimes ~ factor(year) + factor(month) + food + university + school + library + parking_lot + bank + social_center + club + law_enforcement + church + cemetery + indoor_sport + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_u2)

model_g1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty + airport + bar + food + university + school + library + parking_lot + bank + hospital + healthcare_professional + pharmacy + social_center + theater + club + law_enforcement + fire_station + church + industrial + military + cemetery + park + indoor_sport + outdoor_sport + golf + office + station + liquor_store + food_and_beverages + mall + clothing + health_and_beauty + DIY + furniture + electronics + car_related_shops + art_and_hobbies + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_g1)

summary(lm_g1)

model_g2 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + pop + hsol + pop_density + poverty + food + university + school + library + parking_lot + bank + hospital + pharmacy + social_center + club + law_enforcement + church + cemetery + park + station + food_and_beverages + mall + clothing + health_and_beauty + DIY + electronics + car_related_shops + books_and_gifts + tourist_attraction + tourist_accommodation, 
               data = data, 
               method = 'lm', 
               trControl = train_kfold)

print(model_g2)
