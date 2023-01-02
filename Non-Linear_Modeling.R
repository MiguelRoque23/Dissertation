# Decision tree

grid <- expand.grid(maxdepth = (2:30))

tree_d1 <- train(crimes ~ factor(year) + factor(month) + black + latino + asian + age + pop + hsol + nilf + male + log(area_kmsq) + pop_density + poverty, 
               data = data, 
               method = 'rpart2', 
               trControl = train_kfold, 
               tuneGrid = grid)

print(tree_d1)

