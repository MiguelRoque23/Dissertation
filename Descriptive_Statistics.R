# Panel balance

data %>% 
  group_by(block) %>% 
  summarise(count = n()) %>% 
  summary()

data %>% 
  group_by(year.month) %>% 
  summarise(count = n()) %>% 
  summary()


# Correlation

corr_matrix <- avg_blocks %>% 
  select(crimes, 6:18) %>% 
  correlate(diagonal = 1)
  
corr_matrix %>% 
  shave() %>% 
  arrange(-crimes) %>%
  fashion(decimals = 3) %>% 
  kable()

corr_matrix2 <- avg_blocks %>% 
  select(crimes, 19:57) %>% 
  correlate(diagonal = 1)
  
corr_matrix2 %>% 
  shave() %>% 
  arrange(-crimes) %>%
  fashion(decimals = 3) %>% 
  kable()
  
ggplot(data = data, aes(x = poverty, y = crimes)) + 
  geom_point(size = 0.5, colour = 'dodgerblue3') + 
  labs(title = 'Figure 1: Relation between poverty rate and number of crimes', x = 'Poverty rate', y = 'Number of crimes') + 
  stat_smooth(method = lm, colour = 'black', se = F) + 
  theme_few()

ggplot(data = data, aes(x = station, y = crimes)) + 
  geom_point(size = 1, colour = 'dodgerblue3') + 
  labs(title = 'Figure 2: Relation between number of stations and number of crimes', x = 'Number of satations', y = 'Number of crimes') + 
  stat_smooth(method = lm, colour = 'black', se = F) + 
  theme_few()

ggplot(data = data, aes(x = white, y = black)) + 
  geom_point(size = 0.5, colour = 'dodgerblue3') + 
  labs(title = 'Figure 3: Relation between white rate and black rate', x = 'White rate', y = 'Black rate') + 
  stat_smooth(method = lm, colour = 'black', se = F) + 
  scale_x_continuous(breaks = seq(0, 1, 0.25)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25)) + 
  theme_few()

ggplot(data = data, aes(x = income, y = hsol)) + 
  geom_point(size = 0.5, colour = 'dodgerblue3') + 
  labs(title = 'Figure 4: Relation between average yearly income and low education rate', x = 'Average yearly income (USD)', y = 'High school or lower rate') +
  stat_smooth(method = lm, colour = 'black', se = F) + 
  scale_y_continuous(breaks = seq(0, 1, 0.25)) + 
  theme_few()

ggplot(data = data, aes(x = factor(month), y = crimes * 1336)) +
  stat_summary(fun.y = 'mean', geom = 'bar', fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 5: Average number of crimes per month', x = 'Month', y = 'Average number of crimes') +
  theme_few()

ggplot(data = data, aes(x = factor(year), y = crimes)) +
  stat_summary(fun.y = 'sum', geom = 'bar', fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 6: Number of crimes per year', x = 'Year', y = 'Number of crimes') +
  theme_few()
  
  
# Variables distributions

ggplot(data = data, aes(x = crimes)) +
  geom_histogram(boundary = 0, fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 7: Histogram of number of crimes', x = 'Number of crimes + 0.1', y = 'Count') +
  theme_few()

ggplot(data = data, aes(x = age)) +
  geom_histogram(boundary = 0, fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 11: Histogram of average age', x = 'Average age (years)', y = 'Count') +
  theme_few()

ggplot(data = data, aes(x = pop)) +
  geom_histogram(boundary = 0, fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 12: Histogram of population', x = 'Population', y = 'Count') +
  theme_few()

ggplot(data = data, aes(x = area_kmsq)) +
  geom_histogram(boundary = 0, fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 9: Histogram of area', x = 'Area (sq. kms)', y = 'Count') +
  theme_few()

ggplot(data = data, aes(x = pop_density)) +
  geom_histogram(boundary = 0, fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 13: Histogram of population density', x = 'Population density (per sq. km)', y = 'Count') +
  theme_few()

ggplot(data = data, aes(x = income)) +
  geom_histogram(boundary = 0, fill = 'dodgerblue3', color = 'black') +
  labs(title = 'Figure 14: Histogram of average yearly income', x = 'Average yearly income (USD)', y = 'Count') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

map_colors <- c(lighten('dodgerblue3', 0.8), 'dodgerblue3', darken('dodgerblue3', 0.8))

ggplot(data = data_sf, aes(fill = crimes)) + 
  geom_sf(color = 'black', size = 0.1) + 
  scale_fill_gradientn(colours = colorRampPalette(map_colors)(10), limits = c(0, 150)) + 
  labs(title = 'Figure 15: Monthly average number of crimes per block', fill = 'Number of crimes') + 
  theme_few() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

ggplot(data = data_sf, aes(fill = latino)) + 
  geom_sf(color = 'black', size = 0.1) + 
  scale_fill_gradientn(colours = colorRampPalette(map_colors)(10)) + 
  labs(title = 'Figure 19: Proportion of Hispanic residents per block', fill = 'Proportion') + 
  theme_few() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
