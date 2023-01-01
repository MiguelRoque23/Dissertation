path <- 'C:/Users/mfroque/OneDrive - KPMG/Documents/Dissertation/data/'
load(paste0(path, 'inclusion.list.Rdata'))
load(paste0(path, 'data_ready.Rdata'))
blocks_data <- read_sf(dsn = paste0(path, 'Census_Block_Groups_2010-shp'), 
                       layer = 'Census_Block_Groups_2010')

feats_data <- data.frame(inclusion.list[[1]]) %>% 
  mutate(year.month = names(inclusion.list)[1]) %>% 
  mutate(OBJECTID = row_number()) %>% 
  relocate(year.month, OBJECTID)

for (n in (2:length(inclusion.list))) {
  df <- data.frame(inclusion.list[[n]]) %>% 
    mutate(year.month = names(inclusion.list)[n]) %>% 
    mutate(OBJECTID = row_number()) %>% 
    relocate(year.month, OBJECTID)
  feats_data <- rbind(feats_data, df)
  rm(df) }

data <- merge(data.ready, feats_data, bY = c('OBJECTID', 'year.month'))

data <- data %>% 
  filter(year >= 2016) %>% 
  mutate(month = as.numeric(str_sub(year.month, 5, 6)), .after = year) %>% 
  mutate(pop_density = as.numeric(pop / area_kmsq), .after = area_kmsq) %>% 
  select(-month.seq, -longitude, -latitude) %>% 
  rename(block = OBJECTID, crimes = count)

data <- data %>% 
  mutate(white = ifelse(pop == 0 & block != 126 | block == 209, NA, white), 
         black = ifelse(pop == 0 & block != 126 | block == 209, NA, black), 
         asian = ifelse(pop == 0 & block != 126 | block == 209, NA, asian), 
         latino = ifelse(pop == 0 & block != 126 | block == 209, NA, latino), 
         nilf = ifelse(pop == 0 & block != 126 | block == 209, NA, nilf), 
         hsol = ifelse(pop == 0 & block != 126 | block == 209, NA, hsol), 
         age = ifelse(pop == 0 & block != 126 | block == 209, NA, age), 
         male = ifelse(pop == 0 & block != 126 | block == 209, NA, male), 
         poverty = ifelse(pop == 0 & block != 126 | block == 209, NA, poverty), 
         income = ifelse(pop == 0 & block != 126 | block == 209, NA, income))

avg_blocks <- data %>% 
  group_by(block) %>% 
  summarise(across(everything(), mean), .groups = 'drop') %>% 
  select(-year.month, -year, -month)

blocks_data <- blocks_data %>% 
  rename(block = OBJECTID)

data_sf <- merge(blocks_data, avg_blocks, bY = 'block')

avg_data <- data %>% 
  group_by(year) %>% 
  summarise(white = mean(white, na.rm = T), 
            black = mean(black, na.rm = T), 
            asian = mean(asian, na.rm = T), 
            latino = mean(latino, na.rm = T), 
            nilf = mean(nilf, na.rm = T), 
            hsol = mean(hsol, na.rm = T), 
            age = mean(age, na.rm = T), 
            male = mean(male, na.rm = T), 
            poverty = mean(male, na.rm = T), 
            income = mean(income, na.rm = T))

years <- c(2016, 2017, 2018, 2019)

for (y in (1:4)) {
  data <- data %>% 
    mutate(white = ifelse(is.na(white) & year == years[y], as.numeric(avg_data[y, 'white']), white), 
           black = ifelse(is.na(black) & year == years[y], as.numeric(avg_data[y, 'black']), black), 
           asian = ifelse(is.na(asian) & year == years[y], as.numeric(avg_data[y, 'asian']), asian), 
           latino = ifelse(is.na(latino) & year == years[y], as.numeric(avg_data[y, 'latino']), latino), 
           nilf = ifelse(is.na(nilf) & year == years[y], as.numeric(avg_data[y, 'nilf']), nilf), 
           hsol = ifelse(is.na(hsol) & year == years[y], as.numeric(avg_data[y, 'hsol']), hsol), 
           age = ifelse(is.na(age) & year == years[y], as.numeric(avg_data[y, 'age']), age), 
           male = ifelse(is.na(male) & year == years[y], as.numeric(avg_data[y, 'male']), male), 
           poverty = ifelse(is.na(poverty) & year == years[y], as.numeric(avg_data[y, 'poverty']), poverty), 
           income = ifelse(is.na(income) & year == years[y], as.numeric(avg_data[y, 'income']), income)) }

rm(n)
rm(y)
rm(inclusion.list)
rm(data.ready)
rm(feats_data)
rm(blocks_data)
rm(avg_data)
rm(years)
