library(tidyverse)
library(dplyr)
data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

filtered_data <- data %>%
  dplyr::select(-extreme_poverty, -new_tests_smoothed_per_thousand, -rural_pop, -urban_pop, -suburban_pop, -unemployment,
         -one_yr_unemp_bene, -two_mth_umemp_bene, -two_yr_unemp_bene, -six_mth_unemp_bene, -five_yr_unemp_bene,
         -BCI, -CLI, -CCI)

na_counts <- filtered_data %>%
  group_by(Country) %>%
  summarise_all(funs(sum(!is.na(.)))) 

na_counts$num_points<-apply(na_counts,1,FUN=min)
sum(as.numeric(na_counts$num_points))

colnames(na_counts)

date_counts <- data %>%
  filter(!(Country %in% c("HKG", "CHN", "RUS", "ISL", "ISR", "TUR"))) %>%
  select(-extreme_poverty, -new_tests_smoothed_per_thousand, -rural_pop, -urban_pop, -suburban_pop, -unemployment,
         -one_yr_unemp_bene, -two_mth_umemp_bene, -two_yr_unemp_bene, -six_mth_unemp_bene, -five_yr_unemp_bene,
         -BCI, -CLI, -CCI)%>%
  filter(!is.na(stringency_ra)) %>% 
  group_by(Date) %>%
  summarise(count = n())

averages <- lapply(data, mean, na.rm = T)  



averages <- aggregate(filtered_data[, 5: 38], list(filtered_data$Country), mean, na.rm = TRUE)


boxplot(averages$stock_change)
