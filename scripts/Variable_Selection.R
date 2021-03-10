library(tidyverse)

data <- read.csv(file = "./data/all_data.csv", stringsAsFactors = FALSE)

na_counts <- data %>%
  filter(!(Country %in% c("HKG", "CHN", "RUS", "ISL", "ISR", "TUR"))) %>%
  select(-extreme_poverty, -new_tests_smoothed_per_thousand, -rural_pop, -urban_pop, -suburban_pop, -unemployment,
         -one_yr_unemp_bene, -two_mth_umemp_bene, -two_yr_unemp_bene, -six_mth_unemp_bene, -five_yr_unemp_bene,
         -BCI, -CLI, -CCI)%>%
  group_by(Country) %>%
  summarise_all(funs(sum(!is.na(.)))) 

na_counts$num_points<-apply(na_counts,1,FUN=min)
sum(as.numeric(na_counts$num_points))

colnames(na_counts)