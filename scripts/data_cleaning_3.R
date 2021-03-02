# Load Packages  ----------------------------------------------------------
library(tidyverse)
library(dplyr)


# Set working directory (if necessary) ------------------------------------

# You will have to change this depending on where youre working directory is
# setwd('/Users/joe/Documents/Joe 2020-2021/21W/QSS Project/QSS82-Team9')


# Load file names and convert if necessary --------------------------------

all_files <- list.files("./data/")
files <- c()
for (file in all_files){
  if(str_detect(file, ".csv")){
    files <- c(files, file)
  } else{
    base_name <- str_split(file, "[.]")[[1]][1]
    extension <- str_split(file, "[.]")[[1]][2]
    csv_name <- paste(base_name, ".csv", sep="")
    # If a file is not .csv, it converts it
    if(!(csv_name %in% all_files) && extension %in% c("xlsx","xls")){
      print("converting")
      convert(paste("./data/",file, sep=""), paste("./data/",csv_name, sep=""))
      files <- c(files, csv_name)
    }
  }
}


# Create stock data file -------------------------------------------------
stock_files <-files[sapply(files, function(x) grepl("_(S|s)tock", x))]
stock_data <- data.frame(stringsAsFactors = TRUE)

for(file in stock_files){
  country_data <- read.csv(file = paste("./data/", file, sep=""), stringsAsFactors = FALSE)
  # Add country column if not there
  
  if(!("Country" %in% colnames(country_data))){
    country_code <- str_split(file, "_")[[1]][1]
    country_data <- country_data %>% 
      mutate(Country = country_code)
  }
  stock_data <- rbind(stock_data, country_data)
}

stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")
stock_data$Adj.Close <- as.numeric(stock_data$Adj.Close)

stock_data <- stock_data %>%
  group_by(Country) %>%
  mutate(stock_change = 100* (Adj.Close/lag(Adj.Close) -1 ))

write.csv(stock_data,"./data/Stock_data.csv", row.names = FALSE)

# Remove stock files from list of 
files <- files[sapply(files, function(x) grepl("_(S|s)tock", x)) == FALSE]
files <- files[files != "all_data.csv" & files != "Stock_data.csv"]
files <- c(files, "Stock_data.csv")



# Initialize dataframe  ---------------------------------------------------

df <- data.frame(Country = character(), Date = character(), stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)



# Country list ------------------------------------------------------------

countries <- c("ARG","AUS", "AUT", "BEL", "BRA", "CAN", "CHE", "CHN", "DEU", "ESP", "FIN", "FRA", "GBR", "HKG", "IDN", "IND", "IRL", "ISL", "ISR", "ITA", "KOR", "LTU", "MEX", "NLD", "NOR", "NZL", "PER", "PRT", "RUS", "SGP", "SWE", "TUR", "USA", "ZAF")
# Our world in data -------------------------------------------------------

data <- read.csv(file = './data/lockdown_and_covid_rate_data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = iso_code, Date = date)%>%
  mutate(Date = as.Date(Date)) %>%
  filter(Country %in% countries) %>%
  select(Country, Date, stringency_index, aged_65_older, human_development_index, median_age, life_expectancy, population_density, extreme_poverty)

colnames(data)

df <- merge(x = df, y = data, by = c("Country", "Date"), all = TRUE)

df <- df %>%
  mutate(Day = format(Date, format = "%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y"))


# Stock data --------------------------------------------------------------

data <- read.csv(file = './data/Stock_data.csv', stringsAsFactors = FALSE)


data <- data %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Country, Date, stock_change)

df <- merge(x = df, y = data, by = c("Country", "Date"), all.x = TRUE)

df <- df %>%
  mutate(Day = format(Date, format = "%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y"))

# Mobility data -----------------------------------------------------------

data <- read.csv(file = './data/google-trends-mobility-data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = Code) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Country, Date, retail_and_recreation, residential)

df <- merge(x = df, y = data, by = c("Country", "Date"), all.x = TRUE)


# Population distribution -------------------------------------------------

data <- read.csv(file = './data/OECD_Population_Distribution.csv', stringsAsFactors = FALSE)

data <- data %>%
  spread(SUBJECT, Value) %>%
  dplyr::rename(Country = LOCATION, Date = TIME, rural_pop = RURAL, urban_pop = URBAN, suburban_pop = INTMD) %>%
  filter(Country %in% countries) %>%
  #mutate(Date = as.Date(Date, format = "%Y")) %>%
  select(Country, rural_pop, urban_pop, suburban_pop)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# BCI ---------------------------------------------------------------------

data <- read.csv(file = './data/OECD_BCI.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, BCI = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, BCI)



df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)


# CCI ---------------------------------------------------------------------

data <- read.csv(file = './data/OECD_CCI.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, CCI = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, CCI)

df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)

# CLI ---------------------------------------------------------------------

data <- read.csv(file = './data/OECD_CLI.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, CLI = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, CLI)



df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)



# Trust in government  ----------------------------------------------------

data <- read.csv(file = './data/OECD_Trust_In_Government.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, trust_in_gov = Value) %>%
  filter(Country %in% countries) %>%
  filter(TIME == 2019) %>%
  select(Country, trust_in_gov)


df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)




# Other economic factors - oil_prices, *price_volatility, *economic_composition
# Provided economic aid - financial_disincentive_type + financial_disincentives_to_work_for_type, unemployment_benefit_type + unemployment_benefits_for_type
# Political factors - trust_in_gov, *political_stability_index, *civil_liberties
#unemployment and gdp

# Stimulus data 