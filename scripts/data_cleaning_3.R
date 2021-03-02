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
  mutate(stock_change = Adj.Close/lag(Adj.Close) -1 )

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
  filter(Country %in% countries)


df <- merge(x = df, y = data, by = c("Country", "Date"), all = TRUE)


# Stock data --------------------------------------------------------------

data <- read.csv(file = './data/Stock_data.csv', stringsAsFactors = FALSE)

data <- data %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(Date))

df <- merge(x = df, y = data, by = c("Country", "Date"), all.x = TRUE)



