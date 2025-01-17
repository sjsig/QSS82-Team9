# File Description --------------------------------------------------------
## This file pulls in all of our data sources and compiles them into the working dataset we use in our analysis.


# Load Packages  ----------------------------------------------------------
library(tidyverse)
library(countrycode)
library(zoo)
library(dplyr)


# Set working directory (if necessary) ------------------------------------
## You will have to change this depending on where your working directory is.


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
    ## If a file is not .csv, it converts it
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
  ## Add country column if not there
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


## Remove stock files from list of data files
files <- files[sapply(files, function(x) grepl("_(S|s)tock", x)) == FALSE]
files <- files[files != "all_data.csv" & files != "Stock_data.csv"]
files <- c(files, "Stock_data.csv")


# Initialize full dataframe  ---------------------------------------------------

df <- data.frame(Country = character(), Date = character(), stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)


# Initialize country list ------------------------------------------------------------

countries <- c("AUS", "AUT", "BEL", "BRA", "CHE", "DEU", "ESP", "FIN", "FRA", "GBR", "IDN", "IND", "IRL", "ITA", "KOR", "LTU", "MEX", "NLD", "NOR", "NZL", "PRT","SWE", "USA", "ZAF")


# Pull Our World In Data -------------------------------------------------------

data <- read.csv(file = './data/lockdown_and_covid_rate_data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = iso_code, Date = date, covid_rate = total_cases_per_million)%>%
  mutate(Date = as.Date(Date, format="%m/%d/%y")) %>%
  filter(Country %in% countries) %>%
  select(Country, Date, stringency_index, aged_65_older, human_development_index, median_age, life_expectancy, population_density, extreme_poverty, covid_rate, gdp_per_capita, hospital_beds_per_thousand, new_vaccinations_smoothed_per_million, new_cases_smoothed_per_million, new_deaths_smoothed_per_million, new_tests_smoothed_per_thousand) %>%
  mutate(new_vaccinations_smoothed_per_million = as.numeric(new_vaccinations_smoothed_per_million)) %>%
  mutate(new_vaccinations_smoothed_per_million = if_else(is.na(new_vaccinations_smoothed_per_million), 0, new_vaccinations_smoothed_per_million)) %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(stringency_ra = zoo::rollmean(stringency_index, k = 7, fill = NA, align = "right")) %>%
  mutate(covid_log = log(covid_rate)) %>%
  arrange(Country, Date)

df <- merge(x = df, y = data, by = c("Country", "Date"), all = TRUE)

df <- df %>%
  mutate(Day = format(Date, format = "%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  mutate(Quarter = ifelse(as.integer(Month) %in% c(1,2,3),1, ifelse(as.integer(Month) %in% c(4,5,6),2,ifelse(as.integer(Month) %in% c(7,8,9),3,ifelse(as.integer(Month) %in% c(10,11,12),4,NA)))))


# Pull Stock data --------------------------------------------------------------

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


# Pull Google Mobility data -----------------------------------------------------------

data <- read.csv(file = './data/google-trends-mobility-data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = Code) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%y")) %>%
  select(Country, Date, retail_and_recreation, residential)

df <- merge(x = df, y = data, by = c("Country", "Date"), all.x = TRUE)


# Pull OECD population distribution data -------------------------------------------------

data <- read.csv(file = './data/OECD_Population_Distribution.csv', stringsAsFactors = FALSE)

data <- data %>%
  spread(SUBJECT, Value) %>%
  dplyr::rename(Country = LOCATION, Date = TIME, rural_pop = RURAL, urban_pop = URBAN, suburban_pop = INTMD) %>%
  filter(Country %in% countries) %>%
  select(Country, rural_pop, urban_pop, suburban_pop)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# Pull BCI data ---------------------------------------------------------------------

data <- read.csv(file = './data/OECD_BCI.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, BCI = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, BCI)



df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)


# Pull CCI data ---------------------------------------------------------------------

data <- read.csv(file = './data/OECD_CCI.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, CCI = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, CCI)

df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)


# Pull CLI data ---------------------------------------------------------------------

data <- read.csv(file = './data/OECD_CLI.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, CLI = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, CLI)



df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)


# Pull OECD unemployment benefits data ------------------------------------------------

data <- read.csv(file = './data/OECD_Unemployment_Benefits.csv', stringsAsFactors = FALSE)

data <- data %>%
  spread(SUBJECT, Value) %>%
  dplyr::rename(Country = LOCATION, Date = TIME, one_yr_unemp_bene = "1YEAR", two_mth_umemp_bene ="2MTH", two_yr_unemp_bene = "2YEAR", five_yr_unemp_bene = "5YEAR", six_mth_unemp_bene = "6MTH") %>%
  filter(Date == 2019) %>%
  filter(Country %in% countries) %>%
  select(Country, one_yr_unemp_bene, two_mth_umemp_bene, two_yr_unemp_bene, five_yr_unemp_bene, six_mth_unemp_bene )

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# Pull WTI oil price data --------------------------------------------------------------

data <- read.csv(file = './data/oil_price_data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(oil_price = WTI_spot_price) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, oil_price)

df <- merge(x = df, y = data, by = c("Date"), all.x = TRUE)


# Pull OECD unemployment data  -----------------------------------------------------------

data <- read.csv(file = './data/OECD_Unemployment.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME, unemployment = Value) %>%
  filter(Country %in% countries) %>%
  mutate(Date = as.Date(paste(Date,"-01",sep=""), format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, format = "%m")) %>%
  mutate(Year = format(Date, format = "%Y")) %>%
  select(Country, Month, Year, unemployment)

df <- merge(x = df, y = data, by = c("Country", "Month", "Year"), all.x = TRUE)


# Pull OECD healthcare spending data -----------------------------------------------------------

data <- read.csv(file = './data/OECD_Health_Spending_As_Percent_GDP.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = LOCATION, Date = TIME) %>%
  filter(Country %in% countries) %>%
  filter(SUBJECT == "TOT") %>%
  group_by(Country) %>%
  arrange(Date) %>%
  filter(row_number() == n()) %>%
  mutate(health_spending_pct_gdp =  Value) %>%
  select(Country, health_spending_pct_gdp)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# Pull IMF stimulus data -----------------------------------------------------------

data <- read.csv(file = './data/IMF_Stimulus_Data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(stimulus_spending_pct_gdp = Total.Stimulus.Spending.Percent.GDP, liquidity_support_pct_gdp = Total.Liquidity.Support.Percent.GDP, health_stimulus_spending_pct_gdp = Total.Stimulus.Spending.in.Health.Sector.Percent.GDP) %>%
  filter(Country %in% countries) %>%
  select(Country,stimulus_spending_pct_gdp , liquidity_support_pct_gdp, health_stimulus_spending_pct_gdp)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)

write.csv(df,"./data/all_data.csv", row.names = FALSE)


# Pull Economic composition data ----------------------------------------------------

data <- read.csv(file = './data/Econ_Composition.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = X) %>%
  mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))%>%
  filter(Country %in% countries) %>%
  mutate(agriculture = as.numeric(agriculture)) %>%
  mutate(industry = as.numeric(industry)) %>%
  mutate(manufacturing = as.numeric(manufacturing)) %>%
  mutate(services = as.numeric(services)) %>%
  select(Country, agriculture, industry, manufacturing, services)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# Pull DPI political factors data -------------------------------------------------------

data <- read.csv(file = './data/dpi_political_variables.csv', stringsAsFactors = FALSE)

data <- data %>%
  filter(Country %in% countries) %>%
  select(Country, maj_DPI, frac_DPI)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)

df_2 <- df %>%
  filter(is.na(stock_change)) %>%
  arrange(Country, Date)

# Pull polity score data ------------------------------------------------------------


data <- read.csv(file = './data/polity_score_data.csv', stringsAsFactors = FALSE)

data <- data %>%
  filter(ID_year == 2019) %>%
  dplyr::rename(Country = ID_country_name, polity = fundamental_rights) %>%
  mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))%>%
  select(Country, polity)

df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# Pull population data--------------------------------------------------------------

data <- read.csv(file = './data/population_data.csv', stringsAsFactors = FALSE)

data <- data %>%
  dplyr::rename(Country = Country.Code, population = Population) %>%
  mutate(population_millions = population/1000000) %>%
  dplyr::select(Country, population_millions)


df <- merge(x = df, y = data, by = c("Country"), all.x = TRUE)


# Re-calculate select variables to fix scaling issues --------------------------------------------------------

scaled_df <- df %>%
  mutate(human_development_index = human_development_index * 100,
         frac_DPI = frac_DPI * 100, # change scale from 0-1 to 0-100
         maj_DPI = maj_DPI * 100,
         polity = polity * 100,
         population_density_log = log(population_density)) # fix skewed distribution 


# Save dataframe ----------------------------------------------------------

write.csv(scaled_df,"./data/all_data.csv", row.names = FALSE)
