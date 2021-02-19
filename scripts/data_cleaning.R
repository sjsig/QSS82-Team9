library("rio")
library(tidyverse)
library(stringr)
library(plyr)
library(readr)




# You will have to change this depending on where youre working directory is
setwd('/Users/joe/Documents/Joe 2020-2021/21W/QSS Project/QSS82-Team9')

# Loads all file names into file array
# If a file is not .csv, it converts it
all_files <- list.files("./data/")
files <- c()
for (file in all_files){
  if(str_detect(file, ".csv")){
    files <- c(files, file)
  } else{
    base_name <- str_split(file, "[.]")[[1]][1]
    extension <- str_split(file, "[.]")[[1]][2]
    csv_name <- paste(base_name, ".csv", sep="")
    if(!(csv_name %in% all_files) && extension %in% c("xlsx","xls")){
      print("converting")
      convert(paste("./data/",file, sep=""), paste("./data/",csv_name, sep=""))
      files <- c(files, csv_name)
    }
  }
}



# Country data

# Title of column holding country name
country_columns <- list(
  "OECD_BCI" = "LOCATION",
  "healthcare-access-and-quality-index" = "Code",
  "google-trends-mobility-data" = "Code",
  "lockdown_and_covid_rate_data" = "iso_code",
  "OECD_Quarterly_GDP" = "LOCATION",
  "OECD_Household_Spending" = "LOCATION",
  "OECD_CCI" = "LOCATION",
  "OECD_CLI" = "LOCATION",
  "OECD_Elderly_Population" = "LOCATION",
  "OECD_Financial_Disincentives_To_Work" = "LOCATION",
  "Stock_Canada_GSPTSE" = "Country",
  "Stock_France_FCHI" = "Country",
  "Stock_Germany_GDAXI" = "Country",
  "Stock_Italy_FTSEMIBMI" = "Country",
  "Stock_Japan_N225" = "Country",
  "Stock_UK_FTAS" = "Country",
  "Stock_US_GSPC" = "Country",
  "OECD_Unemployment" = "LOCATION",
  "OECD_Unemployment_Benefits" = "LOCATION",
  "OECD_Trust_In_Government" = "LOCATION",
  "OECD_Poverty_Rates" = "LOCATION",
  "OECD_Population_Distribution" = "LOCATION",
  "OECD_Health_Spending_As_Percent_GDP" = "LOCATION"
)
# Title of column holding the date
time_columns <- list(
  "OECD_BCI" = "TIME",
  "healthcare-access-and-quality-index" = "Year",
  "google-trends-mobility-data" = "Date",
  "lockdown_and_covid_rate_data" = "date",
  "OECD_Quarterly_GDP" = "TIME",
  "OECD_Household_Spending" = "TIME",
  "OECD_CCI" = "TIME",
  "OECD_CLI" = "TIME",
  "OECD_Elderly_Population" = "TIME",
  "OECD_Financial_Disincentives_To_Work" = "TIME",
  "Stock_Canada_GSPTSE" = "Date",
  "Stock_France_FCHI" = "Date",
  "Stock_Germany_GDAXI" = "Date",
  "Stock_Italy_FTSEMIBMI" = "Date",
  "Stock_Japan_N225" = "Date",
  "Stock_UK_FTAS" = "Date",
  "Stock_US_GSPC" = "Date",
  "OECD_Unemployment" = "TIME",
  "OECD_Unemployment_Benefits" = "TIME",
  "OECD_Trust_In_Government" = "TIME",
  "OECD_Poverty_Rates" = "TIME",
  "OECD_Population_Distribution" = "TIME",
  "OECD_Health_Spending_As_Percent_GDP" = "TIME"
)
# Format of the date column
time_formats <- list(
  "OECD_BCI" = "yyyy-mm",
  "healthcare-access-and-quality-index" = "yyyy",
  "google-trends-mobility-data" = "yyyy-mm-dd",
  "lockdown_and_covid_rate_data" = "yyyy-mm-dd",
  "OECD_Quarterly_GDP" = "yyyy-Qq",
  "OECD_Household_Spending" = "yyyy",
  "OECD_CCI" = "yyyy-mm",
  "OECD_CLI" = "yyyy-mm",
  "OECD_Elderly_Population" = "yyyy",
  "OECD_Financial_Disincentives_To_Work" = "yyyy",
  "Stock_Canada_GSPTSE" = "mm/dd/yy",
  "Stock_France_FCHI" = "mm/dd/yy",
  "Stock_Germany_GDAXI" = "mm/dd/yy",
  "Stock_Italy_FTSEMIBMI" = "mm/dd/yy",
  "Stock_Japan_N225" = "mm/dd/yy",
  "Stock_UK_FTAS" = "mm/dd/yy",
  "Stock_US_GSPC" = "mm/dd/yy",
  "OECD_Unemployment" = "yyyy-mm",
  "OECD_Unemployment_Benefits" = "yyyy",
  "OECD_Trust_In_Government" = "yyyy",
  "OECD_Poverty_Rates" = "yyyy",
  "OECD_Population_Distribution" = "yyyy",
  "OECD_Health_Spending_As_Percent_GDP" = "yyyy"
  
)
# List of variables from dataset that we want to use
value_variables <- list(
  "OECD_BCI" = "Value",
  "healthcare-access-and-quality-index" = "HAQ Index (IHME (2017))",
  "google-trends-mobility-data" = c("retail_and_recreation",	"grocery_and_pharmacy",	"parks",	"transit_stations",	"workplaces",	"residential"),
  "lockdown_and_covid_rate_data" = c("total_cases",	"new_cases",	"new_cases_smoothed",	"total_deaths",	"new_deaths",	"new_deaths_smoothed",	"total_cases_per_million",	"new_cases_per_million",	"new_cases_smoothed_per_million",	"total_deaths_per_million",	"new_deaths_per_million",	"new_deaths_smoothed_per_million",	"reproduction_rate",	"icu_patients",	"icu_patients_per_million",	"hosp_patients",	"hosp_patients_per_million",	"weekly_icu_admissions",	"weekly_icu_admissions_per_million",	"weekly_hosp_admissions",	"weekly_hosp_admissions_per_million",	"new_tests",	"total_tests",	"total_tests_per_thousand",	"new_tests_per_thousand",	"new_tests_smoothed",	"new_tests_smoothed_per_thousand",	"positive_rate",	"tests_per_case",	"tests_units",	"total_vaccinations",	"people_vaccinated",	"people_fully_vaccinated",	"new_vaccinations",	"new_vaccinations_smoothed",	"total_vaccinations_per_hundred",	"people_vaccinated_per_hundred",	"people_fully_vaccinated_per_hundred",	"new_vaccinations_smoothed_per_million",	"stringency_index",	"population",	"population_density",	"median_age",	"aged_65_older",	"aged_70_older",	"gdp_per_capita",	"extreme_poverty",	"cardiovasc_death_rate",	"diabetes_prevalence",	"female_smokers",	"male_smokers",	"handwashing_facilities",	"hospital_beds_per_thousand",	"life_expectancy",	"human_development_index"),
  "OECD_Quarterly_GDP" = "Value",
  "OECD_Household_Spending" = c("SUBJECT","Value"),
  "OECD_CCI" = "Value",
  "OECD_CLI" = "Value",
  "OECD_Elderly_Population" = "Value",
  "OECD_Financial_Disincentives_To_Work" = c("SUBJECT", "Value"),
  "Stock_Canada_GSPTSE" = c("Open","High","Low","Close"),
  "Stock_France_FCHI" = c("Open","High","Low","Close"),
  "Stock_Germany_GDAXI" = c("Open","High","Low","Close"),
  "Stock_Italy_FTSEMIBMI" = c("Open","High","Low","Close"),
  "Stock_Japan_N225" = c("Open","High","Low","Close"),
  "Stock_UK_FTAS" = c("Open","High","Low","Close"),
  "Stock_US_GSPC" = c("Open","High","Low","Close"),
  "OECD_Unemployment" = "Value",
  "OECD_Unemployment_Benefits" = c("SUBJECT","Value"),
  "OECD_Trust_In_Government" = "Value",
  "OECD_Poverty_Rates" = c("SUBJECT", "Value"),
  "OECD_Population_Distribution" = c("SUBJECT", "Value"),
  "OECD_Health_Spending_As_Percent_GDP" = c("SUBJECT", "Value")
)
# Proper name of value variables (1-1 correspondence with value_variables list)
value_names <- list(
  "OECD_BCI" = "BCI",
  "healthcare-access-and-quality-index" = "HAQ_index",
  "google-trends-mobility-data" = c("retail_and_recreation",	"grocery_and_pharmacy",	"parks",	"transit_stations",	"workplaces",	"residential"),
  "lockdown_and_covid_rate_data" = c("total_cases",	"new_cases",	"new_cases_smoothed",	"total_deaths",	"new_deaths",	"new_deaths_smoothed",	"total_cases_per_million",	"new_cases_per_million",	"new_cases_smoothed_per_million",	"total_deaths_per_million",	"new_deaths_per_million",	"new_deaths_smoothed_per_million",	"reproduction_rate",	"icu_patients",	"icu_patients_per_million",	"hosp_patients",	"hosp_patients_per_million",	"weekly_icu_admissions",	"weekly_icu_admissions_per_million",	"weekly_hosp_admissions",	"weekly_hosp_admissions_per_million",	"new_tests",	"total_tests",	"total_tests_per_thousand",	"new_tests_per_thousand",	"new_tests_smoothed",	"new_tests_smoothed_per_thousand",	"positive_rate",	"tests_per_case",	"tests_units",	"total_vaccinations",	"people_vaccinated",	"people_fully_vaccinated",	"new_vaccinations",	"new_vaccinations_smoothed",	"total_vaccinations_per_hundred",	"people_vaccinated_per_hundred",	"people_fully_vaccinated_per_hundred",	"new_vaccinations_smoothed_per_million",	"stringency_index",	"population",	"population_density",	"median_age",	"aged_65_older",	"aged_70_older",	"gdp_per_capita",	"extreme_poverty",	"cardiovasc_death_rate",	"diabetes_prevalence",	"female_smokers",	"male_smokers",	"handwashing_facilities",	"hospital_beds_per_thousand",	"life_expectancy",	"human_development_index"),
  "OECD_Quarterly_GDP" = "quarterly_GDP",
  "OECD_Household_Spending" = c("household_spending_type","household_spending_for_type"),
  "OECD_CCI" = "CCI",
  "OECD_CLI" = "CLI",
  "OECD_Elderly_Population" = "elderly_population",
  "OECD_Financial_Disincentives_To_Work" = c("financial_disincentive_type", "financial_disincentives_to_work_for_type"),
  "Stock_Canada_GSPTSE" = c("Open","High","Low","Close"),
  "Stock_France_FCHI" = c("Open","High","Low","Close"),
  "Stock_Germany_GDAXI" = c("Open","High","Low","Close"),
  "Stock_Italy_FTSEMIBMI" = c("Open","High","Low","Close"),
  "Stock_Japan_N225" = c("Open","High","Low","Close"),
  "Stock_UK_FTAS" = c("Open","High","Low","Close"),
  "Stock_US_GSPC" = c("Open","High","Low","Close"),
  "OECD_Unemployment" = "unemployment",
  "OECD_Unemployment_Benefits" = c("unemployment_benefit_type", "unemployment_benefits_for_type"),
  "OECD_Trust_In_Government" = "trust_in_gov",
  "OECD_Poverty_Rates" = c("poverty_type", "poverty_rate_for_type"),
  "OECD_Population_Distribution" = c("pop_area", "pop_distribution_for_area"),
  "OECD_Health_Spending_As_Percent_GDP" = c("health_spending_type", "health_spending_for_type")
)

df <- data.frame(Country = character(), Date = character(), stringsAsFactors = TRUE)

for(file in files){
  base_name <- str_split(file, "[.]")[[1]][1]
  if(!(base_name %in% names(country_columns))){
    # print(paste("You did not add the country column name for", file))
  } else if(!(base_name %in% names(time_columns))){
    # print(paste("You did not add the time column name for", file))
  } else if(!(base_name %in% names(time_formats))){
    # print(paste("You did not add the time format for", file))
  } else if(!(base_name %in% names(value_variables))){
    # print(paste("You did not add the value column name(s) for", file))
  } else {
    print("reading")
    data <- read.csv(file = paste("./data/", file, sep=""))
    colnames(data)[which(names(data) == country_columns[base_name])] <- "Country"
    colnames(data)[which(names(data) == time_columns[base_name])] <- "Date"
    time_format <- time_formats[base_name]
    if(time_format == "yyyy"){
      data$Date <- as.character(as.Date(paste(data$Date,"-01-01",sep=""), format = "%Y-%m-%d"))
    } else if(time_format == "yyyy-mm"){
      data$Date <- as.character(as.Date(paste(data$Date,"-01",sep=""), format = "%Y-%m-%d"))
    }
    else if(time_format == "yyyy-Qq"){
      # data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
    }
    
    
    variables = value_variables[base_name]
    variable_names = value_names[base_name]
    if(typeof(variables)=="list"){
      variables <- unlist(value_variables[base_name], use.names=FALSE)
      variable_names <- unlist(value_names[base_name], use.names=FALSE)
    }
    variables <- make.names(variables)
    data <- data[c("Country","Date",variables)]
    
    colnames(data) <- c("Country","Date",variable_names)
    
    print(head(data))
    df <- full_join(df, data, by = c("Country", "Date"), all = TRUE)
  }
}

write.csv(df,"./data/all_data.csv", row.names = FALSE)

unique(df$Country)

# Not yet dealt with: oil_price_data, G20_stimulus_data

# dates
# cleaning 
# stock data
# percentage change calculations

 
