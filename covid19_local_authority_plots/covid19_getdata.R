# Functions to download data on Coronavirus cases in the UK by country, 
# region, and in England by local authority from the PHE dashboard API.
#
# The dashboard is here:
# https://coronavirus.data.gov.uk
# 
# The download process is based on the code here:
# https://github.com/PublicHealthEngland/coronavirus-dashboard/blob/master/src/hooks/useLoadData.js
#
# Run ...
#
# download_data()
# 
# ... to download the data as CSVs into the current directory.

# Imports --------------------------------------------------------------------

library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(purrr)
library(readr)
library(rvest)
library(tidyr)

# Constants ------------------------------------------------------------------

COUNTRIES_URL <- "https://c19downloads.azureedge.net/downloads/data/countries_latest.json"
REGIONS_URL <- "https://c19downloads.azureedge.net/downloads/data/regions_latest.json"
UTLAS_URL <- "https://c19downloads.azureedge.net/downloads/data/utlas_latest.json"
MIN_DATE_CASES <- as.Date("2020-01-30")
MIN_DATE_DAILY_DEATHS <- as.Date("2020-03-28")
MIN_DATE_CUMULATIVE_DEATHS <- as.Date("2020-03-27")

# Functions ------------------------------------------------------------------

fetch_datafile <- function(datafile_url) {
  
  # Get the json at the datafile url and parse it
  response <- GET(datafile_url)
  text <- content(response, "text", encoding = "UTF-8")
  fromJSON(text)
}

get_data_for_geography <- function(
  json, 
  value_type,
  value_name,
  geography, 
  min_date,
  fill_missing = TRUE) {
  
  # For a given geography, get the data of the given value type for each area
  map2_dfr(names(json), json, function(code, data) {
    
    # If the value type is missing for this json key, skip.
    if (! value_type %in% names(data)) return(tibble())
    
    # Otherwise set up the tibble
    df <- as_tibble(data[[value_type]]) %>% 
      mutate(
        date = as.Date(date),
        area_type = geography,
        area_code = code,
        area_name = data$name$value,
        value_name = value_name,
        number = value) %>% 
      select(
        date, 
        area_type, 
        area_code, 
        area_name, 
        value_name, 
        number) %>% 
      arrange(date, area_name)
    
    # Optionally insert rows for missing dates
    if (fill_missing) {
      
      max_date <- max(df$date)
      dates <- seq(min_date, max_date, by = "days")
      
      date_spine <- tibble(
        date = dates,
        area_type = geography,
        area_code = code,
        area_name = data$name$value,
        value_name = value_name)
      
      merged <- date_spine %>% 
        left_join(
          df %>% select(date, number), 
          by = "date")
      
      fill_types <- c(
        "dailyTotalConfirmedCases", 
        "dailyTotalDeaths")
      
      if (value_type %in% fill_types) {
        merged <- merged %>% fill(number)
      } 
      
      df <- merged %>% replace_na(list(number = 0))
    }
    
    df
  })
}

# Daily cases data -----------------------------------------------------------

get_daily_cases_country <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(COUNTRIES_URL), 
    "dailyConfirmedCases", 
    "cases",
    "country",
    MIN_DATE_CASES,
    fill_missing)
}

get_daily_cases_region <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(REGIONS_URL),
    "dailyConfirmedCases",
    "cases",
    "region",
    MIN_DATE_CASES,
    fill_missing)
}

get_daily_cases_utla <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(UTLAS_URL),
    "dailyConfirmedCases",
    "cases",
    "utla",
    MIN_DATE_CASES,
    fill_missing)
}

# Cumulative cases data ------------------------------------------------------

get_cumulative_cases_country <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(COUNTRIES_URL), 
    "dailyTotalConfirmedCases", 
    "cases",
    "country",
    MIN_DATE_CASES,
    fill_missing)
}

get_cumulative_cases_region <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(REGIONS_URL),
    "dailyTotalConfirmedCases",
    "cases",
    "region", 
    MIN_DATE_CASES,
    fill_missing)
}

get_cumulative_cases_utla <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(UTLAS_URL),
    "dailyTotalConfirmedCases",
    "cases",
    "utla", 
    MIN_DATE_CASES, 
    fill_missing)
}

# Daily deaths data -----------------------------------------------------

get_daily_deaths_country <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(COUNTRIES_URL), 
    "dailyDeaths", 
    "deaths",
    "country", 
    MIN_DATE_DAILY_DEATHS,
    fill_missing)
}

# Cumulative deaths data -----------------------------------------------------

get_cumulative_deaths_country <- function(json, fill_missing = TRUE) {
  get_data_for_geography(
    fetch_datafile(COUNTRIES_URL), 
    "dailyTotalDeaths", 
    "deaths",
    "country",
    MIN_DATE_CUMULATIVE_DEATHS,
    fill_missing)
}

# Download all data ----------------------------------------------------------

download_data <- function(dataset_dir = ".") {
  
  # Extract and save the daily cases
  get_daily_cases_country() %>% 
    write_csv(file.path(dataset_dir, "cv-daily-cases-country.csv"))
  
  get_daily_cases_region() %>% 
    write_csv(file.path(dataset_dir, "cv-daily-cases-region.csv"))
  
  get_daily_cases_utla() %>% 
    write_csv(file.path(dataset_dir, "cv-daily-cases-utla.csv"))
  
  # Extract and save the cumulative cases
  get_cumulative_cases_country() %>% 
    write_csv(file.path(dataset_dir, "cv-cumulative-cases-country.csv"))
  
  get_cumulative_cases_region() %>% 
    write_csv(file.path(dataset_dir, "cv-cumulative-cases-region.csv"))
  
  get_cumulative_cases_utla() %>% 
    write_csv(file.path(dataset_dir, "cv-cumulative-cases-utla.csv"))
  
  # Extract and save the daily deaths
  get_daily_deaths_country() %>% 
    write_csv(file.path(dataset_dir, "cv-daily-deaths-country.csv"))
  
  # Extract and save the cumulative deaths
  get_cumulative_deaths_country() %>% 
    write_csv(file.path(dataset_dir, "cv-cumulative-deaths-country.csv"))    
}    
