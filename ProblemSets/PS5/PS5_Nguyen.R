library(tidyverse)
library(rvest)
library(dplyr)
library(ipumsr)


url = "https://en.wikipedia.org/wiki/Oklahoma_City"
webpage <- read_html(url)

#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(47)

table_target <- webpage %>%
  html_element("#mw-content-text div.mw-parser-output table:nth-child(47)") %>%
  html_table(fill = TRUE)

# Using my IPUMS API KEY, I can import CPS data

cps_data <- define_extract_micro(
  collection = "cps",
  description = "Oklahoma Promise data",
  samples = c("cps2023_10s", "cps2022_10s", "cps2021_10s", "cps2020_10s"),
  variables = c(
    "YEAR", "STATEFIP", "AGE", "FAMINC",  # Core variables
    "SCHLCOLL", "EDUC"                     # Outcomes
  )
)
submitted_extract <- submit_extract(cps_extract)
download_extract(submitted_extract)
list.files(pattern = "cps_")
cps_data <- read_ipums_micro("cps_00003.xml")
ok_data <- cps_data %>% filter(STATEFIP == 40)




