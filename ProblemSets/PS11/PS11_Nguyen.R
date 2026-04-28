library(tidyverse)
library(readxl)
library(janitor)
library(fixest)
library(modelsummary)

# Loading in CPS data from IPUMS (https://cps.ipums.org/cps-action/variables/ASECFLAG#description_section)

cps_raw <- read_csv("cps_00004.csv.gz")

# Cleaning CPS data for our target industries
cps_clean <- cps_raw %>%
  filter(
    AGE >= 16, AGE <= 64,
    ASECFLAG == 1,
    EMPSTAT != 0
  ) %>%
  mutate(
    employed = as.numeric(EMPSTAT == 10 | EMPSTAT == 12),
    immigrant = as.numeric(NATIVITY != 1),
    post = as.numeric(YEAR >= 2017),
    target_industry = case_when(
      IND %in% c(170,180,190,270,280,290) ~ 1,        # agriculture
      IND == 770 ~ 1,                                   # construction
      IND %in% c(1070,1080,1090,1170,1180,1190,
                 1270,1280,1290) ~ 1,                   # food manufacturing
      IND %in% c(8680,8690) ~ 1,                       # food services
      TRUE ~ 0
    )
  )

# Determining our pretreatment variables and treatment assignment at the state level
pre_treatment <- cps_clean %>%
  filter(target_industry == 1, YEAR < 2017) %>%
  group_by(STATEFIP) %>%
  summarise(
    immigrant_share = weighted.mean(immigrant, ASECWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(high_immigrant = as.numeric(immigrant_share > median(immigrant_share)))

summary(pre_treatment$immigrant_share)
pre_treatment %>% count(high_immigrant)


# Gathering QWEC data for our target industries at the county level, then aggregating to MSA level using the Census crosswalk.
# Downloading Crosswalk

temp <- tempfile(fileext = ".xls")
download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls", 
              temp, mode = "wb")

crosswalk <- read_xls(temp, skip = 2) %>%
  clean_names() %>%
  filter(!is.na(fips_state_code), !is.na(cbsa_code)) %>%
  mutate(area_fips = str_pad(paste0(fips_state_code, fips_county_code), 5, pad = "0")) %>%
  select(area_fips, cbsa_code, cbsa_title) %>%
  distinct()

county_fips <- crosswalk %>% pull(area_fips) %>% unique()

# Pulling QWEC data year by year and by industries (this took like 1.5 hrs :\ )
target_industries <- c("722", "23", "11", "311")

pull_county <- function(fips, year) {
  url <- paste0("https://data.bls.gov/cew/data/api/", year, "/a/area/", fips, ".csv")
  tryCatch({
    read_csv(url, show_col_types = FALSE) %>%
      filter(industry_code %in% target_industries,
             own_code == 5) %>%
      select(area_fips, industry_code, annual_avg_emplvl, avg_annual_pay) %>%
      mutate(
        area_fips = as.character(area_fips),
        industry_code = as.character(industry_code),
        year = year
      )
  }, error = function(e) NULL)
}

for(yr in 2014:2020) {
  message("Pulling year: ", yr)
  year_data <- map_dfr(county_fips, pull_county, year = yr)
  saveRDS(year_data, paste0("qcew_", yr, ".rds"))
  message("Saved year: ", yr, " — rows: ", nrow(year_data))
}

qcew_raw <- map_dfr(2014:2020, function(yr) {
  readRDS(paste0("qcew_", yr, ".rds"))
})

# I saved these data tables locally to ensure that I don't have to rerun when I put into an RMD later

saveRDS(qcew_raw, "qcew_raw.rds")

# create a panel using the crosswalk to aggregate QWEC data to MSA levels
crosswalk_clean <- crosswalk %>%
  filter(!is.na(area_fips), !is.na(cbsa_code)) %>%
  mutate(area_fips = str_pad(as.character(area_fips), 5, pad = "0")) %>%
  select(area_fips, cbsa_code, cbsa_title) %>%
  distinct()

msa_panel <- qcew_raw %>%
  left_join(crosswalk_clean, by = "area_fips") %>%
  filter(!is.na(cbsa_code)) %>%
  group_by(cbsa_code, cbsa_title, industry_code, year) %>%
  summarise(
    employment = sum(annual_avg_emplvl, na.rm = TRUE),
    avg_pay = mean(avg_annual_pay, na.rm = TRUE),
    .groups = "drop"
  )

msa_total <- msa_panel %>%
  group_by(cbsa_code, cbsa_title, year) %>%
  summarise(
    total_employment = sum(employment, na.rm = TRUE),
    avg_pay = mean(avg_pay, na.rm = TRUE),
    .groups = "drop"
  )


# Merging CPS and QWEC data at the MSA level using the crosswalk.
# Because some MSAs span multiple states, we'll assign the treatment status based on the state with the largest population share in the MSA (this is a common approach in the literature).
crosswalk_with_state <- crosswalk %>%
  mutate(STATEFIP = as.numeric(str_sub(area_fips, 1, 2))) %>%
  select(cbsa_code, cbsa_title, STATEFIP) %>%
  distinct()

qcew_cps_panel <- msa_total %>%
  left_join(crosswalk_with_state %>% 
              select(cbsa_code, STATEFIP) %>% 
              distinct(cbsa_code, .keep_all = TRUE),
            by = "cbsa_code") %>%
  left_join(pre_treatment %>% 
              select(STATEFIP, immigrant_share, high_immigrant),
            by = "STATEFIP") %>%
  mutate(
    post = as.numeric(year >= 2017),
    log_employment = log(total_employment)
  ) %>%
  filter(!is.na(high_immigrant), is.finite(log_employment))

saveRDS(qcew_cps_panel, "qcew_cps_panel.rds")

# Event studying through the DID 

qcew_model2 <- feols(log(total_employment) ~ 
                       i(year, high_immigrant, ref = 2016) | 
                       cbsa_code + year,
                     data = qcew_cps_panel,
                     cluster = ~STATEFIP)

summary(qcew_model2)

# Developing Tables ad Figures for the paper
# Summary Stats
table_data <- qcew_cps_panel %>%
  mutate(
    Log_Employment = log_employment,
    Total_Employment = total_employment,
    Avg_Annual_Pay = avg_pay,
    Group = factor(high_immigrant, 
                   levels = c(0, 1), 
                   labels = c("Low Immigrant", "High Immigrant"))
  ) %>%
  select(Log_Employment, Total_Employment, Avg_Annual_Pay, Group)

datasummary_balance(
  ~ Group,
  data = table_data,
  output = "latex"
)

# Regression Table
modelsummary(
  qcew_model2,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    "year::2014:high_immigrant" = "2014 x High Immigrant",
    "year::2015:high_immigrant" = "2015 x High Immigrant",
    "year::2017:high_immigrant" = "2017 x High Immigrant",
    "year::2018:high_immigrant" = "2018 x High Immigrant",
    "year::2019:high_immigrant" = "2019 x High Immigrant",
    "year::2020:high_immigrant" = "2020 x High Immigrant"
  ),
  gof_map = c("nobs", "adj.r.squared"),
  notes = "Standard errors clustered at the state level. Reference year is 2016.",
  output = "latex"
)

# Plot that thang
pdf("event_study_plot.pdf", width = 8, height = 5)

iplot(qcew_model2,
      main = "Event Study: Employment in Immigrant-Intensive Industries",
      xlab = "Year",
      ylab = "Log Employment (relative to 2016)",
      ci_level = 0.95)

abline(v = 2016.5, lty = 2, col = "grey40")

dev.off()

message("Done! event_study_plot.pdf saved to working directory.")