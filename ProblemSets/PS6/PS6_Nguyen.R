
library(tidycensus)
library(tidyverse)
library(scales)

years <- c(2019, 2021, 2022, 2023)

acs_data <- map_dfr(years, function(y) {
  get_acs(
    geography = "state",
    variables = c(
      foreign_born    = "B05002_013",
      total_pop       = "B05002_001",
      poverty_foreign = "B05010_002",
      poverty_native  = "B05010_010",
      income          = "B19013_001"
    ),
    year   = y,
    survey = "acs1"
  ) %>%
    mutate(year = y)
})

head(acs_data)

# PNG 1: Foreign Born Population from 2019-2023
fig1 <- acs_data %>%
  filter(variable == "foreign_born") %>%
  group_by(year) %>%
  summarise(total = sum(estimate, na.rm = TRUE))

p1 <- ggplot(fig1, aes(x = year, y = total)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "black", size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title   = "Figure 1: U.S. Foreign-Born Population (2019-2023)",
        y       = "Foreign-Born Population",
    caption = "Source: ACS 1-Year Estimates, Table B05002"
  ) +
  theme_minimal()

print(fig1)
print(p1)


# PNG 2: Poverty Rates for Foreign-Born vs Native-Born

fig2 <- acs_data %>%
  filter(variable %in% c("poverty_foreign", "poverty_native")) %>%
  group_by(year, variable) %>%
  summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = ifelse(variable == "poverty_foreign",
                        "Foreign-Born", "Native-Born"))

p2 <- ggplot(fig2, aes(x = year, y = total, color = label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Foreign-Born" = "#d7191c",
                                "Native-Born"  = "#2c7bb6")) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 2: Population Below Poverty Line by Nativity (2019-2023)",
    subtitle = "Foreign-born individuals consistently face higher poverty exposure",
    y        = "Population Below Poverty Line",
    caption  = "Source: ACS 1-Year Estimates, Table B05010"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(p2)

# PNG 3: Uninsured Foreign-Born Population from 2019-2023

uninsured <- map_dfr(years, function(y) {
  get_acs(
    geography = "state",
    variables = c(
      uninsured_foreign = "B27020_006"  # foreign-born, no health insurance
    ),
    year   = y,
    survey = "acs1"
  ) %>%
    mutate(year = y)
})

head(uninsured)

fig3 <- uninsured %>%
  group_by(year) %>%
  summarise(total = sum(estimate, na.rm = TRUE))

p3 <- ggplot(fig3, aes(x = year, y = total)) +
  geom_line(color = "#d7191c", linewidth = 1.2) +
  geom_point(color = "#d7191c", size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 3: Uninsured Foreign-Born Population (2019-2023)",
    subtitle = "Trends in health insurance gaps among foreign-born residents",
    y        = "Uninsured Foreign-Born Population",
    x        = NULL,
    caption  = "Source: ACS 1-Year Estimates, Table B27020"
  ) +
  theme_minimal()

print(p3)

ggsave("PS6a_Nguyen.png", plot = p1, width = 9, height = 5, dpi = 300, bg = "white")
ggsave("PS6b_Nguyen.png", plot = p2, width = 9, height = 5, dpi = 300, bg = "white")
ggsave("PS6c_Nguyen.png", plot = p3, width = 9, height = 5, dpi = 300, bg = "white")
