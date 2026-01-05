# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)

# Read and clean dataset
dat <- read.csv("health_progress_in_SA.csv")  
dat <- dat %>% clean_names()

# Remove the last column (data for 2024)
dat <- dat[, -ncol(dat)]

# Rename columns like "x2001" to "2001"
is_year <- grepl("^x\\d{4}", names(dat))
names(dat)[is_year] <- substring(names(dat)[is_year], 2, 5)

# Convert year columns to numeric
dat[, 5:ncol(dat)] <- lapply(dat[, 5:ncol(dat)], as.numeric)

# Identify year columns
year <- names(dat)[grepl("^\\d{4}$", names(dat))]

# Convert from wide to long format
dat_long <- dat %>%
  pivot_longer(
    cols = all_of(year),
    names_to = "year",
    values_to = "value"
  )

# Clean text fields and factor levels
dat_long <- dat_long %>%
  mutate(
    series_name = trimws(series_name),
    series_name = gsub("[[:cntrl:]]", "", series_name),
    series_name = droplevels(as.factor(series_name)),
    year = as.numeric(year)
  ) %>%
  filter(series_name != "" & !is.na(series_name))

# Create regional median and India-specific subsets
regional_median <- dat_long %>%
  group_by(series_name, year) %>%
  summarize(median_value = median(value, na.rm = TRUE))

india_data <- dat_long %>%
  filter(country_name == "India")

# Plot 1: Yearly trends for all countries
ggplot(dat_long, aes(x = year, y = value, color = country_name)) +
  geom_line(size = 1) +
  facet_wrap(~ series_name, scales = "free") +
  theme_bw() +
  labs(title = "Yearly Trends (2001–2023)", x = "Year", y = "Value")
ggsave("yearly_trends.png", width = 10, height = 6, dpi = 300)

# Plot 2: India vs Regional Median
ggplot() +
  geom_line(data = regional_median, aes(x = year, y = median_value),
            color = "gray", size = 1, linetype = "dashed") +
  geom_line(data = india_data, aes(x = year, y = value, color = "India"), size = 1) +
  facet_wrap(~ series_name, scales = "free_y") +
  theme_bw() +
  labs(title = "India vs South Asia (Median)", x = "Year", y = "Value", color = "")
ggsave("india_vs_south_asia_median.png", width = 10, height = 6, dpi = 300)


# Plot 3: Health Expenditure vs Life Expectancy
subset_expenditure <- dat_long %>%
  filter(series_name %in% c("Current health expenditure (% of GDP)",
                            "Life expectancy at birth, total (years)")) %>%
  pivot_wider(id_cols = c(country_name, year),
              names_from = series_name,
              values_from = value) %>%
  filter(!is.na(`Current health expenditure (% of GDP)`),
         !is.na(`Life expectancy at birth, total (years)`))

ggplot(subset_expenditure, aes(
  x = `Current health expenditure (% of GDP)`,
  y = `Life expectancy at birth, total (years)`,
  color = country_name
)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Health Expenditure vs Life Expectancy",
       x = "Health Expenditure (% of GDP)",
       y = "Life Expectancy (years)")
ggsave("health_expenditure_vs_life_expectancy.png", width = 10, height = 6, dpi = 300)

# Plot 4: Immunization vs Child Mortality
subset_imm <- dat_long %>%
  mutate(
    series_name = trimws(series_name),
    country_name = trimws(country_name)
  ) %>%
  filter(series_name %in% c("Immunization, DPT (% of children ages 12-23 months)",
                            "Mortality rate, under-5 (per 1,000 live births)")) %>%
  pivot_wider(id_cols = c(country_name, year),
              names_from = series_name,
              values_from = value) %>%
  filter(!is.na(`Immunization, DPT (% of children ages 12-23 months)`),
         !is.na(`Mortality rate, under-5 (per 1,000 live births)`),
         is.finite(`Immunization, DPT (% of children ages 12-23 months)`),
         is.finite(`Mortality rate, under-5 (per 1,000 live births)`))

ggplot(subset_imm, aes(
  x = `Immunization, DPT (% of children ages 12-23 months)`,
  y = `Mortality rate, under-5 (per 1,000 live births)`,
  color = country_name
)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Immunization vs Child Mortality",
       x = "Immunization Rate (%)",
       y = "Under-5 Mortality (per 1,000 births)")
ggsave("immunization_vs_child_mortality.png", width = 10, height = 6, dpi = 300)

# Plot 5: Distribution of values by indicator
ggplot(dat_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ series_name, scales = "free") +
  theme_bw() +
  labs(title = "Distribution of Values by Indicator", x = "Value", y = "Count")
ggsave("distribution_by_indicator.png", width = 10, height = 6, dpi = 300)

# Plot 6: Is India an Outlier? (IQR comparison)
iqr_plot <- dat_long %>%
  group_by(series_name, year) %>%
  summarize(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE)
  ) %>%
  left_join(india_data, by = c("series_name", "year"))

ggplot(iqr_plot, aes(x = year)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "lightgray", alpha = 0.5) +
  geom_line(aes(y = median_value), color = "gray30", linetype = "dashed") +
  geom_line(aes(y = value, color = "India"), size = 1) +
  facet_wrap(~ series_name, scales = "free_y") +
  theme_bw() +
  labs(title = "India’s Value vs Regional IQR", x = "Year", y = "Value", color = "")
ggsave("india_vs_regional_iqr.png", width = 10, height = 6, dpi = 300)

# Plot 7: Regional Comparison Heatmap (Deviation from Median)
heat_df <- dat_long %>%
  left_join(regional_median, by = c("series_name", "year")) %>%
  mutate(diff = value - median_value)

ggplot(heat_df, aes(x = year, y = country_name, fill = diff)) +
  geom_tile() +
  facet_wrap(~ series_name, scales = "free_x") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Deviation from Regional Median (All Countries)",
       x = "Year", y = "Country", fill = "Difference")
ggsave("regional_deviation_heatmap.png", width = 12, height = 8, dpi = 300)

