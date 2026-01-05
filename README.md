# Health Progress in South Asia (2000–2020): An Exploratory Analysis

## Table of Contents

- [Background](#background)
- [Data Collection](#data-collection)
- [Research Questions](#research-questions)
- [Methodology](#methodology)
- [Key Findings](#key-findings)
  - [Yearly Trends by Country](#1-yearly-trends-by-country)
  - [India vs Regional IQR](#2-indias-value-vs-regional-iqr)
  - [India vs South Asia Median](#3-india-vs-south-asia-median)
  - [Distributions by Indicator](#4-distributions-by-indicator)
  - [Immunization vs Child Mortality](#5-immunization-vs-child-mortality)
  - [Health Expenditure vs Life Expectancy](#6-health-expenditure-vs-life-expectancy)
  - [Regional Deviation Heatmap](#7-regional-deviation-heatmap)
- [Limitations](#limitations)
- [Ethics & References](#ethics--references)
- [Reproducibility Notes](#reproducibility-notes)
- [Conclusion](#conclusion)

## Background
Health indicators are vital for monitoring development and equity. South Asia — comprising Afghanistan, Bangladesh, Bhutan, India, Maldives, Nepal, Pakistan, and Sri Lanka — has experienced major demographic and health transitions over the past two decades.  
This project explores trends in life expectancy, healthcare spending, immunization, and mortality using open World Bank data, with findings shared via a written report and an interactive Shiny dashboard.

## Data Collection
- **Source:** [World Bank Health, Nutrition, and Population Statistics](https://databank.worldbank.org/source/health-nutrition-and-population-statistics)  
- **Acquisition:** API scraping, saved as `health_progress_in_SA.csv`  
- **Variables:** Birth/death rates, life expectancy, immunization (DPT), health expenditure, population growth, infant & under-five mortality  
- **Format:** Cleaned and provided in wide CSV format for EDA and dashboard use  

## Research Questions
1. How have major health indicators changed across South Asia in the last 20+ years?  
2. How does India’s progress compare to the regional median and IQR?  
3. What are the relationships between immunization & mortality, and expenditure & life expectancy?  
4. Do disparities persist between countries and within regional trends?  

## Methodology
- **Data Cleaning & Processing:** R (`dplyr`, `janitor`, `tidyverse`, `ggplot2`)  
- **EDA:** Trends, outlier detection, distributions, medians, heatmaps, inter-country comparisons  
- **Interactive Visualization:** Shiny app (`app.R`) with country/indicator selection, dataset upload, interactive plots, and regional comparisons  
- **Code Modularity:** No hardcoded paths; reusable with similar datasets  

## Key Findings
- **Yearly Trends:** Most countries improved immunization and reduced child mortality; COVID-19 caused spikes in death rates and dips in life expectancy (esp. India).  
- **India vs Regional IQR/Median:** India generally aligned with or exceeded the median, except during COVID years.  
- **Distributions:** Pandemic broadened death rate and expenditure distributions; immunization and mortality remained tighter.  
- **Immunization vs Mortality:** Strong negative correlation; higher immunization linked to lower child mortality.  
- **Expenditure vs Life Expectancy:** Positive correlation; outliers (Afghanistan) highlight socioeconomic factors.  
- **Regional Heatmap:** Maldives & Sri Lanka consistently outperform; Afghanistan & Pakistan lag behind.  

## Limitations
- Data gaps for certain years/countries  
- Country-level aggregation may mask internal inequality  
- Observed trends are correlative, not causal  
- All data is public and aggregated; ethical use ensured  


## Reproducibility Notes
1. **Pre-requisites:** R ≥ 4.0, required packages installed  
2. **Static Analysis:** Run `scripts/EDA_FINAL.R` for plots and tables  
3. **Shiny App:**  
   ```r
   shiny::runApp("app/app.R")
