## Install and load neccesary packages
install.packages("tidyverse")
install.packages("scales")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)


## Read excel file
PAHO_COVID_Projects <- read.xlsx(xlsxFile = "COVID-19-Research-Project-Tracker-PAHO-foR.xlsx")



# Check the column names to ensure correct reference
colnames(PAHO_COVID_Projects)

# Clean and convert 'Amount Awarded' to numeric
PAHO_COVID_Projects <- PAHO_COVID_Projects %>%
  mutate(Amount.Awarded = as.numeric(gsub("[^0-9.]", "", Amount.Awarded)))


#### Research question 1: What were the primary sources of funding for COVID-19 research conducted in the PAHO region?

### Descriptive and statistical analysis of COVID-19 research funding sources

## Number of research projects funded, number of countries where projects are being conducted and total amount mapped to funders

Funder_mapping <- PAHO_COVID_Projects %>%
  # Split the 'Country/ countries research are being conducted' into multiple rows
  separate_rows(`Country/.countries.research.are.being.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.are.being.conducted` = trimws(`Country/.countries.research.are.being.conducted`)) %>%
  group_by(Funders) %>%
  summarise(
    Total_Projects = n(),
    No_of_Countries = n_distinct(`Country/.countries.research.are.being.conducted`),
    Total_Amount_Awarded = sum(Amount.Awarded, na.rm = TRUE),
    )

# Format the Total_Amount_Awarded with commas
Funder_mapping <- Funder_mapping %>%
  mutate(
    Total_Amount_Awarded = scales::comma(Total_Amount_Awarded) )


## Classification of funders to within and outside of PAHO, mapped to the number of projects and proportion of funding awarded
Funder_location <- PAHO_COVID_Projects %>%
  group_by(Location.classification) %>%
  summarise(
    Number_of_Funders = n_distinct(Funders),
    Total_Amount_Awarded = sum(Amount.Awarded, na.rm = TRUE)
  ) %>%
  mutate(
    Proportion_of_Total_Funds = Total_Amount_Awarded / sum(Total_Amount_Awarded)
  )

# Format the Total_Amount_Committed with commas and percentage
Funder_location <- Funder_location %>%
  mutate(
    Total_Amount_Awarded = scales::comma(Total_Amount_Awarded),
    Proportion_of_Total_Funds = scales::percent(Proportion_of_Total_Funds)
  )

# t-test to check for statistical significance in the amount awarded between funders from within and outside of PAHO
t_test_funder_location <- t.test(Amount.Awarded ~ Location.classification, data = PAHO_COVID_Projects)
print(t_test_funder_location)



## Funding landscape across member states

# Descriptive analysis of number of research project conducted and total number of funders per member state

PAHO_COVID_Projects <- PAHO_COVID_Projects %>%
# Split the 'Country/ countries research are being conducted' into multiple rows
separate_rows(`Country/.countries.research.are.being.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.are.being.conducted` = trimws(`Country/.countries.research.are.being.conducted`))

# Analyze the number of projects and funders for each unique country
country_analysis <- PAHO_COVID_Projects %>%
  group_by(`Country/.countries.research.are.being.conducted`) %>%
  summarise(
    Total_Projects = n(),
    No_of_Funders = n_distinct(Funders)
  ) %>%
  rename(Country = `Country/.countries.research.are.being.conducted`)

# Descriptive analysis of number of research project conducted classified by income classification of locations



