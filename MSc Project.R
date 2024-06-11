## Install and load neccesary packages
install.packages("tidyverse")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)


## Read excel file
PAHO_COVID_Projects <- read.xlsx(xlsxFile = "COVID-19-Research-Project-Tracker-PAHO-foR.xlsx")



# Check the column names to ensure correct reference
colnames(PAHO_COVID_Projects)

# Clean and convert 'Amount Awarded' to numeric
PAHO_COVID_Projects <- PAHO_COVID_Projects %>%
  mutate(Amount.Awarded = as.numeric(gsub("[^0-9.]", "", Amount.Awarded)))


### Research question 1: What were the primary sources of funding for COVID-19 research conducted in the PAHO region?

## Descriptive and statiscal analysis of COVID-19 research funding sources

# Number of research projects funded, number of countries where funded projects are being conducted and total amount mapped to funders

result_1 <- PAHO_COVID_Projects %>%
  # Split the 'Country/ countries research are being conducted' into multiple rows
  separate_rows(`Country/.countries.research.are.being.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.are.being.conducted` = trimws(`Country/.countries.research.are.being.conducted`)) %>%
  group_by(Funders) %>%
  summarise(
    Total_Projects = n(),
    No_of_Countries = n_distinct(`Country/.countries.research.are.being.conducted`),
    Total_Amount_Awarded = sum(Amount.Awarded, na.rm = TRUE),
    )

