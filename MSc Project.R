## Install and load neccesary packages
install.packages("tidyverse")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)


## Read excel file
PAHO_COVID_Projects <- read.xlsx(xlsxFile = "COVID-19-Research-Project-Tracker-PAHO-foR.xlsx")

# Function to count unique countries per funder
count_unique_countries <- function(...) {
  unique_countries <- unique(unlist(list(...)))
  return(length(unique_countries))
}


# Check the column names to ensure correct reference
colnames(PAHO_COVID_Projects)

# Clean and convert 'Amount Awarded' to numeric
PAHO_COVID_Projects <- PAHO_COVID_Projects %>%
  mutate(Amount.Awarded = as.numeric(gsub("[^0-9.]", "", Amount.Awarded)))


# Descriptive analysis
# Data processing
result <- PAHO_COVID_Projects %>%
  group_by(Funders) %>%
  summarise(
    Total_Projects = n(),
    Countries_funded = count_unique_countries
    (`Country/.countries.research.is.being.conducted_1`, 
      `Country/.countries.research.is.being.conducted_2`,
      `Country/.countries.research.is.being.conducted_3`,
      `Country/.countries.research.is.being.conducted_4`,
      `Country/.countries.research.is.being.conducted_5`,
      `Country/.countries.research.is.being.conducted_6`,
      `Country/.countries.research.is.being.conducted_7`),
    Total_Amount_Awarded = sum(Amount.Awarded, na.rm = TRUE)
    )


