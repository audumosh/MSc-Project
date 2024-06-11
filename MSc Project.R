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
    Total_Amount_Awarded = sum(Amount.Awarded, na.rm = TRUE)
    )



# Split the 'Country/ countries research is being conducted' into multiple rows
PAHO_COVID_Projects <- PAHO_COVID_Projects %>%
  separate_rows(`Country/.countries.research.is.being.are.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.is.being.are.conducted` = trimws(`Country/.countries.research.is.being.are.conducted`))

# Group by 'Funders' and count unique countries
result2 <- PAHO_COVID_Projects %>%
  group_by(Funders) %>%
  summarise(Unique_Countries = n_distinct(`Country/.countries.research.is.being.are.conducted`))
