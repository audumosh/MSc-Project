## Install and load neccesary packages
# install.packages("tidyverse")
# install.packages("scales")
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

# Function to format the amount

format_amount <- function(amount) {
  if (amount >= 1e9) {
    formatted_amount <- paste0("$", format(round(amount / 1e9, 1), nsmall = 1), "b")
  } else if (amount >= 1e6) {
    formatted_amount <- paste0("$", format(round(amount / 1e6, 1), nsmall = 1), "m")
  } else if (amount >= 1e5) {
    formatted_amount <- paste0("$", round(amount / 1e3), "k")
  } else {
    formatted_amount <- paste0("$", round(amount))
  }
  return(formatted_amount)
}


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

# Apply the function to create a new formatted amount column
Funder_mapping <- Funder_mapping %>%
  mutate(
    Formatted_Amount_Awarded = sapply(Total_Amount_Awarded, function(x) {
      amount <- as.numeric(gsub(",", "", x))
      if (amount == 0) {
        formatted_amount <- "N/A"
      } else {
        formatted_amount <- format_amount(amount)
      }
      return(formatted_amount)
    }),
    Funder_and_Amount = paste(Funders, " ", "(", Formatted_Amount_Awarded, ")", sep = "")
  )
  

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
    Proportion_of_Total_Funds = scales::percent(Proportion_of_Total_Funds)
  )


# Conduct chisq-test
chisq <- chisq.test(Funder_location$Total_Amount_Awarded)
print(chisq)



## Funding landscape across member states

# Descriptive analysis of number of research project conducted and total number of funders per member state

PAHO_COVID_Projects1 <- PAHO_COVID_Projects %>%
# Split the 'Country/ countries research are being conducted' into multiple rows
separate_rows(`Country/.countries.research.are.being.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.are.being.conducted` = trimws(`Country/.countries.research.are.being.conducted`))

# Analyze the number of projects and funders for each unique country
country_analysis <- PAHO_COVID_Projects1 %>%
  group_by(`Country/.countries.research.are.being.conducted`) %>%
  summarise(
    Total_Projects = n(),
    No_of_Funders = n_distinct(Funders)
  ) %>%
  rename(Country = `Country/.countries.research.are.being.conducted`)

# Descriptive analysis of number of research project conducted classified by income classification of locations

# Clean and preprocess the data
PAHO_COVID_Projects2 <- PAHO_COVID_Projects %>%
  mutate(`Income.classification` = ifelse(is.na(`Income.classification`), "Not-HIC", `Income.classification`)) %>%
  mutate(`Income.classification` = ifelse(`Income.classification` == "HIC", "HIC", "Not-HIC"))

# Count the number of projects by income classification
Project_Income_classification <- PAHO_COVID_Projects2 %>%
  group_by(`Income.classification`) %>%
  summarise(Projects = n()) %>%
  ungroup()

# Calculate proportions
Project_Income_classification <- Project_Income_classification %>%
  mutate(Proportion = Projects / sum(Projects))

# Format the proportions to percentage
Project_Income_classification <- Project_Income_classification %>%
  mutate(
    Proportion = scales::percent(Proportion)
  )
# Perform chi-squared test to check for statistical significance between HIC and Not-HIC projects
chisq_test <- chisq.test(Project_Income_classification$Projects)
print(chisq_test)


# Plot the bar chart of number of projects per funder
ggplot(Funder_mapping, aes(x = reorder(Funder_and_Amount, Total_Projects), y = Total_Projects)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Total_Projects), hjust = -0.2, color = "black", size = 3) +
  labs(title = "Number of Projects per Funder and Amount Awarded",
       x = "Funder and Amount Awarded",
       y = "Number of Projects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + coord_flip()


# Plot the bar chart of number of projects per funder
ggplot(Funder_mapping, aes(x = reorder(Funder_and_Amount, No_of_Countries), y = No_of_Countries)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = Total_Projects), hjust = -0.2, color = "black", size = 3) +
  labs(title = "Number of Countries per Funder and Amount Awarded",
       x = "Funder and Amount Awarded",
       y = "Number of Countries") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + coord_flip()
