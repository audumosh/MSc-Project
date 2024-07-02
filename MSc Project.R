## Install and load neccesary packages
# install.packages("tidyverse")
# install.packages("scales")
# install.packages("maps")
# install.packages("viridis")
# install.packages("writexl")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(maps)
library(viridis)
library(writexl)


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
  

# Plot the bar chart of number of projects per funder
ggplot(Funder_mapping, aes(x = reorder(Funder_and_Amount, Total_Projects), y = Total_Projects)) +
  geom_bar(stat = "identity", fill = "#1F78B4") +
  geom_text(aes(label = Total_Projects), hjust = -0.2, color = "black", size = 3) +
  labs(
       x = "",
       y = "Number of Projects") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + coord_flip()


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

country_analysis <- country_analysis %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Analyze the number of projects and funders for each unique country
country_analysis2 <- PAHO_COVID_Projects1 %>%
  group_by(`Country/.countries.research.are.being.conducted`) %>%
  summarise(
    Total_Projects = n(),
    ) %>%
  rename(Country = `Country/.countries.research.are.being.conducted`)

country_analysis2 <- country_analysis2 %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Rename the Total_Projects column to Total Projects
country_analysis2 <- country_analysis2 %>%
  rename(`Total Projects` = Total_Projects)

# Write the data frame to an Excel file
write_xlsx(country_analysis2, "country_analysis2.xlsx")

# Get world map data
world_map <- map_data("world")


# Merge your data with the map data
world_map_df <- left_join(world_map, country_analysis, by = c("region" = "Country"))


# Plot the map
ggplot(world_map_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Total_Projects), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "#1F78B4", na.value = "gray90") +
  labs(title = "",
       fill = "Projects") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )


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





#### Research question 2: To what extent did the COVID-19 research funding align with the established priorities?

### Descriptive analysis of research projects alignment with WHO priority areas

## Assess alignment of research projects with WHO primary research priorities

PAHO_COVID_Projects3 <- PAHO_COVID_Projects %>%
  # Split the 'primary research priorities' into multiple rows
  separate_rows(`PRIMARY.WHO.Research.Priority.Area.Names`, sep = ";") %>%
  mutate(`PRIMARY.WHO.Research.Priority.Area.Names` = trimws(`PRIMARY.WHO.Research.Priority.Area.Names`))

# Analyze the number of projects that maps to primary research priorities
WHO_priority_alignment_1 <- PAHO_COVID_Projects3 %>%
  group_by(`PRIMARY.WHO.Research.Priority.Area.Names`) %>%
  summarise(
    Primary_focus = n()) %>%
  rename(Research_areas = `PRIMARY.WHO.Research.Priority.Area.Names`)

## Assess alignment of research projects with WHO secondary research priorities

PAHO_COVID_Projects4 <- PAHO_COVID_Projects %>%
  # Split the 'secondary research priorities' into multiple rows
  separate_rows(`SECONDARY.WHO.Research.Priority.Area.Name(s)`, sep = ";") %>%
  mutate(`SECONDARY.WHO.Research.Priority.Area.Name(s)` = trimws(`SECONDARY.WHO.Research.Priority.Area.Name(s)`))

# Analyze the number of projects that maps to secondary research priorities
WHO_priority_alignment_2 <- PAHO_COVID_Projects4 %>%
  group_by(`SECONDARY.WHO.Research.Priority.Area.Name(s)`) %>%
  summarise(
    Secondary_focus = n()) %>%
  rename(Research_areas = `SECONDARY.WHO.Research.Priority.Area.Name(s)`)

## Merge primary and secondary priority analysis

WHO_priority_alignment <- full_join(WHO_priority_alignment_1, WHO_priority_alignment_2, by = "Research_areas")


# Replace NA values with zero
WHO_priority_alignment <- WHO_priority_alignment %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Identify and change the specific value (4468) in the secondary focus for NA to 0
WHO_priority_alignment <- WHO_priority_alignment %>%
  mutate(across(everything(), ~ replace(., . == 4468, 0)))


# Replace NA values in Research_areas with a specific label
WHO_priority_alignment <- WHO_priority_alignment %>%
  mutate(Research_areas = replace_na(Research_areas, "NA"))

# Create a new column to order the research areas
WHO_priority_alignment <- WHO_priority_alignment %>%
  mutate(Total_focus = Primary_focus + Secondary_focus) %>%
  arrange(Total_focus)

# Convert Research_areas to a factor with levels in the desired order, ensuring "N/A" is last
WHO_priority_alignment <- WHO_priority_alignment %>%
  mutate(Research_areas = factor(Research_areas, levels = c(setdiff(unique(Research_areas), "N/A"), "N/A")))

# Pivot data to long format for easier plotting
WHO_priority_alignment_long <- WHO_priority_alignment %>%
  pivot_longer(cols = c(Primary_focus, Secondary_focus), 
               names_to = "Focus", 
               values_to = "Count")

# Ensure the order of the focus levels is correct
WHO_priority_alignment_long <- WHO_priority_alignment_long %>%
  mutate(Focus = factor(Focus, levels = c("Secondary_focus", "Primary_focus")))


# Plot the stacked bar chart
ggplot(WHO_priority_alignment_long, aes(x = Research_areas, y = Count, fill = Focus)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), position = position_stack(vjust = 0.5), size = 2.5) +
  scale_fill_manual(values = c("Primary_focus" = "#B0BDE0", "Secondary_focus" = "#1F78B4"), 
                    labels = c("Primary_focus" = "Primary area of focus", "Secondary_focus" = "Secondary area of focus"), 
                    name = "") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  # Remove major horizontal gridlines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal gridlines
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  coord_flip()



## Assess alignment of research projects with WHO primary research sub-priorities

PAHO_COVID_Projects5 <- PAHO_COVID_Projects %>%
  # Split the 'primary research sub-priorities' into multiple rows
  separate_rows(`PRIMARY.WHO.Research.Sub-Priority.Number(s)`, sep = ",") %>%
  mutate(`PRIMARY.WHO.Research.Sub-Priority.Number(s)` = trimws(`PRIMARY.WHO.Research.Sub-Priority.Number(s)`))

# Analyze the number of projects that maps to primary research sub-priorities
WHO_sub_priority_alignment_1 <- PAHO_COVID_Projects5 %>%
  group_by(`PRIMARY.WHO.Research.Sub-Priority.Number(s)`) %>%
  summarise(
    Primary_subpriority = n()) %>%
  rename(Research_numbers = `PRIMARY.WHO.Research.Sub-Priority.Number(s)`)

## Assess alignment of research projects with WHO secondary research sub-priorities

PAHO_COVID_Projects6 <- PAHO_COVID_Projects %>%
  # Split the 'secondary research sub-priorities' into multiple rows
  separate_rows(`SECONDARY.WHO.Research.Sub-Priority.Number(s)`, sep = ",") %>%
  mutate(`SECONDARY.WHO.Research.Sub-Priority.Number(s)` = trimws(`SECONDARY.WHO.Research.Sub-Priority.Number(s)`))

# Analyze the number of projects that maps to secondary research sub-priorities
WHO_sub_priority_alignment_2 <- PAHO_COVID_Projects6 %>%
  group_by(`SECONDARY.WHO.Research.Sub-Priority.Number(s)`) %>%
  summarise(
    Secondary_subpriority = n()) %>%
  rename(Research_numbers = `SECONDARY.WHO.Research.Sub-Priority.Number(s)`)

## Merge primary and secondary sub-priority analysis

WHO_sub_priority_alignment <- full_join(WHO_sub_priority_alignment_1, WHO_sub_priority_alignment_2, by = "Research_numbers")
