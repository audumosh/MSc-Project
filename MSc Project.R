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

# Map funders to no of countries funded

PAHO_COVID_Projects1 <- PAHO_COVID_Projects %>%
  # Split the 'Country/ countries research are being conducted' into multiple rows
  separate_rows(`Country/.countries.research.are.being.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.are.being.conducted` = trimws(`Country/.countries.research.are.being.conducted`)) 



Funder_mapping_countries <- PAHO_COVID_Projects1 %>%
  group_by(Funders) %>%
  summarise(
    No_of_Countries = n_distinct(`Country/.countries.research.are.being.conducted`),
  )

# Map funders to number of projects and total amount awarded 

Funder_mapping_xx <- PAHO_COVID_Projects %>% group_by(Funders) %>%
  summarise(
    Total_Projects = n(),
    Total_Amount_Awarded = sum(Amount.Awarded, na.rm = TRUE),
  )


# Format the Total_Amount_Awarded with commas
Funder_mapping_xx <- Funder_mapping_xx %>%
  mutate(
    Total_Amount_Awarded = scales::comma(Total_Amount_Awarded) )

# Join the two tables
Funder_mapping_joined <- left_join(Funder_mapping_xx, Funder_mapping_countries, by = "Funders")


# Apply the function to create a new formatted amount column
Funder_mapping <- Funder_mapping_joined %>%
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

# Format the Total_Amount_Committed with percentage
Funder_location <- Funder_location %>%
  mutate(
    Proportion_of_Total_Funds = scales::percent(Proportion_of_Total_Funds)
  )

# Apply the format_amount function to the Total_Amount_Awarded column
Funder_location_formatted <- Funder_location %>%
  mutate(
    Total_Amount_Awarded = sapply(Total_Amount_Awarded, format_amount)
  )


# Conduct chisq-test
chisq <- chisq.test(Funder_location$Total_Amount_Awarded)
print(chisq)



## Funding landscape across member states

# Descriptive analysis of number of research project conducted and total number of funders per member state

PAHO_COVID_Projects2 <- PAHO_COVID_Projects %>%
# Split the 'Country/ countries research are being conducted' into multiple rows
separate_rows(`Country/.countries.research.are.being.conducted`, sep = ",") %>%
  mutate(`Country/.countries.research.are.being.conducted` = trimws(`Country/.countries.research.are.being.conducted`))

# Analyze the number of projects and funders for each unique country
country_analysis <- PAHO_COVID_Projects2 %>%
  group_by(`Country/.countries.research.are.being.conducted`) %>%
  summarise(
    Total_Projects = n(),
    No_of_Funders = n_distinct(Funders)
  ) %>%
  rename(Country = `Country/.countries.research.are.being.conducted`)

country_analysis <- country_analysis %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

# Analyze the number of projects for each unique country
country_analysis2 <- PAHO_COVID_Projects2 %>%
  group_by(`Country/.countries.research.are.being.conducted`) %>%
  summarise(
    Total_Projects = n(),
    ) %>%
  rename(Country = `Country/.countries.research.are.being.conducted`)

country_analysis2 <- country_analysis2 %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

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


# Count the number of projects by income classification
Project_Income_classification <- PAHO_COVID_Projects %>%
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
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

# Identify and change the specific value (8965) in the secondary focus for NA to 0
WHO_priority_alignment <- WHO_priority_alignment %>%
  mutate(across(everything(), ~ replace(., . == 8965, 0)))


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


## Include subpriority names in dataframe 

WHO_sub_priority_alignment <- WHO_sub_priority_alignment %>%
  mutate(description = case_when(
    Research_numbers == "1a" ~ "Support development of diagnostic products to improve clinical processes",
    Research_numbers == "1b" ~ "Understand virus compartments, shedding and natural history of disease",
    Research_numbers == "1c" ~ "Develop tools and conduct studies to monitor phenotypic change and potential adaptation",
    Research_numbers == "1d" ~ "Characterize immunity (naturally acquired, population and vaccine-induced, including mucosal immunity)",
    Research_numbers == "1e" ~ "Develop disease models (animal models and 3Rs approaches)",
    Research_numbers == "1f" ~ "Virus stability in the environment",
    Research_numbers == "2a" ~ "Investigation of animal source and route of transmission",
    Research_numbers == "2b" ~ "Socioeconomic and behavioural risk factors for spill-over",
    Research_numbers == "2c" ~ "Risk reduction strategies at the human-animal environment interface",
    Research_numbers == "3a" ~ "Transmission dynamics - Clarify the relative importance of pre-symptomatic/ asymptomatic transmission (including distinction between virus shedding and infectious transmission)",
    Research_numbers == "3b" ~ "Disease severity - Identify groups at high risk of severe infection; Determine the role of different age groups in transmission",
    Research_numbers == "3c" ~ "Susceptibility - Determine if children are infected, and if so, are they infectious?",
    Research_numbers == "3d" ~ "Control and mitigation measures - Predict the most effective measures to reduce the peak burden on healthcare providers and other societal functions; Estimate the effects of social distancing measures and other non-pharmaceutical interventions on transmissibility",
    Research_numbers == "4a" ~ "Prognostic factors for severe disease (Different populations - pregnancy, young children, risk groups - immunosuppressed)",
    Research_numbers == "4b" ~ "Understand pathophysiology of COVID-19 infection, including understanding mild disease and the role of co-infections / infection, transmissibility, viral shedding",
    Research_numbers == "4c" ~ "Optimal endpoints for clinical trials",
    Research_numbers == "4d" ~ "Improve processes of care, including early diagnosis, discharge criteria; Determine interventions that improve the clinical outcome of infected patients (Steroids, High flow oxygen therapy)",
    Research_numbers == "4e" ~ "Optimal adjuvant therapies for patients (and contacts)",
    Research_numbers == "4f" ~ "Develop core clinical outcomes to maximize usability of data across range of trials",
    Research_numbers == "5a" ~ "Effectiveness of restriction of movement of healthy exposed and infected persons to prevent secondary transmission (home, congregate setting, geographical restriction vs nothing)",
    Research_numbers == "5b" ~ "Effectiveness of specific PPE to reduce the risk of COVID-19 transmission among HCWs, patients and individuals in the community",
    Research_numbers == "5c" ~ "Effectiveness of activities to minimize the role of the environment in COVID-19 transmission",
    Research_numbers == "5d" ~ "Factors and methods influencing compliance with evidence-based IPC interventions during outbreak response",
    Research_numbers == "6a" ~ "Develop in vitro and in vivo testing to identify candidates",
    Research_numbers == "6b" ~ "Evaluate efficacy and safety in prophylactic use",
    Research_numbers == "6c" ~ "Promote adequate supply of therapeutics showing efficacy",
    Research_numbers == "6d" ~ "Evaluate efficacy and safety of therapeutics through randomised clinical trials",
    Research_numbers == "6e" ~ "Investigate combination therapies",
    Research_numbers == "7a" ~ "Identification of candidates for clinical evaluation in addition to the ones already prioritized",
    Research_numbers == "7b" ~ "To develop and standardize animal models to evaluate the potential for vaccine effectiveness and to understand the potential for enhanced disease after vaccination. Results from animal models are expected to be important prior to large-scale efficacy studies and prior to studies in which enhanced disease is considered a significant possibility",
    Research_numbers == "7c" ~ "To develop and standardize assays to support vaccine development, particularly to support the evaluation of immune responses and to support clinical case definition. Basic reagents should be shared to accelerate the development of international standards and reference panels that will help support the development of ELISAs, pseudovirion neutralization and PCR assays",
    Research_numbers == "7d" ~ "To develop a multi-country Master Protocol for Phase 2b/Phase 3 vaccine evaluation to determine whether candidate vaccines are safe and effective before widespread distribution, using methodologically sound and ethically acceptable vaccine trial design. Vaccine efficacy trials should be done if such are feasible to implement",
    Research_numbers == "7e" ~ "To develop potency assays and manufacturing processes to rapidly enable the production of high quality large quantities of clinical grade and GMP materials",
    Research_numbers == "8a" ~ "Articulate and translate existing ethical standards to salient issues in COVID-19",
    Research_numbers == "8b" ~ "Sustained education, access, and capacity building",
    Research_numbers == "8c" ~ "The impact of restrictive public health measures (e.g., quarantine, isolation, cordon sanitaire)",
    Research_numbers == "8d" ~ "Public health communications and the ‘infodemic’; ensuring accurate and responsible Communications",
    Research_numbers == "8e" ~ "Ethical governance of global epidemic research",
    Research_numbers == "9a" ~ "Public Health - What are relevant, feasible,\neffective approaches to promote acceptance,\nuptake, and adherence to public health measures for COVID-19 prevention and control;\nand how can secondary impacts be rapidly identified and mitigated?",
    Research_numbers == "9b" ~ "(Clinical) care and health Systems - \nWhat are the relevant, acceptable and \nfeasible approaches for supporting the physical health and \npsychosocial needs of those providing care for COVID-19 patients?",
    Research_numbers == "9c" ~ "Media and communication - \nHow are individuals and communities communicating and \nmaking sense of COVID-19? \nWhat are the most effective ways to \naddress the underlying drivers of fear, anxieties, rumours, \nstigma regarding COVID-19, and improve public knowledge, awareness, and trust during the response?",
    Research_numbers == "9d" ~ "Engagement - What are the relevant, \nacceptable and feasible approaches for rapid engagement \nand good participatory practice that \nincludes communities in the public health response?",
    Research_numbers == "9e" ~ "Sexual and reproductive health - \nWhat are the relevant, acceptable and feasible \napproaches to communicating uncertainty regarding \nmother to child transmission of COVID-19, \nand possible sexual transmission?",
    Research_numbers == "9f" ~ "International cooperation - What international coordination \nmechanisms can optimize the international response to COVID-19?"))

# Reorder columns to place "description" in front of "Research_numbers"
WHO_sub_priority_alignment <- WHO_sub_priority_alignment %>%
  select(Research_numbers, description, everything())

# Remove the NA row
WHO_sub_priority_alignment_clean <- WHO_sub_priority_alignment [-45, ]

## Analysis of subpriority areas that do not map to any WHO research area

PAHO_COVID_Projects7 <- PAHO_COVID_Projects %>%
  # Split the 'primary research priorities' into multiple rows
  separate_rows(`PRIMARY.WHO.Research.Priority.Area.Names`, sep = ";") %>%
  mutate(`PRIMARY.WHO.Research.Priority.Area.Names` = trimws(`PRIMARY.WHO.Research.Priority.Area.Names`))

# Count N/A per priority area
NA_mapping <- PAHO_COVID_Projects7 %>%
  filter(`PRIMARY.WHO.Research.Sub-Priority.Number(s)` == "N/A") %>%
  group_by(`PRIMARY.WHO.Research.Priority.Area.Names`) %>%
  summarise(NA_count = n())

# delete row of NA
NA_mapping1 <- NA_mapping [-8, ]    

# reshape dataframe for merging
NA_mapping1 <- NA_mapping1 %>%
  mutate(Research_numbers = case_when(
    PRIMARY.WHO.Research.Priority.Area.Names == "Virus: natural history, transmission and diagnostics" ~ "1-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Animal and environmental research on the virus origin, and management measures at the human-animal interface" ~ "2-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Epidemiological studies" ~ "3-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Clinical characterization and management" ~ "4-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Infection prevention and control, including health care workers’ protection" ~ "5-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Candidate therapeutics R&D" ~ "6-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Candidate vaccines R&D" ~ "7-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Ethics considerations for research" ~ "8-NA",
    PRIMARY.WHO.Research.Priority.Area.Names == "Social sciences in the outbreak response" ~ "9-NA"))

NA_mapping1 <- NA_mapping1 %>%
  mutate(description = case_when(
    Research_numbers == "1-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "2-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "3-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "4-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "5-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "6-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "7-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "8-NA" ~ "does not map to WHO sub-priority",
    Research_numbers == "9-NA" ~ "does not map to WHO sub-priority"))

NA_mapping1 <- NA_mapping1 %>% rename(Primary_subpriority = NA_count)

# Reorder columns to place "description" in front of "Research_numbers"
NA_mapping1 <- NA_mapping1 %>%
  select(Research_numbers, description, Primary_subpriority, everything())


# Merge the two tables based on the common columns
WHO_sub_priority_alignment_final <- full_join(WHO_sub_priority_alignment_clean, NA_mapping1, by = c("Research_numbers", "description", "Primary_subpriority"))

# Reorder columns to keep the sceondary_subpriority
Secondary_subpriority <- names(WHO_sub_priority_alignment_clean)[4]
WHO_sub_priority_alignment_final <- WHO_sub_priority_alignment_final %>%
  select(-matches(Secondary_subpriority), everything(), Secondary_subpriority)

# Delete primary research priority column from final dataframe 
WHO_sub_priority_alignment_final <- WHO_sub_priority_alignment_final %>%
  select(-`PRIMARY.WHO.Research.Priority.Area.Names`)


# Arrange the dataframe in ascending order based on Research_numbers
WHO_sub_priority_alignment_final <- WHO_sub_priority_alignment_final %>%
  arrange(Research_numbers)

# Split the social science in outbreak response data for plotting

SSOR <- WHO_sub_priority_alignment_final %>% slice(47:53)

# Replace NA values with zero

SSOR <- SSOR %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

# Create a new column combining Research_numbers and description
SSOR <- SSOR %>%
  mutate(Subpriority = paste(Research_numbers, description, sep = ": "))

# Reverse the order of Subpriority levels
SSOR <- SSOR %>%
  mutate(Subpriority = factor(Subpriority, levels = rev(levels(factor(Subpriority)))))

# Reshape the data for plotting
SSOR_long <- SSOR %>%
  select(Subpriority, Primary_subpriority, Secondary_subpriority) %>%
  pivot_longer(cols = c(Primary_subpriority, Secondary_subpriority), 
               names_to = "Priority_type", values_to = "Count")

# Ensure the Priority_type column is a factor with the desired order
SSOR_long <- SSOR_long %>%
  mutate(Priority_type = factor(Priority_type, levels = c("Secondary_subpriority", "Primary_subpriority")))

# Plot the stacked bar chart
ggplot(SSOR_long, aes(x = Subpriority, y = Count, fill = Priority_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), position = position_stack(vjust = 0.5), size = 2.5) +
  scale_fill_manual(values = c("Primary_subpriority" = "#B0BDE0", "Secondary_subpriority" = "#1F78B4"), 
                    labels = c("Primary_subpriority" = "Primary subpriority area", "Secondary_subpriority" = "Secondary subpriority area"), 
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




# Split the animal and enviromental research data for plotting

AER <- WHO_sub_priority_alignment_final %>% slice(8:11)

# Replace NA values with zero

AER <- AER %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

# Create a new column combining Research_numbers and description
AER <- AER %>%
  mutate(Subpriority = paste(Research_numbers, description, sep = ": "))

# Reverse the order of Subpriority levels
AER <- AER %>%
  mutate(Subpriority = factor(Subpriority, levels = rev(levels(factor(Subpriority)))))

# Reshape the data for plotting
AER_long <- AER %>%
  select(Subpriority, Primary_subpriority, Secondary_subpriority) %>%
  pivot_longer(cols = c(Primary_subpriority, Secondary_subpriority), 
               names_to = "Priority_type", values_to = "Count")

# Ensure the Priority_type column is a factor with the desired order
AER_long <- AER_long %>%
  mutate(Priority_type = factor(Priority_type, levels = c("Secondary_subpriority", "Primary_subpriority")))

# Plot the stacked bar chart
ggplot(AER_long, aes(x = Subpriority, y = Count, fill = Priority_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), position = position_stack(vjust = 0.5), size = 2.5) +
  scale_fill_manual(values = c("Primary_subpriority" = "#B0BDE0", "Secondary_subpriority" = "#1F78B4"), 
                    labels = c("Primary_subpriority" = "Primary subpriority area", "Secondary_subpriority" = "Secondary subpriority area"), 
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


