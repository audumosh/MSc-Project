## Install and load neccesary packages
install.packages("tidyverse")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)


## Read excel file
PAHO_COVID_Projects <- read.xlsx(xlsxFile = "COVID-19-Research-Project-Tracker-PAHO-foR.xlsx")
