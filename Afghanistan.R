
library(tidyverse)
library(ggplot2)
library(haven)
library(dplyr)
library(broom)
library(gmodels)
library(poliscidata)
library(corrplot)
library(Hmisc)

#Load quantitative data sheets and check column names

AfghanistanData_Quant <- read_csv("~/Documents/GitHub/Afghanistan/Afghanistan_Survey.csv")
afghan_data <- AfghanistanData_Quant
afghan_7 <- read_csv("~/Documents/GitHub/Afghanistan/AfghanistanData_7.csv")
afghan_10a <- read_csv("~/Documents/GitHub/Afghanistan/AfghanistanData_10a.csv")
afghan_10b <- read_csv("~/Documents/GitHub/Afghanistan/AfghanistanData_10b.csv")
afghan_10c <- read_csv("~/Documents/GitHub/Afghanistan/AfghanistanData_10c.csv")
afghan_11a <- read_csv("~/Documents/GitHub/Afghanistan/AfghanistanData_11a.csv")
afghan_17 <- read_csv("~/Documents/GitHub/Afghanistan/AfghanistanData_17.csv")

names(afghan_data)

#Create master list of all UUIDs who have answered one of the open-ends

data_joined_7 <- full_join(afghan_data, afghan_7, by="UUID")
data_joined_10a <- full_join(data_joined_7, afghan_10a, by="UUID")
data_joined_10b <- full_join(data_joined_10a, afghan_10b, by="UUID")
data_joined_10c <- full_join(data_joined_10b, afghan_10c, by="UUID")
data_joined_11a <- full_join(data_joined_10c, afghan_11a, by="UUID")
data_joined_full <- full_join(data_joined_11a, afghan_17, by="UUID")

#Frequency distribution of question 9, 
#'How much would you agree or disagree with the following sentence: “I believe achieving human rights for women is among the top priorities for the future of my country”?'

freq(data_joined_full$q09_human_rights_women)
freq(data_joined_full$q22_religion_importance)

#Analytical Setup

#Creating variables combining responses to Q9 and responses to Q22 (religiosity)

data_joined_full <- data_joined_full %>% 
  mutate(religiosity_rights = case_when(
    q09_human_rights_women==1 & q22_religion_importance==4 ~ "Religiosity 4, Strongly Disagree",
    q09_human_rights_women==2 & q22_religion_importance==4 ~ "Religiosity 4, Disagree",
    q09_human_rights_women==3 & q22_religion_importance==4 ~ "Religiosity 4, Undecided",
    q09_human_rights_women==4 & q22_religion_importance==4 ~ "Religiosity 4, Agree",
    q09_human_rights_women==5 & q22_religion_importance==4 ~ "Religiosity 4, Strongly Agree",
    q09_human_rights_women==1 & q22_religion_importance==3 ~ "Religiosity 3, Strongly Disagree",
    q09_human_rights_women==2 & q22_religion_importance==3 ~ "Religiosity 3, Disagree",
    q09_human_rights_women==3 & q22_religion_importance==3 ~ "Religiosity 3, Undecided",
    q09_human_rights_women==4 & q22_religion_importance==3 ~ "Religiosity 3, Agree",
    q09_human_rights_women==5 & q22_religion_importance==3 ~ "Religiosity 3, Strongly Agree",
    q09_human_rights_women==1 & q22_religion_importance==2 ~ "Religiosity 2, Strongly Disagree",
    q09_human_rights_women==2 & q22_religion_importance==2 ~ "Religiosity 2, Disagree",
    q09_human_rights_women==3 & q22_religion_importance==2 ~ "Religiosity 2, Undecided",
    q09_human_rights_women==4 & q22_religion_importance==2 ~ "Religiosity 2, Agree",
    q09_human_rights_women==5 & q22_religion_importance==2 ~ "Religiosity 2, Strongly Agree",
    q09_human_rights_women==1 & q22_religion_importance==1 ~ "Religiosity 1, Strongly Disagree",
    q09_human_rights_women==2 & q22_religion_importance==1 ~ "Religiosity 1, Disagree",
    q09_human_rights_women==3 & q22_religion_importance==1 ~ "Religiosity 1, Undecided",
    q09_human_rights_women==4 & q22_religion_importance==1 ~ "Religiosity 1, Agree",
    q09_human_rights_women==5 & q22_religion_importance==1 ~ "Religiosity 1, Strongly Agree"
  ))


freq(data_joined_full$religiosity_rights)


