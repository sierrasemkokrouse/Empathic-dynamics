#### Cleaning ####

#### Packages ####
library(tidyverse)
library(psych)

#### Session 1 Data ####
data <- read.csv("2023-Fall_Workplace-Intervention_Session1_August+28,+2024_15.05.csv")

data <- data[-c(1:8),]

data <- data %>% 
  select(-c(StartDate, EndDate, Status, IPAddress, Finished, RecipientEmail, RecipientFirstName, RecipientLastName, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage))

data <- data %>% 
  mutate(schoolDivision = case_when(schoolDivision == "1" ~ "Goldman", 
                                    schoolDivision == "2" ~ "Law",
                                    schoolDivision == "3" ~ "Haas",
                                    schoolDivision == "4" ~ "UDAR",
                                    schoolDivision == "5" ~ "Student Affairs",
                                    schoolDivision == "6" ~ "Social Welfare",
                                    schoolDivision == "7" ~ "Education",
                                    schoolDivision == "8" ~ "Athletics",
                                    schoolDivision == "9" ~ "Equity & Inclusion"))

data <- data %>% 
  mutate(academic = case_when(academic == "1" ~ "Academic",
                              academic == "2" ~ "Staff"))

data <- data %>% 
  mutate(manager = case_when(manager == "1" ~ "Yes",
                             manager == "2" ~ "No"))


data <- data %>% 
  mutate(race = case_when(race == "1" ~ "African American / Black", 
                          race == "2" ~ "American Indian / Alaskan Native",
                          race == "3" ~ "Chinese",
                          race == "4" ~ "Filipino",
                          race == "5" ~ "Hispanic / Latino",
                          race == "6" ~ "Japanese",
                          race == "7" ~ "Korean",
                          race == "8" ~ "Middle Eastern / Southwest Asian / North African (SWANA)",
                          race == "9" ~ "Other Asian",
                          race == "10" ~ "Pacific Islander", 
                          race == "11" ~ "South Asian",
                          race == "12" ~ "Two or more races",
                          race == "13" ~ "Vietnamese",
                          race == "14" ~ "White"))

data <- data %>% 
  mutate(URM = case_when(race == "African American / Black" ~ "Yes",
                         race == "American Indian / Alaskan Native" ~ "Yes",
                         race == "Chinese" ~ "No",
                         race == "Filipino" ~ "No",
                         race == "Hispanic / Latino" ~ "Yes",
                         race == "Japanese" ~ "No",
                         race == "Korean" ~ "No",
                         race == "Middle Eastern / Southwest Asian / North African (SWANA)" ~ "Yes",
                         race == "Other Asian" ~ "No",
                         race == "Pacific Islander" ~ "Yes", 
                         race == "South Asian" ~ "No",
                         race == "Two or more races" ~ "No",
                         race == "Vietnamese" ~ "No",
                         race == "White" ~ "No"))


data <- data %>% 
  mutate(gender = case_when(gender == "1" ~ "Female", 
                            gender == "2" ~ "Male",
                            gender == "3" ~ "Trans Female / Trans Woman",
                            gender == "4" ~ "Trans Male / Trans Man",
                            gender == "5" ~ "Genderqueer or Nonbinary Gender",
                            gender == "6" ~ "Different identity"))


data <- data %>% 
  mutate(ladder = as.numeric(ladder))

data <- data %>% 
  mutate(income = case_when(income == "1" ~ "less than $15,000", 
                            income == "2" ~ "$15,000 - $30,000",
                            income == "3" ~ "$30,001 - $50,000",
                            income == "4" ~ "$50,001 - $75,000",
                            income == "5" ~ "$75,001 - $100,000",
                            income == "6" ~ "$100,001 - $150,000",
                            income == "7" ~ "greater than $150,000"))

data <- data %>% 
  select(-c(timer_First.Click:timer_Click.Count.1,
            timer_First.Click.2:timer_Click.Count.2,
            timer_First.Click.3:timer_Click.Count.5, 
            timer_First.Click.6:timer_Click.Count.6,
            timer_First.Click.7:timer_Click.Count.7,
            timer_First.Click.8:timer_Click.Count.8, 
            timer_First.Click.9:timer_Click.Count.9,
            timer_First.Click.10:timer_Click.Count.10,
            scenario1Timer_First.Click:scenario1Timer_Click.Count,
            scenario2Timer_First.Click:scenario2Timer_Click.Count))

data <- data %>% 
  mutate(yearsWork = as.numeric(yearsWork),
         managerHowMany = as.numeric(managerHowMany),
         SOW_2_1 = as.numeric(SOW_2_1),
         SOW_2_2 = as.numeric(SOW_2_2),
         SOW_2_3 = as.numeric(SOW_2_3),
         SOW_2_4 = as.numeric(SOW_2_4),
         SOW_2_5 = as.numeric(SOW_2_5),
         manipTreat1 = as.numeric(manipTreat1),
         manipTreat2 = as.numeric(manipTreat2),
         manipControl = as.numeric(manipControl),
         IRI_PT_1 = as.numeric(IRI_PT_1),
         IRI_PT_2 = as.numeric(IRI_PT_2),
         IRI_PT_3 = as.numeric(IRI_PT_3),
         IRI_PT_4 = as.numeric(IRI_PT_4),
         burnout_1_x = as.numeric(burnout_1_x),
         burnout_1_y = as.numeric(burnout_1_y),
         burnout = as.numeric(burnout),
         attributions_1 = as.numeric(attributions_1),
         IRI_PT1_1 = as.numeric(IRI_PT1_1),
         IRI_PT2_1 = as.numeric(IRI_PT2_1),
         hardToWorkWith_1 = as.numeric(hardToWorkWith_1),
         valued_1 = as.numeric(valued_1),
         X360_1 = as.numeric(X360_1),
         complaint_1 = as.numeric(complaint_1),
         attributions_2 = as.numeric(attributions_2),
         IRI_PT1_2 = as.numeric(IRI_PT1_2),
         IRI_PT2_2 = as.numeric(IRI_PT2_2),
         X360_2 = as.numeric(X360_2),
         complaint_2 = as.numeric(complaint_2),
         engagement = as.numeric(engagement),
         trust_SOW_2 = as.numeric(trust_SOW_2),
         trustLeader_SOW_2 = as.numeric(trustLeader_SOW_2),
         session2 = as.numeric(session2),
         attentionCheck = as.numeric(attentionCheck))

data <- data %>% 
  mutate(manipTreat2_recode = case_match(manipTreat2, 
                                  1 ~ 5,
                                  2 ~ 4,
                                  3 ~ 3,
                                  4 ~ 2,
                                  5 ~ 1))

data$attributionsDiff <- data$attributions_1 - data$attributions_2

data$IRI_PT1Diff <- data$IRI_PT1_1 - data$IRI_PT1_2

data$IRI_PT2Diff <- data$IRI_PT2_1 - data$IRI_PT2_2

data <- data %>% 
  mutate(valued_1_recode = case_match(valued_1, 
                                         1 ~ 5,
                                         2 ~ 4,
                                         3 ~ 3,
                                         4 ~ 2,
                                         5 ~ 1))

#### Creating data collection wave variable ####
data <- data %>% 
  mutate(wave = case_when(schoolDivision == "Education" ~ 2,
                          schoolDivision == "Goldman" ~ 1,
                          schoolDivision == "Haas" ~ 1,
                          schoolDivision == "Law" ~ 1,
                          schoolDivision == "Social Welfare" ~ 2,
                          schoolDivision == "Student Affairs" ~ 2,
                          schoolDivision == "UDAR" ~ 1))

#### Removing unassigned participants ####
data <- data %>% 
  filter(Condition != "")

#### Writing data ####
write_csv(data, "clean_session1_20240910.csv")

#### Session 2 Data ####
session2 <- read.csv("2023-Fall_Workplace-Intervention_Session2_August+27,+2024_16.55.csv")

session2 <- session2 %>% 
  select(-c(StartDate, EndDate, Status, IPAddress, Finished, RecipientEmail, RecipientFirstName, RecipientLastName, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage))

session2 <- session2[-c(1:2),]

names(session2) <- paste(names(session2), "S2", sep="_")

session2 <- session2 %>% 
  mutate(workerRelations1_S2_recode = case_match(workerRelations1_S2, 
                                         "1" ~ 5,
                                         "2" ~ 4,
                                         "3" ~ 3,
                                         "4" ~ 2,
                                         "5" ~ 1))

session2 <- session2 %>% 
  mutate(workerRelations2_S2_recode = case_match(workerRelations2_S2, 
                                                 "1" ~ 5,
                                                 "2" ~ 4,
                                                 "3" ~ 3,
                                                 "4" ~ 2,
                                                 "5" ~ 1))

session2 <- session2 %>% 
  mutate(workerRelations3_S2_recode = case_match(workerRelations3_S2, 
                                                 "1" ~ 5,
                                                 "2" ~ 4,
                                                 "3" ~ 3,
                                                 "4" ~ 2,
                                                 "5" ~ 1))

#### Joining Data ####
fullData <- full_join(data, 
                      session2, 
                      by = join_by("email" == "email_S2"))

write_csv(fullData, "clean_fulldata_20240910.csv")
