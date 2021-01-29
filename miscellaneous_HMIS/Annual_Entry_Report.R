library(tidyverse)
library(lubridate)
library(readxl)
library(pander)
library(dplyr)

##  update these to the current report year dates
year_start <- ymd("20200101")
year_end <- ymd("20201231")

##  Runs on Program Dashboard v1.5
##  Before loading this in, remove spaces in headers
##  find and load in the program dashboard file
all_data <- file.choose()

ee_during_period <- read_excel(all_data, sheet = 1) %>%
  rename(ClientId = EntryExitClientId) %>%
  group_by(EntryExitGroupUID) %>%
  mutate(EntryExitExitDate = as.Date(EntryExitExitDate),
         max_entry_age = max(ClientAgeatEntry),
         min_entry_age = min(ClientAgeatEntry)) %>%
  ungroup()

demographic_data <- read_excel(all_data, sheet = 2) %>%
  rename(ClientId = ClientUid)

program_data <- read_excel(all_data, sheet = 3)

##  set up system map calculations
system_map_numbers <- function(df, column, title)
{
  entered_shelter <- nrow(distinct(df %>%
                                     filter(EntryExitProviderProgramTypeCode == "Emergency Shelter (HUD)" &
                                              EntryExitEntryDate >= year_start &
                                              EntryExitEntryDate <= year_end)
                                   , !!column))
  entered_diversion <- nrow(distinct(df %>%
                                     filter(EntryExitProviderProgramTypeCode == "Diversion" &
                                              EntryExitEntryDate >= year_start &
                                              EntryExitEntryDate <= year_end)
                                   , !!column))
  diverted <- nrow(distinct(df %>%
                                     filter(EntryExitProviderProgramTypeCode == "Diversion" &
                                              ExitDestinationType == "Permanent Situation" &
                                              EntryExitExitDate >= year_start &
                                              EntryExitExitDate <= year_end)
                                   , !!column))
  in_ce <- nrow(distinct(df %>%
                              filter(EntryExitProviderProgramTypeCode == "Coordinated Entry (HUD)")
                            , !!column))
  entered_housing_program <- nrow(distinct(df %>%
                                     filter(EntryExitProviderProgramTypeCode %in% c("Transitional housing (HUD)",
                                                                                    "PH - Rapid Re-Housing (HUD)",
                                                                                    "PH - Permanent Supportive Housing (disability required for entry) (HUD)") &
                                              EntryExitEntryDate >= year_start &
                                              EntryExitEntryDate <= year_end)
                                   , !!column))
  homelessness_prevented <- nrow(distinct(df %>%
                              filter(EntryExitProviderProgramTypeCode == "Homelessness Prevention (HUD)" &
                                       ExitDestinationType == "Permanent Situation" &
                                       EntryExitExitDate >= year_start &
                                       EntryExitExitDate <= year_end)
                            , !!column))
  
  cat("\n", title,
      "\nEntered shelter: ", paste0(entered_shelter),
      "\nEntered diversion: ", paste0(entered_diversion),
      "\nHoused through diversion: ", paste0(diverted),
      "\nWere open in CE: ", paste0(in_ce),
      "\nEntered TH/RRH/PSH program: ", paste0(entered_housing_program),
      "\nStabilized through prevention: ", paste0(homelessness_prevented), "\n")
}

##  run system number calculations
system_map_numbers(ee_during_period, quo(VARCreatedHouseholdID), "All Households")

system_map_numbers(ee_during_period %>%
                     filter(min_entry_age < 18)
                   , quo(VARCreatedHouseholdID), "Families With Children")

system_map_numbers(ee_during_period %>%
                     inner_join(demographic_data %>%
                                  filter(VeteranStatus == "Yes (HUD)"),
                                by = "ClientId")
                   , quo(ClientId), "Veterans")

system_map_numbers(ee_during_period %>%
                     filter(max_entry_age >= 18 &
                              max_entry_age <= 24)
                   , quo(VARCreatedHouseholdID), "Youth Households")

system_map_numbers(ee_during_period %>%
                     filter(ClientAgeatEntry >= 55)
                   , quo(ClientId), "Seniors 55+")

system_map_numbers(ee_during_period %>%
                     filter(ClientAgeatEntry >= 18) %>%
                     inner_join(demographic_data %>%
                                  filter(PrimaryRace %in% c("Black or African American (HUD)", "Native Hawaiian or Other Pacific Islander (HUD)",
                                                           "Other Multi-Racial", "American Indian or Alaska Native (HUD)", "Other", "Asian (HUD)") |
                                           SecondaryRace %in% c("Black or African American (HUD)", "Native Hawaiian or Other Pacific Islander (HUD)",
                                                              "Other Multi-Racial", "American Indian or Alaska Native (HUD)", "Other", "Asian (HUD)") |
                                           Ethnicity == "Hispanic/Latino (HUD)"),
                                by = "ClientId")
                   , quo(VARCreatedHouseholdID), "BIPOC Households")

##  get program numbers
housing_program_entries <- ee_during_period %>%
  filter(EntryExitProviderProgramTypeCode %in% c("Transitional housing (HUD)",
                                                 "PH - Rapid Re-Housing (HUD)",
                                                 "PH - Permanent Supportive Housing (disability required for entry) (HUD)") &
           EntryExitEntryDate >= year_start &
           EntryExitEntryDate <= year_end) %>%
  left_join(program_data, by = c("EntryExitProviderId" = "Provider"))

agency_count <- nrow(distinct(housing_program_entries, Agency))
program_count <- nrow(distinct(housing_program_entries, EntryExitProviderId))
household_count <- nrow(distinct(housing_program_entries, VARCreatedHouseholdID))  ## this will match number from above

cat(paste0(agency_count), "agencies administered", paste0(program_count),
    "programs that entered", paste0(household_count), "households in 2020.")

housing_program_types <- housing_program_entries %>%
  select(EntryExitProviderId, EntryExitProviderProgramTypeCode) %>%
  distinct() %>%
  group_by(EntryExitProviderProgramTypeCode) %>%
  summarise(programs = n())

