library(tidyverse)
library(readxl)
library(dplyr)

##  Runs on Hotline Calls for Impact Report 2.0
##  Before loading this in, remove spaces in headers
##  find and load in the program dashboard file
all_data <- file.choose()

caller_data <- read_excel(all_data, sheet = 1) %>%
  group_by(ClientUid) %>%
  mutate(client_requested_shelter = if_else(max(
    if_else(
      NeedCodeDescription %in% c("Homeless Motel Vouchers", "Emergency Shelter", "Domestic Violence Shelters"), 1, 0)
  ) == 1, "Yes", "No"),
  veteran = if_else(max(
    if_else(
      ClientVeteranStatus == "Yes (HUD)", 1, 0)
  ) == 1, "Yes", "No")) %>%
  ungroup() %>%
  select(NeedHouseholdId, ClientUid, Age, client_requested_shelter, veteran) %>%
  distinct() %>%
  group_by(NeedHouseholdId) %>%
  mutate(household_size = n(),
         max_age = max(Age),
         min_age = min(Age)) %>%
  arrange(desc(household_size)) %>%
  ungroup() %>%
  group_by(ClientUid) %>%
  slice(1L) %>%
  ungroup()

all_calling <- nrow(distinct(caller_data, NeedHouseholdId))

all_for_shelter <- nrow(distinct(caller_data %>%
                               filter(client_requested_shelter == "Yes")
                             , NeedHouseholdId))

families <- nrow(distinct(caller_data %>%
                                   filter(min_age < 18)
                                 , NeedHouseholdId))

families_for_shelter <- nrow(distinct(caller_data %>%
                                filter(min_age < 18 &
                                         client_requested_shelter == "Yes")
                              , NeedHouseholdId))

veterans <- nrow(distinct(caller_data %>%
                            filter(veteran == "Yes")
                          , ClientUid))

veterans_for_shelter <- nrow(distinct(caller_data %>%
                                        filter(veteran == "Yes" &
                                                 client_requested_shelter == "Yes")
                                      , ClientUid))

youth <- nrow(distinct(caller_data %>%
                                filter(max_age >= 18 &
                                         max_age <= 24)
                              , NeedHouseholdId))

youth_for_shelter <- nrow(distinct(caller_data %>%
                                            filter(max_age >= 18 &
                                                     max_age <= 24 &
                                                     client_requested_shelter == "Yes")
                                          , NeedHouseholdId))

seniors <- nrow(distinct(caller_data %>%
                            filter(Age >= 55)
                          , ClientUid))

seniors_for_shelter <- nrow(distinct(caller_data %>%
                                        filter(Age >= 55 &
                                                 client_requested_shelter == "Yes")
                                      , ClientUid))

cat("\nAll households calling: ", paste0(all_calling),
    "\nAll households calling for shelter: ", paste0(all_for_shelter),
    "\nFamilies calling: ", paste0(families),
    "\nFamilies calling for shelter: ", paste0(families_for_shelter),
    "\nVeterans calling: ", paste0(veterans),
    "\nVeterans calling for shelter: ", paste0(veterans_for_shelter),
    "\nYouth households calling: ", paste0(youth),
    "\nYouth households calling for shelter: ", paste0(youth_for_shelter),
    "\nSeniors calling: ", paste0(seniors),
    "\nSeniors calling for shelter: ", paste0(seniors_for_shelter), "\n")
