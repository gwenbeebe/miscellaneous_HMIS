library(tidyverse)
library(readxl)
library(dplyr)

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

##  Runs on Hotline Calls for Impact Report 2.1
##  Before loading this in, remove spaces in headers
##  and commas in household IDs on first tab
##  find and load in the program dashboard file
all_data <- file.choose()

hmis_call_data <- read_excel(all_data, sheet = 1) %>%
  `colnames<-`(gsub(" ", "", names(.))) %>%
  mutate(NeedDateAdded = as.Date(NeedDateAdded)) %>%
  filter(ClientUid != 26860)

hmis_living_situations <- read_excel(all_data, sheet = 2) %>%
`colnames<-`(gsub(" ", "", names(.)))

hmis_demographics <- read_excel(all_data, sheet = 3) %>%
  `colnames<-`(gsub(" ", "", names(.))) %>%
  mutate(DateofBirth = as.Date(DateofBirth))

call_data <- read_excel(all_data, sheet = 4) %>%
  `colnames<-`(gsub(" ", "", names(.))) %>%
  mutate(CallRecordStartDate = as.Date(CallRecordStartDate),
         DateofBirth = as.Date(DateofBirth)) %>%
  filter(ClientID != 26860 & !is.na(NameString))


##  process non-CallPoint data
hmis_caller_data <- hmis_call_data %>%
  left_join(hmis_living_situations, by = c("ClientUid", "NeedDateAdded" = "EventDate")) %>%
  left_join(hmis_demographics, by = "ClientUid") %>%
  mutate(client_requested_shelter = if_else(
    NeedCodeDescription %in% c("Homeless Motel Vouchers", "Emergency Shelter", "Domestic Violence Shelters"), 1, 0),
    Housed_Status = case_when(
           ## identify those that are definitely literally homeless
           NeedCodeDescription == "Coordinated Entry (all literally homeless)" |
             # FleeingDV == "Yes" |
             PriorLivingSituation %in% c("Place not meant for habitation (HUD)", 
                                         "Emergency shelter, incl. hotel/motel paid for w/ ES voucher, or RHY-funded Host Home shelter (HUD)",
                                         "Domestic Violence Situation", "Homeless/on the Street", "Safe Haven (HUD)",
                                         "Transitional housing for homeless persons (including homeless youth) (HUD)")
           ~ "Literally Homeless",
           ## identify those that are definitely housed
           NeedCodeDescription %in% c("Prevention - Available (at-risk of homelessness, may refer)",
                                      "Prevention - Not Available (at-risk of homelessness, no referral possible)",
                                      "Prevention - ERAP (CARES money)",
                                      "Information Only (not literally homeless or at-risk)") |
             PriorLivingSituation %in% c("Jail, prison or juvenile detention facility (HUD)", "Psychiatric hospital or other psychiatric facility (HUD)",
                                         "Substance abuse treatment facility or detox center (HUD)", "Rental by client, no ongoing housing subsidy (HUD)",
                                         "Staying or living in a friend's room, apartment or house (HUD)", 
                                         "Staying or living in a family member's room, apartment or house (HUD)",
                                         "Owned by client, no ongoing housing subsidy (HUD)", "Rental by client, with VASH housing subsidy (HUD)",
                                         "Rental by client, with other ongoing housing subsidy (HUD)",
                                         "Hotel or motel paid for without emergency shelter voucher (HUD)",
                                         "Residential project or halfway house with no homeless criteria (HUD)",
                                         "Rental by client in a public housing unit (HUD)",
                                         "Hospital or other residential non-psychiatric medical facility (HUD)",
                                         "Foster care home or foster care group home (HUD)", "Rental by client, with RRH or equivalent subsidy (HUD)",
                                         "Owned by client, with ongoing housing subsidy (HUD)", 
                                         "Permanent housing (other than RRH) for formerly homeless persons (HUD)",
                                         "Rental by client, with HCV voucher (tenant or project based) (HUD)",
                                         "Long-term care facility or nursing home (HUD)", "Group home (not foster care)",
                                         "Rental by client, with GPD TIP housing subsidy (HUD)", "Interim Housing (HUD) (Retired)")
           ~ "Not Literally Homeless",
           ## identify those that are probably literally homeless
           # NeedCodeDescription %in% c("Housing Related Coordinated Entry", "Emergency Shelter", "Homeless Motel Vouchers",
           #                            "Homeless Safe Parking Programs", "Domestic Violence Shelters",
           #                            ## "Animal Shelters" is an artifact of how some shelter referrals are made
           #                            "Animal Shelters")
           # ~ "Literally Homeless",
           ## identify those that are probably housed
           NeedCodeDescription %in% c("Rent Payment Assistance", "Homelessness Prevention Programs")
           ~ "Not Literally Homeless",
           ## all others are unknown but marked not literal for these purposes
           TRUE ~ "Not Literally Homeless"
         ),
         BIPOC_flag = case_when(PrimaryRace %in% c("Native Hawaiian or Pacific Islander (HUD)",
                                                   "Black, African American, or African (HUD)",
                                                   "Other Multi-Racial", "Other",
                                                   "American Indian, Alaska Native, or Indigenous (HUD)",
                                                   "Asian or Asian American (HUD)") |
                           SecondaryRace %in% c("Native Hawaiian or Pacific Islander (HUD)",
                                                "Black, African American, or African (HUD)",
                                                "Other Multi-Racial", "Other",
                                                "American Indian, Alaska Native, or Indigenous (HUD)",
                                                "Asian or Asian American (HUD)") |
                           Ethnicity == "Hispanic/Latin(a)(o)(x) (HUD)" ~ 1, 
                           TRUE ~ 0)) %>%
  select(NeedHouseholdId, ClientUid, NeedDateAdded, ClientVeteranStatus, NameString, DateofBirth, Housed_Status, BIPOC_flag, client_requested_shelter)


##  process CallPoint data
callpoint_caller_data <- call_data %>%
  mutate(Housed_Status = case_when(
    ## identify those that are definitely literally homeless
    CallRecordCallType == "Coordinated Entry (all literally homeless)" |
      # FleeingDV == "Yes" |
      PriorLivingSituation %in% c("Place not meant for habitation (HUD)", 
                                  "Emergency shelter, incl. hotel/motel paid for w/ ES voucher, or RHY-funded Host Home shelter (HUD)",
                                  "Domestic Violence Situation", "Homeless/on the Street", "Safe Haven (HUD)",
                                  "Transitional housing for homeless persons (including homeless youth) (HUD)")
    ~ "Literally Homeless",
    ## identify those that are definitely housed
    CallRecordCallType %in% c("Prevention - Available (at-risk of homelessness, may refer)",
                               "Prevention - Not Available (at-risk of homelessness, no referral possible)",
                               "Prevention - ERAP (CARES money)",
                               "Information Only (not literally homeless or at-risk)") |
      PriorLivingSituation %in% c("Jail, prison or juvenile detention facility (HUD)", "Psychiatric hospital or other psychiatric facility (HUD)",
                                  "Substance abuse treatment facility or detox center (HUD)", "Rental by client, no ongoing housing subsidy (HUD)",
                                  "Staying or living in a friend's room, apartment or house (HUD)", 
                                  "Staying or living in a family member's room, apartment or house (HUD)",
                                  "Owned by client, no ongoing housing subsidy (HUD)", "Rental by client, with VASH housing subsidy (HUD)",
                                  "Rental by client, with other ongoing housing subsidy (HUD)",
                                  "Hotel or motel paid for without emergency shelter voucher (HUD)",
                                  "Residential project or halfway house with no homeless criteria (HUD)",
                                  "Rental by client in a public housing unit (HUD)",
                                  "Hospital or other residential non-psychiatric medical facility (HUD)",
                                  "Foster care home or foster care group home (HUD)", "Rental by client, with RRH or equivalent subsidy (HUD)",
                                  "Owned by client, with ongoing housing subsidy (HUD)", 
                                  "Permanent housing (other than RRH) for formerly homeless persons (HUD)",
                                  "Rental by client, with HCV voucher (tenant or project based) (HUD)",
                                  "Long-term care facility or nursing home (HUD)", "Group home (not foster care)",
                                  "Rental by client, with GPD TIP housing subsidy (HUD)", "Interim Housing (HUD) (Retired)")
    ~ "Not Literally Homeless",
    ## all others are unknown but marked not literal for these purposes
    TRUE ~ "Not Literally Homeless"
  ),
  BIPOC_flag = case_when(PrimaryRace %in% c("Native Hawaiian or Pacific Islander (HUD)",
                                            "Black, African American, or African (HUD)",
                                            "Other Multi-Racial", "Other",
                                            "American Indian, Alaska Native, or Indigenous (HUD)",
                                            "Asian or Asian American (HUD)") |
                      SecondaryRace %in% c("Native Hawaiian or Pacific Islander (HUD)",
                                           "Black, African American, or African (HUD)",
                                           "Other Multi-Racial", "Other",
                                           "American Indian, Alaska Native, or Indigenous (HUD)",
                                           "Asian or Asian American (HUD)") |
                      Ethnicity == "Hispanic/Latin(a)(o)(x) (HUD)" ~ 1, 
                    TRUE ~ 0)) %>%
  select(ClientID, CallRecordStartDate, VeteranStatus, NameString, DateofBirth, Housed_Status, BIPOC_flag)

##  join data for more processing
all_caller_data <- hmis_caller_data %>%
  full_join(callpoint_caller_data, by = c("ClientUid" = "ClientID", "NeedDateAdded" = "CallRecordStartDate",
                                          "ClientVeteranStatus" = "VeteranStatus", "NameString", "DateofBirth",
                                          "Housed_Status", "BIPOC_flag")) %>%
  group_by(NameString) %>%
  mutate(DateofBirth = if_else(is.na(DateofBirth),
                               max(if_else(is.na(DateofBirth), ymd("18000101"), DateofBirth)),
                               DateofBirth),
         DateofBirth = case_when(DateofBirth != ymd("18000101") ~ DateofBirth)) %>%
  group_by(NameString, DateofBirth) %>%
  ## resolve any duplicate client IDs
  mutate(HMIS_ClientID = min(if_else(ClientUid > 0, ClientUid, 999999)),
         HMIS_ClientID = case_when(HMIS_ClientID != 999999 ~ HMIS_ClientID),
         First_Call_ClientID = max(ClientUid),
         ClientUid = if_else(is.na(HMIS_ClientID), First_Call_ClientID, HMIS_ClientID)) %>%
  ungroup()  %>%
  mutate(NeedHouseholdId = if_else(is.na(NeedHouseholdId), paste0(ClientUid, "-C"), NeedHouseholdId)) %>%
  group_by(ClientUid) %>%
  mutate(veteran = if_else(max(
    if_else(
      ClientVeteranStatus == "Yes (HUD)", 1, 0)
  ) == 1, "Yes", "No"),
  BIPOC_flag = max(BIPOC_flag),
  client_requested_shelter = max(case_when(!is.na(client_requested_shelter) ~ client_requested_shelter,
                                           TRUE ~ 0)),
  Age = age(DateofBirth, NeedDateAdded)) %>%
  ungroup() %>%
  select(-c(ClientVeteranStatus, NameString, HMIS_ClientID, First_Call_ClientID, DateofBirth))

check <- all_caller_data %>%
  select(NameString, DateofBirth, Housed_Status) %>%
  distinct() %>%
  group_by(Housed_Status) %>%
  summarise(count = n())

##  prepare table for summarizing
for_summary <- all_caller_data %>%
  select(-NeedDateAdded) %>%
  distinct() %>%
  group_by(NeedHouseholdId, Housed_Status) %>%
  mutate(household_size = n()) %>%
  ungroup() %>%
  group_by(NeedHouseholdId) %>%
  mutate(max_age = max(if_else(is.na(Age), -99, Age)),
         max_age = case_when(max_age != -99 ~ max_age),
         min_age = min(if_else(is.na(Age), 99, Age)),
         min_age = case_when(min_age != 99 ~ min_age),
         client_requested_shelter = case_when(client_requested_shelter == 1 ~ "Yes",
                                              TRUE ~ "No")
         # , Housed_Status = factor(Housed_Status,
         #                        levels = c("Literally Homeless", "Not Literally Homeless", "Unknown"))) %>%
         #                        levels = c("Not Literally Homeless", "Literally Homeless", "Unknown"))
         ) %>%
  arrange(desc(household_size), Housed_Status) %>%
  ungroup() %>%
  group_by(ClientUid) %>%
  slice(1L) %>%
  ungroup()


##  set up summary calculations
hotline_summary_numbers <- function(df, title)
{
  all_calling <- nrow(distinct(df, NeedHouseholdId))
  
  all_for_shelter <- nrow(distinct(df %>%
                                     filter(client_requested_shelter == "Yes")
                                   , NeedHouseholdId))
  
  families <- nrow(distinct(df %>%
                              filter(min_age < 18)
                            , NeedHouseholdId))
  
  families_for_shelter <- nrow(distinct(df %>%
                                          filter(min_age < 18 &
                                                   client_requested_shelter == "Yes")
                                        , NeedHouseholdId))
  
  veterans <- nrow(distinct(df %>%
                              filter(veteran == "Yes")
                            , ClientUid))
  
  veterans_for_shelter <- nrow(distinct(df %>%
                                          filter(veteran == "Yes" &
                                                   client_requested_shelter == "Yes")
                                        , ClientUid))
  
  youth <- nrow(distinct(df %>%
                           filter(max_age >= 18 &
                                    max_age <= 24)
                         , NeedHouseholdId))
  
  youth_for_shelter <- nrow(distinct(df %>%
                                       filter(max_age >= 18 &
                                                max_age <= 24 &
                                                client_requested_shelter == "Yes")
                                     , NeedHouseholdId))
  
  seniors <- nrow(distinct(df %>%
                             filter(Age >= 55)
                           , ClientUid))
  
  seniors_for_shelter <- nrow(distinct(df %>%
                                         filter(Age >= 55 &
                                                  client_requested_shelter == "Yes")
                                       , ClientUid))
  
  BIPOC <- nrow(distinct(df %>%
                           filter((max_age >= 18 |
                                     is.na(max_age)) &
                                    BIPOC_flag == 1)
                         , NeedHouseholdId))
  
  BIPOC_for_shelter <- nrow(distinct(df %>%
                                       filter((max_age >= 18 |
                                                 is.na(max_age)) &
                                                BIPOC_flag == 1 &
                                                client_requested_shelter == "Yes")
                                     , NeedHouseholdId))
  
  cat("\n", title,
      "\nAll households calling: ", paste0(all_calling),
      "\nAll households calling for shelter: ", paste0(all_for_shelter),
      "\nFamilies calling: ", paste0(families),
      "\nFamilies calling for shelter: ", paste0(families_for_shelter),
      "\nVeterans calling: ", paste0(veterans),
      "\nVeterans calling for shelter: ", paste0(veterans_for_shelter),
      "\nYouth households calling: ", paste0(youth),
      "\nYouth households calling for shelter: ", paste0(youth_for_shelter),
      "\nSeniors calling: ", paste0(seniors),
      "\nSeniors calling for shelter: ", paste0(seniors_for_shelter), 
      "\nBIPOC households calling: ", paste0(BIPOC),
      "\nBIPOC households calling for shelter: ", paste0(BIPOC_for_shelter), "\n")
}


##  run hotline summary calculations
hotline_summary_numbers(for_summary, "All Calling")

hotline_summary_numbers(for_summary %>%
                          filter(Housed_Status == "Literally Homeless")
                        , "Literally Homeless Calling")

hotline_summary_numbers(for_summary %>%
                          filter(Housed_Status != "Literally Homeless")
                        , "Not Homeless/Unknown Calling")
