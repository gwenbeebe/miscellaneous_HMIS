library(tidyverse)
library(lubridate)
library(readxl)
library(pander)
library(dplyr)

##  update these to the current report year dates
year_start <- ymd("20220101")
year_end <- ymd("20221231")

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

##  Runs on BNL v1.1 - Demos and Households
##  Before loading this in, merge and de-dupe 
##  the two tables on the households tab

##  find and load in the BNL
bnl_data <- file.choose()

by_name_list <- read_excel(bnl_data, sheet = 1) %>%
  rename(NewlyHomeless = `...9`,
         RelationshiptoHoH = `CLIENTDETAILRelationshiptoHoH`) %>%
  `colnames<-`(gsub(" ", "", names(.))) %>%
  select("ClientId", "DateofBirth", "Gender", "CensusRace", "Ethnicity", "ClientStatus", "NewlyHomeless",
         "StatusDate", "Chronic?", "Veteran?", "Youth?", "DV?", "PriorZipCode", "StatedReasonforHomelessness",
         "FirstDateinActivityRange", "LastDateinActivityRange", "RelationshiptoHoH") %>%
  ## keep people who became homeless in the year, are still active, or had their status changed to housed this year
  filter(NewlyHomeless == "Newly Homeless" |
           ClientStatus %in% c("Return from Housed", "Active", "Newly Homeless", "Return from Inactive") |
           (ClientStatus == "Housed" & 
              StatusDate >=  year_start & 
              StatusDate <= year_end)) %>%
  mutate(age_at_year_end = age(DateofBirth, year_end),
         CensusRace = if_else(CensusRace == "Other Multi-Racial", "Two or More Races", CensusRace),
         Gender = case_when(
           str_detect(Gender, "Transgender") & str_detect(Gender, "Female") ~ "Trans Woman",
           str_detect(Gender, "Transgender") & str_detect(Gender, "Male") ~ "Trans Man",
           str_detect(Gender, "non-binary") |
             (str_detect(Gender, "Female") &
                str_detect(Gender, "Male")) ~ "Non Binary",
           TRUE ~ Gender
         ))
  
by_name_list_hhs <- 
  read_excel(bnl_data, sheet = 2) %>%
  rename(ClientId = ClientUid,
         RelationshiptoHeadofHousehold = HouseholdRelationship)


##  get household relationship information from BNL with households joined and de-duped
# hh_relationships <- 
#   read_excel(file.choose(), sheet = 1) %>%
#   rename(ClientId = ClientUid, HouseholdId = EntryExitHouseholdId)

hh_relationships_cleaned <- 
  # hh_relationships %>%
  by_name_list_hhs %>%
  mutate(RelationshiptoHeadofHousehold = case_when(
    RelationshiptoHeadofHousehold == "Self (head of household)" ~ "Self", 
    RelationshiptoHeadofHousehold %in% c("Head of household's child", "non-binary child",
                                         "daughter", "son", "adult daughter",
                                         "adult son", "step-son", "step-daughter",
                                         "son-in-law", "daughter-in-law",
                                         "child of partner") ~ "Child",
    RelationshiptoHeadofHousehold %in% c("Head of household's spouse or partner",
                                         "partner", "husband", "wife") ~ "Partner",
    RelationshiptoHeadofHousehold %in% c("step-brother", "brother",
                                         "sister", "step-sister") ~ "Sibling",
    RelationshiptoHeadofHousehold %in% c("father", "mother", "father-in-law",
                                         "mother-in-law", "step-father") ~ "Parent",
    RelationshiptoHeadofHousehold %in% c("granddaughter", "grandson") ~ "Grandchild",
    RelationshiptoHeadofHousehold %in% c("grandfather", "grandmother") ~ "Grandparent",
    RelationshiptoHeadofHousehold %in% c("uncle", "niece", "nephew", "other relative", 
                                         "cousin", "aunt",
                                         "Head of household's other relation member (other relation to head of household)") ~ "Other Relative",
    RelationshiptoHeadofHousehold %in% c("other non-relative", "unknown", "friend of family", 
                                         "Data not collected", "Other: non-relation member") |
      is.na(RelationshiptoHeadofHousehold) ~ "Unknown",
    TRUE ~ RelationshiptoHeadofHousehold
  ))

##  Use this to check for unaccounted-for relationship values
unique(hh_relationships_cleaned$RelationshiptoHeadofHousehold)

##  assign each active person to only one household
##  if they are in multiple households, assign them to the largest household
##  identify youth and family households
# hold <- by_name_list_hhs
by_name_list_hhs <- 
  # hold %>%
  # inner_join(by_name_list %>%
  #              select(ClientId, age_at_year_end, Gender),
             # by = "ClientId") %>%
  by_name_list %>%
  select(ClientId, age_at_year_end, Gender) %>%
  filter(ClientId %in% by_name_list$ClientId) %>%
  # left_join(hh_relationships_cleaned, by = c("ClientId", "HouseholdId")) %>%
  left_join(hh_relationships_cleaned, by = "ClientId",
            multiple = "all") %>%
  group_by(HouseholdId) %>%
  mutate(household_size = n(),
         max_household_age = max(age_at_year_end, na.rm = TRUE),
         min_household_age = min(age_at_year_end, na.rm = TRUE),
         self_partner_only = min(if_else(RelationshiptoHeadofHousehold %in% c("Self", "Partner"), 1, 0)),
         has_child = max(if_else(RelationshiptoHeadofHousehold == "Child", 1, 0)),
         has_parent = max(if_else(RelationshiptoHeadofHousehold == "Parent", 1, 0)),
         all_hoh = min(if_else(RelationshiptoHeadofHousehold == "Self", 1, 0)),
         spans_three_gens = max(if_else(RelationshiptoHeadofHousehold %in% c("Grandchild", "Grandparent"), 1, 0)),
         has_other_family = max(if_else(RelationshiptoHeadofHousehold %in% c("Sibling", "Other Relative"), 1, 0))) %>%
  ungroup() %>%
  arrange(desc(household_size)) %>%
  group_by(ClientId) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-age_at_year_end)

household_types <- by_name_list_hhs %>%
  mutate(HouseholdType = case_when(min_household_age < 18 ~ "Family With Children",
                                   household_size == 1 & 
                                     Gender %in% c("Trans Woman",
                                                   "Female") ~ "Single Female",
                                   household_size == 1 & 
                                     Gender %in% c("Trans Man",
                                                   "Male") ~ "Single Male",
                                   household_size == 1 & 
                                     Gender %in% c("Client refused", "Data not collected") |
                                     is.na(Gender) ~ "Single Person - Gender Unknown",
                                   household_size == 1 & 
                                     Gender == "Non Binary" ~ "Single Non-Binary Person",
                                   self_partner_only == 1 ~ "Couple",
                                   min_household_age >= 18 & (has_child == 1 |
                                                                has_parent == 1 |
                                                                spans_three_gens == 1) ~ "Household with Adult Child/ren",
                                   is.na(min_household_age) & (has_child == 1 |
                                                                has_parent == 1 |
                                                                 spans_three_gens == 1) ~ "Household with Child/ren, Age Unknown",
                                   has_other_family == 1 ~ "Other Familial Relationship",
                                   min_household_age >= 18 ~ "Adults Only, Relationship Unknown",
                                   TRUE ~ "Cannot Determine"))

check <- household_types %>%
  filter(HouseholdType == "Cannot Determine") %>%
  arrange(HouseholdId)

household_types %>% 
  distinct(HouseholdId, HouseholdType) %>%
  group_by(HouseholdType) %>%
  summarise(households = n())

unique(check$Gender)


##  get system map counts
total_households <- nrow(distinct(by_name_list_hhs, HouseholdId))

families_with_children <- nrow(distinct(by_name_list_hhs %>%
                                          filter(min_household_age < 18)
                                          , HouseholdId))

veterans <- nrow(distinct(by_name_list %>%
                                          filter(`Veteran?` == "Yes")
                                        , ClientId))

youth_households <- nrow(distinct(by_name_list_hhs %>%
                                          filter(max_household_age >= 18 &
                                                   max_household_age <= 24)
                                        , HouseholdId))

seniors <- nrow(distinct(by_name_list %>%
                                          filter(age_at_year_end >= 55)
                                        , ClientId))

BIPOC_households <- nrow(distinct(by_name_list %>%
                                    filter((CensusRace %in% c("Native Hawaiian or Other Pacific Islander (HUD)","Two or More Races",
                                                             "Other", "Black or African American (HUD)", "American Indian or Alaska Native (HUD)",
                                                             "Asian (HUD)", "Native Hawaiian or Pacific Islander (HUD)", 
                                                             "Black, African American, or African (HUD)", "Asian or Asian American (HUD)",
                                                             "American Indian, Alaska Native, or Indigenous (HUD)") |
                                             Ethnicity %in% c("Hispanic/Latino (HUD)",
                                                              "Hispanic/Latin(a)(o)(x) (HUD)" )) &
                                    age_at_year_end >= 18) %>%
                                    left_join(by_name_list_hhs, by = "ClientId")
                          , HouseholdId))

BIPOC_household_detail <- by_name_list %>%
  filter((CensusRace %in% c("Native Hawaiian or Other Pacific Islander (HUD)","Two or More Races",
                            "Other", "Black or African American (HUD)", "American Indian or Alaska Native (HUD)",
                            "Asian (HUD)", "Native Hawaiian or Pacific Islander (HUD)", 
                            "Black, African American, or African (HUD)", "Asian or Asian American (HUD)",
                            "American Indian, Alaska Native, or Indigenous (HUD)") |
            Ethnicity %in% c("Hispanic/Latino (HUD)",
                             "Hispanic/Latin(a)(o)(x) (HUD)" )) &
           age_at_year_end >= 18) %>%
  left_join(by_name_list_hhs, by = "ClientId") %>%
  # group_by(CensusRace) %>%
  group_by(Ethnicity) %>%
  summarise(households = n())

cat("\nTotal households: ", paste0(total_households),
    "\nFamilies: ", paste0(families_with_children),
    "\nVeterans: ", paste0(veterans),
    "\nYouth households: ", paste0(youth_households),
    "\nSeniors 55+: ", paste0(seniors),
    "\nBIPOC Households: ", paste0(BIPOC_households), "\n")

##  get client counts
all_people <- nrow(distinct(by_name_list, ClientId))

children <- nrow(distinct(by_name_list %>%
                           filter(age_at_year_end < 18)
                         , ClientId))

BIPOC <- nrow(distinct(by_name_list %>%
                         filter(CensusRace %in% c("Native Hawaiian or Other Pacific Islander (HUD)","Two or More Races",
                                                   "Other", "Black or African American (HUD)", "American Indian or Alaska Native (HUD)",
                                                   "Asian (HUD)", "Native Hawaiian or Pacific Islander (HUD)", 
                                                   "Black, African American, or African (HUD)", "Asian or Asian American (HUD)",
                                                   "American Indian, Alaska Native, or Indigenous (HUD)") |
                                   Ethnicity %in% c("Hispanic/Latino (HUD)",
                                                    "Hispanic/Latin(a)(o)(x) (HUD)"))
                          , ClientId))

non_male <- nrow(distinct(by_name_list %>%
                         filter(Gender %in% c("Female", "Non Binary",
                                              "Trans Woman"))
                       , ClientId))

newly_homeless <- nrow(distinct(by_name_list %>%
                            filter(!is.na(NewlyHomeless))
                          , ClientId))

latinx <- nrow(distinct(by_name_list %>%
                            filter(Ethnicity %in% c("Hispanic/Latino (HUD)",
                                                    "Hispanic/Latin(a)(o)(x) (HUD)"))
                          , ClientId))

cat("\nAll people: ", paste0(all_people),
    "\nChildren: ", paste0(children),
    "\nBIPOC: ", paste0(BIPOC),
    "\nWomen + NB: ", paste0(non_male),
    "\nNewly Homeless: ", paste0(newly_homeless), "\n",
    "\nLatinx (for bucket): ", paste0(latinx), "\n")

##  bucket clients by age
age_table <- by_name_list %>%
  select(ClientId, age_at_year_end) %>%
  mutate(age_bucket = case_when(age_at_year_end <= 5 ~ "0 - 5", 
                                age_at_year_end <= 11 ~ "6 – 11",
                                age_at_year_end <= 17 ~ "12 – 17",
                                age_at_year_end <= 24 ~ "18 – 24",
                                age_at_year_end <= 34 ~ "25 – 34",
                                age_at_year_end <= 44 ~ "35 – 44",
                                age_at_year_end <= 54 ~ "45 – 54",
                                age_at_year_end <= 69 ~ "55 – 69",
                                age_at_year_end >= 70 ~ "70+", 
                                TRUE ~ "Unknown"),
         age_bucket = factor(age_bucket,
                             levels = 
                               c("0 - 5", "6 – 11", "12 – 17", "18 – 24", "25 – 34",
                                 "35 – 44", "45 – 54", "55 – 69", "70+", "Unknown"))) %>%
  group_by(age_bucket) %>%
  summarise(people = n())


##  bucket clients by race
race_table <- by_name_list %>%
  mutate(CensusRace = if_else(CensusRace == "White (HUD)" &
                                Ethnicity != "Hispanic/Latin(a)(o)(x) (HUD)",
                              "White, Non-Hispanic", CensusRace),
         CensusRace = factor(CensusRace,
                             levels = c("White, Non-Hispanic", "Native Hawaiian or Pacific Islander (HUD)",
                                        "Black, African American, or African (HUD)",
                                        "Two or More Races",
                                        "American Indian, Alaska Native, or Indigenous (HUD)",
                                        "Asian or Asian American (HUD)" ))) %>%
  select(ClientId, CensusRace) %>%
  group_by(CensusRace) %>%
  summarise(people = n())


##  bucket clients by gender
gender_table <- by_name_list %>%
  # mutate(Gender = factor(Gender,
  #                            levels = unique(by_name_list$Gender))) %>%
  group_by(Gender) %>%
  summarise(people = n())
                         

##  addition 2023 questions

people_in_families <- nrow(distinct(by_name_list_hhs %>%
                                          filter(min_household_age < 18)
                                        , ClientId))

newly_homeless_in_families <- nrow(distinct(by_name_list %>%
                                              left_join(by_name_list_hhs, by = "ClientId") %>%
                                              filter(min_household_age < 18 &
                                                       !is.na(NewlyHomeless))
                                            , ClientId))

families_with_newly_homeless <- nrow(distinct(by_name_list %>%
                                                left_join(by_name_list_hhs, by = "ClientId") %>%
                                    filter(min_household_age < 18 &
                                             !is.na(NewlyHomeless))
                                  , HouseholdId))

families_with_newly_homeless_adult <- nrow(distinct(by_name_list %>%
                                                left_join(by_name_list_hhs, by = "ClientId") %>%
                                                filter(min_household_age < 18 &
                                                         age_at_year_end >= 18 &
                                                         !is.na(NewlyHomeless))
                                              , HouseholdId))

cat("\npeople_in_families: ", paste0(people_in_families),
    "\nnewly_homeless_in_families: ", paste0(newly_homeless_in_families),
    "\nfamilies_with_newly_homeless: ", paste0(families_with_newly_homeless),
    "\nfamilies_with_newly_homeless_adult: ", paste0(families_with_newly_homeless_adult))
