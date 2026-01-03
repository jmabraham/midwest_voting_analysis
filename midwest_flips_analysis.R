library(tidyverse)
library(janitor)
library(dplyr)

# source: https://github.com/tonmcg/US_County_Level_Election_Results_08-24
election_2016 <- read_csv("~/Documents/nyt_election_data/2016.csv") %>% clean_names()
election_2020 <- read_csv("~/Documents/nyt_election_data/2020.csv") %>% clean_names()
election_2024 <- read_csv("~/Documents/nyt_election_data/2024.csv") %>% clean_names()

# converts 2016 state_abbr to state_name
state_map <- setNames(state.name, state.abb)
election_2016 <- election_2016 %>%
  mutate(state_name = state_map[state_abbr],
         combined_fips = as.numeric(combined_fips))

# converts 2020 and 2024 FIPS to num
election_2020 <- election_2020 %>%
  mutate(county_fips = as.numeric(county_fips))
election_2024 <- election_2024 %>%
  mutate(county_fips = as.numeric(county_fips))

# hardcoding as vector
midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", 
                    "Minnesota", "Missouri", "Nebraska", "North Dakota", 
                    "Ohio", "South Dakota", "Wisconsin")

# filters datasets for Midwest vector items and selects relevant fields
midwest_2016 <- election_2016 %>% filter(state_name %in% midwest_states)
midwest_2020 <- election_2020 %>% filter(state_name %in% midwest_states)
midwest_2024 <- election_2024 %>% filter(state_name %in% midwest_states)
midwest_2016 <- midwest_2016 %>% select(state_name, county_name, votes_dem, votes_gop, per_dem, per_gop)
midwest_2020 <- midwest_2020 %>% select(state_name, county_name, votes_dem, votes_gop, per_dem, per_gop)
midwest_2024 <- midwest_2024 %>% select(state_name, county_name, votes_dem, votes_gop, per_dem, per_gop)

# dataset merge
midwest_merge <- midwest_2016 %>%
  left_join(midwest_2020, by = c("state_name", "county_name"), suffix = c("_2016", "_2020")) %>%
  left_join(midwest_2024, by = c("state_name", "county_name")) %>%
  rename(votes_dem_2024 = votes_dem,
         votes_gop_2024 = votes_gop,
         per_dem_2024 = per_dem,
         per_gop_2024 = per_gop)

# flip and percentage point change calc
midwest_flips <- midwest_merge %>%
  mutate(
    flip_16_20 = case_when(
      per_dem_2016 > per_gop_2016 & per_gop_2020 > per_dem_2020 ~ "Dem -> GOP",
      per_gop_2016 > per_dem_2016 & per_dem_2020 > per_gop_2020 ~ "GOP -> Dem",
      TRUE ~ "No Flip"
    ),
    flip_20_24 = case_when(
      per_dem_2020 > per_gop_2020 & per_gop_2024 > per_dem_2024 ~ "Dem -> GOP",
      per_gop_2020 > per_dem_2020 & per_dem_2024 > per_gop_2024 ~ "GOP -> Dem",
      TRUE ~ "No Flip"
    ),
# need to check if this calc makes sense
    swing_dem_16_24 = per_dem_2024 - per_dem_2016,
    swing_gop_16_24 = per_gop_2024 - per_gop_2016
  )

# temp view of flips in 2020 to 2024
flip_filter <- midwest_flips %>% filter(flip_20_24 != "No Flip") %>% arrange(desc(abs(swing_dem_16_24)))

