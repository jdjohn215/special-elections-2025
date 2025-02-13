rm(list = ls())

library(tidyverse)
library(sf)

# This script allocates 2024 presidential precinct votes into state legislative
#   districts. It does this based on centroid overlap, assuming that each
#   precinct lies entirely within a single legislative district. I use
#   precinct data and boundaries collected by NYT.
# https://github.com/nytimes/presidential-precinct-map-2024?tab=readme-ov-file#general-caveats

################################################################################
# this function aggregates votes into districts
aggregate_precincts_to_district <- function(district_sf, precinct_sf){
  precinct_sf |>
    select(starts_with("votes"), geometry) |>
    mutate(geometry = st_point_on_surface(geometry)) |>
    st_join(district_sf) |>
    st_drop_geometry() |>
    group_by(state, house, district) |>
    summarise(harris = sum(votes_dem),
              trump = sum(votes_rep),
              total_2024 = sum(votes_total), .groups = "drop")
}

################################################################################
# Virginia
virginia.upper.shp <- tigris::state_legislative_districts("VA", "upper", year = 2023) |>
  select(district = SLDUST) |>
  mutate(state = "virginia",
         house = "upper") |>
  st_make_valid()
virginia.lower.shp <- tigris::state_legislative_districts("VA", "lower", year = 2023) |>
  select(district = SLDLST) |>
  mutate(state = "virginia",
         house = "lower") |>
  st_make_valid()

virginia.votes <- st_read("2024-precinct-data/VA-precincts-with-results.geojson") |>
  st_transform(crs = st_crs(virginia.lower.shp)) |>
  st_make_valid()

virginia.upper.votes <- aggregate_precincts_to_district(virginia.upper.shp, virginia.votes)
virginia.lower.votes <- aggregate_precincts_to_district(virginia.lower.shp, virginia.votes)

################################################################################
# Iowa
iowa.upper.shp <- tigris::state_legislative_districts("IA", "upper", year = 2023) |>
  select(district = SLDUST) |>
  mutate(state = "iowa",
         house = "upper") |>
  st_make_valid()
iowa.lower.shp <- tigris::state_legislative_districts("IA", "lower", year = 2023) |>
  select(district = SLDLST) |>
  mutate(state = "iowa",
         house = "lower") |>
  st_make_valid()

iowa.votes <- st_read("2024-precinct-data/IA-precincts-with-results.geojson") |>
  st_transform(crs = st_crs(iowa.lower.shp)) |>
  st_make_valid()

iowa.upper.votes <- aggregate_precincts_to_district(iowa.upper.shp, iowa.votes)
iowa.lower.votes <- aggregate_precincts_to_district(iowa.lower.shp, iowa.votes)

################################################################################
# Delaware
delaware.upper.shp <- tigris::state_legislative_districts("DE", "upper", year = 2023) |>
  select(district = SLDUST) |>
  mutate(state = "delaware",
         house = "upper") |>
  st_make_valid()
delaware.lower.shp <- tigris::state_legislative_districts("DE", "lower", year = 2023) |>
  select(district = SLDLST) |>
  mutate(state = "delaware",
         house = "lower") |>
  st_make_valid()

delaware.votes <- st_read("2024-precinct-data/DE-precincts-with-results.geojson") |>
  st_transform(crs = st_crs(delaware.lower.shp)) |>
  st_make_valid()

delaware.upper.votes <- aggregate_precincts_to_district(delaware.upper.shp, delaware.votes)
delaware.lower.votes <- aggregate_precincts_to_district(delaware.lower.shp, delaware.votes)

################################################################################
# combine state chambers
all.districts <- bind_rows(
  virginia.upper.votes, virginia.lower.votes,
  iowa.upper.votes, iowa.lower.votes,
  delaware.upper.votes, delaware.lower.votes
)
write_csv(all.districts, "district-votes-pres2024.csv")
