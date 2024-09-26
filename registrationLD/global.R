

library(dplyr)
library(sf)
library(ggplot2)
library(leaflet.extras)
library(plotly)
library(jsonlite)
library(tidyr)
library(plotly)
library(GGally)
library(RColorBrewer)
library(leaflet.providers)
library(htmlwidgets)

## Slight data transformations
ldDat = ld %>%
  mutate(
    percentRepublican = as.numeric(republican_registration)*100,
    percentDemocrat   = as.numeric(democratic_registration)*100,
    percentIndependent = as.numeric(independent_registration)*100,
    totalVoters       = total_voters,
    earlyVoter        = early_voter/general2022,
    pollingVoter      = polling_voters/general2022,
    provisionalVoters = provisional_voters/general2022) %>%
  dplyr::select(LD, percentRepublican,
           percentDemocrat, percentIndependent, totalVoters,
           earlyVoter, pollingVoter, provisionalVoters, shape_geom)


tractDat = tracts_ld %>%
  mutate(
    percentRepublican = as.numeric(republican_registration)*100,
    percentDemocrat   = as.numeric(democratic_registration)*100,
    percentIndependent = as.numeric(independent_registration)*100,
    totalVoters       = total_voters,
    earlyVoter        = early_voter/general2022,
    pollingVoter      = polling_voters/general2022,
    provisionalVoters = provisional_voters/general2022) %>%
  dplyr::select(shape_fips, LD, percentRepublican,
           percentDemocrat, percentIndependent, totalVoters,
           earlyVoter, pollingVoter, provisionalVoters, shape_geom)


##### Custom Palettes

azReds <- colorRampPalette(c( az_color("azgrey"),  az_color("chili"), az_color("azred")))
azBlues <- colorRampPalette(c( az_color("azgrey"),  az_color("oasis"), az_color("azblue")))
azGreys <- colorRampPalette(c( az_color("white"),  az_color("azgrey"), az_color("midnight")))
