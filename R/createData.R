shape_data = function(data = ld, level = c("ld", "county", "cd", "tract")){
  data = data %>%
    mutate(total_voters = republican + democrat + independent,
           vote_pr_2024 = voterPrediction, na.rm = TRUE,
           republican_registration = republican/total_voters,
           democratic_registration = democrat/total_voters,
           independent_registration = independent/total_voters,
           early_voter_2022 = early_voter/total_voters,
           polling_voter_2022 = polling_voters/total_voters
    )
  if(level == "ld"){
    data = data %>%
      rename_all(~stringr::str_replace_all(., "ld_|acs_2022_ld", ""))
  }
  if(level == "cd"){
    data = data %>% rename_all(~stringr::str_replace_all(., "cd_|acs_2022_cd", ""))
  }
  if(level == "county"){
    data = data %>%
      #drop shape_id column
      select(-shape_id) %>%
      rename_all(~stringr::str_replace_all(., "county_|acs_2022_county", ""))
  }
  if(level == "tract"){
    data = data %>% rename_all(~stringr::str_replace_all(., "tract_|acs_2022_tract", ""))
  }
  data = data %>%
    mutate(
      total_population  =  Total_Population,
      white_proportion  =  White_Alone/ Population_One_Race,
      black_proportion  =  Black_or_African_American_Alone/Population_One_Race,
      native_proportion =  American_Indian_and_Alaska_Native_Alone/ Population_One_Race,
      asian_proportion  =  Asian_Alone/ Population_One_Race,
      other_race_proportion = (Native_Hawaiian_and_Other_Pacific_Islander_Alone + Some_Other_Race_Alone)/ Population_One_Race,
      latino            =  Hispanic_or_Latino/ total_Hispanic_or_Latino,
      two_or_more_race  =  Two_or_More_Races/ Total_Population,
      median_age        = `_Median_Age`,
      poverty_rate      = `_Poverty_Count`/ Total_Population,
      female            = `_Female_Population`/ Total_Population,
    )
  if(level == "ld"){
    data = data %>% mutate(median_household_income = `_Median_HousehoIncome`)
  }
  else{
    data = data %>% mutate(median_household_income = `_Median_Household_Income`)
  }
  return(data)
}


  ld <- read.csv("~/Dropbox/github_repos/avp-map/avp_map_viz/data/voter_ld_public.csv") %>%
  shape_data(level = "ld") %>%
  st_as_sf(wkt = "shape_geom") %>%
  st_simplify(dTolerance = 0.02)


  save(ld, file = "~/Dropbox/github_repos/avp-map/avp_map_viz/simpleMap/ld_public.rda")


  shape_properties_extracted <- ld$shape_properties
  parsed_data <- list()




  cd <- read.csv("~/Dropbox/github_repos/avp-map/avp_map_viz/data/voter_cd_public.csv") %>%
    shape_data(level = "cd") %>%
    st_as_sf(wkt = "shape_geom") %>%
    st_simplify(dTolerance = 0.02)


  save(cd, file = "~/Dropbox/github_repos/avp-map/avp_map_viz/cdParticipation/cd_public.rda")

  county <- read.csv("~/Dropbox/github_repos/avp-map/avp_map_viz/data/voter_county_public.csv") %>%
    shape_data(level = "county") %>%
    st_as_sf(wkt = "shape_geom") %>%
    st_simplify(dTolerance = 0.02)


  save(county, file = "~/Dropbox/github_repos/avp-map/avp_map_viz/countyParticipation/county_public.rda")

####tracts


  shape_properties_extracted <- cd$shape_properties
  parsed_data <- list()







  # Loop through each shape property
  for (shape_property in shape_properties_extracted) {
    parsed_list <- fromJSON(shape_property)
    parsed_data <- append(parsed_data, list(parsed_list))
  }

  # Combine the parsed data into a data frame
  df <- bind_rows(parsed_data)
  # Join df and ld
  voting <- left_join(ld, df, by = c("LD" = "DISTRICT"))
  save(voting, file = "~/Dropbox/github_repos/avp-map/avp_map_viz/votingPatterns/ld_public.rda")




  cd <- read.csv("~/Dropbox/github_repos/avp-map/avp_map_viz/data/voter_cd_public.csv") %>%
    shape_data(level = "cd") %>%
    st_as_sf(wkt = "shape_geom") %>%
    st_simplify(dTolerance = 0.02)






  shape_properties_extracted <- cd$shape_properties
  parsed_data <- list()

  # Loop through each shape property
  for (shape_property in shape_properties_extracted) {
    parsed_list <- fromJSON(shape_property)
    parsed_data <- append(parsed_data, list(parsed_list))
  }

  # Combine the parsed data into a data frame
  df <- bind_rows(parsed_data)
  # Join df and ld
  voting <- left_join(cd, df, by = c("CD" = "DISTRICT"))
  save(voting, file = "~/Dropbox/github_repos/avp-map/avp_map_viz/votingPatterns/cd_public.rda")


