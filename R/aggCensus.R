#' grab_census_data: Construct a census dataset for each level of aggregation.
#'
#'
#' @param levels {"congressional_district", "legislative_district, "census_tract, "county}
#' @return A dataframe with aggregated voter and census data:
#'
#' @importFrom dplyr
#' @importFrom stringr str_replace_all
#' @export

library(dplyr)
grab_census_data = function(group = "congressional_district", data = voter_data){ ``
  if(group == "congressional_district"){
    data = data %>%
      dplyr::select(CD, dplyr::starts_with("cd_"), dplyr::starts_with( "acs_2022_cd") )  %>%
      # strip all cd_ and acs_2022_cd_ from column names
      dplyr::rename_all(~stringr::str_replace_all(., "cd_|acs_2022_cd", "")) %>%
      # Drop CD is NA
      dplyr::filter(!is.na(CD)) %>%
      dplyr::group_by(CD) %>%
      dplyr::summarize(
        total_population = min(Total_Population),
        white_proportion = min(White_Alone)/ min(Population_One_Race),
        black_proportion = min(Black_or_African_American_Alone)/ min(Population_One_Race),
        native_proportion = min(American_Indian_and_Alaska_Native_Alone)/ min(Population_One_Race),
        asian_proportion = min(Asian_Alone)/ min(Population_One_Race),
        other_race_proportion = (min(Native_Hawaiian_and_Other_Pacific_Islander_Alone) + min(Some_Other_Race_Alone))/ min(Population_One_Race),
        latino = min(Hispanic_or_Latino)/ min(total_Hispanic_or_Latino),
        two_or_more_race = min(Two_or_More_Races)/ min(Total_Population),
        median_age = mean(`_Median_Age`),
        poverty_rate = mean(`_Poverty_Count`),
        female = min(`_Female_Population`)/ min(Total_Population),
        median_household_income = mean(`_Median_Household_Income`)
      )
  }
  if(group == "legislative_district"){
    data = data %>%
      dplyr::select(LD, dplyr::starts_with("ld_"), dplyr::starts_with( "acs_2022_ld") )  %>%
      # strip all cd_ and acs_2022_cd_ from column names
      dplyr::rename_all(~stringr::str_replace_all(., "ld_|acs_2022_ld", "")) %>%
      # Drop CD is NA
      dplyr::filter(!is.na(LD)) %>%
      dplyr::group_by(LD) %>%
      dplyr::summarize(
        total_population = min(Total_Population),
        white_proportion = min(White_Alone)/ min(Population_One_Race),
        black_proportion = min(Black_or_African_American_Alone)/ min(Population_One_Race),
        native_proportion = min(American_Indian_and_Alaska_Native_Alone)/ min(Population_One_Race),
        asian_proportion = min(Asian_Alone)/ min(Population_One_Race),
        other_race_proportion = (min(Native_Hawaiian_and_Other_Pacific_Islander_Alone) + min(Some_Other_Race_Alone))/ min(Population_One_Race),
        latino = min(Hispanic_or_Latino)/ min(total_Hispanic_or_Latino),
        two_or_more_race = min(Two_or_More_Races)/ min(Total_Population),
        median_age = mean(`_Median_Age`),
        poverty_rate = mean(`_Poverty_Count`),
        female = min(`_Female_Population`)/ min(Total_Population),
        median_household_income = mean(`_Median_Household_Income`)
      )
  }
  if(group == "census_tract"){
    data = data %>%
      dplyr::select(tract, dplyr::starts_with("tract_"), dplyr::starts_with( "acs_2022_tract") )  %>%
      # strip all cd_ and acs_2022_cd_ from column names
      dplyr::rename_all(~stringr::str_replace_all(., "tract_|acs_2022_tract", "")) %>%
      # Drop tract_fips_id is NA
      dplyr::filter(!is.na(tract)) %>%
      dplyr::group_by(tract) %>%
      dplyr::summarize(
        total_population = min(Total_Population),
        white_proportion = min(White_Alone)/ min(Population_One_Race),
        black_proportion = min(Black_or_African_American_Alone)/ min(Population_One_Race),
        native_proportion = min(American_Indian_and_Alaska_Native_Alone)/ min(Population_One_Race),
        asian_proportion = min(Asian_Alone)/ min(Population_One_Race),
        other_race_proportion = (min(Native_Hawaiian_and_Other_Pacific_Islander_Alone) + min(Some_Other_Race_Alone))/ min(Population_One_Race),
        latino = min(Hispanic_or_Latino)/ min(total_Hispanic_or_Latino),
        two_or_more_race = min(Two_or_More_Races)/ min(Total_Population),
        median_age = mean(`_Median_Age`),
        poverty_rate = mean(`_Poverty_Count`),
        female = min(`_Female_Population`)/ min(Total_Population),
        median_household_income = mean(`_Median_Household_Income`)
      )
  }
  if(group == "county"){
    data = data %>%
      dplyr::select(county, dplyr::starts_with("county_"), dplyr::starts_with( "acs_2022_county") )  %>%
      dplyr::rename_all(~stringr::str_replace_all(., "county_|acs_2022_county", "")) %>%
      # Drop CD is NA
      dplyr::filter(!is.na(county)) %>%
      dplyr::group_by(county) %>%
      dplyr::summarize(
        total_population = mean(Total_Population, na.rm = T),
        white_proportion = min(White_Alone)/ min(Population_One_Race),
        black_proportion = min(Black_or_African_American_Alone)/ min(Population_One_Race),
        native_proportion = min(American_Indian_and_Alaska_Native_Alone)/ min(Population_One_Race),
        asian_proportion = min(Asian_Alone)/ min(Population_One_Race),
        other_race_proportion = (min(Native_Hawaiian_and_Other_Pacific_Islander_Alone) + min(Some_Other_Race_Alone))/ min(Population_One_Race),
        latino = min(Hispanic_or_Latino)/ min(total_Hispanic_or_Latino),
        two_or_more_race = min(Two_or_More_Races)/ min(Total_Population),
        median_age = mean(`_Median_Age`),
        poverty_rate = mean(`_Poverty_Count`),
        female = min(`_Female_Population`)/ min(Total_Population),
        median_household_income = mean(`_Median_Household_Income`)
      )
  }
  return(data)
}

