#' build_data: Construct a dataset for each level of aggregation.
#'
#'
#' @param levels {"congressional_district", "legislative_district, "census_tract, "county}
#' @return A dataframe with aggregated voter and census data:
#'

#' @export
#'

build_data = function(level = c("congressional_district", "legislative_district", "census_tract", "county")){
  if(level == "congressional_district") {dat = aggData(level = "CD") %>% left_join(grab_census_data(group = "congressional_district", data = voter_data), by = c("CD" = "CD")) }
  if(level == "legislative_district") {dat = aggData(level = "LD") %>% left_join(grab_census_data(group = "legislative_district", data = voter_data), by = c("LD" = "LD")) }
  if(level == "census_tract") {dat = aggData(level = "tract_fips_id") %>% left_join(grab_census_data(group = "census_tract", data = voter_data), by = c("tract_fips_id" = "tract")) }
  if(level == "county") {data = aggData(level = "county_fips_id") %>% left_join(grab_census_data(group = "county", data = voter_data), by = c("county_fips_id" = "county"))}
  return(dat)
}


