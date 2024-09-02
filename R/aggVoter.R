#' agg_voter: Construct a census dataset for each level of aggregation.
#'
#'
#' @param levels {"congressional_district", "legislative_district, "census_tract, "county}
#' @return A dataframe with aggregated voter and census data:
#'

#' @export
#'


aggData = function(level = "CD"){
  voter_data %>%
    filter(!is.na(!!sym(level))) %>%
    mutate(total_voters = sum(!is.na(pid))) %>%
    group_by(across({{ level }})) %>%
    summarise(
      total_voters = sum(republican + democrat + independent),
      vote_pr_2024 = mean(voterPrediction, na.rm = TRUE),
      republican_registration = sum(republican)/sum(total_voters),
      democratic_registration = sum(democrat)/total_voters,
      independent_registration = sum(independent)/total_voters,
      early_voter_2022 = sum(early_voter)/total_voters,
      polling_voter_2022 = sum(polling_voters)/total_voters
    )
}

table(voter_data$county)

aggData("county_fips_id")
