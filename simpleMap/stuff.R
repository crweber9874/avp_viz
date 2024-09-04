library(dplyr)
library(stringr)
library(jsonlite)
library(tidyr)
load("ld_public.rda")

# Make a pie chart in ggplot
library(ggplot2)

library(plotly)

azblue = "#0C234B"
azred  = "#AB0520"
oasis = "#378DBD"
load("ld_public.rda")

new_colors = c(azred, azblue, oasis)


ld %>%
  subset(select = c(republican_registration, democratic_registration, independent_registration)) %>%
  rename(Republican = republican_registration,
         Democrat = democratic_registration,
         Independent = independent_registration) %>%
  pivot_longer(cols = c(Republican, Democrat, Independent),
               names_to = "Party", values_to = "Registration") %>%
  # drop geom
  as.data.frame() %>%
  plot_ly(labels = ~Party, values = ~Registration,
          type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~paste0(round(Registration, 2)*100, ' % of registered voters'),
          marker = list(
            colors = new_colors,
            line = list(color = '#FFFFFF', width = 1)
          ),
          hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.5)'),
          showlegend = FALSE) %>%
  layout(title = 'Party Registration in Arizaon',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         plot_bgcolor = '#f0f0f0',  # Light gray background for the plot area
         paper_bgcolor = '#f0f0f0'   # Light gray background for the entire image/paper
         )

ld %>%
  subset(select = c(republican_registration,
                    democratic_registration,
                    independent_registration)) %>%
  rename(Republican = republican_registration,
         Democrat = democratic_registration,
         Independent = independent_registration) %>%
  pivot_longer(cols = c(Republican, Democrat, Independent),
               names_to = "Party", values_to = "Registration") %>%
  # drop geom
  as.data.frame() %>%
  plot_ly(labels = ~Party, values = ~Registration,
  type = 'pie',
  textposition = 'inside',
  textinfo = 'label+percent',
  insidetextfont = list(color = '#FFFFFF'),
  hoverinfo = 'text',
  text = ~paste0(round(Registration, 2)*100, '% of registered voters'),
  marker = list(colors = new_colors,
                line = list(color = '#FFFFFF', width = 1)),
  hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.1)'),  # 50% transparent white background
  showlegend = FALSE) %>%
  layout(title = 'Party Registration in LD',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig




shape_properties_extracted <- ld$shape_properties
parsed_data <- list()

# Loop through each shape property
for (shape_property in shape_properties_extracted) {
  parsed_list <- fromJSON(shape_property)
  parsed_data <- append(parsed_data, list(parsed_list))
}

# Combine the parsed data into a data frame
df <- bind_rows(parsed_data)





#

shape_properties_extracted = str_extract(dat[[1]], "(?<=\\{).*(?=\\})")
shape_properties_extracted = str_replace_all(shape_properties_extracted, "'", "")
shape_properties_extracted = str_trim(shape_properties_extracted) # Trim any leading/trailing whitespace, including newlines

parse_string_to_list <- function(input_string) {
  # Split the string by commas
  key_value_pairs <- unlist(strsplit(input_string, ", "))

  # Initialize an empty list
  result_list <- list()

  # Loop through each key-value pair
  for (pair in key_value_pairs) {
    # Split the pair by the colon
    key_value <- unlist(strsplit(pair, ": "))

    # Trim whitespace and assign to the list
    key <- trimws(key_value[1])
    value <- trimws(key_value[2])

    # Convert value to numeric if possible
    if (!is.na(as.numeric(value))) {
      value <- as.numeric(value)
    }

    # Add to the result list
    result_list[[key]] <- value
  }

  return(result_list)
}

# Example usage
input_string <- "AtG2018_DE: 65.85, COLOR: -16726656, CompDemVot: 67.41, CompRepVot: 32.59, DISTRICT: 6, FID: 6, G_AINH18: 96186, G_AINH18_P: 57.73, Gov2018_DE: 60.61, LONGNAME: District 6, Mine2018_D: 67.43, OMB_ASNPI_: 1.08, OMB_BLK_P: 0.8, OMB_LATINO: 9.55, OMB_NATIVE: 61.39, OMB_NH_WHT: 26.08, POPULATION: 225474, Pres2016_D: 66.93, Pres2020_D: 67.31, SHORTNAME: D6, ST1519_M21: 7.83, ST1519_M22: 28.13, ST1519_M23: 0.79, ST1519_M24: 62.55, ST1519_M25: 0.61, ST1519_M2_: 163538, SecState20: 68.2, Sen2018_DE: 68.29, Sen2020_DE: 68.18, SuptEd2018: 69.24, TARGET_DEV: -12909, TARGET_D_1: -5.42, TOTAL: 225474, TOTAL18: 166613, Treas2018_: 65.17"
parsed_list <- parse_string_to_list(input_string)
print(parsed_list)



# Preprocessing to make it valid JSON
# 1. Handle the first value which doesn't have a key
# Preprocessing to make it valid JSON
# 1. Handle the first value which doesn't have a key
json_string <- paste0("\"UNKNOWN_KEY\": ", shape_properties_extracted)

# 2. Replace colons with equals signs
json_string <- gsub(": ", " = ", json_string)

# 3. Enclose keys and string values in double quotes
json_string <- gsub("([a-zA-Z0-9_]+) =", "\"\\1\" =", json_string)
json_string <- gsub(" = ([a-zA-Z]+)", " = \"\\1\"", json_string) # Add quotes around string values

# 4. Add curly braces to create a valid JSON object
json_string <- paste0("{", json_string, "}")

# Parse the JSON string into a list
result_list <- fromJSON(json_string)

# Print the resulting named list
print(result_list)



shape_properties_extracted = gsub(": ", " = ", shape_properties_extracted)
shape_properties_extracted = str_split(shape_properties_extracted, ", ")[[1]]


json_string <- gsub("", "\"", shape_properties_extracted)
result_list <- fromJSON(json_string)

process_string <- function(input_string) {
  # Split the string into name and value parts
  parts <- strsplit(input_string, "\=")[[1]]

  # Assign the value to the name as a named list element
  setNames(list(as.numeric(parts[2])), parts[1]) # Convert value to numeric if possible
}

# Apply the function to each element in the list and combine into a single named list
named_list <- unlist(lapply(result_list, process_string))

# Print the resulting named list
print(named_list)





shape_properties_extracted = ld$shape_properties[[1]]

parsed_list <- fromJSON(shape_properties_extracted)


shape_properties_extracted = str_replace_all(shape_properties_extracted, "'", "\"")
shape_properties_extracted = str_replace(shape_properties_extracted, ",\\s*\\}$", "}")
shape_properties_extracted = str_trim(shape_properties_extracted) # Trim any leading/trailing whitespace, including newlines
shape_properties_extracted = fromJSON(shape_properties_extracted, flatten = TRUE) # Convert JSON to a list
  ) %>%
  pivot_wider(names_from = names(shape_properties_extracted), values_from = shape_properties_extracted)
  # Turn these to objects into variables. in the data farme

library(jsonlite)

string_variable <- "{'AtG2018_DE': 65.85, 'COLOR': -16726656, 'CompDemVot': 67.41, 'CompRepVot': 32.59, 'DISTRICT': 6, 'FID': 6, 'G_AINH18': 96186, 'G_AINH18_P': 57.73, 'Gov2018_DE': 60.61, 'LONGNAME': 'District 6', 'Mine2018_D': 67.43, 'OMB_ASNPI_': 1.08, 'OMB_BLK_P': 0.8, 'OMB_LATINO': 9.55, 'OMB_NATIVE': 61.39, 'OMB_NH_WHT': 26.08, 'POPULATION': 225474, 'Pres2016_D': 66.93, 'Pres2020_D': 67.31, 'SHORTNAME': 'D6', 'ST1519_M21': 7.83, 'ST1519_M22': 28.13, 'ST1519_M23': 0.79, 'ST1519_M24': 62.55, 'ST1519_M25': 0.61, 'ST1519_M2_': 163538, 'SecState20': 68.2, 'Sen2018_DE': 68.29, 'Sen2020_DE': 68.18, 'SuptEd2018': 69.24, 'TARGET_DEV': -12909, 'TARGET_D_1': -5.42, 'TOTAL': 225474, 'TOTAL18': 166613, 'Treas2018_': 65.17}"

# Convert single quotes to double quotes for valid JSON
json_string <- gsub("'", "\"", string_variable)

# Parse the JSON string into a list
result_list <- fromJSON(json_string)

# Print the result
print(result_list)




  rowwise() %>% # Operate row-by-row
  mutate(
    AtG2018_DE = shape_properties_extracted$AtG2018_DE,
    COLOR = shape_properties_extracted$COLOR,
    CompDemVot = shape_properties_extracted$CompDemVot,
    # ... and so on for all the other variables
    Treas2018_ = shape_properties_extracted$Treas2018_
  ) %>%
  ungroup()
