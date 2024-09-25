
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet.extras)
library(plotly)
library(jsonlite)
library(tidyr)
library(RColorBrewer)
library(MASS)

state_boundary <- function(shapefile, markers) {
  removeNotification(id = "region_error", session = getDefaultReactiveDomain())

  dat <- data.frame(Longitude = markers$lon,
                    Latitude = markers$lat,
                    names = c("Point"))

  dat <- sf::st_as_sf(dat,
                      coords = c("Longitude", "Latitude"))

  sf::st_crs(dat) <- sf::st_crs(shapefile)

  return(as.data.frame(shapefile)[which(sapply(sf::st_intersects(shapefile,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}
azblue =  az_color("azblue")
azred  =   az_color("azred")
oasis  =  az_color("oasis")

ld = ld %>%
  mutate(
    averageGeneral = rowMeans(across(c(general2020, general2016), as.numeric), na.rm = TRUE) %>% round(),
    averageMidterm = rowMeans(across(c(general2022, general2018), as.numeric), na.rm = TRUE) %>% round(),
    averagePrimary = rowMeans(across(c(primary2022, primary2020, primary2018, primary2016), as.numeric), na.rm = TRUE) %>% round()
  )


ggtheme <- theme(
  plot.title = element_text(hjust = 0, vjust = 0, colour = azblue, size = 13),
  axis.text.x = element_text(size = 13, colour = azblue),
  axis.text.y = element_text(size = 13, colour = azblue),
  axis.title = element_text(size = 13, colour = azblue ),
  axis.title.y = element_text(size = 13, colour = azblue,  vjust = 1.5),
  axis.ticks = element_blank(),
  strip.text.x = element_text(size = 13),
  panel.grid.major = element_line(colour = "#D0D0D0", size = .25),
  panel.background = element_rect(fill = "white"),
  legend.text = element_text(size = 13),
  legend.title = element_text(size = 13)
)
shape_properties_extracted <- ld$shape_properties
parsed_data <- list()

for (shape_property in shape_properties_extracted) {
  parsed_list <- fromJSON(shape_property)
  parsed_data <- append(parsed_data, list(parsed_list))
}

vote_data <- bind_rows(parsed_data)
