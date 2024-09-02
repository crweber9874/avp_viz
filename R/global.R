library(dplyr)
library(sf)
library(ggplot2)

setwd("~/Dropbox/github_repos/avp-map/avp_map_viz/simpleMap")

ld <- read.csv("~/Dropbox/github_repos/avp-map/avp_map_viz/data/voter_ld_public.csv") %>%
  shape_data(level = "ld") %>%
  st_as_sf(wkt = "shape_geom") %>%
  st_simplify(dTolerance = 0.05)


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

ggtheme <- theme(
  plot.title = element_text(hjust = 0, vjust = 0, colour = "#3C3C3C", size = 13),
  axis.text.x = element_text(size = 13, colour = "#535353", ),
  axis.text.y = element_text(size = 13, colour = "#535353", ),
  axis.title = element_text(size = 13, colour = "#535353", ),
  axis.title.y = element_text(size = 13, colour = "#535353",  vjust = 1.5),
  axis.ticks = element_blank(),
  strip.text.x = element_text(size = 13),
  panel.grid.major = element_line(colour = "#D0D0D0", size = .25),
  panel.background = element_rect(fill = "white"),
  legend.text = element_text(size = 13),
  legend.title = element_text(size = 13)
)
