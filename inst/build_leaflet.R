library(leaflet)
library(sf)
library(dplyr)
library(leaflet.extras)

setwd("~/Dropbox/github_repos/avp-map/avp_map_viz/data")
# Shapes


## Data
ld <- read.csv("voter_ld_public.csv")
ld <- shape_data(data = ld, level = "ld") %>% st_as_sf(wkt = "shape_geom")
cd <- read.csv("voter_cd_public.csv")
cd <- shape_data(data = cd, level = "cd") %>% st_as_sf(wkt = "shape_geom")

county <- read.csv("voter_county_public.csv")
county <- shape_data(data = county, level = "county") %>% st_as_sf(wkt = "shape_geom")

tract <- read.csv("voter_tract_public.csv")
tract <- shape_data(data = tract, level = "tract")  %>% st_as_sf(wkt = "shape_geom")

bins <- seq(0,1, by = 0.12)
# Make a palette


pal <- colorNumeric(c("red", "green", "blue"), seq(0.25,0.75, 0.1))

### Leaflet map

m <- leaflet() %>%
  # Add multiple basemaps
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  # Add your polygons with group assignments, event listeners, and labels on hover
  addPolygons(data = tract,
              fillColor = ~pal(vote_pr_2024),
              weight = 0.5,
              color = "black",
              opacity = 0.3,
              group = "Tracts",
              layerId = ~row.names(tract),
              highlightOptions = highlightOptions(color = "red", weight = 3, bringToFront = TRUE))

mutate(fill_color = if_else(!is.na(region), "#1C00ff00", "#A9A9A9"))


%>%

  addPolygons(data = ld,
              weight = 1.5,
              color = "black",
              opacity = 1.0,
              fillOpacity = 0.04,
              fillColor = "grey",
              group = "Legislative Districts",
              layerId = ~ld$shape_id, # Use 'shape_id' for unique layer IDs
              highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE),
              # Add label on hover
              label = ~ld$map_id,
              labelOptions = labelOptions(noHide = FALSE, direction = 'auto')  # Show on hover
  ) %>%

  addPolygons(data = cd,
              weight = 1.5,
              color = "black",
              opacity = 1.0,
              fillOpacity = 0.04,
              fillColor = "grey",
              group = "Congressional Districts",
              layerId = ~cd$shape_id, # Use 'shape_id' for unique layer IDs
              highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE),

              # Add label on hover
              label = ~cd$shape_id,
              labelOptions = labelOptions(noHide = FALSE, direction = 'auto') # Show on hover
  ) %>%

  # Add layer control to switch between basemaps and polygon layers
  addLayersControl(
    baseGroups = c("CartoDB Positron", "OpenStreetMap"),
    overlayGroups = c("Tracts", "Legislative Districts", "Congressional Districts"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Enable zoom animation for smoother transitions
  setView(lng = -110.9, lat = 32.2, zoom = 6) %>%

  # Add JavaScript code to show/hide tracts on hover
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      var tractLayer = map.layerManager.getLayer('Overlay', 'Tracts');

      // Function to show/hide tracts based on hovered district
      function updateTracts(e) {
        if (e.type === 'mouseover') {
          var hoveredDistrict = e.target;
          var districtGeom = hoveredDistrict.toGeoJSON().geometry;

          tractLayer.eachLayer(function(tract) {
            var tractGeom = tract.toGeoJSON().geometry;
            if (turf.intersect(districtGeom, tractGeom)) {
              tract.setStyle({fillOpacity: 0.2}); // Show tract
            } else {
              tract.setStyle({fillOpacity: 0}); // Hide tract
            }
          });
        } else if (e.type === 'mouseout') {
          // Reset tract fillOpacity on mouseout
          tractLayer.eachLayer(function(tract) {
            tract.setStyle({fillOpacity: 0.2});
          });
        }
      }

      // Add event listeners to ld and cd layers
      map.eachLayer(function(layer) {
        if (layer.options.group === 'Legislative Districts' || layer.options.group === 'Congressional Districts') {
          layer.on('mouseover', updateTracts);
          layer.on('mouseout', updateTracts);
        }
      });
    }
  ")

m
# Create the Leaflet map
# Create the Leaflet map
m <- leaflet() %>%
  # Add multiple basemaps
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%

  # Add your polygons with group assignments and event listeners
  addPolygons(data = tract,
              weight = 0.5,
              color = "black",
              opacity = 1.0,
              fillOpacity = 0.2,
              fillColor = "grey",
              group = "Tracts",
              highlightOptions = highlightOptions(color = "yellow", weight = 3, bringToFront = TRUE)) %>%

  addPolygons(data = ld,
              weight = 1.5,
              color = "black",
              opacity = 1.0,
              fillOpacity = 0.04,
              fillColor = "white",
              group = "Legislative Districts",
              layerId = ~row.names(ld$X),
              highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE)) %>%

  addPolygons(data = cd,
              weight = 1.5,
              color = "black",
              opacity = 1.0,
              fillOpacity = 0.04,
              fillColor = "white",
              group = "Congressional Districts",
              layerId = ~row.names(cd$X),
              highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)) %>%

  # Add layer control to switch between basemaps and polygon layers
  addLayersControl(
    baseGroups = c("CartoDB Positron", "OpenStreetMap"),
    overlayGroups = c("Tracts", "Legislative Districts", "Congressional Districts"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Add labels for Legislative Districts
  addLabelOnlyMarkers(
    data = st_centroid(ld),  # Calculate centroids for labels
    label = ~ld$shape_id,         # Assuming 'ld' has a 'NAME' column for labels
    labelOptions = labelOptions(noHide = TRUE, direction = 'center')
  ) %>%

  # Add labels for Congressional Districts
  addLabelOnlyMarkers(
    data = st_centroid(cd),  # Calculate centroids for labels
    label = ~cd$shape_id,         # Assuming 'cd' has a 'NAME' column for labels
    labelOptions = labelOptions(noHide = TRUE, direction = 'center')
  )
# Only show labels on click

# Add JavaScript code to handle click events, zoom, and labels


# Display the map
m
