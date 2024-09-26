pie_chart <- function(data) {
  plotly::plot_ly(data,
                  labels = ~labels,  # Use column names directly
                  values = ~values,
                  type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#F4F4F4'),
                  hoverinfo = 'text',
                  text = ~paste0(round(values*100, 2) , '% of voters'),
                  marker = list(
                    colors = c("#AB0520", "#0C234B", "#378DBD"),
                    line = list(color = '#FFFFFF', width = 1)
                  ),
                  hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.01)',  # Fully transparent background
                                    color = "white"
                  ),
                  showlegend = FALSE) %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(title = list(
      text = "Party Registration",
      font = list(size = 16,
                  color = 'black',
                  family = "Arial, sans-serif",
                  weight = "bold")
    ),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
    )
}


az_color <- function(color = c("azblue", "azred", "oasis", "grey", "warmgrey", "midnight", "azurite", "chili", "white")) {
  if(color == "azblue") {return("#0C234B")}
  if(color == "azred")  {return("#AB0520")}
  if(color == "oasis")  {return("#378DBD")}
  if(color == "azgrey")   {return("#E2E9EB")}
  if(color == "warmgrey") {return("#F4EDE5")}
  if(color == "midnight") {return("#001C48")}
  if(color == "azurite") {return("#1E5288")}
  if(color == "chili") {return("#8B0015")}
  if(color == "azwhite") {return("#FFFFFF")}

}


az_color_list = list(
  az_template = c(az_color("azblue"), az_color("azred"), az_color("oasis"),
                  az_color("azgrey"), az_color("warmgrey"), az_color("midnight"),
                  az_color("azurite"), az_color("chili"), az_color("white")),
  fav_colors = c(az_color("azgrey"), az_color("azred"), az_color("oasis"))
)


### Leaflet map
create_leaflet_map <- function(cdDat, pal, ques, title, district) {
  leaflet(cdDat) %>%
    clearShapes()%>%
    fitBounds(lng1 = -114.818269, lat1 = 31.332177, lng2 = -109.045223, lat2 = 37.00426) %>%
    setView(lng = -111.9309, lat = 34.1682, zoom = 6.2) %>%
    addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
    addProviderTiles(providers$Stadia.StamenToner, group = "Light") %>%
    addProviderTiles(providers$Stadia.AlidadeSmoothDark, group = "Dark") %>%
    # Add layer control
    addLayersControl(
      baseGroups = c("Street Map", "Light", "Dark"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addPolygons(
      data  = cdDat,
      color = "black",
      fillColor = ~pal(ques),
      weight = 1,
      layerId = ~CD,
      fillOpacity = 0.7)%>%
    leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>%
    addLegend(
      pal = pal,
      values = ~ques,
      opacity = 0.5,
      title = title,
      position = "bottomleft" # Move the legend to the bottom right
    )
}


custom_palette <- colorRampPalette(az_color_list$fav_colors)


