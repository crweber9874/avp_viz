### Congressional District Map and Info

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
library(shinydashboard)

### Data ###
load("cd_public.rda")
load("tract_cd_public.rda")

# Some functions and styling stuff
includeCSS("www/style.css")
source("functions.R")
source("global.R")



## Slight data transformations
cdDat = cd %>%
  mutate(
    percentRepublican = as.numeric(republican_registration)*100,
    percentDemocrat   = as.numeric(democratic_registration)*100,
    percentIndependent = as.numeric(independent_registration)*100,
    totalVoters       = total_voters,
    earlyVoter        = early_voter/general2022,
    pollingVoter      = polling_voters/general2022,
    provisionalVoters = provisional_voters/general2022) %>%
  dplyr::select(c(CD, percentRepublican,
                  percentDemocrat, percentIndependent, totalVoters,
                  earlyVoter, pollingVoter, provisionalVoters, shape_geom))


tractDat = tracts_cd %>%
  mutate(
    percentRepublican = as.numeric(republican_registration)*100,
    percentDemocrat   = as.numeric(democratic_registration)*100,
    percentIndependent = as.numeric(independent_registration)*100,
    totalVoters       = total_voters,
    earlyVoter        = early_voter/general2022,
    pollingVoter      = polling_voters/general2022,
    provisionalVoters = provisional_voters/general2022) %>%
  dplyr::select(c(shape_fips, CD, percentRepublican,
                  percentDemocrat, percentIndependent, totalVoters,
                  earlyVoter, pollingVoter, provisionalVoters, shape_geom))


##### Custom Palettes

azReds <- colorRampPalette(c( az_color("azgrey"),  az_color("chili"), az_color("azred")))
azBlues <- colorRampPalette(c( az_color("azgrey"),  az_color("oasis"), az_color("azblue")))
azGreys <- colorRampPalette(c( az_color("white"),  az_color("azgrey"), az_color("midnight")))



### Leaflet map
create_leaflet_map <- function(cdDat, pal, ques, title, district) {
  leaflet(cdDat) %>%
    clearShapes()%>%
    clearControls()%>%
    clearMarkers()%>%
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
      fillOpacity = 0.7,
      label = lapply(seq_len(nrow(cdDat)), function(i) {
        ~paste0("Congressional District: ", cdDat$CD[i], "<br>",
                "Registered Republican", " : ", round(cdDat$percentRepublican[i], 2), "%", "</br>",
                "Registered Independent", " : ", round(cdDat$percentIndependent[i], 2), "%", "</br>",
                "Registered Democrat", " : ", round(cdDat$percentDemocrat[i], 2), "%", "</br>"
        ) %>%
          htmltools::HTML()
      }), # Add hover tooltip
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px",
          "background-color" = "rgba(255, 255, 255, 0.8)", # Transparent background
          "text-align" = "left" # Left justified text
        ),
        textsize = "15px",
        direction = "auto"
     ))%>%
    leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>%
    addLegend(
      pal = pal,
      values = ~ques,
      opacity = 0.5,
      title = title,
      position = "bottomleft"
    )
}


### Server Logic ####

server <- function(input, output) {
  variable_name = reactiveVal("Predicted Vote in 2024")
  # CD Hovers
  cd_reactive = reactiveVal(NULL)
  # CD geoocodes
  cd_geocode = reactiveVal(NULL)
  # cd clicks
  click_reactive = reactiveVal(2)
  # tract clicks
  tract_click_reactive = reactiveVal(NULL)
  filtered = reactiveVal(NULL)
  first_click_done <- reactiveVal(FALSE)


  output$map <- renderLeaflet({
    selected_var <- input$variable
    ques = cdDat[[selected_var]]
    min = min(ques, na.rm = T)
    max = max(ques, na.rm = T)

    if(selected_var == "percentDemocrat"){
      pal <- colorNumeric(palette = azBlues(10), domain = c(min, max))
    } else if(selected_var == "percentIndependent"){
      pal <- colorNumeric(palette = azGreys(10), domain = c(min, max))
    } else {
      pal <- colorNumeric(palette = azReds(10), domain = c(min, max))
    }


    display_name <- switch(selected_var,
                           "percentIndependent" = "Independents",
                           "percentRepublican"  = "Republicans",
                           "percentDemocrat"    = "Democrats")
    # Construct the map
    create_leaflet_map(cdDat, pal, ques, title = display_name)
  })


  observeEvent(input$map_shape_click$id,{
    if (!first_click_done()) {
      selected_cd = click_reactive(input$map_shape_click$id)
      click_reactive(input$map_shape_click$id)
      print(input$map_shape_click$id)
      if (!is.null(selected_cd)) {
        filtered(tractDat %>% filter(CD == input$map_shape_click$id))
        # Double-click detected, just zoom in
        selected_var <- input$variable
        filtered = filtered()
         centroid <- st_centroid(st_union(filtered$shape_geom))
        centroid_coords <- st_coordinates(centroid)

        display_name <- switch(selected_var,
                               "percentIndependent" = "Independents",
                               "percentRepublican" = "Republicans",
                               "percentDemocrat" = "Democrats")



        if (nrow(filtered) > 0) {

          ques = filtered[[selected_var]]
          min = min(ques, na.rm = TRUE)
          max = max(ques, na.rm = TRUE)


          if(selected_var == "percentDemocrat"){
            pal <- colorNumeric(palette = azBlues(10), domain = c(min, max))
          } else if(selected_var == "percentIndependent"){
            pal <- colorNumeric(palette = azGreys(10), domain = c(min, max))
          } else {
            pal <- colorNumeric(palette = azReds(10), domain = c(min, max))
          }

          bbox <- st_bbox(filtered$shape_geom)

          leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(
              data = filtered,
              layerId = ~shape_fips,
              fillColor = ~pal(ques),
              weight = 2,
              fillOpacity = 0.8,
              color = "black",
              label = lapply(seq_len(nrow(filtered)), function(i) {
                ~paste0("This tract is located in Congressional District: ", filtered$CD[i], "<br>",
                        "Registered Republican", " : ",  round(filtered$percentRepublican[i], 2), "%", "</br>",
                        "Registered Independent", " : ", round(filtered$percentIndependent[i], 2), "%", "</br>",
                        "Registered Democrat", " : ",    round(filtered$percentDemocrat[i], 2), "%", "</br>"
                ) %>%
                  htmltools::HTML()
              }), # Add hover tooltip
              labelOptions = labelOptions(
                style = list(
                  "font-weight" = "normal",
                  padding = "3px 8px",
                  "background-color" = "rgba(255, 255, 255, 0.8)",
                  "text-align" = "left"
                ),
                textsize = "15px",
                direction = "auto"
              )
            ) %>%
            fitBounds(lng1 = bbox["xmin"], lat1 = bbox["ymin"], lng2 = bbox["xmax"], lat2 = bbox["ymax"]) %>%
            setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = 7) %>%
            leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE))

        } else {
          print("Filtered data is empty.")
        }
      }
      first_click_done(TRUE)
    }
  })


  observeEvent(input$reset_map, {

    req(first_click_done())

    filtered(cdDat)

    selected_var <- input$variable

    display_name <- switch(selected_var,
                           "percentIndependent" = "Independents",
                           "percentRepublican" = "Republicans",
                           "percentDemocrat" = "Democrats")


    ques <- cdDat[[selected_var]]
    min = min(ques, na.rm = T)
    max = max(ques, na.rm = T)


    if(selected_var == "percentDemocrat"){
      pal <- colorNumeric(palette = azBlues(10), domain = c(min, max))
    } else if(selected_var == "percentIndependent"){
      pal <- colorNumeric(palette = azGreys(10), domain = c(min, max))
    } else {
      pal <- colorNumeric(palette = azReds(7), domain = c(min, max))
    }
    leafletProxy("map") %>%
      clearShapes()%>%
      clearMarkers()%>%
      clearControls()%>%
      fitBounds(lng1 = -114.818269, lat1 = 31.332177, lng2 = -109.045223, lat2 = 37.00426) %>% # Arizona bounds
      setView(lng = -111.9309, lat = 34.1682, zoom = 6) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Stadia.StamenToner, group = "Light") %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, group = "Dark")%>%
      # Add layer control
      addLayersControl(
        baseGroups = c("Street Map", "Light", "Dark"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(
        data = cdDat,
        color = "black",
        fillColor = ~pal(ques),
        weight = 1,
        layerId = ~CD,
        fillOpacity = 0.7,
        label = lapply(seq_len(nrow(cdDat)), function(i) {
          ~paste0("Congressional District: ", cdDat$CD[i], "<br>",
                  "Registered Republican",  " : ",  round(cdDat$percentRepublican[i], 2),  "%", "</br>",
                  "Registered Independent", " : ",  round(cdDat$percentIndependent[i], 2), "%", "</br>",
                  "Registered Democrat",    " : ",  round(cdDat$percentDemocrat[i], 2),    "%", "</br>"
          ) %>%
            htmltools::HTML()
        }),
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px",
            "background-color" = "rgba(255, 255, 255, 0.8)",
            "text-align" = "left" #
          ),
          textsize = "15px",
          direction = "auto"
        )) %>%
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>%
      addLegend(
        data = cdDat,
        pal = pal,
        values = ~ques,
        opacity = 0.7,
        title = display_name,
        position = "bottomleft"
      )
    print(click_reactive)
    first_click_done(FALSE)
    click_reactive(2)

  })
#### Analytics
###  Republicans, Histogram

  output$repHist <- renderPlotly({

    selectedCD = click_reactive()
    if(!is.null(click_reactive())){
      filtered = tractDat %>%
        filter(CD == click_reactive())
      cdTractmean =
        filtered %>%
        group_by(CD) %>%
        summarise(midRepublican = quantile(percentRepublican, 0.5))

    }

    azmean =
      quantile(cdDat$percentRepublican, 0.5)[1]

    print(azmean)

    if(nrow(filtered) > 0){
      tractDat %>% as.data.frame() %>%
        plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~ percentRepublican,
                      histnorm = "percent",
                      name = "Arizona" ,
                      text = ~paste("Percent Republican in Arizona: ", round(percentRepublican, 2), "%"),
                      hoverinfo = "text",
                      alpha = 0.8,
                      color = I(az_color("midnight"))) %>%
        add_histogram(data = filtered,
                      nbinsx = 60,
                      x = ~ filtered$percentRepublican,
                      histnorm = "percent",
                      name = paste0("CD ", click_reactive()) , color = I(az_color("azred")),
                      text = ~paste0("Percent Republican in CD ", click_reactive(), "\n", round(filtered$percentRepublican, 2), "%"),
                      hoverinfo = "text",
                      alpha = 0.5) %>%
        add_lines(x = c(cdTractmean$midRepublican, cdTractmean$midRepublican),
                  y = c(0, 20),
                  line = list(color = az_color("oasis"), dash = 'dash'),
                  name = paste("CD", click_reactive(),  "Median")) %>%
        add_lines(x = c(azmean, azmean),
                  y = c(0, 20),
                  line = list(color = az_color("az_red"), dash = 'solid'), name = paste0("Arizona Median")) %>%

        layout(
          title = paste0("Republican Registration"),
          xaxis = list(
            title = "Census Tracts",
            range = c(0,60)

          ),
          yaxis = list(
            title = ""
          ),
          legend = list(
            x = 0,
            y = 0.90
          )
        )
    }
    else{
      tractDat %>% as.data.frame() %>%
        plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~ percentRepublican,
                      histnorm = "percent",
                      name = "Arizona" ,
                      text = ~paste("Percent Republican in Arizona: ", round(percentRepublican, 2), "%"),
                      hoverinfo = "text", # Display custom hover text
                      alpha = 0.2,
                      color = I(az_color("midnight"))) %>%
        layout(
          title = paste0("Republican Registration"),
          xaxis = list(
            title = "Census Tracts",
            range = c(0,60)

          ),
          yaxis = list(
            title = ""
          ),
          legend = list(
            x = 0,
            y = 0.90
          )
        )
    }
  })


## Information card
  output$types <- renderUI({
    selected_cd = click_reactive()
    geom_data <- cdDat %>% filter(CD == selected_cd)

    if (!is.null(geom_data)) {
      formatted_text <- paste0(
        paste0("<b> Congressional District </b>", "<b>", geom_data$CD, "</b>"),
        # ADD TO STYLE CSS
        "<hr style='border: none; border-top: 1px solid #000; width: 100px; margin: 10px 0;'>",  # Small vertical line
        "<div style='display: flex; justify-content: space-between;'>",
        "<div>",
        "<b>Registered Independent</b><br>",
        "<b>Registered Republican</b><br>",
        "<b>Registered Democrat</b><br>",
        "<b>Early Voters (2022)</b><br>",
        "<b>Polling Place Voters (2022)</b><br>",
        "<b>Total Voters:</b><br>",
        "</div>",
        "<div style='text-align: right;'>",
        round(geom_data$percentIndependent, 2), "%<br>",
        round(geom_data$percentRepublican, 2), "%<br>",
        round(geom_data$percentDemocrat, 2), "%<br>",
        round(geom_data$earlyVoter, 4)*100, "%<br>",
        round(geom_data$pollingVoter, 4)*100, "%<br>",
        format(geom_data$totalVoters, big.mark = ","), "<br>",
        "</div>",
        "</div>"
      )

      div(class = "card", style = "padding: 10px; margin-top: 10px;
                                   box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.5);",
          HTML(formatted_text)
      )

    } else {
      div(class = "card", style = "padding: 20px; margin-top: 10px;
                                   box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.5);",
          p("No geometry selected yet.")
      )
    }
  })


  output$demHist <- renderPlotly({

    selectedCD = click_reactive()
    if(!is.null(click_reactive())){
      filtered = tractDat %>%
        filter(CD == click_reactive())
      cdTractmean =
        filtered %>%
        group_by(CD) %>%
        summarise(midDemocrat = quantile(percentDemocrat, 0.5))

    }

    azmean =
      quantile(cdDat$percentDemocrat, 0.5)[1]

    print(azmean)

    if(nrow(filtered) > 0){
      tractDat %>% as.data.frame() %>%
        plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~ percentDemocrat,
                      histnorm = "percent",
                      name = "Arizona" ,
                      text = ~paste("Percent Democratic in Arizona: ", round(percentDemocrat, 2), "%"), # Custom hover text
                      hoverinfo = "text", # Display custom hover text
                      alpha = 0.8,
                      color = I(az_color("midnight"))) %>%
        add_histogram(data = filtered,
                      nbinsx = 60,
                      x = ~ filtered$percentDemocrat,
                      histnorm = "percent",
                      name = paste0("CD ", click_reactive()) , color = I(az_color("azblue")),
                      text = ~paste0("Percent Independent in CD ", click_reactive(), "\n", round(filtered$percentDemocrat, 2), "%"),
                      hoverinfo = "text",
                      alpha = 0.5) %>%
        add_lines(x = c(cdTractmean$midDemocrat, cdTractmean$midDemocrat),
                  y = c(0, 20),
                  line = list(color = az_color("oasis"), dash = 'dash'),
                  name = paste("CD", click_reactive(),  "Median")) %>%
        add_lines(x = c(azmean, azmean),
                  y = c(0, 20),
                  line = list(color =  az_color("az_red"), dash = 'solid', size = 10), name = paste0("Arizona Median")) %>%

        layout(
          title = paste0("Democratic Registration"),
          xaxis = list(
            title = "Census Tracts",
            range = c(0,60)

          ),
          yaxis = list(
            title = ""
          ),
          legend = list(
            x = 0,
            y = 0.90
          )
        )
    }
    else{
      tractDat %>% as.data.frame() %>%
        plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~ percentIndependent,
                      histnorm = "percent",
                      name = "Arizona" ,
                      text = ~paste("Percent Independent in Arizona: ", round(percentIndependent, 2), "%"), # Custom hover text
                      hoverinfo = "text", # Display custom hover text
                      alpha = 0.2,
                      color = I(az_color("midnight"))) %>%
        layout(
          title = paste0("Independent Registration"),
          xaxis = list(
            title = "Census Tracts",
            range = c(0,60)

          ),
          yaxis = list(
            title = ""
          ),
          legend = list(
            x = 0,
            y = 0.90
          )
        )
    }
  })


output$indHist <- renderPlotly({


    selectedCD = click_reactive()
    if(!is.null(click_reactive())){
      filtered = tractDat %>%
        filter(CD == click_reactive())
      cdTractmean =
        filtered %>%
        group_by(CD) %>%
        summarise(midIndependent = quantile(percentIndependent, 0.5))

    }

    azmean =
      quantile(cdDat$percentIndependent, 0.5)[1]
    print(azmean)
if(nrow(filtered) > 0){
    tractDat %>% as.data.frame() %>%
      plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~ percentIndependent,
                    histnorm = "percent",
                    name = "Arizona" ,
                    text = ~paste("Percent Independent in Arizona: ", round(percentIndependent, 2), "%"), # Custom hover text
                    hoverinfo = "text", # Display custom hover text
                    alpha = 0.8,
                    color = I(az_color("midnight"))) %>%
      add_histogram(data = filtered,
                    nbinsx = 60,
                    x = ~ filtered$percentIndependent,
                    histnorm = "percent",
                    name = paste0("CD ", click_reactive()) , color = I("purple"),
                    text = ~paste0("Percent Independent in CD ", click_reactive(), "\n", round(filtered$percentIndependent, 2), "%"), # Custom hover text
                    hoverinfo = "text", # Display custom hover text
                    alpha = 0.5 ) %>% # Set the number of bins to 10 (adjust as needed)) %>%
    add_lines(x = c(cdTractmean$midIndependent, cdTractmean$midIndependent),
              y = c(0, 20),
              line = list(color = az_color("oasis"), dash = 'dash'),
              name = paste("CD", click_reactive(),  "Median")) %>%
    add_lines(x = c(azmean, azmean),
              y = c(0, 20),
              line = list(color = az_color("az_red"), dash = 'solid'), name = paste0("Arizona Median")) %>%

      layout(
        title = paste0("Independent Registration"),
        xaxis = list(
          title = "Census Tracts",
          range = c(0, 60)

        ),
        yaxis = list(
          title = ""
        ),
        legend = list(
          x = 0,
          y = 0.90
        )
      )
}
    else{
      tractDat %>% as.data.frame() %>%
        plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~ percentIndependent,
                      histnorm = "percent",
                      name = "Arizona" ,
                      text = ~paste("Percent Independent in Arizona: ", round(percentIndependent, 2), "%"), # Custom hover text
                      hoverinfo = "text", # Display custom hover text
                      alpha = 0.2,
                      color = I(az_color("midnight"))) %>%
        layout(
          title = paste0("Independent Registration"),
          xaxis = list(
            title = "Census Tracts",
            range = c(0,60)

          ),
          yaxis = list(
            title = ""
          ),
          legend = list(
            x = 0,
            y = 0.90
          )
        )
}
  })


### Labels
  output$clicked_cd_output<- renderUI({
    h3(paste("Statistics Displayed for Congressional District \n",
             click_reactive()))
  })


output$geo_cd<- renderUI({
  h3(paste("Searched address is in\n
           Congressional District \n",
           geoCD_reactive()))
})

}


#
#   geoCD_reactive= reactiveVal(NULL)
#
#   observeEvent(input$geocode, {
#     address <- input$address
#     if (address != "") {
#       # Use a geocoding service to get the latitude and longitude
#       url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(address), "&format=json&limit=1")
#       res <- httr::GET(url)
#       if (res$status_code == 200) {
#         geocode_data <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
#         if (length(geocode_data) > 0) {
#           lat <- as.numeric(geocode_data$lat)
#           lon <- as.numeric(geocode_data$lon)
#
#           # Create a point geometry
#           point <- st_sfc(st_point(c(lon, lat)), crs = st_crs(tractDat))
#
#           # Check if the point is within any of the polygons
#           contained <- st_contains(tractDat$shape_geom, point, sparse = FALSE)
#           contained_row <- which(contained, arr.ind = TRUE)
#           print(contained)
#           # Print the contained_row to the terminal
#
#           if (length(contained_row) > 0) {
#             # Get the corresponding polygon data
#             polygon_data <- tractDat[contained, ]
#             print(paste("Address is in tract:", polygon_data$shape_fips))
#             print(contained_row)
#
#             cd = print(tractDat %>%
#                          filter(shape_fips == polygon_data$shape_fips) )
#
#             geoCD_reactive(cd$CD)
#
#           } else {
#             showNotification("Address is not within any polygon", type = "warning")
#           }
#
#           leafletProxy("map") %>%
#             setView(lng = lon, lat = lat, zoom = 10) %>%
#             addMarkers(lng = lon, lat = lat, popup = address)
#         } else {
#           showNotification("Address not found", type = "error")
#         }
#       } else {
#         showNotification("Geocoding service error", type = "error")
#       }
#
#     }
#   })

