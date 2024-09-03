rm(list = ls())
source("global.R")

server <- function(input, output) {
 # Set UA lat lon
   LAT = 32.228779
   LON = - 110.976743
  variable_name = reactiveVal("Predicted Vote in 2024")
  # Load the data
  output$map <- renderLeaflet({
    selected_var <- input$variable

    min <- min(ld[[selected_var]], na.rm = TRUE)
    max <- max(ld[[selected_var]], na.rm = TRUE)
    pal <- colorNumeric("YlOrRd", domain = c(0, 1))
    ques = (ld[[selected_var]] - min) / (max - min)

    leaflet(ld) %>%
      setView(lng = -111.9309, lat = 34.1682, zoom = 6) %>%
      addPolygons(
        color = "black",
        fillColor = ~pal(ques),
        weight = 1,
        label = lapply(seq_len(nrow(ld)), function(i) {
          paste0(
            "<b>Legislative District:</b> ", ld$LD[i], # Add the LD label here
            "<br>",
            "<b>Vote Score:</b> ", round(ld[[selected_var]][i], 2),
            "<br>",
            "<b>Percent Republican:</b> ", round(ld$republican_registration[i] *100, 2), "<span style='font-weight: normal;'> %</span>",  # Replace with your actual column names
            "<br>",
            "<b>Percent Democrat:</b> ",   round(ld$democratic_registration[i] *100, 2), "<span style='font-weight: normal;'> %</span>",  # Replace with your actual column names
            "<br>",
            "<b>Percent Independent:</b> ", round(ld$independent_registration[i] *100, 2), "<span style='font-weight: normal;'> %</span>",  # Replace with your actual column names
            "<br>") %>%
            htmltools::HTML()
        }),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "sans-serif",
            "font-size" = "13px",
            "color" = "grey"
          )
        )
      ) %>%
      addMarkers(
        lng = LON,
        lat = LAT,
        options = markerOptions(draggable = TRUE, minZoom = 15, maxZoom = 20 )) %>%
      addTiles() %>%
      addLegend(pal = pal,
                values = ~ques,
                opacity = 0.7,
                title = input$variable %>% names()) %>%
      addControl(html = paste0("<h3>", names(input$variable), "</h3>"), position = "topleft") %>%
      addLabelOnlyMarkers(
        data = ld,
        lng = ~st_coordinates(st_centroid(shape_geom))[,1], # Extract longitude from centroid
        lat = ~st_coordinates(st_centroid(shape_geom))[,2], # Extract latitude from centroid
        label = ~as.character(LD), # Assuming 'LD' column contains the labels
        labelOptions = labelOptions(
          noHide = FALSE,
          direction = 'center',
          style = list("background-color" = "transparent") # Make the box transparent
        )
      )




#      addPopups(-112.074037, 33.448377, "content",
#                options = popupOptions(closeButton = FALSE)
#      ) %>%
#      addMarkers(~-112, ~34, popup = ~htmlEscape("blip"))
  })



  current_markers <- reactiveValues(
    lat = LAT, lon = LON)

  # Reactive value to store the filtered data based on marker interaction
  filtered_data <- reactiveVal(NULL)

  # This pulls the User's LD
  intersecting_geom_reactive <- reactiveVal(20)

  # Set the variable name
  output$variable <- renderText({
    input$variable
  })

  observeEvent(input$map_marker_dragend, {
    rd <- state_boundary(shapefile = ld,
                         markers = data.frame(lat = input$map_marker_dragend$lat, lon = input$map_marker_dragend$lng))

    if(nrow(rd) == 0){
      showNotification("Error: No data for this location, this tool is for the state of Arizona",
                       id = "region_error")

    } else {
      current_markers$lat <- input$map_marker_dragend$lat
      current_markers$lon <- input$map_marker_dragend$lng

      # Find the intersecting geometry
      intersecting_geom <- ld[st_intersects(ld, st_sfc(st_point(c(input$map_marker_dragend$lng, input$map_marker_dragend$lat)), crs = st_crs(ld))) %>% lengths > 0, ]

      if (nrow(intersecting_geom) > 0) {
        # Print the geometry type (assuming 'shape_geom' contains geometry type information)
        cat("Marker is within LD type:", intersecting_geom$LD, "\n")

        # Update filtered_data with intersecting geometries
        filtered_data(intersecting_geom)
        intersecting_geom_reactive(intersecting_geom$LD)

      } else {
        cat("Marker is not within any known geometry.\n")
      }
    }

# Make a dragaable marker that is observed at drag end
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = current_markers$lat,
                                   lng = current_markers$lon),
                 options = markerOptions(draggable = TRUE))
  })

  # Set observers for the map click event
  observeEvent(input$map_shape_click, {
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE )) %>%

    setView(lng = input$map_shape_click$lng, lat = input$map_shape_click$lat, zoom = 8) # Adjust zoom level as needed

    current_markers$lat <- input$map_shape_click$lat
    current_markers$lng <- input$map_shape_click$lng

    # Find the intersecting geometry
    intersecting_geom <- ld[which(st_intersects(ld, st_sfc(st_point(c(input$map_shape_click$lng, input$map_shape_click$lat)), crs = st_crs(ld))) %>% lengths > 0), ]

    if (nrow(intersecting_geom) > 0) {
      # Print the geometry type (assuming 'shape_geom' contains geometry type information)
      cat("Marker is within geometry type:", intersecting_geom$LD, "\n")
      intersecting_geom_reactive(intersecting_geom$LD)

      # Update filtered_data with intersecting geometries
      filtered_data(intersecting_geom)

    } else {
      cat("Marker is not within any known geometry.\n")
      intersecting_geom_reactive(NULL)

    }
  })

 output$shapeInfo <- renderText({
   print(intersecting_geom_reactive())
 })

 output$hist <- renderPlot({

   # Calculate the mean for the selected LD and variable
   mean <- ld %>%
     filter(LD == intersecting_geom_reactive()) %>%
     pull(input$variable)

   # Print the mean value (for debugging)
   print(mean)

 filtered_data <- ld %>% select(input$variable)
print(dim(filtered_data))

       ggplot(filtered_data, aes(x = .data[[input$variable]])) +
         geom_histogram(fill = "lightblue", color = "white", binwidth = 0.02) +

         # Add a vertical line at the mean with a label
         geom_vline(aes(xintercept = mean), color = "darkgrey", linetype = "dashed", size = 1) +

         # Add distribution median
         geom_vline(aes(xintercept = median(.data[[input$variable]])), color = "black", linetype = "solid", size = 1) +

         annotate("text", x = mean, y = Inf, label = paste0( "LD:", intersecting_geom_reactive(), " \nScore (Dashed)"),
                  vjust = 1.5, hjust = 0, color = "black", size = 4) +

         annotate("text", x = median(filtered_data[[input$variable]]), y = 3,
                  label = paste0("Arizona \nMedian \n(Black)"), vjust = 1.5, hjust = 1, color = "black", size = 4) +

         ggtheme +
         labs(
           title = "",
           x = "Voting Score",
           y = "Number of Legislative Districts"
         )

 })

 output$race <- renderUI({
  geom_data <- ld %>% filter(LD == intersecting_geom_reactive())

  if (!is.null(geom_data)) {

    # Extract and format the desired columns with row markers
    formatted_text <- paste(
      "<h3>Race/Ethnicity <sup>2</sup></h3>",
      "<div style='display: flex; justify-content: space-between;'>",
        "<div>",
    #  "<b style='font-size: 1.2em; padding-bottom: 10px;'>Legislative District:</b><br>", # Increase font size and add bottom padding
          "<b style='background-color: #f2f2f2; padding: 1px;'>Total Population:</b><br>",
          "<b>White:</b><br>",
          "<b style='background-color: #f2f2f2; padding: 1px;'>Black:</b><br>",
          "<b>American Indian:</b><br>",
          "<b style='background-color: #f2f2f2; padding: 1px;'>Asian:</b><br>",
          "<b>Other Race:</b><br>",
          "<b style='background-color: #f2f2f2; padding: 1px;'>Two or More Races:</b><br>",
          "<b>Latino:</b>",
        "</div>",
        "<div style='text-align: right;'>",
#          geom_data$LD, "<br>",
          format(geom_data$total_population, big.mark = ","), "<br>",
          round(geom_data$white_proportion * 100, 2), "%<br>",
          round(geom_data$black_proportion * 100, 2), "%<br>",
          round(geom_data$native_proportion * 100, 2), "%<br>",
          round(geom_data$asian_proportion * 100, 2), "%<br>",
          round(geom_data$other_race_proportion * 100, 2), "%<br>",
          round(geom_data$two_or_more_race * 100, 2), "%<br>",
          round(geom_data$latino * 100, 2), "%",
        "</div>",
      "</div>"
    )

    # Wrap the formatted text in a div with card-like styling and 3D effect
    div(class = "card", style = "padding: 20px; margin-top: 10px;
                                box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.2);",
        HTML(formatted_text)
    )

  } else {
    div(class = "card", style = "padding: 20px; margin-top: 10px;
                                box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.2);",
        p("No geometry selected yet.")
    )
  }
})




 output$economics <- renderUI({
   geom_data <- ld %>% filter(LD == intersecting_geom_reactive())

   if (!is.null(geom_data)) {

     # Extract and format the desired columns with row markers
     formatted_text <- paste(
       "<h3>Demographics <sup>3</sup></h3>",
       "<div style='display: flex; justify-content: space-between;'>",
       "<div>",
       #  "<b style='font-size: 1.2em; padding-bottom: 10px;'>Legislative District:</b><br>", # Increase font size and add bottom padding
       "<b>Poverty Rate:</b><br>",
       "<b style='background-color: #f2f2f2; padding: 1px;'>Median Household Income:</b><br>",
       "<b>Median Age:</b><br>",
       "</div>",
       "<div style='text-align: right;'>",
       #          geom_data$LD, "<br>",
       round(geom_data$poverty_rate * 100, 2), "%<br>",
       "$",format(geom_data$median_household_income, big.mark = ","), "<br>",
       round(geom_data$median_age, 2), "years<br>",
       "</div>",
       "</div>"
     )

     # Wrap the formatted text in a div with card-like styling and 3D effect
     div(class = "card", style = "padding: 20px; margin-top: 10px;
                                box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.2);",
         HTML(formatted_text)
     )

   } else {
     div(class = "card", style = "padding: 20px; margin-top: 10px;
                                box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.2);",
         p("No geometry selected yet.")
     )
   }
 })



  # Add an observer to handle the zoom control
  observeEvent(input$zoomControl, {
    leafletProxy("map") %>%
      setView(zoom = input$zoomControl)
  })

  output$map_title <- renderUI({
    if(input$variable == "voterPrediction") {
      h2("Predicted Voting in 2024 Election")
    } else if(input$variable == "generalVoterScore") {
      h2("General Voting Score")
    } else if(input$variable == "primaryVoterScore") {
      h2("Primary Voting Score")
    }
  })

}

