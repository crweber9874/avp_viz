## Build the Server for a Shiny App of Legislative Districts
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet.extras)
library(plotly)
library(jsonlite)
library(tidyr)
library(avpMap)

# Clean code
## Add function calls.

azblue =  az_color("azblue")
azred  =   az_color("azred")
oasis  =  az_color("oasis")

includeCSS("www/style.css")

load("ld_public.rda")


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


# Construct vote data from shape properties
shape_properties_extracted <- ld$shape_properties
parsed_data <- list()

# Loop through each shape property
for (shape_property in shape_properties_extracted) {
  parsed_list <- fromJSON(shape_property)
  parsed_data <- append(parsed_data, list(parsed_list))
}

# Combine the parsed data into a data frame
vote_data <- bind_rows(parsed_data)

server <- function(input, output) {
 # Set UA lat lon
   LAT =  32.228779
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
            "<b>Legislative District:</b> ", ld$LD[i],
            "<br>",
            "<b>Vote Score:</b> ", round(ld[[selected_var]][i], 2),
            "<br>") %>%
            htmltools::HTML()
        }),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "sans-serif",
            "font-size" = "13px",
            "color" = "rgba(128, 128, 128, 0.95)",
            "background-color" = "rgba(255, 255, 255, 0.5)"

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
      addLabelOnlyMarkers(
        data = ld,
        lng = ~st_coordinates(st_centroid(shape_geom))[,1], # longitude from centroid
        lat = ~st_coordinates(st_centroid(shape_geom))[,2], # latitude from centroid
        label = ~as.character(LD),
        labelOptions = labelOptions(
          noHide = FALSE,
          direction = 'center',
          style = list("background-color" = "transparent") # Style box
        )
      ) %>%
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE))
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

    setView(lng = input$map_shape_click$lng, lat = input$map_shape_click$lat, zoom = 9) # Adjust zoom level as needed

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

 output$pie <- renderPlotly({
   ld %>%
     filter(LD == intersecting_geom_reactive()) %>%
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
             insidetextfont = list(color = '#F4F4F4'),
             hoverinfo = 'text',
             text = ~paste0(round(Registration, 2)*100, '% of registered voters'),
             marker = list(
               colors = c("#AB0520", "#0C234B" ,"#378DBD"),
               line = list(color = '#FFFFFF', width = 1)
             ),
             hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.01)', # Fully transparent background
                               color = "white"
                               ),          # Keep text color black for visibility

             showlegend = FALSE) %>%
     config(displayModeBar = FALSE) %>%
     layout(title = list(
       text = paste0('Legislative District ', intersecting_geom_reactive()),
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
         geom_histogram(fill = oasis, color = "white", binwidth = 0.02) +

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
      "<h3>Race/Ethnicity </h3>",
      "<div style='display: flex; justify-content: space-between;'>",
        "<div>",
    #  "<b style='font-size: 1.2em; padding-bottom: 10px;'>Legislative District:</b><br>", # Increase font size and add bottom padding
          "<b>Total Population:</b><br>",
          "<b>White:</b><br>",
          "<b>Black:</b><br>",
          "<b>American Indian:</b><br>",
          "<b>Asian:</b><br>",
          "<b>Other Race:</b><br>",
          "<b>Two or More Races:</b><br>",
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




 output$registration <- renderUI({
   geom_data <- ld %>% filter(LD == intersecting_geom_reactive())

   if (!is.null(geom_data)) {

     # Extract and format the desired columns with row markers
     formatted_text <- paste(
       "<h3>Party Registration </h3>",
       "<div style='display: flex; justify-content: space-between;'>",
       "<div>",
       #  "<b style='font-size: 1.2em; padding-bottom: 10px;'>Legislative District:</b><br>", # Increase font size and add bottom padding
       "<b>Republican Registration:</b><br>",
       "<b>Independent:</b><br>",
       "<b>Democratic Registration:</b><br>",
       "<b>Total Registered Voters:</b><br>",
       "</div>",
       "<div style='text-align: right;'>",
       #          geom_data$LD, "<br>",
       format(geom_data$republican, big.mark = ","), "<br>",
       format(geom_data$independent,big.mark = ","), " <br>",
       format(geom_data$democrat, big.mark = ",") ,"<br>",
       format(geom_data$democrat + geom_data$democrat + geom_data$independent,  big.mark = ","), "<br>",
              "</div>",
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
       "<h3>Demographics</h3>",
       "<div style='display: flex; justify-content: space-between;'>",
       "<div>",
       #  "<b style='font-size: 1.2em; padding-bottom: 10px;'>Legislative District:</b><br>", # Increase font size and add bottom padding
       "<b>Poverty Rate:</b><br>",
       "<b>Median Household Income:</b><br>",
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

  output$ld <- renderUI({
      h3("Displayed: Legislative District", intersecting_geom_reactive())
  })

}

