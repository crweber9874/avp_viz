################# Begin Preamble ###############
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet.extras)
library(plotly)
library(jsonlite)
library(tidyr)
library(plotly)
library(GGally)

includeCSS("www/style.css")

load("cd_public.rda")
## Color Palette

source("functions.R")


azblue =  az_color("azblue")
azred  =   az_color("azred")
oasis  =  az_color("oasis")
azgrey  =  az_color("azgrey")


# The working data
dat = voting %>%
  mutate(
    votePresD_2016 = as.numeric(Pres2016_D),
    votePresD_2020 = as.numeric(Pres2020_D),
    votePresR_2016 = 100 - as.numeric(Pres2016_D),
    votePresR_2020 = 100 - as.numeric(Pres2020_D),
    partisan_balance = rowMeans(cbind(votePresD_2020 - votePresR_2020, votePresD_2016 - votePresR_2016)),
    partisan_tilt = cut(partisan_balance, breaks = c(-Inf, -15, -3, 3, 15, Inf),
                               labels = c("Strong Republican", "Leans Republican", "Toss Up", "Leans Democrat", "Strong Democrat")),
    percentRepublican = as.numeric(republican_registration)*100,
    percentDemocrat = as.numeric(democratic_registration)*100,
    percentIndependent = as.numeric(independent_registration)*100,
    totalVoters       = total_voters,
    earlyVoter    = early_voter/general2022,
    pollingVoter  = polling_voters/general2022,
    provisionalVoters = provisional_voters/general2022) %>%
    # Order partisan_balance and create a variable that shows the rank ordering of each row
  select(c(CD, votePresD_2020, votePresD_2016, votePresR_2020, votePresR_2016, partisan_tilt, partisan_balance, percentRepublican,
           percentDemocrat, percentIndependent, totalVoters,
           earlyVoter, pollingVoter, provisionalVoters, shape_geom))

###

# This should be created in necessary long fofrmat requests

vote_data = dat %>%subset(select = c(CD, votePresD_2016,
                  votePresD_2020,
                  votePresR_2016,
                  votePresR_2020)) %>%
pivot_longer(cols = c(votePresD_2016,
                      votePresD_2020,
                      votePresR_2016,
                      votePresR_2020),
               names_to = "label", values_to = "value") %>%
  mutate(Democrat = ifelse(grepl("D", label), "Democrat", "Republican")) %>%
  mutate(year = as.numeric(gsub("\\D", "", label)))

### Democratic Line Plot ###

### Republican line plot

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
shape_properties_extracted <- dat$shape_properties
parsed_data <- list()

# Loop through each shape property
for (shape_property in shape_properties_extracted) {
  parsed_list <- fromJSON(shape_property)
  parsed_data <- append(parsed_data, list(parsed_list))
}

# Combine the parsed data into a data frame
# vote_data <- bind_rows(parsed_data)

server <- function(input, output) {
 # Set UA lat lon
   LAT =  32.228779
   LON = - 110.976743


  variable_name = reactiveVal("Predicted Vote in 2024")

  library(RColorBrewer)


  # Load the data
  output$map <- renderLeaflet({


selected_var <- input$variable


display_name <- switch(selected_var,
                       "percentIndependent" = "Registered Independent Voters",
                       "percentRepublican" = "Registered Republican Voters",
                       "percentDemocrat" = "Registered Democrat Voters",
                       "votePresD_2020" = "Biden Vote (2020)",
                       "votePresR_2020" = "Trump Vote (2020)",
                       "votePresD_2016" = "Clinton Vote (2016)",
                       "votePresR_2016" = "Trump Vote (2016)")

    min <- min(dat[[selected_var]], na.rm = TRUE)
    max <- max(dat[[selected_var]], na.rm = TRUE)
    pal <- colorNumeric(palette = brewer.pal(7, "YlGnBu"), domain = c(0, 1))
    ques = (dat[[selected_var]] - min) / (max - min)

print(display_name)
    leaflet(dat) %>%
      setView(lng = -111.9309, lat = 34.1682, zoom = 6) %>%
      addPolygons(
        color = "black",
        fillColor = ~pal(ques),
        weight = 1,
        label = lapply(seq_len(nrow(dat)), function(i) {
          paste0(
            "<b>Congressional District:</b> ", dat$CD[i], # Add the LD label here
            "<br>",
            "<b>", display_name, ": ",  "</b>",round(dat[[selected_var]][i], 2), "%",
            "<br>",
            "<b>Partisan Tilt:</b> ", dat[["partisan_tilt"]][i],
            "<br>"

            ) %>%
            htmltools::HTML()
        })      ,
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
                title = "CD") %>%
      addLabelOnlyMarkers(
        data = dat,
        lng = ~st_coordinates(st_centroid(shape_geom))[,1], # longitude from centroid
        lat = ~st_coordinates(st_centroid(shape_geom))[,2], # latitude from centroid
        label = ~as.character(dat),
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

  intersecting_geom_reactive <- reactiveVal(7)

  # Set the variable name
  output$variable <- renderText({
    input$variable
  })

  observeEvent(input$map_marker_dragend, {
    rd <- state_boundary(shapefile = dat,
                         markers = data.frame(lat = input$map_marker_dragend$lat, lon = input$map_marker_dragend$lng))

    if(nrow(rd) == 0){
      showNotification("Error: No data for this location, this tool is for the state of Arizona",
                       id = "region_error")

    } else {
      current_markers$lat <- input$map_marker_dragend$lat
      current_markers$lon <- input$map_marker_dragend$lng

      # Find the intersecting geometry
      intersecting_geom <- dat[st_intersects(dat, st_sfc(st_point(c(input$map_marker_dragend$lng, input$map_marker_dragend$lat)), crs = st_crs(dat))) %>% lengths > 0, ]

      if (nrow(intersecting_geom) > 0) {
        # Print the geometry type (assuming 'shape_geom' contains geometry type information)
        cat("Marker is within LD type:", intersecting_geom$CD, "\n")

        # Update filtered_data with intersecting geometries
        filtered_data(intersecting_geom)
        intersecting_geom_reactive(intersecting_geom$CD)

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
    intersecting_geom <- dat[which(st_intersects(dat, st_sfc(st_point(c(input$map_shape_click$lng, input$map_shape_click$lat)), crs = st_crs(dat))) %>% lengths > 0), ]

    if (nrow(intersecting_geom) > 0) {
      # Print the geometry type (assuming 'shape_geom' contains geometry type information)
      cat("Marker is within geometry type:", intersecting_geom$CD, "\n")
      intersecting_geom_reactive(intersecting_geom$CD)

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
   dat %>% as.data.frame() %>%
     filter(CD == intersecting_geom_reactive()) %>%
     subset(select = c(percentDemocrat, percentIndependent, percentRepublican)) %>%
     rename(Republican = percentRepublican,
            Democrat = percentDemocrat,
            Independent = percentIndependent) %>%
     pivot_longer(cols = c(Republican, Democrat, Independent),
                  names_to = "labels", values_to = "values") %>%
     pie_chart()
 }
 )

 output$voteD <- renderPlotly({

   dat <- vote_data %>% as.data.frame() %>%
     filter(Democrat == "Democrat") %>%
     mutate(value = as.numeric(value)) %>%  # Ensure value is numeric
     mutate(plotval = ifelse(year == 2016, value, -2)) %>%
     group_by(CD)

   fig <- dat %>%
     plot_ly(x = ~year) %>%
     add_trace(y = ~value,
               type = 'scatter',
               mode = 'lines',
               line = list(color = az_color("oasis"), width = 1, opacity = 0.001),
               name = 'Democratic voting') %>%
        add_markers(y = ~as.numeric(value),
                    marker = list(color = az_color("oasis"), size = 20, opacity = 1), text = ~paste("<br>CD:", CD,"<br>Democrat Presidential Vote:", value, "%"), hoverinfo = 'text') %>%
     layout(
       title = "Democratic Presidential Vote",
       annotations = list(
         x = ~paste0(2016),
         y = ~ plotval,
         text = ~paste0("CD: ", CD),
         xanchor = 'left',
         yanchor = 'bottom',
         showarrow = FALSE,
         font = list(size = 10, color = 'rgba(67,67,67,1)')
       ),
       xaxis = list(
         tickwidth = 2,
         tickvals = unique(dat$year),
         title = "Year"
       ),
       yaxis = list(
         title = "Democratic Presidential Vote",
         range = c(20, 90)
       ),
       showlegend = FALSE , # Remove the legend

       hoverlabel = list(
         bgcolor = "white",  # Background color
         bordercolor = "black",  # Border color
         font = list(
           size = 12,  # Font size
           color = "black"  # Font color
         )
       )
     )
   filtered_dat = vote_data %>%
     # Use listener here
     filter(CD == intersecting_geom_reactive()) %>%
     filter(Democrat == "Democrat")

   fig %>%
     add_trace(data = filtered_dat, y = ~value, type = 'scatter', mode = 'lines', line = list(color = 'red', width = 4)) %>%
     add_markers(data = filtered_dat, y = ~value, marker = list(color = 'red', size = 20, opacity = 0.90), text = ~paste("<br>CD:", CD,"<br>Democrat Presidential Vote:", value, " %"), hoverinfo = 'text')
    })

 output$voteR <- renderPlotly({

   dat <- vote_data %>% as.data.frame() %>%
     filter(Democrat == "Republican") %>%
     mutate(value = as.numeric(value)) %>%  # Ensure value is numeric
     mutate(plotval = ifelse(year == 2016, value, -2)) %>%
     group_by(CD)

   fig <- dat %>%
     plot_ly(x = ~year) %>%
     add_trace(y = ~value,
               type = 'scatter',
               mode = 'lines',
               line = list(color = az_color("oasis"), width = 1, opacity = 0.001),
               name = 'Republican voting') %>%
     add_markers(y = ~as.numeric(value),
                 marker = list(color = az_color("oasis"), size = 20, opacity = 1), text = ~paste("<br>CD:", CD,"<br>Republican Presidential Vote:", value, "%"), hoverinfo = 'text') %>%
     layout(
         title = "Republican Presidential Vote",

       annotations = list(
         x = ~paste0(2016),
         y = ~ plotval,
         text = ~paste0("CD: ", CD),
         xanchor = 'left',
         yanchor = 'bottom',
         showarrow = FALSE,
         font = list(size = 10, color = 'rgba(67,67,67,1)')
       ),
       xaxis = list(
         tickwidth = 2,
         tickvals = unique(dat$year),
         title = "Year"
       ),
       yaxis = list(
         title = "Percent",
         range = c(20, 90)
       ),
       showlegend = FALSE , # Remove the legend

       hoverlabel = list(
         bgcolor = "white",  # Background color
         bordercolor = "black",  # Border color
         font = list(
           size = 12,  # Font size
           color = "black"  # Font color
         )
       )
     )
   filtered_dat = vote_data %>%
     # Use listener here
     filter(CD == intersecting_geom_reactive()) %>%
     filter(Democrat == "Republican")

   fig %>%
     add_trace(data = filtered_dat, y = ~value, type = 'scatter', mode = 'lines', line = list(color = 'red', width = 4)) %>%
     add_markers(data = filtered_dat, y = ~value, marker = list(color = 'red', size = 20, opacity = 0.90), text = ~paste("<br>CD:", CD,"<br>Democrat Presidential Vote:", value, " %"), hoverinfo = 'text')
 })




 output$types <- renderUI({
  geom_data <- dat %>% filter(CD == intersecting_geom_reactive())

  if (!is.null(geom_data)) {


    # Extract and format the desired columns with row markers
    formatted_text <- paste0(
      paste0("<b> Congressional District </b>", "<b>", geom_data$CD, "</b>"),
      "<hr style='border: none; border-top: 1px solid #000; width: 100px; margin: 10px 0;'>",  # Small vertical line

      "<div style='display: flex; justify-content: space-between;'>",
        "<div>",
    #  "<b style='font-size: 1.2em; padding-bottom: 10px;'>Legislative District:</b><br>", # Increase font size and add bottom padding
          "<b>Partisan Tilt</b><br>",
          "<b>Partisan Balance (Vote Dem - Vote Rep):</b><br>",
          "<b>Registered Independent</b><br>",
          "<b>Registered Republican</b><br>",
          "<b>Registered Democrat</b><br>",
          "<b>Early Voters (2022)</b><br>",
          "<b>Polling Place Voters (2022)</b><br>",
          "<b>Total Voters:</b><br>",
        "</div>",
        "<div style='text-align: right;'>",
#          geom_data$LD, "<br>",
          geom_data$partisan_tilt, "<br>",
          geom_data$partisan_balance, "<br>",
          round(geom_data$percentIndependent, 2), "%<br>",
          round(geom_data$percentRepublican, 2), "%<br>",
          round(geom_data$percentDemocrat, 2), "%<br>",
          round(geom_data$earlyVoter, 4)*100, "%<br>",
          round(geom_data$pollingVoter, 4)*100, "%<br>",
          format(geom_data$totalVoters, big.mark = ","), "<br>",
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

 output$cd <- renderUI({
   h3("Congressional District", intersecting_geom_reactive())
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






  # Add an observer to handle the zoom control
  observeEvent(input$zoomControl, {
    leafletProxy("map") %>%
      setView(zoom = input$zoomControl)
  })

  output$map_title <- renderUI({
    display_name <- switch(input$variable,
                           "percentIndependent" = "Registered Independent Voters",
                           "percentRepublican" = "Registered Republican Voters",
                           "percentDemocrat" = "Registered Democrat Voters",
                           "votePresD_2020" = "Biden Vote (2020)",
                           "votePresR_2020" = "Trump Vote (2020)",
                           "votePresD_2016" = "Clinton Vote (2016)",
                           "votePresR_2016" = "Trump Vote (2016)")
    h3(display_name, style = "color: grey; text-align: left;")
  })



}

