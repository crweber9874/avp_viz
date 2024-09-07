library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(plotly)


library(plotly)

ui <- fluidPage(
  titlePanel("Politics in Arizona's Nine Congressional Districts"),


  fluidRow(
    column(width = 4, offset = 1,
           selectInput("variable", "Color the map below by:",
                       choices = c("Registered Independent" = "percentIndependent",
                                   "Registered Republican" = "percentRepublican",
                                   "Registered Democrat" = "percentDemocrat",
                                   "Biden Vote (2020)" = "votePresD_2020",
                                   "Trump Vote (2020)" = "votePresR_2020",
                                   "Clinton Vote (2016)" = "votePresD_2020",
                                   "Trump Vote (2016)" = "votePresR_2016")



    )
  )),
#  column(width = 3,
#         uiOutput(outputId = "map_title")),
fluidRow(
  column(width = 6,
         height = 4,
         leafletOutput(outputId = "map", height = "400px")
  ),
  column(width = 6,
            height = 8,
            uiOutput(outputId = "types")),
  fluidRow(
    column(width = 6,
           tags$p("Note: Drag the marker to interact with the map and display the characteristics of Arizona's congressional districts.
                  The map can be colored by voting in the 2016 and 2020 election, along with the percentage of Republican, Independent,
                  and Democratic voters in each district, updated in March 2023.",
                  style = "font-size: 14px; color: gray; margin-top: 10px;")
    ))),
# Add a space
fluidRow(
  column(width = 10,
         HTML("<p></p>")
  )
),
fluidRow(
  column(width = 6,
         plotlyOutput(outputId = "voteD")
  ),
  column(width = 6,
         plotlyOutput(outputId = "voteR")
  )
 ),
# Add a space
fluidRow(
  column(width = 12,
         height = 5,
         HTML("<p></p>")
  )
),
)


