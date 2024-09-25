library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(plotly)

source('functions.R')


ui <- fluidPage(
  titlePanel(
    div(
      style = "text-align: center;",
    )),
  fluidRow( # Row for selectInput and race table
    column(width = 4,
           selectInput("variable", "   Color the map below by:",
                       choices = c("Average General Participation" = "averageGeneral",
                                   "Average Primary Participation" = "averagePrimary",
                                   "Average Midterm Participation" = "averageMidterm",
                                   "Predicted Vote in 2024" = "voterPrediction",
                                   "General Voting Score" = "generalVoterScore",
                                   "Primary Voting Score" = "primaryVoterScore"
        )
      )
    )
  ),

  fluidRow(
    column(width = 5,
           uiOutput(outputId = "map_title")
    ),
    column(width = 5,
           uiOutput(outputId = "cd")
    )
  ),
  #         uiOutput(outputId = "map_title")),
  fluidRow(
    column(width = 5,
           height = 4,
           leafletOutput(outputId = "map", height = "400px")
    ),
    column(width = 5,
           height = 5,
           plotOutput(outputId = "hist"))
),
fluidRow(
  column(width = 5,
         height = 6,
         uiOutput(outputId = "demographics")),
  column(width = 5,
         height = 4,
         uiOutput(outputId = "voteChar")
  )),
# add a space
fluidRow(
  column(width = 10,
         height = 3,
         tags$p("Note: This tool explores voting rates in Arizona.
              The user may select a variable to visualize on both a map and a histogram of voting in the state.
              The map can be colored with 1 of 6 scores: (1) Expected number of presidential election voters;
              (2) Expected number of primary election voters; (3) Expected number of midterm election voters;
              (4) Average Predicted Vote in 2024: The voter averaged by county scores for predicted vote in the 2024 election.
              (5) General Voting Score: The district averaged score based on general election voting in the 2016, 2018, 2020, and 2022 elections.
              (6) Primary Voting Score: The district averaged score based on general election voting in the 2016, 2018, 2020, and 2022 elections.",
               style = "font-size: 14px; color: gray; margin-top: 10px;")
  )
)
)


# column(width = 6,
#        height = 1,
#        tags$p("Note: Drag the marker to interact with the map and display the characteristics of Arizona's congressional districts.
#                   The map can be colored by voting in the 2016 and 2020 election, along with the percentage of Republican, Independent,
#                   and Democratic voters in each district, updated in March 2023.",
#               style = "font-size: 14px; color: gray; margin-top: 10px;")
# )
