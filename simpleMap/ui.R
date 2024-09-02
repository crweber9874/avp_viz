library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = paste("Voting in Arizona")),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Legislative Districts", tabName = "legislative_districts", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "legislative_districts",
    fluidRow(
                column(width = 12,
                       column(width = 8,
                              uiOutput(outputId = "map_title")
                       )
                )
              ),
    fluidRow(
        column(
          width = 12,
          HTML(
            "<p>
        This tool explores voting patterns in Arizona.
        You can select a variable to visualize on both a map and a histogram of voting in the state.
        We have created three scores:<sup>1</sup>
      </p>
      <ul>
        <li> Predicted Vote in 2024: This is the predicted vote in the 2024 election based on the 2020 election.</li>
        <li> Latent Variable General Voting Score: This is a score based on general voting patterns.</li>
        <li> Latent Variable Primary Voting Score: This is a score based on primary voting patterns.</li>
      </ul>"
          )
        )
      ),
        fluidRow( # Row for selectInput and race table
              column(width = 6,
                       selectInput("variable", "Choose variable to plot:",
                                   choices = c("Predicted Vote in 2024" = "voterPrediction",
                                               "Latent Variable General Voting Score" = "generalVoterScore",
                                               "Latent Variable Primary Voting Score" = "primaryVoterScore"
                                   )
                       )
                )
              ),
              fluidRow( # Row for map and histogram
                column(width = 6,
                       leafletOutput(outputId = "map", height = "400px")
                ),
                column(width = 5,
                       plotOutput(outputId = "hist")
                )
              ),
              # line space
              fluidRow(
                column(width = 2,
                       height = 3

                )
              ),

              fluidRow(
                column(width = 5,
                       uiOutput(outputId = "race")
                ),
                column(width = 6,
                       uiOutput(outputId = "economics")
                ),

              )

      )
    )
  )
)
