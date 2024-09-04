library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(plotly)


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = paste("Voting in Arizona")),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Legislative Districts", tabName = "legislative_districts", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '))),
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
                column(width = 12,
                       column(width = 8,
                              uiOutput(outputId = "ld")
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
              We created three scores:<sup>1</sup>
            </p>
            <ul>
              <li> Predicted Vote in 2024: The district averaged predicted vote in the 2024 election.</li>
              <li> General Voting Score: The district averaged score based on general election voting in the 2016, 2018, 2020, and 2022 elections.</li>
              <li> Primary Voting Score: The district averaged score based on general election voting in the 2016, 2018, 2020, and 2022 elections.</li>
            </ul>
            <p>
              <b>Drag the marker, or click on a region to examine participation rates across legislative district.</b>"
                  )
                )
              ),
              fluidRow( # Row for selectInput and race table
                column(width = 6,
                       selectInput("variable", "Choose variable to plot:",
                                   choices = c("Predicted Vote in 2024" = "voterPrediction",
                                               "General Voting Score" = "generalVoterScore",
                                               "Primary Voting Score" = "primaryVoterScore"
                                   )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       hr()
                )),

              fluidRow( # Row for map and histogram
                column(width = 6,
                       leafletOutput(outputId = "map", height = "400px")
                ),
                column(width = 6,
                       height = 4,
                       plotOutput(outputId = "hist")
                )),
              fluidRow(
                column(width = 6,
                       height = 5,
                       uiOutput(outputId = "registration")),
                column(width = 6,
                       uiOutput(outputId = "economics")
                )
              ),
              fluidRow(
                column(width = 12,
                       hr()
                )),

              fluidRow(
                column(width = 6,
                       height = 6,
                       plotlyOutput(outputId = "pie")
                ),
                column(width = 6,
                       uiOutput(outputId = "race"))
              ),

              fluidRow(
                includeCSS("style.css"),
                column(
                  width = 12,
                  div(
                    class = "info-card", # Apply the CSS class for styling
                    HTML(
                      "<p>
                <b>Notes</b>
                <br>
                <sup>1</sup> Scores are generated from several statistical models. The prediction for 2024 is based on a neural network.
                The latent variable scores are generated from a factor analysis model.
                <br>

              </p>
              "
                    )
                  )
                )
              )
      ), # <-- Comma added here

      tabItem(tabName = "about",
              column(
                width = 10,
                div(
                  class = "info-card", # Apply the CSS class for styling
                  HTML(
                    "<p>
              <h3>About these Estimates</h3>
              <p> Voting estimates come from February 2023 registration data reported by the Arizona Secretary of State's Office. These data consist of 4,100,688 active records.
              These data include registration, encoded as a Democrat, Republican, or Independent. The data also include voting -- whether one voted, not for whome one voted -- in 2016, 2018, 2020, and 2022
              primary and general elections. From these data, we build a number of statistical models to understand voting rates throughout the state. No voter information is reported in our dashboards. Rather,
              we report statistical averages for every legislative district in the state.<br>

              <p>The model we use to predict 2024 general election participation comes from a machine learning model known as a neural network; in particular, a recurrent neural network. Voters are represented as
              a sequence built from on prior participation. For instance, suppose a person participated in the 2016 general election, didn't participate in 2018, and then participated
              in 2020 and 2022. They would have a sequence of 1, 0, 1, 1. This sequence can in turn be used to predict the 2024 general election participation for active voters. The dashboard
              reports the average predicted vote for each legislative district. <br>

              <p>The latent variable scores are generated from a factor analysis model. As an alternative to a 2024 prediction, we construct a score for each voter, based on their participation
              in every primary and general election since 2016. We then estimate two confirmatory factor models, one for general election participation and one for the primary election participation.
              We again report averages for each legislative district. <br>

              <p>The dashboard also includes data reported by the United States Census.
              </p>"
                  )
                )
              )
      )


    )
  )
)
