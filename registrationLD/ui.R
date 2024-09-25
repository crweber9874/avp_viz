library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .btn-3d {
        background-color: #007bff;
        border: none;
        color: white;
        padding: 10px 20px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin: 4px 2px;
        cursor: pointer;
        box-shadow: 0 4px #999;
      }
      .btn-3d:active {
        box-shadow: 0 2px #666;
        transform: translateY(2px);
      }
    "))
  ),
  fluidRow(
    column(
      width = 8,
      height = 1,
      uiOutput(outputId = "clicked_ld_output")
    )),


  fluidRow(
    column(width = 4,
           selectInput("variable", "Color the map below by:",
                       choices = c("Registered Independent" = "percentIndependent",
                                   "Registered Republican" = "percentRepublican",
                                   "Registered Democrat" = "percentDemocrat")
           )
    )
  ),
  fluidRow(
    column(width = 6,
           height = 6,
           leafletOutput(outputId = "map")),
    column(
      width = 5,
      height = 5,
      uiOutput(outputId = "types")
    )),
  fluidRow(
    column(width = 2,
           actionButton("reset_map", "Reset Map", class = "btn-3d")
    )),
  # fluidRow(
  #   column(width = 4, offset = 1,
  #          textInput("address", "Enter Address:", value = ""),
  #          actionButton("geocode", "Address Search", class = "btn-3d")
  #   ),
  #   column(width = 4,
  #          uiOutput(outputId = "geo_cd")
  #   ),
  #

  br(),  # Add space
  br(),  # Add space

  fluidRow(
    column(
      width = 4,
      height = 5        ,
      plotlyOutput(outputId = "repHist")
    ),
    column(
      width = 4,
      height = 5        ,
      plotlyOutput(outputId = "demHist")
    ),
    column(
      width = 4,
      height = 5        ,
      plotlyOutput(outputId = "indHist")
    )
  )
)

