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




