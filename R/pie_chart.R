#' Create a Pie Chart
#'
#' @param data A data frame containing the data to plot.
#' @return A plotly pie chart.
#' @export
#' @examples
#' ld <- read.csv("/Users/Chris/Dropbox/github_repos/avp-vote-models/avp-vote-models/tmp/preML.csv")
#' test <- data_pie(ld, c("republican_registration", "democratic_registration", "independent_registration"))
#' pie_chart(test)
pie_chart <- function(data) {
  plotly::plot_ly(data,
                  labels = ~labels,  # Use column names directly
                  values = ~values,
                  type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#F4F4F4'),
                  hoverinfo = 'text',
                  text = ~paste0(round(values, 2) * 100, '% of voters'),
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
      text = "",
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
