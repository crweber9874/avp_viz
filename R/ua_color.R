#' UA Color Scheme
#'
#' @param levels {"azblue", "azred", "oasis", "grey", "warmgrey", "midnight", "azurite", "chili", "white"}
#' @return A dataframe with aggregated voter and census data:
#'
#' @export


az_color <- function(color = c("azblue", "azred", "oasis", "grey", "warmgrey", "midnight", "azurite", "chili", "white")) {
  if(color == "azblue") return("#0C234B")
  if(color == "azred")  return("#AB0520")
  if(color == "oasis")  return("#378DBD")
  if(color == "grey")   return("#E2E9EB")
  if(color == "warmgrey") return("#F4EDE5")
  if(color == "midnight") return("#001C48")
  if(color == "azurite") return("#1E5288")
  if(color == "chili") return("#8B0015")
  if(color == "white") return("#FFFFFF")

}
