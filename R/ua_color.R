#' UA Color Scheme
#'
#' @param levels {"azblue", "azred", "oasis", "grey", "warmgrey", "midnight", "azurite", "chili", "white"}
#' @return A dataframe with aggregated voter and census data:
#'
#' @export


az_color <- function(color = c("azblue", "azred", "oasis", "grey", "warmgrey", "midnight", "azurite", "chili", "white")) {
  if(color == "azblue") "#0C234B"
  if(color == "azred") "#AB0520"
  if(color == "oasis") "#378DBD"
  if(color == "grey") "#E2E9EB"
  if(color == "warmgrey") "#F4EDE5"
  if(color == "midnight") "#001C48"
  if(color == "azurite") "#1E5288"
  if(color == "chili") "#8B0015"
  if(color == "white") "#FFFFFF"
}

cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}
