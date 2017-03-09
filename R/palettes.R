# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Paul Tol's qualitative color palettes for up to 21 discrete colors.
#' See https://personal.sron.nl/~pault/.
#' @param n the number of colors
#' @export
tol_palette <- function(n) {

  if (n > 21) {
    stop("max number of colors = 21")
  } else if (n == 13) {
    .n <- 14
  } else if (n <= 21 & n > 14) {
    .n <- 21
  } else if (n < 1) {
    stop("n must be at least 1")
  } else {
    .n <- n
  }

  palettes <- list(
    "1"=c("#4477AA"),
    "2"=c("#4477AA", "#CC6677"),
    "3"=c("#4477AA", "#DDCC77", "#CC6677"),
    "4"=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
    "5"=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
    "6"=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
    "7"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
    "8"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
    "9"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
    "10"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
    "11"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
    "12"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),
    "14"=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"),
    "21"=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
  )

  palettes[[as.character(.n)]]

}

mono_palette <- function(n, color=c("red", "green", "blue", "grey")) {
  if (n > 8) stop("Max number of levels is 8")
  else if (n < 1) stop("Must give at least one level")

  color <- match.arg(color)

  if (color == "grey") {
    color <- ifelse(n <= 6, "grey6", "grey8")
  }

  palettes <- list(
    red = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0"),
    green = c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5"),
    blue = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF"),
    grey8 = c("#000000","#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0"),
    grey6 = c("#242424", "#494949", "#6D6D6D", "#929292", "#B6B6B6", "#DBDBDB")
  )

  palettes[[color]]
}

#' Adds/sets a specific level color, i.e. to make an "Other" level a specific
#' color outside the palette.
#' @param palette the named vector representing the palette
#' @param level level name (if missing, will be added to end)
#' @param color color to assign to specified level
#' @export
color_level <- function(palette, level="Other", color="grey85") {
  palette[[level]] <- color
  return(palette)
}

#' Creates a named vector that can be used as in scale_*_manual
#' @param levels the levels of a palette
#' @param colors the colors that go to each level. Must be at least as long as `levels`
#' @export
named_palette <- function(levels, colors) {
  stopifnot(length(levels) <= length(colors))
  pal <- colors[1:length(levels)]
  names(pal) <- levels
  return(pal)
}

