#' cleanr theme palette
#'
#' This function outputs n colours from the cleanr ggplot2 theme palette
#'
#' @param n the number of colours to output
#'
#' @seealso \code{\link{cleanrColours}}
#'
#' @export
cleanrPalette <- function(n = 5) {
  if (n == 4) return(cleanrColours[c(1, 3, 4, 5)])
  grDevices::colorRampPalette(cleanrColours, space = "Lab")(n)
}

#' cleanr theme colours
#'
#' This is a vector with 6 colours to be used in palettes and other visual
#' elements.
#'
#' @seealso \code{\link{cleanrPalette}}
#'
#' @export
cleanrColours <- c("#413E38", "#5F787F", "#C9773C", "#81969D", "#CBA78D", "#2E3B3E")

cleanrColoursAlt <- c("#DBD9D5", "#DEE5E6", "#F4E4D8", "#E6EAEB", "#F5EDE8", "#D1DBDD")

cleanrColors <- cleanrColours
cleanrColorsAlt <- cleanrColoursAlt

#' cleanr discrete colour scales
#'
#' Colour scales belonging to the cleanr theme
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @param continuous whether the associated variable should be considered
#' continuous. Typically used after "Error: Continuous value supplied to
#' discrete scale"
#'
#' @seealso \code{\link{cleanrPalette}}
#'
#' @rdname scale_cleanr
#' @export
scale_fill_cleanr <- function(..., continuous = FALSE) {
  if (continuous) {
    pal <- grDevices::colorRampPalette(c(cleanrColours[1], cleanrColours[5]),
                                       space = "Lab")
    return(ggplot2::scale_fill_gradientn(..., colours = pal(256)))
  }
  ggplot2::discrete_scale("fill", paste0("cleanr"), cleanrPalette, ...)
}


#' @rdname scale_cleanr
#' @export
scale_colour_cleanr <- function(..., continuous = FALSE) {
  if (continuous) {
    pal <- grDevices::colorRampPalette(c(cleanrColours[1], cleanrColours[5]),
                                       space = "Lab")
    return(ggplot2::scale_colour_gradientn(..., colours = pal(256)))
  }
  ggplot2::discrete_scale("colour", paste0("cleanr"), cleanrPalette, ...)
}

#' @rdname scale_cleanr
#' @export
scale_color_cleanr <- scale_colour_cleanr


