#' cleanr theme
#'
#' This theme uses Mozilla's Fira Sans as its font.
#' Save to pdf using \code{cleanrSave()}.
#'
#' @param family Change the font family. Defaults to Fira Sans
#'
#' @return ggplot theme
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = mpg*0.43, y = wt*0.4535924, colour = factor(cyl))) +
#'   geom_point(size = 2) +
#'   labs(title = "Car weight vs efficiency",
#'        subtitle = "Using sensible metrics",
#'        x = "Efficiency (km/l)",
#'        y = "Weight (1000 kg)",
#'        colour = "Cylinders") +
#'   theme_cleanr() +
#'   scale_colour_cleanr()
#'
#' @seealso \code{\link{cleanrSave}}
#'
#' @export
theme_cleanr <- function(type_family = "Avenir", type_size = 12) {

      # if (!fontsReady()) setupFont()
      ggplot2::`%+replace%`(
        ggplot2::theme_bw(base_size = type_size, base_family = type_family),
        ggplot2::theme(

          # Explicitly set panel and plot background to white to avoid transparency leaks in some graphics devices.
          panel.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
          panel.border = ggplot2::element_blank(),
          panel.spacing = grid::unit(rep(0, 4), "cm"),

          panel.grid.major = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),


          plot.background = ggplot2::element_rect(fill = "#ffffff", colour = "#ffffff"),
          plot.margin = ggplot2::margin(20,30,30,10),


          plot.title = ggplot2::element_text(size=18, face="bold", color="#222222", margin=ggplot2::margin(b=10)),
          plot.subtitle = ggplot2::element_text(size=14, margin=ggplot2::margin(b=15)),
          # This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
          plot.caption = ggplot2::element_blank(),


          axis.title = ggplot2::element_text(size = 12, colour = "#454545", face="bold"),
          axis.text = ggplot2::element_text(size = 10, colour = "#212121"),
          axis.ticks = ggplot2::element_line(color = "#454545", size = 0.3),
          axis.line = ggplot2::element_line(color = "#454545", size = 0.3),


          legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          legend.key = ggplot2::element_rect(fill = "transparent",colour = NA),
          legend.title = ggplot2::element_text(size = 11, colour = "#454545"),
          legend.text = ggplot2::element_text(size = 10, colour = "#454545"),


          # Facets
          strip.background = ggplot2::element_rect(fill = "#ffffff", colour = "#ffffff"),
          strip.text = ggplot2::element_text(size = 12, colour = "#454545", margin = ggplot2::margin(10, 10, 10, 10, "pt"))
        )
      )

}


#' Save plots that use the cleanr theme
#'
#' This function behaves like \code{ggsave} but automatically embeds the fira
#' font if the output format requires it. Install 64-bit GhostScript for this
#' functionality. Currently only works automatically on Windows. For other
#' platforms, run the following with the _correct_ location to the installed
#' GhostScript Binary: Sys.setenv(R_GSCMD = "bin/gs/gs9.23/binaryname")
#'
#' @param filename path to a file
#' @param device which type of output device to use
#' @param ... other arguments passed to ggsave
#'
#' @seealso \code{\link[ggplot2]{ggsave}}
#'
#' @export
cleanrSave <- function(filename = "plot.pdf", device = "pdf", ...) {
  needsFont <- device == "pdf" || device == "eps" || device == "ps"

  # set up ghostscript if needed
  if (Sys.getenv("R_GSCMD") == "" && needsFont) setupGhostScript()

  # save the image
  ggplot2::ggsave(filename = filename, device = device, ...)

  if (needsFont) extrafont::embed_fonts(filename)
}