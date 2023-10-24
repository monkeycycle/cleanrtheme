#' @importFrom grDevices pdfFonts
.onAttach <- function(libname, pkgname) {
  ## Load all fonts
  extrafont::loadfonts(quiet = TRUE)
  if (.Platform$OS.type == "windows") {
    windowsFonts <- grDevices::windowsFonts
    extrafont::loadfonts("win", quiet = TRUE)
  }
}

setupFont <- function() {
  # Loadfonts
  if (!"Fira Sans" %in% extrafont::fonts()) {
    extrafont::ttf_import(paths = system.file("font", package = "firatheme"))
  }

  ## Load all fonts
  extrafont::loadfonts("pdf", quiet = TRUE)
  extrafont::loadfonts("postscript", quiet = TRUE)
  if (.Platform$OS.type == "windows") {
    extrafont::loadfonts("win", quiet = TRUE)
  }
}

fontsReady <- function() {
  if (.Platform$OS.type == "windows") {
    if ("Fira Sans" %in% grDevices::windowsFonts()) return(TRUE)
  } else {
    if ("Fira Sans" %in% extrafont::fonts()) return(TRUE)
  }
  return(FALSE)
}

setupGhostScript <- function() {
  # Setup ghostscript for pdf output
  if (.Platform$OS.type == "windows") {
    binpath <- "bin/gswin64c.exe"
    basepath <- "C:/Program Files/gs"
    gsdirs <- list.dirs(basepath, recursive = FALSE)
    matches <- regexpr("[0-9].[0-9]*$", gsdirs)
    potentials <- matches > 0
    nmatches <- sum(potentials)

    if (nmatches == 1) {
      # only one version is installed
      Sys.setenv(R_GSCMD = file.path(gsdirs[potentials], binpath))
    } else if (nmatches > 1) {
      # find newest version
      versions <- numeric(nmatches)
      for (i in 1:nmatches) {
        m <- matches[potentials][i]
        mlen <- attr(matches, "match.length")[potentials][i]
        gsdir <- gsdirs[potentials][i]
        versions[i] <- as.numeric(gsub("\\.", "", substr(gsdir, m, m + mlen)))
      }
      Sys.setenv(R_GSCMD = file.path(gsdirs[potentials][which.max(versions)],
                                     binpath))
    } else {
      stop("64-bit GhostScript could not be found. Install 64-bit GhostScript",
           " or run the following with the _correct_ location",
           " to the installed GhostScript Binary:\n Sys.setenv(R_GSCMD =",
           ' "C:/Program Files/gs/gs<version.number>/bin/gswin64c.exe")')
    }
  }
}


################################################################################
# https://github.com/bbc/bbplot/
################################################################################

save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

#Left align text
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

create_footer <- function (source_name) {
  #Make the footer
  footer <- grid::grobTree(
                           grid::textGrob(source_name,
                                          x = 0.015, hjust = 0, gp = grid::gpar(fontfamily="Avenir", fontsize=10))
                           )
  return(footer)

}

#' Arrange alignment and save BBC ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication for a BBC News graphic.
#' It will left align your title, subtitle and source, add the BBC blocks at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#'  which needs to be a PNG file - defaults to BBC blocks image that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalize_plot
#' @examples
#' finalize_plot(plot_name = myplot,
#' source = "The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width_pixels = 640,
#' height_pixels = 450
#' )
#'
#' @export
finalize_plot <- function(plot_name,
                          source_name,
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450) {

  footer <- create_footer(source_name)

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("title", "subtitle", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}

