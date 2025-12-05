#' geom_braket: draw a bracket between two x positions with a label
#'
#' Draws a horizontal bracket between `x` and `xend` at height `y`, with a label centered above it.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data
#' @param position Position adjustment
#' @param inherit.aes Whether to inherit aesthetics from ggplot default
#' @param na.rm Remove missing values?
#' @param show.legend Should this layer be included in legends?
#' @param ... Other arguments passed on to methods
#'
#' @return A ggplot2 layer
#' 
#' @export
#'
#' @importFrom backports "%||%"
#' @import grid
#' 
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1, xend = 3, y = 5, label = 'p < 0.05')
#' ggplot() +
#'   geom_point(aes(1:5, 1:5)) +
#'   geom_braket(data = df, aes(x = x, xend = xend, y = y, label = label)) +
#'   ylim(0, 6)

geom_braket <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ..., 
                        inherit.aes = TRUE, na.rm = FALSE,
                        show.legend = NA) {
  layer(
    geom = GeomBraket, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomBraket <- ggproto("GeomBraket", Geom,
  required_aes = c("x", "xend", "y"),

  default_aes = aes(
    label = NA_character,
    colour = from_theme(colour %||% ink),
    family = from_theme(family),
    fontsize = from_theme(fontsize),
    linewidth = from_theme(linewidth),
    linetype = from_theme(linetype),
    tip_length = I(0.01),
    alpha = NA
  ),

  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    y_range <- panel_params$y.range
    
    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      with(coords[i, ], {
        x0 <- min(x, xend)
        x1 <- max(x, xend)
        y <- y
        y_start <- y - tip_length

        lines <- segmentsGrob(
          x0 = c(x, x, xend),
          x1 = c(x, xend, xend),
          y0 = c(y_start, y, y),
          y1 = c(y, y, y_start),
          gp = gpar(col = colour, lwd = linewidth, lty = linetype)
        )

        if (!is.na(label)) {
          label_grob <- textGrob(
            label = label,
            x = (x + xend)/2,
            y = y,
            just = c(0.5,-0.5),
            gp = gpar(col = colour, fontsize = fontsize*3)
          )
          return(grobTree(lines, label_grob))
        } else {
          return(lines)
        }
    })
    })

    grobTree(children = do.call(gList, grobs))
  }
)
