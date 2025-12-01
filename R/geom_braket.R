#' geom_braket: draw a bracket between two x positions with a label
#'
#' Draws a horizontal bracket between `x` and `xend` at height `y`, with a label centered above it.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data
#' @param position Position adjustment
#' @param tip_length Vertical extent of bracket tips (default: `unit(0.02, "npc")`)
#' @param inherit.aes Whether to inherit aesthetics from ggplot default
#' @param na.rm Remove missing values?
#' @param show.legend Should this layer be included in legends?
#' @param ... Other arguments passed on to methods
#'
#' @return A ggplot2 layer
#' @export
#' @import ggplot2 grid
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1, xend = 3, y = 5, label = '*p* < 0.05')
#' ggplot() +
#'   geom_point(aes(1:5, 1:5)) +
#'   geom_braket(data = df, aes(x = x, xend = xend, y = y, label = label)) +
#'   ylim(0, 6)

geom_braket <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ..., tip_length = unit(0.02, "npc"),
                        inherit.aes = TRUE, na.rm = FALSE,
                        show.legend = NA) {
  layer(
    geom = GeomBraket, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(tip_length = tip_length, na.rm = na.rm, ...)
  )
}

GeomBraket <- ggproto("GeomBraket", Geom,
  required_aes = c("x", "xend", "y"),

  default_aes = aes(label = NULL, colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_panel = function(data, panel_params, coord, tip_length) {
    coords <- coord$transform(data, panel_params)
    y_range <- panel_params$y.range

    tip_height <- if (is.unit(tip_length) && attr(tip_length, "unit") == "npc") {
      diff(y_range) * as.numeric(tip_length)
    } else if (is.unit(tip_length)) {
      convertUnit(tip_length, "native", valueOnly = TRUE)
    } else {
      tip_length
    }

    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      row <- coords[i, ]
      x0 <- min(row$x, row$xend)
      x1 <- max(row$x, row$xend)
      y <- row$y
      y_top <- y + tip_height

      lines <- segmentsGrob(
        x0 = unit(c(x0, x0, x1), "native"),
        x1 = unit(c(x0, x1, x1), "native"),
        y0 = unit(c(y, y_top, y_top), "native"),
        y1 = unit(c(y_top, y_top, y), "native"),
        gp = gpar(col = row$colour, lwd = row$size * .pt, lty = row$linetype)
      )

      if (!is.na(row$label)) {
        fontsize <- if (!is.na(row$size)) row$size * 5 else 5
        label_grob <- textGrob(
          label = row$label,
          x = unit((x0 + x1)/2, "native"),
          y = unit(y_top, "native") + unit(1, "mm"),
          just = "bottom",
          gp = gpar(col = row$colour, fontsize = fontsize)
        )
        return(grobTree(lines, label_grob))
      } else {
        return(lines)
      }
    })

    grobTree(children = do.call(gList, grobs))
  }
)
