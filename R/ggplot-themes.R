#' Basic plot theme for manuscript
#'
#' @param base_size base font size
#' @param strip_text_size size of text that goes over facets
#' @param axis_title_size size of axis label text
#' @param axis_text_size size of axis tick mark text
#' @param title_text_size size of plot title text
#' @param legend_text_size size of text in legend
#' @param font_family font to use in plot text
#' @param ... Other arguments passed to \code{theme_minimal}.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'     geom_point() +
#'     theme_ms()
#' }
#'
#' \dontrun{
#' ggplot2::theme_set(hgp::theme_ms())
#' }
#'
#' @export
theme_ms <- function(base_size = 12,
                     strip_text_size = 14,
                     axis_title_size = 14,
                     axis_text_size = 12,
                     title_text_size = 16,
                     legend_text_size = 14,
                     font_family = "",
                     ...) {
  ret <- ggplot2::theme_minimal(
    base_family = font_family,
    base_size = base_size,
    ...
  )

  ret$strip.text <- ggplot2::element_text(
    size = strip_text_size,
    color = "black",
    face = "bold"
  )

  ret$axis.title <- ggplot2::element_text(
    size = axis_title_size,
    color = "black"
  )

  ret$axis.text <- ggplot2::element_text(
    size = axis_text_size,
    color = "black"
  )

  ret$ plot.title <- ggplot2::element_text(
    hjust = 0.5,
    size = title_text_size,
    face = "bold",
    color = "black"
  )

  ret$legend.position <- "bottom"

  ret$legend.justification <- "center"

  ret$legend.text <- ggplot2::element_text(
    size = legend_text_size,
    color = "black"
  )

  return(ret)
}
