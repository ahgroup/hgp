#' Basic plot theme for manuscript
#'
#' @param base_size base font size
#' @param text_size_strip size of text that goes over facets
#' @param text_size_axis_text size of axis tick text
#' @param text_size_axis_title size of axis label/title text
#' @param text_size_title size of plot title text
#' @param text_size_subtitle size of plot subtitle text
#' @param text_size_legend size of legend text
#' @param text_size_legend_title size of legend title text
#' @param text_size_caption size of plot caption text
#' @param font_family font to use in plot text, defaults to ggplot's base font
#' @param legend_position where the legend should go, bottom by default.
#' accepts any argument allowed by ggplot2.
#' @param panel_spacing_lines Amount of space in line units between faceted
#' panels.
#' @param margin_t Margin space above top of plot.
#' @param margin_r Margin space to the right of plot.
#' @param margin_b Margin space underneath the of plot.
#' @param margin_l Margin space to the left of plot.
#' @param ... Other arguments passed to \code{theme_minimal()}.
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
#' ggplot2::theme_set(zlib::theme_ms())
#' }
#'
#' @export
theme_ms <- function(base_size = 12,
										 text_size_strip = 16,
										 text_size_axis_text = 12,
										 text_size_axis_title = 18,
										 text_size_title = 20,
										 text_size_subtitle = 16,
										 text_size_legend = 16,
										 text_size_legend_title = 18,
										 text_size_caption = 14,
										 font_family = "",
										 legend_position = "bottom",
										 panel_spacing_lines = 2,
										 margin_t = 6,
										 margin_r = 6,
										 margin_b = 6,
										 margin_l = 6,
										 ...) {

	# Make sure ggplot2 is installed
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop(
			"Package \"ggplot2\" must be installed to use this function.",
			call. = FALSE
		)
	}

	# Use theme_minimal() as a default
	ret <- ggplot2::theme_minimal(
		base_family = font_family,
		base_size = base_size,
		...
	)

	# Everything that follows here is an update to a specific theme aspect. You
	# can customize any element listed in the documentation of `ggplot2::theme()`

	# WHITE background to prevent the annoying darkmode thingy
	ret$plot.background <- ggplot2::element_rect(
		fill = "white", color = "white"
	)

	ret$axis.text <- ggplot2::element_text(
		size = text_size_axis_text,
		color = "black"
	)

	ret$axis.title <- ggplot2::element_text(
		size = text_size_axis_title,
		color = "black"
	)

	ret$strip.text <- ggplot2::element_text(
		size = text_size_strip,
		color = "black",
		face = "bold"
	)

	ret$plot.subtitle <- ggplot2::element_text(
		size = text_size_subtitle,
		hjust = 0,
		margin = ggplot2::margin(b = 2)
	)

	ret$plot.title <- ggplot2::element_text(
		size = text_size_title,
		hjust = 0,
		margin = ggplot2::margin(b = 2, l = 2)
	)

	ret$plot.caption <- ggplot2::element_text(
		size = text_size_caption
	)

	ret$strip.text = ggplot2::element_text(
		size = 16, hjust = 0.5, margin = ggplot2::margin(b = 2, t = 2)
	)

	ret$panel.spacing <- ggplot2::unit(panel_spacing_lines, "lines")

	ret$legend.text <- ggplot2::element_text(
		size = 16,
		color = "black"
	)
	ret$legend.title = ggplot2::element_text(
		size = text_size_legend_title,
		color = "black"
	)

	ret$legend.position <- legend_position

	ret$legend.justification <- "center"

	ret$plot.margin <- ggplot2::margin(
		t = margin_t,
		r = margin_r,
		b = margin_b,
		l = margin_l
	)

	return(ret)
}
