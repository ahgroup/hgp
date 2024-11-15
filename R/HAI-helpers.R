####
# Functions for dealing with HAIs and censoring
# Zane Billings
# 2024-08-23
# We need to put the HAI outcomes into the correct censoring format before we
# can pass them to a stan model. This code contains functions for putting either
# titer outcomes or titer increase into the correct censoring format.
####

#' hai_to_log_scale()
#' Convert an HAI titer to log scale using the log2(x / 5) transform.
#'
#' @param x the natural scale titer
#'
#' @return log2(x / 5)
#' @export
#'
hai_to_log_scale <- function(x) {
	return(log2(x / 5))
}

#' hai_to
#' Convert a logged HAI titer back to natural scale using 5 * 2 ^ x transform.
#'
#' @param x the log scale titer vector
#'
#' @return 5 * 2 ^ x
#' @export
#'
hai_to_natural_scale <- function(x) {
	return(5 * 2 ^ x)
}

#' transform HAI titers
#' Function to convert between log and natural scale for HAI titers.
#'
#' @param x HAI titers on either the log or natural scale.
#' @param dir either 'log' if the titers should be transformed to the log scale
#' or 'natural' if the titers should be transformed to the natural scale.
#' Partial matching is supported.
#'
#' @return The transformed titer values, a numeric vector
#' @export
#'
hai_trans <- function(x, dir = c("log", "natural")) {
	if (startsWith(tolower(dir)) == "l") {
		return(hai_to_log_scale(x))
	} else if (startsWith(tolower(dir)) == "n") {
		return(hai_to_natural_scale(x))
	} else {
		stop("'dir' should be either 'log' or 'natural'.")
	}
}


#' Convert HAI data to brms censoring format
#' Main function that takes a post titer data column as a string
#' (and optionally a pretiter column if you want the outcome to be titer
#' increase), calculates the censoring bounds, and then converts to the
#' correct brms format.
#'
#' @param data A data frame
#' @param post_titer A string, the name of the column in `data` containing the
#' post-vaccination titer values.
#' @param pre_titer A string, the name of the column in `data` containing
#' the pre-vaccination titer values. Optional, only required if titer increase
#' should be calculated.
#' @param log_scale Boolean, TRUE if the titers are already on the log scale
#' and FALSE if the titer columns are on the natural scale. Defaults to TRUE.
#' Currently the post_titer and pre_titer columns must both be on the same
#' scale.
#' @param increase Boolean, TRUE if the outcome should be titer increase, and
#' FALSE if the outcome should be post-vaccination titer. Defaults to FALSE.
#' @param log_out Boolean, TRUE if the outcomes should be returned on the
#' log scale, FALSE if the outcomes should be returned on the natural scale.
#' Defaults to the scale of the input variables.
#'
#' @return A data frame with three additional columns named y, y2, and c. Note
#' that if you already have these columns in the data frame, they will be
#' overwritten in the result and you might get unexpected behavior.
#' @export
#'
format_hai_data <- function(data, post_titer, pre_titer = NULL,
														log_scale = TRUE, increase = FALSE,
														log_out = log_scale) {
	# If post_titer is named "y" we'll get a conflict!
	if (isTRUE(post_titer == "y")) {
		stop(paste0(
			"If the post_titer argument is set to 'y' you'll get a ",
			"column named conflict with the brms format!"
		))
	}

	# First deal with the posttiter and make sure it is on the log scale
	if (isTRUE(log_scale)) {
		log_post <- data[[post_titer]]
		post <- 5 * 2 ^ log_post
	} else if (isFALSE(log_scale)) {
		post <- data[[post_titer]]
		log_post <- log2(post / 5)
	} else {
		stop("'log scale' argument should be TRUE or FALSE.")
	}

	# Now deal with the pretiter -- everything is the same, but we need to check
	# whether it's NULL, which is OK if increase = FALSE.
	if (is.null(pre_titer)) {
		# Error if pre_titer isn't specified and increase = TRUE
		if (isTRUE(increase)) {
			stop("If 'increase = TRUE', you must specify the pre-titer column.")
		}
	} else {
		# Make sure pre titer is on the log scale, we don't need to validate that
		# argument a second time though.
		if (isTRUE(log_scale)) {
			log_pre <- data[[pre_titer]]
			pre <- 5 * 2 ^ log_pre
		} else {
			pre <- data[[pre_titer]]
			log_pre <- log2(pre / 5)
		}
	}

	# The parameter "increase" should be TRUE if the data represents titer
	# increases, or false if the data represents actual titers.
	# The two censoring schemes have to be handled differently.
	# First we handle the case of raw HAI titer.
	if (isFALSE(increase)) {
		out <- format_posttiter(log_post, log_out)
	} else if (isTRUE(increase)) {
		out <- format_titerincrease(log_post, log_pre, log_out)
	} else {
		stop(paste0(
			"'increase' should be TRUE if the outcome value is titer increase or ",
			"FALSE if the outcome is raw titer values."
		))
	}

	# Add the new columns back to the original data set
	out_data <- dplyr::bind_cols(
		data,
		out
	)

	return(out_data)
}

#' Do the calculation and formatting for titer outcomes.
#' Internal helper used for censoring formatting that you shouldn't use
#' unless you know what you are doing.
#'
#' @param log_post log-scale vector of post-titers.
#' @param log_out Boolean, TRUE if the output should be on the log scale.
#'
#' @return formatted post-titer values.
#'
format_posttiter <- function(log_post, log_out) {
	# Fix "no visible binding" R CMD CHK note
	lwr <- upr <- y <- y2 <- NULL

	# Construct the base table of censoring limits
	out <- tibble::tibble(
		lwr = log_post,
		upr = log_post + 1
	)

	# Transform to brms format
	out <-
		out |>
		dplyr::mutate(
			c = dplyr::if_else(lwr < 1, -1, 2),
			y = ifelse(c == -1, 1, lwr),
			y2 = ifelse(c == -1, 1, upr)
		) |>
		dplyr::select(-upr, -lwr)

	# If we need to return on the log scale, do that here
	if (isFALSE(log_out)) {
		out <- dplyr::mutate(
			out,
			dplyr::across(c(y, y2), hai_to_natural_scale)
		)
	}

	return(out)
}

#' TI censoring bounds calculation
#' Internal helper for censoring transformation that you shouldn't use unless
#' you know what you are doing. Based on the table in the supplement of
#' Yang's censoring paper: https://doi.org/10.1101/2022.08.25.22279230
#'
#' @param y Vector of log-scale post-vacciation titer values
#' @param x Vector of log-scale pre-vaccination titer values.
#'
#' @return A tibble containining three numeric columns: `z`, the log-scale
#' titer increase, `z_l`, the lower bound of `z`, and `z_u`, the upper
#' bound of `z`.
#'
calculate_ti_bounds <- function(y, x) {
	ly <- ifelse(y == 0, -Inf, y)
	lx <- ifelse(x == 0, -Inf, x)
	uy <- ifelse(y == 0, 1, y + 1)
	ux <- ifelse(x == 0, 1, x + 1)

	val_set <- list(ly-lx, ly-ux, uy-lx, uy-ux, na.rm = TRUE)
	bounds <- tibble::tibble(
		z = y - x,
		z_l = do.call(pmin, val_set),
		z_u = do.call(pmax, val_set)
	)
	return(bounds)
}

#' Calculation of censoring bounds for TI outcome
#' Internal helper for censoring data transformation that you should't use
#' unless you know what you are doing.
#'
#' @param log_post Vector of log-scale post-vaccination titer values.
#' @param log_pre Vector of log-scale pre-vaccination titer values.
#' @param log_out Boolean, TRUE if the outcome should be on the log scale.
#'
#' @return A data frame containing the formatted `c`, `y`, and `y2` numeric
#' vectors representing the titer increase in brms censoring notation.
#'
format_titerincrease <- function(log_post, log_pre, log_out) {
	# Fix 'no visible binding' R CMD CHK note
	z <- z_l <- z_u <- y <- y2 <- NULL

	# First calculate the censoring bounds for each observation
	censoring_bounds <- calculate_ti_bounds(log_post, log_pre)

	# Now format the data in brms format
	out <-
		censoring_bounds |>
		dplyr::mutate(
			c = dplyr::case_when(
				# can't go to stan, gotta be dropped
				is.infinite(z_l) & is.infinite(z_u) ~ NA_integer_,
				is.finite(z_l) & is.infinite(z_u) ~ 1, # right censored
				is.infinite(z_l) & is.finite(z_u) ~ -1, # left censored
				is.finite(z_l) & is.finite(z_u) ~ 2 # interval censored
			),
			y = dplyr::case_when(
				c ==  2 ~ z_l,
				c == -1 ~ z_u,
				c ==  1 ~ z_l
			),
			y2 = dplyr::case_when(
				c ==  2 ~ z_u,
				c == -1 ~ z_u,
				c ==  1 ~ z_l
			)
		) |>
		dplyr::select(-z, -z_l, -z_u)

	#	If we need to return on the log scale, do that here
	if (isFALSE(log_out)) {
		out <- dplyr::mutate(
			out,
			dplyr::across(c(y, y2), \(x) 2 ^ x)
		)
	}

	return(out)
}

# END OF FILE ####
