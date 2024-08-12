#' Replace strain names with a different format
#'
#' Using the standardized list of handelgroup strain names from
#' ["handelgroup_strain_names"], pass in a vector of strain names of arbitrary
#' length and return the same sequence of names in a different format.
#'
#' @param x A vector of strain names.
#' @param from Format of the names in the vector `x`. Should be one of
#' "analysis", "full", or "short". See ["handelgroup_strain_names"] for the
#' allowed names in each of the formats. If you want to transform a strain
#' that is not currently in the strain list, you will need to add it and
#' submit a PR to `hgp`!
#' @param to Format of the returned names. Should be one of "short", "full",
#' "analysis", or "subtype".
#' @param drop If TRUE, levels of the returned factor variable are dropped. If
#' FALSE, the level set of the factor will still contain every strain in
#' ["handelgroup_strain_names"], which is typically not desirable.
#'
#' @return A factor vector of the same length as `x`.
#' @export
#'
#' @examples
#' replace_strain_names("CA/09", from = "short", to = "analysis")
#'
#' dat <- data.frame(s = c("CA/09", "MI/15"), x = c(1, 2))
#' transform(
#'     dat,
#'     s_long = replace_strain_names(s, from = "short", to = "analysis")
#' )
replace_strain_names <- function(x, from = "analysis", to = "short",
																 drop = TRUE) {
	# Load needed packages
	requireNamespace("forcats", quietly = TRUE)
	requireNamespace("tibble", quietly = TRUE)
	requireNamespace("dplyr", quietly = TRUE)
	requireNamespace("readr", quietly = TRUE)

	# Load the strain names data
	#utils::data("handelgroup_strain_names", envir=environment())
	handelgroup_strain_names <- hgp::handelgroup_strain_names

	# Check if from and to are the same
	if (from == to) {
		warning("From and to are the same, returning original vector.")
		return(x)
	}

	# Find the right column for selecting names from
	if (from == "analysis") {
		from_vec <- handelgroup_strain_names$analysis_name
	} else if (from == "full") {
		from_vec <- handelgroup_strain_names$genbank_strain_name
	} else if (from == "short") {
		from_vec <- handelgroup_strain_names$short_name
	} else {
		stop("'from' should be 'analysis', 'full', or 'short'.")
	}

	# Make sure all values of x exist in the virus info table
	if (!(all(x %in% from_vec))) {
		stop(paste0(
			"'x' should be a vector of ", from, " names that exist in the",
			' virus-info sheet.'
		))
	}

	# Now get the location in the virus info table for each element of x
	locs <- match(x, from_vec)

	# Based on the names argument, get the correct names to return.
	if (to == "analysis") {
		vals <- handelgroup_strain_names$analysis_name[locs]
	} else if (to == "full") {
		vals <- handelgroup_strain_names$genbank_strain_name[locs]
	} else if (to == "short") {
		vals <- handelgroup_strain_names$short_name[locs]
	} else if (to == "subtype") {
		vals <- handelgroup_strain_names$subtype[locs]
	} else {
		stop("'to' should be 'analysis', 'full', 'short', or 'subtype'.")
	}

	# If requested, remove unseen factor levels
	if (isTRUE(drop)) {
		vals <- forcats::fct_drop(vals)
	}

	return(vals)
}
