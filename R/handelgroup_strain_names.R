#' Handelgroup Standardized Strain Names Dataset
#'
#' A dataset containing various formats of the names for the influenza strains
#' we use in our research.
#'
#' @format ## `handelgroup_strain_names`
#' A data frame with 46 rows and 6 columns:
#' \describe{
#'   \item{subtype}{Whether the strain is H1N1 or H3N2.}
#'   \item{analysis_name}{Strain name format used in clean_data.Rds.}
#'   \item{genbank_strain_name}{The accepted full strain name, as found in
#'   genbank.}
#'   \item{short_name}{The abbrevated name, usually 2-4 letters and the
#'   last two digits of the year, useful for saving space in displays.}
#' }
"handelgroup_strain_names"
