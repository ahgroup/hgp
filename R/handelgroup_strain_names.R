#' Handelgroup Standardized Strain Names Dataset
#'
#' A dataset containing various formats of the names for the influenza strains
#' we use in our research.
#'
#' @format ## `handelgroup_strain_names`
#' A data frame with 46 rows and 6 columns:
#' \describe{
#'   \item{strain_type}{Whether the strain is type A or B.}
#'   \item{strain_subtype}{For type A strains, whether the strain is of the
#'   H3N2 or H1N1 subtype. FOr type B strains, whether the strain is of the
#'   Victoria lineage, the Yamagata lineage, or Presplit (predating the
#'   divergence between the lineages).}
#'   \item{strain_subtype_short}{The shorter version of the strain_subtype
#'   which is occasionally used in the analysis code: h1, h3, byam, bvic,
#'   or bpre.}
#'   \item{subtype}{Whether the strain is H1N1 or H3N2.}
#'   \item{analysis_name}{Strain name format used in clean_data.Rds.}
#'   \item{genbank_strain_name}{The accepted full strain name, as found in
#'   genbank.}
#'   \item{short_name}{The abbrevated name, usually 2-4 letters and the
#'   last two digits of the year, useful for saving space in displays.}
#' }
"handelgroup_strain_names"
