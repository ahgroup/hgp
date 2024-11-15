###
# Code to prepare the strain names data for package inclusion
# Zane Billings
# 2024-08-12
# The strain names data is a table of strain names, currently the ones that
# are used in UGAFluVac. We encourage updates to raw CSV file to add strain
# names that are used in other handelgroup datasets.
###

handelgroup_strain_names <- readr::read_csv(
	here::here("data-raw", "handelgroup-strain-names.csv"),
	col_types = 'fffcccil'
) |>
	# Remove the useless columns
	dplyr::select(-c(vaccine_strain)) |>
	# Append a row so sorting the overall entry for CATEs is easy
	tibble::add_row(
		strain_type = "",
		strain_subtype = "",
		strain_subtype_short = "",
		analysis_name = "Overall",
		short_name = "Overall",
		genbank_strain_name = "Overall",
		factor_order = 9999L
	) |>
	# Make all of the name variables ordered factors and clean up the subtypes
	dplyr::mutate(
		# Put the different name factors in order
		dplyr::across(
			c(analysis_name, genbank_strain_name, short_name),
			\(x) forcats::fct_reorder(x, factor_order)
		),
	)

usethis::use_data(handelgroup_strain_names, overwrite = TRUE)
