###
# Large text file (de)chunker
# Zane
# 2024-06-30
# Updated 2025-02-28
# Takes a large file (typically Rds, qs, or qs2) and splits it into multiple
# smaller files which are sufficiently small to be hosted on GitHub.
# Those smaller files can then be recombined into the original full file on
# the user-end and loadly correctly, preventing the need for a way to distribute
# large model files as one huge file.
# NOTE that GitHub repoistories must still be less than 5 GB in size, and they
# sometimes get mad if your repo is more than 2 or so GB. So this is only
# for model files of an intermediate size.
###


#' Extract directory from file path
#'
#' Utility function that, given a file path, returns the entire file path
#' without the final component that has a file extension. The point is to
#' get the full directory path that a file should live in.
#' @param file_path a character vector of file paths
#'
#' @returns a character vector of directorys in which the files would be saved.
#' @export
get_directory_path <- function(file_path) {
	# Check if the file path contains an extension
	if (!grepl("\\.[^\\\\/]+$", file_path)) {
		stop("Error: File name does not contain an extension.")
	}

	# Extract the directory path
	directory_path <- dirname(file_path)
	return(directory_path)
}


#' Detect operating system
#'
#' We need to run a shell command for the model (de)chunker, and the way we have
#' to do that is different or Windows vs unix-based OS. So this function will
#' detect the OS that an individual is using. If it isn't Windows or Unix, the
#' user will get an error and the (de)chunker won't work.
#'
#' @param verbose should a message be printed when the function runs?
#'
#' @returns `.Platform$OS.type`
#' @export
#'
#' @examples detect_os()
detect_os <- function(verbose = TRUE) {
	requireNamespace("rlang", quietly = TRUE)
	requireNamespace("crayon", quietly = TRUE)

	# This part does the entire OS detection and seems to be the most stable
	# or preferred way to do that.
	os_detected <- .Platform$OS.type

	# This part prints a nicely formatted message if the verbose argument is
	# set to TRUE.
	if (isTRUE(verbose)) {
		if (os_detected == "windows") {
			crayon::green("Detected Windows OS.") |> rlang::inform()
		} else if (os_detected == "unix") {
			crayon::yellow("Detected a unix OS.") |> rlang::inform()
		} else {
			rlang::abort(paste0(
				"OS detected isn't 'windows' or 'unix'!\nThis code isn't configured",
				" to work with your OS. Sorry!"
			))
		}
	}

	invisible(os_detected)
}

#' Run the appropriate bash-command invoker function by OS
#'
#' This function constructs a shell call that is appropriate for the system
#' OS. The system OS is passed as the first argument, so it should be detected
#' before calling this function or passed manually.
#' The remaining arguments are the arguments for the shell call, which are the
#' same regardless of OS for the purposes of the model (de)chunker.
#' We also implement error handling via purrr::possibly(). The "otherwise" value
#' in purrr possibly is set to -1 because this is the error code normally
#' returned by a shell() or system() invocation if a system-level error occurs.
#'
#' @param os_detected a string, usually the result of `.Platform$OS.type`.
#' @param ... the arguments to pass to either `shell()` or `system()`.
#'
#' @returns Whatever is returned by `shell()` or `system()`.
#' @export
os_call <- function(os_detected, ...) {
	requireNamespace("purrr", quietly = TRUE)
	# Construct the function call based on the OS
	if (os_detected == "windows") {
		FUN <- shell
	} else if (os_detected == "unix") {
		FUN <- system
	} else {
		rlang::abort(paste0(
			"OS detected isn't 'windows' or 'unix'!\nThis code isn't configured",
			" to work with your OS. Sorry!"
		))
	}

	# Add error handling to the call
	fun_possibly <- purrr::possibly(FUN, otherwise = -1)

	# Run the call
	out <- fun_possibly(...)

	invisible(out)
}

# This function is the model chunker.
#' Model Chunker
#'
#' The two arguments are both character vectors that should be equal length. It
#' takes each file specified in the vector "files_to_chunk", separates it into
#' smaller chunks with a fixed memory size, and saves all of the chunks to the
#' DIRECTORY PATH at the same position specified in "destination_directories".
#' This invokes the bash command "split" which is available natively on both
#' windows and MacOS. If you're on a different OS you might need to install it
#' or something, I don't know. You can learn how the "split" command works here:
#' \url{https://web.archive.org/web/20250228173733/https://man7.org/linux/man-pages/man1/split.1.html}
#' or by googling it. Note that the backslash character indicates an escape
#' character in an R string (\url{https://en.wikipedia.org/wiki/Escape_character}).
#'
#' @param files_to_chunk character vector of length n.
#' @param destination_directories character vector also of length n.
#'
#' @returns invisibly, the last result of a shell command to execute.
#' @export
model_split <- function(files_to_chunk, destination_directories) {
	requireNamespace("rlang", quietly = TRUE)
	requireNamespace("purrr", quietly = TRUE)
	requireNamespace("crayon", quietly = TRUE)

	# Detect the OS and message user about it
	this_os <- detect_os()

	# Validate the input arguments
	if (!is.character(files_to_chunk)) {
		rlang::abort(paste0(
			"'current_location' should be a character vector, not an object of",
			" class ", class(files_to_chunk), "."
		))
	}

	if (!is.character(destination_directories)) {
		rlang::abort(paste0(
			"'destination_location' should be a character vector, not an object of",
			" class ", class(destination_directories), "."
		))
	}

	if (length(files_to_chunk) != length(destination_directories)) {
		rlang::abort(paste0(
			"'current_location' and 'destination_location' should be ",
			"the same length."
		))
	}

	for (i in seq_along(files_to_chunk)) {
		# Make sure a directory exists to save the splitted models
		dir.create(
			destination_directories[[i]],
			showWarnings = FALSE, recursive = TRUE
		)

		# Create a string for the system command that uses the right file names
		split_cmd <-
			paste0(
				# Split command and options
				"split -b 25m ",
				# Current location of file
				"\"", files_to_chunk[[i]], "\" ",
				# Directory the splitted files will go to
				"\"", destination_directories[[i]], "/\""
			)

		# Invoke the bash system command
		res <- os_call(this_os, split_cmd)

		# Message if successful -- i.e. we get error code 0.
		if (res == 0) message(paste0("Successfully chunked model ", i, "."))
	}

	invisible(res)
}

#' Model dechunker
#'
#' The two arguments are both character vectors that should be equal length.
#' Each directory specified in the vector "chunk_directories" should be a
#' directory created by invoking model_cat() that contains the individual chunks
#' from a splitted file. This function recombines then and saves the combined
#' file to the file name in the same position in the vector "destination_file".
#' This invokes the bash command "cat" which is available natively on both
#' windows and MacOS. If you're on a different OS you might need to install it
#' or something, I don't know. You can learn how the "cat" command works here:
#' \url{https://web.archive.org/web/20250228174620/https://www.man7.org/linux/man-pages/man1/cat.1.html}
#' or by googling it.
#'
#' @param chunk_directories character vector of length n.
#' @param destination_file character vector also of length n.
#'
#' @returns invisibly, the last shell call result.
#' @export
model_cat <- function(chunk_directories, destination_file) {
	requireNamespace("rlang", quietly = TRUE)
	requireNamespace("purrr", quietly = TRUE)
	requireNamespace("crayon", quietly = TRUE)

	# Detect the OS and message user about it
	this_os <- detect_os()

	# Validate the input arguments
	if (!is.character(chunk_directories)) {
		rlang::abort(paste0(
			"'current_location' should be a character vector, not an object of",
			" class ", class(chunk_directories), "."
		))
	}

	if (!is.character(destination_file)) {
		rlang::abort(paste0(
			"'destination_location' should be a character vector, not an object of",
			" class ", class(destination_file), "."
		))
	}

	if (length(chunk_directories) != length(destination_file)) {
		rlang::abort(paste0(
			"'current_location' and 'destination_location' should be ",
			"the same length."
		))
	}

	for (i in seq_along(chunk_directories)) {
		# Make sure a directory exists to save the splitted models
		dir.create(
			get_directory_path(destination_file[[i]]),
			showWarnings = FALSE, recursive = TRUE
		)

		# Create a string for the system command
		cat_cmd <-
			paste0(
				# Split command and options
				"cat ",
				# Current location of file
				"\"", paste0(chunk_directories[[i]]), "\"/* > ",
				# Directory the splitted files will go to
				"\"", paste0(destination_file[[i]]), "\""
			)

		# Invoke the bash system command
		res <- os_call(this_os, cat_cmd)

		# Message if successful -- i.e. we get error code 0.
		if (res == 0) message(paste0("Successfully dechunked model ", i, "."))
	}

	invisible(res)
}
