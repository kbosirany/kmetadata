#' Load or Create Metadata
#'
#' @description
#' This function loads existing metadata from a file, or creates new
#' metadata if the file doesn't exist or if forced to regenerate. It
#' provides a convenient wrapper around `create_metadata()` with caching.
#'
#' @param path Character string. The path to the data file for which
#'   metadata should be loaded or created.
#' @param which Character string. Either "vars" for variable metadata or
#'   "values" for value metadata.
#' @param vars Character vector. Variables to include in metadata.
#'   Passed to `create_metadata()` if metadata needs to be created.
#' @param force Logical. If TRUE, forces recreation of metadata even if
#'   a metadata file already exists. Default is FALSE.
#' @param ... Additional arguments passed to `create_metadata()`.
#'
#' @return A list containing data frames with metadata. Each data frame
#'   has columns: `label`, `new_label`, and `description`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load or create variable metadata
#' var_meta <- load_metadata(
#'   path = "data/survey.csv",
#'   which = "vars",
#'   vars = c("age", "gender", "score")
#' )
#'
#' # Force recreation of metadata
#' var_meta <- load_metadata(
#'   path = "data/survey.csv",
#'   which = "vars",
#'   vars = c("age", "gender", "score"),
#'   force = TRUE
#' )
#'
#' # Load value metadata for specific variables
#' val_meta <- load_metadata(
#'   path = "data/survey.csv",
#'   which = "values",
#'   vars = c("gender", "status")
#' )
#' }
load_metadata <- function(path, which, vars, force = FALSE, ...) {
  path_metadata <- file.path(
    dirname(path),
    sprintf(
      "%s - %s - metadata.xlsx",
      tools::file_path_sans_ext(basename(path)),
      which
    )
  )

  metadata <- try(read(path_metadata), silent = TRUE)

  if (inherits(metadata, "try-error") || force) {
    metadata <- create_metadata(
      read(path), which = which, vars = vars, path_metadata = path_metadata, ...
    )
  }

  return(metadata)
}
