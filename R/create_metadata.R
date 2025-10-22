#' Create Metadata for Variables or Values
#'
#' @description
#' This function creates metadata templates for either variables (column
#' names) or values (unique values within columns) of a dataset. The
#' metadata can be written to an Excel file for manual editing.
#'
#' @param x An object for which to create metadata. Can be a character
#'   string (file path), a data.frame, or a list of data.frames.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list containing data frames with metadata templates. Each
#'   data frame has columns: `label`, `new_label`, and `description`.
#'
#' @export
#'
#' @examples
#' # Create metadata for a data frame
#' df <- data.frame(
#'   age = c(25, 30, 35),
#'   gender = c("M", "F", "M"),
#'   score = c(85, 90, 88)
#' )
#'
#' # Create variable metadata
#' var_meta <- create_metadata(df, which = "vars")
#'
#' # Create value metadata for specific variables
#' val_meta <- create_metadata(df, which = "values", vars = "gender")
create_metadata <- function(x, ...) {
  UseMethod("create_metadata")
}

#' @rdname create_metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Create metadata from a file path
#' meta <- create_metadata(
#'   "data/mydata.csv",
#'   which = "vars",
#'   write_results = TRUE
#' )
#' }
create_metadata.character <- function(x, ...) {
  create_metadata(read(x), path = x, ...)
}

#' @rdname create_metadata
#' @export
#'
#' @param path Character string. The path to the original data file.
#'   Used to construct the metadata file path.
#' @param which Character string. Either "vars" for variable metadata
#'   or "values" for value metadata.
#' @param path_metadata Character string. The path where the metadata
#'   file will be saved. Default constructs path from `path` and
#'   `which` parameters.
#' @param vars Character vector. For "vars": variables to include. For
#'   "values": variables for which to extract unique values. Default is
#'   all columns.
#' @param write_results Logical. If TRUE, writes metadata to an Excel
#'   file. Default is FALSE.
#' @param overwrite Logical. If TRUE, overwrites existing metadata file.
#'   Default is FALSE.
#'
#' @examples
#' # Create variable metadata for a data frame
#' df <- data.frame(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie"),
#'   status = c("active", "inactive", "active")
#' )
#'
#' var_metadata <- create_metadata(
#'   df,
#'   path = "data.csv",
#'   which = "vars"
#' )
#'
#' # Create value metadata for specific variables
#' val_metadata <- create_metadata(
#'   df,
#'   path = "data.csv",
#'   which = "values",
#'   vars = c("status")
#' )
create_metadata.data.frame <- function(
  x,
  path,
  which,
  path_metadata = file.path(
    dirname(path),
    sprintf(
      "%s - %s - metadata.xlsx",
      tools::file_path_sans_ext(basename(path)),
      which
    )
  ),
  vars = colnames(x),
  write_results = FALSE,
  overwrite = FALSE,
  ...
) {
  switch(
    which,
    vars = {
      metadata <- list(
        data.frame(label = names(x), new_label = NA, description = NA)
      )
    },
    values = {
      metadata <- lapply(
        setNames(nm = vars),
        function(v) {
          x %>%
            group_by(across(all_of(v))) %>%
            summarise(.groups = "drop") %>%
            rename(label = all_of(v)) %>%
            mutate(new_label = NA, description = NA)
        }
      )
    }
  )

  if (write_results) {
    dir.create(dirname(path_metadata), showWarnings = FALSE, recursive = TRUE)
    if (file.exists(path_metadata) && overwrite) file.remove(path_metadata)
    writexl::write_xlsx(c(list(README = data.frame()), metadata), path_metadata)
  }

  return(metadata)
}

#' @rdname create_metadata
#' @export
#'
#' @examples
#' # Create metadata for a list of data frames
#' df_list <- list(
#'   sheet1 = data.frame(x = 1:3, y = letters[1:3]),
#'   sheet2 = data.frame(a = 4:6, b = LETTERS[1:3])
#' )
#'
#' # Create variable metadata for all sheets
#' list_var_meta <- create_metadata(
#'   df_list,
#'   path = "data.xlsx",
#'   which = "vars"
#' )
#'
#' # Create value metadata for specific variables in each sheet
#' list_val_meta <- create_metadata(
#'   df_list,
#'   path = "data.xlsx",
#'   which = "values",
#'   vars = list(sheet1 = "y", sheet2 = "b")
#' )
create_metadata.list <- function(
  x,
  path,
  which,
  path_metadata = file.path(
    dirname(path),
    sprintf(
      "%s - %s - metadata.xlsx",
      tools::file_path_sans_ext(basename(path)),
      which
    )
  ),
  vars = lapply(x, colnames),
  write_results = FALSE,
  overwrite = FALSE,
  ...
) {
  switch(
    which,
    vars = {
      metadata <- lapply(lapply(x, create_metadata, which = "vars"), "[[", 1)
    },
    values = {
      metadata <- Map(
        function(df, v, nm) {
          create_metadata(df, which = "values", vars = v)
        },
        x, vars, names(x)
      )
      # Flatten and rename with sheet prefix
      metadata <- unlist(metadata, recursive = FALSE)
      # The names will be like sheet1.varname, sheet2.varname
    }
  )

  if (write_results) {
    dir.create(dirname(path_metadata), showWarnings = FALSE, recursive = TRUE)
    if (file.exists(path_metadata) && overwrite) file.remove(path_metadata)
    writexl::write_xlsx(c(list(README = data.frame()), metadata), path_metadata)
  }

  return(metadata)
}
