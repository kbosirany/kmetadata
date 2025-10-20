#' @export
#'
create_metadata <- function(x, ...) {
  UseMethod("create_metadata")
}

#' @export
#' @rdname create_metadata
#'
create_metadata.character <- function(x, ...) {
  create_metadata(read(x), path = x, ...)
}

#' @export
#' @rdname create_metadata
#'
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
            group_by(across(v)) %>%
            summarise(.groups = "drop") %>%
            rename(label = .data[[v]]) %>%
            mutate(new_label = NA, description = NA)
        }
      )
    }
  )

  if (write_results) {
    dir.create(dirname(path_metadata), showWarnings = FALSE, recursive = TRUE)
    if (file.exists(path_metadata) && overwrite) file.remove(path_metadata)
    writexl::write_xlsx(c(list(README = ""), metadata), path_metadata)
  }

  return(metadata)
}
