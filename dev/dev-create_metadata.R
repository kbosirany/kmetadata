devtools::load_all()

test <- read(readxl::readxl_example("clippy.xls"))
class(test[[1]])

# metadata <- create_metadata(
#   test,
#   which = "vars",
#   path_metadata = "dev/list-metadata-vars.xlsx",
#   write_results = TRUE,
#   overwrite = FALSE
# )

metadata <- create_metadata(
  test,
  which = "values",
  path_metadata = "dev/list-metadata-values.xlsx",
  write_results = TRUE,
  overwrite = TRUE
)
