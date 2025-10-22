test_that("load_metadata creates metadata when file doesn't exist", {
  skip_on_cran()
  skip_if_not_installed("kread")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "load_test.csv")
  meta_file <- file.path(
    temp_dir,
    "load_test - vars - metadata.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create a simple CSV file with multiple columns
  df <- data.frame(
    id = 1:3,
    name = c("A", "B", "C"),
    value = c(10, 20, 30)
  )
  write.csv(df, temp_file, row.names = FALSE)
  
  # Load metadata (should create it)
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("id", "name", "value")
  )
  
  # Check structure
  expect_type(result, "list")
  expect_s3_class(result[[1]], "data.frame")
  expect_named(result[[1]], c("label", "new_label", "description"))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata loads existing metadata file", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("writexl")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "existing_test.csv")
  meta_file <- file.path(
    temp_dir,
    "existing_test - vars - metadata.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(
    col1 = 1:3,
    col2 = letters[1:3],
    col3 = LETTERS[1:3]
  )
  write.csv(df, temp_file, row.names = FALSE)
  
  # Create metadata file
  metadata <- list(
    README = data.frame(),
    vars = data.frame(
      label = c("col1", "col2", "col3"),
      new_label = c("Column 1", "Column 2", "Column 3"),
      description = c("First column", "Second column", "Third column")
    )
  )
  writexl::write_xlsx(metadata, meta_file)
  
  # Load metadata
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("col1", "col2", "col3")
  )
  
  # Should have loaded the existing metadata
  expect_type(result, "list")
  expect_true("vars" %in% names(result))
  expect_equal(
    result$vars$new_label,
    c("Column 1", "Column 2", "Column 3")
  )
  expect_equal(
    result$vars$description,
    c("First column", "Second column", "Third column")
  )
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata force parameter recreates metadata", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("writexl")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "force_test.csv")
  meta_file <- file.path(
    temp_dir,
    "force_test - vars - metadata.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(x = 1:3, y = letters[1:3], z = LETTERS[1:3])
  write.csv(df, temp_file, row.names = FALSE)
  
  # Create existing metadata with custom values
  metadata <- list(
    README = data.frame(),
    vars = data.frame(
      label = c("x", "y", "z"),
      new_label = c("Old X", "Old Y", "Old Z"),
      description = c("Old desc X", "Old desc Y", "Old desc Z")
    )
  )
  writexl::write_xlsx(metadata, meta_file)
  
  # Load with force = TRUE (should recreate)
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("x", "y", "z"),
    force = TRUE
  )
  
  # Should have new metadata (with NA values)
  expect_type(result, "list")
  expect_s3_class(result[[1]], "data.frame")
  expect_true(all(is.na(result[[1]]$new_label)))
  expect_true(all(is.na(result[[1]]$description)))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata handles different 'which' values", {
  skip_on_cran()
  skip_if_not_installed("kread")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "which_test.csv")
  meta_file_vars <- file.path(
    temp_dir,
    "which_test - vars - metadata.xlsx"
  )
  meta_file_values <- file.path(
    temp_dir,
    "which_test - values - metadata.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file_vars)) file.remove(meta_file_vars)
  if (file.exists(meta_file_values)) file.remove(meta_file_values)
  
  # Create data file
  df <- data.frame(
    id = 1:3,
    status = c("active", "inactive", "active"),
    type = c("A", "B", "A")
  )
  write.csv(df, temp_file, row.names = FALSE)
  
  # Load variable metadata
  result_vars <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("id", "status", "type")
  )
  
  # Load value metadata
  result_values <- load_metadata(
    path = temp_file,
    which = "values",
    vars = c("status")
  )
  
  # Check structures are different
  expect_type(result_vars, "list")
  expect_type(result_values, "list")
  
  # vars should have one element with all variables
  expect_length(result_vars, 1)
  expect_equal(nrow(result_vars[[1]]), 3)
  
  # values should have unique status values
  expect_length(result_values, 1)
  expect_equal(nrow(result_values[[1]]), 2) # active, inactive
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file_vars)) file.remove(meta_file_vars)
  if (file.exists(meta_file_values)) file.remove(meta_file_values)
})

test_that("load_metadata passes additional arguments", {
  skip_on_cran()
  skip_if_not_installed("kread")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "args_test.csv")
  meta_file <- file.path(
    temp_dir,
    "args_test - vars - metadata.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(a = 1:3, b = letters[1:3], c = LETTERS[1:3])
  write.csv(df, temp_file, row.names = FALSE)
  
  # Load metadata with additional arguments
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("a", "b", "c"),
    write_results = FALSE,
    overwrite = FALSE
  )
  
  # Should work without errors
  expect_type(result, "list")
  
  # Metadata file should not be created
  expect_false(file.exists(meta_file))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata handles errors gracefully", {
  skip_on_cran()
  skip_if_not_installed("kread")
  
  # Try to load metadata for non-existent file
  temp_dir <- tempdir()
  non_existent <- file.path(temp_dir, "does_not_exist.csv")
  
  # Should error when trying to read the file
  expect_error(
    load_metadata(
      path = non_existent,
      which = "vars",
      vars = c("col1")
    )
  )
})
