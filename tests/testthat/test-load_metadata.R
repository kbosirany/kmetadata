test_that("load_metadata creates metadata when file doesn't exist", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  skip_if_not_installed("readr")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "load_test_create.csv")
  meta_file <- file.path(
    temp_dir,
    "load_test_create - metadata - vars.xlsx"
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
  readr::write_csv(df, temp_file)
  
  # Load metadata (should create it)
  # With simplify = TRUE (default), single element is returned as data.frame
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("id", "name", "value")
  )
  
  # Check structure - should be a data.frame (simplified)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("label", "new_label", "description"))
  expect_equal(nrow(result), 3)
  expect_equal(result$label, c("id", "name", "value"))
  expect_true(all(is.na(result$new_label)))
  expect_true(all(is.na(result$description)))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata loads existing metadata file", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  skip_if_not_installed("writexl")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "existing_test_load.csv")
  meta_file <- file.path(
    temp_dir,
    "existing_test_load - metadata - vars.xlsx"
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
  readr::write_csv(df, temp_file)
  
  # Create metadata file (note: the sheet name should match)
  metadata <- list(
    README = data.frame(),
    vars = data.frame(
      label = c("col1", "col2", "col3"),
      new_label = c("Column 1", "Column 2", "Column 3"),
      description = c("First column", "Second column", "Third column"),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(metadata, meta_file)
  
  # Load metadata - with simplify = FALSE to get the full list
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("col1", "col2", "col3"),
    simplify = FALSE
  )
  
  # Should have loaded the existing metadata (without README)
  expect_type(result, "list")
  expect_true("vars" %in% names(result))
  expect_false("README" %in% names(result))
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
  skip_if_not_installed("readr")
  skip_if_not_installed("writexl")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "force_test_reload.csv")
  meta_file <- file.path(
    temp_dir,
    "force_test_reload - metadata - vars.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(x = 1:3, y = letters[1:3], z = LETTERS[1:3])
  readr::write_csv(df, temp_file)
  
  # Create existing metadata with custom values
  metadata <- list(
    README = data.frame(),
    vars = data.frame(
      label = c("x", "y", "z"),
      new_label = c("Old X", "Old Y", "Old Z"),
      description = c("Old desc X", "Old desc Y", "Old desc Z"),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(metadata, meta_file)
  
  # Load with force = TRUE (should recreate)
  # simplify = TRUE by default, so returns a data.frame
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("x", "y", "z"),
    force = TRUE
  )
  
  # Should have new metadata (with NA values)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("label", "new_label", "description"))
  expect_equal(result$label, c("x", "y", "z"))
  expect_true(all(is.na(result$new_label)))
  expect_true(all(is.na(result$description)))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata handles different 'which' values", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "which_test_load.csv")
  meta_file_vars <- file.path(
    temp_dir,
    "which_test_load - metadata - vars.xlsx"
  )
  meta_file_values <- file.path(
    temp_dir,
    "which_test_load - metadata - values.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file_vars)) file.remove(meta_file_vars)
  if (file.exists(meta_file_values)) file.remove(meta_file_values)
  
  # Create data file
  df <- data.frame(
    id = 1:3,
    status = c("active", "inactive", "active"),
    type = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, temp_file)
  
  # Load variable metadata (simplify = TRUE by default)
  result_vars <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("id", "status", "type")
  )
  
  # Load value metadata (simplify = FALSE to get list)
  result_values <- load_metadata(
    path = temp_file,
    which = "values",
    vars = c("status"),
    simplify = FALSE
  )
  
  # Check structures are different
  # vars with simplify = TRUE should be a data.frame
  expect_s3_class(result_vars, "data.frame")
  expect_equal(nrow(result_vars), 3)
  expect_equal(result_vars$label, c("id", "status", "type"))
  
  # values should be a list with one element
  expect_type(result_values, "list")
  expect_length(result_values, 1)
  expect_named(result_values, "status")
  expect_equal(nrow(result_values$status), 2) # active, inactive
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file_vars)) file.remove(meta_file_vars)
  if (file.exists(meta_file_values)) file.remove(meta_file_values)
})

test_that("load_metadata passes additional arguments", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "args_test_load.csv")
  meta_file <- file.path(
    temp_dir,
    "args_test_load - metadata - vars.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(a = 1:3, b = letters[1:3], c = LETTERS[1:3])
  readr::write_csv(df, temp_file)
  
  # Load metadata with additional arguments
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("a", "b", "c"),
    write_results = FALSE,
    overwrite = FALSE
  )
  
  # Should work without errors and return simplified result
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # Metadata file should not be created
  expect_false(file.exists(meta_file))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata handles errors gracefully", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  
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

test_that("load_metadata simplify parameter works correctly", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "simplify_test.csv")
  meta_file <- file.path(
    temp_dir,
    "simplify_test - metadata - vars.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(x = 1:3, y = letters[1:3])
  readr::write_csv(df, temp_file)
  
  # Test with simplify = TRUE (default)
  result_simplified <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("x", "y"),
    simplify = TRUE
  )
  
  # Should be a data.frame
  expect_s3_class(result_simplified, "data.frame")
  expect_equal(nrow(result_simplified), 2)
  
  # Clean metadata file for next test
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Test with simplify = FALSE
  result_not_simplified <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("x", "y"),
    simplify = FALSE
  )
  
  # Should be a list
  expect_type(result_not_simplified, "list")
  expect_length(result_not_simplified, 1)
  expect_s3_class(result_not_simplified[[1]], "data.frame")
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata with multiple value variables doesn't simplify", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "multi_value_test.csv")
  meta_file <- file.path(
    temp_dir,
    "multi_value_test - metadata - values.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(
    status = c("active", "inactive", "active"),
    type = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, temp_file)
  
  # Load value metadata for multiple variables
  result <- load_metadata(
    path = temp_file,
    which = "values",
    vars = c("status", "type"),
    simplify = TRUE  # Even with TRUE, should return list if > 1 element
  )
  
  # Should be a list because there are 2 variables
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("status", "type"))
  expect_s3_class(result$status, "data.frame")
  expect_s3_class(result$type, "data.frame")
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata README sheet is always removed", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  skip_if_not_installed("writexl")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "readme_test.csv")
  meta_file <- file.path(
    temp_dir,
    "readme_test - metadata - vars.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(a = 1:3, b = 4:6)
  readr::write_csv(df, temp_file)
  
  # Create metadata file with README
  metadata <- list(
    README = data.frame(info = "This is metadata"),
    vars = data.frame(
      label = c("a", "b"),
      new_label = c("A", "B"),
      description = c("First", "Second"),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(metadata, meta_file)
  
  # Load with simplify = FALSE to see all sheets
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("a", "b"),
    simplify = FALSE
  )
  
  # README should not be present
  expect_false("README" %in% names(result))
  expect_true("vars" %in% names(result))
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata preserves custom metadata edits", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  skip_if_not_installed("writexl")
  
  # Create test data file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "custom_edits_test.csv")
  meta_file <- file.path(
    temp_dir,
    "custom_edits_test - metadata - vars.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create data file
  df <- data.frame(
    employee_id = 1:3,
    dept_code = c("IT", "HR", "FN"),
    salary = c(50000, 60000, 70000)
  )
  readr::write_csv(df, temp_file)
  
  # Create custom metadata
  metadata <- list(
    README = data.frame(),
    vars = data.frame(
      label = c("employee_id", "dept_code", "salary"),
      new_label = c("Employee ID", "Department Code", "Annual Salary"),
      description = c(
        "Unique employee identifier",
        "Department code",
        "Annual salary in USD"
      ),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(metadata, meta_file)
  
  # Load metadata (should use existing file)
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("employee_id", "dept_code", "salary"),
    simplify = FALSE
  )
  
  # Custom edits should be preserved
  expect_equal(
    result$vars$new_label,
    c("Employee ID", "Department Code", "Annual Salary")
  )
  expect_equal(
    result$vars$description[1],
    "Unique employee identifier"
  )
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("load_metadata works with Excel input files", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")
  skip_if_not_installed("writexl")
  
  # Create test Excel file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "excel_input_test.xlsx")
  meta_file <- file.path(
    temp_dir,
    "excel_input_test - metadata - vars.xlsx"
  )
  
  # Clean up if exists
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create Excel file
  df <- data.frame(
    col_a = 1:3,
    col_b = letters[1:3]
  )
  writexl::write_xlsx(list(Sheet1 = df), temp_file)
  
  # Load metadata
  result <- load_metadata(
    path = temp_file,
    which = "vars",
    vars = c("col_a", "col_b")
  )
  
  # Should work and return simplified data.frame
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})
