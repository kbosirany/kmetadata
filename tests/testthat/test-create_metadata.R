test_that("create_metadata.data.frame creates variable metadata", {
  # Create test data
  df <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    status = c("active", "inactive", "active")
  )

  # Create variable metadata
  result <- create_metadata.data.frame(
    df,
    path = "test_data.csv",
    which = "vars"
  )

  # Check structure
  expect_type(result, "list")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "data.frame")

  # Check columns
  expect_named(result[[1]], c("label", "new_label", "description"))

  # Check content
  expect_equal(result[[1]]$label, colnames(df))
  expect_true(all(is.na(result[[1]]$new_label)))
  expect_true(all(is.na(result[[1]]$description)))
})

test_that("create_metadata.data.frame creates value metadata", {
  # Create test data
  df <- data.frame(
    id = 1:3,
    status = c("active", "inactive", "active"),
    level = c("high", "low", "medium")
  )

  # Create value metadata for specific variables
  result <- create_metadata.data.frame(
    df,
    path = "test_data.csv",
    which = "values",
    vars = c("status", "level")
  )

  # Check structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("status", "level"))

  # Check status metadata
  expect_s3_class(result$status, "data.frame")
  expect_named(result$status, c("label", "new_label", "description"))
  expect_equal(nrow(result$status), 2) # active and inactive
  expect_true(all(result$status$label %in% c("active", "inactive")))

  # Check level metadata
  expect_s3_class(result$level, "data.frame")
  expect_named(result$level, c("label", "new_label", "description"))
  expect_equal(nrow(result$level), 3) # high, low, medium
})

test_that("create_metadata.data.frame respects vars parameter", {
  # Create test data
  df <- data.frame(
    col1 = 1:3,
    col2 = letters[1:3],
    col3 = LETTERS[1:3]
  )

  # Create variable metadata for subset
  result <- create_metadata.data.frame(
    df,
    path = "test_data.csv",
    which = "vars",
    vars = c("col1", "col3")
  )

  # Only selected vars should be in metadata
  expect_equal(result[[1]]$label, c("col1", "col2", "col3"))
})

test_that("create_metadata.data.frame writes to file when requested", {
  skip_on_cran()
  
  # Create test data
  df <- data.frame(x = 1:3, y = letters[1:3])
  
  # Create temporary directory
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test_data.csv")
  meta_file <- file.path(temp_dir, "test_data - vars - metadata.xlsx")
  
  # Clean up if exists
  if (file.exists(meta_file)) file.remove(meta_file)
  
  # Create metadata with write_results = TRUE
  result <- create_metadata.data.frame(
    df,
    path = temp_file,
    which = "vars",
    write_results = TRUE
  )
  
  # Check file was created
  expect_true(file.exists(meta_file))
  
  # Clean up
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("create_metadata.list creates variable metadata for lists", {
  # Create test data
  df_list <- list(
    sheet1 = data.frame(a = 1:3, b = letters[1:3]),
    sheet2 = data.frame(x = 4:6, y = LETTERS[1:3])
  )

  # Create variable metadata
  result <- create_metadata.list(
    df_list,
    path = "test_data.xlsx",
    which = "vars"
  )

  # Check structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("sheet1", "sheet2"))

  # Check each sheet
  expect_equal(result$sheet1$label, c("a", "b"))
  expect_equal(result$sheet2$label, c("x", "y"))
})

test_that("create_metadata.list creates value metadata for lists", {
  # Create test data
  df_list <- list(
    sheet1 = data.frame(
      id = 1:3,
      type = c("A", "B", "A")
    ),
    sheet2 = data.frame(
      code = 4:6,
      category = c("X", "Y", "X")
    )
  )

  # Create value metadata
  result <- create_metadata.list(
    df_list,
    path = "test_data.xlsx",
    which = "values",
    vars = list(sheet1 = "type", sheet2 = "category")
  )

  # Check structure - results are flattened from nested list
  expect_type(result, "list")
  
  # The mapply and do.call(c, .) flattens the structure
  # So we get: sheet1.type, sheet2.category as names
  expect_true("sheet1.type" %in% names(result))
  expect_true("sheet2.category" %in% names(result))
  
  # Check content
  expect_s3_class(result$sheet1.type, "data.frame")
  expect_s3_class(result$sheet2.category, "data.frame")
  expect_equal(nrow(result$sheet1.type), 2) # A and B
  expect_equal(nrow(result$sheet2.category), 2) # X and Y
})

test_that("create_metadata handles empty data frames", {
  # Create empty data frame with columns
  df <- data.frame(
    col1 = integer(),
    col2 = character(),
    stringsAsFactors = FALSE
  )

  # Should still create metadata structure
  result <- create_metadata.data.frame(
    df,
    path = "test_data.csv",
    which = "vars"
  )

  expect_type(result, "list")
  expect_equal(result[[1]]$label, c("col1", "col2"))
})

test_that("create_metadata.data.frame handles single column", {
  # Single column data frame
  df <- data.frame(value = 1:5)

  result <- create_metadata.data.frame(
    df,
    path = "test_data.csv",
    which = "vars"
  )

  expect_equal(nrow(result[[1]]), 1)
  expect_equal(result[[1]]$label, "value")
})

test_that("path_metadata parameter works correctly", {
  df <- data.frame(x = 1:3)

  # Custom metadata path
  custom_path <- "custom/location/my_metadata.xlsx"

  result <- create_metadata.data.frame(
    df,
    path = "original_data.csv",
    which = "vars",
    path_metadata = custom_path,
    write_results = FALSE
  )

  # Should not error and return proper structure
  expect_type(result, "list")
  expect_s3_class(result[[1]], "data.frame")
})

test_that("overwrite parameter prevents accidental file deletion", {
  skip_on_cran()
  
  df <- data.frame(x = 1:3)
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "overwrite_test.csv")
  meta_file <- file.path(temp_dir, "overwrite_test - vars - metadata.xlsx")
  
  # Create initial metadata file
  if (file.exists(meta_file)) file.remove(meta_file)
  
  create_metadata.data.frame(
    df,
    path = temp_file,
    which = "vars",
    write_results = TRUE
  )
  
  initial_time <- file.info(meta_file)$mtime
  Sys.sleep(1)
  
  # Try to write again without overwrite
  create_metadata.data.frame(
    df,
    path = temp_file,
    which = "vars",
    write_results = TRUE,
    overwrite = FALSE
  )
  
  # File should not be modified
  expect_equal(file.info(meta_file)$mtime, initial_time)
  
  # Clean up
  if (file.exists(meta_file)) file.remove(meta_file)
})
