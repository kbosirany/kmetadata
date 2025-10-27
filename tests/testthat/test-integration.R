test_that("full workflow: create, write, and load metadata", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readr")

  # Setup
  temp_dir <- tempdir()
  data_file <- file.path(temp_dir, "workflow_test.csv")
  meta_file <- file.path(
    temp_dir,
    "workflow_test - metadata - vars.xlsx"
  )

  # Clean up if exists
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)

  # Step 1: Create test data
  test_data <- data.frame(
    id = 1:5,
    name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
    department = c("Sales", "IT", "Sales", "HR", "IT"),
    score = c(85, 90, 88, 92, 87),
    stringsAsFactors = FALSE
  )
  readr::write_csv(test_data, data_file)

  # Step 2: Create and write metadata
  metadata_created <- create_metadata(
    data_file,
    which = "vars",
    write_results = TRUE
  )

  expect_true(file.exists(meta_file))
  expect_type(metadata_created, "list")
  expect_equal(
    metadata_created[[1]]$label,
    c("id", "name", "department", "score")
  )

  # Step 3: Manually edit metadata (simulate user editing)
  edited_metadata <- list(
    README = data.frame(
      info = "This is test metadata",
      stringsAsFactors = FALSE
    ),
    vars = data.frame(
      label = c("id", "name", "department", "score"),
      new_label = c(
        "ID",
        "Employee Name",
        "Department",
        "Performance Score"
      ),
      description = c(
        "Unique identifier",
        "Full name of employee",
        "Department assignment",
        "Performance rating (0-100)"
      ),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(edited_metadata, meta_file)

  # Step 4: Load the edited metadata (use simplify = FALSE to get list)
  loaded_metadata <- load_metadata(
    path = data_file,
    which = "vars",
    vars = c("id", "name", "department", "score"),
    simplify = FALSE
  )

  expect_type(loaded_metadata, "list")
  expect_true("vars" %in% names(loaded_metadata))
  expect_equal(
    loaded_metadata$vars$new_label,
    c("ID", "Employee Name", "Department", "Performance Score")
  )

  # Step 5: Force recreation (with simplify = TRUE for data.frame)
  recreated_metadata <- load_metadata(
    path = data_file,
    which = "vars",
    vars = c("id", "name", "department", "score"),
    force = TRUE,
    simplify = TRUE
  )

  # Should have NA values again and be a data.frame
  expect_s3_class(recreated_metadata, "data.frame")
  expect_true(all(is.na(recreated_metadata$new_label)))

  # Clean up
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("workflow with value metadata for multiple variables", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("readr")

  # Setup
  temp_dir <- tempdir()
  data_file <- file.path(temp_dir, "values_workflow.csv")
  meta_file <- file.path(
    temp_dir,
    "values_workflow - metadata - values.xlsx"
  )

  # Clean up if exists
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)

  # Create test data with categorical variables
  test_data <- data.frame(
    id = 1:6,
    gender = c("M", "F", "M", "F", "M", "F"),
    status = c("A", "I", "A", "A", "I", "A"),
    level = c("L1", "L2", "L1", "L3", "L2", "L1"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(test_data, data_file)

  # Create value metadata
  value_metadata <- create_metadata(
    data_file,
    which = "values",
    vars = c("gender", "status", "level"),
    write_results = TRUE
  )

  expect_true(file.exists(meta_file))
  expect_type(value_metadata, "list")
  expect_length(value_metadata, 3)
  expect_named(value_metadata, c("gender", "status", "level"))

  # Check unique values captured
  expect_equal(nrow(value_metadata$gender), 2) # M, F
  expect_equal(nrow(value_metadata$status), 2) # A, I
  expect_equal(nrow(value_metadata$level), 3) # L1, L2, L3

  # Load metadata - simplify = FALSE because multiple variables
  loaded <- load_metadata(
    path = data_file,
    which = "values",
    vars = c("gender", "status", "level"),
    simplify = FALSE
  )

  expect_type(loaded, "list")
  # After removing README, should have 3 elements
  expect_length(loaded, 3)
  expect_named(loaded, c("gender", "status", "level"))

  # Clean up
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("workflow with list data (multiple sheets)", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("writexl")

  # Setup
  temp_dir <- tempdir()
  data_file <- file.path(temp_dir, "multisheet_test.xlsx")
  meta_file <- file.path(
    temp_dir,
    "multisheet_test - metadata - vars.xlsx"
  )

  # Clean up if exists
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)

  # Create test data with multiple sheets
  test_data <- list(
    employees = data.frame(
      emp_id = 1:3,
      emp_name = c("Alice", "Bob", "Charlie"),
      stringsAsFactors = FALSE
    ),
    departments = data.frame(
      dept_id = 1:2,
      dept_name = c("Sales", "IT"),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(test_data, data_file)

  # Create metadata for list
  metadata <- create_metadata(
    data_file,
    which = "vars",
    write_results = TRUE
  )

  expect_true(file.exists(meta_file))
  expect_type(metadata, "list")
  expect_length(metadata, 2)
  expect_named(metadata, c("employees", "departments"))

  # Check content
  expect_equal(metadata$employees$label, c("emp_id", "emp_name"))
  expect_equal(metadata$departments$label, c("dept_id", "dept_name"))

  # Load metadata
  loaded <- load_metadata(
    path = data_file,
    which = "vars",
    vars = list(
      employees = c("emp_id", "emp_name"),
      departments = c("dept_id", "dept_name")
    ),
    simplify = FALSE
  )

  expect_type(loaded, "list")
  expect_length(loaded, 2)
  expect_named(loaded, c("employees", "departments"))

  # Clean up
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})

test_that("metadata persists correctly across multiple loads", {
  skip_on_cran()
  skip_if_not_installed("kread")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readr")

  # Setup
  temp_dir <- tempdir()
  data_file <- file.path(temp_dir, "persist_test.csv")
  meta_file <- file.path(temp_dir, "persist_test - metadata - vars.xlsx")

  # Clean up if exists
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)

  # Create data - use more columns to avoid delimiter guess issues
  test_data <- data.frame(
    x = 1:3,
    y = letters[1:3],
    z = LETTERS[1:3],
    stringsAsFactors = FALSE
  )
  readr::write_csv(test_data, data_file)

  # First load - creates metadata
  meta1 <- load_metadata(
    path = data_file,
    which = "vars",
    vars = c("x", "y", "z"),
    simplify = FALSE
  )

  # Edit and save metadata
  edited <- list(
    README = data.frame(),
    vars = data.frame(
      label = c("x", "y", "z"),
      new_label = c("X Variable", "Y Variable", "Z Variable"),
      description = c("X description", "Y description", "Z description"),
      stringsAsFactors = FALSE
    )
  )
  writexl::write_xlsx(edited, meta_file)

  # Second load - should get edited version (with simplify = FALSE)
  meta2 <- load_metadata(
    path = data_file,
    which = "vars",
    vars = c("x", "y", "z"),
    simplify = FALSE
  )

  expect_equal(
    meta2$vars$new_label,
    c("X Variable", "Y Variable", "Z Variable")
  )

  # Third load - should still get same edited version
  meta3 <- load_metadata(
    path = data_file,
    which = "vars",
    vars = c("x", "y", "z"),
    simplify = FALSE
  )

  expect_equal(
    meta3$vars$new_label,
    c("X Variable", "Y Variable", "Z Variable")
  )
  expect_equal(meta2, meta3)

  # Clean up
  if (file.exists(data_file)) file.remove(data_file)
  if (file.exists(meta_file)) file.remove(meta_file)
})
