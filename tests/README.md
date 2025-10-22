# Tests for kmetadata Package

## Overview

This directory contains comprehensive tests for all functions in the kmetadata package.

## Test Files

### test-create_metadata.R
Tests for the `create_metadata()` function and its methods:
- `create_metadata.data.frame()`: Tests variable and value metadata creation for data frames
- `create_metadata.list()`: Tests metadata creation for lists of data frames
- Edge cases: empty data frames, single columns, custom paths
- File writing and overwrite behavior

**Total tests: 38**

### test-load_metadata.R
Tests for the `load_metadata()` function:
- Loading existing metadata files
- Creating metadata when files don't exist
- Force recreation of metadata
- Different metadata types (vars vs values)
- Passing additional arguments
- Error handling

**Total tests: 20**

### test-integration.R
End-to-end integration tests:
- Complete workflows: create → write → load → edit → reload
- Multi-sheet Excel file handling
- Value metadata for categorical variables
- Metadata persistence across multiple loads
- Complex scenarios combining multiple features

**Total tests: 26**

## Test Results

All **84 tests** pass successfully with:
- ✅ 0 failures
- ✅ 0 warnings
- ✅ 0 skipped tests

## Running Tests

To run all tests:
```r
devtools::test()
```

To run a specific test file:
```r
testthat::test_file("tests/testthat/test-create_metadata.R")
```

To run with coverage:
```r
covr::package_coverage()
```

## Test Coverage

The test suite covers:
- All exported functions
- All S3 methods
- Edge cases and error conditions
- Integration scenarios
- File I/O operations
