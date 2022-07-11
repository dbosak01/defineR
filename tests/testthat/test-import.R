base_path <- "c:/packages/defineR/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

test_that("import1: Import works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata_xl(fp)


  expect_equal(length(res), 9)


})
