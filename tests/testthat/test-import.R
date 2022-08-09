base_path <- "c:/packages/defineR/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

test_that("import1: Import sdtm metadata xls works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 9)


})


test_that("import2: Import sdtm metadata xlsx works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA2.xlsx")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 10)


})


test_that("import3: Import adam metadata works as expected.", {

  fp <- file.path(data_dir, "data/ADAM_METADATA.xlsx")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 10)


})


test_that("import4: Import sdtm robustness metadata works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_robustness.xls")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 10)


})


test_that("import5: Parameter checks work.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_robustmess.xls")

  expect_error(import_metadata(fp, "DB2"))
  expect_error(import_metadata(fp))


  fp <- file.path(data_dir, "xsl/define2-0-0.xsl")

  expect_error(import_metadata(fp))


})
