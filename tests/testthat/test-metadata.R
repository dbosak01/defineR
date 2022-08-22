base_path <- "c:/packages/defineR/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

test_that("metadata1: Import sdtm metadata xls works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 9)


})


test_that("metadata2: Import sdtm metadata xlsx works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA2.xlsx")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 10)


})


test_that("metadata3: Import adam metadata works as expected.", {

  fp <- file.path(data_dir, "data/ADAM_METADATA.xlsx")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 10)


})


test_that("metadata4: Import sdtm robustness metadata works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_robustness.xls")

  res <- import_metadata(fp)


  res

  expect_equal(length(res), 10)


})


test_that("metadata5: Parameter checks work.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_robustmess.xls")

  expect_error(import_metadata(fp, "DB2"))
  expect_error(import_metadata(fp))


  fp <- file.path(data_dir, "xsl/define2-0-0.xsl")

  expect_error(import_metadata(fp))


})


test_that("metadata6: Templates can be created.", {

  dp1 <- file.path(base_path, "metadata/template")
  dp2 <- file.path(base_path, "metadata/demo")


  fp1 <- file.path(dp1, "SDTM_METADATA.xlsx")
  fp2 <- file.path(dp2, "SDTM_METADATA.xlsx")
  fp3 <- file.path(dp2, "SDTM_METADATA(1).xlsx")
  fp4 <- file.path(dp2, "SDTM_METADATA(2).xlsx")

  if (file.exists(fp1))
    file.remove(fp1)

  if (file.exists(fp2))
    file.remove(fp2)

  if (file.exists(fp3))
    file.remove(fp3)

  if (file.exists(fp4))
    file.remove(fp4)

  res <- write_metadata(dp1)

  expect_equal(is.null(res), FALSE)
  expect_equal(file.exists(fp1), TRUE)
  expect_equal(file.exists(res), TRUE)


  res <- write_metadata(dp2, demo = TRUE)

  expect_equal(file.exists(fp2), TRUE)
  expect_equal(file.exists(res), TRUE)


  res <- write_metadata(dp2, demo = TRUE)

  expect_equal(file.exists(fp3), TRUE)
  expect_equal(file.exists(res), TRUE)

  res <- write_metadata(dp2, demo = TRUE)

  expect_equal(file.exists(fp4), TRUE)
  expect_equal(file.exists(res), TRUE)


})

test_that("metadata7: ADAM Templates can be created.", {

  dp1 <- file.path(base_path, "metadata/template")
  dp2 <- file.path(base_path, "metadata/demo")


  fp1 <- file.path(dp1, "ADAM_METADATA.xlsx")
  fp2 <- file.path(dp2, "ADAM_METADATA.xlsx")
  fp3 <- file.path(dp2, "ADAM_METADATA(1).xlsx")
  fp4 <- file.path(dp2, "ADAM_METADATA(2).xlsx")

  if (file.exists(fp1))
    file.remove(fp1)

  if (file.exists(fp2))
    file.remove(fp2)

  if (file.exists(fp3))
    file.remove(fp3)

  if (file.exists(fp4))
    file.remove(fp4)

  res <- write_metadata(dp1, type = "adam")

  expect_equal(is.null(res), FALSE)
  expect_equal(file.exists(fp1), TRUE)
  expect_equal(file.exists(res), TRUE)


  res <- write_metadata(dp2, demo = TRUE, type = "adam")

  expect_equal(file.exists(fp2), TRUE)
  expect_equal(file.exists(res), TRUE)


  res <- write_metadata(dp2, demo = TRUE, type = "adam")

  expect_equal(file.exists(fp3), TRUE)
  expect_equal(file.exists(res), TRUE)

  res <- write_metadata(dp2, demo = TRUE, type = "adam")

  expect_equal(file.exists(fp4), TRUE)
  expect_equal(file.exists(res), TRUE)


})

test_that("metadata8: Parameter checks work.", {

  fp <- file.path(base_path, "metadata/template")

  expect_error(write_metadata(fp, ver = "2.1"))
  expect_error(write_metadata(fp, type = "fork"))
  expect_error(write_metadata(fp, type = NULL))

})
