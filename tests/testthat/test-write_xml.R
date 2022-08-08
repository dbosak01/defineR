
base_path <- "c:\\packages\\defineR\\tests\\testthat"
base_path <- "~/cheese/defineR/tests/testthat"
data_dir <- base_path


base_path <- tempdir()
data_dir <- "."

DEV <- FALSE



test_that("create_xml test", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
  op <- file.path(base_path, "xml/test1.xml")

  mdt <- import_metadata_xl(fp)


  xml <- create_xml(mdt)

  write_xml(xml, op)

  expect_equal(file.exists(op), TRUE)

})

test_that("Base robustness tests", {

  fp <- file.path(getwd(), "inst/extdata/robust/SDTM_METADATA_robustness.xls")
  op <- file.path(base_path, "xml/test_robust.xml")

  mdt <- import_metadata_xl(fp)

  xml <- create_xml(mdt)

  write_xml(xml, op)

  expect_equal(file.exists(op), TRUE)

})
