
base_path <- "c:\\packages\\defineR\\tests\\testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

DEV <- FALSE



test_that("write_define1: The write_define function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
  op <- file.path(base_path, "xml/test1.xml")

  mdt <- import_metadata_xl(fp)


  xml <- create_xml(mdt)

  write_xml(xml, op)

  expect_equal(file.exists(op), TRUE)

})
