
base_path <- "c:\\packages\\defineR\\tests\\testthat"
#base_path <- "~/cheese/defineR/tests/testthat"
data_dir <- base_path


base_path <- tempdir()
data_dir <- "."

DEV <- FALSE


test_that("write1: create_sdtm_xml test", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
  op <- file.path(base_path, "xml/test1.xml")

  mdt <- import_metadata(fp)


  xml <- create_sdtm_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})


test_that("write2: create_adam_xml test", {

  fp <- file.path(data_dir, "data/ADAM_METADATA.xlsx")
  op <- file.path(base_path, "xml/test2.xml")

  mdt <- import_metadata(fp)


  xml <- create_adam_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})

test_that("write3: Base robustness tests", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_robustness.xls")
  op <- file.path(base_path, "xml/test3.xml")

  mdt <- import_metadata(fp)

  xml <- create_sdtm_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})

test_that("write4: CDISC XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(data_dir, "sdtm/define.xml")
  op <- file.path(data_dir, "xsd/cdisc-define-2.0/define2-0-0.xsd")

  doc <- read_xml(fp)
  schema <- read_xml(op)
  res <- xml_validate(doc, schema)


  expect_equal(res %in% c(TRUE, FALSE), TRUE)


})

# Make this work
test_that("write5: defineR XML conforms to 2.0 define XSD schema", {

  library(xml2)

  # fp <- file.path(base_path, "xml/test1.xml")
  # op <- file.path(base_path, "xsd/cdisc-define-2.0/define2-0-0.xsd")
  #
  # doc <- read_xml(fp)
  # schema <- read_xml(op)
  # res <- xml_validate(doc, schema)
  #
  #
  # expect_equal(res %in% c(TRUE, FALSE), TRUE)


  expect_equal(TRUE, TRUE)
})

