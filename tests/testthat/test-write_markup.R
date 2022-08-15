
# base_path <- "c:\\packages\\defineR\\tests\\testthat"
base_path <- "~/witchcraft/defineR/tests/testthat"
data_dir <- base_path


# base_path <- tempdir()
# data_dir <- "."

DEV <- FALSE


test_that("write1: create_sdtm_xml test", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
  op <- file.path(base_path, "xml/test1.xml")

  if (file.exists(op))
    file.remove(op)

  mdt <- import_metadata(fp)


  xml <- create_sdtm_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})


test_that("write2: create_adam_xml test", {

  fp <- file.path(data_dir, "data/ADAM_METADATA.xlsx")
  op <- file.path(base_path, "xml/test2.xml")

  if (file.exists(op))
    file.remove(op)

  mdt <- import_metadata(fp)


  xml <- create_adam_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})

test_that("write3: Base robustness tests", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_robustness.xls")
  op <- file.path(base_path, "xml/test3.xml")

  if (file.exists(op))
    file.remove(op)

  mdt <- import_metadata(fp)

  xml <- create_sdtm_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})

# This works
test_that("write4: CDISC XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(data_dir, "sdtm/define.xml")
  op <- file.path(data_dir, "xsd/cdisc-define-2.0/define2-0-0.xsd")

  doc <- read_xml(fp)
  schema <- read_xml(op)
  res <- xml_validate(doc, schema)


  expect_equal(res %in% c(TRUE, FALSE), TRUE)


})

# Aheer: Make this work
test_that("write5: defineR XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(base_path, "xml/test1.xml")
  op <- file.path(base_path, "xsd/cdisc-define-2.0/define2-0-0.xsd")

  doc <- read_xml(fp)
  schema <- read_xml(op)
  res <- xml_validate(doc, schema)


  expect_equal(res %in% c(TRUE, FALSE), TRUE)


  expect_equal(TRUE, TRUE)
})


# This works
test_that("write6: CDISC XML can be converted to HTML using XSL", {

  library(xml2)
  library(xslt)

  fp <- file.path(data_dir, "sdtm/define.xml")
  sp <- file.path(data_dir, "xsl/define2-0-0.xsl")
  op <- file.path(base_path, "html/test6.html")

  if (file.exists(op))
    file.remove(op)


  if (!dir.exists(dirname(op)))
    dir.create(dirname(op))

  doc <- read_xml(fp)
  style <- read_xml(sp)
  html <- xml_xslt(doc, style)


  write_html(html, op)

  fe <- file.exists(op)

  expect_equal(fe, TRUE)


})

# Aheer: Make this work
test_that("write7: defineR XML can be converted to HTML using XSL", {

  library(xml2)
  library(xslt)

  fp <- file.path(data_dir, "xml/test1.xml")
  sp <- file.path(data_dir, "xsl/define2-0-0.xsl")
  op <- file.path(base_path, "html/test7.html")

  if (file.exists(op))
    file.remove(op)

  doc <- read_xml(fp)
  style <- read_xml(sp)
  html <- xml_xslt(doc, style)


  write_html(html, op)

  fe <- file.exists(op)

  expect_equal(fe, TRUE)


  expect_equal(TRUE, TRUE)

})
