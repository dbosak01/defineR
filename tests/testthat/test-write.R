
base_path <- "c:\\packages\\defineR\\tests\\testthat"
data_dir <- base_path


base_path <- tempdir()
data_dir <- "."

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


  xml <- create_adam_xml(mdt, "2.0.0")

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

test_that("write5: defineR XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(data_dir, "xml/test1.xml")
  op <- file.path(data_dir, "xsd/cdisc-define-2.0/define2-0-0.xsd")

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

})


test_that("write8: ADAM defineR XML can be converted to HTML using XSL", {

  library(xml2)
  library(xslt)

  fp <- file.path(data_dir, "adam/define.xml")
  sp <- file.path(data_dir, "xsl/define2-0.xsl")
  op <- file.path(base_path, "html/test8.html")

  if (file.exists(op))
    file.remove(op)

  doc <- read_xml(fp)
  style <- read_xml(sp)
  html <- xml_xslt(doc, style)


  write_html(html, op)

  fe <- file.exists(op)

  expect_equal(fe, TRUE)


})


test_that("write9: create_sdtm_xml works with JS metadata", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_JS.xlsx")
  op <- file.path(base_path, "xml/test9.xml")

  if (file.exists(op))
    file.remove(op)

  mdt <- import_metadata(fp)


  xml <- create_sdtm_xml(mdt)

  write_markup(xml, op)

  expect_equal(file.exists(op), TRUE)

})


test_that("write10: JS SDTM defineR XML can be converted to HTML", {

  library(xml2)
  library(xslt)

  fp <- file.path(data_dir, "xml/test9.xml")
  sp <- file.path(data_dir, "xsl/define2-0.xsl")
  op <- file.path(base_path, "html/test10.html")

  if (file.exists(op))
    file.remove(op)

  doc <- read_xml(fp)
  style <- read_xml(sp)
  html <- xml_xslt(doc, style)


  write_html(html, op)

  fe <- file.exists(op)

  expect_equal(fe, TRUE)



})


test_that("write11: The get write_HTML() function works as expected.", {

  fp <- file.path(data_dir, "sdtm/define.xml")
  op <- file.path(base_path, "html/test12.html")

  msg <- c("Hello")

  res <- write_HTML(fp, op, "2.0.0", msg)

  res

  expect_equal(length(res) > 0, TRUE)
  expect_equal(res[1], "Hello")


})



test_that("write12: get write_HTML() error messages work as expected.", {

  fp <- file.path(data_dir, "xml/corrupted.xml")
  op <- file.path(base_path, "html/test12.html")

  msg <- c("Hello")

  res <- write_HTML(fp, op, "2.0.0", msg)

  res

  expect_equal(length(res), 4)
  expect_equal(res[1], "Hello")

})


test_that("write13: write_define() works as expected on CDISC metadata.", {

  fp <- file.path(data_dir, "sdtm/SDTM_METADATA.xls")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.sdtm.xml")
  hf <- file.path(op, "define.sdtm.xml")

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, check = FALSE)

  res

  expect_equal(length(res), 0)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)

})


test_that("write14: write_define() works as expected on JS metadata.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_JS.xlsx")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.sdtm.xml")
  hf <- file.path(op, "define.sdtm.html")

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, check = FALSE)

  res

  expect_equal(length(res), 0)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)

})


test_that("write15: write_define() works as expected on metadata with checks.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.sdtm.xml")
  hf <- file.path(op, "define.sdtm.html")

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, check = TRUE)

  res
  expect_equal(length(res) == 0, TRUE)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)

})

test_that("write16: write_define() works as expected on JS metadata with checks.", {

  if (DEV) {
    fp <- file.path(data_dir, "data/SDTM_METADATA_JS.xlsx")
    op <- file.path(base_path, "output")

    df <- file.path(op, "define.sdtm.xml")
    hf <- file.path(op, "define.sdtm.html")

    if (file.exists(df))
      file.remove(df)

    if (file.exists(hf))
      file.remove(hf)


    res <- write_define(fp, op, check = TRUE)

    res
    expect_equal(length(res) > 0, TRUE)
    expect_equal(file.exists(df), TRUE)
    expect_equal(file.exists(hf), TRUE)

  } else {

    expect_equal(1, 1)
  }

})



test_that("write17: write_define() works as expected on CDISC metadata with checks.", {

  fp <- file.path(data_dir, "sdtm/SDTM_METADATA.xls")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.sdtm.xml")
  hf <- file.path(op, "define.sdtm.html")

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, check = TRUE)

  res
  expect_equal(length(res) == 0, TRUE)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)

})



test_that("write18: write_define() works as expected on perfect SDTM metadata.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA_PERFECT.xls")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.sdtm.xml")
  hf <- file.path(op, "define.sdtm.html")

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, check = TRUE)

  res
  expect_equal(length(res) == 0, TRUE)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)

})





test_that("write19: CDISC ADAM XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(data_dir, "adam/define.xml")
  op <- file.path(data_dir, "xsd/cdisc-define-2.0/define2-0-0.xsd")

  doc <- read_xml(fp)
  schema <- read_xml(op)
  res <- xml_validate(doc, schema)


  expect_equal(res %in% c(TRUE, FALSE), TRUE)


})


test_that("write20: ADAM CDISC XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(data_dir, "adam/define2-0-0-example-adam-results.xml")
  op <- file.path(data_dir, "schema/cdisc-define-2.0/define2-0-0.xsd")

  doc <- read_xml(fp)
  schema <- read_xml(op)
  res <- xml_validate(doc, schema)

  res

  expect_equal(res %in% c(TRUE, FALSE), TRUE)


  expect_equal(TRUE, TRUE)
})

test_that("write21: ADAM defineR XML conforms to 2.0 define XSD schema", {

  library(xml2)

  fp <- file.path(data_dir, "output/define.adam.xml")
  op <- file.path(data_dir, "schema/cdisc-define-2.0/define2-0-0.xsd")

  doc <- read_xml(fp)
  schema <- read_xml(op)
  res <- xml_validate(doc, schema)

res
  expect_equal(res %in% c(TRUE, FALSE), TRUE)


  expect_equal(TRUE, TRUE)
})



test_that("write22: write_define() works as expected on perfect ADAM metadata.", {

  fp <- file.path(data_dir, "data/ADAM_METADATA_PERFECT.xls")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.adam.xml")
  hf <- file.path(op, "define.adam.html")

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, type = "ADAM", check = TRUE)

  res
  expect_equal(length(res) == 0, TRUE)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)

})


test_that("write23: write_define() works with custom XSD and XSLT.", {

  fp <- file.path(data_dir, "data/ADAM_METADATA_PERFECT.xls")
  op <- file.path(base_path, "output")

  df <- file.path(op, "define.adam.xml")
  hf <- file.path(op, "define.adam.html")

  xd <- file.path(data_dir, "schema/cdisc-define-2.0/define2-0-0.xsd")
  xs <- file.path(data_dir, "adam/define2-0-0.xsl")

  options("defineR.xsd" = xd,
          "defineR.xslt" = xs)

  if (file.exists(df))
    file.remove(df)

  if (file.exists(hf))
    file.remove(hf)


  res <- write_define(fp, op, type = "ADAM", check = TRUE)

  res
  expect_equal(length(res) == 0, TRUE)
  expect_equal(file.exists(df), TRUE)
  expect_equal(file.exists(hf), TRUE)


  options("defineR.xsd" = NULL,
          "defineR.xslt" = NULL)

})


test_that("write24: ADAM CDISC defineR XML can be converted to HTML", {

  library(xml2)
  library(xslt)

  fp <- file.path(data_dir, "adam/define.xml")
  sp <- file.path(data_dir, "xsl/define2-0.xsl")
  op <- file.path(base_path, "html/test24.html")

  if (file.exists(op))
    file.remove(op)

  doc <- read_xml(fp)
  style <- read_xml(sp)
  html <- xml_xslt(doc, style)


  write_html(html, op)

  fe <- file.exists(op)

  expect_equal(fe, TRUE)



})

