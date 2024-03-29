
base_path <- "c:\\packages\\defineR\\tests\\testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

DEV <- FALSE


test_that("create1: The get_header function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  hd <- get_header(res[["DEFINE_HEADER_METADATA"]])

  hd

  expect_equal(length(hd), 1)


})



test_that("create2: The get_footer function works as expected.", {



  ft <- get_footer()

  ft

  expect_equal(length(ft), 3)


})


test_that("create3: The get_item_groups function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  itg <- get_item_groups(res[["TOC_METADATA"]], res[["VARIABLE_METADATA"]])

  itg

  expect_equal(length(itg) > 1, TRUE)


})


test_that("create4: The get_comments function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  cmts <- get_comments(res[["COMMENTS"]])

  cmts

  expect_equal(length(cmts), 3)


})



test_that("create5: The get_where function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  whr <- get_where(res[["WHERE_CLAUSES"]], res[["VALUELEVEL_METADATA"]])

  whr

  expect_equal(length(whr), 45)


})


test_that("create6: The get_value_level function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")

  res <- import_metadata(fp)


  vl <- get_value_level(res[["VALUELEVEL_METADATA"]], res[["WHERE_CLAUSES"]])

  vl

  expect_equal(length(vl)> 10, TRUE)


})


test_that("create7: Parameter checks on create_sdtm_xml work.", {

  expect_error(create_sdtm_xml(c("A", "B")))
  expect_error(create_sdtm_xml(mtcars))
  expect_error(create_sdtm_xml(list(mtcars), "2.1.0"))
  expect_error(create_sdtm_xml(list(mtcars), "2.0.0"))

})





