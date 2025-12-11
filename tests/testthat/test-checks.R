
base_path <- "c:\\packages\\defineR\\tests\\testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

DEV <- FALSE


test_that("checks1: The get check_xsd() function works as expected.", {

  fp <- file.path(data_dir, "sdtm/define.xml")

  msg <- c("Hello")

  res <- check_xsd(fp, "2.0.0", msg)

  res

  expect_equal(length(res) > 0, TRUE)
  expect_equal(res[1], "Hello")


})



test_that("checks2: get check_xsd() error messages work as expected.", {

  fp <- file.path(data_dir, "xml/corrupted.xml")

  msg <- c("Hello")

  res <- check_xsd(fp, "2.0.0", msg)

  res

  expect_equal(length(res), 4)
  expect_equal(res[1], "Hello")

})

# test_that("checks3: check_metadata() tab check works as expected.", {
#
#   msg <- "Hello"
#
#   fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
#
#   lst <- import_metadata(fp)
#
#   res <- check_metadata(lst, "2.0.0", FALSE, msg)
#
#   expect_equal(length(res), 1)
#
#
#   lst$VARIABLE_METADATA <- NULL
#   lst$CODELISTS <- NULL
#
#   res <- check_metadata(lst, "2.0.0", FALSE, msg)
#
#   expect_equal(length(res), 3)
#
# })

# test_that("checks4: write_check_report() works as expected.", {
#
#   msg <- c("Hello", "Goodbye")
#
#
#   op <- file.path(base_path, "checks/test4.pdf")
#
#   if (file.exists(op))
#     file.remove(op)
#
#   res <- write_check_report(op, msg, "PDF")
#
#
#   expect_equal(file.exists(op), TRUE)
#
#
# })
#
#
# test_that("checks5: view_check_report() works as expected.", {
#
#   if (DEV) {
#
#     res <- view_check_report(c())
#
#
#     msg <- c("Hello", "Goodbye")
#
#
#
#     res <- view_check_report(msg)
#
#
#   }
#
#   # No way to test this
#   # Must test interactively
#   expect_equal(1, 1)
#
#
# })

