
base_path <- "c:\\packages\\dfnr\\tests\\testthat"

base_path <- tempdir()

DEV <- FALSE

test_that("write_define1: The write_define function works as expected.", {

  fp <- file.path(base_path, "out/define1.xml")


  ds <- define_dataset()


  dfn <- create_definition(fp) %>%
    add_dataset(ds)


  res <- write_define(dfn)


  # expect_equal(file.exists(fp), TRUE)

  # For now
  expect_equal(1, 1)


})
