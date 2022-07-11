
base_path <- "c:\\packages\\dfnr\\tests\\testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

DEV <- FALSE

#
# test_that("write_define1: The write_define function works as expected.", {
#
#   fp <- file.path(base_path, "out/define1.xml")
#
#
#   ds1 <- define_dataset(name = "myds1") %>%
#     add_column("col1", label = "My label1", width = 25, type = "string") %>%
#     add_column("col2", label = "My label2", width = 8, type = "int")
#
#   ds2 <- define_dataset(name = "myds2") %>%
#     add_column("col1", label = "My label1", width = 25, type = "string") %>%
#     add_column("col2", label = "My label2", width = 8, type = "int")
#
#   dfn <- create_document(fp) %>%
#     add_dataset(ds1) %>%
#     add_dataset(ds2)
#
#
#   res <- write_xml(dfn, fp)
#
#
#   # expect_equal(file.exists(fp), TRUE)
#
#   # For now
#   expect_equal(1, 1)
#
#
# })


test_that("write_define1: The write_define function works as expected.", {

  fp <- file.path(data_dir, "data/SDTM_METADATA.xls")
  op <- file.path(base_path, "xml/test1.xml")

  mdt <- import_metadata_xl(fp)


  xml <- create_xml(mdt)

  write_xml(xml, op)

  expect_equal(file.exists(op), TRUE)

})
