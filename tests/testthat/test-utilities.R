


test_that("utils1: The vencode() function works as expected.", {


  str <- c("Here > is < a single & string.")

  spl <- strsplit(str, split = "")[[1]]

  res <- vencode(spl)

  res

  expect_equal(res[[6]], "&gt;")
  expect_equal(res[[11]], "&lt;")
  expect_equal(res[[22]], "&amp;")

})


test_that("utils2: The encodeMarkup() function works as expected.", {

  str <- c("Here >", "is < ", "a single", "& string")

  res <- encodeMarkup(str)

  res

  expect_equal(length(res), 4)
  expect_equal(res[1], "Here &gt;")
  expect_equal(res[2], "is &lt; ")
  expect_equal(res[3], "a single")
  expect_equal(res[4], "&amp; string")

})



test_that("utils3: The encodeMarkup function works with NA as expected.", {

  str <- c("Here >", "is < ", NA, "a single", "& string")

  res <- encodeMarkup(str)

  res

  expect_equal(length(res), 5)
  expect_equal(res[1], "Here &gt;")
  expect_equal(res[2], "is &lt; ")
  expect_equal(res[3], "")
  expect_equal(res[4], "a single")
  expect_equal(res[5], "&amp; string")

})


test_that("utils4: cleanlabel() function is working.", {


  v1 <- ("here is, a / nice; label-with (some) stuff.")

  res <- cleanlabel(v1)

  res

  expect_equal(res, "here is, a / nice; label-with (some) stuff.")
})

