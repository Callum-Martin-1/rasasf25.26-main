library(testthat)

fillTheBlanks <- function(x, missing = "") {
  r <- rle(as.character(x))
  empty <- which(r$values == missing)
  
  empty <- empty[empty > 1]
  
  r$values[empty] <- r$values[empty - 1]
  inverse.rle(r)
}


test_that("fillTheBlanks fills blanks with previous value", {
  x <- c(
    "A",
    "",
    "",
    "B",
    "",
    "C"
  )

  result <- fillTheBlanks(x)

  expect_equal(
    result,
    c("A", "A", "A", "B", "B", "C")
  )
})

test_that("fillTheBlanks handles leading blanks", {
  expect_no_error(
    fillTheBlanks(c("", "", "A"))
  )
})
