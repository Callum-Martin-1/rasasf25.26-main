library(testthat)

fillTheBlanksBackwards <- function(x, missing = "") {
  r <- rle(rev(as.character(x)))
  
  empty <- which(r$values == missing)
  empty <- empty[empty > 1]
  
  r$values[empty] <- r$values[empty - 1]
  
  rev(inverse.rle(r))
}

test_that("fillTheBlanksBackwards fills blanks with next value", {
  x <- c(
    "",
    "",
    "A",
    "",
    "B",
    "C"
  )

  result <- fillTheBlanksBackwards(x)

  expect_equal(
    result,
    c("A", "A", "A", "B", "B", "C")
  )
})

test_that("fillTheBlanksBackwards handles leading blanks", {
  expect_no_error(
    fillTheBlanks(c("", "", "A"))
  )
})
