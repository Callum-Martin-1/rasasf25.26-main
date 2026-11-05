library(testthat)

final_clean <- function(x, y) {
  
  x$Number_num <- suppressWarnings(
    as.numeric(x[[y]])
  )
  
  x$Number_num[is.na(x$Number_num)] <- 0
  
  x
  
}

test_that("final_clean converts values to numeric", {
  
  df <- data.frame(
    Number = c("10", "20")
  )
  
  result <- final_clean(df, 1)
  
  expect_equal(
    result$Number_num,
    c(10, 20)
  )
  
})

test_that("final_clean converts invalid values to zero", {
  
  df <- data.frame(
    Number = c("abc")
  )
  
  result <- final_clean(df, 1)
  
  expect_equal(
    result$Number_num,
    0
  )
  
})
