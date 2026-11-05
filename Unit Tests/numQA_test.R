library(testthat)

numQA <- function(x) {
  x %>%
    mutate(Number_QA = case_when(
      Number == "" | is.na(Number) ~ "No_number_given",
      grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Number) == TRUE ~ "Text_given",
      TRUE ~ as.character(Number)
    ))
}

test_that("numQA identifies missing values", {
  
  df <- data.frame(
    Number = c(NA, "")
  )
  
  result <- numQA(df)
  
  expect_equal(
    result$Number_QA,
    c("No_number_given",
      "No_number_given")
  )
  
})

test_that("numQA identifies text", {
  
  df <- data.frame(
    Number = c("abc")
  )
  
  result <- numQA(df)
  
  expect_equal(
    result$Number_QA,
    "Text_given"
  )
  
})

test_that("numQA accepts numbers", {
  
  df <- data.frame(
    Number = c("123")
  )
  
  result <- numQA(df)
  
  expect_equal(
    result$Number_QA,
    "123"
  )
  
})