library(testthat)
library(readxl)

final_clean <- function(x, y) {
  
  x$Number_num <- suppressWarnings(
    as.numeric(x[[y]])
  )
  
  x$Number_num[is.na(x$Number_num)] <- 0
  
  x
  
}

f_waits2 <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "9. Waiting Lists  !K28:Q42") %>%
    filter(!is.na(.[[1]])) %>%
    rename(
      "Support type" = 1,
      "Total" = 2,
      "Closed?" = 3,
      "Closed from" = 4,
      "Closed to" = 5,
      "Average" = 6,
      "Longest" = 7
    ) %>%
    gather("Variable", "Number", c(2, 6, 7)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    numQA()
  
  df <- as.data.frame(df)
  # Creating a new column to convert response to numeric class
  df <- final_clean(df, 6)
  
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_waits2 extracts organisation name", {
  
  result <- f_waits2(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_waits2(x)
  
  expect_true(is.numeric(result$Number_num))
  
})

test_that("Metrics are created", {
  
  result <- f_waits2(x)
  
  expect_equal(
    sort(unique(result$Variable)),
    sort(c("Total", "Average", "Longest"))
  )
})

test_that("Final structure is correct", {
  
  result <- f_waits2(x)
  
  expect_true(
    all(c("Support type", "Variable", "Number", "prov") %in% names(result))
  )
})
