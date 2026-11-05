library(testthat)
library(readxl)

final_clean <- function(x, y) {
  
  x$Number_num <- suppressWarnings(
    as.numeric(x[[y]])
  )
  
  x$Number_num[is.na(x$Number_num)] <- 0
  
  x
  
}

f_waits <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "9. Waiting Lists  !K13:R25") %>%
    select(1, 4, 6) %>%
    rename(
      "Date" = 1,
      "Adults" = 2,
      "CYP" = 3
    ) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    gather("Age", "Number", 2:3) %>%
    numQA()
  
  df <- as.data.frame(df)
  # Creating a new column to convert response to numeric class
  df <- final_clean(df, 4)
  
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_waits extracts organisation name", {
  
  result <- f_waits(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_waits(x)
  
  expect_true(is.numeric(result$Number_num))
  
})

test_that("Final structure is correct", {
  
  result <- f_waits(x)
  
  expect_true(
    all(c("Date", "Age", "Number", "prov") %in% names(result))
  )
})
