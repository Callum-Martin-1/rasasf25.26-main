library(testthat)
library(readxl)

f_staffing <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "2. Organisation  !E11:G16") %>%
    rename(
      "variable" = 1,
      "response" = 3
    ) %>%
    filter(!is.na(variable)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))
  
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_staffing extracts organisation name", {
  
  result <- f_staffing(x)
  
  expect_true(all(result$prov == "PETERBOROUGH RAPE CRISIS"))
  
})

test_that("f_staffing removes rows with missing variables", {
  
  result <- f_staffing(x)
  
  expect_equal(nrow(result), 5)
  expect_false(any(is.na(result$variable)))
  
})

test_that("f_staffing returns expected columns", {
  
  result <- f_staffing(x)
  
  expect_named(
    result,
    c("variable", names(result)[2], "response", "prov")
  )
  
})
