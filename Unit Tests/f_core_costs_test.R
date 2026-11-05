library(testthat)
library(readxl)

f_core_costs <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, sheet = "4 Financial Report 25-26")
  
  row_number <- match("Total Other Expenditure", df$...1)
  
  core_costs <- df[row_number, "...12"]
  
  core_costs <- core_costs %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))
  
  return(core_costs)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_core_costs extracts organisation name", {
  
  result <- f_core_costs(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("f_core_costs returns core expenditure", {
  
  result <- f_core_costs(x)
  
  expect_equal(result$...12, '0')
  
})

test_that("f_core_costs only returns one row", {
  
  result <- f_core_costs(x)
  
  expect_equal(nrow(result), 1)
})
