library(testthat)
library(readxl)

f_comp <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "3. Compliance !E7:G16") %>%
    select(1, 3) %>%
    rename(
      "variable" = 1,
      "response" = 2
    ) %>%
    filter(!is.na(response)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))
  
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_comp extracts organisation name", {
  
  result <- f_comp(x)
  
  expect_true(all(result$prov == "PETERBOROUGH RAPE CRISIS"))
  
})

test_that("f_comp removes rows with missing variables", {
  
  result <- f_comp(x)
  
  expect_equal(nrow(result), 9)
  expect_false(any(is.na(result$variable)))
  
})

test_that("f_comp selects first and third columns only", {
  
  result <- f_comp(x)
  
  expect_false("col2" %in% names(result))
  expect_true(all(c("variable", "response") %in% names(result)))
  
})

test_that("f_comp returns expected columns", {
  
  result <- f_comp(x)
  
  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c("variable", "response", "prov")
  )
  
})
