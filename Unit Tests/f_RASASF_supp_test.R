library(testthat)
library(readxl)

final_clean <- function(x, y) {
  
  x$Number_num <- suppressWarnings(
    as.numeric(x[[y]])
  )
  
  x$Number_num[is.na(x$Number_num)] <- 0
  
  x
  
}

f_RASASF_supp <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "5. Referrals !B119", col_names = FALSE) %>%
    rename("Number" = 1) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    numQA()
  
  df <- as.data.frame(df)
  # Creating a new column to convert response to numeric class
  df <- final_clean(df, 1)
  
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_RASASF_supp extracts organisation name", {
  
  result <- f_RASASF_supp(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Numeric column created", {
  
  result <- f_RASASF_supp(x)
  
  expect_true(is.numeric(result$Number_num))
  
})