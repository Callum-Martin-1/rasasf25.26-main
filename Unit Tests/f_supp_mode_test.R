library(testthat)
library(readxl)


f_supp_mode <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "7. Support Type  !B66:O104") %>%
    filter(!is.na(.[[1]])) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    gather("Support type", "Support mode", 1:11)
  
  df <- as.data.frame(df)
  
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_supp_mode extracts organisation name", {
  
  result <- f_supp_mode(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Check final structure", {
  
  result <- f_supp_mode(x)
  
  expect_true(
    all(c("Support type", "Support mode", "prov") %in% names(result))
  )
  
  expect_false(any(is.na(result$prov)))
  
  expect_false(any(is.na(result$`Support type`)))
  
})

