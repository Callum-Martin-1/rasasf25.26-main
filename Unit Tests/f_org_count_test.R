library(testthat)
library(readxl)

f_org_count <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- as.data.frame(prov_name) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), paste0(prov_name))))
  return(df)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_org_count extracts organisation name", {
  
  result <- f_org_count(x)
  
  expect_equal(result$prov, "PETERBOROUGH RAPE CRISIS")
  
})

test_that("f_org_count returns expected columns", {
  
  result <-f_org_count(x)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("prov_name", "prov"))
  
})