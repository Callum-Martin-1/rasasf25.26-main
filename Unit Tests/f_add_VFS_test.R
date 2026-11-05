library(testthat)
library(readxl)

fillTheBlanks <- function(x, missing = "") {
  r <- rle(as.character(x))
  empty <- which(r$values == missing)
  
  empty <- empty[empty > 1]
  
  r$values[empty] <- r$values[empty - 1]
  inverse.rle(r)
}

final_clean <- function(x, y) {
  
  x$Number_num <- suppressWarnings(
    as.numeric(x[[y]])
  )
  
  x$Number_num[is.na(x$Number_num)] <- 0
  
  x
  
}

f_add_VFS <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "10. Additional VFS Metrics !I15:N52") %>%
    mutate(PC_temp = case_when(
      `...1` %in% c(
        "GENDER IDENTITY",
        "AGE",
        "ETHNICITY",
        "DISABILITY",
        "SEXUAL ORIENTATION"
      ) ~ `...1`,
      TRUE ~ ""
    )) %>%
    rename("PC_category" = 2) %>%
    filter(
      !is.na(PC_category),
      PC_category != "TOTAL"
    ) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    select(-1, -3)
  
  df$PC <- fillTheBlanks(df$PC_temp)
  
  df2 <- df %>%
    select(-PC_temp) %>%
    gather("Variable", "Number", 2:4) %>%
    numQA()
  
  df2 <- as.data.frame(df2)
  # Creating a new column to convert response to numeric class
  df2 <- final_clean(df2, 5)
  
  return(df2)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_add_VFS extracts organisation name", {
  
  result <- f_add_VFS(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Blanks are successfully filled", {
  
  result <- f_add_VFS(x)
  
  expect_false(any(is.na(result$PC)))
  expect_false(any(result$PC == ""))
  
})

test_that("Totals are removed", {
  
  result <- f_add_VFS(x)
  
  expect_false(any(result$PC_category == "TOTAL"))
  
})

test_that("Numeric column is numeric", {
  
  result <- f_add_VFS(x)
  
  expect_true(is.numeric(result$Number_num))
  
})
