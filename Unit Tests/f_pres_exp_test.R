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

f_pres_exp <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "6. Presenting Experience !B43:H60") %>%
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
    filter(!is.na(PC_category)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))
  # prov = str_trim(ifelse(grepl("Name of", prov_name), sub("Name of Organisation:\r\n", "", prov_name), prov_name)) %>%
  
  df$PC <- fillTheBlanks(df$PC_temp)
  
  df2 <- df %>%
    select(-1, -8) %>%
    filter(PC_category != "TOTAL") %>%
    gather("Presenting experience", "Number", 2:6) %>%
    numQA()
  
  df2 <- as.data.frame(df2)
  # Creating a new column to convert response to numeric class
  df2 <- final_clean(df2, 5)
  
  return(df2)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_pres_exp extracts organisation name", {
  
  result <- f_pres_exp(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Blanks successfully filled", {
  
  result <- f_RASASF_supp(x)
  
  expect_true(
    all(
      result$PC %in% c(
        "GENDER IDENTITY",
        "AGE",
        "ETHNICITY",
        "DISABILITY",
        "SEXUAL ORIENTATION"
      )
    )
  )
  
})

test_that("Total columns removed", {
  
  result <- f_pres_exp(x)
  
  expect_false(
    any(result$PC_category == "TOTAL")
  )
  
})

test_that("Numeric column is created", {
  
  result <- f_pres_exp(x)
  
  expect_true(is.numeric(result$Number_num))
  
})