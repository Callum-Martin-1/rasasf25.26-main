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

f_refs_supp <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "5. Referrals !B71:S109") %>%
    mutate(
      PC_category = case_when(
        grepl("disability", `...3`) ~ `...3`,
        grepl("illness", `...3`) ~ `...3`,
        TRUE ~ `...2` # changed from 2
      ),
      PC_temp = case_when(
        `...1` %in% c(
          "GENDER IDENTITY",
          "AGE",
          "ETHNICITY",
          "DISABILITY",
          "SEXUAL ORIENTATION"
        ) ~ `...1`,
        TRUE ~ ""
      )
    ) %>%
    filter(!is.na(PC_category)) %>%
    select(-c(1:3)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))
  
  df$PC <- fillTheBlanks(df$PC_temp)
  
  df2 <- df %>%
    select(-PC_temp, -`TOTAL`) %>%
    gather("Variable", "Number", 1:14) %>%
    filter(PC_category != "TOTAL") %>%
    mutate(Category = case_when(
      grepl("supported", Variable) ~ "Supported victims",
      TRUE ~ "New referrals"
    )) %>%
    numQA()
  
  df2 <- as.data.frame(df2)
  # Creating a new column to convert response to numeric class
  df2 <- final_clean(df2, 5)
  
  return(df2)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_refs_supp extracts organisation name", {
  
  result <- f_refs_supp(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("PC categories are completely populated", {
  
  result <- f_refs_supp(x)
  
  expect_false(any(is.na(result$PC)))
  expect_false(any(result$PC == ""))
  
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

test_that("f_refs_supp pivots data correctly", {
  
  result <- f_refs_supp(x)
  
  expect_true("Variable" %in% names(result))
  expect_true("Number" %in% names(result))
  
})