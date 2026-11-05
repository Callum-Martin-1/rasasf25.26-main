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

f_outcomes <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)
  
  df <- read_xlsx(x, range = "8. Outcomes  !B65:V104") %>%
    mutate(
      PC_category = case_when(
        grepl("disability", `...3`) ~ `...3`,
        grepl("illness", `...3`) ~ `...3`,
        TRUE ~ `...2`
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
    filter(
      !is.na(PC_category),
      PC_category != "TOTAL"
    ) %>%
    select(-c(1:3)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))
  
  df$PC <- fillTheBlanks(df$PC_temp)
  
  df_T1 <- df %>%
    select(1:3, 19, 21, 22) %>%
    rename(
      "Improved" = 1,
      "Deteriorated" = 2,
      "No change" = 3
    ) %>%
    gather("Response", "Number", 1:3) %>%
    mutate(Outcome = "Able to cope and build resilience")
  
  df_T2 <- df %>%
    select(4:6, 19, 21, 22) %>%
    rename(
      "Improved" = 1,
      "Deteriorated" = 2,
      "No change" = 3
    ) %>%
    gather("Response", "Number", 1:3) %>%
    mutate(Outcome = "More informed and empowered to act")
  
  df_T3 <- df %>%
    select(7:9, 19, 21, 22) %>%
    rename(
      "Improved" = 1,
      "Deteriorated" = 2,
      "No change" = 3
    ) %>%
    gather("Response", "Number", 1:3) %>%
    mutate(Outcome = "Feeling of safety")
  
  df_T4 <- df %>%
    select(10:12, 19, 21, 22) %>%
    rename(
      "Improved" = 1,
      "Deteriorated" = 2,
      "No change" = 3
    ) %>%
    gather("Response", "Number", 1:3) %>%
    mutate(Outcome = "Health and wellbeing")
  
  df_T5 <- df %>%
    select(13:15, 19, 21, 22) %>%
    rename(
      "Yes" = 1,
      "No" = 2,
      "Not stated" = 3
    ) %>%
    gather("Response", "Number", 1:3) %>%
    mutate(Outcome = "Received all support they needed when they wanted it")
  
  df_T6 <- df %>%
    select(16:18, 19, 21, 22) %>%
    rename(
      "Yes" = 1,
      "No" = 2,
      "Not stated" = 3
    ) %>%
    gather("Response", "Number", 1:3) %>%
    mutate(Outcome = "Increased engagement with CJS")
  
  df2 <- rbind(df_T1, df_T2, df_T3, df_T4, df_T5, df_T6) %>%
    numQA()
  
  df2 <- as.data.frame(df2)
  # Creating a new column to convert response to numeric class
  df2 <- final_clean(df2, 5)
  
  return(df2)
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_outcomes extracts organisation name", {
  
  result <- f_outcomes(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("Blanks are successfully filled", {
  
  result <- f_outcomes(x)
  
  expect_false(any(is.na(result$PC)))
  expect_false(any(result$PC == ""))
  
})

test_that("Totals are removed", {
  
  result <- f_outcomes(x)
  
  expect_false(any(result$PC_category == "TOTAL"))
  
})

test_that("Numeric column is numeric", {
  
  result <- f_outcomes(x)
  
  expect_true(is.numeric(result$Number_num))
  
})

test_that("Final structure is correct", {
  
  result <- f_outcomes(x)
  
  expect_equal(
    sort(unique(result$Response)),
    sort(c(
      "Improved",
      "Deteriorated",
      "No change",
      "Yes",
      "No",
      "Not stated"
    ))
  )
  
})
