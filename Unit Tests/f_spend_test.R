library(testthat)
library(readxl)

fillTheBlanksBackwards <- function(x, missing = "") {
  r <- rle(rev(as.character(x)))
  
  empty <- which(r$values == missing)
  empty <- empty[empty > 1]
  
  r$values[empty] <- r$values[empty - 1]
  
  rev(inverse.rle(r))
}

f_spend <- function(x) {
  
  # Provider name ----
  prov <- colnames(
    read_xlsx(
      x,
      range = "2. Organisation  !G4"
    )
  ) %>%
    sub("Name of Organisation:\r\n", "", .) %>%
    stringr::str_trim()
  
  # Read financial sheet ONCE ----
  fin <- read_xlsx(
    x,
    sheet = "4 Financial Report 25-26",
    range = "A1:M72",
    col_names = FALSE
  )
  
  # Grant amount ----
  grant <- as.numeric(
    unlist(
      fin[72, 2]
    )
  )
  
  # Single support offer ----
  df <- fin[17:72, ] %>%
    transmute(
      spend_cat = .[[1]],
      `Apr 25 - Mar 26 Forecast` = .[[8]],
      `Apr 25 - Mar 26 Actual` = .[[12]],
      `Support offer` = as.character(unlist(fin[18, 2])),
      prov = prov,
      grant_amount = grant,
      group2 = "total cost costs"
    ) %>%
    
    # Remove blank rows
    filter(
      !(is.na(spend_cat) &
          (
            (is.na(`Apr 25 - Mar 26 Forecast`) |
               `Apr 25 - Mar 26 Forecast` == 0) |
              (is.na(`Apr 25 - Mar 26 Actual`) |
                 `Apr 25 - Mar 26 Actual` == 0)
          ))
    ) %>%
    
    # Assign groups
    mutate(
      group = case_when(
        spend_cat == "Total salaries" ~ "Salaries costs",
        spend_cat == "Total staff and volunteer costs" ~ "Staff and volunteer costs",
        spend_cat == "Total support costs" ~ "Support costs",
        spend_cat == "Total premises and office costs" ~ "Premises and office costs",
        spend_cat == "Total legal & professional costs" ~ "Legal & professional costs",
        spend_cat == "Total central functions" ~ "Central functions",
        TRUE ~ ""
      ),
      group2 = fillTheBlanksBackwards(group)
    )
  
  # Totals to exclude ----
  totals <- c(
    "Total salaries",
    "Total staff and volunteer costs",
    "Total support costs",
    "Total premises and office costs",
    "Total legal & professional costs",
    "Total central functions",
    "Total Grant Expenditure for Support Offer"
  )
  
  # Final output ----
  df %>%
    filter(
      is.na(spend_cat) |
        !spend_cat %in% totals
    ) %>%
    filter(group == "") %>%
    select(-group) %>%
    mutate(
      Actual_spend_QA = case_when(
        is.na(`Apr 25 - Mar 26 Actual`) |
          `Apr 25 - Mar 26 Actual` == "" ~ "No_number_given",
        grepl(
          "[A-Za-z]|^[[:punct:]]|[[:punct:]]$",
          `Apr 25 - Mar 26 Actual`
        ) ~ "Text_given",
        TRUE ~ as.character(`Apr 25 - Mar 26 Actual`)
      ),
      Forecast_spend_QA = case_when(
        is.na(`Apr 25 - Mar 26 Forecast`) |
          `Apr 25 - Mar 26 Forecast` == "" ~ "No_number_given",
        grepl(
          "[A-Za-z]|^[[:punct:]]|[[:punct:]]$",
          `Apr 25 - Mar 26 Forecast`
        ) ~ "Text_given",
        TRUE ~ as.character(`Apr 25 - Mar 26 Forecast`)
      ),
      Actual_spend_num = coalesce(
        suppressWarnings(
          as.numeric(`Apr 25 - Mar 26 Actual`)
        ),
        0
      ),
      Forecast_spend_num = coalesce(
        suppressWarnings(
          as.numeric(`Apr 25 - Mar 26 Forecast`)
        ),
        0
      )
    )
  
}

x = './RASASF Returns/002 PRCCG RASASF 2025-26 EY Monitoring Form TYPE 1.xlsx'

test_that("f_spend extracts organisation name", {
  
  result <- f_spend(x)
  
  expect_equal(unique(result$prov), "PETERBOROUGH RAPE CRISIS")
  
})

test_that("f_spend extracts grant amount", {
  
  result <- f_spend(x)
  
  expect_equal(unique(result$grant_amount), 113000)
  
})

test_that("f_spend extracts support offer", {
  
  result <- f_spend(x)
  
  expect_equal(unique(result$`Support offer`), "COUNSELLING/THERAPY - Specialist trauma informed emotional support to anyone affected by sexual violence who resides in Bedfordshire")
  
})

test_that("f_spend removes total rows", {
  
  result <- f_spend(x)
  
  expect_true(is.na(any(result$spend_cat == "Total salaries")))
  
})

test_that("group2 is backfilled correctly", {
  
  result <- f_spend(x)
  
  expect_false(any(result$group2 == ""))
  
})
