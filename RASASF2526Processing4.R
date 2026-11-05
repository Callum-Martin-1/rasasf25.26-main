# SETUP -----------------------------------------------------------------------------------
# Empty the RASASF Returns folder before running MASTER SCRIPT

# Load in packages
library(tidyverse)
library(readxl)
library(validate)
library(misty)
library(lubridate)
library(stringr)
# library(aws.s3)
library(Rs3tools)

# Listing out RASASF returns
file.list <- Rs3tools::list_files_in_buckets("alpha-rasasf2526", prefix = "4 Support Offers/")
file.list <- file.list$filename
file.list <- paste0("./RASASF Returns/", file.list)

# Functions -------------------------------------------------------------------------------------------

# Function created for filling blanks 
fillTheBlanks <- function(x, missing = "") {
  r <- rle(as.character(x))
  empty <- which(r$values == missing)
  
  empty <- empty[empty > 1]
  
  r$values[empty] <- r$values[empty - 1]
  inverse.rle(r)
}

# Function created for back filling blanks in order to assign the right groupings to the sub-groups
fillTheBlanksBackwards <- function(x, missing = "") {
  r <- rle(rev(as.character(x)))
  
  empty <- which(r$values == missing)
  empty <- empty[empty > 1]
  
  r$values[empty] <- r$values[empty - 1]
  
  rev(inverse.rle(r))
}

# Common data cleaning functions
numQA <- function(x) {
  x %>%
    mutate(Number_QA = case_when(
      Number == "" | is.na(Number) ~ "No_number_given",
      grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Number) == TRUE ~ "Text_given",
      TRUE ~ as.character(Number)
    ))
}


final_clean <- function(x, y) {
  x$Number_num <- sapply(x[y], as.numeric)
  x["Number_num"][is.na(x["Number_num"])] <- 0

  return(x)
}


# Function to extract general contact info from the form
f_org_count <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- as.data.frame(prov_name) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), paste0(prov_name))))
  return(df)
}


# Function to extract general contact info from the form
f_info <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- read_xlsx(x, range = "2. Organisation  !E3:G9") %>%
    rename(
      "variable" = 1,
      "response" = 3
    ) %>%
    filter(!is.na(variable)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))

  return(df)
}


# Function to extract staffing info from the form
f_staffing <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- read_xlsx(x, range = "2. Organisation  !E11:G16") %>%
    rename(
      "variable" = 1,
      "response" = 3
    ) %>%
    filter(!is.na(variable)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))

  return(df)
}


# Function to extract compliance info from the form
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


# Function to extract spend data from the form
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
    range = "A1:AZ72",
    col_names = FALSE
  )
  
  # Grant amount ----
  grant <- sum(
    as.numeric(
      unlist(
        fin[72, c(2, 15, 28, 41)]
      )
    ),
    na.rm = TRUE
  )
  
  # Block offsets ----
  offsets <- c(0, 13, 26, 39)
  
  # Build all support offer tables ----
  df <- purrr::map_dfr(offsets, function(o) {
    
    support_offer <- as.character(
      unlist(fin[18, 2 + o])
    )
    
    fin[17:72, ] %>%
      transmute(
        spend_cat = .[[1]],
        `Apr 25 - Mar 26 Forecast` = .[[8 + o]],
        `Apr 25 - Mar 26 Actual` = .[[12 + o]],
        `Support offer` = support_offer,
        prov = prov,
        grant_amount = grant,
        group2 = "total cost costs"
      )
    
  }) %>%
    
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

# Function to extract data on individuals supported
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


# Function to extract data on RASASF individuals supported
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


# Function to extract data on presenting experience from the form
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


# Function to extract data on support mode for each support type
f_supp_mode <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- read_xlsx(x, range = "7. Support Type  !B65:O103") %>%
    filter(!is.na(.[[1]])) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    gather("Support type", "Support mode", 1:11)

  df <- as.data.frame(df)

  return(df)
}


# Function to extract data on one-to-one support provision from the form
f_sessions <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- read_xlsx(x, range = "7. Support Type  !B66:O104") %>%
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
    filter(!is.na(PC_category)) %>%
    select(-c(1:3)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name)))


  df$PC <- fillTheBlanks(df$PC_temp)

  df2 <- df %>%
    select(-PC_temp) %>%
    gather("Support type", "Number", 1:11) %>%
    filter(PC_category != "TOTAL") %>%
    numQA()

  df2 <- as.data.frame(df2)
  # Creating a new column to convert response to numeric class
  df2 <- final_clean(df2, 5)

  return(df2)
}


# Function to extract data on outcomes
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


# Function to extract data on additional VFS metrics
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


# Function to extract number of people on wait list by month from the form
f_waits <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- read_xlsx(x, range = "9. Waiting Lists  !K13:R21") %>%
    select(1, 4, 6) %>%
    rename(
      "Date" = 1,
      "Adults" = 2,
      "CYP" = 3
    ) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    gather("Age", "Number", 2:3) %>%
    numQA()

  df <- as.data.frame(df)
  # Creating a new column to convert response to numeric class
  df <- final_clean(df, 4)

  return(df)
}


# Function to extract average days and longest days on their wait list from the form
f_waits2 <- function(x) {
  prov_temp <- read_xlsx(x, range = "2. Organisation  !G4")
  prov_name <- colnames(prov_temp)

  df <- read_xlsx(x, range = "9. Waiting Lists  !K28:Q42") %>%
    filter(!is.na(.[[1]])) %>%
    rename(
      "Support type" = 1,
      "Total" = 2,
      "Closed?" = 3,
      "Closed from" = 4,
      "Closed to" = 5,
      "Average" = 6,
      "Longest" = 7
    ) %>%
    gather("Variable", "Number", c(2, 6, 7)) %>%
    mutate(prov = str_trim(ifelse(grepl("Name of", prov_name) == TRUE, sub("Name of Organisation:\r\n", "", prov_name), prov_name))) %>%
    numQA()

  df <- as.data.frame(df)
  # Creating a new column to convert response to numeric class
  df <- final_clean(df, 6)

  return(df)
}


# Function to pull CORE COSTS from financial report 25-26
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


# FOR LOOPS ------------------------------------------------------------------------

# Setting all the dataframes to NULL before running the below for loops
org_count <- NULL
info <- NULL
staffing <- NULL
comp <- NULL
spend <- NULL
refs_supp <- NULL
RASASF_supp <- NULL
pres_exp <- NULL
supp_mode <- NULL
sessions <- NULL
outcomes <- NULL
add_VFS <- NULL
waitlist1 <- NULL
avg_long_waitlist1 <- NULL
core_costs <- NULL
i <- NULL
df <- NULL
df2 <- NULL
x <- NULL
prov <- NULL
prov_name <- NULL

# For loop to create master tables for RASASF returns
for (i in file.list) {
  a <- f_org_count(i)
  org_count <- rbind(org_count, a)

  a <- f_info(i)
  info <- rbind(info, a)

  a <- f_staffing(i)
  staffing <- rbind(staffing, a)

  a <- f_comp(i)
  comp <- rbind(comp, a)

  a <- f_spend(i)
  spend <- rbind(spend, a)

  a <- f_refs_supp(i)
  refs_supp <- rbind(refs_supp, a)

  a <- f_RASASF_supp(i)
  RASASF_supp <- rbind(RASASF_supp, a)

  a <- f_pres_exp(i)
  pres_exp <- rbind(pres_exp, a)

  a <- f_supp_mode(i)
  supp_mode <- rbind(supp_mode, a)

  a <- f_sessions(i)
  sessions <- rbind(sessions, a)

  a <- f_outcomes(i)
  outcomes <- rbind(outcomes, a)

  a <- f_add_VFS(i)
  add_VFS <- rbind(add_VFS, a)

  a <- f_waits(i)
  waitlist1 <- rbind(waitlist1, a)

  a <- f_waits2(i)
  avg_long_waitlist1 <- rbind(avg_long_waitlist1, a)

  a <- f_core_costs(i)
  core_costs <- rbind(core_costs, a)
}
