# Sets the number to the total numbers of providers you're expecting from.
num <- 1

# Spend QA --------------------------------------

# Checking every provider has the relevant groups for spend
# OUTCOME: Data frame should have 0 observations
spend_QA <- spend %>%
  group_by(group2, prov) %>%
  summarise(count = n()) %>%
  spread(group2, count) %>%
  filter_all(any_vars(is.na(.)))

# Setting up check for missing data in spend data
rule <- validator(
  !is.na(spend_cat),
  !is.na(prov),
  !is.na(grant_amount)
)

# Check for missing data in spend
# OUTCOME: All numbers in 'fails' column should be 0
summary(confront(spend, rule))

# Checking all providers sent in spend
# OUTCOME: Should state 'TRUE' in console
n_distinct(spend$prov) == num

# Checking how many spend entries do NOT have an expenditure item name
# OUTCOME: Data frame should have 0 observations
spend_QA2 <- spend %>%
  filter(is.na(spend_cat))

# Checking for missing/wrong class data
spend_QA3 <- spend %>%
  filter(Actual_spend_QA %in% c("Text_given", "No_number_given") |
    Forecast_spend_QA %in% c("Text_given", "No_number_given"))

# Info QA -------------------------------------

# Checking every provider has the relevant groups for info
# OUTCOME: Data frame should have 0 observations
info_QA <- info %>%
  group_by(variable, prov) %>%
  summarise(count = n()) %>%
  spread(variable, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in info data
# OUTCOME: Should state 'TRUE' in console
n_distinct(info$prov) == num

# Checking for missing data
info_QA2 <- info %>%
  filter(is.na(response) | response == "")

# Staffing QA ------------------------------

# Checking every provider has the relevant groups for staffing
# OUTCOME: Data frame should have 0 observations
staffing_QA <- staffing %>%
  group_by(variable, prov) %>%
  summarise(count = n()) %>%
  spread(variable, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in staffing data
# OUTCOME: Should state 'TRUE' in console
n_distinct(staffing$prov) == num

# Checking for missing data
staffing_QA2 <- staffing %>%
  filter(is.na(response) | response == "")

# Compliance QA ----------------------------------------

# Checking every provider has the relevant groups for compliance
# OUTCOME: Data frame should have 0 observations
comp_QA <- comp %>%
  group_by(variable, prov) %>%
  summarise(count = n()) %>%
  spread(variable, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in compliance data
# OUTCOME: Should state 'TRUE' in console
n_distinct(comp$prov) == num

# Checking all said 'Yes' to compliance checks
# OUTCOME: Data frame should have 0 observations
comp_QA2 <- comp %>%
  filter(response != "Yes")

# Individuals Supported QA -------------------------------

# Checking every provider has the relevant groups for individuals supported
# OUTCOME: Data frame should have 0 observations
refs_supp_QA <- refs_supp %>%
  group_by(Variable, prov) %>%
  summarise(count = n()) %>%
  spread(Variable, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in individuals supported data
# OUTCOME: Should state 'TRUE' in console
n_distinct(refs_supp$prov) == num

# Checking that all providers have at least one number submitted in this section
# OUTCOME: Data frame should have 0 observations
refs_supp_QA2 <- refs_supp %>%
  group_by(Category, prov) %>%
  summarise(sum = sum(Number_num)) %>%
  filter(sum == 0)

# Checking QA column to see if there's any data where text was given or no number was given
refs_supp_QA3 <- refs_supp %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))


# Checking all providers have given the number supported by RASASF funding
n_distinct(RASASF_supp$prov) == num

# Checking QA column to see if there's any data where text was given or no number was given
RASASF_supp_QA <- RASASF_supp %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))

# Presenting experience QA -------------------------------------------

# Checking every provider has the relevant groups for presenting experience
# OUTCOME: Data frame should have 0 observations
pres_exp_QA <- pres_exp %>%
  group_by(`Presenting experience`, prov) %>%
  summarise(count = n()) %>%
  spread(`Presenting experience`, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all 75 providers sent in presenting experience data
# OUTCOME: Should state 'TRUE' in console
n_distinct(pres_exp$prov) == num

# Checking that all providers have at least one number submitted in this section
# OUTCOME: Data frame should have 0 observations
pres_exp_QA2 <- pres_exp %>%
  group_by(prov) %>%
  summarise(sum = sum(Number_num)) %>%
  filter(sum == 0)

# Checking QA column to see if there's any data where text was given or no number was given
pres_exp_QA3 <- pres_exp %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))


# Support mode QA --------------------------------------------------

# Checking every provider has the relevant groups for support type
# OUTCOME: Data frame should have 0 observations
supp_mode_QA <- supp_mode %>%
  group_by(`Support type`, prov) %>%
  summarise(count = n()) %>%
  spread(`Support type`, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in support mode data
# OUTCOME: Should state 'TRUE' in console
n_distinct(supp_mode$prov) == num

# Checking QA column to see if there's any data where text was given or no number was given
supp_mode_QA2 <- supp_mode %>%
  filter(is.na(`Support mode`) | `Support mode` == "")


# Sessions QA ----------------------------------------

# Checking every provider has the relevant groups for sessions
# OUTCOME: Data frame should have 0 observations
sessions_QA <- sessions %>%
  group_by(`Support type`, prov) %>%
  summarise(count = n()) %>%
  spread(`Support type`, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in sessions data
# OUTCOME: Should state 'TRUE' in console
n_distinct(sessions$prov) == num

# Checking that all providers have at least one number submitted in this section
# OUTCOME: Data frame should have 0 observations
sessions_QA2 <- sessions %>%
  group_by(prov) %>%
  summarise(sum = sum(Number_num)) %>%
  filter(sum == 0)

# Checking for missing/wrong class data
sessions_QA3 <- sessions %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))


# Outcomes QA ---------------------------------------------------

# Checking every provider has the relevant groups for outcomes
# OUTCOME: Data frame should have 0 observations
outcomes_QA <- outcomes %>%
  group_by(Outcome, prov) %>%
  summarise(count = n()) %>%
  spread(Outcome, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in outcomes data
# OUTCOME: Should state 'TRUE' in console
n_distinct(outcomes$prov) == num

# Checking that all providers have at least one number submitted in this section
# OUTCOME: Data frame should have 0 observations
outcomes_QA2 <- outcomes %>%
  group_by(prov) %>%
  summarise(sum = sum(Number_num)) %>%
  filter(sum == 0)

# Checking QA column to see if there's any data where text was given or no number was given
outcomes_QA3 <- outcomes %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))


# Additional VFS metrics QA ---------------------------------------------------

# Checking every provider has the relevant groups for additional VFS metrics
# OUTCOME: Data frame should have 0 observations
add_VFS_QA <- add_VFS %>%
  group_by(Variable, prov) %>%
  summarise(count = n()) %>%
  spread(Variable, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in this data
# OUTCOME: Should state 'TRUE' in console
n_distinct(add_VFS$prov) == num

# Checking that all providers have at least one number submitted in this section
add_VFS_QA2 <- add_VFS %>%
  group_by(prov) %>%
  summarise(sum = sum(Number_num)) %>%
  filter(sum == 0)

# Checking QA column to see if there's any data where text was given or no number was given
add_VFS_QA3 <- add_VFS %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))


# Waitlist data QA --------------------------------------------------

# Checking every provider has the relevant groups for waitlist data
# OUTCOME: Data frame should have 0 observations
waitlist_QA <- waitlist1 %>%
  group_by(Date, prov) %>%
  summarise(count = n()) %>%
  spread(Date, count) %>%
  filter_all(any_vars(is.na(.)))

# Checking all providers sent in waitlist data
# OUTCOME: Should state 'TRUE' in console
n_distinct(waitlist1$prov) == num

# Checking that all providers have at least one number submitted in this section
# OUTCOME: Data frame should have 0 observations
waitlist_QA2 <- waitlist1 %>%
  group_by(prov, Age) %>%
  summarise(sum = sum(Number_num)) %>%
  filter(sum == 0)

# Checking QA column to see if there's any data where text was given or no number was given
waitlist_QA3 <- waitlist1 %>%
  filter(Number_QA %in% c("Text_given", "No_number_given"))


# Average and longest waitlist data QA -----------------------------------------

# Check if they said waitlist was closed that they've given dates
# OUTCOME: Data frame should have 0 observations
avg_long_waitlist_QA <- avg_long_waitlist1 %>%
  # Just choosing one variable to cut down data as closure data is duplicated
  filter(
    Variable == "Total",
    `Closed?` == "Yes" & (is.na(`Closed from`) | is.na(`Closed to`))
  )

# Check if they stated whether the waitlist was open or closed
# OUTCOME: Data frame should have 0 observations
avg_long_waitlist_QA2 <- avg_long_waitlist1 %>%
  # Just choosing one variable to cut down data as closure data is duplicated
  filter(
    Variable == "Total",
    is.na(`Closed?`) | (`Closed from` != "Yes" & `Closed to` != "No")
  )

# Check if they stated a support type for each waiting list (and that it's from dropdown)
# OUTCOME: Data frame should have 0 observations
avg_long_waitlist_QA3 <- avg_long_waitlist1 %>%
  # Just choosing one variable to cut down data as closure data is duplicated
  filter(
    Variable == "Total",
    is.na(`Support type`) | (`Support type` != "Initial assessments/ referral hub" &
      `Support type` != "ISVA services" &
      `Support type` != "Other advocacy services" &
      `Support type` != "Pre-trial therapy" &
      `Support type` != "Therapy & Counselling" &
      `Support type` != "Peer support services" &
      `Support type` != "Other groupwork services" &
      `Support type` != "Telephone" &
      `Support type` != "Email/text/WhatsApp/webchat" &
      `Support type` != "Other social/ online media" &
      `Support type` != "Child and family services")
  )


# Checking for missing/wrong class data
# OUTCOME: Data frame should have 0 observations
avg_long_waitlist_QA4 <- avg_long_waitlist1 %>%
  filter(Number_QA %in% c("No_number_given", "Text_given"))

# Checking all providers sent in waitlist data
# OUTCOME: Should state 'TRUE' in console
n_distinct(avg_long_waitlist1$prov) == num
