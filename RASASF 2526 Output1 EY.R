# INFO ----------------------------------
# Change for different support offers

output_folder <- "R CSV Outputs/"

# Spreading to wide format
info_clean1 <- info %>%
  spread(variable, response)


# COMPLIANCE ------------------------------
write.csv(comp, paste0(output_folder, "Compliance.csv"), row.names = F)

# STAFFING ----------------------------------

# Spreading to wide format
staffing_clean <- staffing %>%
  pivot_wider(
    names_from = variable,
    values_from = response
  )
write.csv(staffing_clean, paste0(output_folder, "Staffing.csv"), row.names = F)

# SPEND -----------------------------------------------

# Grouping the spend data using the overarching groups
spend_grouped_clean <- spend %>%
  group_by(`Support offer`, group2, prov) %>%
  summarise(
    Actual_spend = sum(Actual_spend_num),
    Forecast_spend = sum(Forecast_spend_num)
  ) %>%
  gather("Date", "amount", 4:5)

# Final QA check of grouped spend data
# OUTCOME: Data frame should have 0 observations
spend_grouped_clean_QA <- spend_grouped_clean %>%
  group_by(prov) %>%
  summarise(tot = sum(amount, na.rm = T)) %>%
  filter(tot == 0)

write.csv(spend_grouped_clean, paste0(output_folder, "Spend by overarching group and provider.csv"), row.names = F)


# REFERRALS AND SUPPORTED ---------------------------------------------
# Filtering out rows where text is given or no number is given
refs_supp_clean <- refs_supp %>%
  filter(
    Number_QA != "Text_given",
    Number_QA != "No_number_given"
  ) %>%
  select(-Number, -Number_QA)

# The following dataframes are to backfill those with missing values i.e. each protected characteristic category should be same total
refs_supp_PC <- refs_supp_clean %>%
  filter(
    PC_category != "Intellectual disability",
    PC_category != "Physical disability",
    PC_category != "Sensory disability",
    PC_category != "Mental illness"
  ) %>%
  group_by(prov, Variable, PC) %>%
  summarise(total = sum(Number_num, na.rm = T))

# Working out the max in each group to know how many need backfilling (i.e. if they are below max)
refs_supp_PC_max <- refs_supp_PC %>%
  group_by(prov, Variable) %>%
  summarise(max_total = max(total))

# Calculating the backfill of 'Unknowns' based on the max for that group
refs_supp_PC_unknown <- refs_supp_PC %>%
  left_join(refs_supp_PC_max, by = c(
    "prov" = "prov",
    "Variable" = "Variable"
  )) %>%
  mutate(Number_num = max_total - total) %>%
  mutate(
    Category = case_when(
      grepl("supported", Variable) ~ "Supported victims",
      TRUE ~ "New referrals"
    ),
    PC_category = "Unknown"
  ) %>%
  select(PC_category, prov, PC, Variable, Category, Number_num)

# Binding the original protected characteristics data with the newly calculated unknowns
refs_supp_incl_unknown <- rbind(as.data.frame(refs_supp_clean), as.data.frame(refs_supp_PC_unknown))

write.csv(refs_supp_incl_unknown, paste0(output_folder, "Refs and supported INCL UNKNOWN.csv"), row.names = F)


# NUMBER OF RASASF FUNDED INDIVIDUALS SUPPORTED ---------------
write.csv(RASASF_supp, paste0(output_folder, "RASASF Funding ONLY - Individuals supported.csv"), row.names = F)

# PRESENTING EXPERIENCE ---------------------------------------
# Filtering out rows where text is given or no number is given
pres_exp_clean <- pres_exp %>%
  filter(
    Number_QA != "Text_given",
    Number_QA != "No_number_given"
  ) %>%
  select(-Number, -Number_QA)

# The following dataframes are to backfill those with missing values i.e. each protected characteristic category should be same total
pres_exp_PC <- pres_exp_clean %>%
  filter(
    PC_category != "Intellectual disability",
    PC_category != "Physical disability",
    PC_category != "Sensory disability",
    PC_category != "Mental illness"
  ) %>%
  group_by(prov, `Presenting experience`, PC) %>%
  summarise(total = sum(Number_num, na.rm = T))

# Working out the max in each group to know how many need backfilling (i.e. if they are below max)
pres_exp_PC_max <- pres_exp_PC %>%
  group_by(prov, `Presenting experience`) %>%
  summarise(max_total = max(total))

# Calculating the backfill of 'Unknowns' based on the max for that group
pres_exp_PC_unknown <- pres_exp_PC %>%
  left_join(pres_exp_PC_max, by = c(
    "prov" = "prov",
    "Presenting experience" = "Presenting experience"
  )) %>%
  mutate(
    Number_num = max_total - total,
    PC_category = "Unknown"
  ) %>%
  select(PC_category, prov, PC, `Presenting experience`, Number_num)

# Binding the original protected characteristics data with the newly calculated unknowns
pres_exp_incl_unknown <- rbind(as.data.frame(pres_exp_clean), as.data.frame(pres_exp_PC_unknown))

write.csv(pres_exp_incl_unknown, paste0(output_folder, "Presenting experience INCL UNKNOWN.csv"), row.names = F)


# SUPPORT MODE ---------------------------------------------
write.csv(supp_mode, paste0(output_folder, "Support mode.csv"), row.names = F)


# SESSIONS ----------------------------------------------------
# Filtering out rows where text is given or no number is given
sessions_clean <- sessions %>%
  filter(
    Number_QA != "Text_given",
    Number_QA != "No_number_given"
  ) %>%
  select(-Number, -Number_QA)

# The following dataframes are to backfill those with missing values i.e. each protected characteristic category should be same total
sessions_PC <- sessions_clean %>%
  filter(
    PC_category != "Intellectual disability",
    PC_category != "Physical disability",
    PC_category != "Sensory disability",
    PC_category != "Mental illness"
  ) %>%
  group_by(prov, `Support type`, PC) %>%
  summarise(total = sum(Number_num, na.rm = T))

# Working out the max in each group to know how many need backfilling (i.e. if they are below max)
sessions_PC_max <- sessions_PC %>%
  group_by(prov, `Support type`) %>%
  summarise(max_total = max(total))

# Calculating the backfill of 'Unknowns' based on the max for that group
sessions_PC_unknown <- sessions_PC %>%
  left_join(sessions_PC_max, by = c(
    "prov" = "prov",
    "Support type" = "Support type"
  )) %>%
  mutate(
    Number_num = max_total - total,
    PC_category = "Unknown"
  ) %>%
  select(PC_category, prov, PC, `Support type`, Number_num)

# Binding the original protected characteristics data with the newly calculated unknowns
sessions_incl_unknown <- rbind(as.data.frame(sessions_clean), as.data.frame(sessions_PC_unknown))

write.csv(sessions_incl_unknown, paste0(output_folder, "Sessions INCL UNKNOWN.csv"), row.names = F)


# OUTCOMES -----------------------------------------------------

# Filtering out rows where text is given or no number is given
outcomes_clean <- outcomes %>%
  filter(
    Number_QA != "Text_given",
    Number_QA != "No_number_given"
  ) %>%
  select(-Number, -Number_QA)

# The following dataframes are to backfill those with missing values i.e. each protected characteristic category should be same total
outcomes_PC <- outcomes_clean %>%
  filter(
    PC_category != "Intellectual disability",
    PC_category != "Physical disability",
    PC_category != "Sensory disability",
    PC_category != "Mental illness"
  ) %>%
  group_by(prov, Response, Outcome, PC) %>%
  summarise(total = sum(Number_num, na.rm = T))

# Working out the max in each group to know how many need backfilling (i.e. if they are below max)
outcomes_PC_max <- outcomes_PC %>%
  group_by(prov, Response, Outcome) %>%
  summarise(max_total = max(total))

# Calculating the backfill of 'Unknowns' based on the max for that group
outcomes_PC_unknown <- outcomes_PC %>%
  left_join(outcomes_PC_max, by = c(
    "prov" = "prov",
    "Response" = "Response",
    "Outcome" = "Outcome"
  )) %>%
  mutate(
    Number_num = max_total - total,
    PC_category = "Unknown"
  ) %>%
  select(PC_category, prov, PC, Response, Outcome, Number_num)

# Binding the original protected characteristics data with the newly calculated unknowns
outcomes_incl_unknown <- rbind(as.data.frame(outcomes_clean), as.data.frame(outcomes_PC_unknown))

write.csv(outcomes_incl_unknown, paste0(output_folder, "Outcomes INCL UNKNOWN.csv"), row.names = F)


# ADDITIONAL VFS METRICS ---------------------------------------

# Filtering out rows where text is given or no number is given
add_VFS_clean <- add_VFS %>%
  filter(
    Number_QA != "Text_given",
    Number_QA != "No_number_given"
  ) %>%
  select(-Number, -Number_QA)

# The following dataframes are to backfill those with missing values i.e. each protected characteristic category should be same total
add_VFS_PC <- add_VFS_clean %>%
  filter(
    PC_category != "Intellectual disability",
    PC_category != "Physical disability",
    PC_category != "Sensory disability",
    PC_category != "Mental illness",
    PC_category != "Granular breakdown of disabilities (if someone has more than 1 type of disability please add them as a count into all groups that apply) [hover over each disability category for more info]"
  ) %>%
  group_by(prov, Variable, PC) %>%
  summarise(total = sum(Number_num, na.rm = T))

# Working out the max in each group to know how many need backfilling (i.e. if they are below max)
add_VFS_PC_max <- add_VFS_PC %>%
  group_by(prov, Variable) %>%
  summarise(max_total = max(total))

# Calculating the backfill of 'Unknowns' based on the max for that group
add_VFS_PC_unknown <- add_VFS_PC %>%
  left_join(add_VFS_PC_max, by = c(
    "prov" = "prov",
    "Variable" = "Variable"
  )) %>%
  mutate(
    Number_num = max_total - total,
    PC_category = "Unknown"
  ) %>%
  select(PC_category, prov, PC, Variable, Number_num)

# Binding the original protected characteristics data with the newly calculated unknowns
add_VFS_incl_unknown <- rbind(as.data.frame(add_VFS_clean), as.data.frame(add_VFS_PC_unknown))

write.csv(add_VFS_incl_unknown, paste0(output_folder, "Additional VFS metrics INCL UNKNOWN.csv"), row.names = F)


# WAITLISTS ---------------------------------------

write.csv(avg_long_waitlist1, paste0(output_folder, "Average vs longest waitlist.csv"), row.names = F)

write.csv(waitlist1, paste0(output_folder, "Waitlist by month and provider.csv"), row.names = F)

# CORE COSTS --------------------------------------

write.csv(core_costs, paste0(output_folder, "Core costs.csv"), row.names = F)
