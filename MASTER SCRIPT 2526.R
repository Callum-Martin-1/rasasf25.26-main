# MASTER SCRIPT --------------------------

# 1. Load in data from S3 Bucket 

#Alter for support offer changes=====================

#source("DownloadSOff_1.R")
#source("DownloadSOff_2.R")
#source("DownloadSOff_3.R")




# 2. Run functions and processing script (CHANGE FOR SUPPORT OFFERS)
source("RASASF2526Processing1.R")

# 3. Run QA script which checks for missing data etc.
source("RASASF 2526 QA EY.R")

# 4. Run outputs scripts
source("RASASF 2526 Output1 EY.R")




