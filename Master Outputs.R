# ---------------------------------------------------------------
# 0.  Load what we need
# ---------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)          # brings in readr, dplyr, purrr, stringr

# ---------------------------------------------------------------
# 1.  Define where to look and what to look for
# ---------------------------------------------------------------
target_file  <- "Staffing.csv"          # <-- your common file name
search_roots <- c(                             # <-- any number of roots
  "/home/echatwin4/rasasf_2425"
)

# ---------------------------------------------------------------
# 2.  Hunt for every occurrence of that file name
# ---------------------------------------------------------------
find_csvs <- function(root, filename) {
  list.files(
    path       = root,
    pattern    = paste0("^", stringr::fixed(filename), "$"), # literal match
    recursive  = TRUE,
    full.names = TRUE
  )
}

csv_paths <- search_roots |>
  map(find_csvs, filename = target_file) |>
  unlist(use.names = FALSE)

if (length(csv_paths) == 0) stop("No matching CSVs found. Check the roots or file name.")

# ---------------------------------------------------------------
# 3.  Read and row-bind
# ---------------------------------------------------------------
combined <- map_dfr(
  csv_paths,
  ~ read_csv(.x, show_col_types = FALSE) |>
    mutate(source_path = .x),   # provenance column
  .id = "batch_id"
)

# ---------------------------------------------------------------
# 4.  Write out the master file
# ---------------------------------------------------------------
outfile <- file.path(
  dirname(csv_paths[1]),          # save alongside first match (or pick a path)
  paste0("combined_", target_file)
)

write_csv(combined, outfile)

message("Combined file written to: ", outfile)