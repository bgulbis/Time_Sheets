# read_data.R

library(readxl)
library(purrr)
library(dplyr)
library(lubridate)

data.raw <- "data/raw"

# get non-Toggl data
cols <- rep("text", 11)

docs <- list.files(data.raw, pattern = "^[Instructor|0-9].*(xlsx)",
                   recursive = TRUE, full.names = TRUE) %>%
    map_df(read_excel, col_names = FALSE, col_types = cols) %>%
    filter(X0 == "TOTAL") %>%
    select(admin = X4, precept = X6, other = X8) %>%
    mutate_all(as.numeric)

hrs <- summarize_all(docs, sum, na.rm = TRUE)

# get Toggl data
toggl <- list.files(data.raw, pattern = ".*(Toggl).*(xlsx)",
                    recursive = TRUE, full.names = TRUE) %>%
    map_df(read_excel)
