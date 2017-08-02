# read_data.R

library(readxl)
library(purrr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

data.raw <- "data/raw/2015-2016"

# get non-Toggl data
cols <- rep("text", 11)

hrs_fy16 <- list.files(data.raw, pattern = "^[Instructor|0-9].*(xlsx)",
                   recursive = TRUE, full.names = TRUE) %>%
    map_df(read_excel, col_names = FALSE, col_types = cols) %>%
    filter(X__1 == "TOTAL") %>%
    select(admin = X__5, precept = X__7, other = X__9) %>%
    mutate_all(as.numeric) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    mutate(User = "preceptors") %>%
    select(User, everything())

jen_fy16 <- list.files(data.raw, pattern = "^[Instructor|0-9].*(Cortes).*(xlsx)",
                  recursive = TRUE, full.names = TRUE) %>%
    map_df(read_excel, col_names = FALSE, col_types = cols) %>%
    filter(X__1 == "TOTAL") %>%
    select(admin = X__5, precept = X__7, other = X__9) %>%
    mutate_all(as.numeric) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    mutate(User = "Jen Cortes") %>%
    select(User, everything())

# get Toggl data
toggl_fy16 <- list.files(data.raw, pattern = ".*(Toggl).*(xlsx)",
                    recursive = TRUE, full.names = TRUE) %>%
    map_df(read_excel) %>%
    filter(str_detect(Project, regex("resid|admin|pgy1", ignore_case = TRUE))) %>%
    mutate(begin = ymd_hms(paste(`Start date`, strftime(`Start time`, format = "%H:%M:%S", tz = "UTC"))),
           end = ymd_hms(paste(`End date`, strftime(`End time`, format = "%H:%M:%S", tz = "UTC"))),
           rec.time = as.numeric(difftime(end, begin, units = "hours")),
           group = if_else(str_detect(Project, regex("admin|timesheet", ignore_case = TRUE)), "admin",
                           if_else(str_detect(Project, regex("supervis|precept", ignore_case = TRUE)), "precept", "other"))) %>%
    group_by(User, group) %>%
    summarize(rec.time = sum(rec.time)) %>%
    spread(group, rec.time, fill = 0)

# combine hours
total_fy16 <- bind_rows(toggl_fy16, hrs_fy16) %>%
    ungroup() %>%
    select(-User) %>%
    summarize_all(sum, na.rm = TRUE)
