# ================================================================================
# Build Your Own: Navy
# Designed and built by Gabriel Coll and Loren Lipsey
# --------------------------------------------------------------------------------
# data processing
# ================================================================================

# --------------------------------------------------------------------------------

# input: navy_350_data.xlsx
# output: navy_data.Rda, ships_stats.Rda 

# --------------------------------------------------------------------------------
# load packages

library(magrittr)
library(tidyverse)
library(openxlsx)

# --------------------------------------------------------------------------------
# load data

navy_book <- loadWorkbook("data/navy_350_data.xlsx")

# --------------------------------------------------------------------------------
# transform data

navy_data <- list()

for (i in 2:length(names(navy_book))) {
  # read sheet
  navy_data[[i - 1]] <- read.xlsx(xlsxFile = navy_book,
                                  sheet = i,
                                  startRow = 3)
  
  # remove 2016
  navy_data[[i - 1]] %<>%
    filter(FY >= 2017)
  
  # turn . back into spaces in the variable names
  names(navy_data[[i - 1]]) <-
    gsub("\\.", " ", names(navy_data[[i - 1]]))
  
  # name the sheet
  names(navy_data)[i - 1] <- names(navy_book)[i]
}

# add blank sheet for use input
navy_data[["Blank Sheet"]] <- navy_data[["Build Plan"]]
navy_data[["Blank Sheet"]][, -1] <- 0

# --------------------------------------------------------------------------------
# save data: navy_data.Rda

save("navy_data", file = "data/navy_data.Rda")

# add ship data for calculations within app
ship_data <- read.xlsx(xlsxFile = navy_book,
                       sheet = 1,
                       startRow = 7)

create_stat_frame <- function(var_name) {
  new_frame <- ship_data %>%
    filter(X1 == var_name) %>%
    select(-X1)
  names(new_frame) <- gsub("\\.", " ", names(new_frame))
  return(new_frame)
}

ship_stats <- list()
for (i in 1:nrow(ship_data)) {
  ship_stats[[i]] <- assign(gsub("\\.", " ", ship_data$X1[i]),
                            create_stat_frame(ship_data$X1[i]))
  
}

names(ship_stats) <- gsub("\\.", " ", ship_data$X1)

# --------------------------------------------------------------------------------
# save data: ship_stats

save("ship_stats", file = "data/ship_stats.Rda")
