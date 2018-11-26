# ================================================================================
# Build Your Own: Navy
# Designed and built by Gabriel Coll and Loren Lipsey
# --------------------------------------------------------------------------------
# app functions
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(tidyverse)
library(forcats)

# ================================================================================
# update_current_ship

update_current_ship <- function(
  
  # note: returns the updated vector of ship counts, given the user's choice of a
  # ...new level for 2046
  # ...ship_name: character string giving ship name
  # ...input: shiny input object
  # ...navy_data: list of data frames
  
  ship_name,
  input,
  navy_data,
  session = getDefaultReactiveDomain()) {
  input_name <- gsub(" ", "_", ship_name)
  new_number <- input[[input_name]]
  orig_number <- navy_data[["Fleet Plan"]][nrow(navy_data[["Fleet Plan"]]),
                                           ship_name]
  
  if (new_number == orig_number)
    return(navy_data[["Fleet Plan"]][, ship_name])
  
  if (new_number > orig_number) {
    new_builds <- new_number - orig_number
    achieved <-
      min(which(navy_data[["Max New Builds"]][, ship_name] >= new_builds))
    
    new_yearly_quantities <-
      navy_data[["Fleet Plan"]][, ship_name] + new_builds
    new_yearly_quantities[1:achieved] <-
      navy_data[["Fleet Plan"]][1:achieved, ship_name] +
      navy_data[["Max New Builds"]][1:achieved, ship_name]
    new_yearly_quantities[achieved] <-
      navy_data[["Fleet Plan"]][achieved, ship_name] + new_builds
    
    return(new_yearly_quantities)
  }
  
  if (new_number < orig_number) {
    cut_builds <- orig_number - new_number
    achieved <-
      min(which(navy_data[["Max Cut Builds"]][, ship_name] >= cut_builds))
    
    new_yearly_quantities <-
      navy_data[["Fleet Plan"]][, ship_name] - cut_builds
    new_yearly_quantities[1:achieved] <-
      navy_data[["Fleet Plan"]][1:achieved, ship_name] -
      navy_data[["Max Cut Builds"]][1:achieved, ship_name]
    new_yearly_quantities[achieved] <-
      navy_data[["Fleet Plan"]][achieved, ship_name] - cut_builds
    
    return(new_yearly_quantities)
  }
  
}

# ================================================================================
# add_diigtheme

# note: a ggplot object to add the theme to

add_diigtheme <- function(passed_plot) {
  themed_plot <- passed_plot +
    theme(
      plot.title = element_text(
        family = "Open Sans",
        color = "#554449",
        size = 18,
        face = "bold",
        hjust = .5
      ),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#FCFCFC"),
      plot.background = element_rect(fill = "#FCFCFC", colour = "#FCFCFC"),
      legend.background = element_rect(fill = "#FCFCFC", colour = "#FCFCFC"),
      panel.grid.major.x = element_line(size = .1, color = "grey80"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size = .1, color = "grey80"),
      panel.grid.minor.y = element_line(size = .1, color = "grey80"),
      legend.text = element_text(
        size = 12,
        family = "Open Sans",
        color = "#554449"
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
      legend.key.width = unit(2, "line"),
      axis.text.x = element_text(
        size = 12,
        color = "#554449",
        family = "Open Sans",
        margin = margin(0, 0, 0, 0),
        angle = 0
      ),
      axis.ticks.length = unit(.00, "cm"),
      axis.text.y = element_text(
        size = 12,
        color = "#554449",
        family = "Open Sans",
        margin = margin(0, 5, 0, 0)
      ),
      axis.title.x = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans",
        margin = margin(15, 0, 0, 0)
      ),
      axis.title.y = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans",
        margin = margin(0, 15, 0, 0)
      ),
      legend.direction = "horizontal"
    )
  
  return(themed_plot)
}

# ================================================================================
# find_budget_y_limits

# returns a vector for y-axis plot limits: c(minimum, maximum)
# note: a budget data frame, formatted for plotting

find_budget_y_limits <- function(budget_data) {
  datamax <- budget_data %>%
    filter(budget_cat == "Added O&S" |
             budget_cat == "Added Acq") %>%
    group_by(FY) %>%
    mutate(Total = sum(Total)) %>%
    ungroup %>%
    summarize(datamax = max(Total)) %>%
    unlist
  datamin <- budget_data %>%
    filter(budget_cat == "Cut O&S" | budget_cat == "Cut Acq") %>%
    group_by(FY) %>%
    mutate(Total = sum(Total)) %>%
    ungroup %>%
    summarize(datamin = min(Total)) %>%
    unlist
  ymax <- (ceiling((datamax + 1) / 10000000000) * 10000000000)
  ymin <- (floor((datamin) / 10000000000) * 10000000000)
  
  return(c(ymin, ymax))
}

# ================================================================================
# get_ships_data

# note: current_ships, current ships dataframe
# ...input: shiny input object

get_ships_data <- function(current_ships,
                           navy_data,
                           ship_stats,
                           input,
                           session = getDefaultReactiveDomain()) {
  baseline <- navy_data[["Fleet Plan"]]
  new_plan <- current_ships
  
  # note: multiply by some other category than ships, if the user asked for it
  
  if (input$top_y != "Ships") {
    for (i in 1:length(ship_stats[[input$top_y]])) {
      baseline[, i + 1] <-
        round(baseline[, i + 1] * ship_stats[[input$top_y]][, i])
      new_plan[, i + 1] <-
        round(new_plan[, i + 1] * ship_stats[[input$top_y]][, i])
    }
  }
  
  baseline$Total <- rowSums(baseline[, -1])
  baseline$Category <- "old plan ships"
  new_plan$Total <- rowSums(new_plan[, -1])
  new_plan$Category <- "new plan ships"
  return(bind_rows(baseline, new_plan))
  
}

# ================================================================================
# get_budget_change_data

# note: returns a tibble in long form for the budget plot
# current_ships: tibble of current ships
# navy_data: list of data frames

get_budget_change_data <- function(current_ships,
                                   navy_data,
                                   session = getDefaultReactiveDomain()) {
  ship_change <- navy_data[["Fleet Plan"]]
  ship_change[-1] <- current_ships[-1] - ship_change[-1]
  
  # added O&S
  added_os <- ship_change
  
  # start count o&s in the year after ship built
  added_os$FY <- added_os$FY + 1
  added_os <- bind_rows(filter(navy_data[["Blank Sheet"]], FY == min(FY)),
                        filter(added_os, FY != max(FY)))
  
  added_os[-1] <- added_os[-1] * navy_data[["O&S Yearly"]][-1]
  added_os[-1][added_os[-1] < 0] <- 0
  added_os$Total <- rowSums(added_os[-1])
  added_os$budget_cat <- "Added O&S"
  
  # cut O&S
  cut_os <- ship_change
  
  # start count o&s in the year after ship built
  cut_os$FY <- cut_os$FY + 1
  cut_os <- bind_rows(filter(navy_data[["Blank Sheet"]], FY == min(FY)),
                      filter(cut_os, FY != max(FY)))
  
  cut_os[-1] <- cut_os[-1] * navy_data[["O&S Yearly"]][-1]
  cut_os[-1][cut_os[-1] > 0] <- 0
  cut_os$Total <- rowSums(cut_os[-1])
  cut_os$budget_cat <- "Cut O&S"
  
  # ================================================================================
  # get_added_acq
  
  get_added_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] > 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == max(ship_change[[shipname]]))[1]
    
    added_acq <-
      rep(0.0, times = length(navy_data[["Max New Acq"]][[shipname]]))
    
    added_acq[1:(final_yr)] <-
      navy_data[["Max New Acq"]][[shipname]][1:(final_yr)]
    
    if (ship_change[[shipname]][final_yr] <
        navy_data[["Sum Free Capacity"]][[shipname]][final_yr]) {
      added_acq[final_yr] <- added_acq[final_yr] -
        (navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (navy_data[["Sum Free Capacity"]][[shipname]][final_yr] -
              ship_change[[shipname]][final_yr]))
    }
    
    return(added_acq)
  }
  
  added_acq <- ship_change
  added_acq[-1] <- sapply(names(added_acq[-1]), get_added_acq)
  added_acq$Total <- rowSums(added_acq[-1])
  added_acq$budget_cat <- "Added Acq"
  
  # ================================================================================
  # get_cut_acq
  
  get_cut_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] < 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == min(ship_change[[shipname]]))[1]
    
    cut_acq <-
      rep(0.0, times = length(navy_data[["Max Cut Acq"]][[shipname]]))
    
    cut_acq[1:(final_yr)] <-
      -1 * (navy_data[["Max Cut Acq"]][[shipname]][1:(final_yr)])
    
    if (-1 * ship_change[[shipname]][final_yr] <
        navy_data[["Sum Planned Work"]][[shipname]][final_yr]) {
      cut_acq[final_yr] <- cut_acq[final_yr] +
        (navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (navy_data[["Sum Planned Work"]][[shipname]][final_yr] +
              ship_change[[shipname]][final_yr]))
    }
    
    return(cut_acq)
  }
  
  cut_acq <- ship_change
  cut_acq[-1] <- sapply(names(cut_acq[-1]), get_cut_acq)
  cut_acq$Total <- rowSums(cut_acq[-1])
  cut_acq$budget_cat <- "Cut Acq"
  
  return(bind_rows(added_acq, added_os, cut_acq, cut_os))
}

# ================================================================================
# get_budget_total_data

# note: returns a tibble in long form for the budget plot
# ...current_ships: tibble of current ships
# navy_data: list of data frames

get_budget_total_data <- function(current_ships,
                                  navy_data,
                                  session = getDefaultReactiveDomain()) {
  ship_total <- current_ships
  ship_change <- navy_data[["Fleet Plan"]]
  ship_change[-1] <- current_ships[-1] - ship_change[-1]
  
  # O&S
  os <- ship_total
  
  # start count o&s in the year after ship built
  os$FY <- os$FY + 1
  os <- bind_rows(filter(ship_total, FY == min(FY)),
                  filter(os, FY != max(FY)))
  
  os[-1] <- os[-1] * navy_data[["O&S Yearly"]][-1]
  os$Total <- rowSums(os[-1])
  os$budget_cat <- "O&S"
  
  # ================================================================================
  # get_added_acq
  
  get_added_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] > 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == max(ship_change[[shipname]]))[1]
    
    added_acq <-
      rep(0.0, times = length(navy_data[["Max New Acq"]][[shipname]]))
    
    added_acq[1:(final_yr)] <-
      navy_data[["Max New Acq"]][[shipname]][1:(final_yr)]
    
    if (ship_change[[shipname]][final_yr] <
        navy_data[["Sum Free Capacity"]][[shipname]][final_yr]) {
      added_acq[final_yr] <- added_acq[final_yr] -
        (navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (navy_data[["Sum Free Capacity"]][[shipname]][final_yr] -
              ship_change[[shipname]][final_yr]))
    }
    
    return(added_acq)
  }
  
  acq <- navy_data[["Planned Work"]]
  acq[-1] <- sapply(names(acq[-1]), get_added_acq)
  acq[-1] <- acq[-1] +
    (navy_data[["Planned Work"]][-1] * navy_data[["Acquisition Yearly"]][-1])
  
  # ================================================================================
  # get_cut_acq
  
  get_cut_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] < 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == min(ship_change[[shipname]]))[1]
    
    cut_acq <-
      rep(0.0, times = length(navy_data[["Max Cut Acq"]][[shipname]]))
    
    cut_acq[1:(final_yr)] <-
      -1 * (navy_data[["Max Cut Acq"]][[shipname]][1:(final_yr)])
    
    if (-1 * ship_change[[shipname]][final_yr] <
        navy_data[["Sum Planned Work"]][[shipname]][final_yr]) {
      cut_acq[final_yr] <- cut_acq[final_yr] +
        (navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (navy_data[["Sum Planned Work"]][[shipname]][final_yr] +
              ship_change[[shipname]][final_yr]))
    }
    
    return(cut_acq)
  }
  
  cut_acq <- navy_data[["Planned Work"]]
  cut_acq[-1] <- sapply(names(cut_acq[-1]), get_cut_acq)
  acq[-1] <- acq[-1] + cut_acq[-1]
  acq$Total <- rowSums(acq[-1])
  acq$budget_cat <- "Acquisition"
  
  
  return(bind_rows(acq, os))
}

# ================================================================================
# deflate_frame

# note: data frame to be deflated

deflate_frame <- function(frame) {
  deflate <-
    c(
      "2017" = 1.02050053248136,
      "2018" = 1.04233226837061,
      "2019" = 1.065406461,
      "2020" = 1.089368122,
      "2021" = 1.113862265,
      "2022" = 1.138907152,
      "2023" = 1.164515166,
      "2024" = 1.190698969,
      "2025" = 1.217471508,
      "2026" = 1.24484602,
      "2027" = 1.272836041,
      "2028" = 1.301455409,
      "2029" = 1.330718276,
      "2030" = 1.360639111,
      "2031" = 1.391232707,
      "2032" = 1.422514192,
      "2033" = 1.454499032,
      "2034" = 1.487203043,
      "2035" = 1.520642395,
      "2036" = 1.554833621,
      "2037" = 1.589793627,
      "2038" = 1.6255397,
      "2039" = 1.662089513,
      "2040" = 1.699461139,
      "2041" = 1.737673055,
      "2042" = 1.776744156,
      "2043" = 1.81669376,
      "2044" = 1.85754162,
      "2045" = 1.899307932,
      "2046" = 1.942013349
    )
  
  deflated_frame <- as_tibble(sapply(names(frame), function(var_name) {
    if (var_name == "FY" |
        var_name == "Category" | var_name == "budget_cat") {
      return(frame[[var_name]])
    }
    return(round(frame[[var_name]] / deflate[as.character(frame[["FY"]])]))
  })
  ,
  validate = FALSE)
  deflated_frame[,
                 which(!(names(deflated_frame) %in% c("Category", "budget_cat")))] %<>% sapply(as.numeric)
  
  return(deflated_frame)
}

# ================================================================================
# format_ylab

# note: formats the y-axis labels for the ship plot
# ...x: a vector of axis labels, as numeric

format_ylab <- function(x) {
  find_one <- function(x) {
    if (is.na(x))
      return(NULL)
    if (abs(x) < 1)
      return(as.character(round(x, 3)))
    if (abs(x) < 10)
      return(as.character(round(x, 2)))
    if (abs(x) < 100)
      return(as.character(round(x, 1)))
    if (abs(x) < 1000)
      return(as.character(round(x)))
    if (abs(x) < 1e4)
      return(paste0(as.character(round(x / 1e3, 2)), "k"))
    if (abs(x) < 1e5)
      return(paste0(as.character(round(x / 1e3, 1)), "k"))
    if (abs(x) < 1e6)
      return(paste0(as.character(round(x / 1e3)), "k"))
    if (abs(x) < 1e7)
      return(paste0(as.character(round(x / 1e6, 2)), "M"))
    if (abs(x) < 1e8)
      return(paste0(as.character(round(x / 1e6, 1)), "M"))
    if (abs(x) < 1e9)
      return(paste0(as.character(round(x / 1e6)), "M"))
    if (abs(x) < 1e10)
      return(paste0(as.character(round(x / 1e9, 2)), "B"))
    if (abs(x) < 1e11)
      return(paste0(as.character(round(x / 1e9, 1)), "B"))
    if (abs(x) < 1e12)
      return(paste0(as.character(round(x / 1e9)), "B"))
    if (abs(x) < 1e13)
      return(paste0(as.character(round(x / 1e12, 2)), "T"))
    if (abs(x) < 1e14)
      return(paste0(as.character(round(x / 1e12, 1)), "T"))
    if (abs(x) < 1e15)
      return(paste0(as.character(round(x / 1e12)), "T"))
    return(x)
  }
  return(sapply(x, find_one))
  return(x)
}

# ================================================================================
# format_ylab2

# note: formats the y-axis labels for the ship plot
# ...x: a vector of axis labels, as numeric

format_ylab2 <- function(x) {
  find_one <- function(x) {
    if (is.na(x))
      return(NULL)
    if (abs(x) < 1)
      return(as.character(round(x, 3)))
    if (abs(x) < 10)
      return(as.character(round(x, 2)))
    if (abs(x) < 100)
      return(as.character(round(x, 1)))
    if (abs(x) < 1000)
      return(as.character(round(x)))
    if (abs(x) < 1e4)
      return(paste0("$", as.character(round(x / 1e3, 2)), "k"))
    if (abs(x) < 1e5)
      return(paste0("$", as.character(round(x / 1e3, 1)), "k"))
    if (abs(x) < 1e6)
      return(paste0("$", as.character(round(x / 1e3)), "k"))
    if (abs(x) < 1e7)
      return(paste0("$", as.character(round(x / 1e6, 2)), "M"))
    if (abs(x) < 1e8)
      return(paste0("$", as.character(round(x / 1e6, 1)), "M"))
    if (abs(x) < 1e9)
      return(paste0("$", as.character(round(x / 1e6)), "M"))
    if (abs(x) < 1e10)
      return(paste0("$", as.character(round(x / 1e9, 2)), "B"))
    if (abs(x) < 1e11)
      return(paste0("$", as.character(round(x / 1e9, 1)), "B"))
    if (abs(x) < 1e12)
      return(paste0("$", as.character(round(x / 1e9)), "B"))
    if (abs(x) < 1e13)
      return(paste0("$", as.character(round(x / 1e12, 2)), "T"))
    if (abs(x) < 1e14)
      return(paste0("$", as.character(round(x / 1e12, 1)), "T"))
    if (abs(x) < 1e15)
      return(paste0("$", as.character(round(x / 1e12)), "T"))
    return(x)
  }
  return(sapply(x, find_one))
  return(x)
}

# ================================================================================
# create_change_tip

# note: returns a html-formatted string to use in the budget change chart tooltip
# hover_year: year the mouse is over
# budget_dataset: current budget data

create_change_tip <- function(hover_year,
                              budget_dataset,
                              session = getDefaultReactiveDomain()) {
  this_year <- filter(budget_dataset, FY == hover_year)
  until_now <- filter(budget_dataset, FY <= hover_year)
  first_year <- min(until_now$FY)
  
  tip <- paste0(
    "<div align = 'left'>",
    "<b><u>Budget in FY",
    hover_year,
    ":</b></u><br>",
    "<b> ",
    hover_year,
    " Net: $</b>",
    round(summarize(this_year, sum(Total)) / 1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Net: $</b>",
    round(summarize(until_now, sum(Total)) / 1e9, 2),
    "B<br/>",
    br(),
    
    "<b> ",
    hover_year,
    " Added Acq: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Added Acq")]
          / 1e9, 2),
    "B<br/>",
    "<b> ",
    hover_year,
    " Added O&S: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Added O&S")]
          / 1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Added Acq: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Added Acq"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Added O&S: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Added O&S"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    br(),
    
    "<b> ",
    hover_year,
    " Cut Acq: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Cut Acq")]
          / 1e9, 2),
    "B<br/>",
    "<b> ",
    hover_year,
    " Cut O&S: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Cut O&S")]
          / 1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Cut Acq: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Cut Acq"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Cut O&S: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Cut O&S"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    "</div>"
  )
  
  return(tip)
}

# ================================================================================
# create_total_tip

# note: returns a html-formatted string to use in the budget total chart tooltip
# hover_year: year the mouse is over
# budget_dataset: current budget data

create_total_tip <- function(hover_year,
                             budget_dataset,
                             session = getDefaultReactiveDomain()) {
  this_year <- filter(budget_dataset, FY == hover_year)
  until_now <- filter(budget_dataset, FY <= hover_year)
  first_year <- min(until_now$FY)
  
  tip <- paste0(
    "<div align = 'left'>",
    "<b><u>Budget in FY",
    hover_year,
    ":</b></u><br>",
    "<b> ",
    hover_year,
    " Net: $</b>",
    round(summarize(this_year, sum(Total)) / 1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Net: $</b>",
    round(summarize(until_now, sum(Total)) / 1e9, 2),
    "B<br/>",
    br(),
    
    "<b> ",
    hover_year,
    " Acquisition: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Acquisition")]
          / 1e9, 2),
    "B<br/>",
    "<b> ",
    hover_year,
    " O&S: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "O&S")]
          / 1e9, 2),
    "B<br/>",
    br(),
    
    "<b> ",
    first_year,
    "-",
    hover_year,
    " Acquisition: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Acquisition"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    "<b> ",
    first_year,
    "-",
    hover_year,
    " O&S: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "O&S"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    "</div>"
  )
  
  return(tip)
}

# ================================================================================