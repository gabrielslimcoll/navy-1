# ================================================================================
# Build Your Own: Navy
# Designed and built by Gabriel Coll and Loren Lipsey
# --------------------------------------------------------------------------------
# server
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(shiny)
library(tidyverse)
library(png)
library(Cairo)
library(forcats)
library(magrittr)
library(shinyjs)
options(scipen = 99)

# --------------------------------------------------------------------------------
# load functions

source("app_functions.R")

# --------------------------------------------------------------------------------
# label fields

fields <- c(
  "Aircraft_Carriers",
  "Large_Surface_Combatants",
  "Small_Surface_Combatants",
  "Attack_Submarines",
  "Ballistic_Missile_Submarines",
  "Amphibious_Assault_Ships",
  "Amphibious_Transport_Docks",
  "Dock_Landing_Ships",
  "Combat_Logistics_Force",
  "Support_Vessels",
  "text",
  "text2"
)

# --------------------------------------------------------------------------------
# begin server section

shinyServer(function(input, output, session) {
  # --------------------------------------------------------------------------------
  # read data and create reactive current$ships frame
  
  load("data/navy_data.Rda")
  load("data/ship_stats.Rda")
  
  current <- reactiveValues(ships = navy_data[["Fleet Plan"]])
  
  # --------------------------------------------------------------------------------
  # update current$ships frame when user changes a slider
  
  observeEvent(input$Aircraft_Carriers, {
    current$ships$`Aircraft Carriers` <-
      update_current_ship("Aircraft Carriers", input, navy_data)
  })
  
  observeEvent(input$Large_Surface_Combatants, {
    current$ships$`Large Surface Combatants` <-
      update_current_ship("Large Surface Combatants", input, navy_data)
  })
  
  observeEvent(input$Small_Surface_Combatants, {
    current$ships$`Small Surface Combatants` <-
      update_current_ship("Small Surface Combatants", input, navy_data)
  })
  
  observeEvent(input$Attack_Submarines, {
    current$ships$`Attack Submarines` <-
      update_current_ship("Attack Submarines", input, navy_data)
  })
  
  observeEvent(input$Ballistic_Missile_Submarines, {
    current$ships$`Ballistic Missile Submarines` <-
      update_current_ship("Ballistic Missile Submarines", input, navy_data)
  })
  
  observeEvent(input$Support_Vessels, {
    current$ships$`Support Vessels` <-
      update_current_ship("Support Vessels", input, navy_data)
  })
  
  observeEvent(input$Combat_Logistics_Force, {
    current$ships$`Combat Logistics Force` <-
      update_current_ship("Combat Logistics Force", input, navy_data)
  })
  
  observeEvent(input$`Amphibious_Assault_Ships`, {
    current$ships$`Amphibious Assault Ships` <-
      update_current_ship("Amphibious Assault Ships", input, navy_data)
  })
  
  observeEvent(input$`Dock_Landing_Ships`, {
    current$ships$`Dock Landing Ships` <-
      update_current_ship("Dock Landing Ships", input, navy_data)
  })
  
  observeEvent(input$`Amphibious_Transport_Docks`, {
    current$ships$`Amphibious Transport Docks` <-
      update_current_ship("Amphibious Transport Docks", input, navy_data)
  })
  
  # --------------------------------------------------------------------------------
  # format data for use by ships plot
  
  ships_dataset <- reactive({
    return(get_ships_data(current$ships, navy_data, ship_stats, input))
  })
  
  # --------------------------------------------------------------------------------
  # create format for y-axis ($_B)
  
  formaty1 <- function(x) {
    x <- gsub("000", "", x)
    x <- gsub("500", ".5", x)
    x <- gsub("250", ".25", x)
    x <- gsub("750", ".75", x)
    paste("$", x, "B", sep = "")
  }
  
  # --------------------------------------------------------------------------------
  # create ships plot
  
  output$ships <- renderPlot({
    ship_plot()
  })
  
  ship_plot <- reactive({
    shown <- ships_dataset()
    
    # deflate if requested and appropriate
    if (input$checkbox == TRUE & input$top_y %in% c("Direct Cost",
                                                    "Indirect Cost",
                                                    "Overhead Cost",
                                                    "Total Cost")) {
      shown %<>% deflate_frame()
    }
    
    # find plot limits
    ymax <- max(c(max(shown$Total), signif(shown$Total, 2)))
    if (input$top_y == "Ships")
      ymax <- max(ymax, 375)
    ymin <- 0
    
    # options unique to line chart
    if (input$bottom_chart == "Change") {
      p <-
        ggplot(data = shown, aes(x = FY, y = Total, color = Category)) +
        geom_line(size = 1) +
        scale_color_manual(
          values = c("#C76363", "#788ca8"),
          labels = c("New Ship Plan ", "Old Ship Plan ")
        )
    }
    
    # options unique to area chart
    if (input$bottom_chart == "Total") {
      shown %<>% filter(Category == "new plan ships") %>%
        select(-Category)
      
      shown <- gather(data = shown,
                      key = "Ship_type",
                      value = "Quantity",-FY,
                      -Total)
      
      # relevel factors
      shown$Ship_type <- factor(
        shown$Ship_type,
        c(
          "Aircraft Carriers",
          "Large Surface Combatants",
          "Small Surface Combatants",
          "Attack Submarines",
          "Ballistic Missile Submarines",
          "Amphibious Assault Ships",
          "Amphibious Transport Docks",
          "Dock Landing Ships",
          "Combat Logistics Force",
          "Support Vessels"
        )
      )
      
      p <-
        ggplot(data = shown, aes(x = FY, y = Quantity, fill = Ship_type)) +
        geom_area() +
        scale_fill_manual(
          values = c(
            "Aircraft Carriers" = "#115175",
            "Large Surface Combatants" = "#0095AB",
            "Small Surface Combatants" = "#66c6cb",
            "Attack Submarines" = "#C74745",
            "Ballistic Missile Submarines" = "#C77373",
            "Amphibious Assault Ships" = "#0a8672",
            "Amphibious Transport Docks" = "#0faa91",
            "Dock Landing Ships" = "#75c596",
            "Combat Logistics Force" = "#566377",
            "Support Vessels" = "#788ca8"
          ),
          labels = c(
            "Aircraft Carriers" = " CVN ",
            "Large Surface Combatants" = " LSC ",
            "Small Surface Combatants" = " SSC ",
            "Attack Submarines" = " SSN ",
            "Ballistic Missile Submarines" = " SSBN/SSGN ",
            "Amphibious Assault Ships" = " LHD/LHA ",
            "Amphibious Transport Docks" = " LPD ",
            "Dock Landing Ships" = " LSD/LXR ",
            "Combat Logistics Force" = " Combat Logistics Force",
            "Support Vessels" = " Support Vessels"
          )
        )
    }
    
    
    # add options common to both views
    p <- p +
      coord_cartesian(ylim = c(ymin, ymax)) +
      ggtitle("U.S. Navy Inventory") +
      scale_y_continuous(labels = (switch(
        input$top_y,
        "Ships" = format_ylab,
        "Tonnage" = format_ylab,
        "VLS Missile Cells" = format_ylab,
        "Tonnage" = format_ylab,
        "VLS Missile Cells" = format_ylab,
        "Aircraft Capacity" = format_ylab,
        "Direct Cost" = format_ylab2,
        "Indirect Cost" = format_ylab2,
        "Overhead Cost" = format_ylab2,
        "Total Cost" = format_ylab2,
        "Direct Personnel" = format_ylab,
        "Indirect Personnel" = format_ylab,
        "Overhead Personnel" = format_ylab,
        "Total Personnel" = format_ylab,
        "Officers" = format_ylab,
        "Enlisted" = format_ylab,
        "Total" = format_ylab
      ))) +
      
      scale_x_continuous(
        breaks = c(2017:2046),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      guides(fill = guide_legend(
        keywidth = 1,
        keyheight = 1,
        nrow = 1 + (input$bottom_chart == "Total"),
        reverse = FALSE
      )) +
      xlab("Fiscal Year") +
      ylab(input$top_y)
    
    if (input$top_y == "Ships") {
      p <- p + geom_hline(
        yintercept = 355,
        color = 'darkgreen',
        size = 0.5,
        linetype = "dashed",
        alpha = .5
      )
    }
    
    p <- add_diigtheme(p)
    
    return(p)
  })
  
  
  
  # ----------------------------------------------------------------------------
  # format data for use by budget plot
  
  budget_dataset <- reactive({
    if (input$bottom_chart == "Change") {
      return(get_budget_change_data(current$ships, navy_data))
    } else if (input$bottom_chart == "Total") {
      return(get_budget_total_data(current$ships, navy_data))
    }
  })
  
  # --------------------------------------------------------------------------------
  # create budget plot
  
  output$budget <- renderPlot({
    budget_plot()
  })
  
  budget_plot <- reactive({
    shown <- budget_dataset()
    
    if (input$checkbox == TRUE) {
      shown %<>% deflate_frame()
    }
    
    # options specific to change chart
    if (input$bottom_chart == "Change") {
      shown$budget_cat <- factor(shown$budget_cat,
                                 levels = c("Added O&S", "Added Acq", "Cut O&S", "Cut Acq"))
      
      p <- ggplot(data = shown, aes(x = FY, y = Total)) +
        geom_area(stat = 'identity',
                  alpha = .80,
                  aes(fill = budget_cat)) +
        ggtitle("Change in Annual Funding for U.S. Navy") +
        scale_fill_manual(
          values = c(
            "Added O&S" = "#115175",
            "Added Acq" = "#0a8672",
            "Cut O&S" = "#788ca8",
            "Cut Acq" = "#0faa91"
          ),
          labels = c(
            "Added O&S" = "Added Operation & Support  ",
            "Added Acq" = "Added Acquisition  ",
            "Cut O&S" = "Decreased Operation & Support  ",
            "Cut Acq" = "Decreased Acquisition  "
          )
        )
      
      # add net change line and y limits
      netdata <- shown %>%
        group_by(FY) %>%
        summarize(Total = sum(Total))
      
      y_limits <- find_budget_y_limits(shown)
      
      p <- p +
        geom_line(data = netdata,
                  aes(color = "Net Change"),
                  size = 1) +
        scale_color_manual(values = "#C76363") +
        coord_cartesian(ylim = c(y_limits[1], y_limits[2]))
    }
    
    # options specific to total chart
    if (input$bottom_chart == "Total") {
      y_limit <- max(shown$Total[shown$budget_cat == "Acquisition"]) +
        max(shown$Total[shown$budget_cat == "O&S"])
      
      if (y_limit > 80e9)
        y_limit <- y_limit * 1.1
      else if (y_limit > 50e9)
        y_limit <- 80e9
      else
        y_limit <- 50e9
      
      p <- ggplot(data = shown, aes(x = FY, y = Total)) +
        geom_area(stat = 'identity',
                  alpha = .80,
                  aes(fill = budget_cat)) +
        ggtitle("Annual Funding for U.S. Navy") +
        scale_fill_manual(
          values = c("O&S" = "#115175",
                     "Acquisition" = "#0a8672"),
          labels = c("O&S" = "Operations & Support  ",
                     "Acquisition" = "Acquisition  ")
        ) +
        coord_cartesian(ylim = c(0, y_limit))
    }
    
    # options applied to both charts
    p <- p +
      scale_y_continuous(labels = formaty1) +
      scale_x_continuous(
        breaks = c(shown$FY[1]:shown$FY[nrow(shown)]),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      guides(fill = guide_legend(
        keywidth = 1,
        keyheight = 1,
        nrow = 1 + (input$bottom_chart == "Change"),
        reverse = FALSE
      )) +
      xlab("Fiscal Year") +
      ylab("Constant FY16 Dollars") +
      
      
      theme(plot.caption = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans"
      )) +
      labs(caption = "Source: U.S. Navy; CBO; GAO; CSIS analysis",
           size = 30,
           family = "Open Sans")
    
    p <- add_diigtheme(p)
    
    return(p)
  })
  
  # --------------------------------------------------------------------------------
  # hover feature for ships plot
  
  output$hover_info_ships <- renderUI({
    hover <- input$plot_hover_ships
    shown <- ships_dataset()
    
    if (is.null(hover))
      return(NULL)
    hover_year <- round(hover$x)
    hover_year_index <- which(shown$FY == hover_year)[1]
    
    hover_ships_new <- shown %>%
      filter(Category == "new plan ships") %>%
      select(Total) %>%
      slice(hover_year_index) %>%
      unlist
    
    if (hover$y > 1.08 * hover_ships_new &
        input$bottom_chart == "Total") {
      return(NULL)
    }
    
    hover_ships_old <- shown %>%
      select(Total) %>%
      slice(hover_year_index) %>%
      unlist
    
    if (hover$y < 0.93 * min(hover_ships_new, hover_ships_old) &
        input$bottom_chart == "Change") {
      return(NULL)
    }
    if (hover$y > 1.08 * max(hover_ships_new, hover_ships_old) &
        input$bottom_chart == "Change") {
      return(NULL)
    }
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    # Use HTML/CSS to change style of tooltip panel here
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )
    
    new_plan <- shown %>%
      filter(FY == hover_year & Category == "new plan ships") %>%
      select(-FY,-Total,-Category)
    
    tooltip_string <- HTML(paste0(sapply(names(new_plan),
                                         function(shipname) {
                                           return(paste0("<i>", shipname, ": </i>",
                                                         as.character(
                                                           prettyNum(new_plan[[shipname]], big.mark  = ",")
                                                         )))
                                           
                                         }),
                                  collapse = "<br>"))
    
    wellPanel(style = style,
              p(HTML(
                paste0(
                  "<div align = 'left'>",
                  "<b><u>",
                  input$top_y,
                  " in FY",
                  hover_year,
                  ":</b></u><br>",
                  "<b> Old Plan: </b>",
                  prettyNum(hover_ships_old, big.mark = ","),
                  br(),
                  "<b> New Plan: </b>",
                  prettyNum(hover_ships_new, big.mark = ","),
                  br(),
                  br(),
                  tooltip_string
                )
              )))
  })
  
  
  # --------------------------------------------------------------------------------
  # hover feature for budget plot
  
  output$hover_info_budget <- renderUI({
    hover <- input$plot_hover_budget
    
    if (is.null(hover)) {
      return()
    }
    
    hover_year <- round(hover$x)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    
    # Use HTML/CSS to change style of tooltip panel here
    
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )
    
    if (input$bottom_chart == "Change") {
      wellPanel(style = style,
                p(HTML(
                  create_change_tip(hover_year, budget_dataset())
                )))
    } else if (input$bottom_chart == "Total") {
      wellPanel(style = style,
                p(HTML(
                  create_total_tip(hover_year, budget_dataset())
                )))
    }
    
  })
  
  # --------------------------------------------------------------------------------
  # download button
  
  output$CSVDownloadBtn <- downloadHandler(filename = paste0('Basic.Chart', '.csv'),
                                           content <- function(file) {
                                             writedata <- full_join(ships_dataset(), budget_dataset())
                                             colnames(writedata) <- c(
                                               "FY",
                                               "Aircraft Carriers",
                                               "Large Surface Combatants",
                                               "Small Surface Combatants",
                                               "Attack Submarines",
                                               "Ballistic Missile Submarines",
                                               "Support Vessels",
                                               "Combat Logistics Force",
                                               "Amphibious Assault Ships",
                                               "Amphibious Transport Docks",
                                               "Dock Landing Ships",
                                               "Total",
                                               "Ship Plan",
                                               "Budget Category"
                                             )
                                             write_csv(writedata, file)
                                           })
  
  # --------------------------------------------------------------------------------
  # popovers for slider bars
  
  addPopover(
    session,
    "Aircraft_Carriersb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Aircraft Carrier </b>",
        "<br/>",
        "<br/>",
        "<img src='CVN.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> Ford class carrier </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$10.64B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$1.18B",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "0.4",
        "<br/>",
        "<b> Length:  </b>",
        "1,092 ft",
        "<br/>",
        "<b> Beam: </b>",
        "134 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "100,000 tons",
        "<br/>",
        "<b> Speed: </b>",
        "30 knots",
        "<br/>",
        "<b> Crew: </b>",
        "~4,539 (537 officers, 4,002 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Aircraft carriers </b>",
        "are the heart of the battle force. Each carries an air wing of about 60 aircraft, which can attack hundreds of targets per day for up to a month before needing to rest. Carriers are the largest ships in the fleet and provide unique capabilities for disaster response and humanitarian assistance.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Large_Surface_Combatantsb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Large Surface Combatant </b>",
        "<br/>",
        "<br/>",
        "<img src='LSC.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> Burke class destroyer </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$1.81B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$140M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "3",
        "<br/>",
        "<b> Length:  </b>",
        "507 ft",
        "<br/>",
        "<b> Beam: </b>",
        "59 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "9,700 tons",
        "<br/>",
        "<b> Speed: </b>",
        "30+ knots",
        "<br/>",
        "<b> Crew: </b>",
        "329 (32 officers, 297 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Large surface comtants</b>",
        ", which include cruisers and destroyers, are the workhorses of the fleet. They provide ballistic missile defense for the fleet and for overseas regions. They defend aircraft carriers and amphibious warfare ships against other surface ships, aircraft, and submarines, and they perform such day-to-day missions as patrolling sea lanes, providing an overseas presence, and conducting exercises with allies. They can also launch Tomahawk missiles to strike land targets. Most of the Navy's surface combatants displace about 9,000 to 10,000 tons.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Small_Surface_Combatantsb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Small Surface Combatant </b>",
        "<br/>",
        "<br/>",
        "<img src='SSC.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> littoral combat ship </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$667M",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$90M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "5",
        "<br/>",
        "<b> Length:  </b>",
        "422 ft",
        "<br/>",
        "<b> Beam: </b>",
        "59 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "3,150 tons",
        "<br/>",
        "<b> Speed: </b>",
        "40+ knots",
        "<br/>",
        "<b> Crew: </b>",
        "50 (10 officers, 40 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Small surface combatants </b>",
        "include littoral combat ships (LCSs) and frigates. LCSs are intended to counter mines, small boats, and diesel-electric submarines in the world's coastal regions. The Navy's new frigates, which are based on the LCS but have enhanced capabilities, will perform similar missions but also include antiship capabilities. More routinely, LCSs and frigates-like their counterparts, the large surface combatants-patrol sea lanes, provide an overseas presence, and conduct exercises with allies. They range in size from 3,000 to 4,000 tons.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Attack_Submarinesb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Attack Submarine </b>",
        "<br/>",
        "<br/>",
        "<img src='SSN.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> Virginia class submarine </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$2.75B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$140M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "4",
        "<br/>",
        "<b> Length:  </b>",
        "377 ft",
        "<br/>",
        "<b> Beam: </b>",
        "34 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "7,800 tons",
        "<br/>",
        "<b> Speed: </b>",
        "25+ knots",
        "<br/>",
        "<b> Crew: </b>",
        "132 (15 officers, 117 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Attack submarines </b>",
        "are the Navy's premier undersea warfare and antisubmarine weapons. Since the end of the Cold War, however, they have mainly been used for covert intelligence gathering. They can also launch Tomahawk missiles at inland targets in the early stages of a conflict. Of the Navy's 51 attack submarines, 36 belong to the Los Angeles class. Displacing 7,000 tons when submerged, they are less than half the size of ballistic missile submarines.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Ballistic_Missile_Submarinesb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Ballistic Missile Submarine </b>",
        "<br/>",
        "<br/>",
        "<img src='SSBN.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> Ohio class submarine </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$7.26B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$160M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "1",
        "<br/>",
        "<b> Length:  </b>",
        "~560 ft",
        "<br/>",
        "<b> Beam: </b>",
        "~43 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "~20,815 tons",
        "<br/>",
        "<b> Speed: </b>",
        "NA",
        "<br/>",
        "<b> Crew: </b>",
        "~310 (30 officers, 280 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Ballistic missile submarines </b>",
        "are one component of the U.S. nuclear triad. Each submarine carries up to 24 Trident missiles armed with one to eight nuclear warheads apiece. The Navy has 14 Ohio class ballistic missile submarines, each of which displaces about 19,000 tons when submerged. The service has 4 other submarines of that class that it converted to a conventional guided missile (SSGN) configuration. Those SSGNs carry up to 154 Tomahawk missiles as well as special operations forces.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Amphibious_Assault_Shipsb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Amphibious Assault Ship </b>",
        "<br/>",
        "<br/>",
        "<img src='LHA.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> America class </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$3.17B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$555M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "0.6",
        "<br/>",
        "<b> Length:  </b>",
        "855 ft",
        "<br/>",
        "<b> Beam: </b>",
        "106 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "43,745 tons",
        "<br/>",
        "<b> Speed: </b>",
        "20+ knots",
        "<br/>",
        "<b> Crew: </b>",
        "1,204 (102 officers, 1,102 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Amphibous assault ships </b>",
        "are capable of carrying helicopters, tilt-rotor aircraft, and specialized fixed-wing aircraft that can perform short takeoffs and vertical landings. These ships also have well decks that allow them to launch and recover Navy landing craft and Marine Corps amphibious assault vehicles.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO, GAO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Amphibious_Transport_Docksb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Amphibious Transport Dock </b>",
        "<br/>",
        "<br/>",
        "<img src='LPD.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> San Antonio class </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$1.95B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$175M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "0.8",
        "<br/>",
        "<b> Length:  </b>",
        "684 ft",
        "<br/>",
        "<b> Beam: </b>",
        "105 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "24,900 tons",
        "<br/>",
        "<b> Speed: </b>",
        "~22+ knots",
        "<br/>",
        "<b> Crew: </b>",
        "380 (29 officers, 351 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Amphibious transport docks ships </b>",
        "are smaller than LHAs and are used to transport and land Marines, their equipment and supplies. These ships support amphibious assault, special operations or expeditionary warfare missions and can serve as secondary aviation platforms for amphibious ready groups.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO, GAO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Dock_Landing_Shipsb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Dock Landing Ship </b>",
        "<br/>",
        "<br/>",
        "<img src='LSD.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "This app uses the ",
        "<b> LXR </b>",
        "to estimate per unit acquisition cost, operating and support costs, build time and shipyard capacity.",
        "<br/>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$1.56B",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$175M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "1",
        "<br/>",
        "<b> Length:  </b>",
        "~684 ft",
        "<br/>",
        "<b> Beam: </b>",
        "~105 ft",
        "<br/>",
        "<b> Displacement: </b>",
        "~24,900 tons",
        "<br/>",
        "<b> Speed: </b>",
        "22+ knots",
        "<br/>",
        "<b> Crew: </b>",
        "~380 (29 officers, 351 enlisted)",
        "<br/>",
        "<br/>",
        "<b> Dock landing ships </b>",
        "support amphibious operations including landings via Landing Craft, Air Cushion (LCAC), conventional landing craft and helicopters, onto hostile shores.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO, GAO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Combat_Logistics_Forceb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Combat Logistics Force </b>",
        "<br/>",
        "<br/>",
        "<img src='CLF.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<br/>",
        "<b> Acquisition cost: </b>",
        "$344M",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$40M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "3",
        "<br/>",
        "<br/>",
        "<b> Combat logistics and support ships </b>",
        "in the Navy's fleet provide the means to
        resupply, repair, salvage, or tow combat ships. The most prominent of those vessels are fast
        combat support ships, which resupply carrier strike groups with fuel, dry cargo (such as food),
        and ammunition. Logistics and support ships can be as small as 2,000 tons for an
        oceangoing tug or as large as 50,000 tons for a fully loaded fast combat support ship.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO, GAO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  addPopover(
    session,
    "Support_Vesselsb",
    title = NA,
    content = HTML(
      paste0(
        "<div align = 'center'>",
        "<b> Support Vessel </b>",
        "<img src='test3.png',
        alt='', style='width:250px;height:auto;'>",
        "<div align = 'left'>",
        "<b> Acquisition cost: </b>",
        "$344M",
        "<br/>",
        "<b> Annual O&S cost: </b>",
        "$40M",
        "<br/>",
        "<b> Annual build capacity: </b>",
        "4",
        "<br/>",
        "<br/>",
        "<b> Combat logistics and support ships </b>",
        "in the Navy's fleet provide the means to
        resupply, repair, salvage, or tow combat ships. The most prominent of those vessels are fast
        combat support ships, which resupply carrier strike groups with fuel, dry cargo (such as food),
        and ammunition. Logistics and support ships can be as small as 2,000 tons for an
        oceangoing tug or as large as 50,000 tons for a fully loaded fast combat support ship.",
        "<br/>",
        "<br/>",
        "<div align = 'right'>",
        "<b> Source: </b>",
        "CSIS, U.S. Navy, CBO, GAO"
      )
    ),
    placement = "right",
    trigger = 'hover'
  )
  
  observeEvent(input$submit, {
    email_string <- ifelse(input$text == "", "None", input$text)
    file <- file.path("responses",
                      paste0(email_string,
                             format(Sys.time(), '_%Y%m%d_%H%M%S'),
                             ".csv"))
    output_frame <- current$ships %>%
      filter(FY == max(FY))
    output_frame$email <- email_string
    output_frame$comments <-
      ifelse(input$text2 == "", "None", input$text2)
    
    write_csv(output_frame, file)
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Success!')
    
  })
  
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
})

# ================================================================================