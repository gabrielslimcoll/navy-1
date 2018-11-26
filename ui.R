# ================================================================================
# Build Your Own: Navy
# Designed and built by Gabriel Coll and Loren Lipsey
# --------------------------------------------------------------------------------
# user interface
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)

# --------------------------------------------------------------------------------
# load terms

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
# load data

load("data/navy_data.Rda")
load("data/ship_stats.Rda")

# --------------------------------------------------------------------------------
# get the min, max, and default settings for sliders

default_ships <- filter(navy_data[["Fleet Plan"]], FY == max(FY))

min_ships <- default_ships -
  filter(navy_data[["Max Cut Builds"]], FY == max(FY))

max_ships <- default_ships +
  filter(navy_data[["Max New Builds"]], FY == max(FY))

ship_stats <- ship_stats[5:length(ship_stats)]

# --------------------------------------------------------------------------------
# begin user interface section

ui <- fluidPage(
  useShinyjs(),
  
  # --------------------------------------------------------------------------------
  # import Google Font "Open Sans"
  
  tags$style(
    HTML(
      "@import url('//fonts.googleapis.com/css?family=Open+Sans');
      body {
      font-family: 'Open Sans',  sans-serif;
      font-weight: 500;
      line-height: 1.1;
      color: #554449;
      }"
)
    ),

# --------------------------------------------------------------------------------
# app design style

tags$head(tags$style(
  HTML("body{background-color: #fcfcfc;}")
)),

tags$style(
  type = "text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
),

# --------------------------------------------------------------------------------
# button style

tags$head(tags$style(
  HTML(".well{
       background-color: #FCFCFC;
       border-color: #FCFCFC;
       }")
)),

tags$style(HTML(
  ".popover({delay: {show: 500, hide: 100}})"
)),

tags$style(
  HTML(
    ".btn {background-color: #4D7FA3;
    color: white;
    border-color: #FCFCFC}"
  )
),

tags$style(
  HTML(
    ".btn:hover {border-color: #FCFCFC;
    background-color: #4D7FA3;
    color: white;
    font-weight: normal}"
  )
  ),

tags$style(HTML(
  ".btn:active:hover {background-color: #6BC6B5}"
)),

tags$style(
  HTML(
    ".btn-primary {background-color: #FCFCFC;
    border-color: #FCFCFC;
    color: #554449}"
  )
),

tags$style(
  HTML(
    ".btn-primary:hover {background-color: #E5E5E5;
    color: #554449}"
  )
),

tags$style(
  HTML(".btn-primary:active:hover{background-color: #E5E5E5}")
),

tags$style(
  HTML(".btn-primary:active:focus{background-color: #E5E5E5}")
),

tags$style(
  HTML(".btn-primary:dropdown-toggle {background-color: #FCFCFC}")
),

tags$style(
  HTML(
    ".btn-basic {background-color: #BDD4DE;
    border-color: #FCFCFC;
    color: #554449}"
  )
),

tags$style(
  HTML(".btn-basic:hover {background-color: #A5B9C2;
       color: #554449}")
  ),

tags$style(HTML(
  ".btn-basic:active:hover{background-color: #6BC6B5}"
)),

tags$style(HTML(
  ".btn-basic:active:focus{background-color: #90ABC2}"
)),
tags$style(
  HTML(".btn-basic:dropdown-toggle {background-color: #BDD4DE}")
),

tags$style(
  HTML(
    ".btn-basic:active,.open>.dropdown-toggle.btn-basic {background-color: #6BC6B5}"
  )
),

tags$style(
  HTML(".btn-basic:dropdown-toggle {background-color: #BDD4DE}")
),

tags$style(
  HTML(
    ".btn-primary:active,.open>.dropdown-toggle.btn-basic {background-color: #6BC6B5}"
  )
),

tags$style(
  HTML(".btn-primary:dropdown-toggle {background-color: #BDD4DE}")
),

# --------------------------------------------------------------------------------
# slider style

tags$style(
  HTML(
    ".irs-bar {background: #788ca8;
    border-top: 1px #566377;
    border-bottom: 1px #566377}"
  )
),

tags$style(HTML(
  ".irs-single, .irs-to, .irs-from {background: #788ca8}"
)),

tags$style(HTML(".irs-max {color: #554449}")),

tags$style(HTML(".irs-min {color: #554449}")),

tags$style(HTML(".irs-bar-edge {border: 1px #566377;
                border-color: 1px #566377;
                border-color: 1px #566377}"
)),

# --------------------------------------------------------------------------------
# CSIS header

tags$div(
  HTML(
    "<div class='fusion-secondary-header'>
    <div class='fusion-row'>
    <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:15px;'><a href='http://csis.org/program/international-security-program' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
    </div>
    </div>"
  )
  ),
tags$style(
  HTML(
    ".fusion-secondary-header {border-bottom: 2.5px solid #6F828F}"
  )
),

br(),

# --------------------------------------------------------------------------------
# begin sidebar panel

fluidRow(
  sidebarPanel(
    shinyjs::useShinyjs(),
    
    tags$head(tags$style(
      HTML("body{background-color: #FCFCFC;}")
    )),
    
    tags$head(tags$style(
      HTML(".well{
           background-color: #FCFCFC;
           border-color: #FCFCFC;
           }")
)),

# --------------------------------------------------------------------------------
# Google analytics script

tags$script(
  HTML(
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-99363803-1', 'auto');
    ga('send', 'pageview')"
)
),

# --------------------------------------------------------------------------------
# info button

bsButton(
  inputId = "info_btn",
  label = strong("Build your own Navy >"),
  style = "default",
  type = "toggle",
  size = "small",
  block = TRUE
),

# --------------------------------------------------------------------------------
# sliders 

conditionalPanel(
  condition = "input.info_btn == 0",
  
  bsButton(
    inputId = "Aircraft_Carriersb",
    label = strong("Aircraft Carriers (CVN)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Aircraft_Carriers",
      label = NA,
      value = unlist(default_ships["Aircraft Carriers"]),
      min = unlist(min_ships["Aircraft Carriers"]),
      max = unlist(max_ships["Aircraft Carriers"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Large_Surface_Combatantsb",
    label = strong("Large Surface Combatant (LSC)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Large_Surface_Combatants",
      label = NA,
      value = unlist(default_ships["Large Surface Combatants"]),
      min = unlist(min_ships["Large Surface Combatants"]),
      max = unlist(max_ships["Large Surface Combatants"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Small_Surface_Combatantsb",
    label = strong("Small Surface Combatant (SSC)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Small_Surface_Combatants",
      label = NA,
      value = unlist(default_ships["Small Surface Combatants"]),
      min = unlist(min_ships["Small Surface Combatants"]),
      max = unlist(max_ships["Small Surface Combatants"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Attack_Submarinesb",
    label = strong("Attack Submarine (SSN)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Attack_Submarines",
      label = NA,
      value = unlist(default_ships["Attack Submarines"]),
      min = unlist(min_ships["Attack Submarines"]),
      max = unlist(max_ships["Attack Submarines"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Ballistic_Missile_Submarinesb",
    label = strong("Ballistic Missile Submarine (SSBN)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Ballistic_Missile_Submarines",
      label = NA,
      value = unlist(default_ships["Ballistic Missile Submarines"]),
      min = unlist(min_ships["Ballistic Missile Submarines"]),
      max = unlist(max_ships["Ballistic Missile Submarines"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Amphibious_Assault_Shipsb",
    label = strong("Amphibious Assault Ship (LHD / LHA)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Amphibious_Assault_Ships",
      label = NA,
      value = unlist(default_ships["Amphibious Assault Ships"]),
      min = unlist(min_ships["Amphibious Assault Ships"]),
      max = unlist(max_ships["Amphibious Assault Ships"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Amphibious_Transport_Docksb",
    label = strong("Amphibious Transport Dock (LPD)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Amphibious_Transport_Docks",
      label = NA,
      value = unlist(default_ships["Amphibious Transport Docks"]),
      min = unlist(min_ships["Amphibious Transport Docks"]),
      max = unlist(max_ships["Amphibious Transport Docks"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Dock_Landing_Shipsb",
    label = strong("Dock Landing Ship (LSD / LXR)"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Dock_Landing_Ships",
      label = NA,
      value = unlist(default_ships["Dock Landing Ships"]),
      min = unlist(min_ships["Dock Landing Ships"]),
      max = unlist(max_ships["Dock Landing Ships"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Combat_Logistics_Forceb",
    label = strong("Combat Logistics Force"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  div(
    style = "height: 40px;",
    sliderInput(
      inputId = "Combat_Logistics_Force",
      label = NA,
      value = unlist(default_ships["Combat Logistics Force"]),
      min = unlist(min_ships["Combat Logistics Force"]),
      max = unlist(max_ships["Combat Logistics Force"]),
      step = 1,
      ticks = FALSE
    )
  ),
  align = "center",
  
  bsButton(
    inputId = "Support_Vesselsb",
    label = strong("Support Vessels"),
    style = "primary",
    type = "action",
    size = "extra-small",
    block = TRUE
  ),
  
  sliderInput(
    inputId = "Support_Vessels",
    label = NA,
    value = unlist(default_ships["Support Vessels"]),
    min = unlist(min_ships["Support Vessels"]),
    max = unlist(max_ships["Support Vessels"]),
    step = 1,
    ticks = FALSE
  ),
  align = "center",
  
  bsButton(
    inputId = "reset_input",
    label = strong("Reset"),
    style = "basic",
    size = "small",
    block = TRUE
  ),
  
  tags$head(tags$script(src = "message-handler.js")),
  bsButton(
    inputId = "submit",
    label = strong("Submit"),
    style = "default",
    size = "small",
    block = TRUE
  )
),

# --------------------------------------------------------------------------------
# background information 

conditionalPanel(condition = "input.info_btn == 1",
                 
                 helpText(
                   HTML(
                     "<div align = 'center'>",
                     "<b> Background Information </b>",
                     "</div>",
                     "<br/>",
                     "<div align = 'left'>",
                     "Each year, the Navy releases its 30-year shipbuilding plan. This document separates Navy ships into nine categories (CVN, LSC, SSC, SSN, SSGN, SSBN, AWS, CLF, and support vessels). For each of the categories, the Navy projects how many ships it will procure and how many ships it will have in its inventory each year. The plan does not include acquisition costs or operations and support (O&S) costs for each category. However, acquisition and O&S cost data is available elsewhere.",
                     "<br/>",
                     "<br/>",
                     "This app adopts the structure of the 30-year shipbuilding plan and integrates Navy budget data. Each of the Navy's ship categories displays an independent slider that allows the user to adjust the quantity of that category. [Amphibious warfare ships are separated into three categories (LHD/LHA, LPD, and LSD/LXR); SSGN and SSBN are consolidated into one category]",
                     "<br/>",
                     "<br/>",
                     "<b> Slider default: </b>",
                     "each slider is set to the number of ships the Navy plans to have in its inventory by 2046. Any increase to a slider will build ships as early as possible and any decrease will cancel the earliest planned procurements.",
                     "<br/>",
                     "<br/>",
                     "<b> Slider max: </b>",
                     "the user can only increase the slider to the degree that shipyard capacity allows. Shipyard capacity is the estimated number of ships that can be built each year for each category.",
                     "<br/>",
                     "<br/>",
                     "<b> Slider min: </b>",
                     "while the user can cancel planned procurements, the user cannot decommission existing ships.",
                     "<br/>",
                     "<br/>",
                     "The new administration's ambitious plan to reach a 355-ship Navy coupled with a lack of specifics to achieve this plan has led to a wide range of recommendations for how the Navy's shipbuilding plan should change. However, these recommendations have a similarly wide range of budgetary assumptions behind them, which are often difficult to understand.",
                     "<br/>",
                     "<br/>",
                     "<b> The purpose of this app is to create a transparent and interpretable analytic model of the Navy's force structure that is easy to use. </b> The model accounts for the key factors that all recommendations should consider: the Navy's current procurement plan, its current inventory plan, unit acquisition cost, unit O&S cost, and capacity constraints.",
                     "<br/>",
                     "<br/>",
                     "There are, of course, factors that this model does not account for (such as the ability to change shipyard capacity, to develop entirely new ship classes, and to account for economies of scale or research and development costs). The CSIS International Security Program is planning to examine these challenges in its future research.",
                     "</div>"
                   )
                 )),
align = "center"
  ),

# --------------------------------------------------------------------------------
# top chart 

mainPanel(
  div(
    style = "position:relative",
    plotOutput(
      "ships",
      height = "320px",
      hover = hoverOpts(id = "plot_hover_ships", delay = 80)
    ),
    uiOutput("hover_info_ships")
  ),
  
  # --------------------------------------------------------------------------------
  # additional user inputs 
  
  wellPanel(column(
    6,
    conditionalPanel(
      condition = "input.options_input != 3",
      div(
        style = "display:inline-block",
        selectizeInput(
          inputId = "top_y",
          label = NA,
          choices = list(
            Ships = c("Ships"),
            Capacity = c("Tonnage",
                         # "VLS Missile Cells",
                         "Aircraft Capacity"),
            "O&S Cost (CBO)" = c("Direct Cost",
                                 "Indirect Cost",
                                 "Overhead Cost",
                                 "Total Cost"),
            "Personnel (CBO)" = c(
              "Direct Personnel",
              "Indirect Personnel",
              "Overhead Personnel",
              "Total Personnel"
            ),
            # "Speed",
            "Crew Size" = c("Officers",
                            "Enlisted",
                            "Total")
          ),
          selected = "Ships",
          width = '200px'
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------------
  # bottom chart 
  
  column(
    6,
    conditionalPanel(condition = "input.options_input != 3",
                     div(
                       style = "display:inline-block",
                       radioButtons(
                         "bottom_chart",
                         NA,
                         c("Change", "Total"),
                         selected = "Total",
                         inline = TRUE
                       )
                     ))
  )),
  
  div(
    style = "position:relative",
    plotOutput(
      "budget",
      height = "320px",
      hover = hoverOpts(id = "plot_hover_budget", delay = 80)
    ),
    uiOutput("hover_info_budget")
  ),
  
  br(),
  
  # --------------------------------------------------------------------------------
  # user submission feature 
  
  fluidRow(
    column(4,
           
           textInput(
             "text", label = NULL,
             placeholder = "Email"
           ), align = "center"),
    
    # --------------------------------------------------------------------------------
    # tooltips 
    
    bsTooltip(
      "submit",
      "CSIS is collecting data from users who are willing to share their Navy shipbuilding plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
      "top",
      options = list(container = "body")
    ),
    
    bsTooltip(
      "text",
      "CSIS is collecting data from users who are willing to share their Navy shipbuilding plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
      "top",
      options = list(container = "body")
    ),
    
    bsTooltip(
      "top_y",
      "This feature is still in development. Variables (beyond ships) are estimates. The combat logistics force and support vessels currently have values of 0 for all other variables",
      "top",
      options = list(container = "body")
    ),
    
    bsTooltip(
      "text2",
      "CSIS is collecting data from users who are willing to share their Navy shipbuilding plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
      "top",
      options = list(container = "body")
    ),
    
    conditionalPanel(
      condition = "input.options_input == 2",
      div(
        style = "display:inline-block",
        checkboxInput("checkbox", label = "Dollars", value = FALSE)
      )
      
    ),
    column(
      8,
      textInput(
        "text2",
        label = NULL,
        width = '100%',
        placeholder = "Comments"
      ),
      bsTooltip(
        "text2",
        "CSIS is collecting data from users who are willing to share their Navy shipbuilding plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
        "top",
        options = list(container = "body")
      )
      
    ),
    
    tags$body(tags$style(HTML(
      ".col-sm-6 {border: 4}"
    ))),
    
    # --------------------------------------------------------------------------------
    # additional style 
    
    tags$style(
      ".selectize-control {
      color: #554449;
      font-size: 14px;
      font-style: normal;
      background-color: #EDECEB;
      border-color: #C76363;
      border-style: solid;
      border-width: 6px;
      border-top: 6px #63c5b8;
      border-bottom: 6px #63c5b8;
      border-right: 6px #63c5b8;
      #border-left: 6px #63c5b8;
      border-radius: 5px
      }"
    ),
    
    tags$style(HTML(".popover {background: #256A91};")),
    
    tags$style(
      HTML(
        ".popover-content {color: white; font-family: 'Open Sans',  sans-serif};"
      )
    ),
    
    tags$style(HTML(".arrow {visibility: hidden};")),
    
    tags$style(
      HTML(
        ".popover.right > .arrow:after {visibility: hidden;
        border-right-color: white;
        border-color: white;
  };"
    )
      ),
    
    tags$style(HTML(".irs-bar {border-top: 1px #687991;
                    border-bottom: 1px #687991}")),
    
    tags$style(HTML(
      ".irs-single, .irs-to, .irs-from {background: #687991}"
    )),
    
    tags$style(HTML(".well {color: #554449}")),
    
    tags$style(
      HTML(
        "img {
        padding:1px;
        #border:1px solid #021a40;
        #background-color:#ff0;
        height: 'auto';
        max-width: '100%';
        border-radius: 5px
    }"
)
      ),

tags$style(HTML(
  ".img .shiny-image-output {border: 4px green}"
)),

tags$style(HTML(
  ".img .shiny-image-output {border-color: 4px purple}"
)),

tags$style(HTML(
  ".irs-bar-edge {border-color: 1px #63c5b8}"
))
      ),

conditionalPanel(
  condition = "input.options_input == 3",
  downloadLink('CSVDownloadBtn',
               "Download (csv)",
               class = "butt")
),
align = 'center'
    )
  )
  )

# ================================================================================
