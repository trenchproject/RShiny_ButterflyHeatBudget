# packages <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "ggmap", "maps", "raster", "sp", "rgdal", "viridis", "shinythemes", "shinyWidgets", "shinycssloaders", "shinyjs", "colorRamps", "sortable", "rnoaa", "chillR", "reshape2", "rasterVis", "tidyr", "gridExtra", "shinyBS", "gridExtra", "ggmap")
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

library("shiny")
library("ggplot2")
library("shinyWidgets")
library("rnoaa")
library("magrittr")
library("dashboardthemes")
library("shinydashboard")
library("shinyjs")
# library("dplyr")
# library("maps")
# library("raster")
# library("sp")
# library("rgdal")
# library("viridis")
#
library("shinycssloaders")
# library("shinyjs")
# library("colorRamps")
# library("sortable")
# library("chillR")
# library("reshape2")
# library("rasterVis")
# library("tidyr")
# library("raster")
# library("shinyBS")
# library("gridExtra")
# library("ggmap")
library("plotly")
library("shinyBS")
shiny::addResourcePath("www", "www")



shinyUI <- dashboardPage(
  title = "Butterfly Heat Budget Model",
  # tags$head(
  #   tags$link(rel = "shortcut icon", href = "www/favicon.ico", type = "image/x-icon")
  # ),
  dashboardHeader(
    title = div(tags$img(src = "TRENCH_Logo_Circle-TrenchEd.png", height = 50), "Animal Heat Budgets", style = "font-size: 32px"), titleWidth = 800,
    # Set height of dashboardHeader
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 50px}"),
      tags$style(".main-header .logo {height: 50px;}"),
      tags$style(".sidebar-toggle {height: 50px; padding-top: 1px !important;}"),
      tags$style(".navbar {min-height:50px !important}")
    )
  ),
  dashboardSidebar(
    width = 300, collapsed = TRUE,
    sidebarMenu(
      menuItem("Weather",
        tabName = "weather", icon = icon("sun"), startExpanded = TRUE,
        radioButtons("data", list(icon("thermometer-half"), "Temperature data"), choices = c("Most recent" = "recent", "Manually input"), inline = TRUE),
        # htmlOutput("temp"),
        uiOutput("manual"),
        sliderInput("wind", list(icon("wind"), "Wind speed (m/s)"), min = 0.2, max = 5, step = 0.2, value = 1),
        radioGroupButtons("weather", list(icon("snowman"), "Weather"),
          choices = c(
            '<i class="fas fa-sun"></i>' = "Clear",
            '<i class="fas fa-cloud-sun"></i>' = "Partly sunny",
            '<i class="fas fa-cloud"></i>' = "Cloudy"
          ),
          status = "info", justified = TRUE
        )
        # selectInput("weather", "Weather", choices = c("Clear", "Partly cloudy", "Cloudy")),
        # sliderInput("zenith", "Zenith angle (°)", min = 0, max = 90, step = 5, value = 30)
      ),
      menuItem("Morphology",
        tabName = "morph", icon = icon("bug"),
        selectInput("abs", list(icon("tint"), "Wing absorptivity (proportion)"), choices = seq(0.4, 0.7, 0.05)),
        numericInput("diam", list(icon("ruler-vertical"), "Thoracic diameter (mm)"), value = 3.6, min = 0.5, step = 0.5),
        numericInput("fur", list(icon("ruler-horizontal"), "Fur thickness (mm)"), value = 1.46, min = 0, step = 0.5)
      )
    ),
    bsTooltip("weather", "Clear / Partly sunny / Cloudy"),
    bsTooltip("abs", "Butterfly absorptivity to solar radiation. The greater the value, the darker the wing coloration.")
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "onenote"
    ),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      includeCSS(path = "www/custom.css"),
    ),
    useShinyjs(),
    includeHTML("intro.html"),
    br(),
    h4(strong("Activity description")),
    includeHTML("instruction.html"),
    br(),
    # bsCollapse(
    #   id = "collapse",
    #   bsCollapsePanel(
    #     title = "Activity description", style = "warning",
    #     includeHTML("instruction.html")
    #   )
    # ),
    # actionButton("showSidebar", "Show sidebar", styleclass = "danger"),
    # actionButton("hideSidebar", "Hide sidebar", styleclass = "info"),
    awesomeCheckbox("sidebar", "Show sidebar", status = "danger"),
    # plotOutput("plot") %>% withSpinner(type = 7),
    br(),
    plotlyOutput("plotly"),
    br(),
    hr(),
    h4(strong("Analysis")),
    fluidRow(
      box(htmlOutput("heat"), width = 8),
      box(
        title = "Specified variables", solidHeader = TRUE, collapsible = TRUE, width = 4,
        htmlOutput("summary")
      )
    ),
    fluidRow(
      column(
        8,
        h4(strong("Detailed analysis")),
        htmlOutput("details")
        # bsCollapse(
        #   id = "collapse",
        #   bsCollapsePanel(
        #     title = "Detailed analysis", style = "warning",
        #     htmlOutput("details")
        #   )
        # )
      ),
      box(
        title = "Other variables", solidHeader = TRUE, collapsible = TRUE, width = 4,
        radioGroupButtons("switch", choices = c("Text", "Value"), size = "xs", status = "warning"),
        htmlOutput("variables")
      )
    )
  )
)
