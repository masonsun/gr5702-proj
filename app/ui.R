# main page 
shinydashboard::dashboardPage(
  # header
  shinydashboard::dashboardHeader(title = "Indego Analytics"),
  # sidebar 
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Home", tabName = "home", icon = shiny::icon("home")),
      shinydashboard::menuItem("Station Usage", tabName="station", icon=shiny::icon("line-chart")),
      shinydashboard::menuItem("Daily Traffic", tabName="traffic", icon=shiny::icon("table")),
      shinydashboard::menuItem("Trip Durations", tabName="trip", icon=shiny::icon("bar-chart")),
      shinydashboard::menuItem("User Profiles", tabName="user", icon=shiny::icon("globe")),
      shinydashboard::menuItem("Real-time analysis", tabName="realtime", icon=shiny::icon("compass")),
      br(),
      shinydashboard::menuItem("Source Code", icon=icon("code"), href="https://github.com/masonsun/gr5702-proj")
    )
  ),
  # body
  shinydashboard::dashboardBody(
    # title
    tags$head(
      tags$title("STAT 5702: Final Project"),
      tags$script(src = "main.js")
    ),
    
    # content of each tab
    shinydashboard::tabItems(
      # home tab
      shinydashboard::tabItem(
        tabName = "home",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            shiny::h1("STAT 5702: Final Project"),
            br(),
            shiny::h5("Kejia Shi (ks3403)"),
            shiny::h5("Mason Sun (zs2321)"),
            shiny::h5("Hung-Shi Lin (hl2997)")
          )
        )
      ),
      
      # station usage tab
      shinydashboard::tabItem(
        tabName = "station",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Station Usage", status = "primary", width = 5, collapsible = T,
            shiny::selectizeInput("station_usage_stn", h5(strong("Select station:")), 
                                  stations, multiple = T),
            shiny::dateRangeInput('date_range_stn', label = "Select dates:",
                                  start = "2016-01-01", end = "2016-01-02",
                                  min = "2015-04-23", max = "2016-12-09"),
            leafletOutput("station_usage_map")
          ),
          shinydashboard::box(
            status = "primary", width = 7, collapsible = T,
            plotlyOutput("station_usage_plot")
          )
        )
      ),
      
      # daily traffic tab
      shinydashboard::tabItem(
        tabName = "traffic",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Daily traffic", status = "primary", width = 8, collapsible = T,
            plotlyOutput("daily_traffic_plot")
          ),
          shinydashboard::box(
            status = "primary", width = 4, collapsible = F,
            shiny::selectInput("daily_traffic_stn", h5(strong("Select station:")), 
                               stations, multiple=T),
            shiny::dateRangeInput('date_range_traffic', label = "Select dates:",
                                  start = "2016-01-01", end = "2016-01-02",
                                  min = "2015-04-23", max = "2016-12-09"),
            shiny::sliderInput("daily_traffic_bins", h5(strong("Select bin size:")),
                               min=1, max=24, value=12)
          )
        )
      ),
      
      # trip duration tab
      shinydashboard::tabItem(
        tabName = "trip",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Trip duration", status = "primary", width = 8, collapsible = T,
            plotOutput("trip_duration_plot")
          ),
          # user input
          shinydashboard::box(
            status = "primary", width = 4, collapsible = F,
            shiny::selectInput("trip_duration_stn", h5(strong("Select station:")), 
                               stations, multiple = T),
            shiny::dateRangeInput('date_range_trip', label = "Select dates:",
                                  start = "2016-01-01", end = "2016-01-02",
                                  min = "2015-04-23", max = "2016-12-09"),
            #shiny::selectInput("traffic_duration_month", h5(strong("Select month:")),
            #                   format(ISOdate(2016,1:12,1),"%B"), multiple=F),
            #shiny::selectInput("traffic_duration_year", h5(strong("Select year:")),
            #                   list("2015", "2016"), multiple=F),
            shiny::radioButtons("trip_duration_radio", h5(strong("Choose one:")),
                                choices=list("None", "Weekdays", "Weekends")),
            shiny::sliderInput("trip_duration_bins", h5(strong("Select bin size:")),
                               min=1, max=24, value=12)
          )
        )
      ),
      
      # user profiles tab
      shinydashboard::tabItem(
        tabName = "user",
        shiny::fluidRow(
          shinydashboard::box(
            title = "User profiles", status = "primary", width = 8, collapsible = T,
            plotOutput("user_plot")
          ),
          shinydashboard::box(
            status = "primary", width = 4, collapsible = F,
            shiny::selectInput("user_stn", h5(strong("Select station:")), 
                               stations, multiple=T),
            shiny::selectInput("user_pass", h5(strong("Select passholder type:")),
                               unique(df$passholder_type), multiple=T),
            shiny::selectInput("user_route", h5(strong("Select route type:")),
                               unique(df$trip_route_category), multiple=T)
          )
        )
      ),
      
      # real-time analysis
      shinydashboard::tabItem(
        tabName = "realtime",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Real-time analysis", status = "primary", width = 12, collapsible = F,
            leaflet::leafletOutput("realtime_plot"),
            br(),
            actionButton("realtime_refresh", "Refresh")
          )
        )
      )
    )
  )
)