# server for the app
shiny::shinyServer(function(input, output, session) {
  
  ##### station usage #####
  output$station_usage_map <- renderLeaflet({
    # user defined parameters
    station_list = input$station_usage_stn
    data_subset = station_loc[which(station_loc$name %in% station_list),]
    start_date = input$date_range_stn[1]
    end_date = input$date_range_stn[2]
    # plot
    leaflet(data_subset) %>%
      addTiles() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addMarkers(~long, ~lat, popup=~name) %>%
      setView(lng=-75.163763, lat=39.952568, zoom = 12)
  })
  
  output$station_usage_plot <- renderPlotly({
    x <- input$station_usage_stn
    d1 <- input$date_range_stn[1]
    d2 <- input$date_range_stn[2]
    
    query <- station_loc[which(station_loc$name %in% x), -5]
    query$id <- as.character(query$id)
    query_df <- df[df$date >= d1 & df$date <= d2, ]
    
    rental <- query_df %>% group_by(date, start_station_id) %>% summarize(Rental = n()) %>% filter(start_station_id %in% query$id)
    colnames(rental)[2] <- "id"
    rental$id <- as.character(rental$id)
    return <- query_df %>% group_by(date, end_station_id) %>% summarize(Return = n()) %>% filter(end_station_id %in% query$id)
    colnames(return)[2] <- "id"
    return$id <- as.character(return$id)
    station_hist <- dplyr::left_join(rental, return, by = c("date" = "date", "id" = "id"))
    station_hist <- station_hist %>% dplyr::right_join(query, by = c("id" = "id"))
    station_hist <- tidyr::gather(station_hist, type, value, 3:4)
    line1 <- ggplot(station_hist, aes(x = date, y = value, color = name, linetype = type, group=interaction(type, name))) +
      geom_point() +
      geom_line() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    plotly::ggplotly(line1)
  })
  
  ##### daily traffic #####
  output$daily_traffic_plot <- renderPlotly({
    # user defined parameters 
    binsize = input$daily_traffic_bins
    start_date = input$date_range_traffic[1]
    end_date = input$date_range_traffic[2]
    station_list = input$daily_traffic_stn
    data_subset = station_loc[which(station_loc$name %in% station_list),]
    # plot
    x <- c("9th & Arch", "19th & Lombard")
    d1 <- "2016-04-23"
    d2 <- "2016-04-24"
    
    date_diff <- as.Date(as.character(d2), format="%Y-%m-%d") - as.Date(as.character(d1), format="%Y-%m-%d")
    date_diff <- as.numeric(date_diff) + 1
    
    query <- station_loc[which(station_loc$name %in% x), -5]
    query$id <- as.character(query$id)
    query_df <- df[df$date >= d1 & df$date <= d2, ]
    
    rental <- query_df %>% group_by(start_station_id, s_hour) %>% summarize(Rental = n()/date_diff) %>% filter(start_station_id %in% query$id)
    colnames(rental)[1] <- "id"
    rental$id <- as.character(rental$id)
    colnames(rental)[2] <- "hour"
    return <- query_df %>% group_by(end_station_id, e_hour) %>% summarize(Return = n()/date_diff) %>% filter(end_station_id %in% query$id)
    colnames(return)[1] <- "id"
    return$id <- as.character(return$id)
    colnames(return)[2] <- "hour"
    
    daily_hist <- dplyr::full_join(rental, return, by = c("hour" = "hour", "id" = "id"))
    daily_hist[is.na(daily_hist)] <- 0
    daily_hist <- daily_hist %>% dplyr::right_join(query, by = c("id" = "id"))
    daily_hist <- daily_hist[,-1]
    daily_hist <- tidyr::gather(data = daily_hist, key = type, value = value, c(2, 3))
    
    p1 <- ggplot(data=daily_hist, aes(x=hour, y=value, fill=name)) +
      geom_bar(stat="identity", position="dodge", width=binsize) +
      labs(title="Average Trip Counts", subtitle="") 
    ggplotly(p1)
  })
  
  ##### trip duration #####
  output$trip_duration_plot <- renderPlot({
    station_list = input$trip_duration_stn
    data_subset = station_loc[which(station_loc$name %in% station_list),]
    start_date = input$date_range_trip[1]
    end_date = input$date_range_trip[2]
    week_choice = input$trip_duration_radio
    bins = input$trip_duration_bins
    # plot
    
  })
  
  ##### user profile #####
  output$user_plot <- renderPlot({
    station_list = input$user_stn
    data_subset = station_loc[which(station_loc$name %in% station_list),]
    pass_choices = input$user_pass
    route_choices = input$user_route
    # plot
    
  })
  
  ##### real time analysis #####
  substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
  }
  getStationStatusData <- function() {
    station_status <- fromJSON("https://gbfs.bcycle.com/bcycle_indego/station_status.json")
    data.frame(bikes = sapply(station_status$data$stations, function(x) x$num_bikes_available),
               docks = sapply(station_status$data$stations, function(x) x$num_docks_available),
               id = sapply(station_status$data$stations, function(x) x$station_id %>% substrRight(4) %>% as.numeric))
  }
  plotStationStatus <- function() {
    station_status <- getStationStatusData()
    station_status <- station_status %>% dplyr::select(id, bikes, docks) %>% 
      dplyr::left_join(station_loc, by = "id")
    content <- paste("<b>", as.character(station_status$name), "</b><br>",
                     "Bikes for Rental: ", station_status$bikes, "<br>",
                     "Docks for Return: ", station_status$docks, "<br>")
    leaflet(station_status) %>%
      addTiles() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addMarkers(~long, ~lat, popup = ~content) %>%
      setView(lng=-75.163763, lat=39.952568, zoom = 12)
  }
  observeEvent(input$realtime_refresh, {
    #session$sendCustomMessage(type='refresh_message', message = 'Successful refresh')
    output$realtime_plot <- renderLeaflet({ plotStationStatus() })
  })
  output$realtime_plot <- renderLeaflet({ plotStationStatus() })
})