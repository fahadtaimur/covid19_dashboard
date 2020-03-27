library(tidyverse)
library(magrittr)
library(readr)
library(plotly)
library(lubridate)
library(tidyquant)
library(DT)
library(ggplot2)
library(ggthemes)
# Get DATA
get_data <- function(){
  # Data sourced from github
  confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = T)
  deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)
  
  
  # Data bind
  corona_tbl <- confirmed %>%
    mutate(type = "confirmed") %>%
    bind_rows(deaths %>%
                mutate(type = "deaths")) %>%
    mutate(Country.Region = Country.Region %>% as.character(), 
           Province.State = Province.State %>% as.character())
  
  # Data Clean
  corona_tbl %>%
    select(type, Country.Region, Province.State, Lat, Long, everything()) %>%
    gather(key = time_period, value = number_of_cases, 6:length(corona_tbl)) %>%
    mutate(time_period = str_remove(time_period, "X")) %>%
    mutate(date = time_period %>% mdy()) %>%
    select(date, Country.Region, Province.State, type, Lat, Long, number_of_cases) -> corona_tbl
  
  return(corona_tbl)
}


# Test it 
data <- get_data() 

# Plot Country profile
plot_country_profile_ts <- function(data, country, outcome, date1, date2){
  
  date_seq <- DATE_SEQUENCE(date1, date2, by = "day")
  
  ggplotly(
    data %>% 
      dplyr::filter(date %>% between(left = date1, right = date2)) %>%
      dplyr::filter(Country.Region == country, type == outcome) %>%
      group_by(Country.Region, date) %>%
      summarise(total_cases = sum(number_of_cases)) %>%
      ungroup() %>%
      mutate(Month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
      select(date, total_cases, Month) %>%
      ggplot(aes(x = date, 
                 y = total_cases)) + 
      geom_line() + 
      theme_tq())
}

# Test it
plot_country_profile_ts(data = data, country = "US", outcome = "confirmed", date1 = date1, date2 = date2)

data <- get_data()

# Datatable logic

data %>%
  group_by(Country.Region, type) %>%
  summarise(Cases = sum(number_of_cases)) %>%
  ungroup() %>%
  spread(key = type, value = (Cases)) %>%
  rename(
    Country = Country.Region,
    Confirmed = confirmed,
    Deaths = deaths
  ) %>%
  mutate(Confirmed= format(Confirmed, big.mark = ",", scientific = F),
         Deaths = format(Deaths, big.mark = ",", scientific = F)) %>%
  DT::datatable()

# Data Table of total cases
total_cases_dt <- function(data){
  data %>%
    group_by(Country.Region, type) %>%
    summarise(Cases = sum(number_of_cases)) %>%
    ungroup() %>%
    spread(key = type, value = (Cases)) %>%
    rename(
      Country = Country.Region,
      Confirmed = confirmed,
      Deaths = deaths
    ) %>%
    mutate(Confirmed= format(Confirmed, big.mark = ",", scientific = F),
           Deaths = format(Deaths, big.mark = ",", scientific = F)) %>%
    DT::datatable(rownames = F,
                  options = list(
                    autoWidth = FALSE,
                    dom = "t",
                    columnDefs = list(list(width = '200px'))
                  ))
}

total_cases_dt(data = data)

# Plotting a map
plot_map <- function(data, outcome = "confirmed"){
  data %>%
    dplyr::filter(date == today() - days(1)) %>%
    dplyr::filter(type == outcome) %>%
    group_by(Country.Region, Province.State, Lat, Long) %>%
    summarise(cases = sum(number_of_cases)) -> corona_summarized
  
  world <- ggplot() +
    borders("world", colour = "gray85", fill = "gray80") +
    theme_map()
  
  map <- world +
    geom_point(aes(x = Long, y = Lat,
                   text = paste('Country: ', Country.Region,
                                '<br /> Province/State : ', Province.State),
                   size = cases),
               data = corona_summarized, colour = 'blue', alpha = .5) +
    scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
    labs(size = 'Cases')
  
  return(ggplotly(map, 
                  tooltip = c("text", "size")) )
}

plot_map(data = data)

# Plot-ly function
plotly_function <- function(data, country, outcome, date1, date2){
  # data modeling
  plot_df <- data %>% 
    dplyr::filter(date %>% between(left = date1, right = date2)) %>%
    dplyr::filter(Country.Region == country, type == outcome) %>%
    group_by(Country.Region, date) %>%
    summarise(total_cases = sum(number_of_cases)) %>%
    ungroup()
  
plotly::plot_ly(data = plot_df) %>%
    # line type
  plotly::add_trace(x = ~ date,
                    y = ~ total_cases,
                    type = "scatter",
                    mode = "lines+markers",
                    name = "Active") %>%
  # format
  plotly::layout(title = "",
                 yaxis = list(title = "Cumulative Cases"),
                 xaxis = list(title = ""),
                 legend = list(x = 0.1, y = 0.9),
                 hovermode = "compare")
}

plotly_function(data = data, country = "US", outcome = "confirmed", date1 = date1, date2 = date2)

# Leaflet Map

leaflet_map_plot <- function(data){
# wrangle data
data_for_plot <- data %>% 
  dplyr::filter(date == date %>% tail(1)) %>%
  dplyr::dplyr::filter(number_of_cases > 0) %>% 
  dplyr::group_by(Country.Region, Province.State, Lat, Long, type) %>% 
  dplyr::summarise(cases = sum(number_of_cases)) %>% 
  dplyr::mutate(log_cases = 2 * log(cases)/ 2) %>% 
  dplyr::ungroup()

# Split the data for types
data_for_plot.split <- data_for_plot %>% split(data_for_plot$type)

# colors for bubbles
pal <- colorFactor(c("green","red"), domain = c("confirmed", "deaths"))

# initialize a map object
map_object <- leaflet() %>% addProviderTiles(providers$Stamen.Toner)


names(data_for_plot.split) %>%
  purrr::walk( function(df) {
    map_object <<- map_object %>%
      addCircleMarkers(data= data_for_plot.split[[df]],
                       lng=~Long, lat=~Lat,
                       #                 label=~as.character(cases),
                       color = ~pal(type),
                       stroke = FALSE,
                       fillOpacity = 0.8,
                       radius = ~log_cases,
                       popup =  leafpop::popupTable(data_for_plot.split[[df]],
                                                    feature.id = FALSE,
                                                    row.numbers = FALSE,
                                                    zcol=c("type","cases","Country.Region","Province.State")),
                       group = df,
                       #                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto'))
  })

map_object %>%
  addLayersControl(
    overlayGroups = names(data_for_plot.split),
    options = layersControlOptions(collapsed = FALSE) 
  )
}

# Test
leaflet_map_plot(data = data)

data <- get_data()
country <- "US"
# Highlight Generation
text_highlights <- function(data, country){
  
  # determine overall confirmed infections and deaths
  global_outcomes <- data %>% 
    dplyr::filter(date == date %>% tail(1)) %>%
    select(Outcome = type, Cases = number_of_cases) %>%
    mutate(row=row_number()) %>%
    spread(key = Outcome, value = Cases) %>%
    select(confirmed, deaths) %>%
    summarise(total_confirmed = sum(confirmed, na.rm = T),
              total_deaths = sum(deaths, na.rm = T)) %>%
    mutate(Percentage = total_deaths/total_confirmed,
           Percentage = Percentage %>% scales::percent(0.01)) %>%
    mutate(total_confirmed = scales::comma(total_confirmed),
           total_deaths = scales::comma(total_deaths)) 
  
  country_outcomes <- data %>% 
    dplyr::filter(Country.Region == country) %>%
    dplyr::filter(date == date %>% tail(1)) %>%
    select(Outcome = type, Cases = number_of_cases) %>%
    mutate(row=row_number()) %>%
    spread(key = Outcome, value = Cases) %>%
    select(confirmed, deaths) %>%
    summarise(total_confirmed = sum(confirmed, na.rm = T),
              total_deaths = sum(deaths, na.rm = T)) %>%
    mutate(Percentage = total_deaths/total_confirmed,
           Percentage = Percentage %>% scales::percent(0.01)) %>%
    mutate(total_confirmed = scales::comma(total_confirmed),
           total_deaths = scales::comma(total_deaths)) %>%
    mutate(country = country)
  
  str_glue("On a global basis, there have been {global_outcomes$total_confirmed} confirmed cases of coronavirus infection. 
           The number of deaths due to coronavirus are estimated at {global_outcomes$total_deaths}. On a percent basis, 
           the death rate globally is {global_outcomes$Percentage}. 
           
           In the {country_outcomes$country}, the confirmed cases and deaths amount to {country_outcomes$total_confirmed} and {country_outcomes$total_deaths} respectively. The
           mortality rate is {country_outcomes$Percentage}.")
}

# Test 
text_highlights(data, country = "Pakistan")


# Store the function
dump(list = c("get_data", 
              "plot_country_profile_ts", 
              "total_cases_dt", 
              "plot_map", 
              "plotly_function",
              "leaflet_map_plot",
              "text_highlights"), file = "00_analysis/scripts.R", append = FALSE)

  















