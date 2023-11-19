library(shiny)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(maps)
library(sf)

# Bird locations
bird_locations = read_csv("bird_locations.csv")

# State temperatures over time
stateTemps = read_csv("allStateTemps.csv")

# State polygon objects for spatial plotting
statePolygons = ne_states(country = "united states of america", returnclass = "sf") %>% filter(!(name %in% c("Alaska", "Hawaii", "District of Columbia")))

# Plotting Functions
graphingFunction = function(g_data, b_data, m, y) {
  ggplot(g_data) + 
    geom_sf(aes(fill = Value)) +
    geom_point(data = b_data,
               mapping = aes(x = LON_DD, y = LAT_DD, color = bird_type),
               size = 3) +
    scale_color_manual(values = c("gold", "burlywood4")) +
    scale_fill_viridis_c(option = "inferno", limits = c(0, 100)) +
    labs(
      title = "Bird Sightings and Average Temperature",
      subtitle = paste(month(m, label = TRUE, abbr = FALSE), y),
      fill = "Avg. Temp (F)",
      color = "Bird Type"
    ) +
    theme_void()
}


graphWrapper = function(yearInput, monthInput) {
  graphingData = left_join(statePolygons, 
                           stateTemps %>% 
                             filter(year == yearInput,
                                    month_num == monthInput)
  )
  
  
  bird_locations_selected = bird_locations %>% 
    filter(EVENT_YEAR == yearInput,
           EVENT_MONTH == monthInput)
  
  return(graphingFunction(graphingData, bird_locations_selected, monthInput, yearInput))
}

# Define server logic required to draw a histogram
function(input, output, session) {

    output$mapPlot <- renderPlot({graphWrapper(input$year, input$month)})

}
