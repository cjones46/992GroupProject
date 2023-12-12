library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(maps)
library(sf)
library(viridis)
library(sp)
library(biscale)
library(cowplot)
library(fipio)
library(geojsonio)
library(ggthemes)
library(rsconnect)


# Load necessary libraries
nrows_to_read <- 100
total_columns <- 23 # there are 23 columns in total in the CSV file
classes <- rep("NULL", total_columns)

# Set the classes for the columns you want to keep
classes[c(1, 2, 5, 6, 7, 8, 9, 11, 12, 20, 21)] <- "character" # Adjust the class as needed

#Read only the first 1000 rows from each CSV file
goldfinch_data <- read.csv("NABBP_2022_grp_39.csv", nrows = nrows_to_read, colClasses = classes)
#goldfinch_data <- read.csv("https://www.sciencebase.gov/catalog/file/get/632b2d7bd34e71c6d67bc161?f=__disk__88%2F55%2F45%2F885545059519d0557ea466b8cf88fa86696da3a4", colClasses = classes)
sparrow_data <- read.csv("NABBP_2022_grp_45.csv", nrows = nrows_to_read, colClasses = classes)
#sparrow_data <- read.csv("https://www.sciencebase.gov/catalog/file/get/632b2d7bd34e71c6d67bc161?f=__disk__56%2F48%2F68%2F564868b7682640729949bbe349ff71c756c0c329", colClasses = classes)


# Or Read and combine the data
# goldfinch_data <- read.csv("NABBP_2022_grp_39.csv", colClasses = classes)
# sparrow_data <- read.csv("NABBP_2022_grp_45.csv", colClasses = classes)

# Add speicies column
goldfinch_data$species <- "American Goldfinch"
sparrow_data$species <- "Song Sparrow"
dataset <- rbind(goldfinch_data, sparrow_data)

dataset$EVENT_DATE <- as.Date(dataset$EVENT_DATE, format = "%m/%d/%Y")

dataset$LAT_DD <- as.numeric(dataset$LAT_DD)
dataset$LON_DD <- as.numeric(dataset$LON_DD)
dataset$EVENT_YEAR <- as.integer(dataset$EVENT_YEAR)
dataset$EVENT_MONTH <- as.integer(dataset$EVENT_MONTH)
dataset$EVENT_DAY <- as.integer(dataset$EVENT_DAY)

# Removing rows with NA in EVENT_DATE
dataset <- dataset[!is.na(dataset$EVENT_DATE), ]
dataset <- dataset[!is.na(dataset$LON_DD), ]
dataset <- dataset[!is.na(dataset$LAT_DD), ]

# US states
states_us <- ne_states(country = "united states of america", returnclass = "sf") %>%
    filter(!(name %in% c("Alaska", "Hawaii", "District of Columbia")))  # Excluding non-continental states

# Canadian provinces
provinces_canada <- ne_states(country = "canada", returnclass = "sf")

# Combine US states and Canadian provinces
states <- rbind(states_us, provinces_canada)

#states <- ne_states(country = "united states of america", returnclass = "sf")

# Transform bird data to an sf object
bird_locations_sf <- st_as_sf(dataset, coords = c("LON_DD", "LAT_DD"), crs = st_crs(4326))

# Perform spatial join
bird_locations_with_state_or_province <- st_join(bird_locations_sf, states)

# Add state or province name back to the original dataset
dataset$State <- bird_locations_with_state_or_province$name



allStateTemps <- read.csv("https://raw.githubusercontent.com/cjones46/992GroupProject/main/allStateTemps.csv")
statePolygons = ne_states(country = "united states of america", returnclass = "sf") %>% dplyr::filter(!(name %in% c("Alaska", "Hawaii", "District of Columbia")))



########################################## SAM'S PROCESSING ####################################

# Read in raw bird data
# birds = bind_rows(read_csv("https://www.sciencebase.gov/catalog/file/get/632b2d7bd34e71c6d67bc161?f=__disk__88%2F55%2F45%2F885545059519d0557ea466b8cf88fa86696da3a4", nrows = nrows_to_read) %>% 
#                       mutate(bird_type = "American Goldfinch"),
#                   read_csv("https://www.sciencebase.gov/catalog/file/get/632b2d7bd34e71c6d67bc161?f=__disk__56%2F48%2F68%2F564868b7682640729949bbe349ff71c756c0c329", nrows = nrows_to_read) %>% 
#                       mutate(bird_type = "Song Sparrow"))

birds = bind_rows(read_csv("NABBP_2022_grp_39.csv") %>% 
                    mutate(bird_type = "American Goldfinch"),
                  read_csv("NABBP_2022_grp_45.csv") %>% 
                    mutate(bird_type = "Song Sparrow"))

# Latitude range: 50-25N
# Longitude range: 125-65W
bird_locations = birds %>% 
    select(BAND, EVENT_DATE, EVENT_YEAR, EVENT_MONTH, LAT_DD, LON_DD, bird_type) %>% 
    mutate(
        EVENT_MONTH = as.numeric(EVENT_MONTH),
        EVENT_YEAR = as.numeric(EVENT_YEAR)
    ) %>% 
    filter(
        EVENT_YEAR >= 2000,
        LAT_DD <= 50 & LAT_DD >= 25, 
        LON_DD >= -125 & LON_DD <= -65
    )

# In the future, would like to exclude points which are outside the USA - will leave this for Milestone 3, having trouble with spatial features

# write_csv(bird_locations, "bird_locations.csv")
# 
# bird_locations = read_csv("bird_locations.csv")

stateTemps = read_csv("https://raw.githubusercontent.com/cjones46/992GroupProject/main/allStateTemps.csv")


# Load U.S. states boundaries data, create an SF from the bird latitude and longitude, and join the two to map to states
us_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
    select(geometry,name) %>%
    filter(!name %in% c("District of Columbia","Alaska","Hawaii"))
bird_sf_data <- st_as_sf(bird_locations, coords = c("LON_DD", "LAT_DD"), crs = st_crs(us_states))
result <- st_join(bird_sf_data, us_states) %>%
    filter(!is.na(name)) # Remove observations that don't map to the continental U.S. states

# Count of birds by month and year
subset_data <- result %>%
    group_by(EVENT_YEAR, EVENT_MONTH, bird_type) %>%
    summarize(total_bird_count = n()) %>%
    ungroup() 
subset_data <- as.data.frame(subset_data) %>%
    select(EVENT_YEAR,EVENT_MONTH,bird_type,total_bird_count)

# Count of birds by month, year, and state
subset_data_state <- result %>%
    group_by(EVENT_YEAR, EVENT_MONTH, bird_type, name) %>%
    summarize(total_bird_count_state = n()) %>%
    ungroup()
subset_data_state <- as.data.frame(subset_data_state) %>%
    select(EVENT_YEAR,EVENT_MONTH,bird_type,name,total_bird_count_state)

# Merge the two dataframes with some cleaning
data_joined <- left_join(subset_data_state, subset_data, by = c("EVENT_YEAR","EVENT_MONTH","bird_type")) %>%
    left_join(., stateTemps, by = c("EVENT_YEAR"= "year", "EVENT_MONTH" = "month_num", "name")) %>% 
    mutate(pct_count = total_bird_count_state/total_bird_count) %>%
    filter(!is.na(name) & !is.na(Value)) %>%
    rename("state" = "name",
           "year" = "EVENT_YEAR",
           "month" = "EVENT_MONTH",
           "Temperature" = "Value") %>%
    left_join(.,us_states, by = c("state" = "name"))


# Define graphing function filtered to year, month, species
graph_filtered = function (inYear,inMonth,inSpecies) {
    data_filtered <- data_joined %>%
        filter(year == inYear, month == inMonth, bird_type == inSpecies)
    
    # Add a requirement for the filtered dataset to be non-empty
    req(nrow(data_filtered) > 0)
    
    # Define bivariate scale
    data <- bi_class(data_filtered, x = Temperature, y = pct_count, style = "equal", dim = 3)
    
    map <- ggplot() + 
        geom_sf(data = data, mapping = aes(fill = bi_class, geometry = geometry), color = "white", size = 0.1, show.legend = FALSE) +
        geom_sf(data = us_states, color = "black", fill = NA) +
        bi_scale_fill(pal = "GrPink", dim = 3) +
        labs(
            subtitle = paste(inMonth, "/", inYear)
        ) +
        bi_theme()
    
    legend <- bi_legend(pal = "GrPink",
                        dim = 3,
                        xlab = "Temperature",
                        ylab = "Population Share",
                        size = 10)
    
    finalPlot <- ggdraw() +
        draw_plot(map, 0, 0, 1, 1) +
        draw_plot(legend, 0, 0, 0.3, 0.3)
    
    return(finalPlot)
    
}

filtered_bar = function (inYear, inMonth, inSpecies, inState) {
    
    month_list = c("January","Februrary","March","April","May","June","July","August","September","October","November","December")
    
    data_filtered <- data_joined %>%
        filter(bird_type == inSpecies, state == inState) %>%
        group_by(year) %>%
        mutate(annual_pop = sum(total_bird_count_state)) %>%
        ungroup() %>%
        filter(month == inMonth, bird_type == inSpecies, state == inState)
    
    bar_plot <- ggplot(data_filtered) +
        geom_col(aes(year, annual_pop, fill = "Annual Population"), alpha = 0.5) +
        geom_col(aes(year, total_bird_count_state, fill = "Month Population")) +
        scale_x_continuous(breaks = round(seq(2000, 2022, by = 1), 2)) +
        labs(
            title = paste0("Population of ", inSpecies, " in ", inState),
            subtitle = paste0(month_list[inMonth], " vs. Annual"),
            y = "Population",
            x = "Year",
            fill = "# Birds"
        ) +
        scale_fill_manual(values = c("Annual Population" = "#9FA4C5", "Month Population" = "#462C7C")) +
        theme_minimal() +
        theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
            axis.line = element_line(color = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1)
        )
    
    # Add a requirement for the filtered dataset to be non-empty
    req(nrow(data_filtered) > 0)
    
    return(bar_plot)
}

population_choropleth = function(inYear, inMonth, inSpecies) {
    data_filtered <- data_joined %>%
        filter(year == inYear, month == inMonth, bird_type == inSpecies)
    
    # Add a requirement for the filtered dataset to be non-empty
    req(nrow(data_filtered) > 0)
    
    month_list = c("January","Februrary","March","April","May","June","July","August","September","October","November","December")
    
    map <- ggplot(data_filtered) + 
        geom_sf(data = us_states, color = "black", fill = NA) +
        geom_sf(aes(fill = total_bird_count_state, geometry = geometry), color = "white", size = 0.1, show.legend = TRUE) +
        theme_light() +
        labs(
            title = paste0(month_list[inMonth]," ",inYear," ",inSpecies," Population"),
            fill = "Bird Count"
        ) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_fill_viridis()
    
    return(map)
}


# UI Definition
ui <- dashboardPage(
    dashboardHeader(title = "Bird Migration"),
    dashboardSidebar(disable = TRUE), # Disable the sidebar
    dashboardBody(
        tags$head(
        ),
        tabBox(
            # First tab: Bird Species
            tabPanel("Bird Species Summary",
                     sidebarLayout(
                         sidebarPanel(
                             sliderInput("month",
                                         "Month within year:",
                                         min = 1,
                                         max = 12,
                                         value = 1),
                             sliderInput("year",
                                         "Year:",
                                         min = 2000,
                                         max = 2022,
                                         value = 2010, 
                                         sep = ""),
                             selectInput("species_select", label = "Species", choices = unique(data_joined$bird_type), selected = "American Goldfinch"),
                             selectInput("state_select", label = "State", choices = unique(data_joined$state), selected = "California")
                         ),
                         
                         
                         
                         mainPanel(
                             
                             plotOutput("bar_plot"),
                             
                             plotOutput("species_map"),
                             
                             leafletOutput("map")
                         )
                     )
            ),
                     # Second tab: Temperature
            tabPanel("Temperature Overview",
                     fluidRow(
                         column(6,
                                plotOutput("timeSeriesPlot"),
                                plotOutput("tempDistPlot")
                         ),
                         column(6,
                                sliderInput("yearSlider", "Select Year:",
                                            min = 2000, max = 2023,
                                            value = c(2000, 2023),
                                            step = 1),
                                sliderInput("monthSlider", "Select Month:",
                                            min = 1, max = 12,
                                            value = c(1, 12),
                                            step = 1),
                                selectInput("stateSelect", "Select State:",
                                            choices = c("None" = "", unique(allStateTemps$name))),
                                leafletOutput("tempMap")
                         )
                     )
            ),
            # Third tab: Operating Hours Analysis
            tabPanel("Temperature Impact on Migration", 
                     fluidRow(
                         column(6,
                                plotOutput("birdSightingsTempPlot"),
                                plotOutput("birdDistributionHeatmap")
                         ),
                         column(6,
                                sliderInput("yearSlider3", "Select Year:",
                                            min = 2000, max = 2023,
                                            value = c(2000, 2023),
                                            step = 1),
                                
                                sliderInput("monthSlider3", "Select Month:",
                                            min = 1, max = 12,
                                            value = c(1, 12),
                                            step = 1),
                                
                                selectInput("stateSelect3", "Select State:",
                                            choices = c("None" = "", unique(allStateTemps$name))),
                                
                                selectInput("speciesSelect3", "Select Species:",
                                            choices = c("None" = "", unique(dataset$species))),
                                plotOutput("birdMigrationMap")
                         )
                     )
            ),
            # Fourth tab: Summary and Suggestions
            tabPanel("Summary and Suggestions",
                     
                     
                     div(
                         style = "font-family: Arial, sans-serif; padding: 20px;",
                         h3("Summary and Recommendations:"),
                         p("This summary provides insights derived from the analysis of bird migration patterns in relation to changing temperatures."),
                         h4("Key Findings:"),
                         p("The analysis highlights significant correlations between temperature fluctuations and bird migration patterns. Specific trends have been observed in bird species such as ", tags$b("American Goldfinch"), " and ", tags$b("Song Sparrow"), "."),                         h4("Optimal Food Choices:"),
                         h4("Data Sources:"),
                         tags$ul(
                             tags$li(tags$b("Bird location data from ScienceBase Catalog")),
                             tags$li(tags$b("Temperature time series data from NOAA National Centers for Environmental Information"))
                         ),
                         h4("Thank you!"),
                         p("STAT679 - FALL23 - Group 2")
                     )
            ),
            # Specify the width to take full width of the container
            width = 12
        )
    )
)


server <- function(input, output) {
    
    
    
    ###################################################### TAB 1 ###################################################
    
    
    output$species_map <- renderPlot(population_choropleth(input$year, input$month, input$species_select))
    output$bar_plot <- renderPlot(filtered_bar(input$year, input$month, input$species_select, input$state_select))
    
    
    bird_locations$season <- cut(bird_locations$EVENT_MONTH, breaks = c(0, 2, 5, 8, 12), labels = c("Winter", "Spring", "Summer", "Autumn"))
    
    output$map <- renderLeaflet({
        selected_data <- bird_locations %>%
            filter(bird_type == input$species_select, EVENT_YEAR == input$year) %>%
            group_by(BAND, season) %>%
            arrange(EVENT_DATE) %>%
            ungroup()
        req(nrow(selected_data) > 0)
        
        # Specify colors for each season
        pal <- colorFactor(
            palette = c("orange", "lightgreen", "red", "darkblue"), 
            domain = c("Winter", "Spring", "Summer", "Autumn")
        )
        
        leaflet(selected_data) %>%
            addTiles() %>%
            addCircleMarkers(
                lat = ~LAT_DD, lng = ~LON_DD,
                radius = 2,
                color = ~pal(season),
                fillOpacity = 0.5,
                group = "markers"
            ) %>%
            addLegend(
                position = "bottomright",
                pal = pal,
                values = unique(selected_data$season),
                title = "Season"
            )
    })
    
    
    ###################################################### TAB 2 ###################################################
    
    
    ####################### DATA FILTERS
    
    filteredData <- reactive({
        temp_data <- allStateTemps
        
        # Apply state filter only if a state is selected
        if (input$stateSelect != "") {
            temp_data <- temp_data %>% filter(name == input$stateSelect)
        }
        
        # Apply year and month range filters
        temp_data <- temp_data %>%
            filter(year >= input$yearSlider[1] & year <= input$yearSlider[2]) %>%
            filter(month_num >= input$monthSlider[1] & month_num <= input$monthSlider[2])
        
        return(temp_data)
    })
    
    
    
    
    ############################################### MAP 2.1
    
    output$tempMap <- renderLeaflet({
        #data <- allStateTemps
        data <- filteredData()
        
        # Ensure to handle the case when the filtered data is empty
        if(nrow(data) == 0) {
            return(NULL)
        }
        
        # Group and summarize
        stateTempsAvg <- data %>% 
            group_by(name, month_num) %>% 
            summarize(Value = mean(Value), .groups = "drop")  # Adding .groups = "drop"
        
        # Join with state polygons
        g_data <- left_join(statePolygons, stateTempsAvg, by = "name")  # Ensure names match between datasets
        
        # Plot using Leaflet
        leaflet(g_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -98, lat = 39.5, zoom = 4) %>%
            addPolygons(fillColor = ~colorNumeric(palette = "viridis", domain = g_data$Value)(Value), 
                        fillOpacity = 0.7, 
                        color = "#BDBDC3", 
                        weight = 1, 
                        smoothFactor = 0.5,
                        popup = ~paste(name, "<br>", "Temp: ", Value)) %>%
            addLegend("bottomright", 
                      pal = colorNumeric(palette = "viridis", domain = g_data$Value), 
                      values = ~Value,
                      title = "Temperature",
                      opacity = 1)
    })
    
    
    ########################################### TIME SERIES 2.2
    
    output$timeSeriesPlot <- renderPlot({
        # Calculate the monthly and annual statistics
        monthly_stats <- filteredData() %>%
            group_by(month_num) %>%
            summarize(
                avg = mean(Value, na.rm = TRUE),
                std = sd(Value, na.rm = TRUE)
            ) %>%
            ungroup()
        
        annual_avg <- mean(filteredData()$Value, na.rm = TRUE)
        
        # Prepare data for plotting
        plot_data <- data.frame(
            month_num = monthly_stats$month_num,
            avg = monthly_stats$avg,
            lower = monthly_stats$avg - monthly_stats$std,
            upper = monthly_stats$avg + monthly_stats$std
        )
        
        # Create a named vector for the fill colors
        fill_colors <- c("Lower" = "red", "Middle" = "green", "Upper" = "blue")
        
        # Add a new column to identify the ribbon
        plot_data$ribbon <- with(plot_data, ifelse(is.na(lower), "Middle", "Lower"))
        plot_data_upper <- transform(plot_data, ribbon = "Upper", lower = avg)
        
        # Create the area plot
        ggplot() +
            geom_ribbon(data = plot_data, aes(x = month_num, ymin = lower, ymax = avg, fill = ribbon), alpha = 0.3) +
            geom_ribbon(data = plot_data_upper, aes(x = month_num, ymin = avg, ymax = upper, fill = ribbon), alpha = 0.3) +
            geom_line(data = plot_data, aes(x = month_num, y = avg), color = "darkgreen") +
            geom_hline(yintercept = annual_avg, linetype = "dashed", color = "black") +
            geom_text(aes(x = 1, y = annual_avg, label = paste("Annual Avg:", round(annual_avg, 2))), hjust = -0.1, vjust = 1.5) +
            scale_fill_manual(values = fill_colors) +
            labs(title = "Monthly Temperature Statistics",
                 x = "Month", y = "Temperature (F)",
                 fill = "Legend") +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
            ) +
            scale_x_continuous(breaks = 1:12, labels = month.abb)
    })
    
    ##################################################### HISTOGRAM 2.3
    
    output$tempDistPlot <- renderPlot({
        data <- filteredData()
        
        # Calculate mean and standard deviation
        avg <- mean(data$Value, na.rm = TRUE)
        std <- sd(data$Value, na.rm = TRUE)
        
        # Create the histogram
        ggplot(data, aes(x = Value)) +
            geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
            stat_function(fun = dnorm, args = list(mean = avg, sd = std), color = "red", size = 1) +
            geom_vline(xintercept = avg, color = "darkgreen", size = 1, linewidth = 1) +
            geom_vline(xintercept = avg - std, linetype = "dashed", color = "blue", size = 1, linewidth = 1) +
            geom_vline(xintercept = avg + std, linetype = "dashed", color = "blue", size = 1, linewidth = 1) +
            geom_text(aes(x = avg, y = 0, label = paste("Avg:", round(avg, 2))), vjust = -1, hjust = 1.1, color = "darkgreen") +
            geom_text(aes(x = avg - std, y = 0, label = paste("- std: ", round(std, 2))), vjust = -1, hjust = 1.1, color = "blue") +
            geom_text(aes(x = avg + std, y = 0, label = paste("+ std: ", round(std, 2))), vjust = -1, hjust = -0.1, color = "blue") +
            labs(title = "Temperature Distribution",
                 x = "Temperature (F)", y = "Density") +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
            )
    })
    
    ######################################################## TAB 3 ####################################################3
    
    ####################### DATA FILTERS
    
    filteredData3 <- reactive({
      temp_data <- allStateTemps
      
      # Apply state filter only if a state is selected
      if (input$stateSelect3 != "") {
        temp_data <- temp_data %>% filter(name == input$stateSelect3)
      }
      
      # Apply year and month range filters
      temp_data <- temp_data %>%
        filter(year >= input$yearSlider3[1] & year <= input$yearSlider3[2]) %>%
        filter(month_num >= input$monthSlider3[1] & month_num <= input$monthSlider3[2])
      
      return(temp_data)
    })
    
    
    bird_data_filtered <- reactive({
      temp_data <- dataset
      
      # Apply state filter only if a species is selected
      if (input$speciesSelect3 != "") {
        temp_data <- temp_data %>% filter(species == input$speciesSelect3)
      }
      
      return(temp_data)
    })
    
    ############################################# MAP 3.1
    
    output$birdMigrationMap <- renderPlot({
      birdsAvg <- bird_data_filtered() %>%
        group_by(species, EVENT_MONTH) %>%
        summarize(LAT = mean(LAT_DD), LON = mean(LON_DD), .groups = "drop") %>%
        rename(month_num = EVENT_MONTH)
      
      birds_start <- birdsAvg %>% filter(month_num == 1)
      
      stateTempsAvg <- filteredData3() %>%
        group_by(name, month_num) %>%
        summarize(Value = mean(Value), .groups = "drop")
      
      g_data <- left_join(statePolygons, stateTempsAvg)
      
      ggplot(g_data) +
        geom_sf(aes(fill = Value)) +
        geom_point(data = birds_start,
                   mapping = aes(LON, LAT, color = species),
                   size = 5) +
        geom_line(data = birdsAvg,
                  mapping = aes(LON, LAT, color = species),
                  size = 2,
                  arrow = arrow(ends = "last")) +
        scale_color_manual(values = c("gold", "burlywood4")) +
        scale_fill_viridis_c(option = "inferno", limits = c(0, 100)) +
        labs(
          title = "Bird Sightings and Average Temperature",
          subtitle = "Average across 2000-2022, Month-By-Month Migration",
          fill = "Avg. Temp (F)",
          color = "Bird Type"
        ) +
        theme_void()
    })
    
    
    ####################################### 3.2 bird sighting temp plot
    
    output$birdSightingsTempPlot <- renderPlot({
      # Assuming bird_locations and stateTemps are already prepared and available
      
      # Monthly bird sightings count
      bird_sightings_monthly <- bird_data_filtered() %>%
        group_by(EVENT_MONTH) %>%
        summarize(Sightings = n(), .groups = "drop")
      
      # Monthly average temperature
      monthly_avg_temp <- filteredData3() %>%
        group_by(month_num) %>%
        summarize(AvgTemp = mean(Value, na.rm = TRUE), .groups = "drop")
      
      # Merging the two datasets
      merged_data <- left_join(bird_sightings_monthly, monthly_avg_temp, by = c("EVENT_MONTH" = "month_num"))
      
      ggplot(merged_data, aes(x = AvgTemp, y = Sightings)) +
        geom_point(aes(color = factor(EVENT_MONTH)), size = 3) +
        geom_smooth(method = "lm", se = FALSE) +
        scale_color_brewer(palette = "Set1", name = "Month") +
        labs(title = "Monthly Bird Sightings vs. Average Temperature",
             x = "Average Temperature (F)", y = "Number of Sightings",
             color = "Month") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")
        )
    })
    
    
    ################################# 3.3
    
    output$birdDistributionHeatmap <- renderPlot({
      
      # Preparing data for heatmap
      bird_distribution <- bird_data_filtered() %>%
        group_by(Species = species, State = State) %>%
        summarize(Sightings = n(), .groups = "drop")
      
      ggplot(bird_distribution, aes(x = State, y = Species, fill = Sightings)) +
        geom_tile() +
        scale_fill_viridis_c(name = "Number of Sightings") +
        labs(title = "Bird Species Distribution Heatmap",
             x = "State", y = "Species") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
    
    
}



# Run the application
shinyApp(ui = ui, server = server)
