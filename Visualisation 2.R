library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# Read and combine the data
# Read only the first 1000 rows from each CSV file
# nrows_to_read <- 100
# goldfinch_data <- read.csv("NABBP_2022_grp_39.csv", nrows = nrows_to_read)
# sparrow_data <- read.csv("NABBP_2022_grp_45.csv", nrows = nrows_to_read)
#goldfinch_data <- read.csv("NABBP_2022_grp_39.csv")
#sparrow_data <- read.csv("NABBP_2022_grp_45.csv")
# goldfinch_data$species <- "American Goldfinch"
# sparrow_data$species <- "Song Sparrow"
# dataset <- rbind(goldfinch_data, sparrow_data)

# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(0)

# Define the number of records for each bird
num_records <- 10

# Bird details
birds <- c("Bird1", "Bird2", "Bird3", "Bird4")
species_ids <- c("S001", "S002", "S003", "S004")

# Migration paths (start and end coordinates)
# Houston to Toronto, Los Angeles to Madison, Dallas to DC, Denver to Seattle
coords <- list(
    c(29.7604, -95.3698, 43.6511, -79.3470),  # Houston to Toronto
    c(34.0522, -118.2437, 43.0731, -89.4012), # Los Angeles to Madison
    c(32.7767, -96.7970, 38.9072, -77.0369),  # Dallas to DC
    c(39.7392, -104.9903, 47.6062, -122.3321) # Denver to Seattle
)

# Function to generate migration data for a single bird
generate_migration_data <- function(bird_id, species_id, start_lat, start_lon, end_lat, end_lon, num_records) {
    lat_changes <- seq(from = start_lat, to = end_lat, length.out = num_records)
    lon_changes <- seq(from = start_lon, to = end_lon, length.out = num_records)
    
    data.frame(
        BAND = rep(bird_id, num_records),
        species_id = rep(species_id, num_records),
        LAT_DD = lat_changes,
        LON_DD = lon_changes,
        EVENT_DATE = seq(as.Date("2023-01-01"), by = "day", length.out = num_records)
    )
}

# Generate data for each bird and combine
all_bird_data <- do.call(rbind, lapply(1:length(birds), function(i) {
    generate_migration_data(
        birds[i], 
        species_ids[i], 
        coords[[i]][1], coords[[i]][2], 
        coords[[i]][3], coords[[i]][4], 
        num_records
    )
}))

# View the dataset
print(all_bird_data)
dataset = all_bird_data
# View the dataset

# Convert EVENT_DATE to Date format
#dataset$EVENT_DATE <- as.Date(dataset$EVENT_DATE, format = "%m/%d/%Y")

ui <- fluidPage(
    titlePanel("Bird Migration Trajectories"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("birdID", "BAND", choices = unique(dataset$BAND), selected = NULL, multiple = TRUE),
            selectInput("species", "Species", choices = unique(dataset$species), selected = NULL, multiple = TRUE),
            dateRangeInput("dateRange", "Date Range", start = min(dataset$EVENT_DATE), end = max(dataset$EVENT_DATE))
        ),
        
        mainPanel(
            leafletOutput("map")
        )
    )
)

server <- function(input, output) {
    
        # Reactive expression to filter data
        filteredData <- reactive({
            filtered_dataset <- dataset
            
            print(head(filtered_dataset)) # Debugging: Print the head of the dataset
            
            # Apply filters only if the user has selected something
            if (!is.null(input$birdID) && length(input$birdID) > 0) {
                filtered_dataset <- filtered_dataset %>% filter(BAND %in% input$birdID)
            }
            if (!is.null(input$species) && length(input$species) > 0) {
                filtered_dataset <- filtered_dataset %>% filter(SPECIES_ID %in% input$species)
            }
            if (!is.null(input$dateRange)) {
                filtered_dataset <- filtered_dataset %>% filter(EVENT_DATE >= input$dateRange[1], EVENT_DATE <= input$dateRange[2])
            }
            
            print(head(filtered_dataset)) # Debugging: Print the head after filtering
            
            return(filtered_dataset)
        })
        
    
    # ... [reactive expression filteredData]

        output$map <- renderLeaflet({
        data <- filteredData()
        
        # Initialize the map with a broader view
        map <- leaflet() %>% addTiles() %>% setView(lng = -98, lat = 39.5, zoom = 4)
        
        if (nrow(data) > 0) {
            colorPalette <- colorFactor(colorRampPalette(brewer.pal(9, "Set1"))(length(unique(data$BAND))), domain = data$BAND)
            for(bird in unique(data$BAND)) {
                birdData <- data[data$BAND == bird,]
                if(nrow(birdData) > 1 && !any(is.na(birdData$LON_DD)) && !any(is.na(birdData$LAT_DD))) {
                    map <- addPolylines(map, data = birdData, ~LON_DD, ~LAT_DD, color = colorPalette(bird), weight = 4, opacity = 0.9, group = bird, dashArray = "5, 5")
                    map <- addCircleMarkers(map, data = birdData[1,], ~LON_DD, ~LAT_DD, radius = 6, group = bird, color = "green", fillColor = "green")
                    map <- addCircleMarkers(map, data = birdData[nrow(birdData),], ~LON_DD, ~LAT_DD, radius = 6, group = bird, color = "red", fillColor = "red")
                }
            }
        }
        
        return(map)
    })
}



shinyApp(ui = ui, server = server)
