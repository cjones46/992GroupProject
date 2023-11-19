#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("American Goldfinch and Song Sparrow Sightings"),

    # Sidebar with a slider input for number of bins
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
                        value = 2022)
              
            ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("mapPlot")
        )
        )

        
    )
