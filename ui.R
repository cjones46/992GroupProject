library(shiny)

fluidPage(

    titlePanel("American Goldfinch and Song Sparrow Sightings"),

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

        mainPanel(
          plotOutput("mapPlot")
        )
      )
)
