library(shiny)
library(suncalc)
library(plotrix)
library(sonicscrewdriver)
library(schite)

year <- as.POSIXlt(Sys.Date())$year + 1900

ui <- fluidPage(
  # Application title
  titlePanel("Daylight Information"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("date",
                  "Date:",
                  min = as.Date(paste0(year,"-01-01"),"%Y-%m-%d"),
                  max = as.Date(paste0(year,"-12-31"),"%Y-%m-%d"),
                  value=Sys.Date(),
                  timeFormat="%Y-%m-%d",
                  step = 3,
                  animate = animationOptions(interval = 250,loop=TRUE)
      ),
      selectInput("loc",
                  "Location:",
                  c("NHM", "York", "Houndwood", "Lyme Regis", "Rovaniemi", "Austin", "Sydney"),
                  selected = "York",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      checkboxGroupInput("times",
                         "Time of day:",
                         choices = c(
                           "Sunrise",
                           "Sunset",
                           "Solar Noon",
                           "Civil Twilight",
                           "Nautical Twilight",
                           "Astronomical Twilight",
                           "Night",
                           "Nadir"
                         ),
                         selected = c(
                           "Sunrise",
                           "Sunset",
                           "Civil Twilight",
                           "Nautical Twilight",
                           "Astronomical Twilight",
                           "Night"
                         ),
      ),
      selectInput("display",
                  "Display:",
                  c("Main", "Core", 'Ring'),
                  selected = "Main",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      checkboxInput("legend",
                    "Show legend",
                    value=FALSE
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tabsetPanel(type = "tabs",
                  tabPanel("How to cite", htmlOutput("citation")),
                  tabPanel("Data Calculation", htmlOutput("citecalc")),
                  tabPanel("Data Visualisation", htmlOutput("citevis")),
                  tabPanel("Misc", htmlOutput("citmisc"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  citation <- list(
    cite_bibentry(
      bibentry(
        bibtype="Misc",
        title="schite: citations for shiny",
        author="Ed Baker",
        url="https://github.com/edwbaker/schite",
        year=2022
      )
    )
  )
  output$citation <- citationTabUI(citation)

  citcalc <- list(
    cite_r_package("suncalc")
  )
  output$citecalc <- citationTabUI(citcalc)

  citvis <- list(
    cite_r_package("sonicscrewdriver"),
    cite_r_package("plotrix")
  )
  output$citevis <- citationTabUI(citvis)

  citmisc <- list(
    cite_r_package("shiny")
  )
  output$citmisc <- citationTabUI(citmisc)

  output$distPlot <- renderPlot({
    if (input$loc == "NHM") {
      lat = 51.49
      lon = -0.17
    }
    if (input$loc == "York") {
      lat = 53.95
      lon = -1.10
    }
    if (input$loc == "Houndwood") {
      lat = 55.87
      lon = -2.25
    }
    if (input$loc == "Lyme Regis") {
      lat = 50.725
      lon = -2.940
    }
    if (input$loc == "Rovaniemi") {
      lat = 66.5
      lon = 25.733333
    }
    if (input$loc == "Austin") {
      lat = 30.267222
      lon = -97.743056
    }
    if (input$loc == "Sydney") {
      lat = -33.867778
      lon = 151.21
    }

    if (input$display == "Main") {
      inner = 0
      outer = 2
    }
    if (input$display == "Core") {
      inner = 0
      outer = 1
    }
    if (input$display == "Ring") {
      inner = 1.75
      outer = 2
    }
    dielPlot(as.POSIXct(input$date),lat,lon,c(inner,outer),input$times,legend=input$legend)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
