library(shiny)
library(leaflet)
library(terra)
library(sf)
library(leaflet.extras)

# Increase the maximum file upload size (100 MB in this example)
options(shiny.maxRequestSize = 100*1024^2)

# Define UI
ui <- fluidPage(
    # Add custom CSS to make the map take up the entire screen
    tags$head(
        tags$style(HTML("
      #map {
        height: 100vh !important;
        width: 100vw !important;
        margin: 0;
        position: absolute;
        top: 0;
        left: 0;
        z-index: 1;
      }
      body, html {
        margin: 0;
        padding: 0;
        height: 100%;
        overflow: hidden;
      }
      .panel-container {
        position: relative;
        z-index: 1000;
      }
    "))
    ),
    
    # Map output (always visible)
    leafletOutput("map", width = "100%", height = "100%"),
    
    # Tab panel layout with control panels
    div(
        class = "panel-container",
        tabsetPanel(
            tabPanel("RGB Image",
                     # Absolute panel for image controls
                     absolutePanel(
                         top = 50, left = 30, width = 300, draggable = TRUE,
                         style = "z-index: 1000; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px;",
                         fileInput("rasterFile", "Choose a Raster File", 
                                   accept = c(".tif", ".asc")),
                         actionButton("loadRaster", "Load Raster"),
                         uiOutput("bandSelectionUI"),
                         actionButton("createRGB", "Create RGB Composition"),
                         helpText("Supported formats: .tif, .asc, etc.")
                     )
            ),
            
            tabPanel("Shapefile Creation",
                     # Absolute panel for shapefile controls
                     absolutePanel(
                         top = 50, left = 30, width = 300, draggable = TRUE,
                         style = "z-index: 1000; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px;",
                         textInput("shapeFileName", "Enter a name for the Shapefile"),
                         textInput("columns", "Enter column names (comma separated)"),
                         actionButton("createShapefile", "Create Shapefile"),
                         actionButton("saveShapefile", "Save Shapefile", disabled = TRUE)
                     )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values to store raster and other data
    rasterData <- reactiveVal(NULL)
    
    #### MAP OUTPUT ####
    
    # Initialize a blank map that remains visible at all times

    
    #### RGB IMAGE TAB ####
    
    # Load raster data and create UI for band selection
    observeEvent(input$loadRaster, {
        req(input$rasterFile)
        
        # Load raster using terra
        rasterFile <- tryCatch({
            terra::rast(input$rasterFile$datapath)
        }, error = function(e) {
            showNotification("Error loading raster. Check the file format.", type = "error")
            return(NULL)
        })
        
        rasterData(rasterFile)
        
        # Create UI for selecting bands
        if (!is.null(rasterFile)) {
            output$bandSelectionUI <- renderUI({
                numBands <- terra::nlyr(rasterFile)
                tagList(
                    selectInput("redBand", "Select Red Band", choices = 1:numBands),
                    selectInput("greenBand", "Select Green Band", choices = 1:numBands),
                    selectInput("blueBand", "Select Blue Band", choices = 1:numBands)
                )
            })
        }
    })
    
    # Create RGB composition and display it on the map
    observeEvent(input$createRGB, {
        req(rasterData(), input$redBand, input$greenBand, input$blueBand)
        
        # Extract the selected bands
        redBand <- rasterData()[[as.numeric(input$redBand)]]
        greenBand <- rasterData()[[as.numeric(input$greenBand)]]
        blueBand <- rasterData()[[as.numeric(input$blueBand)]]
        
        # Try creating an RGB composite, catch errors
        tryCatch({
            # Stack the selected bands to create an RGB composite
            rgbRaster <- c(redBand, greenBand, blueBand)
            RGB(rgbRaster) <- 1:3  # Assign RGB channels
            
            # Add to leaflet map
            leafletProxy("map") %>%
                addTiles() %>% 
                # clearImages() %>%
                addRasterImage(rgbRaster) 
            
            showNotification("RGB Image created successfully!", type = "message")
        }, error = function(e) {
            showNotification(paste("Error creating RGB composition:", e$message), type = "error")
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
