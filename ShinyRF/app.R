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
      }
      body, html {
        margin: 0;
        padding: 0;
        height: 100%;
        overflow: hidden;
      }
    "))
    ),
    
    # Fullscreen map with an absolute panel for controls
    absolutePanel(
        top = 10, left = 10, width = 300, draggable = TRUE,
        style = "z-index: 1000; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px;",
        fileInput("rasterFile", "Choose a Raster File", 
                  accept = c(".tif", ".asc")),
        actionButton("loadRaster", "Load Raster"),
        uiOutput("bandSelectionUI"),
        actionButton("createRGB", "Create RGB Composition"),
        textInput("shapeFileName", "Enter a name for the Shapefile"),
        textInput("columns", "Enter column names (comma separated)"),
        actionButton("createShapefile", "Create Shapefile"),
        actionButton("saveShapefile", "Save Shapefile", disabled = TRUE),
        helpText("Supported formats: .tif, .asc, etc.")
    ),
    
    # Map output
    leafletOutput("map", width = "100%", height = "100%")
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values to store polygons and attributes
    drawnPolygons <- reactiveVal(NULL)
    shapefileStructure <- reactiveVal(NULL)
    rasterData <- reactiveVal(NULL)
    
    # Disable drawing initially
    observe({
        leafletProxy("map") %>%
            removeDrawToolbar()
    })
    
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
        
        redBand <- rasterData()[[as.numeric(input$redBand)]]
        greenBand <- rasterData()[[as.numeric(input$greenBand)]]
        blueBand <- rasterData()[[as.numeric(input$blueBand)]]
        
        # Normalize and stack the selected bands to create an RGB composite
        rgbRaster <- c(redBand, greenBand, blueBand)
        RGB(rgbRaster) <- 1:3
        
        rgbRaster_col <- colorize(rgbRaster, "col", stretch = "hist")
        
        output$map <- renderLeaflet({
            leaflet(options = leafletOptions(maxZoom = 28)) %>%
                addTiles() %>%
                addRasterImage(rgbRaster_col, group = "RGB Raster")
        })
    })
    
    # Create the shapefile structure with columns
    observeEvent(input$createShapefile, {
        req(input$shapeFileName, input$columns)
        
        # Extract columns from input and store the shapefile structure
        columns <- strsplit(input$columns, ",\\s*")[[1]]
        shapefileStructure(list(name = input$shapeFileName, columns = columns))
        
        showNotification("Shapefile created. You can now draw polygons.", type = "message")
        
        # Enable drawing once shapefile structure is created
        leafletProxy("map") %>%
            addDrawToolbar(
                targetGroup = "drawnPoly",
                polylineOptions = FALSE,
                polygonOptions = TRUE,
                circleOptions = FALSE,
                rectangleOptions = FALSE,
                markerOptions = FALSE,
                editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = NULL)
            )
    })
    
    # Capture drawn polygons and ask for column values
    observeEvent(input$map_draw_new_feature, {
        req(shapefileStructure())
        
        # Extract coordinates from the drawn polygon
        newFeature <- input$map_draw_new_feature
        coords <- newFeature$geometry$coordinates[[1]]
        coords_matrix <- do.call(rbind, lapply(coords, function(x) c(x[[1]], x[[2]])))
        polygon <- st_sf(geometry = st_sfc(st_polygon(list(coords_matrix)), crs = 4326))
        
        # Show modal to input values for each column
        columns <- shapefileStructure()$columns
        columnInputs <- lapply(columns, function(col) {
            textInput(paste0("col_", col), label = paste("Enter value for", col))
        })
        
        showModal(modalDialog(
            title = "Enter Values for the Columns",
            columnInputs,
            footer = tagList(
                actionButton("submitCols", "Submit"),
                modalButton("Cancel")
            )
        ))
        
        # Store polygon with column data after submission
        observeEvent(input$submitCols, {
            # Create a data frame for the attribute values
            attrValues <- sapply(columns, function(col) input[[paste0("col_", col)]], USE.NAMES = TRUE)
            attrDF <- data.frame(t(attrValues), stringsAsFactors = FALSE)
            
            # Add the attributes to the polygon
            polygon_with_attrs <- cbind(polygon, attrDF)
            
            # Append the new polygon to the existing set
            currentPolygons <- drawnPolygons()
            drawnPolygons(rbind(currentPolygons, polygon_with_attrs))
            
            # Close the modal
            removeModal()
            
            showNotification("Polygon added with attributes.")
            
            # Enable saving shapefile after at least one polygon is added
            updateActionButton(session, "saveShapefile", disabled = FALSE)
        })
    })
    
    # Save the shapefile
    observeEvent(input$saveShapefile, {
        req(drawnPolygons(), shapefileStructure())
        
        polygons <- drawnPolygons()
        shapefilePath <- file.path(getwd(), paste0(shapefileStructure()$name, ".shp"))
        
        # Save the shapefile
        st_write(polygons, shapefilePath, delete_layer = TRUE)
        showNotification(paste("Shapefile saved at:", shapefilePath), type = "message")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
