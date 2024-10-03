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
        z-index: 1000;
      }
    "))
  ),
  
  # Layout with sidebar for controls and main panel for map
  sidebarLayout(
    # Sidebar with two tabs for controls
    sidebarPanel(
      width = 3,  # Adjust width of sidebar if needed
      tabsetPanel(
        tabPanel("Raster",
                 # Panel for raster controls
                 fileInput("rasterFile", "Choose a Raster File", 
                           accept = c(".tif", ".asc")),
                 actionButton("loadRaster", "Load Raster"),
                 uiOutput("bandSelectionUI"),
                 actionButton("createRGB", "Create RGB Composition"),
                 helpText("Supported formats: .tif, .asc, etc.")
        ),
        
        tabPanel("Shapefile",
                 # Panel for shapefile controls
                 textInput("shapeFileName", "Enter a name for the Shapefile"),
                 textInput("columns", "Enter column names (comma separated)"),
                 actionButton("createShapefile", "Create Shapefile"),
                 actionButton("saveShapefile", "Save Shapefile", disabled = TRUE)
        )
      )
    ),
    
    # Main panel for leaflet map
    mainPanel(
      leafletOutput("map", width = "100%", height = "100%")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store raster, polygons and attributes
  rasterData <- reactiveVal(NULL)
  drawnPolygons <- reactiveVal(NULL)
  shapefileStructure <- reactiveVal(NULL)
  currentPolygon <- reactiveVal(NULL)  # To store the current polygon being drawn
  
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
      rgbRaster_col <- colorize(rgbRaster, "col", stretch = "hist")
      
      # Add to leaflet map
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(maxZoom = 28)) %>%
          addTiles() %>%
          addRasterImage(rgbRaster_col, group = "RGB Raster")
      })
      
      showNotification("RGB Image created successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error creating RGB composition:", e$message), type = "error")
    })
  })
  
  #### SHAPEFILE CREATION TAB ####
  
  # Disable drawing initially
  observe({
    leafletProxy("map") %>%
      removeDrawToolbar()
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
    
    currentPolygon(polygon)  # Store the polygon being drawn
    
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
  })
  
  # Store polygon with column data after submission
  observeEvent(input$submitCols, {
    req(currentPolygon(), shapefileStructure())
    
    # Get the columns and attribute values
    columns <- shapefileStructure()$columns
    attrValues <- sapply(columns, function(col) input[[paste0("col_", col)]], USE.NAMES = TRUE)
    attrDF <- data.frame(t(attrValues), stringsAsFactors = FALSE)
    
    # Add the attributes to the current polygon
    polygon_with_attrs <- cbind(currentPolygon(), attrDF)
    
    # Append the new polygon to the existing set
    currentPolygons <- drawnPolygons()
    drawnPolygons(rbind(currentPolygons, polygon_with_attrs))
    
    # Close the modal
    removeModal()
    
    showNotification("Polygon added with attributes.")
    
    # Enable saving shapefile after at least one polygon is added
    updateActionButton(session, "saveShapefile", disabled = FALSE)
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
