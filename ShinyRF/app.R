library(shiny)
library(leaflet)
library(terra)
library(sf)
library(leaflet.extras)
library(randomForest)
library(shinyjs)  # Add shinyjs library

# Increase the maximum file upload size (100 MB in this example)
options(shiny.maxRequestSize = 100*1024^2)

# Define UI
ui <- fluidPage(
    useShinyjs(),  # Include shinyjs
    
    # Add custom CSS to make the map take up the entire screen and include the loader CSS
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
            #loading-animation {
                position: fixed;
                top: 50%;
                left: 50%;
                transform: translate(-50%, -50%);
                z-index: 2000;
                display: none;
            }

            /* Loader CSS */
            .loader {
                width: 50px;
                padding: 8px;
                aspect-ratio: 1;
                border-radius: 50%;
                background: #25b09b;
                --_m: 
                    conic-gradient(#0000 10%, #000),
                    linear-gradient(#000 0 0) content-box;
                -webkit-mask: var(--_m);
                mask: var(--_m);
                -webkit-mask-composite: source-out;
                mask-composite: subtract;
                animation: l3 1s infinite linear;
            }
            @keyframes l3 { to { transform: rotate(1turn); } }
        "))
    ),
    
    # Replace loading-animation div
    div(id = "loading-animation", div(class = "loader")),
    
    # Layout with sidebar for controls and main panel for map
    sidebarLayout(
        # Sidebar with three tabs for controls
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
                ),
                
                tabPanel("Random Forest",
                         # Sub-tabs for Random Forest workflow
                         tabsetPanel(
                             tabPanel("Training",
                                      helpText("Select shapefile and define class column for training."),
                                      fileInput("trainingShapefile", "Choose Shapefile (select all files)",
                                                multiple = TRUE,
                                                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg','.xml')),
                                      uiOutput("trainingClassColumnUI"),
                                      actionButton("trainRF", "Train Random Forest"),
                                      verbatimTextOutput("trainResults")
                             ),
                             tabPanel("Prediction",
                                      helpText("Select trained model to predict on raster."),
                                      fileInput("modelFile", "Choose a Trained Model File", accept = c(".rds")),
                                      actionButton("predictRF", "Predict on Raster"),
                                      verbatimTextOutput("predictResults")
                             ),
                             tabPanel("Validation",
                                      helpText("Select shapefile and define class column for validation."),
                                      fileInput("validationShapefile", "Choose Shapefile (select all files)",
                                                multiple = TRUE,
                                                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg','.xml')),
                                      uiOutput("validationClassColumnUI"),
                                      actionButton("validateRF", "Validate Model"),
                                      verbatimTextOutput("validationResults")
                             )
                         )
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
    
    # Reactive values to store raster, polygons, and attributes
    rasterData <- reactiveVal(NULL)
    drawnPolygons <- reactiveVal(NULL)
    shapefileStructure <- reactiveVal(NULL)
    currentPolygon <- reactiveVal(NULL)  # To store the current polygon being drawn
    
    #### MAP OUTPUT ####
    
    # Initialize a blank map that remains visible at all times
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(maxZoom = 28, zoomAnimation = TRUE, fadeAnimation = TRUE, preferCanvas = TRUE)) %>%
            addProviderTiles(providers$OpenStreetMap)
    })
    
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
        
        # Start loading animation
        shinyjs::show("loading-animation")
        
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
            extent_raster <- project(ext(rasterData()), from = crs(rasterData()), to = "+proj=longlat +datum=WGS84 +no_defs")
            
            leafletProxy("map") %>%
                clearImages() %>%
                addRasterImage(rgbRaster_col, group = "RGB Raster") %>%
                flyToBounds(
                    lng1 = as.numeric(extent_raster[1]),
                    lat1 = as.numeric(extent_raster[3]),
                    lng2 = as.numeric(extent_raster[2]),
                    lat2 = as.numeric(extent_raster[4])
                )
            
            # Stop loading animation
            shinyjs::hide("loading-animation")
            
            showNotification("RGB Image created successfully!", type = "message")
        }, error = function(e) {
            # Stop loading animation in case of error
            shinyjs::hide("loading-animation")
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
        columns <- strsplit(input$columns, ",[ ]*")[[1]]
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
    
    #### RANDOM FOREST TAB ####
    
    # Train Random Forest model
    observeEvent(input$trainRF, {
        req(input$trainingShapefile)
        
        # Load the shapefile for training
        trainingShapefile <- tryCatch({
            tempDir <- tempdir()
            shapefilePaths <- input$trainingShapefile$datapath
            shapefileNames <- input$trainingShapefile$name
            
            # Copy uploaded files to the temporary directory
            for (i in 1:length(shapefilePaths)) {
                file.copy(shapefilePaths[i], file.path(tempDir, shapefileNames[i]), overwrite = TRUE)
            }
            
            # Identify the .shp file
            shpFiles <- list.files(tempDir, pattern = "\\.shp$", full.names = TRUE)
            if (length(shpFiles) == 0) {
                showNotification("No .shp file found in uploaded files.", type = "error")
                return(NULL)
            } else {
                shpPath <- shpFiles[1]
                
                # Check for presence of .shx and .dbf files
                shpBaseName <- tools::file_path_sans_ext(basename(shpPath))
                requiredExtensions <- c(".shp", ".shx", ".dbf")
                missingFiles <- c()
                for (ext in requiredExtensions) {
                    filePath <- file.path(tempDir, paste0(shpBaseName, ext))
                    if (!file.exists(filePath)) {
                        missingFiles <- c(missingFiles, paste0(shpBaseName, ext))
                    }
                }
                
                if (length(missingFiles) > 0) {
                    showNotification(paste("Missing required shapefile components:", paste(missingFiles, collapse = ", ")), type = "error")
                    return(NULL)
                } else {
                    # Read shapefile using sf package and handle errors
                    shp <- st_read(shpPath)
                    
                    # Ensure geometries are valid
                    shp <- st_make_valid(shp)
                    
                    # Check and transform CRS if necessary
                    if (is.na(st_crs(shp))) {
                        showNotification("Shapefile CRS is missing. Cannot proceed.", type = "error")
                        return(NULL)
                    } else if (st_crs(shp) != st_crs(rasterData())) {
                        shp <- st_transform(shp, crs(rasterData()))
                    }
                    shp
                }
            }
        })
        
        # Extract the class column
        output$trainingClassColumnUI <- renderUI({
            req(trainingShapefile)
            selectInput("trainingClassColumn", "Select Class Column", choices = names(trainingShapefile))
        })
    })
    
    # Train the model after selecting the class column
    observeEvent(input$trainRF, {
        req(rasterData(), input$trainingClassColumn)
        
        tryCatch({
            # Extract raster values for each polygon
            trainingData <- extract(rasterData(), st_geometry(trainingShapefile))
            response <- as.factor(trainingShapefile[[input$trainingClassColumn]])
            
            # Create a dataframe for training
            trainDF <- data.frame(response, trainingData)
            
            # Train Random Forest model
            rfModel <- randomForest(response ~ ., data = trainDF, ntree = 100)
            
            # Save model to disk
            saveRDS(rfModel, file = "rf_model.rds")
            
            # Display results
            output$trainResults <- renderPrint({
                print(rfModel)
            })
            
            showNotification("Random Forest training completed successfully!", type = "message")
        }, error = function(e) {
            showNotification(paste("Error in Random Forest training:", e$message), type = "error")
        })
    })
    
    # Predict using trained Random Forest model
    observeEvent(input$predictRF, {
        req(input$modelFile, rasterData())
        
        # Load the trained model
        rfModel <- tryCatch({
            readRDS(input$modelFile$datapath)
        }, error = function(e) {
            showNotification("Error loading model. Check the file format.", type = "error")
            return(NULL)
        })
        
        tryCatch({
            # Predict on raster
            prediction <- predict(rasterData(), rfModel, type = "response")
            
            # Display results on the map
            leafletProxy("map") %>%
                clearImages() %>%
                addRasterImage(prediction, group = "Prediction")
            
            showNotification("Prediction completed successfully!", type = "message")
        }, error = function(e) {
            showNotification(paste("Error in prediction:", e$message), type = "error")
        })
    })
    
    # Validate the model
    observeEvent(input$validateRF, {
        req(input$validationShapefile)
        
        # Load the shapefile for validation
        validationShapefile <- tryCatch({
            tempDir <- tempdir()
            shapefilePaths <- input$validationShapefile$datapath
            shapefileNames <- input$validationShapefile$name
            
            # Copy uploaded files to the temporary directory
            for (i in 1:length(shapefilePaths)) {
                file.copy(shapefilePaths[i], file.path(tempDir, shapefileNames[i]), overwrite = TRUE)
            }
            
            # Identify the .shp file
            shpFiles <- list.files(tempDir, pattern = "\\.shp$", full.names = TRUE)
            if (length(shpFiles) == 0) {
                showNotification("No .shp file found in uploaded files.", type = "error")
                return(NULL)
            } else {
                shpPath <- shpFiles[1]
                
                # Check for presence of .shx and .dbf files
                shpBaseName <- tools::file_path_sans_ext(basename(shpPath))
                requiredExtensions <- c(".shp", ".shx", ".dbf")
                missingFiles <- c()
                for (ext in requiredExtensions) {
                    filePath <- file.path(tempDir, paste0(shpBaseName, ext))
                    if (!file.exists(filePath)) {
                        missingFiles <- c(missingFiles, paste0(shpBaseName, ext))
                    }
                }
                
                if (length(missingFiles) > 0) {
                    showNotification(paste("Missing required shapefile components:", paste(missingFiles, collapse = ", ")), type = "error")
                    return(NULL)
                } else {
                    # Read shapefile using sf package and handle errors
                    shp <- st_read(shpPath)
                    
                    # Ensure geometries are valid
                    shp <- st_make_valid(shp)
                    
                    # Check and transform CRS if necessary
                    if (is.na(st_crs(shp))) {
                        showNotification("Shapefile CRS is missing. Cannot proceed.", type = "error")
                        return(NULL)
                    } else if (st_crs(shp) != st_crs(4326)) {
                        shp <- st_transform(shp, 4326)
                    }
                    shp
                }
            }
        })
        
        # Extract the class column
        output$validationClassColumnUI <- renderUI({
            req(validationShapefile)
            selectInput("validationClassColumn", "Select Class Column", choices = names(validationShapefile))
        })
    })
    
    # Validate after selecting the class column
    observeEvent(input$validateRF, {
        req(input$modelFile, validationShapefile, input$validationClassColumn)
        
        # Load the trained model
        rfModel <- tryCatch({
            readRDS(input$modelFile$datapath)
        }, error = function(e) {
            showNotification("Error loading model. Check the file format.", type = "error")
            return(NULL)
        })
        
        tryCatch({
            # Extract raster values for each validation polygon
            validationData <- extract(rasterData(), st_geometry(validationShapefile))
            actual <- as.factor(validationShapefile[[input$validationClassColumn]])
            predicted <- predict(rfModel, validationData)
            
            # Create confusion matrix
            confusionMatrix <- table(Predicted = predicted, Actual = actual)
            
            # Display confusion matrix
            output$validationResults <- renderPrint({
                print(confusionMatrix)
            })
            
            showNotification("Validation completed successfully!", type = "message")
        }, error = function(e) {
            showNotification(paste("Error in validation:", e$message), type = "error")
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))