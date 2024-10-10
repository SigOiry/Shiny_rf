Define server logic
server <- function(input, output, session) {
    
    # Reactive values to store raster, polygons, and attributes
    rasterData <- reactiveVal(NULL)
    shp_training <- reactiveVal(NULL)
    
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
        
        # Try creating an RGB composite, catch errors
        tryCatch({
            # Stack the selected bands to create an RGB composite
            rgbRaster <- stars::read_stars(input$rasterFile$datapath)
            
            # terra::RGB(rgbRaster)<-c(as.numeric(input$redBand),as.numeric(input$greenBand),as.numeric(input$blueBand))
            
            # rgbRaster_col <- terra::colorize(rgbRaster, "rgb",stretch = "hist")
            # rgbRaster_col <- terra::colorize(rgbRaster_col, "rgb",stretch = "hist")
            
            bbox <- st_bbox(rgbRaster)
            
            # Step 2: Convert the bounding box to an sf object
            bbox_sf <- st_as_sfc(bbox)
            
            # Step 3: Reproject the bounding box to WGS84 (EPSG:4326)
            bbox_wgs84 <- st_transform(bbox_sf, crs = 4326)
            
            # Step 4: Get the new reprojected bounding box (extent)
            reprojected_bbox <- st_bbox(bbox_wgs84)
            
            
            
            # Add to leaflet map
            extent_raster <- terra::project(ext(rasterData()), from = terra::crs(rasterData()), to = "+proj=longlat +datum=WGS84 +no_defs")
            
            leafletProxy("map") %>%
                clearImages() %>%
                addStarsRGB(rgbRaster,as.numeric(input$redBand),
                            as.numeric(input$greenBand),
                            as.numeric(input$blueBand),
                            quantiles = c(0.05,0.95),
                            maxBytes = 6 * 1024 * 1024) %>% 
                # addRasterImage(rgbRaster_col) %>%
                flyToBounds(
                    lng1 = as.numeric(reprojected_bbox[1]),
                    lat1 = as.numeric(reprojected_bbox[2]),
                    lng2 = as.numeric(reprojected_bbox[3]),
                    lat2 = as.numeric(reprojected_bbox[4])
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
                        shp <- st_transform(shp, terra::crs(rasterData()))
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
        req(rasterData(), input$trainingClassColumn,trainingShapefile)
        
        tryCatch({
            # Extract raster values for each polygon
            trainingData <- terra::extract(rasterData(), st_geometry(trainingShapefile))
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
# shinyApp(ui = ui, server = server
#          # options = list(launch.browser = TRUE)
# )