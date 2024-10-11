library(shiny)
library(leaflet)
library(leafem)
library(terra)
library(sf)
library(leaflet.extras)
library(tidymodels)
library(shinyjs)  
library(stars)# Add shinyjs library

# Increase the maximum file upload size (100 MB in this example)
options(shiny.maxRequestSize = 100*1024^2)

# Define UI
ui <- fluidPage(
    useShinyjs(),  # Include shinyjs
    
    # Add custom CSS to make the map take up the entire screen and include the loader CSS
    tags$head(
        tags$style(HTML("
            #map {
                height: 90vh !important;
                width: 70vw !important;
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
                                      actionButton("loadTrainingdata", "Load Training data"),
                                      uiOutput("showpolyonmapbutton"),
                                      uiOutput("trainingClassColumnUI"),
                                      actionButton("selectsaving", "Select a directory"),
                                      verbatimTextOutput("model_saving_path"),
                                      textInput("modelname", "Enter the name of the model"),
                                      uiOutput("RunTraining"),
                                      verbatimTextOutput("trainResults")
                             ),
                             tabPanel("Prediction",
                                      helpText("Select trained model to predict on raster."),
                                      fileInput("modelFile", "Choose a Trained Model File", accept = c(".rds")),
                                      actionButton("selectsaving_prediction", "Select a directory"),
                                      verbatimTextOutput("prediction_saving_path"),
                                      textInput("prediction_name", "Enter the name of the prediction"),
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
            leafletOutput("map")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values to store raster, polygons, and attributes
    rasterData <- reactiveVal(NULL)
    shp_training <- reactiveVal(NULL)
    drawnPolygons <- reactiveVal(NULL)
    shapefileStructure <- reactiveVal(NULL)
    currentPolygon <- reactiveVal(NULL)
    savingpathmodel <- reactiveVal(NULL)
    savingpathpred <- reactiveVal(NULL)# To store the current polygon being drawn
    
    #### MAP OUTPUT ####
    
    # Initialize a blank map that remains visible at all times
    
    output$map <- 
        renderLeaflet({
            leaflet(options = leafletOptions(maxZoom = 28, zoomAnimation = TRUE, fadeAnimation = TRUE, preferCanvas = TRUE)) %>%
                # Create separate panes for base layers and the raster overlay
                addMapPane("rasterPane", zIndex = 1000) %>%
                addMapPane("basePane", zIndex = 0) %>%
                
                
                # Add base map tiles to the basePane
                addTiles(options = tileOptions(pane = "basePane", noWrap = TRUE), group = "OpenStreetMap") %>%
                addProviderTiles("Esri.WorldImagery", options = tileOptions(pane = "basePane"), group = "ESRI Satellite") %>% 
                leaflet::addLayersControl(
                    baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                    options = layersControlOptions(collapsed = FALSE),
                    position = "topright"
                )
                
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
                    selectInput("blueBand", "Select Blue Band", choices = 1:numBands),
                    actionButton("createRGB", "Create RGB Composition")
                )
            })
        }
    })
    
    # Create RGB composition and display it on the map
    observeEvent(input$createRGB, {
        req(rasterData(), input$redBand, input$greenBand, input$blueBand)
        input$map_groups
        # Start loading animation
        shinyjs::show("loading-animation")
        
        # Try creating an RGB composite, catch errors
        tryCatch({
            # Stack the selected bands to create an RGB composite
            rgbRaster <- stars::read_stars(input$rasterFile$datapath)
            rgbRaster[rgbRaster == 0] <- NA

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

            
            # Convert stars object to terra for Leaflet compatibility
            # terra_img <- terra::rast(input$rasterFile$datapath)
            # terra_img[terra_img == 0] <- NA
            # 
            # terra_img<- terra_img %>% 
            #     terra::stretch(minv=0, maxv=1)
            # 
            # extent_raster <- terra::project(ext(rasterData()), from = terra::crs(rasterData()), to = "+proj=longlat +datum=WGS84 +no_defs")
            # 
            # 
            # # Create an RGB composition from the three bands
            # RGB(terra_img,type="rgb") <- c(as.numeric(input$redBand),as.numeric(input$greenBand),as.numeric(input$blueBand))
            # 
            # img_rgb <- colorize(terra_img , "col", stretch = "hist")
            
            
            leafletProxy("map") %>%
                clearImages() %>%
                 # Add the RGB raster image in the custom pane with the higher zIndex

                flyToBounds(
                    lng1 = as.numeric(extent_raster[1]),
                    lat1 = as.numeric(extent_raster[3]),
                    lng2 = as.numeric(extent_raster[2]),
                    lat2 = as.numeric(extent_raster[4])
                )%>%
                addStarsRGB(rgbRaster, as.numeric(input$redBand),
                            as.numeric(input$greenBand),
                            as.numeric(input$blueBand),
                            group = "test",
                            quantiles = c(0.05, 0.95),
                            maxBytes = 6 * 1024 * 1024,
                            options = tileOptions(pane = "rasterPane"))
                #            rgb = T,
                #            group = "test"
                #            )
                # addRasterImage(img_rgb, 
                #                method = "ngb",
                #             group = "test",
                #             maxBytes = 6 * 1024 * 1024,
                #             options = tileOptions(pane = "rasterPane"))
            
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
    # observe({
    #     leafletProxy("map") %>%
    #         removeDrawToolbar()
    # })
    
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
    observeEvent(input$loadTrainingdata, {
        req(input$trainingShapefile)
        
        # Load the shapefile for training
        trainingShapefile <- tryCatch({
            tempDir <- tempdir()
            shapefilePaths <- input$trainingShapefile$datapath
            shapefileNames <- input$trainingShapefile$name
            
            files_to_remove <- list.files(tempDir,full.names = T)
            for(i in 1:length(files_to_remove)){
                file.remove(files_to_remove[i])
            }
            
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
                    shp_training(shp)
                    # Check and transform CRS if necessary
                    if (is.na(st_crs(shp))) {
                        showNotification("Shapefile CRS is missing. Have you provided the .prj ?", type = "error")
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
            selectInput("trainingClassColumn", "Select Class Column", choices = names(trainingShapefile)[-length(names(trainingShapefile))])
        })
        
        output$showpolyonmapbutton <- renderUI({
            req(trainingShapefile)
            actionButton("showpolyonmap", "Show polygons on the map")
            })
        
        output$RunTraining <- renderUI({
            req(trainingShapefile)
            actionButton("RunTraining", "Run Random Forest Training", choices = names(trainingShapefile))
        })
    })
    
    
    observeEvent(input$showpolyonmap, {
        req(input$trainingClassColumn, shp_training())
        
        # Dynamically access the column chosen by the user
        selected_column <- shp_training()[[input$trainingClassColumn]]
        
        # Create a color palette based on the selected column
        pal <- colorFactor(palette = "Set1", domain = selected_column)
        
        # Use leafletProxy to add polygons with colors based on the selected column
        leafletProxy("map") %>%
            clearShapes() %>%  # Clear any previous shapes
            clearControls() %>%  # Clear any previous legends
            addPolygons(
                data = shp_training(),
                color = ~pal(selected_column),  # Dynamically color polygons
                weight = 2,
                opacity = 1.0,
                fillOpacity = 0.5,
                group = "Shapefile"
            ) %>%
            addLegend(
                pal = pal,  # Same color palette
                values = selected_column,  # Corresponding values
                title = input$trainingClassColumn,  # Legend title as the column name
                position = "bottomright",  # Position the legend
                opacity = 1.0
            )
        
    })
    # Train the model after selecting the class column
    
    observeEvent(input$selectsaving,{
        path_model <- rstudioapi::selectDirectory(caption = "where to save the model")
        savingpathmodel(path_model)
        output$model_saving_path <- renderPrint({ path_model})
    })
    
    observeEvent(input$RunTraining, {
        req(rasterData(), input$trainingClassColumn,shp_training())
        
     tryCatch({
           if(nchar(input$modelname) == 0 ){
              break 
           }else if(nchar(savingpathmodel()) == 0){
               break
            }else{
                   tryCatch({
                   # Extract raster values for each polygon
                      shinyjs::show("loading-animation")
                       
                       training_img <- rasterData()
                       training_shp <- terra::vect(shp_training()) 
                       
                   if(!is.numeric(training_shp[input$trainingClassColumn] [[1]][[1]])){
                       showNotification(paste("The column of the shapefile used to train the model must contain numeric data"), type = "warning")
                       shinyjs::hide("loading-animation")
                   }else{
                       
                       names(training_img)<-paste0("B",c(1:nlyr(training_img)))
                       
                       if(crs(training_img) != crs(training_shp)){
                           training_shp <- training_shp %>% 
                               terra::project(crs(training_img))
                       }
                       
                       x <- terra::rasterize(training_shp, training_img, field = input$trainingClassColumn)
                       
                       df <- c(training_img,x) %>% 
                           as.data.frame() %>%
                           dplyr::rename(class = colnames(.)[ncol(.)]) %>% 
                           dplyr::filter(!is.na(class)) %>% 
                           mutate(class =as.factor(class))
                       
                       
                       # Example dataframe: assuming df is already available in your environment
                       # target_column_name is the name of the column you're trying to predict
                       
                       # Set a seed for reproducibility
                       set.seed(123)
                       
                       # Split the data into training and testing sets (80% training, 20% testing)
                       data_split <- initial_split(df, prop = 0.8)
                       train_data <- training(data_split)
                       test_data <- testing(data_split)
                       
                       # Create a recipe: specify the target and predictors
                       # Assumes target_column_name is the column to predict, with all other columns as predictors
                       rf_recipe <- recipe(class ~ ., data = train_data)
                       
                       # Define the random forest model specification
                       rf_spec <- rand_forest(
                           trees = 500,       # Number of trees
                           min_n = 3          # Minimum number of samples in a node
                       ) %>%
                           set_engine("ranger") %>%
                           set_mode("classification")  # Change to "regression" if predicting a continuous variable
                       
                       # Create the workflow
                       rf_workflow <- workflow() %>%
                           add_recipe(rf_recipe) %>%
                           add_model(rf_spec)
                       
                       # Train the model using 5-fold cross-validation
                       set.seed(123)
                       rf_resamples <- vfold_cv(train_data, v = 5)
                       
                       rf_fit <- rf_workflow %>%
                           fit_resamples(
                               rf_resamples,
                               metrics = metric_set(accuracy, roc_auc),  # Choose metrics for classification
                               control = control_resamples(save_pred = TRUE)
                           )
                       
                       # Finalize the model by fitting on the full training data
                       rf_final_fit <- rf_workflow %>%
                           last_fit(data_split)
                       
                       # Print results from the final model
                       metrics <- rf_final_fit %>% collect_metrics()
                       
                       # Get the fitted model
                       final_model <- rf_final_fit %>% extract_workflow()
                       
                       # Make predictions on the test set
                       # test_predictions <- predict(final_model, test_data)
                       
                       # Save model to disk
                       saveRDS(final_model, file = paste0(savingpathmodel(),"/",input$modelname,".rds"))
                       shinyjs::hide("loading-animation")
                       
                       
                       # Display results
                       output$trainResults <- renderPrint({
                           print(paste0("Out-of-the-bag accuracy is ", round(metrics$.estimate[1],2)))
                       })
                       
                       showNotification("Random Forest training completed successfully!", type = "message")
                   }
                   
                   
               }, error = function(e) {
                   showNotification(paste("Error in Random Forest training:", e$message), type = "error")
               })
               
           }
        }, error = function(e) {
            showNotification("Give a directory and a name to the model !", type = "error")
            return(NULL)
        })
        

    })
    
    observeEvent(input$selectsaving_prediction,{
        path_pred <- rstudioapi::selectDirectory(caption = "where the model is saved")
        savingpathpred(path_pred)
        output$prediction_saving_path <- renderPrint({path_pred})
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
        
        if(is.null(savingpathpred()) | nchar(input$prediction_name) == 0){
            showNotification(paste("Give a path and a name to the prediction"), type = "warning")
        }else{
            tryCatch({
                shinyjs::show("loading-animation")
                
                img<-rasterData()
                names(img) <- paste0("B",c(1:nlyr(img)))
                
                raster_df <- as.data.frame(img, xy = TRUE, na.rm = TRUE)
                
                # Make sure the column names of the raster_df match the variable names the model expects
                # For example, if your model expects variables named "var1", "var2", etc.
                # you can rename the raster_df columns if needed:
                # names(raster_df)[3:5] <- c("var1", "var2", "var3")  # Adjust based on your variables
                
                # Make predictions using the model
                predictions <- predict(rfModel, new_data = raster_df)
                
                # Add the predictions to the raster_df
                raster_df$predictions <- predictions$.pred_class  # If classification model
                # For regression, use `predictions$.pred`
                
                # Convert the predictions back to a raster format
                prediction_raster <- rast(raster_df[, c("x", "y", "predictions")], type = "xyz")
                
                crs(prediction_raster) <- crs(img)
                
                # Save the predicted raster
                writeRaster(prediction_raster, "predicted_raster.tif", overwrite = TRUE)
                
                extent_raster <- terra::project(ext(prediction_raster), from = terra::crs(rasterData()), to = "+proj=longlat +datum=WGS84 +no_defs")
                
                # Display results on the map
                leafletProxy("map") %>%
                    clearGroup("Prediction") %>%
                    addRasterImage(prediction_raster, group = "Prediction",method = "ngb") %>% 
                    
                    flyToBounds(
                        lng1 = as.numeric(ext(extent_raster)[1]),
                        lat1 = as.numeric(ext(extent_raster)[3]),
                        lng2 = as.numeric(ext(extent_raster)[2]),
                        lat2 = as.numeric(ext(extent_raster)[4])) %>% 
                    removeLayersControl() %>% 
                    leaflet::addLayersControl(
                        baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                        overlayGroups = c("Prediction"),
                        options = layersControlOptions(collapsed = FALSE),
                        position = "topright"
                    )
                
                shinyjs::hide("loading-animation")    
                
                
                showNotification("Prediction completed successfully!", type = "message")
            }, error = function(e) {
                showNotification(paste("Error in prediction:", e$message), type = "error")
            })   
        }
        
        
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
shinyApp(ui = ui, server = server
         # options = list(launch.browser = TRUE)
)