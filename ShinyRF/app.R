library(shiny)
library(shinyWidgets) 
library(leaflet)
library(leafem)
library(terra)
library(sf)
library(leaflet.extras)
library(tidymodels)
library(shinyjs)  
library(stars)
library(bslib)
library(colourpicker)

# Increase the maximum file upload size (100 MB in this example)
options(shiny.maxRequestSize = 100*1024^2)


ui <- navbarPage(
    
    title = "My Shiny App",
    id = "nav",
    theme = bs_theme(), 
    useShinyjs(),# Start with an empty theme that we'll customize
    # header = tagList(
    #     # Add a switch input for light/dark mode
    #     tags$style(HTML("
    #         .navbar-nav {
    #             flex-direction: row;
    #         }
    #         .custom-control-label::before {
    #             background-color: #adb5bd;
    #         }
    #         .ms-auto {
    #             margin-left: auto !important;
    #         }
    #         .me-3 {
    #             margin-right: 1rem !important;
    #         }
    #         
    #     "))
    #     # div(
    #     #     class = "ms-auto me-3",
    #     #     switchInput(
    #     #         inputId = "dark_mode",
    #     #         label = "Dark Mode",
    #     #         value = FALSE,
    #     #         onLabel = "On",
    #     #         offLabel = "Off",
    #     #         size = "small"
    #     #     )
    #     # )
    # ),
    tabPanel("Map",
             # Loading animation
             div(id = "loading-animation", div(class = "loader")),
             
             # Main content
             fluidRow(
                 column(
                     width = 3,
                     # Sidebar with three tabs for controls
                     tabsetPanel(
                         tabPanel("Raster",
                                  h4("1. Load Raster Data"),
                                  helpText("Select a raster file to load."),
                                  fileInput("rasterFile", "Choose a Raster File",
                                            accept = c(".tif", ".asc")),
                                  actionButton("loadRaster", "Load Raster", class = "btn-primary"),
                                  
                                  hr(),
                                  
                                  h4("2. Band Selection"),
                                  uiOutput("bandSelectionUI"),
                                  
                                  hr(),
                                  
                                  helpText("Supported formats: .tif, .asc, etc.")
                         ),
                         
                         tabPanel("Shapefile",
                                  h4("1. Create Shapefile"),
                                  textInput("shapeFileName", "Enter a name for the Shapefile"),
                                  textInput("columns", "Enter column names (comma separated)"),
                                  actionButton("createShapefile", "Create Shapefile", class = "btn-success"),
                                  
                                  hr(),
                                  
                                  h4("2. Save Shapefile"),
                                  actionButton("savedirselectshp", "Select a Directory", class = "btn-success"),
                                  verbatimTextOutput("shp_saving_path"),
                                  actionButton("saveShapefile", "Save Shapefile", class = "btn-success", disabled = TRUE)
                         ),
                         
                         tabPanel("Random Forest",
                                  # Sub-tabs for Random Forest workflow
                                  tabsetPanel(
                                      tabPanel("Training",
                                               # First section: Selection and loading of the shapefile
                                               h4("1. Load Training Data"),
                                               helpText("Select shapefile and define class column for training."),
                                               fileInput("trainingShapefile", "Choose Shapefile (select all files)",
                                                         multiple = TRUE,
                                                         accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.cpg', '.xml')),
                                               actionButton("loadTrainingdata", "Load Training Data", class = "btn-primary"),
                                               
                                               hr(),
                                               
                                               # Second section: Displaying and symbology
                                               h4("2. Display and Customize Polygons"),
                                               fluidRow(
                                                   column(
                                                       width = 6,
                                                       uiOutput("showpolyonmapbutton")
                                                   ),
                                                   column(
                                                       width = 6,
                                                       uiOutput("symbology_training")
                                                   )
                                               ),
                                               
                                               hr(),
                                               
                                               # Third section: Model parameters
                                               h4("3. Model Parameters"),
                                               uiOutput("trainingClassColumnUI"),
                                               actionButton("selectsaving", "Select a Directory", class = "btn-secondary"),
                                               verbatimTextOutput("model_saving_path"),
                                               textInput("modelname", "Enter the name of the model"),
                                               uiOutput("RunTraining")
                                               # verbatimTextOutput("trainResults")
                                      ),
                                      tabPanel("Prediction",
                                               # First section: Load trained model
                                               h4("1. Load Trained Model"),
                                               helpText("Select a trained model to predict on raster."),
                                               fileInput("modelFile", "Choose a Trained Model File", accept = c(".rds")),
                                               
                                               hr(),
                                               
                                               # Second section: Prediction settings
                                               h4("2. Prediction Settings"),
                                               actionButton("selectsaving_prediction", "Select a Directory", class = "btn-secondary"),
                                               verbatimTextOutput("prediction_saving_path"),
                                               textInput("prediction_name", "Enter the name of the prediction"),
                                               
                                               hr(),
                                               
                                               # Third section: Run prediction
                                               actionButton("predictRF", "Predict on Raster", class = "btn-primary"),
                                               
                                               hr(),
                                               h4("4. Raster Symbology"),
                                               actionButton("Symbology_Pred", "Symbology", class = "btn-primary", disabled = T)
                                               
                                               
                                      ),
                                      tabPanel("Validation",
                                               # First section: Load validation data
                                               h4("1. Load Validation Data"),
                                               helpText("Select shapefile and define class column for validation."),
                                               fileInput("validationShapefile", "Choose Shapefile (select all files)",
                                                         multiple = TRUE,
                                                         accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.cpg', '.xml')),
                                               
                                               hr(),
                                               
                                               # Second section: Validation parameters
                                               h4("2. Validation Parameters"),
                                               uiOutput("validationClassColumnUI"),
                                               
                                               hr(),
                                               
                                               # Third section: Run validation
                                               actionButton("validateRF", "Validate Model", class = "btn-primary"),
                                               verbatimTextOutput("validationResults")
                                      )
                                  )
                         )
                     )
                 ),
                 column(

                     width = 9,
                     # Leaflet map output
                     leafletOutput("map", height = "85vh")
                 )
             )
    ),
    
    
    # Custom CSS
    tags$head(
        tags$style(HTML("
            /* Loader CSS */
            #loading-animation {
                position: fixed;
                top: 50%;
                left: 50%;
                transform: translate(-50%, -50%);
                z-index: 2000;
                display: none;
            }
            .loader {
                width: 50px;
                height: 50px;
                border: 5px solid #f3f3f3;
                border-top: 5px solid #3498db;
                border-radius: 50%;
                animation: spin 1s linear infinite;
            }
            @keyframes spin {
                to { transform: rotate(360deg); }
            }
            /* Adjustments for the sidebar */
            .tab-content {
                padding-top: 20px;
            }
            /* Adjustments for the map */
            #map {
                border: 1px solid #ccc;
            }
        "))
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
    savingpathpred <- reactiveVal(NULL)
    savingpathshp <- reactiveVal(NULL)
    shapefileData <- reactiveVal(NULL)
    color_pal_polygons <- reactiveVal(NULL)
    isTrainingPlotted <- reactiveVal(NULL)
    pred_raster<- reactiveVal(NULL)
    color_pal_pred <- reactiveVal(NULL)
    
    
    isTrainingPolygonsPlotted <- reactiveVal(NULL)
    TrainingPolygons_colors <- reactiveVal(NULL)
    
    isPredictionPlotted <- reactiveVal(NULL)
    Prediction_colors <- reactiveVal(NULL)
    
    #### MAP OUTPUT ####
    
    # Initialize a blank map that remains visible at all times
    
    # Observe the dark_mode switch and update the theme
    observeEvent(input$dark_mode, {
        session$setCurrentTheme(
            bs_theme(
                bootswatch = if (isTRUE(input$dark_mode)) "darkly" else "minty",
                base_font = font_google("Roboto"),
                heading_font = font_google("Montserrat"),
                bg = if (isTRUE(input$dark_mode)) "#343a40" else "#ffffff",
                fg = if (isTRUE(input$dark_mode)) "#f8f9fa" else "#343a40",
                primary = if (isTRUE(input$dark_mode)) "#0d6efd" else "#0d6efd"
            )
        )
    })
    
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
    
    observeEvent(input$savedirselectshp,{
        path_shp <- rstudioapi::selectDirectory(caption = "where to save the shapefile")
        savingpathshp(path_shp)
        output$shp_saving_path <- renderPrint({path_shp})
    })
    
    # Save the shapefile
    observeEvent(input$saveShapefile, {
        req(drawnPolygons(), shapefileStructure())
        
        
        polygons <- drawnPolygons()
        shapefilePath <- file.path(savingpathshp(), paste0(shapefileStructure()$name, ".shp"))
        a<- polygons %>% as.data.frame()
        for(i in 1:(length(names(a))-1)){
            
            test <- any(is.na(as.numeric(a[,i])))
            
            if (!test) {
                a[,i] <- as.numeric(a[,i])  
                
            }
        }
        polygons <- st_as_sf(a)
        # Save the shapefile
        st_write(polygons, shapefilePath, delete_layer = TRUE)
        showNotification(paste("Shapefile saved at:", shapefilePath), type = "message")
        
        leafletProxy("map") %>%
            clearShapes()
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
                    shp <- st_make_valid(shp) %>% 
                        mutate(AA = c(1:n())) %>% 
                        arrange(AA) %>% 
                        dplyr::select(-AA)
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
        output$symbology_training <- renderUI({
            req(trainingShapefile)
            actionButton("symbologyTraining", "Symbology")
        })
        
        output$RunTraining <- renderUI({
            req(trainingShapefile)
            actionButton("RunTraining", "Run Random Forest Training", choices = names(trainingShapefile))
        })
    })
    
    observeEvent(input$symbologyTraining, {
        req(shp_training())
        showModal(modalDialog(
            title = "Customize Symbology",
            selectInput("symbologyColumn", "Select Column for Coloring", choices = names(shp_training())),
            uiOutput("colorAssignmentUI"),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("applySymbology", "Apply", class = "btn-primary")
            ),
            size = "l"
        ))
    })
    
    # Generate UI for color assignment based on selected column
    output$colorAssignmentUI <- renderUI({
        req(input$symbologyColumn)

        unique_values <- unique(shp_training()[[input$symbologyColumn]] )%>% sort()
        
        # Generate a color input for each unique value
        lapply(seq_along(unique_values), function(i) {
            value <- unique_values[i]
            colourInput(
                inputId = paste0("color_", i),
                label = paste("Color for", value),
                value = "blue",
                allowTransparent = TRUE
            )
        })
    })
    
    observeEvent(input$applySymbology, {
        removeModal()
        req(input$symbologyColumn)

        unique_values <- unique(shp_training()[[input$symbologyColumn]])
        
        # Collect the colors assigned to each value
        colors_assigned <- sapply(seq_along(unique_values), function(i) {
            input[[paste0("color_", i)]]
        })
        names(colors_assigned) <- unique_values
        
        # Create a color mapping function
        colorFunc <- colorFactor(palette = colors_assigned, domain = unique_values)
        
        color_pal_polygons(colorFunc)
        
        TrainingPolygons_colors(colorFunc(shp_training()[[input$symbologyColumn]]))
        
        isplotted_shp <- isTrainingPolygonsPlotted()
        isplotted_pred <- isPredictionPlotted()
        if(!is.null(isplotted_shp) & is.null(isplotted_pred)){
            leafletProxy("map") %>%
                clearShapes() %>%  # Clear any previous shapes
                clearControls() %>% 
                clearGroup("Training_Shapefile") %>%
                clearGroup("Prediction") %>% 
                addPolygons(
                    data = shp_training(),
                    color = ~ TrainingPolygons_colors(),  # Dynamically color polygons
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    group = "Training_Shapefile"
                ) %>%
                addLegend(
                    pal = colorFunc,  # Same color palette
                    values = unique_values,  # Corresponding values
                    title = input$symbologyColumn,  # Legend title as the column name
                    position = "bottomright",  # Position the legend
                    opacity = 1.0,
                    group = "Training_Shapefile"
                ) %>% 
                leaflet::addLayersControl(
                    baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                    overlayGroups = c("Training_Shapefile"),
                    options = layersControlOptions(collapsed = FALSE),
                    position = "topright"
                )
            
        }else if(!is.null(isplotted_shp) & !is.null(isplotted_pred)){
            
            leafletProxy("map") %>%
                clearShapes() %>%  # Clear any previous shapes
                clearControls() %>% 
                clearGroup("Training_Shapefile") %>%
                clearGroup("Prediction") %>% 
                addRasterImage(pred_raster(), group = "Prediction",method = "ngb", colors = function(values){
                    Prediction_colors(values) 
                }) %>% 
                addPolygons(
                    data = shp_training(),
                    color = ~ TrainingPolygons_colors(),  # Dynamically color polygons
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    group = "Training_Shapefile"
                ) %>%
                # removeLayersControl() %>% 
                leaflet::addLayersControl(
                    baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                    overlayGroups = c("Prediction", "Training_Shapefile"),
                    options = layersControlOptions(collapsed = FALSE),
                    position = "topright"
                ) %>%
                addLegend(
                    pal = colorFunc,  # Same color palette
                    values = unique_values,  # Corresponding values
                    title = input$symbologyColumn,  # Legend title as the column name
                    position = "bottomright",  # Position the legend
                    opacity = 1.0,
                    group = "Training_Shapefile"
                ) %>% 
                addLegend(
                    pal = colorFunc_pred,  # Same color palette
                    values = unique(values(raster_pred)),  # Corresponding values
                    title = "Prediction",  # Legend title as the column name
                    position = "bottomright",  # Position the legend
                    opacity = 1.0,
                    group = "Prediction"
                )
        }
        # Update the map with the new symbology
        # leafletProxy("map") %>%
        #     clearShapes() %>%
        #     addPolygons(
        #         data = shp_training(),
        #         fillColor = ~colorFunc(unique_values),
        #         fillOpacity = 0.7,
        #         color = "black",
        #         weight = 1
        #         # popup = ~paste(input$symbologyColumn, ":", .data[[input$symbologyColumn]])
        #     )
    })
    
    
    observeEvent(input$showpolyonmap, {
        req(shp_training())
        
        pal <- TrainingPolygons_colors()
        isplotted_pred <- isPredictionPlotted()
        
        if(is.null(pal)){
            leafletProxy("map") %>%
                clearShapes() %>%  # Clear any previous shapes
                clearControls() %>% # Clear any previous legends
                addPolygons(
                    data = shp_training(),
                    color = "blue",  # Dynamically color polygons
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    group = "Training_Shapefile"
                )%>% 
                leaflet::addLayersControl(
                    baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                    overlayGroups = c("Training_Shapefile"),
                    options = layersControlOptions(collapsed = FALSE),
                    position = "topright"
                )
        }else{
            if(is.null(isplotted_pred)){
                
                leafletProxy("map") %>%
                    clearShapes() %>%  # Clear any previous shapes
                    clearControls() %>%
                    clearGroup("Training_Shapefile") %>% 
                    addPolygons(
                        data = shp_training(),
                        color = ~TrainingPolygons_colors(),  # Dynamically color polygons
                        weight = 2,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Training_Shapefile"
                    ) %>%
                    addLegend(
                        pal = pal,  # Same color palette
                        values = unique(shp_training()[[input$symbologyColumn]]),  # Corresponding values
                        title = input$symbologyColumn,  # Legend title as the column name
                        position = "bottomright",  # Position the legend
                        opacity = 1.0,
                        group = "Training_Shapefile"
                    )%>% 
                    leaflet::addLayersControl(
                        baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                        overlayGroups = c("Training_Shapefile"),
                        options = layersControlOptions(collapsed = FALSE),
                        position = "topright"
                    ) 
            }else{
                leafletProxy("map") %>%
                    clearShapes() %>%  # Clear any previous shapes
                    clearControls() %>% 
                    clearGroup("Training_Shapefile") %>%
                    clearGroup("Prediction") %>% 
                    addRasterImage(pred_raster(), group = "Prediction",method = "ngb", colors = function(values){
                        Prediction_colors(values) 
                    }) %>% 
                    addPolygons(
                        data = shp_training(),
                        color = ~ TrainingPolygons_colors(),  # Dynamically color polygons
                        weight = 2,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Training_Shapefile"
                    ) %>%
                    # removeLayersControl() %>% 
                    leaflet::addLayersControl(
                        baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                        overlayGroups = c("Prediction", "Training_Shapefile"),
                        options = layersControlOptions(collapsed = FALSE),
                        position = "topright"
                    ) %>%
                    addLegend(
                        pal = colorFunc,  # Same color palette
                        values = unique_values,  # Corresponding values
                        title = input$symbologyColumn,  # Legend title as the column name
                        position = "bottomright",  # Position the legend
                        opacity = 1.0,
                        group = "Training_Shapefile"
                    ) %>% 
                    addLegend(
                        pal = colorFunc_pred,  # Same color palette
                        values = unique(values(raster_pred)),  # Corresponding values
                        title = "Prediction",  # Legend title as the column name
                        position = "bottomright",  # Position the legend
                        opacity = 1.0,
                        group = "Prediction"
                    )
            } 
            
        }
        
        isTrainingPolygonsPlotted(TRUE)
        

        
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
                       showNotification(print(paste0("Out-of-the-bag accuracy is ", round(metrics$.estimate[1],2))), type = "message")
                       
                       
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
                writeRaster(prediction_raster, paste0(savingpathpred(),"/",input$prediction_name,".tif"), overwrite = TRUE)
                
                rm(prediction_raster)
                pred_rast <- rast(paste0(savingpathpred(),"/",input$prediction_name,".tif"))
                
                pred_raster(pred_rast)
                
                extent_raster <- terra::project(ext(pred_rast), from = terra::crs(rasterData()), to = "+proj=longlat +datum=WGS84 +no_defs")
                
                # Display results on the map
                leafletProxy("map") %>%
                    clearGroup("Prediction") %>%
                    addRasterImage(pred_rast, group = "Prediction",method = "ngb", layerId = "Pred_Ras") %>% 
                    flyToBounds(
                        lng1 = as.numeric(ext(extent_raster)[1]),
                        lat1 = as.numeric(ext(extent_raster)[3]),
                        lng2 = as.numeric(ext(extent_raster)[2]),
                        lat2 = as.numeric(ext(extent_raster)[4])) %>% 
                    # removeLayersControl() %>% 
                    leaflet::addLayersControl(
                        baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                        overlayGroups = c("Prediction"),
                        options = layersControlOptions(collapsed = FALSE),
                        position = "topright"
                    )
                
                shinyjs::hide("loading-animation")
                enable("Symbology_Pred")
                
                
                showNotification("Prediction completed successfully!", type = "message")
            }, error = function(e) {
                showNotification(paste("Error in prediction:", e$message), type = "error")
            })   
        }
        
        
    })
    
    observeEvent(input$Symbology_Pred, {
        req(pred_raster())
        showModal(modalDialog(
            title = "Customize Symbology",
            helpText("Select color for each pixel value of the prediction :"),
            uiOutput("colorAssignmentUI_Rast"),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("applySymbology_pred", "Apply", class = "btn-primary")
            ),
            size = "l"
        ))
    })
    
    # Generate UI for color assignment based on selected column
    output$colorAssignmentUI_Rast <- renderUI({
        req(pred_raster())
        
        unique_values_pred <- c(unique(values(pred_raster())) %>% as.numeric() %>% sort())
        
        # Generate a color input for each unique value
        lapply(seq_along(unique_values_pred), function(i) {
            value <- unique_values_pred[i]
            colourInput(
                inputId = paste0("color_", i),
                label = paste("Color for", value),
                value = "blue",
                allowTransparent = TRUE
            )
        })
    })
    
    observeEvent(input$applySymbology_pred, {
        removeModal()
        req(pred_raster())
        
        raster_pred <- pred_raster()

        unique_values_pred <-  unique(values(raster_pred)) %>% as.numeric()  %>% sort()
        
        # Collect the colors assigned to each value
        colors_assigned <- sapply(seq_along(unique_values_pred), function(i) {
            input[[paste0("color_", i)]]
        })
        names(colors_assigned) <- unique_values_pred
        
        # Create a color mapping function
        colorFunc_pred <- colorFactor(palette = colors_assigned, domain = unique_values_pred, na.color = "transparent")
        
        Prediction_colors(colorFunc_pred)
        
       leafletProxy("map") %>%
            clearGroup("Prediction") %>%
            addRasterImage(raster_pred, group = "Prediction",method = "ngb", colors = function(values){
                colorFunc_pred(values) 
            }) %>% 
            # removeLayersControl() %>% 
            leaflet::addLayersControl(
                baseGroups = c("OpenStreetMap", "ESRI Satellite"),
                overlayGroups = c("Prediction"),
                options = layersControlOptions(collapsed = FALSE),
                position = "topright"
            ) %>%
           addLegend(
               pal = colorFunc_pred,  # Same color palette
               values = unique(values(raster_pred)),  # Corresponding values
               title = "Prediction",  # Legend title as the column name
               position = "bottomright",  # Position the legend
               opacity = 1.0,
               group = "Prediction"
           )
            
        isPredictionPlotted(TRUE)

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