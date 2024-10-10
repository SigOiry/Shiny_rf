library(shiny)
library(leaflet)
library(leafem)
library(terra)
library(sf)
library(leaflet.extras)
library(randomForest)
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
                                      actionButton("loadTrainingdata", "Load Training data"),
                                      uiOutput("RunTraining"),
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