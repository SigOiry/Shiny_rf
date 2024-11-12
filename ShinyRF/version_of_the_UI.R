############ UI 1 #################

ui <- navbarPage(
  
  title = "My Shiny App",
  id = "nav",
  theme = bs_theme(), 
  useShinyjs(),
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

############ UI 2 #################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "My Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster", tabName = "raster", icon = icon("image")),
      menuItem("Shapefile", tabName = "shapefile", icon = icon("draw-polygon")),
      menuItem("Random Forest", tabName = "random_forest", icon = icon("tree")),
      menuItem("Map", tabName = "map", icon = icon("globe"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # Include animate.css for animations
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$style(HTML("
        /* Custom CSS for loader */
        #loading-animation {
          position: fixed;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          z-index: 2000;
        }
        .loader {
          border: 12px solid #f3f3f3;
          border-top: 12px solid #3498db;
          border-radius: 50%;
          width: 70px;
          height: 70px;
          animation: spin 2s linear infinite;
        }
        @keyframes spin {
          to { transform: rotate(360deg); }
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "raster",
              fluidRow(
                box(
                  title = "Load Raster Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fileInput("rasterFile", "Choose a Raster File",
                            accept = c(".tif", ".asc")),
                  actionButton("loadRaster", "Load Raster", 
                               icon = icon("upload"), class = "btn-primary"),
                  bsTooltip("loadRaster", "Click to load the selected raster file", 
                            placement = "right", trigger = "hover")
                )
              ),
              fluidRow(
                box(
                  title = "Band Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("bandSelectionUI")
                )
              ),
              helpText("Supported formats: .tif, .asc, etc.")
      ),
      tabItem(tabName = "shapefile",
              fluidRow(
                box(
                  title = "Create Shapefile",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  textInput("shapeFileName", "Enter a name for the Shapefile"),
                  textInput("columns", "Enter column names (comma separated)"),
                  actionButton("createShapefile", "Create Shapefile", 
                               icon = icon("plus-circle"), class = "btn-success"),
                  bsTooltip("createShapefile", "Click to create a new shapefile", 
                            placement = "right", trigger = "hover")
                )
              ),
              fluidRow(
                box(
                  title = "Save Shapefile",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("savedirselectshp", "Select a Directory", 
                               icon = icon("folder-open"), class = "btn-success"),
                  verbatimTextOutput("shp_saving_path"),
                  actionButton("saveShapefile", "Save Shapefile", 
                               icon = icon("save"), class = "btn-success", disabled = TRUE),
                  bsTooltip("saveShapefile", "Click to save the shapefile", 
                            placement = "right", trigger = "hover")
                )
              )
      ),
      tabItem(tabName = "random_forest",
              tabBox(
                width = 12,
                id = "randomForestTabset",
                tabPanel("Training",
                         fluidRow(
                           box(
                             title = "Load Training Data",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             fileInput("trainingShapefile", "Choose Shapefile (select all files)",
                                       multiple = TRUE,
                                       accept = c('.shp', '.dbf', '.sbn', '.sbx', 
                                                  '.shx', '.prj', '.cpg', '.xml')),
                             actionButton("loadTrainingdata", "Load Training Data", 
                                          icon = icon("upload"), class = "btn-primary"),
                             bsTooltip("loadTrainingdata", "Click to load training data", 
                                       placement = "right", trigger = "hover")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Display and Customize Polygons",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             uiOutput("showpolyonmapbutton"),
                             uiOutput("symbology_training")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Model Parameters",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             uiOutput("trainingClassColumnUI"),
                             actionButton("selectsaving", "Select a Directory", 
                                          icon = icon("folder-open"), class = "btn-secondary"),
                             verbatimTextOutput("model_saving_path"),
                             textInput("modelname", "Enter the name of the model"),
                             uiOutput("RunTraining")
                           )
                         )
                ),
                tabPanel("Prediction",
                         fluidRow(
                           box(
                             title = "Load Trained Model",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 12,
                             fileInput("modelFile", "Choose a Trained Model File", 
                                       accept = c(".rds")),
                             actionButton("loadModel", "Load Model", 
                                          icon = icon("upload"), class = "btn-primary"),
                             bsTooltip("loadModel", "Click to load the trained model", 
                                       placement = "right", trigger = "hover")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Prediction Settings",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 12,
                             actionButton("selectsaving_prediction", "Select a Directory", 
                                          icon = icon("folder-open"), class = "btn-secondary"),
                             verbatimTextOutput("prediction_saving_path"),
                             textInput("prediction_name", "Enter the name of the prediction")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Run Prediction",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 12,
                             actionButton("predictRF", "Predict on Raster", 
                                          icon = icon("play"), class = "btn-primary"),
                             bsTooltip("predictRF", "Click to start prediction", 
                                       placement = "right", trigger = "hover")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Raster Symbology",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 12,
                             actionButton("Symbology_Pred", "Symbology", 
                                          icon = icon("palette"), class = "btn-primary", disabled = TRUE),
                             bsTooltip("Symbology_Pred", "Click to adjust symbology", 
                                       placement = "right", trigger = "hover")
                           )
                         )
                ),
                tabPanel("Validation",
                         fluidRow(
                           box(
                             title = "Load Validation Data",
                             status = "danger",
                             solidHeader = TRUE,
                             width = 12,
                             fileInput("validationShapefile", "Choose Shapefile (select all files)",
                                       multiple = TRUE,
                                       accept = c('.shp', '.dbf', '.sbn', '.sbx', 
                                                  '.shx', '.prj', '.cpg', '.xml')),
                             bsTooltip("validationShapefile", "Select validation shapefile files", 
                                       placement = "right", trigger = "hover")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Validation Parameters",
                             status = "danger",
                             solidHeader = TRUE,
                             width = 12,
                             uiOutput("validationClassColumnUI")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Run Validation",
                             status = "danger",
                             solidHeader = TRUE,
                             width = 12,
                             actionButton("validateRF", "Validate Model", 
                                          icon = icon("check"), class = "btn-primary"),
                             bsTooltip("validateRF", "Click to validate the model", 
                                       placement = "right", trigger = "hover"),
                             verbatimTextOutput("validationResults")
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  width = 12,
                  leafletOutput("map", height = "85vh") %>% withSpinner(color = "#3498db")
                )
              )
      )
    ),
    # Loading animation
    tags$div(id = "loading-animation", tags$div(class = "loader animate__animated animate__fadeIn"))
  )
)


#################### UI 3 #############################


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "My Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster", tabName = "raster", icon = icon("image")),
      menuItem("Shapefile", tabName = "shapefile", icon = icon("draw-polygon")),
      menuItem("Random Forest", tabName = "random_forest", icon = icon("tree"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # Include animate.css for animations
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$style(HTML("
        /* Custom CSS for loader */
        #loading-animation {
          position: fixed;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          z-index: 2000;
        }
        .loader {
          border: 12px solid #f3f3f3;
          border-top: 12px solid #3498db;
          border-radius: 50%;
          width: 70px;
          height: 70px;
          animation: spin 2s linear infinite;
        }
        @keyframes spin {
          to { transform: rotate(360deg); }
        }
        /* Adjustments for the sidebar */
        .sidebar {
          height: 100vh;
          overflow-y: auto;
        }
      "))
    ),
    fluidRow(
      column(
        width = 4,
        tabItems(
          tabItem(tabName = "raster",
                  box(
                    title = "Load Raster Data",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    fileInput("rasterFile", "Choose a Raster File",
                              accept = c(".tif", ".asc")),
                    actionButton("loadRaster", "Load Raster", 
                                 icon = icon("upload"), class = "btn-primary"),
                    bsTooltip("loadRaster", "Click to load the selected raster file", 
                              placement = "right", trigger = "hover"),
                    helpText("Supported formats: .tif, .asc, etc.")
                  ),
                  box(
                    title = "Band Selection",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    uiOutput("bandSelectionUI")
                  )
          ),
          tabItem(tabName = "shapefile",
                  box(
                    title = "Create Shapefile",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    textInput("shapeFileName", "Enter a name for the Shapefile"),
                    textInput("columns", "Enter column names (comma separated)"),
                    actionButton("createShapefile", "Create Shapefile", 
                                 icon = icon("plus-circle"), class = "btn-success"),
                    bsTooltip("createShapefile", "Click to create a new shapefile", 
                              placement = "right", trigger = "hover")
                  ),
                  box(
                    title = "Save Shapefile",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    actionButton("savedirselectshp", "Select a Directory", 
                                 icon = icon("folder-open"), class = "btn-success"),
                    verbatimTextOutput("shp_saving_path"),
                    actionButton("saveShapefile", "Save Shapefile", 
                                 icon = icon("save"), class = "btn-success", disabled = TRUE),
                    bsTooltip("saveShapefile", "Click to save the shapefile", 
                              placement = "right", trigger = "hover")
                  )
          ),
          tabItem(tabName = "random_forest",
                  tabBox(
                    width = NULL,
                    id = "randomForestTabset",
                    tabPanel("Training",
                             box(
                               title = "Load Training Data",
                               status = "info",
                               solidHeader = TRUE,
                               width = NULL,
                               fileInput("trainingShapefile", "Choose Shapefile (select all files)",
                                         multiple = TRUE,
                                         accept = c('.shp', '.dbf', '.sbn', '.sbx', 
                                                    '.shx', '.prj', '.cpg', '.xml')),
                               actionButton("loadTrainingdata", "Load Training Data", 
                                            icon = icon("upload"), class = "btn-primary"),
                               bsTooltip("loadTrainingdata", "Click to load training data", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Display and Customize Polygons",
                               status = "info",
                               solidHeader = TRUE,
                               width = NULL,
                               uiOutput("showpolyonmapbutton"),
                               uiOutput("symbology_training")
                             ),
                             box(
                               title = "Model Parameters",
                               status = "info",
                               solidHeader = TRUE,
                               width = NULL,
                               uiOutput("trainingClassColumnUI"),
                               actionButton("selectsaving", "Select a Directory", 
                                            icon = icon("folder-open"), class = "btn-secondary"),
                               verbatimTextOutput("model_saving_path"),
                               textInput("modelname", "Enter the name of the model"),
                               uiOutput("RunTraining")
                             )
                    ),
                    tabPanel("Prediction",
                             box(
                               title = "Load Trained Model",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               fileInput("modelFile", "Choose a Trained Model File", 
                                         accept = c(".rds")),
                               actionButton("loadModel", "Load Model", 
                                            icon = icon("upload"), class = "btn-primary"),
                               bsTooltip("loadModel", "Click to load the trained model", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Prediction Settings",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("selectsaving_prediction", "Select a Directory", 
                                            icon = icon("folder-open"), class = "btn-secondary"),
                               verbatimTextOutput("prediction_saving_path"),
                               textInput("prediction_name", "Enter the name of the prediction")
                             ),
                             box(
                               title = "Run Prediction",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("predictRF", "Predict on Raster", 
                                            icon = icon("play"), class = "btn-primary"),
                               bsTooltip("predictRF", "Click to start prediction", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Raster Symbology",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("Symbology_Pred", "Symbology", 
                                            icon = icon("palette"), class = "btn-primary", disabled = TRUE),
                               bsTooltip("Symbology_Pred", "Click to adjust symbology", 
                                         placement = "right", trigger = "hover")
                             )
                    ),
                    tabPanel("Validation",
                             box(
                               title = "Load Validation Data",
                               status = "danger",
                               solidHeader = TRUE,
                               width = NULL,
                               fileInput("validationShapefile", "Choose Shapefile (select all files)",
                                         multiple = TRUE,
                                         accept = c('.shp', '.dbf', '.sbn', '.sbx', 
                                                    '.shx', '.prj', '.cpg', '.xml')),
                               bsTooltip("validationShapefile", "Select validation shapefile files", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Validation Parameters",
                               status = "danger",
                               solidHeader = TRUE,
                               width = NULL,
                               uiOutput("validationClassColumnUI")
                             ),
                             box(
                               title = "Run Validation",
                               status = "danger",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("validateRF", "Validate Model", 
                                            icon = icon("check"), class = "btn-primary"),
                               bsTooltip("validateRF", "Click to validate the model", 
                                         placement = "right", trigger = "hover"),
                               verbatimTextOutput("validationResults")
                             )
                    )
                  )
          )
        )
      ),
      column(
        width = 8,
        box(
          width = NULL,
          leafletOutput("map", height = "90vh") %>% withSpinner(color = "#3498db")
        )
      )
    ),
    # Loading animation
    tags$div(id = "loading-animation", tags$div(class = "loader animate__animated animate__fadeIn"))
  )
)


################## UI4 #########################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/c1/Logotype_Nantes-U_noir-72dpi.png", height = "30px", style = "margin-right:10px;"),
      "RFapp"
    ),
    tags$li(class = "dropdown",
            tags$a(href = "#", icon("info-circle"), id = "about_link", 
                   style = "font-size:18px; margin-right:15px;"),
            bsTooltip("about_link", "About this app", placement = "bottom", trigger = "hover")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster", tabName = "raster", icon = icon("image")),
      menuItem("Shapefile", tabName = "shapefile", icon = icon("draw-polygon")),
      menuItem("Random Forest", tabName = "random_forest", icon = icon("tree"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # Include animate.css for animations
    tags$head(
      tags$link(rel = "stylesheet", 
                href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$style(HTML("
        /* Custom CSS for loader */
        #loading-animation {
          position: fixed;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          z-index: 2000;
        }
        .loader {
          border: 12px solid #f3f3f3;
          border-top: 12px solid #3498db;
          border-radius: 50%;
          width: 70px;
          height: 70px;
          animation: spin 2s linear infinite;
        }
        @keyframes spin {
          to { transform: rotate(360deg); }
        }
        /* Adjustments for the sidebar */
        .sidebar {
          height: 80vh;
          overflow-y: auto;
        }
      "))
    ),
    fluidRow(
      column(
        width = 4,
        tabItems(
          tabItem(tabName = "raster",
                  box(
                    title = "Load Raster Data",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    fileInput("rasterFile", "Choose a Raster File",
                              accept = c(".tif", ".asc")),
                    actionButton("loadRaster", "Load Raster", 
                                 icon = icon("upload"), class = "btn-primary"),
                    bsTooltip("loadRaster", "Click to load the selected raster file", 
                              placement = "right", trigger = "hover"),
                    helpText("Supported formats: .tif, .asc, etc.")
                  ),
                  box(
                    title = "Band Selection",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    uiOutput("bandSelectionUI")
                  )
          ),
          tabItem(tabName = "shapefile",
                  box(
                    title = "Create Shapefile",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    textInput("shapeFileName", "Enter a name for the Shapefile"),
                    textInput("columns", "Enter column names (comma separated)"),
                    actionButton("createShapefile", "Create Shapefile", 
                                 icon = icon("plus-circle"), class = "btn-success"),
                    bsTooltip("createShapefile", "Click to create a new shapefile", 
                              placement = "right", trigger = "hover")
                  ),
                  box(
                    title = "Save Shapefile",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    actionButton("savedirselectshp", "Select a Directory", 
                                 icon = icon("folder-open"), class = "btn-success"),
                    verbatimTextOutput("shp_saving_path"),
                    actionButton("saveShapefile", "Save Shapefile", 
                                 icon = icon("save"), class = "btn-success", disabled = TRUE),
                    bsTooltip("saveShapefile", "Click to save the shapefile", 
                              placement = "right", trigger = "hover")
                  )
          ),
          tabItem(tabName = "random_forest",
                  tabBox(
                    width = NULL,
                    id = "randomForestTabset",
                    tabPanel("Training",
                             box(
                               title = "Load Training Data",
                               status = "info",
                               solidHeader = TRUE,
                               width = NULL,
                               fileInput("trainingShapefile", "Choose Shapefile (select all files)",
                                         multiple = TRUE,
                                         accept = c('.shp', '.dbf', '.sbn', '.sbx', 
                                                    '.shx', '.prj', '.cpg', '.xml')),
                               actionButton("loadTrainingdata", "Load Training Data", 
                                            icon = icon("upload"), class = "btn-primary"),
                               bsTooltip("loadTrainingdata", "Click to load training data", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Display and Customize Polygons",
                               status = "info",
                               solidHeader = TRUE,
                               width = NULL,
                               uiOutput("showpolyonmapbutton"),
                               uiOutput("symbology_training")
                             ),
                             box(
                               title = "Model Parameters",
                               status = "info",
                               solidHeader = TRUE,
                               width = NULL,
                               uiOutput("trainingClassColumnUI"),
                               actionButton("selectsaving", "Select a Directory", 
                                            icon = icon("folder-open"), class = "btn-secondary"),
                               verbatimTextOutput("model_saving_path"),
                               textInput("modelname", "Enter the name of the model"),
                               uiOutput("RunTraining")
                             )
                    ),
                    tabPanel("Prediction",
                             box(
                               title = "Load Trained Model",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               fileInput("modelFile", "Choose a Trained Model File", 
                                         accept = c(".rds")),
                               actionButton("loadModel", "Load Model", 
                                            icon = icon("upload"), class = "btn-primary"),
                               bsTooltip("loadModel", "Click to load the trained model", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Prediction Settings",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("selectsaving_prediction", "Select a Directory", 
                                            icon = icon("folder-open"), class = "btn-secondary"),
                               verbatimTextOutput("prediction_saving_path"),
                               textInput("prediction_name", "Enter the name of the prediction")
                             ),
                             box(
                               title = "Run Prediction",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("predictRF", "Predict on Raster", 
                                            icon = icon("play"), class = "btn-primary"),
                               bsTooltip("predictRF", "Click to start prediction", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Raster Symbology",
                               status = "warning",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("Symbology_Pred", "Symbology", 
                                            icon = icon("palette"), class = "btn-primary", disabled = TRUE),
                               bsTooltip("Symbology_Pred", "Click to adjust symbology", 
                                         placement = "right", trigger = "hover")
                             )
                    ),
                    tabPanel("Validation",
                             box(
                               title = "Load Validation Data",
                               status = "danger",
                               solidHeader = TRUE,
                               width = NULL,
                               fileInput("validationShapefile", "Choose Shapefile (select all files)",
                                         multiple = TRUE,
                                         accept = c('.shp', '.dbf', '.sbn', '.sbx', 
                                                    '.shx', '.prj', '.cpg', '.xml')),
                               bsTooltip("validationShapefile", "Select validation shapefile files", 
                                         placement = "right", trigger = "hover")
                             ),
                             box(
                               title = "Validation Parameters",
                               status = "danger",
                               solidHeader = TRUE,
                               width = NULL,
                               uiOutput("validationClassColumnUI")
                             ),
                             box(
                               title = "Run Validation",
                               status = "danger",
                               solidHeader = TRUE,
                               width = NULL,
                               actionButton("validateRF", "Validate Model", 
                                            icon = icon("check"), class = "btn-primary"),
                               bsTooltip("validateRF", "Click to validate the model", 
                                         placement = "right", trigger = "hover"),
                               verbatimTextOutput("validationResults")
                             )
                    )
                  )
          )
        )
      ),
      column(
        width = 8,
        box(
          width = NULL,
          leafletOutput("map", height = "88vh") %>% withSpinner(color = "#3498db")
        )
      )
    ),
    # Loading animation
    tags$div(id = "loading-animation", 
             tags$div(class = "loader animate__animated animate__fadeIn")),
    # About modal
    bsModal("aboutModal", "About This App", "about_link", size = "medium",
            p("This application was developed for teaching purposes, for master students of the University of Nantes, France"),
            p("It allows users to load raster data, create and manage shapefiles, 
              and perform Random Forest analysis, including model training, predicting on raster, and validation."),
            p("For more information, please contact the developer : "),
            p("Simon Oiry : oirysimon@gmail.com")
    )
  )
)
