#########################################################
# Enhanced Brain Network Analysis Shiny App
# app.R - Main application file with integrated recommendations and citation agreement
#########################################################

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(dplyr)
library(ggplot2)
library(igraph)
library(reshape2)
library(tidyr)
library(readr)
library(openxlsx)
library(colourpicker)
library(RColorBrewer)
library(zip)  # Make sure this is installed

# Source fixed module files
source("module_import.R")  
source("module_preferences.R")   
source("module_results.R") 
source("utils.R")

# Define UI
ui <- shinydashboard::dashboardPage(
  # Dashboard header
  shinydashboard::dashboardHeader(
    title = "Brain Network Analysis",
    tags$li(
      class = "dropdown",
      actionButton("about_btn", "About", icon = icon("info-circle"))
    )
  ),
  
  # Dashboard sidebar
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("1. Data Import", tabName = "data_import", icon = icon("upload")),
      shinydashboard::menuItem("2. Analysis Preferences", tabName = "preferences", icon = icon("sliders-h")),
      shinydashboard::menuItem("3. Results", tabName = "results", icon = icon("chart-bar"))
    ),
    
    shiny::hr(),
    
    # Progress indicator
    shiny::uiOutput("progress_indicator"),
    
    shiny::hr(),
    
    # Footer info
    shiny::div(
      style = "padding: 15px;",
      shiny::p("Brain Network Analysis App", style = "font-weight: bold;"),
      shiny::p("Version 0.2"),
      shiny::p("© 2025", style = "font-size: 90%;"),
      shiny::p("Enhanced with smart parameter recommendations", style = "font-size: 85%; font-style: italic;")
    )
  ),
  
  shinydashboard::dashboardBody(
    # Use shinyjs
    shinyjs::useShinyjs(),
    
    # Custom CSS with scrolling fixes and enhanced styling
    tags$head(
      tags$style(
        HTML("
          /* Basic styling */
          .content-wrapper, .right-side {
            background-color: #f8f9fa;
            height: auto !important;
            min-height: 100vh;
            overflow-y: auto !important;
          }
          
          /* Fix scrolling issues */
          .tab-content {
            overflow-y: visible !important;
            height: auto !important;
            padding-bottom: 50px;
          }
          
          /* Prettier alerts */
          .alert {
            border-radius: 4px;
            margin-bottom: 20px;
          }
          
          .alert-info {
            background-color: #d1ecf1;
            border-color: #bee5eb;
            color: #0c5460;
          }
          
          .alert-success {
            background-color: #d4edda;
            border-color: #c3e6cb;
            color: #155724;
          }
          
          .alert-warning {
            background-color: #fff3cd;
            border-color: #ffeeba;
            color: #856404;
          }
          
          /* Progress indicator styling */
          .progress-indicator {
            padding: 10px;
            margin-top: 10px;
          }
          
          .progress-item {
            padding: 5px;
            margin-bottom: 5px;
            border-radius: 3px;
          }
          
          .progress-complete {
            background-color: #d4edda;
            color: #155724;
          }
          
          .progress-pending {
            background-color: #f8f9fa;
            color: #6c757d;
          }
          
          .progress-active {
            background-color: #cce5ff;
            color: #004085;
          }
          
          /* Enhanced notifications */
          .shiny-notification {
            border-radius: 6px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          }
          
          .shiny-notification.shiny-notification-message {
            background-color: #e3f2fd;
            border-left: 4px solid #2196f3;
          }
          
          .shiny-notification.shiny-notification-warning {
            background-color: #fff8e1;
            border-left: 4px solid #ff9800;
          }
          
          .shiny-notification.shiny-notification-error {
            background-color: #ffebee;
            border-left: 4px solid #f44336;
          }
          
          /* Improved button styling */
          .btn-success {
            background-color: #4caf50;
            border-color: #4caf50;
            box-shadow: 0 2px 4px rgba(76,175,80,0.3);
          }
          
          .btn-success:hover {
            background-color: #45a049;
            border-color: #45a049;
            box-shadow: 0 4px 8px rgba(76,175,80,0.4);
          }
          
          .btn-primary {
            background-color: #2196f3;
            border-color: #2196f3;
            box-shadow: 0 2px 4px rgba(33,150,243,0.3);
          }
          
          .btn-primary:hover {
            background-color: #1976d2;
            border-color: #1976d2;
            box-shadow: 0 4px 8px rgba(33,150,243,0.4);
          }
          
          /* Enhanced well panels */
          .well {
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.08);
            border: 1px solid #e0e0e0;
          }
          
          /* Loading overlay */
          .loading-overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background-color: rgba(255,255,255,0.9);
            z-index: 9999;
            display: flex;
            justify-content: center;
            align-items: center;
            font-size: 18px;
            color: #666;
          }
          
          .loading-spinner {
            border: 4px solid #f3f3f3;
            border-top: 4px solid #3498db;
            border-radius: 50%;
            width: 40px;
            height: 40px;
            animation: spin 1s linear infinite;
            margin-right: 15px;
          }
          
          @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
          }
          
          /* Help panel styling */
          .help-panel {
            margin-top: 5px;
            margin-bottom: 10px;
            padding: 8px 12px;
            background-color: #f8f9fa;
            border-left: 4px solid #6c757d;
            border-radius: 3px;
            font-size: 0.9em;
          }
          
          .help-title {
            font-weight: bold;
            color: #495057;
            cursor: pointer;
            display: flex;
            justify-content: space-between;
            align-items: center;
          }
          
          .help-icon {
            color: #6c757d;
            margin-right: 5px;
          }
          
          .help-content {
            margin-top: 8px;
            display: none;
          }
          
          .help-panel.active .help-content {
            display: block;
          }
          
          /* Citation agreement modal styling */
          .citation-box {
            background: #f8f9fa;
            padding: 15px;
            border-left: 4px solid #007bff;
            margin: 15px 0;
            font-family: monospace;
            line-height: 1.6;
          }
          
          .citation-checkbox {
            margin-top: 20px;
            padding: 15px;
            background: #e3f2fd;
            border-radius: 5px;
          }
          
          .continue-button {
            width: 100%;
            padding: 10px;
            font-weight: bold;
          }
          
          /* Modal dialog enhancements */
          .modal-header {
            background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
            border-bottom: 3px solid #007bff;
          }
          
          .modal-title {
            font-weight: bold;
            color: #2c3e50;
          }
          
          .modal-body h4 {
            color: #2c3e50;
            border-bottom: 2px solid #007bff;
            padding-bottom: 8px;
            margin-top: 20px;
          }
        ")
      ),
      
      # Simple help panel toggle JavaScript
      tags$script(
        HTML("
          $(document).ready(function() {
            $(document).on('click', '.help-title', function() {
              $(this).parent().toggleClass('active');
            });
          });
        ")
      )
    ),
    
    # Tab content
    shinydashboard::tabItems(
      # Data Import tab
      shinydashboard::tabItem(
        tabName = "data_import",
        dataImportUI("data_import")
      ),
      
      # Preferences tab
      shinydashboard::tabItem(
        tabName = "preferences",
        preferencesUI("preferences")
      ),
      
      # Results tab
      shinydashboard::tabItem(
        tabName = "results",
        resultsUI("results")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize reactive values for workflow tracking
  workflow <- shiny::reactiveValues(
    data_import_complete = FALSE,
    preferences_complete = FALSE,
    current_step = "data_import",
    citation_agreed = FALSE  # New value to track citation agreement
  )
  
  # Show citation agreement modal when app starts
  shiny::showModal(
    shiny::modalDialog(
      title = "Citation Agreement",
      
      shiny::tags$div(
        shiny::tags$h4("How to Cite This Software", style = "color: #2c3e50; border-bottom: 2px solid #007bff; padding-bottom: 8px;"),
        shiny::tags$p("If you use this software in your research, please cite it as follows:"),
        shiny::tags$div(
          class = "citation-box",
          style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 15px 0; font-family: monospace; line-height: 1.6;",
          shiny::tags$p(
            "McGovern et al (2025). Insert paper information here"
          ),
          shiny::tags$p("GitHub: tbd")
        ),
        
        shiny::tags$h4("Software Information", style = "color: #2c3e50; border-bottom: 2px solid #007bff; padding-bottom: 8px; margin-top: 20px;"),
        shiny::tags$div(
          style = "display: flex; justify-content: space-between;",
          shiny::tags$div(
            style = "width: 48%;",
            shiny::tags$strong("Version:"), " 0.2"
          ),
          shiny::tags$div(
            style = "width: 48%;",
            shiny::tags$strong("Released:"), " June 2025"
          )
        ),
        
        shiny::tags$p(
          style = "font-style: italic; color: #666; margin-top: 15px;",
          "This software provides tools for analyzing functional connectivity networks from preclinical neuroscience data. The application implements advanced statistical methods for correlation analysis, community detection, and network visualization."
        ),
        
        shiny::tags$hr(style = "margin: 20px 0; border-top: 1px solid #eee;"),
        
        shiny::tags$div(
          class = "citation-checkbox",
          style = "margin-top: 20px; padding: 15px; background: #e3f2fd; border-radius: 5px;",
          shiny::checkboxInput(
            "citation_agreement",
            "I agree to cite this software in any publication or presentation that uses results generated by this application",
            value = FALSE,
            width = "100%"
          )
        )
      ),
      
      footer = shiny::tagList(
        shiny::actionButton(
          "accept_citation",
          "Continue to Application",
          class = "btn-primary continue-button",
          style = "width: 100%; padding: 10px; font-weight: bold;"
        )
      ),
      
      size = "m",
      easyClose = FALSE,
      fade = TRUE
    )
  )
  
  # Only remove the modal when the user agrees and clicks Continue
  shiny::observeEvent(input$accept_citation, {
    if (input$citation_agreement) {
      workflow$citation_agreed <- TRUE
      shiny::removeModal()
      shiny::showNotification(
        "Thank you for agreeing to cite this software!",
        type = "message",
        duration = 5
      )
    } else {
      shiny::showNotification(
        "You must agree to cite this software to proceed.",
        type = "warning",
        duration = 5
      )
    }
  })
  
  # Module calls
  data_import_results <- dataImport("data_import", parent_session = session)
  preferences_results <- preferences("preferences", data_import_results)
  results_info <- results("results", data_import_results, preferences_results)
  
  # Update workflow status based on module results
  observe({
    # Update status flags
    new_import_status <- FALSE
    new_preferences_status <- FALSE
    
    # Safely check data import status
    if (!is.null(data_import_results())) {
      if (!is.null(data_import_results()$data_configured)) {
        new_import_status <- data_import_results()$data_configured
      }
    }
    
    # Safely check preferences status
    if (!is.null(preferences_results())) {
      if (!is.null(preferences_results()$configuration_saved)) {
        new_preferences_status <- preferences_results()$configuration_saved
      }
    }
    
    # Update workflow state
    if (!workflow$data_import_complete && new_import_status) {
      workflow$data_import_complete <- TRUE
      workflow$current_step <- "preferences"
      
      # Show notification
      shiny::showNotification(
        "Data import completed! You can now configure analysis preferences.",
        type = "message",
        duration = 5
      )
    }
    
    if (!workflow$preferences_complete && new_preferences_status) {
      workflow$preferences_complete <- TRUE
      workflow$current_step <- "results"
      
      # Show notification
      shiny::showNotification(
        "Preferences configured! Proceeding to analysis results.",
        type = "message",
        duration = 5
      )
    }
  })
  
  # Enhanced auto-navigation with smooth transitions
  observe({
    current_tab <- input$tabs
    
    # Auto-navigate to preferences when data import is complete
    if (workflow$data_import_complete && 
        !workflow$preferences_complete && 
        current_tab == "data_import") {
      
      # Small delay for smooth transition
      shiny::invalidateLater(1000, session)
      shinydashboard::updateTabItems(session, "tabs", "preferences")
    }
    
    # Auto-navigate to results when preferences are complete
    if (workflow$preferences_complete && current_tab == "preferences") {
      # Small delay for smooth transition
      shiny::invalidateLater(1000, session)
      shinydashboard::updateTabItems(session, "tabs", "results")
    }
  })
  
  # Enhanced tab access control with user-friendly messages
  observe({
    # Trigger the observer when tab changes or workflow status changes
    input$tabs
    workflow$data_import_complete
    workflow$preferences_complete
    workflow$citation_agreed
    
    # Disable all tabs if citation not agreed
    if (!workflow$citation_agreed) {
      shinyjs::disable(selector = ".sidebar-menu li")
      return()
    }
    
    # Enable first tab always after citation agreement
    shinyjs::enable(selector = ".sidebar-menu li:nth-child(1)")
    
    # Preferences tab requires completed data import
    if (!workflow$data_import_complete) {
      shinyjs::disable(selector = ".sidebar-menu li:nth-child(2)")
    } else {
      shinyjs::enable(selector = ".sidebar-menu li:nth-child(2)")
    }
    
    # Results tab requires completed preferences
    if (!workflow$preferences_complete) {
      shinyjs::disable(selector = ".sidebar-menu li:nth-child(3)")
    } else {
      shinyjs::enable(selector = ".sidebar-menu li:nth-child(3)")
    }
  })
  
  # Enhanced progress indicator in sidebar
  output$progress_indicator <- shiny::renderUI({
    shiny::div(
      class = "progress-indicator",
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "data_import") "progress-active" 
          else if (workflow$data_import_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$data_import_complete) "check-circle" else "circle"),
        "1. Data Import",
        if (workflow$data_import_complete) 
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "✓ Complete")
        else if (input$tabs == "data_import")
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "In Progress...")
        else
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "Pending")
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "preferences") "progress-active" 
          else if (workflow$preferences_complete) "progress-complete" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$preferences_complete) "check-circle" else "circle"),
        "2. Analysis Preferences",
        if (workflow$preferences_complete) 
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "✓ Complete")
        else if (input$tabs == "preferences" && workflow$data_import_complete)
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "In Progress...")
        else if (workflow$data_import_complete)
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "Ready")
        else
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "Locked")
      ),
      
      shiny::div(
        class = paste(
          "progress-item",
          if (input$tabs == "results") "progress-active" 
          else "progress-pending"
        ),
        shiny::icon(if (workflow$preferences_complete) "chart-bar" else "lock"),
        "3. Results",
        if (input$tabs == "results" && workflow$preferences_complete)
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "Analyzing...")
        else if (workflow$preferences_complete)
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "Ready")
        else
          shiny::tags$small(style = "display: block; margin-top: 2px; font-size: 0.8em;", "Locked")
      )
    )
  })
  
  # Enhanced manual tab navigation with better user feedback
  observeEvent(input$tabs, {
    # First check if citation has been agreed to
    if (!workflow$citation_agreed) {
      shiny::showNotification(
        "Please agree to the citation requirement before proceeding.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    # When user manually navigates, check if they're going to a tab that requires previous steps
    current_tab <- input$tabs
    
    # Define required workflows for each tab
    tab_requirements <- list(
      "data_import" = NULL,  # No requirements for first tab
      "preferences" = "data_import_complete",
      "results" = "preferences_complete"
    )
    
    # Check if current tab has requirements
    if (!is.null(tab_requirements[[current_tab]])) {
      required_workflow <- tab_requirements[[current_tab]]
      
      # If requirement is not met, show enhanced warning and go back to appropriate tab
      if (!is.null(required_workflow) && !workflow[[required_workflow]]) {
        # Determine which tab to go back to and what step is needed
        go_back_info <- switch(current_tab,
                               "preferences" = list(tab = "data_import", step = "import your data"),
                               "results" = list(tab = "preferences", step = "configure analysis preferences")
        )
        
        # Show enhanced warning with guidance
        shiny::showNotification(
          HTML(paste0(
            "<strong>Step Required:</strong><br/>",
            "Please ", go_back_info$step, " before proceeding to this section."
          )),
          type = "warning",
          duration = 8
        )
        
        # Navigate back to appropriate tab
        shinydashboard::updateTabItems(session, "tabs", go_back_info$tab)
      }
    }
  })
  
  # Enhanced About modal with new features
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About Brain Network Analysis App",
      
      p("This application enables researchers to analyze functional connectivity networks in the brain using preclinical data with advanced statistical methods for robust network analysis."),
      
      h4("Key Features:"),
      tags$ul(
        tags$li(strong("Adaptive Parameter Optimization:"), " Data-driven algorithm for optimizing correlation methods, thresholds, and statistical approaches based on sample size and data characteristics"),
        
        tags$li(strong("Comprehensive Data Quality Assessment:"), " Multi-metric evaluation including distribution analysis, outlier detection, statistical power calculation, and group balance verification"),
        
        tags$li(strong("Advanced Connectivity Methodologies:"), " Implementation of standard, partial, and regularized correlation techniques with robust estimation options for handling non-normal distributions and outliers"),
        
        tags$li(strong("Rigorous Statistical Framework:"), " Integrated hypothesis testing with false discovery rate control and significance thresholding for reliable network edge detection"),
        
        tags$li(strong("Interactive Topological Visualization:"), " Dynamic network representations with force-directed, anatomically-informed, and community-based layouts with quantitative node metrics"),
        
        tags$li(strong("Anatomical-Functional Integration:"), " Tools for mapping connectivity patterns to anatomical brain organization and analyzing distance-dependent connectivity profiles"),
        
        tags$li(strong("Multi-condition Comparative Analysis:"), " Statistical comparison of network topology, edge weights, and node centrality measures between experimental conditions"),
        
        tags$li(strong("Comprehensive Results Integration:"), " Export functionality for publication-ready visualizations, raw connectivity matrices, and statistical reports in standardized formats")
      ),
      
      # Add citation information to the About modal as well
      h4("How to Cite:"),
      tags$div(
        class = "citation-box",
        style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 15px 0; font-family: monospace; line-height: 1.6;",
        tags$p(
          "McGovern et al (2025). Publication info to be inserted here"
        )
      ),
      
      footer = modalButton("Close"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # Enhanced error handling and user feedback
  session$onSessionEnded(function() {
    message("Session ended. Cleaning up...")
  })
  
  observeEvent(shiny::getShinyOption("shiny.error"), {
    err <- shiny::getShinyOption("shiny.error")
    message("Error occurred: ", err$message)
    
    shiny::showNotification(
      HTML(paste0(
        "<strong>An unexpected error occurred:</strong><br/>",
        "Please check the console for more details."
      )),
      type = "error",
      duration = 10
    )
  })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
