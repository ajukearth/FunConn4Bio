#########################################################
# Enhanced Brain Network Analysis Shiny App
# module_import.R - Enhanced data import module with comprehensive quality assessment
#########################################################

#' UI for the data import module
#' 
#' @param id Namespace identifier for the module
#' @return A shiny tagList containing UI elements
#' @export
dataImportUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3("Data Import"),
        shiny::p("Upload your preclinical functional connectivity data file to begin the analysis.")
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::fileInput(
            ns("file"), 
            "Upload CSV or Excel File",
            accept = c(
              "text/csv",
              "application/vnd.ms-excel",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            )
          ),
          
          shiny::conditionalPanel(
            condition = paste0("output['", ns("file_uploaded"), "']"),
            shiny::selectInput(
              ns("sheet_name"),
              "Excel Sheet (if applicable):",
              choices = c("First sheet" = "")
            )
          ),
          
          shiny::hr(),
          
          shiny::downloadButton(
            ns("download_template"),
            "Download Template"
          ),
          
          shiny::hr(),
          
          shiny::wellPanel(
            shiny::h4("Data Format Information"),
            shiny::HTML("
              <p>Your data should be structured with:</p>
              <ul>
                <li>A Subject/ID column for each sample</li>
                <li>One or more grouping columns (e.g., condition, treatment)</li>
                <li>Multiple columns for brain region activity measurements</li>
                <li>Optional behavioral measure columns</li>
              </ul>
              <p><strong>Quality considerations:</strong></p>
              <ul>
                <li>Provide data from at least 5-10 subjects per group for robust analysis</li>
                <li>Minimize missing values where possible</li>
                <li>Check for outliers or data entry errors</li>
                <li>Use consistent units and scales across regions</li>
              </ul>
              <p><em>The app will automatically assess your data quality and provide recommendations for optimal analysis parameters.</em></p>
            ")
          )
        )
      ),
      
      shiny::column(
        width = 8,
        shiny::conditionalPanel(
          condition = paste0("output['", ns("data_available"), "']"),
          shiny::wellPanel(
            shiny::h4("Data Configuration"),
            
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectInput(
                  ns("id_column"),
                  "Subject/ID Column:",
                  choices = c("Select column" = "")
                )
              ),
              
              shiny::column(
                width = 6,
                shiny::selectizeInput(
                  ns("group_columns"),
                  "Group Columns:",
                  choices = c("Select columns" = ""),
                  multiple = TRUE
                )
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectizeInput(
                  ns("behavior_columns"),
                  "Behavioral Measure Columns:",
                  choices = c("Select columns" = ""),
                  multiple = TRUE,
                  options = list(placeholder = "Optional")
                )
              ),
              
              shiny::column(
                width = 6,
                shiny::checkboxInput(
                  ns("combine_groups"),
                  "Create Combined Group Column",
                  value = TRUE
                )
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::actionButton(
                  ns("configure_data"),
                  "Configure Data",
                  icon = shiny::icon("check"),
                  class = "btn-success"
                )
              )
            )
          )
        )
      )
    ),
    
    # Data Preview - Always show when data is loaded
    shiny::conditionalPanel(
      condition = paste0("output['", ns("data_available"), "']"),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4("Data Preview"),
          DT::DTOutput(ns("data_preview"))
        )
      )
    ),
    
    # Enhanced data quality summary when configuration is complete
    shiny::conditionalPanel(
      condition = paste0("output['", ns("config_complete"), "']"),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::wellPanel(
            shiny::h4("Data Configuration Summary"),
            shiny::verbatimTextOutput(ns("data_summary")),
            
            # Enhanced data quality assessment panel
            shiny::wellPanel(
              shiny::h4("Comprehensive Data Quality Assessment"),
              
              # Quick summary cards
              shiny::fluidRow(
                shiny::column(
                  width = 3,
                  shiny::div(
                    class = "info-card",
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 10px;",
                    shiny::h5(shiny::icon("users"), "Sample Size", style = "margin: 0; color: #1976d2;"),
                    shiny::h3(shiny::textOutput(ns("sample_size_display")), style = "margin: 5px 0; color: #0d47a1;")
                  )
                ),
                shiny::column(
                  width = 3,
                  shiny::div(
                    class = "info-card",
                    style = "background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%); padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 10px;",
                    shiny::h5(shiny::icon("brain"), "Brain Regions", style = "margin: 0; color: #7b1fa2;"),
                    shiny::h3(shiny::textOutput(ns("regions_count_display")), style = "margin: 5px 0; color: #4a148c;")
                  )
                ),
                shiny::column(
                  width = 3,
                  shiny::div(
                    class = "info-card",
                    style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 10px;",
                    shiny::h5(shiny::icon("layer-group"), "Groups", style = "margin: 0; color: #388e3c;"),
                    shiny::h3(shiny::textOutput(ns("groups_count_display")), style = "margin: 5px 0; color: #1b5e20;")
                  )
                ),
                shiny::column(
                  width = 3,
                  shiny::div(
                    class = "info-card",
                    style = "background: linear-gradient(135deg, #fff3e0 0%, #ffcc02 100%); padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 10px;",
                    shiny::h5(shiny::icon("exclamation-triangle"), "Data Quality", style = "margin: 0; color: #f57c00;"),
                    shiny::h3(shiny::textOutput(ns("quality_score_display")), style = "margin: 5px 0; color: #e65100;")
                  )
                )
              ),
              
              # Detailed quality assessment
              shiny::tabsetPanel(
                id = ns("quality_tabs"),
                
                shiny::tabPanel(
                  "Overall Assessment",
                  shiny::br(),
                  shiny::uiOutput(ns("overall_quality_ui"))
                ),
                
                shiny::tabPanel(
                  "Distribution Analysis",
                  shiny::br(),
                  shiny::uiOutput(ns("distribution_analysis_ui"))
                ),
                
                shiny::tabPanel(
                  "Outlier Detection",
                  shiny::br(),
                  shiny::uiOutput(ns("outlier_analysis_ui"))
                ),
                
                shiny::tabPanel(
                  "Statistical Power",
                  shiny::br(),
                  shiny::uiOutput(ns("power_analysis_ui"))
                ),
                
                shiny::tabPanel(
                  "Group Balance",
                  shiny::br(),
                  shiny::uiOutput(ns("group_balance_ui"))
                )
              )
            ),
            
            shiny::tags$div(
              style = "text-align: center; margin-top: 20px;",
              shiny::actionButton(
                ns("proceed_button"),
                "Proceed to Analysis Preferences",
                icon = shiny::icon("arrow-right"),
                class = "btn-lg btn-primary"
              )
            )
          )
        )
      )
    )
  )
}

#' Enhanced server function for the data import module
#' 
#' @param id Namespace identifier for the module
#' @param parent_session Parent session object (optional)
#' @return A reactive list with data and configuration info
#' @export
dataImport <- function(id, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive values to store data and configuration
    rv <- shiny::reactiveValues(
      raw_data = NULL,
      processed_data = NULL,
      validation_complete = FALSE,
      column_info = NULL,
      has_missing_data = FALSE,
      data_quality_metrics = NULL,
      quality_assessment_complete = FALSE
    )
    
    # Enhanced template data for download
    template_data <- data.frame(
      Subject = 1:8,
      Sex = c("M", "M", "F", "F", "M", "F", "M", "F"),
      Group = c("Control", "Stress", "Control", "Stress", "Control", "Stress", "Control", "Stress"),
      dDG = c(3.1, 4.2, 3.8, 4.5, 3.5, 3.9, 3.3, 4.1),
      dCA1 = c(2.3, 2.8, 2.6, 3.1, 2.5, 2.9, 2.4, 3.0),
      dCA3 = c(3.5, 3.9, 3.6, 4.1, 3.8, 4.0, 3.4, 3.7),
      vDG = c(4.2, 4.6, 4.5, 5.0, 4.4, 4.8, 4.1, 4.7),
      vCA1 = c(2.8, 3.2, 3.0, 3.5, 2.9, 3.3, 2.7, 3.1),
      BLA = c(5.1, 5.5, 5.3, 6.0, 5.2, 5.6, 5.0, 5.4),
      CeA = c(4.8, 5.2, 5.0, 5.5, 4.9, 5.3, 4.7, 5.1),
      ACC = c(6.2, 6.8, 6.5, 7.2, 6.4, 6.9, 6.1, 6.7),
      Freezing = c(25.3, 42.1, 23.8, 38.7, 27.2, 40.5, 24.6, 39.3)
    )
    
    # Handle file upload
    shiny::observeEvent(input$file, {
      req(input$file)
      
      # Reset reactive values
      rv$processed_data <- NULL
      rv$validation_complete <- FALSE
      rv$data_quality_metrics <- NULL
      rv$quality_assessment_complete <- FALSE
      
      # Check file extension
      ext <- tools::file_ext(input$file$name)
      
      tryCatch({
        if (ext == "csv") {
          rv$raw_data <- load_data(input$file$datapath)
          output$file_uploaded <- shiny::reactive(TRUE)
          shiny::outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
        } else if (ext %in% c("xls", "xlsx")) {
          # For Excel files, show sheet selection
          sheets <- openxlsx::getSheetNames(input$file$datapath)
          shiny::updateSelectInput(session, "sheet_name", choices = sheets)
          output$file_uploaded <- shiny::reactive(TRUE)
          shiny::outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
          
          # Load the first sheet by default
          rv$raw_data <- load_data(input$file$datapath, sheet_name = sheets[1])
        } else {
          shiny::showNotification("Unsupported file format. Please upload a CSV or Excel file.", 
                                  type = "error")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error loading file:", e$message), type = "error")
        rv$raw_data <- NULL
      })
    })
    
    # Handle Excel sheet selection
    shiny::observeEvent(input$sheet_name, {
      req(input$file, input$sheet_name)
      
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("xls", "xlsx") && input$sheet_name != "") {
        tryCatch({
          rv$raw_data <- load_data(input$file$datapath, sheet_name = input$sheet_name)
        }, error = function(e) {
          shiny::showNotification(paste("Error loading sheet:", e$message), type = "error")
        })
      }
    })
    
    # Download template
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        "brain_network_analysis_template.csv"
      },
      content = function(file) {
        write.csv(template_data, file, row.names = FALSE)
      }
    )
    
    # Update column selection dropdowns when data is available
    shiny::observe({
      req(rv$raw_data)
      
      # Get column names
      col_names <- names(rv$raw_data)
      
      # Update column selection inputs
      shiny::updateSelectInput(session, "id_column", choices = c("Select column" = "", col_names))
      shiny::updateSelectizeInput(session, "group_columns", choices = c(col_names))
      shiny::updateSelectizeInput(session, "behavior_columns", choices = c(col_names))
    })
    
    # Show data preview when data is available
    output$data_preview <- DT::renderDT({
      req(rv$raw_data)
      DT::datatable(
        rv$raw_data,
        options = list(
          scrollX = TRUE,
          scrollY = "300px",
          paging = TRUE,
          searching = TRUE,
          pageLength = 10,
          dom = 'ftip'
        ),
        rownames = FALSE
      )
    })
    
    # Reactive to check if data is available
    output$data_available <- shiny::reactive({
      return(!is.null(rv$raw_data))
    })
    shiny::outputOptions(output, "data_available", suspendWhenHidden = FALSE)
    
    # Configure data with enhanced quality assessment
    shiny::observeEvent(input$configure_data, {
      # Make sure we have data and column selection
      req(rv$raw_data, input$id_column)
      
      # Validate input selections with better error handling
      tryCatch({
        # Validate input selections
        if (input$id_column == "") {
          shiny::showNotification("Please select an ID column", type = "warning")
          return()
        }
        
        if (length(input$group_columns) == 0) {
          shiny::showNotification("Please select at least one group column", type = "warning")
          return()
        }
        
        # Show progress notification
        progress_id <- shiny::showNotification(
          "Configuring data and running quality assessment...",
          duration = NULL,
          closeButton = FALSE,
          type = "message"
        )
        
        on.exit({
          shiny::removeNotification(progress_id)
        })
        
        # Identify potential region columns
        all_columns <- names(rv$raw_data)
        excluded_cols <- c(input$id_column, input$group_columns, input$behavior_columns)
        potential_region_cols <- setdiff(all_columns, excluded_cols)
        
        if (length(potential_region_cols) == 0) {
          shiny::showNotification("No columns available for region data. Make sure not all columns are selected as ID, group, or behavior columns.", type = "error")
          return()
        }
        
        # Validate the data structure
        validation_results <- validate_data(
          rv$raw_data,
          required_columns = c(input$id_column, input$group_columns),
          id_column = input$id_column,
          group_columns = input$group_columns
        )
        
        if (!validation_results$valid) {
          # Display validation errors
          error_message <- paste(validation_results$messages, collapse = "\n")
          shiny::showNotification(error_message, type = "error", duration = 10)
          return()
        }
        
        # Store column info
        rv$column_info <- list(
          id_column = input$id_column,
          group_columns = input$group_columns,
          behavior_columns = input$behavior_columns,
          region_columns = potential_region_cols
        )
        
        # Prepare data for analysis
        processed_data <- rv$raw_data
        
        # Rename ID column to "Subject" for consistency
        names(processed_data)[names(processed_data) == input$id_column] <- "Subject"
        
        # Create combined group column if requested
        if (input$combine_groups && length(input$group_columns) > 1) {
          # Ensure all group columns exist and have valid data
          valid_group_cols <- input$group_columns[input$group_columns %in% names(processed_data)]
          if (length(valid_group_cols) > 0) {
            processed_data$Group <- apply(processed_data[, valid_group_cols, drop = FALSE], 1, 
                                          function(x) paste(x, collapse = "-"))
          } else {
            # Fallback: use first available group column
            processed_data$Group <- processed_data[[input$group_columns[1]]]
          }
        } else if (input$combine_groups && length(input$group_columns) == 1) {
          processed_data$Group <- processed_data[[input$group_columns[1]]]
        } else {
          # If not combining groups, create Group column from first group column
          processed_data$Group <- processed_data[[input$group_columns[1]]]
        }
        
        # Validate Group column was created successfully
        if (!"Group" %in% names(processed_data) || all(is.na(processed_data$Group))) {
          stop("Failed to create Group column. Please check your group column selections.")
        }
        
        # Ensure Group column has valid values
        processed_data$Group <- as.character(processed_data$Group)
        processed_data$Group[is.na(processed_data$Group)] <- "Unknown"
        
        # Store processed data
        rv$processed_data <- processed_data
        
        # Check for missing data
        missing_counts <- sapply(processed_data[, potential_region_cols], function(x) sum(is.na(x)))
        total_missing <- sum(missing_counts)
        rv$has_missing_data <- total_missing > 0
        
        # Run comprehensive data quality analysis
        shiny::showNotification(
          "Running comprehensive data quality assessment...", 
          type = "message", 
          duration = 3
        )
        
        rv$data_quality_metrics <- analyze_data_quality(
          processed_data, 
          potential_region_cols,
          input$group_columns
        )
        
        rv$quality_assessment_complete <- TRUE
        
        # Mark configuration as complete
        rv$validation_complete <- TRUE
        
        # Show notification of successful configuration
        shiny::showNotification(
          "Data configured successfully with comprehensive quality assessment!",
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        # Catch any errors and display them
        shiny::showNotification(
          paste("Error in data configuration:", e$message), 
          type = "error",
          duration = 15
        )
      })
    })
    
    # Flag to indicate configuration is complete
    output$config_complete <- shiny::reactive({
      return(rv$validation_complete)
    })
    shiny::outputOptions(output, "config_complete", suspendWhenHidden = FALSE)
    
    # Enhanced quality display outputs
    output$sample_size_display <- shiny::renderText({
      req(rv$data_quality_metrics)
      
      # Check if we have group-specific sizes
      if (!is.null(rv$data_quality_metrics$min_group_size)) {
        paste0(rv$data_quality_metrics$min_group_size, " min/group")
      } else {
        as.character(rv$data_quality_metrics$sample_size)
      }
    })
    
    output$regions_count_display <- shiny::renderText({
      req(rv$column_info)
      as.character(length(rv$column_info$region_columns))
    })
    
    output$groups_count_display <- shiny::renderText({
      req(rv$processed_data)
      as.character(length(unique(rv$processed_data$Group)))
    })
    
    output$quality_score_display <- shiny::renderText({
      req(rv$data_quality_metrics)
      
      # Calculate overall quality score (0-100)
      metrics <- rv$data_quality_metrics
      
      score <- 100
      
      # Penalize for small sample size
      if (metrics$sample_size < 10) score <- score - 30
      else if (metrics$sample_size < 20) score <- score - 15
      
      # Penalize for missing data
      if (metrics$missing_percent > 10) score <- score - 20
      else if (metrics$missing_percent > 5) score <- score - 10
      
      # Penalize for high outlier burden
      outlier_percent <- (metrics$outliers_count / (metrics$sample_size * length(rv$column_info$region_columns))) * 100
      if (outlier_percent > 10) score <- score - 15
      else if (outlier_percent > 5) score <- score - 8
      
      # Penalize for highly non-normal data
      if (length(metrics$normality_tests) > 0) {
        non_normal_count <- sum(sapply(metrics$normality_tests, function(x) !x$consensus_normal))
        non_normal_percent <- non_normal_count / length(metrics$normality_tests) * 100
        if (non_normal_percent > 70) score <- score - 15
        else if (non_normal_percent > 40) score <- score - 8
      }
      
      # Ensure score is between 0 and 100
      score <- max(0, min(100, score))
      
      # Return with appropriate rating
      if (score >= 80) paste0(score, " (Excellent)")
      else if (score >= 65) paste0(score, " (Good)")
      else if (score >= 50) paste0(score, " (Fair)")
      else paste0(score, " (Poor)")
    })
    
    # Overall quality assessment UI
    output$overall_quality_ui <- shiny::renderUI({
      req(rv$data_quality_metrics)
      
      metrics <- rv$data_quality_metrics
      
      # Create quality indicators
      quality_items <- list()
      
      # Sample size assessment
      if (metrics$sample_size >= 30) {
        quality_items[["sample_size"]] <- list(
          icon = "check-circle",
          color = "success",
          title = "Sample Size",
          text = paste("Large sample size (n =", metrics$sample_size, ") supports robust analysis")
        )
      } else if (metrics$sample_size >= 10) {
        quality_items[["sample_size"]] <- list(
          icon = "exclamation-triangle",
          color = "warning",
          title = "Sample Size",
          text = paste("Moderate sample size (n =", metrics$sample_size, ") - consider conservative methods")
        )
      } else {
        quality_items[["sample_size"]] <- list(
          icon = "times-circle",
          color = "danger",
          title = "Sample Size",
          text = paste("Small sample size (n =", metrics$sample_size, ") - use very conservative methods")
        )
      }
      
      # Missing data assessment
      if (metrics$missing_percent < 1) {
        quality_items[["missing"]] <- list(
          icon = "check-circle",
          color = "success",
          title = "Missing Data",
          text = paste("Minimal missing data (", round(metrics$missing_percent, 1), "%)")
        )
      } else if (metrics$missing_percent < 5) {
        quality_items[["missing"]] <- list(
          icon = "info-circle",
          color = "info",
          title = "Missing Data",
          text = paste("Low missing data (", round(metrics$missing_percent, 1), "%)")
        )
      } else {
        quality_items[["missing"]] <- list(
          icon = "exclamation-triangle",
          color = "warning",
          title = "Missing Data",
          text = paste("Substantial missing data (", round(metrics$missing_percent, 1), "%) - consider imputation")
        )
      }
      
      # Create UI elements
      ui_elements <- lapply(quality_items, function(item) {
        shiny::div(
          class = paste0("alert alert-", item$color),
          style = "margin-bottom: 10px;",
          shiny::icon(item$icon),
          shiny::strong(paste0(item$title, ": ")),
          item$text
        )
      })
      
      # Add recommendations summary
      if (!is.null(metrics$recommendations) && length(metrics$recommendations) > 0) {
        recommendations_text <- shiny::div(
          class = "alert alert-info",
          shiny::h5(shiny::icon("lightbulb"), "Key Recommendations:"),
          shiny::tags$ul(
            lapply(names(metrics$recommendations), function(rec_name) {
              rec <- metrics$recommendations[[rec_name]]
              shiny::tags$li(shiny::strong(rec_name, ": "), rec)
            })
          )
        )
        ui_elements <- c(ui_elements, list(recommendations_text))
      }
      
      do.call(shiny::tagList, ui_elements)
    })
    
    # Distribution analysis UI
    output$distribution_analysis_ui <- shiny::renderUI({
      req(rv$data_quality_metrics$normality_tests)
      
      normality_tests <- rv$data_quality_metrics$normality_tests
      
      if (length(normality_tests) == 0) {
        return(shiny::p("No normality tests available."))
      }
      
      # Create summary table
      normality_summary <- data.frame(
        Region = names(normality_tests),
        Normal = sapply(normality_tests, function(x) x$consensus_normal),
        Confidence = sapply(normality_tests, function(x) round(x$normality_confidence, 2)),
        Skewness = sapply(normality_tests, function(x) round(x$moments$skewness, 2)),
        Kurtosis = sapply(normality_tests, function(x) round(x$moments$kurtosis, 2)),
        stringsAsFactors = FALSE
      )
      
      # Count normal vs non-normal
      normal_count <- sum(normality_summary$Normal)
      total_count <- nrow(normality_summary)
      normal_percent <- round((normal_count / total_count) * 100, 1)
      
      shiny::tagList(
        shiny::div(
          class = if (normal_percent >= 70) "alert alert-success" else if (normal_percent >= 50) "alert alert-warning" else "alert alert-danger",
          shiny::h5("Distribution Summary"),
          shiny::p(paste0(normal_count, " out of ", total_count, " regions (", normal_percent, "%) show normal distributions"))
        ),
        
        DT::DTOutput(session$ns("normality_table"))
      )
    })
    
    # Render normality table
    output$normality_table <- DT::renderDT({
      req(rv$data_quality_metrics$normality_tests)
      
      normality_tests <- rv$data_quality_metrics$normality_tests
      
      normality_summary <- data.frame(
        Region = names(normality_tests),
        Normal = sapply(normality_tests, function(x) ifelse(x$consensus_normal, "✓", "✗")),
        Confidence = sapply(normality_tests, function(x) round(x$normality_confidence, 2)),
        Skewness = sapply(normality_tests, function(x) round(x$moments$skewness, 2)),
        Kurtosis = sapply(normality_tests, function(x) round(x$moments$kurtosis, 2)),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        normality_summary,
        options = list(
          pageLength = 15,
          scrollY = "400px",
          paging = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Normal",
          backgroundColor = DT::styleEqual(c("✓", "✗"), c("#d4edda", "#f8d7da"))
        )
    })
    
    # Outlier analysis UI
    output$outlier_analysis_ui <- shiny::renderUI({
      req(rv$data_quality_metrics$outlier_details)
      
      outlier_details <- rv$data_quality_metrics$outlier_details
      
      # Create outlier summary
      outlier_summary <- data.frame(
        Region = names(outlier_details),
        IQR_Outliers = sapply(outlier_details, function(x) x$iqr_outliers),
        Z_Score_Outliers = sapply(outlier_details, function(x) x$z_outliers),
        MAD_Outliers = sapply(outlier_details, function(x) x$mad_outliers),
        Total_Outliers = sapply(outlier_details, function(x) x$total_unique_outliers),
        Percentage = sapply(outlier_details, function(x) round(x$outlier_percentage, 1)),
        stringsAsFactors = FALSE
      )
      
      total_outliers <- sum(outlier_summary$Total_Outliers)
      total_points <- rv$data_quality_metrics$sample_size * length(rv$column_info$region_columns)
      outlier_percent <- round((total_outliers / total_points) * 100, 1)
      
      shiny::tagList(
        shiny::div(
          class = if (outlier_percent < 2) "alert alert-success" else if (outlier_percent < 5) "alert alert-warning" else "alert alert-danger",
          shiny::h5("Outlier Summary"),
          shiny::p(paste0("Total outliers detected: ", total_outliers, " (", outlier_percent, "% of all data points)"))
        ),
        
        DT::DTOutput(session$ns("outlier_table"))
      )
    })
    
    # Render outlier table
    output$outlier_table <- DT::renderDT({
      req(rv$data_quality_metrics$outlier_details)
      
      outlier_details <- rv$data_quality_metrics$outlier_details
      
      outlier_summary <- data.frame(
        Region = names(outlier_details),
        IQR_Outliers = sapply(outlier_details, function(x) x$iqr_outliers),
        Z_Score_Outliers = sapply(outlier_details, function(x) x$z_outliers),
        MAD_Outliers = sapply(outlier_details, function(x) x$mad_outliers),
        Total_Outliers = sapply(outlier_details, function(x) x$total_unique_outliers),
        Percentage = sapply(outlier_details, function(x) paste0(round(x$outlier_percentage, 1), "%")),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        outlier_summary,
        options = list(
          pageLength = 15,
          scrollY = "400px",
          paging = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Percentage",
          backgroundColor = DT::styleInterval(c(2, 5, 10), c("#d4edda", "#fff3cd", "#f8d7da", "#f5c6cb"))
        )
    })
    
    # Statistical power analysis UI
    output$power_analysis_ui <- shiny::renderUI({
      req(rv$data_quality_metrics$statistical_power)
      
      power_analysis <- rv$data_quality_metrics$statistical_power
      
      # Create power summary
      power_data <- data.frame(
        Effect_Size = c("Small (r = 0.3)", "Medium (r = 0.5)", "Large (r = 0.7)"),
        Power = c(
          round(power_analysis$r_0.3, 3),
          round(power_analysis$r_0.5, 3),
          round(power_analysis$r_0.7, 3)
        ),
        Adequate = c(
          power_analysis$r_0.3 >= 0.8,
          power_analysis$r_0.5 >= 0.8,
          power_analysis$r_0.7 >= 0.8
        ),
        stringsAsFactors = FALSE
      )
      
      shiny::tagList(
        shiny::div(
          class = "alert alert-info",
          shiny::h5("Statistical Power Analysis"),
          shiny::p("Power to detect correlations of different effect sizes at α = 0.05 (two-tailed)"),
          shiny::p(shiny::em("Power ≥ 0.80 is generally considered adequate"))
        ),
        
        DT::DTOutput(session$ns("power_table"))
      )
    })
    
    # Render power table
    output$power_table <- DT::renderDT({
      req(rv$data_quality_metrics$statistical_power)
      
      power_analysis <- rv$data_quality_metrics$statistical_power
      
      power_data <- data.frame(
        Effect_Size = c("Small (r = 0.3)", "Medium (r = 0.5)", "Large (r = 0.7)"),
        Power = c(
          round(power_analysis$r_0.3, 3),
          round(power_analysis$r_0.5, 3),
          round(power_analysis$r_0.7, 3)
        ),
        Adequate = c(
          ifelse(power_analysis$r_0.3 >= 0.8, "✓", "✗"),
          ifelse(power_analysis$r_0.5 >= 0.8, "✓", "✗"),
          ifelse(power_analysis$r_0.7 >= 0.8, "✓", "✗")
        ),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        power_data,
        options = list(
          pageLength = 5,
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Adequate",
          backgroundColor = DT::styleEqual(c("✓", "✗"), c("#d4edda", "#f8d7da"))
        ) %>%
        DT::formatStyle(
          "Power",
          backgroundColor = DT::styleInterval(c(0.6, 0.8), c("#f8d7da", "#fff3cd", "#d4edda"))
        )
    })
    
    # Group balance analysis UI
    output$group_balance_ui <- shiny::renderUI({
      req(rv$data_quality_metrics$group_balance)
      
      group_balance <- rv$data_quality_metrics$group_balance
      
      ui_elements <- list()
      
      for (group_name in names(group_balance)) {
        balance_info <- group_balance[[group_name]]
        
        # Create balance summary
        balance_summary <- data.frame(
          Group = names(balance_info$counts),
          Count = as.numeric(balance_info$counts),
          stringsAsFactors = FALSE
        )
        
        # Assess balance quality
        balance_quality <- if (balance_info$balance_ratio >= 0.8) "Excellent" 
        else if (balance_info$balance_ratio >= 0.6) "Good"
        else if (balance_info$balance_ratio >= 0.4) "Fair"
        else "Poor"
        
        balance_color <- if (balance_info$balance_ratio >= 0.8) "success"
        else if (balance_info$balance_ratio >= 0.6) "info"
        else if (balance_info$balance_ratio >= 0.4) "warning"
        else "danger"
        
        ui_elements[[group_name]] <- shiny::div(
          shiny::h5(paste("Group Variable:", group_name)),
          shiny::div(
            class = paste0("alert alert-", balance_color),
            shiny::p(paste0("Balance Quality: ", balance_quality, " (Ratio: ", round(balance_info$balance_ratio, 2), ")")),
            shiny::p(paste0("Size range: ", balance_info$min_size, " - ", balance_info$max_size, " subjects per group"))
          ),
          DT::DTOutput(session$ns(paste0("balance_table_", gsub("[^a-zA-Z0-9]", "_", group_name)))),
          shiny::br()
        )
        
        # Create the table output
        output[[paste0("balance_table_", gsub("[^a-zA-Z0-9]", "_", group_name))]] <- DT::renderDT({
          DT::datatable(
            balance_summary,
            options = list(
              pageLength = 10,
              paging = FALSE,
              searching = FALSE
            ),
            rownames = FALSE
          )
        })
      }
      
      do.call(shiny::tagList, ui_elements)
    })
    
    # Display data summary
    output$data_summary <- shiny::renderText({
      req(rv$validation_complete)
      
      # Generate summary text
      summary_text <- paste(
        "Data Configuration Summary:",
        "----------------------------",
        paste("Total observations:", nrow(rv$processed_data)),
        paste("Subject ID Column:", rv$column_info$id_column),
        paste("Group Columns:", paste(rv$column_info$group_columns, collapse = ", ")),
        if (length(rv$column_info$behavior_columns) > 0) 
          paste("Behavior Columns:", paste(rv$column_info$behavior_columns, collapse = ", "))
        else "No behavior columns selected",
        paste("Brain Region Columns:", paste(rv$column_info$region_columns, collapse = ", ")),
        paste("Number of unique groups:", length(unique(rv$processed_data$Group))),
        paste("Groups:", paste(unique(rv$processed_data$Group), collapse = ", ")),
        "",
        paste("Missing Data:", if(rv$has_missing_data) "Yes (recommendations will address this)" else "None detected"),
        "",
        "Comprehensive quality assessment completed. Smart recommendations will be available in the next step.",
        sep = "\n"
      )
      
      return(summary_text)
    })
    
    # Handle proceed button
    shiny::observeEvent(input$proceed_button, {
      if (exists("parent_session") && !is.null(parent_session)) {
        shinydashboard::updateTabItems(parent_session, "tabs", "preferences")
      }
    })
    
    # Return reactive values for use in other modules
    return(shiny::reactive({
      list(
        raw_data = rv$raw_data,
        processed_data = rv$processed_data,
        column_info = rv$column_info,
        data_configured = rv$validation_complete,
        has_missing_data = rv$has_missing_data,
        data_quality_metrics = rv$data_quality_metrics,
        quality_assessment_complete = rv$quality_assessment_complete
      )
    }))
  })
}