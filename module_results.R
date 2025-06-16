#########################################################
# Enhanced Brain Network Analysis Shiny App
# module_results.R - Enhanced results display module with balanced functionality
#########################################################

#' Enhanced Results UI with comprehensive visualization options
#' 
#' @param id Module id
#' @return Shiny UI
resultsUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Include modest CSS enhancements
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
          /* Enhanced help panel styling */
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
          
          .help-content ul {
            padding-left: 20px;
            margin-bottom: 5px;
          }
          
          .method-recommendation {
            margin-top: 5px;
            font-weight: bold;
            color: #155724;
          }
          
          .plot-info {
            margin-top: 5px;
            margin-bottom: 10px;
            font-size: 0.85em;
            color: #6c757d;
            font-style: italic;
            background-color: #f8f9fa;
            padding: 8px;
            border-radius: 4px;
            border-left: 3px solid #007bff;
          }
          
          /* Analysis progress indicator */
          .analysis-progress {
            background-color: #e8f5e9;
            border-left: 4px solid #4caf50;
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 20px;
          }
          
          .analysis-progress.running {
            background-color: #fff3e0;
            border-left-color: #ff9800;
          }
          
          .analysis-progress.error {
            background-color: #ffebee;
            border-left-color: #f44336;
          }
          
          /* Loading spinner for analysis */
          .analysis-spinner {
            display: inline-block;
            width: 20px;
            height: 20px;
            border: 3px solid #f3f3f3;
            border-top: 3px solid #007bff;
            border-radius: 50%;
            animation: spin 1s linear infinite;
            margin-right: 10px;
          }
          
          @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
          }
        ")
      ),
      # Simple help panel toggle JavaScript
      shiny::tags$script(
        shiny::HTML("
          $(document).on('click', '.help-title', function() {
            $(this).parent().toggleClass('active');
          });
        ")
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3("Results"),
        shiny::p("Explore your network analysis results through interactive visualizations and download comprehensive reports.")
      )
    ),
    
    # Analysis status indicator
    shiny::uiOutput(ns("analysis_status")),
    
    shiny::fluidRow(
      # Main content area
      shiny::column(
        width = 9,
        shiny::wellPanel(
          # Enhanced help panel for visualizations
          shiny::div(
            class = "help-panel",
            shiny::div(
              class = "help-title",
              shiny::icon("question-circle", class = "help-icon"),
              "About Enhanced Visualizations",
              shiny::icon("chevron-down")
            ),
            shiny::div(
              class = "help-content",
              shiny::HTML("
                <p>This enhanced analysis suite provides comprehensive views of your brain network data:</p>
                <ul>
                  <li><strong>Network Graph:</strong> Interactive visualization showing brain regions as nodes and functional connections as edges. Enhanced with multiple layout algorithms and node sizing options.</li>
                  <li><strong>Correlation Heatmap:</strong> Matrix visualization of all pairwise correlations with hierarchical clustering and significance marking.</li>
                  <li><strong>Global Metrics:</strong> Network-wide properties comparison between groups, revealing overall connectivity patterns.</li>
                  <li><strong>Node Metrics:</strong> Region-specific centrality measures identifying network hubs and influential areas.</li>
                  <li><strong>Group Comparison:</strong> Statistical comparison between experimental conditions with effect size visualization.</li>
                  <li><strong>Quality Report:</strong> Comprehensive assessment of your data quality and analysis reliability.</li>
                </ul>
                <p class='method-recommendation'>Use multiple visualization types to gain complementary insights into your network structure. Each view reveals different aspects of brain connectivity organization.</p>
              ")
            )
          ),
          
          shiny::h4("Visualization"),
          
          # Current visualization info with enhanced styling
          shiny::uiOutput(ns("current_viz_info")),
          
          # Add the visualization selection dropdown
          shiny::selectInput(
            ns("display_type"),
            "Select Visualization Type:",
            choices = c(
              "Network Graph" = "network",
              "Correlation Heatmap" = "heatmap",
              "Global Metrics" = "global",
              "Node Metrics" = "node",
              "Group Comparison" = "group_comparison",
              "Quality Report" = "quality_report"
            ),
            selected = "network"
          ),
          
          # Add group selection
          shiny::selectInput(
            ns("selected_group"),
            "Select Group:",
            choices = NULL
          ),
          
          # Placeholder for when analysis isn't ready
          shiny::conditionalPanel(
            condition = paste0("!output['", ns("analysis_complete"), "']"),
            shiny::div(
              class = "alert alert-info",
              shiny::icon("info-circle"),
              "Complete the previous steps to view your analysis results here."
            )
          ),
          
          # Visualization output
          shiny::conditionalPanel(
            condition = paste0("output['", ns("analysis_complete"), "']"),
            shiny::plotOutput(
              ns("main_plot"),
              width = "100%",
              height = "700px"
            )
          ),
          
          # Export options
          shiny::div(
            style = "margin-top: 20px;",
            shiny::downloadButton(
              ns("download_current_plot"),
              "Download Plot",
              class = "btn-primary",
              icon = shiny::icon("download")
            )
          )
        )
      ),
      
      # Control panel
      shiny::column(
        width = 3,
        # Display options specific to current visualization
        shiny::wellPanel(
          shiny::h4("Display Options"),
          
          # Network-specific controls
          shiny::conditionalPanel(
            condition = paste0("input['", ns("display_type"), "'] == 'network'"),
            
            shiny::selectInput(
              ns("network_layout"),
              "Layout Algorithm:",
              choices = c(
                "Force-Directed (FR)" = "fr",
                "Force-Directed (KK)" = "kk", 
                "Force-Directed (LGL)" = "lgl",
                "Circle" = "circle",
                "Circle (by Brain Area)" = "circle_area",
                "Grid" = "grid"
              ),
              selected = "fr"
            ),
            
            shiny::selectInput(
              ns("network_color_by"),
              "Color Nodes By:",
              choices = c(
                "Brain Area" = "brain_area",
                "Community" = "community",
                "Metric Value" = "metric"
              ),
              selected = "brain_area"
            ),
            
            shiny::selectInput(
              ns("network_size_by"),
              "Size Nodes By:",
              choices = c("Uniform Size" = "uniform"),
              selected = "uniform"
            ),
            
            shiny::conditionalPanel(
              condition = paste0("input['", ns("network_size_by"), "'] == 'uniform'"),
              shiny::sliderInput(
                ns("node_size"),
                "Node Size:",
                min = 5,
                max = 25,
                value = 15,
                step = 1
              )
            ),
            
            shiny::checkboxInput(
              ns("show_node_labels"),
              "Show Node Labels",
              value = TRUE
            ),
            
            shiny::checkboxInput(
              ns("show_significant_only"),
              "Show Only Significant Connections",
              value = FALSE
            )
          ),
          
          # Heatmap-specific controls
          shiny::conditionalPanel(
            condition = paste0("input['", ns("display_type"), "'] == 'heatmap'"),
            
            shiny::checkboxInput(
              ns("cluster_heatmap"),
              "Apply Hierarchical Clustering",
              value = TRUE
            ),
            
            shiny::checkboxInput(
              ns("show_significance"),
              "Mark Significant Correlations",
              value = TRUE
            )
          ),
          
          # Global metrics controls
          shiny::conditionalPanel(
            condition = paste0("input['", ns("display_type"), "'] == 'global'"),
            
            shiny::selectInput(
              ns("global_metric"),
              "Select Metric:",
              choices = c(
                "Network Density" = "Density",
                "Global Clustering Coefficient" = "Global_Clustering_Coefficient",
                "Average Path Length" = "Average_Path_Length",
                "Modularity" = "Modularity"
              ),
              selected = "Density"
            ),
            
            shiny::selectInput(
              ns("global_plot_type"),
              "Plot Type:",
              choices = c(
                "Bar Chart" = "bar",
                "Point Plot" = "point"
              ),
              selected = "bar"
            )
          ),
          
          # Node metrics controls
          shiny::conditionalPanel(
            condition = paste0("input['", ns("display_type"), "'] == 'node'"),
            
            shiny::selectInput(
              ns("node_metric"),
              "Select Metric:",
              choices = NULL
            ),
            
            shiny::checkboxInput(
              ns("node_color_by_area"),
              "Color by Brain Area",
              value = TRUE
            )
          ),
          
          # Group comparison controls
          shiny::conditionalPanel(
            condition = paste0("input['", ns("display_type"), "'] == 'group_comparison'"),
            
            shiny::selectInput(
              ns("comparison_metric"),
              "Comparison Type:",
              choices = c(
                "Edge-wise Difference" = "edge",
                "Node Strength Difference" = "node",
                "Global Network Metrics" = "global"
              ),
              selected = "edge"
            ),
            
            shiny::selectInput(
              ns("reference_group"),
              "Reference Group:",
              choices = NULL
            ),
            
            shiny::selectInput(
              ns("comparison_group"),
              "Comparison Group:",
              choices = NULL
            ),
            
            shiny::conditionalPanel(
              condition = paste0("input['", ns("comparison_metric"), "'] == 'edge'"),
              shiny::checkboxInput(
                ns("show_significant_diffs"),
                "Show Only Significant Differences",
                value = FALSE
              )
            )
          ),
          
          # Quality report controls
          shiny::conditionalPanel(
            condition = paste0("input['", ns("display_type"), "'] == 'quality_report'"),
            
            shiny::selectInput(
              ns("quality_focus"),
              "Report Focus:",
              choices = c(
                "Overall Summary" = "summary",
                "Data Quality Details" = "data_quality",
                "Recommendations" = "recommendations"
              ),
              selected = "summary"
            )
          )
        ),
        
        # Download options
        shiny::wellPanel(
          shiny::h4("Export Options"),
          
          shiny::downloadButton(
            ns("download_all_results"),
            "Download All Results",
            class = "btn-success btn-block",
            style = "margin-top: 10px;"
          ),
          
          shiny::div(
            style = "margin-top: 10px; font-size: 0.85em; color: #6c757d;",
            "Complete package with all visualizations, data, and documentation"
          )
        )
      )
    )
  )
}

#' Enhanced Results Server with advanced analysis capabilities
#' 
#' @param id Module id
#' @param data_import_results Reactive containing data import results
#' @param preferences_results Reactive containing analysis preferences
#' @return None
results <- function(id, data_import_results, preferences_results) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create reactive values for this module
    rv <- shiny::reactiveValues(
      analysis_complete = FALSE,
      current_plot = NULL,
      networks = NULL,
      global_metrics = NULL,
      node_metrics = NULL,
      significance_matrices = NULL,
      distance_matrix = NULL,
      analysis_start_time = NULL,
      analysis_summary = NULL
    )
    
    # Enhanced analysis status indicator
    output$analysis_status <- shiny::renderUI({
      if (is.null(preferences_results()) || !preferences_results()$configuration_saved) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            shiny::strong("Configuration Required: "),
            "Please complete the Analysis Preferences step before viewing results."
          )
        )
      } else if (!rv$analysis_complete) {
        return(
          shiny::div(
            class = "analysis-progress running",
            shiny::HTML('<div class="analysis-spinner"></div>'),
            shiny::strong("Running Network Analysis..."),
            shiny::p("Processing your data with the configured parameters. This may take a few moments.", 
                     style = "margin: 5px 0 0 0;")
          )
        )
      } else {
        return(
          shiny::div(
            class = "analysis-progress",
            shiny::icon("check-circle"),
            shiny::strong("Analysis Complete!"),
            shiny::p(paste("Network analysis completed successfully.", 
                           if (!is.null(rv$analysis_start_time)) 
                             paste("Processing time:", round(difftime(Sys.time(), rv$analysis_start_time, units = "secs"), 1), "seconds")
                           else ""), 
                     style = "margin: 5px 0 0 0;")
          )
        )
      }
    })
    
    # Current visualization info with enhanced content
    output$current_viz_info <- shiny::renderUI({
      # Determine which visualization is selected
      viz_type <- input$display_type
      
      if (viz_type == "network") {
        group <- if (!is.null(input$selected_group)) input$selected_group else ""
        layout <- if (!is.null(input$network_layout)) input$network_layout else "fr"
        return(
          shiny::div(
            class = "plot-info",
            shiny::HTML(paste0(
              "<strong>Network Graph:</strong> ", group, " (", gsub("_", " ", layout), " layout)<br/>",
              "Nodes represent brain regions and edges represent functional connections above the correlation threshold. ",
              "Edge thickness indicates connection strength. Red edges = positive correlations, blue = negative correlations."
            ))
          )
        )
      } else if (viz_type == "heatmap") {
        group <- if (!is.null(input$selected_group)) input$selected_group else ""
        clustering <- if (!is.null(input$cluster_heatmap) && input$cluster_heatmap) "with hierarchical clustering" else "without clustering"
        return(
          shiny::div(
            class = "plot-info",
            shiny::HTML(paste0(
              "<strong>Correlation Heatmap:</strong> ", group, " ", clustering, "<br/>",
              "Color intensity represents correlation strength between brain region pairs. ",
              "Red = positive correlations, blue = negative correlations. ",
              "Asterisks (*) mark statistically significant correlations."
            ))
          )
        )
      } else if (viz_type == "global") {
        metric <- if (!is.null(input$global_metric)) gsub("_", " ", input$global_metric) else ""
        return(
          shiny::div(
            class = "plot-info",
            shiny::HTML(paste0(
              "<strong>Global Network Metric:</strong> ", metric, "<br/>",
              "This metric describes network-wide properties compared across all groups. ",
              "Higher values typically indicate more integrated or efficient networks, depending on the specific metric."
            ))
          )
        )
      } else if (viz_type == "node") {
        metric <- if (!is.null(input$node_metric)) gsub("_", " ", input$node_metric) else ""
        return(
          shiny::div(
            class = "plot-info",
            shiny::HTML(paste0(
              "<strong>Node Metric:</strong> ", metric, "<br/>",
              "This metric quantifies the importance or role of individual brain regions in the network. ",
              "Higher values typically indicate more central or influential regions."
            ))
          )
        )
      } else if (viz_type == "group_comparison") {
        ref_group <- if (!is.null(input$reference_group)) input$reference_group else ""
        comp_group <- if (!is.null(input$comparison_group)) input$comparison_group else ""
        metric <- if (!is.null(input$comparison_metric)) {
          if (input$comparison_metric == "edge") "Edge-wise Difference"
          else if (input$comparison_metric == "node") "Node Strength Difference"
          else "Global Network Metrics"
        } else ""
        
        return(
          shiny::div(
            class = "plot-info",
            shiny::HTML(paste0(
              "<strong>", metric, ":</strong> ", comp_group, " vs. ", ref_group, "<br/>",
              "This visualization shows differences between the two groups. ",
              "Red indicates stronger connectivity in the comparison group, blue indicates weaker connectivity."
            ))
          )
        )
      } else if (viz_type == "quality_report") {
        focus <- if (!is.null(input$quality_focus)) input$quality_focus else "summary"
        return(
          shiny::div(
            class = "plot-info",
            shiny::HTML(paste0(
              "<strong>Quality Report:</strong> ", gsub("_", " ", focus), "<br/>",
              "Comprehensive assessment of your data quality and analysis reliability. ",
              "This report helps validate your results and provides recommendations for interpretation."
            ))
          )
        )
      } else {
        return(NULL)
      }
    })
    
    # Run analysis when preferences are saved
    shiny::observe({
      # Only run if configuration is actually saved and we haven't already analyzed
      req(preferences_results(), preferences_results()$configuration_saved)
      
      # Check if analysis was already completed to prevent re-running
      if (rv$analysis_complete) {
        return()
      }
      
      # Record analysis start time
      rv$analysis_start_time <- Sys.time()
      
      # Show progress notification
      progress_id <- shiny::showNotification(
        "Running network analysis...",
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      
      on.exit({
        shiny::removeNotification(progress_id)
      })
      
      # Get the data and preferences
      prefs <- shiny::isolate(preferences_results())
      data_info <- shiny::isolate(data_import_results())
      
      # Get analysis data
      analysis_data <- prefs$analysis_data
      
      # Filter for selected groups
      if ("Group" %in% names(analysis_data)) {
        analysis_data <- analysis_data[analysis_data$Group %in% prefs$selected_groups, ]
      }
      
      # Create distance matrix for the selected regions (simplified placeholder)
      num_regions <- length(prefs$selected_regions)
      set.seed(123)  # For reproducibility
      rv$distance_matrix <- matrix(runif(num_regions * num_regions, 1, 20), nrow = num_regions)
      rownames(rv$distance_matrix) <- prefs$selected_regions
      colnames(rv$distance_matrix) <- prefs$selected_regions
      rv$distance_matrix[lower.tri(rv$distance_matrix)] <- t(rv$distance_matrix)[lower.tri(rv$distance_matrix)]
      diag(rv$distance_matrix) <- 0
      
      # Run enhanced network analysis
      tryCatch({
        # Perform the actual analysis
        # This would call to the utility functions in utils.R
        network_results <- run_network_analysis_by_group(
          data = analysis_data,
          region_columns = prefs$selected_regions,
          group_column = "Group",
          correlation_threshold = prefs$correlation_threshold,
          correlation_method = prefs$correlation_method,
          correlation_type = prefs$correlation_type,
          use_robust = prefs$use_robust,
          use_regularization = prefs$use_regularization,
          perform_significance = prefs$perform_significance,
          apply_fdr = prefs$apply_fdr
        )
        
        # Store results
        rv$networks <- network_results$networks
        rv$global_metrics <- network_results$global_metrics
        rv$node_metrics <- network_results$node_metrics
        rv$significance_matrices <- network_results$significance_matrices
        
        # Add brain area information to node metrics
        if (!is.null(prefs$brain_areas) && !is.null(rv$node_metrics) && nrow(rv$node_metrics) > 0) {
          # Create region to area mapping
          region_to_area <- data.frame(
            Node = character(),
            Brain_Area = character(),
            stringsAsFactors = FALSE
          )
          
          for (area in names(prefs$brain_areas)) {
            if (length(prefs$brain_areas[[area]]) > 0) {
              area_df <- data.frame(
                Node = prefs$brain_areas[[area]],
                Brain_Area = area,
                stringsAsFactors = FALSE
              )
              region_to_area <- rbind(region_to_area, area_df)
            }
          }
          
          # Merge with node metrics
          if (nrow(region_to_area) > 0) {
            rv$node_metrics <- merge(
              rv$node_metrics,
              region_to_area,
              by = "Node",
              all.x = TRUE
            )
          }
        }
        
        # Create basic analysis summary
        rv$analysis_summary <- list(
          completion_time = Sys.time(),
          processing_duration = round(difftime(Sys.time(), rv$analysis_start_time, units = "secs"), 1),
          groups_analyzed = names(rv$networks),
          total_connections = sum(sapply(rv$networks, function(net) if (!is.null(net$graph)) igraph::ecount(net$graph) else 0)),
          total_nodes = length(prefs$selected_regions)
        )
        
        rv$analysis_complete <- TRUE
        
        # Update group selection choices
        group_choices <- names(rv$networks)
        shiny::updateSelectInput(
          session,
          "selected_group",
          choices = group_choices,
          selected = if (length(group_choices) > 0) group_choices[1] else NULL
        )
        
        # Update comparison group dropdowns
        if (length(group_choices) > 0) {
          shiny::updateSelectInput(
            session,
            "reference_group",
            choices = group_choices,
            selected = group_choices[1]
          )
          
          if (length(group_choices) > 1) {
            shiny::updateSelectInput(
              session,
              "comparison_group",
              choices = group_choices,
              selected = group_choices[2]
            )
          } else {
            shiny::updateSelectInput(
              session,
              "comparison_group",
              choices = group_choices,
              selected = group_choices[1]
            )
          }
        }
        
        # Update node metric choices
        if (!is.null(rv$node_metrics)) {
          metric_cols <- c(
            "Degree_Centrality",
            "Betweenness_Centrality", 
            "Closeness_Centrality",
            "Eigenvector_Centrality",
            "Node_Clustering_Coefficient"
          )
          
          # Filter to only include metrics that exist in the data
          available_metrics <- metric_cols[metric_cols %in% names(rv$node_metrics)]
          
          if (length(available_metrics) > 0) {
            metric_labels <- gsub("_", " ", available_metrics)
            names(available_metrics) <- metric_labels
            
            shiny::updateSelectInput(
              session,
              "node_metric", 
              choices = available_metrics,
              selected = if (length(available_metrics) > 0) available_metrics[1] else NULL
            )
            
            # Also update network size by options
            size_choices <- c("Uniform Size" = "uniform")
            size_choices <- c(size_choices, available_metrics)
            
            shiny::updateSelectInput(
              session,
              "network_size_by",
              choices = size_choices
            )
          }
        }
        
        shiny::showNotification(
          "Network analysis completed successfully!",
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        shiny::showNotification(
          paste("Error in network analysis:", e$message),
          type = "error",
          duration = 10
        )
        rv$analysis_complete <- FALSE
      })
    })
    
    # Flag to indicate analysis is complete
    output$analysis_complete <- shiny::reactive({
      return(rv$analysis_complete)
    })
    shiny::outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
    
    # Update display options when visualization type changes
    observeEvent(input$display_type, {
      # Sync selected_group input with specific visualization inputs
      if (input$display_type == "network") {
        updateSelectInput(session, "selected_group", selected = input$selected_group)
      } else if (input$display_type == "heatmap") {
        updateSelectInput(session, "selected_group", selected = input$selected_group)
      } else if (input$display_type == "node") {
        updateSelectInput(session, "selected_group", selected = input$selected_group)
      }
    })
    
    # Make sure reference and comparison groups aren't the same
    observeEvent(input$reference_group, {
      req(input$reference_group, length(names(rv$networks)) > 1)
      
      # Don't allow same group to be selected for both reference and comparison
      if (input$reference_group == input$comparison_group) {
        other_groups <- setdiff(names(rv$networks), input$reference_group)
        if (length(other_groups) > 0) {
          shiny::updateSelectInput(
            session,
            "comparison_group",
            selected = other_groups[1]
          )
        }
      }
    })
    
    # Main plot output
    output$main_plot <- shiny::renderPlot({
      req(rv$analysis_complete)
      
      viz_type <- input$display_type
      
      # Generate appropriate plot based on selected visualization type
      plot_result <- switch(viz_type,
                            "network" = render_network_plot(),
                            "heatmap" = render_heatmap_plot(),
                            "global" = render_global_metrics_plot(),
                            "node" = render_node_metrics_plot(),
                            "group_comparison" = render_group_comparison_plot(),
                            "quality_report" = render_quality_report_plot(),
                            render_default_plot()
      )
      
      # Store current plot for download
      rv$current_plot <- recordPlot()
      
      return(plot_result)
    })
    
    # Network graph visualization
    render_network_plot <- function() {
      req(input$selected_group, rv$networks)
      
      # Get network for selected group
      network <- rv$networks[[input$selected_group]]
      
      if (is.null(network) || is.null(network$graph)) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "No network connections above threshold for this group") +
            ggplot2::theme_void() +
            ggplot2::labs(title = paste("Network Graph:", input$selected_group))
        )
      }
      
      # Get node metrics for this group
      group_metrics <- if (!is.null(rv$node_metrics)) {
        rv$node_metrics[rv$node_metrics$Group == input$selected_group, ]
      } else {
        NULL
      }
      
      # Determine sizing options
      size_by <- if (input$network_size_by == "uniform") NULL else input$network_size_by
      
      # Get significance matrix if needed
      significant_edges <- NULL
      if (input$show_significant_only && !is.null(rv$significance_matrices) && 
          input$selected_group %in% names(rv$significance_matrices)) {
        significant_edges <- rv$significance_matrices[[input$selected_group]]
      }
      
      # Create enhanced plot
      p <- plot_network(
        network$graph,
        node_metrics = group_metrics,
        color_by = input$network_color_by,
        size_by = size_by,
        area_colors = preferences_results()$area_colors,
        layout = input$network_layout,
        title = paste("Network Graph:", input$selected_group),
        uniform_node_size = input$node_size,
        show_edge_labels = FALSE,
        show_node_labels = input$show_node_labels,
        significant_edges = significant_edges,
        significance_threshold = preferences_results()$significance_threshold
      )
      
      return(p)
    }
    
    # Heatmap visualization
    render_heatmap_plot <- function() {
      req(input$selected_group, rv$networks)
      
      network <- rv$networks[[input$selected_group]]
      
      if (is.null(network) || is.null(network$correlation_matrix)) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "No correlation data available for this group") +
            ggplot2::theme_void() +
            ggplot2::labs(title = paste("Correlation Heatmap:", input$selected_group))
        )
      }
      
      correlation_matrix <- network$correlation_matrix
      
      # Apply clustering if requested
      if (input$cluster_heatmap) {
        # Hierarchical clustering
        dist_matrix <- stats::dist(1 - abs(correlation_matrix))
        hc <- stats::hclust(dist_matrix, method = "complete")
        correlation_matrix <- correlation_matrix[hc$order, hc$order]
      }
      
      # Get significance matrix if requested
      p_value_matrix <- NULL
      if (input$show_significance && !is.null(rv$significance_matrices) && 
          input$selected_group %in% names(rv$significance_matrices)) {
        p_value_matrix <- rv$significance_matrices[[input$selected_group]]
        
        # Reorder p-values if clustering applied
        if (input$cluster_heatmap) {
          p_value_matrix <- p_value_matrix[hc$order, hc$order]
        }
      }
      
      # Convert to long format for ggplot
      melted_corr <- reshape2::melt(correlation_matrix)
      
      # Add significance information if available
      if (!is.null(p_value_matrix)) {
        melted_p <- reshape2::melt(p_value_matrix)
        melted_corr$significant <- melted_p$value <= preferences_results()$significance_threshold
        melted_corr$p_value <- melted_p$value
      } else {
        melted_corr$significant <- TRUE  # All significant by default
        melted_corr$p_value <- 0.01
      }
      
      # Create enhanced heatmap
      p <- ggplot2::ggplot(melted_corr, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile(color = "white", size = 0.1) +
        ggplot2::scale_fill_gradient2(
          low = "#2166ac", 
          mid = "white", 
          high = "#b2182b", 
          midpoint = 0, 
          limits = c(-1, 1),
          name = "Correlation",
          breaks = c(-1, -0.5, 0, 0.5, 1),
          labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0")
        )
      
      # Add significance markers if requested
      if (input$show_significance && !is.null(p_value_matrix)) {
        # Add asterisks for significant correlations
        sig_data <- melted_corr[melted_corr$significant & melted_corr$Var1 != melted_corr$Var2, ]
        if (nrow(sig_data) > 0) {
          p <- p + ggplot2::geom_text(
            data = sig_data,
            ggplot2::aes(label = "*"),
            size = 4,
            color = "black",
            fontface = "bold"
          )
        }
      }
      
      p <- p + 
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = ggplot2::element_text(size = 9),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right",
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          title = paste("Correlation Heatmap:", input$selected_group),
          subtitle = if (input$cluster_heatmap) "Hierarchical clustering applied" else "Original order",
          x = "",
          y = ""
        ) +
        ggplot2::coord_fixed()
      
      return(p)
    }
    
    # Global metrics visualization
    render_global_metrics_plot <- function() {
      req(rv$global_metrics)
      
      if (nrow(rv$global_metrics) == 0) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "No global metrics data available") +
            ggplot2::theme_void() +
            ggplot2::labs(title = "Global Network Metrics")
        )
      }
      
      # Filter for selected metric
      metric_data <- rv$global_metrics[rv$global_metrics$Metric == input$global_metric, ]
      
      if (nrow(metric_data) == 0) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = paste("No data available for", input$global_metric)) +
            ggplot2::theme_void() +
            ggplot2::labs(title = input$global_metric)
        )
      }
      
      # Add colors for groups if available
      if (!is.null(preferences_results()$group_colors)) {
        metric_data$Color <- preferences_results()$group_colors[metric_data$Group]
      } else {
        metric_data$Color <- "#1F78B4"
      }
      
      # Create plot based on selected type
      if (input$global_plot_type == "bar") {
        p <- ggplot2::ggplot(metric_data, ggplot2::aes(x = Group, y = Value, fill = Color)) +
          ggplot2::geom_col(alpha = 0.8, color = "black", size = 0.3) +
          ggplot2::scale_fill_identity()
      } else if (input$global_plot_type == "point") {
        p <- ggplot2::ggplot(metric_data, ggplot2::aes(x = Group, y = Value, color = Color)) +
          ggplot2::geom_point(size = 6, alpha = 0.8) +
          ggplot2::scale_color_identity()
      }
      
      # Add value labels
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = round(Value, 3)),
        vjust = -0.5,
        color = "black",
        fontface = "bold"
      )
      
      p <- p +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = ggplot2::element_text(size = 12),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
          axis.title = ggplot2::element_text(size = 12, face = "bold")
        ) +
        ggplot2::labs(
          title = paste("Global Network Metric:", gsub("_", " ", input$global_metric)),
          subtitle = "Comparison across experimental groups",
          x = "Experimental Group",
          y = gsub("_", " ", input$global_metric)
        )
      
      return(p)
    }
    
    # Node metrics visualization
    render_node_metrics_plot <- function() {
      req(rv$node_metrics, input$node_metric)
      
      if (nrow(rv$node_metrics) == 0 || !input$node_metric %in% names(rv$node_metrics)) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "No node metrics data available") +
            ggplot2::theme_void() +
            ggplot2::labs(title = "Node Metrics")
        )
      }
      
      # Prepare data
      plot_data <- rv$node_metrics
      
      # Set colors
      if (input$node_color_by_area && "Brain_Area" %in% names(plot_data) && 
          !is.null(preferences_results()$area_colors)) {
        plot_data$Color <- preferences_results()$area_colors[plot_data$Brain_Area]
        color_by <- "Brain_Area"
      } else if (!is.null(preferences_results()$group_colors)) {
        plot_data$Color <- preferences_results()$group_colors[plot_data$Group]
        color_by <- "Group"
      } else {
        plot_data$Color <- "#1F78B4"
        color_by <- "None"
      }
      
      # Create plot for node metrics
      metric_col <- input$node_metric
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "Node", y = metric_col, fill = "Color")) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
        ggplot2::scale_fill_identity() +
        ggplot2::facet_wrap(~Group, scales = "free_x") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
          strip.text = ggplot2::element_text(size = 12, face = "bold")
        ) +
        ggplot2::labs(
          title = paste("Node Metric:", gsub("_", " ", metric_col)),
          subtitle = paste("Colored by", tolower(gsub("_", " ", color_by))),
          x = "Brain Region",
          y = gsub("_", " ", metric_col)
        )
      
      return(p)
    }
    
    # Group comparison visualization
    render_group_comparison_plot <- function() {
      req(input$reference_group, input$comparison_group, rv$networks)
      
      # Check if we have both groups
      if (!input$reference_group %in% names(rv$networks) || 
          !input$comparison_group %in% names(rv$networks)) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "Missing network data for one or both groups") +
            ggplot2::theme_void() +
            ggplot2::labs(title = "Group Comparison")
        )
      }
      
      # Get networks for comparison
      network1 <- rv$networks[[input$reference_group]]
      network2 <- rv$networks[[input$comparison_group]]
      
      # Make the comparison
      comparison_result <- compare_networks(
        network1, 
        network2, 
        comparison_type = input$comparison_metric,
        significance_threshold = preferences_results()$significance_threshold
      )
      
      if (is.null(comparison_result)) {
        return(
          ggplot2::ggplot() + 
            ggplot2::annotate("text", x = 0, y = 0, 
                              label = "Unable to compare networks") +
            ggplot2::theme_void() +
            ggplot2::labs(title = "Group Comparison")
        )
      }
      
      if (input$comparison_metric == "edge") {
        # Edge-wise difference matrix visualization
        if (input$show_significant_diffs) {
          diff_matrix <- comparison_result$sig_diff
          subtitle <- "Showing only statistically significant differences"
        } else {
          diff_matrix <- comparison_result$diff_matrix
          subtitle <- "Showing all differences"
        }
        
        # Melt for ggplot
        melted_diff <- reshape2::melt(diff_matrix)
        
        # Create enhanced plot
        p <- ggplot2::ggplot(melted_diff, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
          ggplot2::geom_tile(color = "white", size = 0.1) +
          ggplot2::scale_fill_gradient2(
            low = "#2166ac", 
            mid = "white", 
            high = "#b2182b", 
            midpoint = 0,
            name = "Difference\n(Comp - Ref)",
            limits = c(-max(abs(melted_diff$value), na.rm = TRUE), max(abs(melted_diff$value), na.rm = TRUE))
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = ggplot2::element_text(size = 10),
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
            panel.grid = ggplot2::element_blank()
          ) +
          ggplot2::labs(
            title = paste("Edge-wise Differences:", input$comparison_group, "vs", input$reference_group),
            subtitle = subtitle,
            x = "",
            y = ""
          ) +
          ggplot2::coord_fixed()
        
      } else if (input$comparison_metric == "node") {
        # Node metrics difference visualization
        metric_cols <- c("Degree_Centrality", "Betweenness_Centrality", 
                         "Closeness_Centrality", "Eigenvector_Centrality")
        
        # Reshape for visualization
        plot_data <- comparison_result[, c("Node", paste0(metric_cols, "_diff"))]
        names(plot_data) <- c("Node", metric_cols)
        
        # Add area mapping if available
        if (!is.null(rv$node_metrics) && "Brain_Area" %in% names(rv$node_metrics)) {
          # Get area information for each node
          area_info <- unique(rv$node_metrics[, c("Node", "Brain_Area")])
          plot_data <- merge(plot_data, area_info, by = "Node", all.x = TRUE)
          
          if (!is.null(preferences_results()$area_colors)) {
            plot_data$Color <- preferences_results()$area_colors[plot_data$Brain_Area]
          } else {
            plot_data$Color <- "#1F78B4"
          }
        } else {
          plot_data$Color <- "#1F78B4"
          plot_data$Brain_Area <- "Unknown"
        }
        
        # Melt for ggplot
        melted_data <- reshape2::melt(plot_data, id.vars = c("Node", "Brain_Area", "Color"))
        
        # Create enhanced parallel coordinates plot
        p <- ggplot2::ggplot(melted_data, ggplot2::aes(x = variable, y = value, group = Node, color = Color)) +
          ggplot2::geom_line(alpha = 0.7, size = 1) +
          ggplot2::geom_point(size = 2, alpha = 0.8) +
          ggplot2::scale_color_identity() +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
          ) +
          ggplot2::labs(
            title = paste("Node Metric Differences:", input$comparison_group, "vs", input$reference_group),
            subtitle = "Lines show changes for individual brain regions (colored by brain area)",
            x = "Network Metric",
            y = "Difference (Comparison - Reference)"
          )
        
      } else if (input$comparison_metric == "global") {
        # Global metrics difference visualization
        p <- ggplot2::ggplot(comparison_result, ggplot2::aes(x = Metric, y = Difference)) +
          ggplot2::geom_col(fill = "#1F78B4", alpha = 0.7, color = "black") +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          ggplot2::geom_text(ggplot2::aes(label = round(Difference, 3)), 
                             vjust = ifelse(comparison_result$Difference >= 0, -0.5, 1.2)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
          ) +
          ggplot2::labs(
            title = paste("Global Metric Differences:", input$comparison_group, "vs", input$reference_group),
            subtitle = "Positive values indicate higher values in comparison group",
            x = "Network Metric",
            y = "Difference (Comparison - Reference)"
          )
      }
      
      return(p)
    }
    
    # Quality report visualization
    render_quality_report_plot <- function() {
      req(data_import_results()$data_quality_metrics)
      
      metrics <- data_import_results()$data_quality_metrics
      focus <- input$quality_focus
      
      if (focus == "summary") {
        # Create overall quality summary visualization
        
        # Calculate quality scores
        sample_score <- ifelse(metrics$sample_size >= 30, 100, 
                               ifelse(metrics$sample_size >= 15, 80, 
                                      ifelse(metrics$sample_size >= 8, 60, 40)))
        
        missing_score <- ifelse(metrics$missing_percent < 1, 100,
                                ifelse(metrics$missing_percent < 5, 80,
                                       ifelse(metrics$missing_percent < 10, 60, 40)))
        
        outlier_percent <- (metrics$outliers_count / (metrics$sample_size * length(data_import_results()$column_info$region_columns))) * 100
        outlier_score <- ifelse(outlier_percent < 2, 100,
                                ifelse(outlier_percent < 5, 80,
                                       ifelse(outlier_percent < 10, 60, 40)))
        
        # Normality score
        if (length(metrics$normality_tests) > 0) {
          normal_count <- sum(sapply(metrics$normality_tests, function(x) x$consensus_normal))
          normal_percent <- (normal_count / length(metrics$normality_tests)) * 100
          normality_score <- ifelse(normal_percent >= 80, 100,
                                    ifelse(normal_percent >= 60, 80,
                                           ifelse(normal_percent >= 40, 60, 40)))
        } else {
          normality_score <- 80  # Default
        }
        
        # Statistical power score
        power_score <- ifelse(metrics$statistical_power$r_0.3 >= 0.8, 100,
                              ifelse(metrics$statistical_power$r_0.3 >= 0.6, 80,
                                     ifelse(metrics$statistical_power$r_0.3 >= 0.4, 60, 40)))
        
        # Create quality summary data
        quality_data <- data.frame(
          Aspect = c("Sample Size", "Missing Data", "Outliers", "Normality", "Statistical Power"),
          Score = c(sample_score, missing_score, outlier_score, normality_score, power_score),
          Category = c("Sample", "Data Quality", "Data Quality", "Distribution", "Power"),
          stringsAsFactors = FALSE
        )
        
        # Add color coding
        quality_data$Color <- ifelse(quality_data$Score >= 80, "#2ca02c",
                                     ifelse(quality_data$Score >= 60, "#ff7f0e", "#d62728"))
        
        # Create the plot
        p <- ggplot2::ggplot(quality_data, ggplot2::aes(x = reorder(Aspect, Score), y = Score, fill = Color)) +
          ggplot2::geom_col(alpha = 0.8, color = "black") +
          ggplot2::scale_fill_identity() +
          ggplot2::geom_text(ggplot2::aes(label = paste0(Score, "%")), 
                             vjust = -0.5, fontface = "bold") +
          ggplot2::ylim(0, 110) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
            plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
          ) +
          ggplot2::labs(
            title = "Data Quality Assessment Summary",
            subtitle = "Higher scores indicate better data quality (Green: Excellent, Orange: Good, Red: Needs Attention)",
            x = "Quality Aspect",
            y = "Quality Score (%)"
          )
        
      } else if (focus == "data_quality") {
        # Show detailed data quality metrics
        
        # Create a comprehensive quality overview
        detail_data <- data.frame(
          Metric = c("Sample Size", "Missing Data %", "Outliers", "Regions", "Groups"),
          Value = c(
            metrics$sample_size,
            round(metrics$missing_percent, 2),
            metrics$outliers_count,
            length(data_import_results()$column_info$region_columns),
            length(unique(data_import_results()$processed_data$Group))
          ),
          Benchmark = c("≥20", "<5%", "<5%", "≥5", "≥2"),
          Status = c(
            ifelse(metrics$sample_size >= 20, "Good", "Concerning"),
            ifelse(metrics$missing_percent < 5, "Good", "Concerning"),
            ifelse(metrics$outliers_count < (metrics$sample_size * 0.05), "Good", "Concerning"),
            ifelse(length(data_import_results()$column_info$region_columns) >= 5, "Good", "Concerning"),
            ifelse(length(unique(data_import_results()$processed_data$Group)) >= 2, "Good", "Concerning")
          ),
          stringsAsFactors = FALSE
        )
        
        detail_data$Color <- ifelse(detail_data$Status == "Good", "#2ca02c", "#d62728")
        
        p <- ggplot2::ggplot(detail_data, ggplot2::aes(x = reorder(Metric, Value), y = Value, fill = Color)) +
          ggplot2::geom_col(alpha = 0.8, color = "black") +
          ggplot2::scale_fill_identity() +
          ggplot2::geom_text(ggplot2::aes(label = paste0(Value, "\n(", Benchmark, ")")), 
                             hjust = 0.5, vjust = 0.5, fontface = "bold", color = "white") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
          ) +
          ggplot2::labs(
            title = "Detailed Data Quality Metrics",
            subtitle = "Values shown with recommended benchmarks in parentheses",
            x = "Quality Metric",
            y = "Measured Value"
          )
        
      } else if (focus == "recommendations") {
        # Create a recommendations visualization
        if (!is.null(metrics$recommendations) && length(metrics$recommendations) > 0) {
          # Create data frame of recommendations
          rec_data <- data.frame(
            Category = names(metrics$recommendations),
            Recommendation = unlist(metrics$recommendations),
            stringsAsFactors = FALSE
          )
          
          # Display as text table using grid.table
          return(
            gridExtra::tableGrob(
              rec_data,
              rows = NULL,
              theme = gridExtra::ttheme_minimal(
                core = list(fg_params = list(fontface = c("bold", "plain"))),
                colhead = list(fg_params = list(fontface = "bold")),
                rowhead = list(fg_params = list(fontface = "italic"))
              )
            )
          )
        } else {
          # No recommendations available
          return(
            ggplot2::ggplot() + 
              ggplot2::annotate("text", x = 0, y = 0, 
                                label = "No specific recommendations available") +
              ggplot2::theme_void() +
              ggplot2::labs(title = "Recommendations")
          )
        }
      } else {
        # Create a text-based report
        report_text <- paste(
          "Data Quality Report",
          paste(rep("=", 50), collapse = ""),
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "",
          "SUMMARY STATISTICS:",
          paste("• Sample size:", metrics$sample_size, "subjects"),
          paste("• Brain regions:", length(data_import_results()$column_info$region_columns)),
          paste("• Experimental groups:", length(unique(data_import_results()$processed_data$Group))),
          paste("• Missing data:", round(metrics$missing_percent, 2), "%"),
          paste("• Outliers detected:", metrics$outliers_count),
          "",
          "RECOMMENDATIONS:",
          if (!is.null(metrics$recommendations)) 
            paste("•", names(metrics$recommendations), ":", metrics$recommendations, collapse = "\n")
          else "No specific recommendations generated",
          "",
          "STATISTICAL POWER:",
          paste("• Power to detect r=0.3:", round(metrics$statistical_power$r_0.3, 3)),
          paste("• Power to detect r=0.5:", round(metrics$statistical_power$r_0.5, 3)),
          paste("• Power to detect r=0.7:", round(metrics$statistical_power$r_0.7, 3)),
          sep = "\n"
        )
        
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = report_text, 
                            hjust = 0, vjust = 1, size = 3.5, family = "mono") +
          ggplot2::theme_void() +
          ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20)) +
          ggplot2::labs(title = "Comprehensive Quality Report")
      }
      
      return(p)
    }
    
    # Default plot when no visualization is selected
    render_default_plot <- function() {
      ggplot2::ggplot() + 
        ggplot2::annotate("text", x = 0, y = 0, label = "Select a visualization type") +
        ggplot2::theme_void()
    }
    
    # Download current plot
    output$download_current_plot <- shiny::downloadHandler(
      filename = function() {
        # Create filename based on current settings
        plot_type <- input$display_type
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        
        base_name <- switch(plot_type,
                            "network" = paste0("network_", gsub("[^a-zA-Z0-9]", "_", input$selected_group)),
                            "heatmap" = paste0("heatmap_", gsub("[^a-zA-Z0-9]", "_", input$selected_group)),
                            "global" = "global_metrics",
                            "node" = "node_metrics",
                            "group_comparison" = paste0("comparison_", gsub("[^a-zA-Z0-9]", "_", input$reference_group), 
                                                        "_vs_", gsub("[^a-zA-Z0-9]", "_", input$comparison_group)),
                            "quality_report" = "quality_report",
                            "plot"
        )
        
        paste0(base_name, "_", timestamp, ".png")
      },
      
      content = function(file) {
        # Save current plot
        png(file, width = 800, height = 600, res = 100)
        if (!is.null(rv$current_plot)) {
          replayPlot(rv$current_plot)
        } else {
          # Fallback if no plot is stored
          plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No plot available")
        }
        dev.off()
      }
    )
    
    # Download all results
    output$download_all_results <- shiny::downloadHandler(
      filename = function() {
        paste0("brain_network_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      
      content = function(file) {
        req(rv$analysis_complete)
        
        # Show progress notification
        progress_id <- shiny::showNotification(
          "Preparing complete results package...",
          duration = NULL,
          closeButton = FALSE,
          type = "message"
        )
        
        on.exit({
          shiny::removeNotification(progress_id)
        })
        
        # Create a temporary directory for all files
        temp_dir <- file.path(tempdir(), "brain_network_results")
        if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
        dir.create(temp_dir, recursive = TRUE)
        
        # Create README file
        readme_file <- file.path(temp_dir, "README.txt")
        readme_content <- paste(
          "Brain Network Analysis Results",
          "=============================",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "",
          "This package contains results from your brain network analysis.",
          "",
          "Contents:",
          "- plots/: Visualizations of networks and metrics",
          "- data/: Correlation matrices and network metrics",
          "- parameters.txt: Analysis parameters used",
          "- quality_report.txt: Data quality assessment",
          "",
          "For questions about interpreting these results,",
          "please refer to the documentation.",
          sep = "\n"
        )
        writeLines(readme_content, readme_file)
        
        # Create plots directory
        plots_dir <- file.path(temp_dir, "plots")
        dir.create(plots_dir)
        
        # Create data directory
        data_dir <- file.path(temp_dir, "data")
        dir.create(data_dir)
        
        # Save parameters
        params_file <- file.path(temp_dir, "parameters.txt")
        if (!is.null(preferences_results())) {
          params_content <- capture.output(print(preferences_results()))
          writeLines(params_content, params_file)
        } else {
          writeLines("No parameters available", params_file)
        }
        
        # Save quality report
        quality_file <- file.path(temp_dir, "quality_report.txt")
        if (!is.null(data_import_results()$data_quality_metrics)) {
          metrics <- data_import_results()$data_quality_metrics
          quality_content <- paste(
            "Data Quality Assessment",
            "=======================",
            "",
            paste("Sample Size:", metrics$sample_size),
            paste("Missing Data:", round(metrics$missing_percent, 2), "%"),
            paste("Outliers:", metrics$outliers_count),
            "",
            "Statistical Power:",
            paste("- To detect r=0.3:", round(metrics$statistical_power$r_0.3, 3)),
            paste("- To detect r=0.5:", round(metrics$statistical_power$r_0.5, 3)),
            paste("- To detect r=0.7:", round(metrics$statistical_power$r_0.7, 3)),
            "",
            "Recommendations:",
            if (!is.null(metrics$recommendations)) {
              paste(sapply(names(metrics$recommendations), function(name) {
                paste("- ", name, ": ", metrics$recommendations[[name]], sep = "")
              }), collapse = "\n")
            } else "No specific recommendations available",
            sep = "\n"
          )
          writeLines(quality_content, quality_file)
        } else {
          writeLines("No quality metrics available", quality_file)
        }
        
        # Save correlation matrices
        if (!is.null(rv$networks)) {
          for (group_name in names(rv$networks)) {
            network <- rv$networks[[group_name]]
            
            if (!is.null(network$correlation_matrix)) {
              # Save as CSV
              corr_file <- file.path(data_dir, paste0("correlation_matrix_", 
                                                      gsub("[^a-zA-Z0-9]", "_", group_name), ".csv"))
              write.csv(network$correlation_matrix, corr_file)
            }
          }
        }
        
        # Save network metrics
        if (!is.null(rv$global_metrics)) {
          global_file <- file.path(data_dir, "global_metrics.csv")
          write.csv(rv$global_metrics, global_file, row.names = FALSE)
        }
        
        if (!is.null(rv$node_metrics)) {
          node_file <- file.path(data_dir, "node_metrics.csv")
          write.csv(rv$node_metrics, node_file, row.names = FALSE)
        }
        
        # Generate plots for each group
        if (!is.null(rv$networks)) {
          for (group_name in names(rv$networks)) {
            # Network plot
            network_file <- file.path(plots_dir, paste0("network_", 
                                                        gsub("[^a-zA-Z0-9]", "_", group_name), ".png"))
            
            png(network_file, width = 800, height = 600, res = 100)
            
            # Temporarily change inputs to generate plots for each group
            old_selected_group <- input$selected_group
            old_display_type <- input$display_type
            
            # Generate network plot
            local({
              # Create a local environment to avoid modifying inputs
              selected_group <- group_name
              display_type <- "network"
              
              # Create simple network plot
              network <- rv$networks[[selected_group]]
              
              if (!is.null(network) && !is.null(network$graph)) {
                # Basic network plot for each group
                plot_network(
                  network$graph,
                  node_metrics = rv$node_metrics[rv$node_metrics$Group == selected_group, ],
                  color_by = "brain_area",
                  size_by = NULL,
                  area_colors = preferences_results()$area_colors,
                  layout = "fr",
                  title = paste("Network Graph:", selected_group),
                  uniform_node_size = 15,
                  show_edge_labels = FALSE,
                  show_node_labels = TRUE
                )
              } else {
                plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
                     main = paste("No network available for", selected_group))
              }
            })
            
            dev.off()
            
            # Heatmap plot
            heatmap_file <- file.path(plots_dir, paste0("heatmap_", 
                                                        gsub("[^a-zA-Z0-9]", "_", group_name), ".png"))
            
            png(heatmap_file, width = 800, height = 600, res = 100)
            
            local({
              # Create a local environment to avoid modifying inputs
              selected_group <- group_name
              
              # Generate basic heatmap for each group
              network <- rv$networks[[selected_group]]
              
              if (!is.null(network) && !is.null(network$correlation_matrix)) {
                correlation_matrix <- network$correlation_matrix
                
                # Basic hierarchical clustering for better visualization
                dist_matrix <- stats::dist(1 - abs(correlation_matrix))
                hc <- stats::hclust(dist_matrix, method = "complete")
                correlation_matrix <- correlation_matrix[hc$order, hc$order]
                
                # Simple heatmap 
                image(1:ncol(correlation_matrix), 1:nrow(correlation_matrix), correlation_matrix,
                      col = colorRampPalette(c("blue", "white", "red"))(200),
                      xlab = "", ylab = "",
                      axes = FALSE,
                      main = paste("Correlation Heatmap:", selected_group))
                
                axis(1, at = 1:ncol(correlation_matrix), labels = colnames(correlation_matrix), 
                     las = 2, cex.axis = 0.7)
                axis(2, at = 1:nrow(correlation_matrix), labels = rownames(correlation_matrix), 
                     las = 2, cex.axis = 0.7)
              } else {
                plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
                     main = paste("No correlation data available for", selected_group))
              }
            })
            
            dev.off()
          }
        }
        
        # Save global metrics plot
        if (!is.null(rv$global_metrics)) {
          global_file <- file.path(plots_dir, "global_metrics.png")
          
          png(global_file, width = 800, height = 600, res = 100)
          
          # Simple barplot of network density
          density_data <- rv$global_metrics[rv$global_metrics$Metric == "Density", ]
          
          if (nrow(density_data) > 0) {
            barplot(density_data$Value, names.arg = density_data$Group,
                    col = "lightblue", main = "Network Density Comparison",
                    ylab = "Density", xlab = "Group")
          } else {
            plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
                 main = "No global metrics available")
          }
          
          dev.off()
        }
        
        # Create the zip file
        files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
        utils::zip(file, files, flags = "-j")
        
        # Clean up
        unlink(temp_dir, recursive = TRUE)
        
        shiny::showNotification(
          "Results package created successfully!",
          type = "message",
          duration = 5
        )
      }
    )  # close download_all_results downloadHandler
    
    # Return empty reactive - this module doesn't need to expose values
    return(shiny::reactive({
      list()
    }))
  })  # close moduleServer
}
