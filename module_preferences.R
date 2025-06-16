#########################################################
# Enhanced module_preferences.R - With integrated recommendations
#########################################################

#' UI for the preferences module with integrated recommendations
#' 
#' @param id Namespace identifier for the module
#' @return A shiny tagList containing UI elements
#' @export
preferencesUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Include custom CSS for help system and recommendations
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
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
          
          .inline-help {
            font-size: 0.85em;
            color: #6c757d;
            margin-top: 2px;
            margin-bottom: 8px;
          }
          
          /* Recommendations panel styling */
          .recommendations-panel {
            margin-bottom: 20px;
            padding: 15px;
            background: linear-gradient(135deg, #e3f2fd 0%, #f3e5f5 100%);
            border: 2px solid #2196f3;
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          }
          
          .recommendations-title {
            font-size: 1.2em;
            font-weight: bold;
            color: #1976d2;
            margin-bottom: 10px;
            display: flex;
            align-items: center;
          }
          
          .recommendations-title .icon {
            margin-right: 8px;
            font-size: 1.3em;
          }
          
          .recommendation-item {
            margin-bottom: 12px;
            padding: 10px;
            background-color: rgba(255,255,255,0.8);
            border-radius: 5px;
            border-left: 4px solid #4caf50;
          }
          
          .recommendation-item h5 {
            margin: 0 0 5px 0;
            color: #2e7d32;
            font-weight: bold;
          }
          
          .recommendation-item p {
            margin: 0 0 5px 0;
            font-size: 0.9em;
            color: #424242;
          }
          
          .recommendation-item .rationale {
            font-size: 0.85em;
            color: #666;
            font-style: italic;
            margin-top: 5px;
          }
          
          .recommendations-actions {
            margin-top: 15px;
            text-align: center;
          }
          
          .recommendations-actions .btn {
            margin: 0 5px;
          }
          
          .data-quality-summary {
            background-color: rgba(255,255,255,0.9);
            padding: 10px;
            border-radius: 5px;
            margin-bottom: 10px;
            border-left: 4px solid #ff9800;
          }
          
          .data-quality-summary h5 {
            margin: 0 0 8px 0;
            color: #f57c00;
          }
          
          .quality-metric {
            display: inline-block;
            margin-right: 15px;
            font-size: 0.9em;
          }
          
          /* Improved input styling */
          .shiny-input-container {
            margin-bottom: 15px;
          }
        ")
      ),
      # Add JavaScript for toggling help panels
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
        shiny::h3("Analysis Preferences"),
        shiny::p("Configure your analysis settings, select regions and groups to include, and customize visualization options.")
      )
    ),
    
    # Initial loading message
    shiny::uiOutput(ns("initial_message")),
    
    # Main preferences UI - only shown when data is ready
    shiny::conditionalPanel(
      condition = paste0("output['", ns("data_ready"), "']"),
      
      shiny::fluidRow(
        # Left column - Region and Group Selection
        shiny::column(
          width = 6,
          shiny::wellPanel(
            shiny::h4("Region and Group Selection"),
            
            # Help panel for region selection
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Region Selection",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>Select the brain regions you want to include in your network analysis.</p>
                  <ul>
                    <li><strong>What to include:</strong> Focus on regions that are functionally or anatomically connected based on your research question.</li>
                    <li><strong>Sample considerations:</strong> Including too many regions with limited samples can reduce statistical power.</li>
                    <li><strong>Balance:</strong> Try to maintain a balanced representation across functional networks.</li>
                  </ul>
                  <p class='method-recommendation'>Recent research suggests focusing on regions within well-defined networks for more reliable results.</p>
                ")
              )
            ),
            
            shiny::selectizeInput(
              ns("selected_regions"),
              "Select Brain Regions to Include:",
              choices = NULL,
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            ),
            
            shiny::div(
              class = "btn-group",
              style = "margin-bottom: 15px;",
              shiny::actionButton(
                ns("select_all_regions"),
                "Select All",
                class = "btn-sm"
              ),
              shiny::actionButton(
                ns("clear_regions"),
                "Clear All",
                class = "btn-sm"
              )
            ),
            
            shiny::hr(),
            
            # Help panel for group selection
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Group Selection",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>Select the experimental groups to include in your analysis.</p>
                  <ul>
                    <li><strong>Group balance:</strong> Try to use groups with similar sample sizes for more reliable comparisons.</li>
                    <li><strong>Statistical power:</strong> Each group should ideally have at least 5-10 samples.</li>
                    <li><strong>Considerations:</strong> Be cautious when comparing groups with very different variances or outlier patterns.</li>
                  </ul>
                  <p class='method-recommendation'>For preclinical studies, matched control groups are essential for reliable network comparison.</p>
                ")
              )
            ),
            
            shiny::selectizeInput(
              ns("selected_groups"),
              "Select Groups to Include:",
              choices = NULL,
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            ),
            
            shiny::div(
              class = "btn-group",
              style = "margin-bottom: 15px;",
              shiny::actionButton(
                ns("select_all_groups"),
                "Select All",
                class = "btn-sm"
              ),
              shiny::actionButton(
                ns("clear_groups"),
                "Clear All",
                class = "btn-sm"
              )
            ),
            
            shiny::checkboxInput(
              ns("impute_missing"),
              "Impute missing values with column means",
              value = TRUE
            ),
            
            shiny::div(
              class = "inline-help",
              "Replaces NA values with the mean of each region. Best when missing data is random and limited."
            ),
            
            shiny::hr(),
            
            # ENHANCED RECOMMENDATIONS PANEL
            shiny::uiOutput(ns("recommendations_panel")),
            
            shiny::h4("Network Analysis Parameters"),
            
            # Help panel for correlation threshold
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Correlation Threshold",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>The correlation threshold determines which connections are included in your network.</p>
                  <ul>
                    <li><strong>Higher thresholds (0.6-0.8):</strong> More stringent, fewer connections, higher confidence but may miss important weaker connections.</li>
                    <li><strong>Lower thresholds (0.2-0.4):</strong> More inclusive, captures weaker connections but may include more noise.</li>
                    <li><strong>Medium thresholds (0.4-0.6):</strong> Balanced approach for most studies.</li>
                  </ul>
                  <p><strong>Sample size considerations:</strong></p>
                  <ul>
                    <li>Small samples (n < 10): Use higher thresholds (0.5-0.7)</li>
                    <li>Medium samples (n = 10-30): Moderate thresholds (0.3-0.5)</li>
                    <li>Large samples (n > 30): Lower thresholds can be reliable (0.2-0.4)</li>
                  </ul>
                  <p class='method-recommendation'>Recent research suggests using a data-driven approach that considers both sample size and network density.</p>
                ")
              )
            ),
            
            shiny::sliderInput(
              ns("correlation_threshold"),
              "Correlation Threshold:",
              min = 0.1,
              max = 0.9,
              value = 0.3,
              step = 0.05
            ),
            
            # Help panel for correlation types
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Correlation Types",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>Different correlation types capture different aspects of brain connectivity:</p>
                  <ul>
                    <li><strong>Standard Correlation:</strong> Measures overall association between regions. More stable with smaller samples but includes both direct and indirect connections.</li>
                    <li><strong>Partial Correlation:</strong> Attempts to identify direct connections by controlling for the influence of all other regions. Better for network discovery but requires more samples or regularization.</li>
                    <li><strong>Covariance-Based:</strong> Preserves magnitude information which can be important for certain neuroscience applications. Better for detecting hubs but more sensitive to scaling issues.</li>
                  </ul>
                  <p class='method-recommendation'>2025 benchmarking research indicates partial correlation with regularization provides the most accurate network topology when sample size is sufficient.</p>
                ")
              )
            ),
            
            # Updated method selection with new options
            shiny::radioButtons(
              ns("correlation_type"),
              "Correlation Type:",
              choices = c(
                "Standard Correlation" = "standard",
                "Partial Correlation (Direct Connections)" = "partial",
                "Covariance-Based" = "covariance"
              ),
              selected = "standard"
            ),
            
            # Only show method selection for standard correlation
            shiny::conditionalPanel(
              condition = paste0("input['", ns("correlation_type"), "'] == 'standard'"),
              
              # Help panel for correlation methods
              shiny::div(
                class = "help-panel",
                shiny::div(
                  class = "help-title",
                  shiny::icon("question-circle", class = "help-icon"),
                  "About Correlation Methods",
                  shiny::icon("chevron-down")
                ),
                shiny::div(
                  class = "help-content",
                  shiny::HTML("
                    <p>Choose the method that best matches your data properties:</p>
                    <ul>
                      <li><strong>Pearson:</strong> Best for normally distributed data with linear relationships. Most common in functional connectivity studies but sensitive to outliers.</li>
                      <li><strong>Spearman:</strong> Rank-based correlation that's robust to outliers and non-normal distributions. Good for data with potential non-linear relationships.</li>
                      <li><strong>Kendall:</strong> Another rank-based method that's more robust than Spearman for smaller sample sizes, but computationally more intensive.</li>
                    </ul>
                    <p class='method-recommendation'>For preclinical data that often contains outliers or non-normal distributions, Spearman correlation is frequently recommended.</p>
                  ")
                )
              ),
              
              shiny::radioButtons(
                ns("correlation_method"),
                "Correlation Method:",
                choices = c(
                  "Pearson" = "pearson",
                  "Spearman" = "spearman",
                  "Kendall" = "kendall"
                ),
                selected = "pearson"
              ),
              
              shiny::checkboxInput(
                ns("use_robust"),
                "Use robust correlation (less sensitive to outliers)",
                value = FALSE
              ),
              
              shiny::div(
                class = "inline-help",
                "Robust correlation methods minimize the influence of outliers, but may require more computational resources."
              ),
              
              shiny::checkboxInput(
                ns("use_absolute"),
                "Use Absolute Correlation Values",
                value = FALSE
              ),
              
              shiny::div(
                class = "inline-help",
                "Consider only the strength of connections, ignoring whether they are positive or negative."
              )
            ),
            
            # Show regularization option for partial correlation
            shiny::conditionalPanel(
              condition = paste0("input['", ns("correlation_type"), "'] == 'partial'"),
              
              # Help panel for regularization
              shiny::div(
                class = "help-panel",
                shiny::div(
                  class = "help-title",
                  shiny::icon("question-circle", class = "help-icon"),
                  "About Regularization",
                  shiny::icon("chevron-down")
                ),
                shiny::div(
                  class = "help-content",
                  shiny::HTML("
                    <p>Regularization stabilizes partial correlation estimates when the sample size is limited:</p>
                    <ul>
                      <li><strong>Why use it:</strong> Prevents overfitting and improves estimation when the number of subjects is close to or less than the number of brain regions.</li>
                      <li><strong>How it works:</strong> Shrinks small correlation values toward zero, retaining stronger connections.</li>
                      <li><strong>When to use:</strong> Almost always recommended for preclinical studies with typical sample sizes (5-30 subjects).</li>
                    </ul>
                    <p class='method-recommendation'>Required when number of regions ≥ number of samples. Strongly recommended for all partial correlation analyses in preclinical settings.</p>
                  ")
                )
              ),
              
              shiny::checkboxInput(
                ns("use_regularization"),
                "Use regularization (recommended for small sample sizes)",
                value = TRUE
              )
            ),
            
            # Statistical significance options
            shiny::hr(),
            
            shiny::h4("Statistical Testing"),
            
            # Help panel for statistical testing
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Statistical Testing",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>Statistical testing helps determine which connections are unlikely to occur by chance:</p>
                  <ul>
                    <li><strong>Purpose:</strong> Identifies statistically significant connections, reducing false positives.</li>
                    <li><strong>Multiple comparisons:</strong> When testing many connections simultaneously, the FDR correction helps control the false discovery rate.</li>
                    <li><strong>Significance threshold:</strong> Traditional value is 0.05, but stricter thresholds (0.01) increase confidence at the cost of potentially missing true connections.</li>
                  </ul>
                  <p class='method-recommendation'>For exploratory analyses, a threshold of 0.05 with FDR correction balances sensitivity and specificity. For confirmatory analyses, consider stricter thresholds (0.01).</p>
                ")
              )
            ),
            
            shiny::checkboxInput(
              ns("perform_significance"),
              "Perform statistical significance testing",
              value = TRUE
            ),
            
            shiny::conditionalPanel(
              condition = paste0("input['", ns("perform_significance"), "'] == true"),
              shiny::checkboxInput(
                ns("apply_fdr"),
                "Apply FDR correction for multiple comparisons",
                value = TRUE
              ),
              
              shiny::div(
                class = "inline-help",
                "FDR (False Discovery Rate) correction helps control false positives when testing many connections."
              ),
              
              shiny::sliderInput(
                ns("significance_threshold"),
                "Significance Threshold (p-value):",
                min = 0.001,
                max = 0.1,
                value = 0.05,
                step = 0.001
              )
            )
          )
        ),
        
        # Right column - Brain Area Configuration (unchanged from original)
        shiny::column(
          width = 6,
          # ... [Rest of the brain area configuration remains the same as in original code]
          shiny::wellPanel(
            shiny::h4("Brain Area Configuration"),
            
            # Help panel for brain area configuration
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Brain Area Assignment",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>Grouping brain regions into functional areas helps with visualization and analysis:</p>
                  <ul>
                    <li><strong>Purpose:</strong> Organizes regions into meaningful functional or anatomical groups (e.g., Hippocampus, Amygdala, Frontal Cortex).</li>
                    <li><strong>Visualization:</strong> Regions in the same area will share colors in network plots and can be analyzed as groups.</li>
                    <li><strong>Analysis:</strong> Allows you to examine connectivity patterns between functional systems.</li>
                  </ul>
                  <p><strong>Best practices:</strong></p>
                  <ul>
                    <li>Group regions based on established anatomical or functional classifications</li>
                    <li>Create new areas for specialized regions relevant to your research</li>
                    <li>Maintain consistent naming conventions across studies</li>
                  </ul>
                  <p class='method-recommendation'>Consistent area assignments improve comparability across studies and sessions.</p>
                ")
              )
            ),
            
            shiny::p("Assign each brain region to a functional area for visualization and analysis."),
            
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::div(
                  style = "max-height: 250px; overflow-y: auto;",
                  shiny::uiOutput(ns("area_assignment_ui"))
                )
              )
            ),
            
            shiny::hr(),
            
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textInput(
                  ns("new_area"),
                  "New Brain Area Name:",
                  placeholder = "e.g., Prefrontal Cortex"
                )
              ),
              
              shiny::column(
                width = 6,
                shiny::actionButton(
                  ns("add_area"),
                  "Add New Area",
                  icon = shiny::icon("plus"),
                  class = "btn-primary"
                )
              )
            ),
            
            shiny::hr(),
            
            shiny::h4("Color Configuration"),
            
            # Help panel for color configuration
            shiny::div(
              class = "help-panel",
              shiny::div(
                class = "help-title",
                shiny::icon("question-circle", class = "help-icon"),
                "About Color Configuration",
                shiny::icon("chevron-down")
              ),
              shiny::div(
                class = "help-content",
                shiny::HTML("
                  <p>Colors help visualize different brain areas and groups in your network analysis:</p>
                  <ul>
                    <li><strong>Brain Area Colors:</strong> Used to distinguish different functional systems in network visualizations.</li>
                    <li><strong>Group Colors:</strong> Used to differentiate experimental conditions in comparison plots.</li>
                  </ul>
                  <p><strong>Color selection tips:</strong></p>
                  <ul>
                    <li>Use distinct colors for different brain areas to aid visual separation</li>
                    <li>Consider using related colors (e.g., shades of blue) for related brain systems</li>
                    <li>For color-blind accessibility, avoid red-green combinations</li>
                    <li>For publications, check if colors will work in grayscale when printed</li>
                  </ul>
                ")
              )
            ),
            
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Brain Area Colors",
                shiny::br(),
                shiny::uiOutput(ns("area_colors_ui"))
              ),
              
              shiny::tabPanel(
                "Group Colors",
                shiny::br(),
                shiny::uiOutput(ns("group_colors_ui"))
              )
            )
          )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            style = "margin-top: 20px; text-align: center;",
            shiny::actionButton(
              ns("save_configuration"),
              "Save Configuration & Proceed to Results",
              icon = shiny::icon("save"),
              class = "btn-success btn-lg"
            )
          )
        )
      )
    )
  )
}

#' Complete Enhanced server function for the preferences module
#' 
#' @param id Namespace identifier for the module
#' @param data_input Reactive expression returning data input results
#' @return A reactive list with preferences configuration
#' @export
preferences <- function(id, data_input) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive values to store configuration
    rv <- shiny::reactiveValues(
      brain_areas = list(),
      selected_regions = NULL,
      selected_groups = NULL,
      area_colors = NULL,
      group_colors = NULL,
      configuration_saved = FALSE,
      data_ready = FALSE,
      initialization_complete = FALSE,
      recommendations = NULL,
      recommendations_applied = FALSE
    )
    
    # Default brain areas
    default_brain_areas <- list(
      "Dorsal HPC" = c("dDG", "dCA1", "dCA2", "dCA3"),
      "Ventral HPC" = c("vDG", "vCA1", "vCA3"),
      "Subiculum" = c("dSub", "vSub"),
      "Nucleus Accumbens" = c("NaC", "NaS"),
      "Frontal" = c("ACC", "IL", "PRL"),
      "Amygdala" = c("CeA", "BLA", "LA"),
      "Retrosplenial" = c("RSGab", "RSGc", "RSD")
    )
    
    # Default area colors
    default_area_colors <- c(
      "Dorsal HPC" = "#D3ADC4",
      "Ventral HPC" = "#C88AB1",
      "Subiculum" = "#9B59B6",
      "Nucleus Accumbens" = "#A3DFD7",
      "Frontal" = "#FAE9BD",
      "Amygdala" = "#F0BC94",
      "Retrosplenial" = "#85C1E9"
    )
    
    # Helper function to create valid input IDs
    make_valid_id <- function(name) {
      # Replace spaces, special characters with underscores
      id <- gsub("[^a-zA-Z0-9]", "_", name)
      # Ensure ID starts with a letter (Shiny requirement)
      if (!grepl("^[a-zA-Z]", id)) {
        id <- paste0("id_", id)
      }
      return(id)
    }
    
    # Enhanced recommendation generator
    generate_comprehensive_recommendations <- function(data_quality_metrics) {
      if (is.null(data_quality_metrics)) return(NULL)
      
      recommendations <- list()
      
      # Use minimum group size instead of total sample size
      n <- data_quality_metrics$sample_size
      
      # Check if we have group-specific information
      min_group_size <- NULL
      if (!is.null(data_quality_metrics$group_balance)) {
        # Extract minimum group size from balance information
        min_sizes <- sapply(data_quality_metrics$group_balance, function(x) x$min_size)
        min_group_size <- min(min_sizes)
      }
      
      # Use minimum group size if available, otherwise fall back to total sample size
      effective_sample_size <- if(!is.null(min_group_size)) min_group_size else n
      n_regions <- length(data_input()$column_info$region_columns)
      
      # Now use effective_sample_size instead of n for recommendations
      if (effective_sample_size < 10) {
        recommendations$sample_size <- list(
          title = "Small Group Size Considerations",
          recommendation = "Use conservative methods and higher thresholds",
          details = paste0("With minimum group size of ", effective_sample_size, " subjects, use standard correlation with higher threshold (0.5-0.7) and avoid partial correlation unless regularized."),
          rationale = "Small sample sizes per group are more susceptible to noise and overfitting. Conservative approaches improve reliability.",
          suggested_values = list(
            correlation_type = "standard",
            correlation_threshold = 0.6,
            correlation_method = "spearman",
            use_robust = TRUE
          )
        )
      } else if (effective_sample_size < 20) {
        recommendations$sample_size <- list(
          title = "Moderate Group Size Strategy",
          recommendation = "Balanced approach with regularization if using partial correlation",
          details = paste0("With minimum group size of ", effective_sample_size, " subjects, you can use moderate thresholds (0.4-0.5) and consider partial correlation with regularization."),
          rationale = "Moderate sample sizes allow for more sophisticated methods but still benefit from conservative parameter choices.",
          suggested_values = list(
            correlation_type = if (effective_sample_size >= n_regions) "partial" else "standard",
            correlation_threshold = 0.4,
            correlation_method = "pearson",
            use_regularization = TRUE
          )
        )
      } else {
        recommendations$sample_size <- list(
          title = "Sufficient Group Size - Full Method Flexibility",
          recommendation = "All methods available with lower thresholds",
          details = paste0("With minimum group size of ", effective_sample_size, " subjects, you can reliably use partial correlation and lower thresholds (0.3-0.4)."),
          rationale = "Large samples provide statistical power for detecting weaker connections and support advanced methods.",
          suggested_values = list(
            correlation_type = "partial",
            correlation_threshold = 0.3,
            correlation_method = "pearson",
            use_regularization = effective_sample_size < (n_regions * 2)
          )
        )
      }
      
      # Normality based recommendations
      non_normal_percent <- 0
      if (length(data_quality_metrics$normality_tests) > 0) {
        non_normal_count <- sum(sapply(data_quality_metrics$normality_tests, function(x) !x$consensus_normal))
        non_normal_percent <- non_normal_count / length(data_quality_metrics$normality_tests) * 100
      }
      
      if (non_normal_percent > 50) {
        recommendations$normality <- list(
          title = "Non-Normal Data Distribution",
          recommendation = "Use rank-based correlation methods",
          details = paste0(round(non_normal_percent), "% of regions show non-normal distributions. Spearman correlation is recommended."),
          rationale = "Non-parametric methods are more robust to violations of normality assumptions and outliers.",
          suggested_values = list(
            correlation_method = "spearman",
            use_robust = TRUE
          )
        )
      } else if (non_normal_percent > 20) {
        recommendations$normality <- list(
          title = "Mixed Distribution Patterns",
          recommendation = "Consider robust correlation methods",
          details = paste0(round(non_normal_percent), "% of regions show non-normal distributions. Both Pearson and Spearman are viable."),
          rationale = "Mixed distribution patterns suggest heterogeneity that robust methods can handle better.",
          suggested_values = list(
            correlation_method = "spearman",
            use_robust = FALSE
          )
        )
      }
      
      # Outlier based recommendations
      outlier_percent <- (data_quality_metrics$outliers_count / (n * n_regions)) * 100
      
      if (data_quality_metrics$outliers_count > (n * 0.1)) {
        recommendations$outliers <- list(
          title = "Outlier Management",
          recommendation = "Use robust correlation methods",
          details = paste0("Detected ", data_quality_metrics$outliers_count, " outliers (", round(outlier_percent, 1), "% of data points)."),
          rationale = "Outliers can dramatically affect correlation estimates. Robust methods minimize their influence.",
          suggested_values = list(
            use_robust = TRUE,
            correlation_method = "spearman"
          )
        )
      }
      
      # Statistical testing recommendations
      n_comparisons <- (n_regions * (n_regions - 1)) / 2
      
      if (n_comparisons > 100) {
        recommendations$multiple_testing <- list(
          title = "Multiple Comparisons Correction",
          recommendation = "Apply FDR correction and consider stricter thresholds",
          details = paste0("With ", n_comparisons, " pairwise comparisons, multiple testing correction is essential."),
          rationale = "Many statistical tests increase the probability of false discoveries. FDR correction controls this risk.",
          suggested_values = list(
            perform_significance = TRUE,
            apply_fdr = TRUE,
            significance_threshold = if (n_comparisons > 300) 0.01 else 0.05
          )
        )
      }
      
      # Data quality specific recommendations
      if (data_quality_metrics$missing_percent > 5) {
        recommendations$missing_data <- list(
          title = "Missing Data Handling",
          recommendation = "Enable data imputation and consider robust methods",
          details = paste0("Missing data: ", round(data_quality_metrics$missing_percent, 1), "% of values."),
          rationale = "Missing data can bias correlation estimates. Imputation with robust methods minimizes this bias.",
          suggested_values = list(
            impute_missing = TRUE,
            use_robust = TRUE
          )
        )
      }
      
      return(recommendations)
    }
    
    # Initial message based on data state
    output$initial_message <- shiny::renderUI({
      if (!is.null(data_input()) && data_input()$data_configured) {
        rv$data_ready <- TRUE
        return(NULL)
      } else {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            "Please complete the Data Import step first before configuring analysis preferences."
          )
        )
      }
    })
    
    # Flag to indicate data is ready
    output$data_ready <- shiny::reactive({
      return(rv$data_ready)
    })
    shiny::outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
    
    # Render recommendations panel
    output$recommendations_panel <- shiny::renderUI({
      # Only show if we have data quality metrics and haven't applied recommendations yet
      req(data_input()$data_quality_metrics)
      
      if (rv$recommendations_applied) {
        return(NULL)
      }
      
      metrics <- data_input()$data_quality_metrics
      rv$recommendations <- generate_comprehensive_recommendations(metrics)
      
      if (is.null(rv$recommendations) || length(rv$recommendations) == 0) {
        return(NULL)
      }
      
      # Create data quality summary
      quality_summary <- shiny::div(
        class = "data-quality-summary",
        shiny::h5(shiny::icon("chart-line"), "Data Quality Assessment"),
        shiny::div(
          shiny::span(class = "quality-metric", paste("Sample size:", metrics$sample_size)),
          shiny::span(class = "quality-metric", paste("Missing data:", round(metrics$missing_percent, 1), "%")),
          shiny::span(class = "quality-metric", paste("Outliers:", metrics$outliers_count)),
          shiny::span(class = "quality-metric", paste("Regions:", length(data_input()$column_info$region_columns)))
        )
      )
      
      # Create recommendation items
      recommendation_items <- lapply(names(rv$recommendations), function(rec_name) {
        rec <- rv$recommendations[[rec_name]]
        
        shiny::div(
          class = "recommendation-item",
          shiny::h5(shiny::icon("lightbulb"), rec$title),
          shiny::p(shiny::strong("Recommendation: "), rec$recommendation),
          shiny::p(rec$details),
          shiny::div(class = "rationale", shiny::strong("Scientific rationale: "), rec$rationale)
        )
      })
      
      # Create the full recommendations panel
      shiny::div(
        class = "recommendations-panel",
        shiny::div(
          class = "recommendations-title",
          shiny::icon("magic", class = "icon"),
          "Smart Parameter Recommendations"
        ),
        
        quality_summary,
        
        do.call(shiny::tagList, recommendation_items),
        
        shiny::div(
          class = "recommendations-actions",
          shiny::actionButton(
            session$ns("apply_all_recommendations"),
            "Apply All Recommendations",
            icon = shiny::icon("check-circle"),
            class = "btn-success"
          ),
          shiny::actionButton(
            session$ns("dismiss_recommendations"),
            "Dismiss",
            icon = shiny::icon("times"),
            class = "btn-outline-secondary"
          )
        )
      )
    })
    
    # Apply all recommendations
    shiny::observeEvent(input$apply_all_recommendations, {
      req(rv$recommendations)
      
      # Collect all suggested values
      all_suggestions <- list()
      
      for (rec in rv$recommendations) {
        if (!is.null(rec$suggested_values)) {
          all_suggestions <- c(all_suggestions, rec$suggested_values)
        }
      }
      
      # Apply suggestions to inputs
      if ("correlation_threshold" %in% names(all_suggestions)) {
        shiny::updateSliderInput(
          session,
          "correlation_threshold",
          value = all_suggestions$correlation_threshold
        )
      }
      
      if ("correlation_type" %in% names(all_suggestions)) {
        shiny::updateRadioButtons(
          session,
          "correlation_type",
          selected = all_suggestions$correlation_type
        )
      }
      
      if ("correlation_method" %in% names(all_suggestions)) {
        shiny::updateRadioButtons(
          session,
          "correlation_method",
          selected = all_suggestions$correlation_method
        )
      }
      
      if ("use_robust" %in% names(all_suggestions)) {
        shiny::updateCheckboxInput(
          session,
          "use_robust",
          value = all_suggestions$use_robust
        )
      }
      
      if ("use_regularization" %in% names(all_suggestions)) {
        shiny::updateCheckboxInput(
          session,
          "use_regularization",
          value = all_suggestions$use_regularization
        )
      }
      
      if ("perform_significance" %in% names(all_suggestions)) {
        shiny::updateCheckboxInput(
          session,
          "perform_significance",
          value = all_suggestions$perform_significance
        )
      }
      
      if ("apply_fdr" %in% names(all_suggestions)) {
        shiny::updateCheckboxInput(
          session,
          "apply_fdr",
          value = all_suggestions$apply_fdr
        )
      }
      
      if ("significance_threshold" %in% names(all_suggestions)) {
        shiny::updateSliderInput(
          session,
          "significance_threshold",
          value = all_suggestions$significance_threshold
        )
      }
      
      if ("impute_missing" %in% names(all_suggestions)) {
        shiny::updateCheckboxInput(
          session,
          "impute_missing",
          value = all_suggestions$impute_missing
        )
      }
      
      rv$recommendations_applied <- TRUE
      
      shiny::showNotification(
        "All recommendations have been applied to your analysis parameters!",
        type = "message",
        duration = 5
      )
    })
    
    # Dismiss recommendations
    shiny::observeEvent(input$dismiss_recommendations, {
      rv$recommendations_applied <- TRUE
      shiny::showNotification(
        "Recommendations dismissed. You can configure parameters manually.",
        type = "message",
        duration = 3
      )
    })
    
    # ONE-TIME INITIALIZATION when data is available
    shiny::observe({
      req(data_input())
      
      # Only initialize once and if data is configured and we haven't saved configuration yet
      if (!data_input()$data_configured || rv$configuration_saved || rv$initialization_complete) {
        return()
      }
      
      # Mark initialization as started to prevent loops
      rv$initialization_complete <- TRUE
      
      # Get region columns
      region_columns <- data_input()$column_info$region_columns
      
      # Update region selection input
      shiny::updateSelectizeInput(session, "selected_regions", 
                                  choices = region_columns,
                                  selected = region_columns)
      
      # Store selected regions
      rv$selected_regions <- region_columns
      
      # Get unique groups
      if ("Group" %in% names(data_input()$processed_data)) {
        groups <- unique(data_input()$processed_data$Group)
      } else {
        # If no combined group column, use the first group column
        group_col <- data_input()$column_info$group_columns[1]
        groups <- unique(data_input()$processed_data[[group_col]])
      }
      
      # Update group selection input
      shiny::updateSelectizeInput(session, "selected_groups", 
                                  choices = groups,
                                  selected = groups)
      
      # Store selected groups
      rv$selected_groups <- groups
      
      # Initialize brain areas
      # Match default areas with available regions
      matched_areas <- list()
      for (area in names(default_brain_areas)) {
        matching_regions <- intersect(default_brain_areas[[area]], region_columns)
        if (length(matching_regions) > 0) {
          matched_areas[[area]] <- matching_regions
        }
      }
      
      # For any unmatched regions, create "Other" category
      unmatched_regions <- setdiff(region_columns, unlist(matched_areas))
      if (length(unmatched_regions) > 0) {
        matched_areas[["Other"]] <- unmatched_regions
      }
      
      rv$brain_areas <- matched_areas
      
      # Initialize colors
      area_colors <- default_area_colors[names(matched_areas)]
      missing_areas <- setdiff(names(matched_areas), names(default_area_colors))
      if (length(missing_areas) > 0) {
        # Generate colors for missing areas
        additional_colors <- generate_area_colors(
          setNames(vector("list", length(missing_areas)), missing_areas)
        )
        area_colors <- c(area_colors, additional_colors)
      }
      rv$area_colors <- area_colors
      
      # Generate group colors
      rv$group_colors <- generate_group_colors(groups)
      names(rv$group_colors) <- groups
    })
    
    # Region selection observers (only active after initialization)
    shiny::observeEvent(input$selected_regions, {
      if (rv$initialization_complete) {
        rv$selected_regions <- input$selected_regions
      }
    }, ignoreInit = TRUE)
    
    # Select all regions
    shiny::observeEvent(input$select_all_regions, {
      req(data_input()$column_info$region_columns)
      shiny::updateSelectizeInput(session, "selected_regions", 
                                  selected = data_input()$column_info$region_columns)
      rv$selected_regions <- data_input()$column_info$region_columns
    })
    
    # Clear all regions
    shiny::observeEvent(input$clear_regions, {
      shiny::updateSelectizeInput(session, "selected_regions", selected = character(0))
      rv$selected_regions <- character(0)
    })
    
    # Group selection observers (only active after initialization)
    shiny::observeEvent(input$selected_groups, {
      if (rv$initialization_complete) {
        rv$selected_groups <- input$selected_groups
      }
    }, ignoreInit = TRUE)
    
    # Select all groups
    shiny::observeEvent(input$select_all_groups, {
      req(data_input()$processed_data)
      
      if ("Group" %in% names(data_input()$processed_data)) {
        groups <- unique(data_input()$processed_data$Group)
      } else {
        # If no combined group column, use the first group column
        group_col <- data_input()$column_info$group_columns[1]
        groups <- unique(data_input()$processed_data[[group_col]])
      }
      
      shiny::updateSelectizeInput(session, "selected_groups", selected = groups)
      rv$selected_groups <- groups
    })
    
    # Clear all groups
    shiny::observeEvent(input$clear_groups, {
      shiny::updateSelectizeInput(session, "selected_groups", selected = character(0))
      rv$selected_groups <- character(0)
    })
    
    # Add new brain area
    shiny::observeEvent(input$add_area, {
      req(input$new_area, nchar(input$new_area) > 0)
      
      # Check if area already exists
      if (input$new_area %in% names(rv$brain_areas)) {
        shiny::showNotification(paste("Brain area", input$new_area, "already exists"), 
                                type = "warning")
      } else {
        # Add new empty area
        rv$brain_areas[[input$new_area]] <- character(0)
        
        # Generate color for new area
        new_color <- generate_area_colors(
          setNames(list(character(0)), input$new_area)
        )
        rv$area_colors <- c(rv$area_colors, new_color)
        
        # Clear input
        shiny::updateTextInput(session, "new_area", value = "")
        
        shiny::showNotification(paste("Added new brain area:", input$new_area), 
                                type = "message")
      }
    })
    
    # UI for area assignment
    output$area_assignment_ui <- shiny::renderUI({
      req(rv$selected_regions, rv$brain_areas, rv$initialization_complete)
      
      # Create UI elements for each selected region
      ui_elements <- lapply(rv$selected_regions, function(region) {
        # Find current assignment
        current_area <- ""
        for (area in names(rv$brain_areas)) {
          if (region %in% rv$brain_areas[[area]]) {
            current_area <- area
            break
          }
        }
        
        shiny::fluidRow(
          style = "margin-bottom: 5px;",
          shiny::column(
            width = 5,
            shiny::strong(region)
          ),
          shiny::column(
            width = 7,
            shiny::selectInput(
              session$ns(paste0("area_", region)),
              label = NULL,
              choices = c("Unassigned" = "", names(rv$brain_areas)),
              selected = current_area,
              width = "100%"
            )
          )
        )
      })
      
      # Combine all UI elements
      do.call(shiny::tagList, ui_elements)
    })
    
    # Single observer for all area assignments (prevents loops)
    shiny::observe({
      req(rv$selected_regions, rv$initialization_complete)
      
      # Get all area assignment inputs
      area_assignments <- list()
      for (region in rv$selected_regions) {
        input_id <- paste0("area_", region)
        assignment <- input[[input_id]]
        if (!is.null(assignment) && assignment != "") {
          area_assignments[[region]] <- assignment
        }
      }
      
      # Rebuild brain_areas from assignments
      new_areas <- list()
      
      # Start with empty lists for each existing area
      for (area in names(rv$brain_areas)) {
        new_areas[[area]] <- character(0)
      }
      
      # Add regions to their assigned areas
      for (region in names(area_assignments)) {
        area <- area_assignments[[region]]
        if (!(area %in% names(new_areas))) {
          new_areas[[area]] <- character(0)
        }
        new_areas[[area]] <- c(new_areas[[area]], region)
      }
      
      # Remove empty areas (except keep at least the original areas)
      original_areas <- names(default_brain_areas)
      for (area in names(new_areas)) {
        if (length(new_areas[[area]]) == 0 && !(area %in% original_areas)) {
          new_areas[[area]] <- NULL
        }
      }
      
      # Update brain_areas only if different
      if (!identical(rv$brain_areas, new_areas)) {
        rv$brain_areas <- new_areas
      }
    })
    
    # UI for area colors
    output$area_colors_ui <- shiny::renderUI({
      req(rv$brain_areas, rv$area_colors, rv$initialization_complete)
      
      ui_elements <- lapply(names(rv$brain_areas), function(area) {
        input_id <- paste0("color_area_", make_valid_id(area))
        
        shiny::fluidRow(
          style = "margin-bottom: 10px;",
          shiny::column(
            width = 4,
            shiny::strong(area),
            shiny::p(paste(length(rv$brain_areas[[area]]), "regions"), 
                     style = "color: #666; font-size: 90%;")
          ),
          shiny::column(
            width = 8,
            colourpicker::colourInput(
              session$ns(input_id),
              label = NULL,
              value = rv$area_colors[area],
              allowTransparent = FALSE,
              returnName = FALSE
            )
          )
        )
      })
      
      do.call(shiny::tagList, ui_elements)
    })
    
    # UI for group colors
    output$group_colors_ui <- shiny::renderUI({
      req(rv$selected_groups, rv$group_colors, rv$initialization_complete)
      
      ui_elements <- lapply(rv$selected_groups, function(group) {
        safe_id <- gsub("[^a-zA-Z0-9]", "_", group)
        input_id <- paste0("color_group_", safe_id)
        
        shiny::fluidRow(
          style = "margin-bottom: 10px;",
          shiny::column(
            width = 4,
            shiny::strong(group)
          ),
          shiny::column(
            width = 8,
            colourpicker::colourInput(
              session$ns(input_id),
              label = NULL,
              value = rv$group_colors[group],
              allowTransparent = FALSE,
              returnName = FALSE
            )
          )
        )
      })
      
      do.call(shiny::tagList, ui_elements)
    })
    
    # Single observer for area colors (prevents loops)
    shiny::observe({
      req(rv$brain_areas, rv$initialization_complete)
      
      # Check each area color input
      for (area in names(rv$brain_areas)) {
        input_id <- paste0("color_area_", make_valid_id(area))
        color_value <- input[[input_id]]
        
        if (!is.null(color_value) && color_value != rv$area_colors[area]) {
          rv$area_colors[area] <- color_value
        }
      }
    })
    
    # Single observer for group colors (prevents loops)
    shiny::observe({
      req(rv$selected_groups, rv$initialization_complete)
      
      # Check each group color input
      for (group in rv$selected_groups) {
        safe_id <- gsub("[^a-zA-Z0-9]", "_", group)
        input_id <- paste0("color_group_", safe_id)
        color_value <- input[[input_id]]
        
        if (!is.null(color_value) && color_value != rv$group_colors[group]) {
          rv$group_colors[group] <- color_value
        }
      }
    })
    
    # Save configuration with manual navigation
    shiny::observeEvent(input$save_configuration, {
      req(rv$selected_regions, rv$selected_groups, rv$brain_areas)
      
      # Validate configuration
      if (length(rv$selected_regions) == 0) {
        shiny::showNotification("Please select at least one brain region", type = "warning")
        return()
      }
      
      if (length(rv$selected_groups) == 0) {
        shiny::showNotification("Please select at least one group", type = "warning")
        return()
      }
      
      # Check if all selected regions are assigned to an area
      assigned_regions <- unlist(rv$brain_areas)
      unassigned_regions <- setdiff(rv$selected_regions, assigned_regions)
      
      if (length(unassigned_regions) > 0) {
        # Create "Other" category for unassigned regions
        if (!"Other" %in% names(rv$brain_areas)) {
          rv$brain_areas[["Other"]] <- unassigned_regions
          
          # Generate color for Other category
          if (!"Other" %in% names(rv$area_colors)) {
            rv$area_colors["Other"] <- "#CCCCCC"
          }
        } else {
          rv$brain_areas[["Other"]] <- c(rv$brain_areas[["Other"]], unassigned_regions)
        }
        
        shiny::showNotification(
          paste("Assigned", length(unassigned_regions), 
                "unassigned region(s) to 'Other' category"),
          type = "message"
        )
      }
      
      # Mark configuration as saved
      rv$configuration_saved <- TRUE
      
      # Show success message
      shiny::showNotification("Configuration saved successfully", type = "message")
      
      # Manual navigation to results tab using JavaScript
      shinyjs::runjs("
        setTimeout(function() {
          $('a[data-value=\"results\"]').click();
        }, 500);
      ")
    })
    
    # Return reactive values for use in other modules
    return(shiny::reactive({
      # Get analysis data (imputed if needed)
      analysis_data <- NULL
      
      if (!is.null(data_input()) && !is.null(data_input()$processed_data)) {
        analysis_data <- data_input()$processed_data
        
        # Apply imputation if selected and there are missing values
        if (input$impute_missing && data_input()$has_missing_data) {
          analysis_data <- impute_with_mean(analysis_data, rv$selected_regions)
        }
      }
      
      list(
        selected_regions = rv$selected_regions,
        selected_groups = rv$selected_groups,
        brain_areas = rv$brain_areas,
        area_colors = rv$area_colors,
        group_colors = rv$group_colors,
        correlation_threshold = input$correlation_threshold,
        correlation_method = input$correlation_method,
        correlation_type = input$correlation_type,
        use_robust = input$use_robust,
        use_regularization = input$use_regularization,
        use_absolute = input$use_absolute,
        impute_missing = input$impute_missing,
        perform_significance = input$perform_significance,
        apply_fdr = input$apply_fdr,
        significance_threshold = input$significance_threshold,
        configuration_saved = rv$configuration_saved,
        analysis_data = analysis_data,
        recommendations = rv$recommendations,
        recommendations_applied = rv$recommendations_applied
      )
    }))
  })
}
