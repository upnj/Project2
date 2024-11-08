# app.R
library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(shinycssloaders)
library(viridis)  # Added for better heatmap colors

# Read data with only required columns
data <- read.csv("user_behavior_dataset.csv") %>%
  select(Device.Model, 
         Operating.System, 
         "App.Usage.Time" = App.Usage.Time..min.day.,
         "Screen.Time" = Screen.On.Time..hours.day.,
         "Battery.Drain" = Battery.Drain..mAh.day.,
         "Number.of.Apps" = Number.of.Apps.Installed,
         "Data.Usage" = Data.Usage..MB.day.,
         Age,
         Gender,
         "User.Behavior.Class" = User.Behavior.Class)

# Create variable labels mapping for numeric variables only
numeric_vars <- c(
  "App Usage Time" = "App.Usage.Time",
  "Screen Time" = "Screen.Time",
  "Battery Drain" = "Battery.Drain",
  "Number of Apps" = "Number.of.Apps",
  "Data Usage" = "Data.Usage",
  "Age" = "Age"
)

# UI
ui <- page_sidebar(
  title = "Smartphone User Behavior Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar
  sidebar = sidebar(
    # Categorical variable selectors
    selectInput("device_select", "Select Device Model",
                choices = c("All", sort(unique(data$Device.Model))),
                selected = "All"),
    
    selectInput("os_select", "Select Operating System",
                choices = c("All", sort(unique(data$Operating.System))),
                selected = "All"),
    
    # First numeric variable selector
    selectInput("num_var1", "Select First Numeric Variable",
                choices = numeric_vars,
                selected = "App.Usage.Time"),
    
    # Dynamic UI for first numeric variable range
    uiOutput("slider1"),
    
    # Second numeric variable selector
    selectInput("num_var2", "Select Second Numeric Variable",
                choices = numeric_vars,
                selected = "Battery.Drain"),
    
    # Dynamic UI for second numeric variable range
    uiOutput("slider2"),
    
    # Action button
    actionButton("update_data", "Update Analysis",
                 class = "btn-primary"),
    
    hr(),
    
    # Record count
    textOutput("record_count")
  ),
  
  # Main Panel
  navset_tab(
    # About tab
    nav_panel("About",
              h2("Smartphone User Behavior Analysis"),
              # Add the image here
              div(
                style = "text-align: left; margin: 20px 0;",  # Centers the image and adds margin
                img(src = "smartphone.jpg",  # Image file from www folder
                    height = "200px",        # Set height
                    alt = "Smartphone Image",# Alternative text
                    style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"  # Optional styling
                )
              ),
              p("This application analyzes smartphone usage patterns across different devices and user demographics."),
              
              h3("Variables Included"),
              tags$ul(
                tags$li("Device Information: Model and Operating System"),
                tags$li("Usage Metrics: App Usage Time, Screen Time, Battery Drain"),
                tags$li("App Statistics: Number of Apps, Data Usage"),
                tags$li("User Information: Age, Gender"),
                tags$li("Behavior Classification")
              ),
              
              h3("How to Use"),
              tags$ul(
                tags$li("Use sidebar filters to select specific subsets of data"),
                tags$li("Click 'Update Analysis' to apply filters"),
                tags$li("Explore data in different tabs"),
                tags$li("Download filtered data if needed")
              )
    ),
    
    # Data Download tab
    nav_panel("Data Download",
              DT::dataTableOutput("data_table") %>% withSpinner(),
              br(),
              downloadButton("download_data", "Download Filtered Data")
    ),
    
    # Data Exploration tab
    nav_panel("Data Exploration",
              navset_tab(
                # Summary Statistics tab
                nav_panel("Summary Statistics",
                          fluidRow(
                            column(6,
                                   selectInput("summary_var", "Variable to Summarize",
                                               choices = numeric_vars,
                                               selected = "App.Usage.Time")
                            ),
                            column(6,
                                   selectInput("group_var", "Group By",
                                               choices = c(
                                                 "Device Model" = "Device.Model",
                                                 "Operating System" = "Operating.System",
                                                 "Gender" = "Gender",
                                                 "User Behavior Class" = "User.Behavior.Class"
                                               ))
                            )
                          ),
                          verbatimTextOutput("stat_summary") %>% withSpinner()
                ),
                
                # Visualizations tab
                nav_panel("Visualizations",
                          fluidRow(
                            column(4,
                                   selectInput("plot_type", "Select Plot Type",
                                               choices = c(
                                                 "Density Plot" = "density",
                                                 "Scatter Plot" = "scatter",
                                                 "Box Plot" = "box",
                                                 "Bar Plot" = "bar",
                                                 "Violin Plot" = "violin",
                                                 "Heat Map" = "heatmap"
                                               ))
                            ),
                            column(4, uiOutput("x_var_ui")),
                            column(4, uiOutput("y_var_ui"))
                          ),
                          fluidRow(
                            column(6,
                                   selectInput("color_var", "Color/Fill By",
                                               choices = c("None",
                                                           "Device Model" = "Device.Model",
                                                           "Operating System" = "Operating.System",
                                                           "Gender" = "Gender",
                                                           "User Behavior Class" = "User.Behavior.Class"))
                            ),
                            column(6,
                                   selectInput("facet_var", "Facet By",
                                               choices = c("None",
                                                           "Device Model" = "Device.Model",
                                                           "Operating System" = "Operating.System",
                                                           "Gender" = "Gender",
                                                           "User Behavior Class" = "User.Behavior.Class"))
                            )
                          ),
                          plotOutput("plot") %>% withSpinner()
                )
              )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamic UI elements for numeric variable sliders
  output$slider1 <- renderUI({
    req(input$num_var1)
    var_range <- range(data[[input$num_var1]], na.rm = TRUE)
    sliderInput("num_var1_range",
                paste("Range of", input$num_var1),
                min = floor(var_range[1]),
                max = ceiling(var_range[2]),
                value = var_range,
                step = ifelse(input$num_var1 == "Screen.Time", 0.1, 1))
  })
  
  output$slider2 <- renderUI({
    req(input$num_var2)
    var_range <- range(data[[input$num_var2]], na.rm = TRUE)
    sliderInput("num_var2_range",
                paste("Range of", input$num_var2),
                min = floor(var_range[1]),
                max = ceiling(var_range[2]),
                value = var_range,
                step = ifelse(input$num_var2 == "Screen.Time", 0.1, 1))
  })
  
  # Reactive filtered dataset
  filtered_data <- eventReactive(input$update_data, {
    req(input$num_var1_range, input$num_var2_range)
    
    data_subset <- data
    
    # Filter categorical variables
    if(input$device_select != "All") {
      data_subset <- data_subset %>% 
        filter(Device.Model == input$device_select)
    }
    if(input$os_select != "All") {
      data_subset <- data_subset %>% 
        filter(Operating.System == input$os_select)
    }
    
    # Filter numeric variables
    data_subset <- data_subset %>%
      filter(
        !!sym(input$num_var1) >= input$num_var1_range[1],
        !!sym(input$num_var1) <= input$num_var1_range[2],
        !!sym(input$num_var2) >= input$num_var2_range[1],
        !!sym(input$num_var2) <= input$num_var2_range[2]
      )
    
    data_subset
  })
  
  # Record count
  output$record_count <- renderText({
    paste("Records shown:", nrow(filtered_data()))
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("smartphone_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Statistical summary output
  output$stat_summary <- renderPrint({
    req(filtered_data(), input$summary_var, input$group_var)
    
    # Get the data
    df <- filtered_data()
    
    # Convert User Behavior Class to factor if it's the grouping variable
    if(input$group_var == "User.Behavior.Class") {
      df$User.Behavior.Class <- as.factor(df$User.Behavior.Class)
    }
    
    # Create summary statistics
    summary_stats <- df %>%
      group_by(!!sym(input$group_var)) %>%
      summarise(
        n = n(),
        Mean = round(mean(!!sym(input$summary_var), na.rm = TRUE), 2),
        SD = round(sd(!!sym(input$summary_var), na.rm = TRUE), 2),
        Median = round(median(!!sym(input$summary_var), na.rm = TRUE), 2),
        Min = round(min(!!sym(input$summary_var), na.rm = TRUE), 2),
        Max = round(max(!!sym(input$summary_var), na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(if(input$group_var == "User.Behavior.Class") 
        as.numeric(as.character(!!sym(input$group_var))) 
        else 
          desc(n))
    
    # Print the summary
    cat("\nSummary Statistics for", gsub("\\.", " ", input$summary_var), 
        "grouped by", gsub("\\.", " ", input$group_var), "\n\n")
    print(summary_stats, n = nrow(summary_stats))
  })
  
  # Dynamic UI for plot variables
  output$x_var_ui <- renderUI({
    choices <- switch(input$plot_type,
                      "density" = numeric_vars,
                      "scatter" = numeric_vars,
                      "heatmap" = numeric_vars,
                      "box" = c(
                        "Device Model" = "Device.Model",
                        "Operating System" = "Operating.System",
                        "Gender" = "Gender",
                        "User Behavior Class" = "User.Behavior.Class"
                      ),
                      "bar" = c(
                        "Device Model" = "Device.Model",
                        "Operating System" = "Operating.System",
                        "Gender" = "Gender",
                        "User Behavior Class" = "User.Behavior.Class"
                      ),
                      "violin" = c(
                        "Device Model" = "Device.Model",
                        "Operating System" = "Operating.System",
                        "Gender" = "Gender",
                        "User Behavior Class" = "User.Behavior.Class"
                      ))
    
    selectInput("x_var", "X Variable", choices = choices)
  })
  
  output$y_var_ui <- renderUI({
    if(input$plot_type %in% c("scatter", "box", "violin", "heatmap")) {
      selectInput("y_var", "Y Variable", 
                  choices = numeric_vars)
    }
  })
  
  # Plot generation
  output$plot <- renderPlot({
    req(filtered_data(), input$plot_type, input$x_var)
    
    df <- filtered_data()
    p <- ggplot(df)
    
    # Add color aesthetic if selected (modified this part)
    if(input$color_var != "None" && input$plot_type != "heatmap") {  # Don't add color for heatmap
      color_mapping <- aes_string(color = input$color_var, 
                                  fill = input$color_var)
      p <- p + color_mapping
    }
    
    # Create specific plot based on type
    p <- switch(input$plot_type,
                "density" = p + 
                  geom_density(aes_string(x = input$x_var), 
                               alpha = 0.5),
                
                "scatter" = {
                  req(input$y_var)
                  p + 
                    geom_point(aes_string(x = input$x_var, 
                                          y = input$y_var), 
                               alpha = 0.6) +
                    geom_smooth(aes_string(x = input$x_var, 
                                           y = input$y_var), 
                                method = "lm", se = FALSE)
                },
                
                "box" = {
                  req(input$y_var)
                  p + 
                    geom_boxplot(aes_string(x = input$x_var, 
                                            y = input$y_var))
                },
                
                "bar" = p + 
                  geom_bar(aes_string(x = input$x_var)),
                
                "violin" = {
                  req(input$y_var)
                  p + 
                    geom_violin(aes_string(x = input$x_var, 
                                           y = input$y_var), 
                                alpha = 0.5)
                },
                
                "heatmap" = {
                  req(input$y_var)
                  
                  # Base heatmap plot
                  p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
                    geom_bin2d(bins = 30) +
                    scale_fill_viridis_c(option = "plasma", 
                                         name = "Count") +
                    theme(panel.grid = element_blank())
                  
                  # If color variable is selected, create separate heatmaps
                  if(input$color_var != "None") {
                    p <- p + facet_wrap(as.formula(paste("~", input$color_var)))
                  }
                  
                  p
                }
    )
    
    # Add faceting if selected (and not already added for heatmap)
    if(input$facet_var != "None" && 
       !(input$plot_type == "heatmap" && input$color_var != "None")) {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    # Add theme and labels
    p <- p + 
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"  # Changed to right for heatmap
      ) +
      labs(
        title = if(input$plot_type == "heatmap") 
          paste("Heatmap of", gsub("\\.", " ", input$x_var), 
                "vs", gsub("\\.", " ", input$y_var))
        else 
          paste("Plot of", gsub("\\.", " ", input$x_var)),
        x = gsub("\\.", " ", input$x_var),
        y = if(!is.null(input$y_var)) 
          gsub("\\.", " ", input$y_var) else "Count"
      )
    
    print(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)