#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#install.packages('rsconnect')
# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(shinycssloaders)
data <- read.csv("C:\\Users\\upnjo\\OneDrive\\Documents\\Project2\\user_behavior_dataset.csv")

# UI Definition
ui <- page_sidebar(
  title = "Smartphone User Behavior Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar panel
  sidebar = sidebar(
    # Categorical variable selectors
    selectInput("device_select", "Select Device Model",
                choices = c("All", unique(data$Device.Model)),
                selected = "All"),
    
    selectInput("os_select", "Select Operating System",
                choices = c("All", unique(data$Operating.System)),
                selected = "All"),
    
    # First numeric variable selector
    selectInput("num_var1", "Select First Numeric Variable",
                choices = c(
                  "App Usage Time" = "App.Usage.Time..min.day.",
                  "Screen Time" = "Screen.On.Time..hours.day.",
                  "Battery Drain" = "Battery.Drain..mAh.day.",
                  "Number of Apps" = "Number.of.Apps.Installed",
                  "Data Usage" = "Data.Usage..MB.day.",
                  "Age" = "Age"
                )),
    
    # Dynamic slider for first numeric variable
    uiOutput("slider1"),
    
    # Second numeric variable selector
    selectInput("num_var2", "Select Second Numeric Variable",
                choices = c(
                  "App Usage Time" = "App.Usage.Time..min.day.",
                  "Screen Time" = "Screen.On.Time..hours.day.",
                  "Battery Drain" = "Battery.Drain..mAh.day.",
                  "Number of Apps" = "Number.of.Apps.Installed",
                  "Data Usage" = "Data.Usage..MB.day.",
                  "Age" = "Age"
                )),
    
    # Dynamic slider for second numeric variable
    uiOutput("slider2"),
    
    # Update button
    actionButton("update_data", "Update Analysis",
                 class = "btn-primary"),
    
    hr(),
    
    # Current selection summary
    textOutput("selection_text")
  ),
  
  # Main panel with tabs
  navset_tab(
    # About tab
    nav_panel("About",
              h2("Smartphone User Behavior Analysis"),
              tags$img(src = "smartphone.jpg", 
                       height = 200,
                       alt = "Smartphone Usage Illustration"),
              p("This application analyzes patterns in smartphone usage across different devices and user demographics."),
              
              h3("Data Description"),
              p("The dataset contains information about smartphone usage patterns including:"),
              tags$ul(
                tags$li("Device and OS information"),
                tags$li("Usage metrics (screen time, battery usage, etc.)"),
                tags$li("User demographics"),
                tags$li("Behavioral classification")
              ),
              
              h3("How to Use This App"),
              tags$ul(
                tags$li("Use the sidebar to filter data based on device type, operating system, and numeric ranges"),
                tags$li("The 'Data Download' tab allows you to view and download the filtered dataset"),
                tags$li("The 'Data Exploration' tab provides various visualizations and statistical summaries")
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
                                               choices = c(
                                                 "App Usage Time" = "App.Usage.Time..min.day.",
                                                 "Screen Time" = "Screen.On.Time..hours.day.",
                                                 "Battery Drain" = "Battery.Drain..mAh.day.",
                                                 "Number of Apps" = "Number.of.Apps.Installed",
                                                 "Data Usage" = "Data.Usage..MB.day.",
                                                 "Age" = "Age"
                                               ))
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
                
                # Categorical Summaries tab
                nav_panel("Categorical Summaries",
                          selectInput("cat_var", "Select Categorical Variable",
                                      choices = c(
                                        "Device Model" = "Device.Model",
                                        "Operating System" = "Operating.System",
                                        "Gender" = "Gender",
                                        "User Behavior Class" = "User.Behavior.Class"
                                      )),
                          verbatimTextOutput("cat_summary") %>% withSpinner()
                ),
                
                # Visualizations tab
                nav_panel("Visualizations",
                          fluidRow(
                            column(4,
                                   selectInput("plot_type", "Select Plot Type",
                                               choices = c(
                                                 "Density Plot" = "density",
                                                 "Scatter Plot" = "scatter",
                                                 "Violin Plot" = "violin",
                                                 "Box Plot" = "box",
                                                 "Bar Plot" = "bar",
                                                 "Heat Map" = "heat"
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

# Server Definition
server <- function(input, output, session) {
  
  # Variable labels for better display
  var_labels <- c(
    "App.Usage.Time..min.day." = "App Usage Time (min/day)",
    "Screen.On.Time..hours.day." = "Screen Time (hours/day)",
    "Battery.Drain..mAh.day." = "Battery Drain (mAh/day)",
    "Number.of.Apps.Installed" = "Number of Apps",
    "Data.Usage..MB.day." = "Data Usage (MB/day)",
    "Age" = "Age"
  )
  
  # Dynamic UI elements for numeric variable sliders
  output$slider1 <- renderUI({
    req(input$num_var1)
    var_range <- range(data[[input$num_var1]], na.rm = TRUE)
    sliderInput("num_var1_range",
                paste("Range of", var_labels[input$num_var1]),
                min = floor(var_range[1]),
                max = ceiling(var_range[2]),
                value = var_range)
  })
  
  output$slider2 <- renderUI({
    req(input$num_var2)
    var_range <- range(data[[input$num_var2]], na.rm = TRUE)
    sliderInput("num_var2_range",
                paste("Range of", var_labels[input$num_var2]),
                min = floor(var_range[1]),
                max = ceiling(var_range[2]),
                value = var_range)
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
  
  # Dynamic selection text
  output$selection_text <- renderText({
    paste("Currently showing", nrow(filtered_data()), "records")
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    filtered_data()
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
  
  # Dynamic UI for plot variables
  output$x_var_ui <- renderUI({
    num_vars <- c(
      "App Usage Time" = "App.Usage.Time..min.day.",
      "Screen Time" = "Screen.On.Time..hours.day.",
      "Battery Drain" = "Battery.Drain..mAh.day.",
      "Number of Apps" = "Number.of.Apps.Installed",
      "Data Usage" = "Data.Usage..MB.day.",
      "Age" = "Age"
    )
    
    cat_vars <- c(
      "Device Model" = "Device.Model",
      "Operating System" = "Operating.System",
      "Gender" = "Gender",
      "User Behavior Class" = "User.Behavior.Class"
    )
    
    choices <- switch(input$plot_type,
                      "density" = num_vars,
                      "scatter" = num_vars,
                      "violin" = cat_vars,
                      "box" = cat_vars,
                      "bar" = cat_vars,
                      "heat" = num_vars)
    
    selectInput("x_var", "X Variable", choices = choices)
  })
  
  output$y_var_ui <- renderUI({
    num_vars <- c(
      "App Usage Time" = "App.Usage.Time..min.day.",
      "Screen Time" = "Screen.On.Time..hours.day.",
      "Battery Drain" = "Battery.Drain..mAh.day.",
      "Number of Apps" = "Number.of.Apps.Installed",
      "Data Usage" = "Data.Usage..MB.day.",
      "Age" = "Age"
    )
    
    if(input$plot_type %in% c("scatter", "violin", "box", "heat")) {
      selectInput("y_var", "Y Variable", choices = num_vars)
    }
  })
  
  # Statistical summary output
  output$stat_summary <- renderPrint({
    req(filtered_data(), input$summary_var, input$group_var)
    
    filtered_data() %>%
      group_by(!!sym(input$group_var)) %>%
      summarise(
        n = n(),
        Mean = mean(!!sym(input$summary_var)),
        SD = sd(!!sym(input$summary_var)),
        Median = median(!!sym(input$summary_var)),
        Min = min(!!sym(input$summary_var)),
        Max = max(!!sym(input$summary_var))
      )
  })
  
  # Categorical summary output
  output$cat_summary <- renderPrint({
    req(filtered_data(), input$cat_var)
    
    table_summary <- table(filtered_data()[[input$cat_var]])
    print(table_summary)
    cat("\nPercentages:\n")
    print(round(prop.table(table_summary) * 100, 2))
  })
  
  # Plot generation
  output$plot <- renderPlot({
    req(filtered_data(), input$plot_type, input$x_var)
    
    df <- filtered_data()
    p <- ggplot(df)
    
    # Add color aesthetic if selected
    if(input$color_var != "None") {
      color_mapping <- aes_string(color = input$color_var, fill = input$color_var)
      p <- p + color_mapping
    }
    
    # Create specific plot based on type
    p <- switch(input$plot_type,
                "density" = p + 
                  geom_density(aes_string(x = input$x_var), alpha = 0.5),
                
                "scatter" = {
                  req(input$y_var)
                  p + 
                    geom_point(aes_string(x = input$x_var, y = input$y_var), 
                               alpha = 0.6) +
                    geom_smooth(aes_string(x = input$x_var, y = input$y_var), 
                                method = "lm", se = FALSE)
                },
                
                "violin" = {
                  req(input$y_var)
                  p + 
                    geom_violin(aes_string(x = input$x_var, y = input$y_var), 
                                alpha = 0.5)
                },
                
                "box" = {
                  req(input$y_var)
                  p + 
                    geom_boxplot(aes_string(x = input$x_var, y = input$y_var))
                },
                
                "bar" = p + 
                  geom_bar(aes_string(x = input$x_var)),
                
                "heat" = {
                  req(input$y_var)
                  p + 
                    geom_bin2d(aes_string(x = input$x_var, y = input$y_var)) +
                    scale_fill_viridis_c()
                }
    )
    
    # Add faceting if selected
    if(input$facet_var != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    # Add theme and labels
    p <- p + 
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      labs(
        title = paste("Plot of", var_labels[input$x_var]),
        x = var_labels[input$x_var],
        y = if(!is.null(input$y_var)) var_labels[input$y_var] else "Count"
      )
    
    print(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
