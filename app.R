# Load packages
pacman::p_load(shiny, shinyFeedback, tidyverse, dplyr, ggplot2, lubridate, scales)

# Set themes
basic_theme <- theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
        axis.ticks.length = unit(.2, "cm"), 
        axis.text.x = element_text(color = "black", family = "sans", size = 12),
        axis.text.y = element_text(color = "black", family = "sans", size = 12),
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12), 
        strip.background = element_rect(color = NA, fill = NA), 
        plot.title = element_text(color = "black", family = "sans", size = 12))

theme_1 <- basic_theme + theme(axis.line = element_line(linewidth = 0.5))

theme_2 <- basic_theme + 
  theme(axis.line = element_line(linewidth = 0.3), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Custom color palette
pal <- c("#B5B5B5", "#80607E", "#64D1C5", "#D1B856", "#8F845B", "#5C7C78", "#524410", "#470C43")
expanded_pal <- colorRampPalette(pal)(40)

# Function to subsample colors based on the number of levels of rat_id
get_subsampled_palette <- function(n) {
  expanded_pal[round(seq(1, length(expanded_pal), length.out = n))]
}



# Example data that matches the format users should upload
example_table <- data.frame(
  rat_id = c("1", "2", "3"),
  X20240101 = c(250, 245, 260),
  X20240102 = c(248, 244, 258),
  X20240103 = c(246, 243, 256)
)


# UI
ui <- fluidPage(
  useShinyFeedback(),  # Initialize shinyFeedback
  titlePanel("Rat weight loss prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      helpText(HTML("Please upload a CSV file with the first column being 'rat_id'. The remaining columns should contain the weight data, with the column names being the date of the recording in the format %Y%m%d (e.g. 20240101).<br><br>",
               "When converting the excel file to csv, an 'X' might be added to the date column names. This is fine and will be removed by the app. Also no problem if your table doesn't have this.<br><br>",
               "The app will visualize weight data make predictions for the next 30 days based on linear models (weighted on the later dates since they are better predictors than earlier dates).")),
      tableOutput("exampleTable"),
      downloadButton("downloadData1", "Download Fig. 1 - weigths"),
      downloadButton("downloadData2", "Download Fig. 2 - weights prediction"),
      width = 6
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  
  # Display the example table
  output$exampleTable <- renderTable({
    example_table
  })
  
  # Function to read and process the CSV files
  df <- reactive({
    req(input$file1)
    files <- input$file1
    df <- read.csv(files$datapath) |>
      pivot_longer(cols = -rat_id, names_to = 'date', values_to = 'weight') |>
      mutate(date = gsub('X', '', date)) |>
      mutate(date = as_date(date, format = '%Y%m%d')) |>
      mutate(rat_id = factor(rat_id))
    df
  })
  
  # Reactive expression to get the number of unique rat_ids and subsample the palette
  subsampled_palette <- reactive({
    num_levels <- nlevels(df()$rat_id)  # Get the number of unique levels of rat_id
    get_subsampled_palette(num_levels)  # Subsample the palette based on the number of levels
  })
  
  
  
  df_endpoints <- reactive({
    #calculate the humane endpoint weights
    df() |> group_by(rat_id) |> 
      filter(date == min(date)) |> 
      mutate(weight_loss_17 = 0.83 * weight,
             weight_loss_15 = 0.85 * weight,
             weight_loss_13 = 0.87 * weight) |>
      pivot_longer(weight_loss_17:weight_loss_13, 
                   names_to = 'weight_loss_perc', values_to = 'threshold_weight') |>
      mutate(weight_loss_perc = factor(weight_loss_perc, 
                                       levels = c('weight_loss_13', 'weight_loss_15', 'weight_loss_17')))
    
    
  })
  
  
  #make a weighted dataframe
  df_weighted <- reactive({
    df() |> 
      group_by(rat_id) |> 
      mutate(weight_factor = as.numeric(date - min(date) + 1)) |>  # Linear weight based on time
      ungroup()
  })
  
  
  #fit a linear model for each rat_id
  model_list <- reactive({
    df_weighted() |> 
      group_by(rat_id) |> 
      do(model = lm(weight ~ date, data = ., weights = weight_factor))
  })
  
  
  #get the max date
  max_date <- reactive({ max(df()$date) })
  
  #create future dates (next 30 days)
  future_dates <- reactive({
    data.frame(date = seq(max_date(), by = "day", length.out = 30))
  })
  
  #expand this for each rat_id
  future_data <- reactive({
    df() |> select(rat_id) |> distinct() |> crossing(future_dates())
  })
  
  #generate future predictions for each rat_id (based on the model)
  future_predictions <- reactive({
    future_data() |> group_by(rat_id) |>
      do(data.frame(., predicted = predict(model_list()$model[[which(model_list()$rat_id == .$rat_id[1])]], newdata = .)))
  })
  
  #combine original data with predictions
  plot_data <- reactive({
    df() |> mutate(predicted = NA) |>  #mark original data
      bind_rows(future_predictions())  #add predictions
  })
  
  # Display plot1 (Fig. 1) in the UI
  reactivePlot1 <- reactive({
    ggplot(df(), aes(x = date, y = weight, color = rat_id)) +
      geom_line() +
      geom_point() +
      facet_wrap(~rat_id) +
      labs(title = 'Fig. 1: Recorded rats weights', x = 'Date', y = 'Weight (g)') +
      
      # Show only the line for weight_loss_17
      geom_hline(data = df_endpoints() %>% filter(weight_loss_perc == 'weight_loss_17'), 
                 aes(yintercept = threshold_weight, 
                     linetype = "17 (humane endpoint)"), 
                 color = '#4A3734', show.legend = TRUE) +
      
      scale_linetype_manual(name = "Weight loss (%)", 
                            values = c("17 (humane endpoint)" = "dashed")) +
      scale_color_manual(values = subsampled_palette(), guide = "none") + 
      scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
      theme_2
    
  })
  # Display plot1 in the UI
  output$plot1 <- renderPlot({
    print(reactivePlot1())
  })
  
  
  
  
  # Dynamically adjust the size of the download for Fig. 1
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Fig_1_recorded_weights_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p1 <- isolate(reactivePlot1())
      g <- ggplot_build(p1)  # Get the plot build object
      
      # Extract the dimensions from the plot (adjust these values as needed)
      plot_width <- length(unique(df()$rat_id)) * 2  # Adjust based on the number of facets
      plot_height <- 6  # Fixed or can also adjust based on factors
      
      pdf(file, width = plot_width, height = plot_height)  # Use dynamic size for the plot
      print(p1)
      dev.off()  # Close PDF device
    }
  )
  
  
  # Display plot2 (Fig. 2) in the UI
  reactivePlot2 <- reactive({
    ggplot(plot_data(), aes(x = date, y = weight)) +
      geom_line(aes(col = rat_id), show.legend = FALSE) +  # Hide rat_id color from the legend
      geom_point(aes(col = rat_id), show.legend = FALSE) + # Hide rat_id color from the legend
      geom_line(aes(y = predicted, col = rat_id), linetype = "dashed", show.legend = FALSE) +  # Predicted data, hide its legend
      facet_wrap(~rat_id, scale = 'free_y') +
      
      geom_hline(data = df_endpoints(), 
                 aes(yintercept = threshold_weight, col = factor(weight_loss_perc)), 
                 linetype = "dashed") +
      
      scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
      labs(title = 'Fig. 2: Rats weights (with predictions)', x = 'Date', y = 'Weight (g)') +
      
      # Manually set the colors for the percentage reductions with your custom palette
      scale_color_manual(name = "Weight loss (%)", 
                         values = c('weight_loss_13' = '#EF2509', 
                                    'weight_loss_15' = '#9E473B', 
                                    'weight_loss_17' = '#4A3734'),
                         
                         labels = c('weight_loss_13' = '13', 
                                    'weight_loss_15' = '15', 
                                    'weight_loss_17' = '17 (humane endpoint)')) +
      
      
      theme_2
    
  })
  output$plot2 <- renderPlot({
    print(reactivePlot2())
  })
  
  
  
  # Dynamically adjust the size of the download for Fig. 2
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Fig_2_predicted_weights_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p2 <- isolate(reactivePlot2())
      g <- ggplot_build(p2)  # Get the plot build object
      
      # Extract the dimensions from the plot (adjust based on the number of facets, etc.)
      plot_width <- length(unique(df()$rat_id)) * 2  # Adjust based on the number of facets
      plot_height <- 6  # Fixed or can also adjust based on factors
      
      pdf(file, width = plot_width, height = plot_height)  # Use dynamic size for the plot
      print(p2)
      dev.off()  # Close PDF device
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)