library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)  

superbowl_data <- read.csv("https://drive.google.com/uc?export=download&id=1zmgyI8kdfGBUESLCujvgXNHgOIF4FoQ-")

# Parse the Date column and extract the Year
superbowl_data <- superbowl_data %>%
  mutate(Date = mdy(Date),   # Convert Date to proper format
         Year = year(Date))  # Extract Year

# Define the UI
ui <- fluidPage(
  titlePanel("Super Bowl Statistics Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select a Team:",
                  choices = unique(c(superbowl_data$Winner, superbowl_data$Loser)),
                  selected = "New England Patriots"),  # Default team
      
      sliderInput("yearRange", "Select Year Range:",
                  min = min(superbowl_data$Year, na.rm = TRUE), 
                  max = max(superbowl_data$Year, na.rm = TRUE), 
                  value = c(2000, 2020), step = 1),
      
      checkboxGroupInput("metrics", "Metrics to Display:",
                         choices = list("Winning Points" = "Winner.Pts",
                                        "Losing Points" = "Loser.Pts"),
                         selected = "Winner.Pts"),  # Pre-select a metric
      
      hr(),
      helpText("Explore Super Bowl history by selecting a team, year range, and metrics.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Points Over Time", plotOutput("pointsPlot")),
        tabPanel("MVP Insights", verbatimTextOutput("mvpText")),
        tabPanel("Location Distribution", plotOutput("locationPlot"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on input
  filtered_data <- reactive({
    superbowl_data %>%
      filter((Winner == input$team | Loser == input$team) &
               Year >= input$yearRange[1] & 
               Year <= input$yearRange[2])
  })
  
  # Render the Points Over Time plot
  output$pointsPlot <- renderPlot({
    req(input$metrics)  # Ensure at least one metric is selected
    
    # Prepare the data for plotting
    data_long <- filtered_data() %>%
      pivot_longer(cols = input$metrics, names_to = "Metric", values_to = "Points")
    
    # Check if there is data to plot
    if (nrow(data_long) > 0) {
      ggplot(data_long, aes(x = Year, y = Points, color = Metric, group = Metric)) +
        geom_line(size = 1.2) + geom_point(size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Points Over Time for", input$team),
             x = "Year", y = "Points")
    } else {
      ggplot() +
        labs(title = "No Data Available for Selected Inputs", x = "", y = "") +
        theme_void()
    }
  })
  
  # Render MVP insights
  output$mvpText <- renderPrint({
    mvp_data <- filtered_data() %>%
      group_by(MVP) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    if (nrow(mvp_data) == 0) {
      print("No MVP data available for the selected inputs.")
    } else {
      print(mvp_data)
    }
  })
  
  # Render the Location Distribution plot
  output$locationPlot <- renderPlot({
    location_data <- filtered_data() %>%
      count(State) %>%
      arrange(desc(n))
    
    if (nrow(location_data) > 0) {
      ggplot(location_data, aes(x = reorder(State, -n), y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = paste("Super Bowl Locations for", input$team),
             x = "State", y = "Count")
    } else {
      ggplot() +
        labs(title = "No Data Available for Selected Inputs", x = "", y = "") +
        theme_void()
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)



