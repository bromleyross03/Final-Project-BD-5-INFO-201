library(shiny)
library(ggplot2)
library(plotly)
library(maps)

# Load your data
source("C:/Users/mabse/Downloads/BD-5 Data Cleaning Project (1).R") 

# Define UI
ui <- navbarPage("GDP and Happiness Analysis",
                 tabPanel("Intro",
                          fluidPage(
                            h2("Welcome to the GDP and Happiness Analysis"),
                            p("Explore the relationship between GDP and happiness scores across countries.")
                          )
                 ),
                 tabPanel("Scatter Plot",
                          fluidPage(
                            plotOutput("scatterPlot")
                          )
                 ),
                 tabPanel("Bar Chart",
                          fluidPage(
                            sidebarPanel(
                              sliderInput("barCount", "Number of countries:",
                                          min = 5, max = nrow(df_sorted), value = 10)
                            ),
                            mainPanel(
                              plotOutput("barChart")
                            )
                          )
                 ),
                 tabPanel("Interactive Map",
                          fluidPage(
                            selectInput("mapType", "Choose Map Type:", 
                                        choices = c("Global", "Northern Europe")),
                            plotlyOutput("interactiveMap")
                          )
                 ),
                 tabPanel("Summary",
                          fluidPage(
                            h2("Summary"),
                            p("Insights from the analysis.")
                          )
                 )
)

# Define server logic
server <- function(input, output) {
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(df_sorted, aes(x = gdp_fix, y = Happiness.score, color = Country)) +
      geom_point() +
      labs(title = "GDP vs Happiness Score", x = "GDP", y = "Happiness Score")
  })
  output$barChart <- renderPlot({
    top_countries <- df_sorted[order(df_sorted$Happiness.score, decreasing = TRUE), ][1:input$barCount, ]
    ggplot(top_countries, aes(x = reorder(Country, Happiness.score), y = Happiness.score)) +
      geom_bar(stat = "identity", fill = "blue") +  # Set bar color to blue
      coord_flip() +
      labs(title = "Top Countries by Happiness Score", x = "Country", y = "Happiness Score")
  })
  output$interactiveMap <- renderPlotly({
    # World map data
    world_map <- map_data("world")
    
    # Convert 'region' in world_map to character for matching
    world_map$region <- as.character(world_map$region)
    
    # Define Northern Europe countries
    northern_europe <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")
    
    # Filter map data based on user selection
    map_data <- if(input$mapType == "Northern Europe") {
      subset(world_map, region %in% northern_europe)
    } else {
      world_map
    }
    
    # Merge the map data with your data frame
    map_data <- merge(map_data, df_sorted, by.x = "region", by.y = "Country", all.x = TRUE)
    
    # Create the plots
    p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Happiness.score)) +
      geom_polygon() +
      scale_fill_viridis_c(option = "C", na.value = "grey") +
      labs(title = paste(input$mapType, "Happiness Score by Country")) +
      theme_void()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
