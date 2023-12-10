library(shiny)
library(ggplot2)
library(plotly)
library(maps)

source("BD-5 Data Cleaning Project (1).R") 

ui <- navbarPage("GDP and Happiness Analysis",
                 tabPanel("Intro",
                          fluidPage(
                            h2("Welcome to the GDP and Happiness Analysis"),
                            p("The goal of this project was to discuss the presence of a correlation between GDP and Happiness scores for each country in the world. Many people claim that money can buy happiness, so the object of our app is to create a visual picture of the correlation between these two variables. The year that we are examining in this app is 2022, which is a special year in the timeline of the last decade. After the COVID-19 pandemic forced almost every country into lockdown, morale and the flow of money decreased significantly. Small and large businesses alike were forced to close their doors due to tanking sales. Social isolation caused waves of negative emotions, and the world saw a decrease in happiness scores. In 2022, social distancing and other COVID-19 precautions were finally lifted, and business began to thrive again and people became happier as they were able to visit friends and family once more. The reason we wanted to look at 2022, in particular, is that it can show us a unique comparison between GDP and happiness scores, and we can more easily determine whether a nation’s wealth has an impact on its citizen's happiness."),
                            p("GDP is calculated by adding up all of the money spent by consumers, businesses, and the government in a given period of time. GDP is similar to a report card that shows the total value of everything made and sold in a country over a certain time. In this project, we used a dataset from Kaggle.com that contained every nation’s GDP in billions for the years of 1999-2022. We cleaned this dataset to only include the data for 2022 so that we could analyze this year without having an extremely large data frame."),
                            p("Happiness scores are collected and stored in the World Happiness Report, which ranks countries based on variables like GDP per capita, social support, healthy life expectancy, freedom, generosity, and corruption. The data is collected from the Gallup World Poll surveys from each country where 1,000 individuals are asked to answer a series of questions on what they call the Cantril ladder, which asks respondents to think of a ladder, with the best possible life for them being a 10 and the worst possible life being a 0. The scores are averaged to create a happiness score for each country for each year. Then, they are ranked from highest to lowest average happiness score. We used a dataset also from Kaggle.com that compiled this data for the year 2022, and it includes columns that break down the happiness score with corresponding columns to the previously mentioned variables above."),
                            p("2022 was a year that affected the lives of billions of people, which is why we found it crucial to analyze. In the following pages, users can find interactive visualizations of these conjoined datasets that help us see just how GDP and happiness go together."),
                          )
                 ),
                 tabPanel("Scatter Plot",
                          fluidPage(
                            h2("GDP vs Happiness Score"),
                            plotOutput("scatterPlot"),
                            p("The above scatterplot visualizes the relationship between a country's GDP and its happiness score. The x-axis is GDP as measured in billions, and the y-axis is the happiness score reported for 2022. Each country listed in our data frame (111) is listed on the right of the scatterplot and has a corresponding dot on the scatterplot which is the intersection of GDP and happiness score."),
                            p("As you may be able to see, the United States and China are outliers in GDP but are not the highest on the list for happiness scores. This means that, while a country may have a high GDP, that does not necessarily make the country happier. However, we can also see that some of the unhappiest countries also have some of the lowest GDP’s as well, which could indicate some sort of correlation between the two."),
                          )
                 ),
                 tabPanel("Bar Chart",
                          fluidPage(
                            h2("Top Countries by Happiness Score"),
                            sidebarPanel(
                              sliderInput("barCount", "Number of countries:",
                                          min = 5, max = nrow(df_sorted), value = 10)
                            ),
                            mainPanel(
                              plotOutput("barChart"),
                            ),
                            p("The above bar chart ranks the countries in our dataset in order from highest to lowest happiness score. The slider allows you to see more or fewer countries based on your needs. This bar chart gives us a ranking system that allows us to see the most happy and least happy countries in the world."),
                            
                          )
                 ),
                 tabPanel("Interactive Map",
                          fluidPage(
                            h2("Global Happiness Score by Country"),
                            selectInput("mapType", "Choose Map Type:", 
                                        choices = c("Global", "Northern Europe")),
                            plotlyOutput("interactiveMap"),
                            p("This interactive map utilizes the drill down method to analyze which countries are happiest visually. Each regions fill color is associated with their happiness score, and the color key is visible on the right side of the map. It is based on the same statistics as the previous bar chart, but it helps visualize the regions and continents of the world and how happiness scores are spread geographically. We start with the world map and then zone in on the happiest region of the world, Northern Europe. In the Northern Europe map, one can see which of the countries rank highest amongst each other."),
                          )
                 ),
                 tabPanel("Summary",
                          fluidPage(
                            h2("Summary"),
                            p("In conclusion, we can see a correlation between happiness and the GDP of a country. In the graphs that we have made, we can see that Europe, which generally has a high GDP, has the highest levels of happiness. However, it is important to note that other factors have been taken into account. Happiness is not just based on GDP because the United States has the highest GDP yet it is not the highest on the happiness index."),
                            p("The year we had analyzed was 2022. This was an interesting year because it was right after the pandemic and caused a lot of economic chaos. However, this also caused a lot of people to be socially isolated and confined within their homes. Many people were feeling negative emotions during this time and governments were very controlling over their citizens. Scandinavia on the other hand was known for having very relaxed covid rules in comparison to other countries. Scandinavia also ranks the happiest in the world. This shows that how a government handled covid may have something to do with how happy the citizens are."),
                            p("To close off, GDP is a factor when looking at the happiness of a country. But it is only one factor of many. Just looking at the GDP of a country is not enough when making decisions such as “which country is the happiest”"),
                          )
                 )
)
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(df_sorted, aes(x = gdp_fix, y = Happiness.score, color = Country)) +
      geom_point() +
      labs(title = "GDP vs Happiness Score", x = "GDP", y = "Happiness Score")
  })
  output$barChart <- renderPlot({
    top_countries <- df_sorted[order(df_sorted$Happiness.score, decreasing = TRUE), ][1:input$barCount, ]
    ggplot(top_countries, aes(x = reorder(Country, Happiness.score), y = Happiness.score)) +
      geom_bar(stat = "identity", fill = "blue") + 
      coord_flip() +
      labs(title = "Top Countries by Happiness Score", x = "Country", y = "Happiness Score")
  })
  output$interactiveMap <- renderPlotly({
    world_map <- map_data("world")
    
    world_map$region <- as.character(world_map$region)
    
    northern_europe <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")
    
    if(input$mapType == "Northern Europe") {
      filtered_regions <- vector("list", length(northern_europe))
      for (i in 1:length(northern_europe)) {
        filtered_regions[[i]] <- subset(world_map, region == northern_europe[i])
      }
      map_data <- do.call(rbind, filtered_regions)
    } else {
      map_data <- world_map
    }
    
    map_data <- merge(map_data, df_sorted, by.x = "region", by.y = "Country", all.x = TRUE)
    
    p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Happiness.score)) +
      geom_polygon() +
      scale_fill_viridis_c(option = "C", na.value = "grey") +
      labs(title = paste(input$mapType, "Happiness Score by Country")) +
      theme_void()
    
    ggplotly(p, tooltip = c("region", "Happiness.score", "gdp_fix"))
  })
}

shinyApp(ui = ui, server = server)
