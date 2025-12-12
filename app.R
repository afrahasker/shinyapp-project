library(shiny)
library(tidyverse)
library(nycflights13)

flights_weather <- flights |>
  left_join(weather, by = c("origin", "time_hour")) |>
  select(origin, dest, dep_delay, precip, wind_speed, visib) |>
  mutate(route = paste(origin, dest, sep = "-"))

compute_slopes <- function(var, origin_filter = NULL, dest_filter = NULL) {
  data <- flights_weather
  if (!is.null(origin_filter)) data <- data |> filter(origin %in% origin_filter)
  if (!is.null(dest_filter)) data <- data |> filter(dest %in% dest_filter)
  
  if (nrow(data) == 0) return(tibble(route = character(), slope = numeric(), 
                                     n_flights = integer()))
  
  data |>
    group_by(route) |>
    filter(n() > 5) |>
    summarise(
      slope = coef(lm(dep_delay ~ .data[[var]], data = cur_data()))[2],
      n_flights = n()
    ) |>
    filter(!is.na(slope)) |>
    arrange(desc(abs(slope)))
}


ui <- fluidPage(
  titlePanel("Route Sensitivity to Weather in NYC 2013"),
  sidebarLayout(
    sidebarPanel(
      selectInput("weather_var", "Weather Factor:", 
                  choices = c("Precipitation" = "precip", 
                              "Wind" = "wind_speed", 
                              "Visibility" = "visib")),
      selectInput("origin_filter", "Origin Airport:", choices = unique(flights$origin),
                  multiple = TRUE),
      selectInput("dest_filter", "Destination Airport:", choices = unique(flights$dest),
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("slope_plot"),
      tableOutput("slope_table")
    )
  )
)

server <- function(input, output) {
  
  output$slope_plot <- renderPlot({
    slopes <- compute_slopes(input$weather_var, input$origin_filter, input$dest_filter)
    
    if (nrow(slopes) == 0) {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No flights match the selected filters")
    } else {
      slopes[1:20, ] |>
        ggplot(aes(x = reorder(route, slope), y = slope)) + 
        geom_col(fill = "steelblue") +
        geom_text(aes(label = n_flights), hjust = -0.1, size = 4) +
        coord_flip() +
        labs(x = "Route", y = "Sensitivity (Slope)",
             title = paste("Top 20 Routes Most Sensitive to", input$weather_var),
             subtitle = "Numbers show total flights per route")
    }
  })
  
  
  output$slope_table <- renderTable({
    compute_slopes(input$weather_var, input$origin_filter, input$dest_filter) |> 
      head(20)
  })
}

shinyApp(ui, server)
