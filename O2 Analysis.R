library(shiny)
library(plotly)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Interactive O2 Current v PPO2 Graph"),
  sidebarLayout(
    sidebarPanel(
      selectInput("diveSelect", "Choose Dive:", choices = unique(dives2$Dive.Number)),
    ),
    mainPanel(
#      plotOutput("O2Plot")
      plotlyOutput("O2Plot")
    )
  )
)

server <- function(input, output, session) {
#  output$O2Plot <- renderPlot({
  output$O2Plot <- renderPlotly({
      selectedDive <- dives2[dives2$Dive.Number == input$diveSelect,]
    ggplot(selectedDive) +
      geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 1 (mV)`, color="O2 Sensor 1")) +
      geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 2 (mV)`, color="O2 Sensor 2")) +
      geom_line(aes(x = `Average PPO2`, y = `External O2 Sensor 3 (mV)`, color="O2 Sensor 3")) +
      scale_colour_manual(" ", values=c("O2 Sensor 1" = "red",
                                        "O2 Sensor 2" = "blue",
                                        "O2 Sensor 3" = "green")) +
      ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title= "Current As a Function of PPO2",
         y="mV", x = "PPO2")
  })
}



shinyApp(ui = ui, server = server)
