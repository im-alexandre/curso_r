library('shiny')
library(bslib)

ui <- page_sidebar(
  tittle = "Meu app!",
  sidebar = sidebar(
    numericInput(
      inputId = 'bins',
      label = 'Number of bins:',
      min = 1,
      max = 10,
      value = 40
    )
  ),
  plotOutput(outputId = 'distPlot')
)

server <- function(input, output) {
  output$distPlot = renderPlot({
    x <- faithful$waiting
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    print(bins)
    hist(x, breaks = bins, col = '#007000', border = 'red',
         xlab = 'Esperando tempo para a próxima explosão',
         main = 'Histograma de tempos de espera')
  })
}

shinyApp(ui = ui, server = server)
