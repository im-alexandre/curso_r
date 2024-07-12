# Instale o pacote shiny se ainda não estiver instalado
# install.packages("shiny")

# Carregue o pacote shiny
library(shiny)

# Defina a interface do usuário
ui <- fluidPage(
  # Crie uma estrutura de navegação
  navbarPage("Aplicação de Comparação", id = "navBar",
             tabPanel("Inserir Alternativas",
                      fluidRow(
                        column(3, actionButton("addAlt", "+")),
                        column(3, actionButton("removeAlt", "-"))
                      ),
                      uiOutput("altInputs"),
                      actionButton("goToCompare", "OK")
             ),
             tabPanel("Comparar Alternativas",
                      uiOutput("compareUI")
             )
  )
)

# Defina a lógica do servidor
server <- function(input, output, session) {
  # Variável reativa para armazenar o número de alternativas
  numAlt <- reactiveVal(3)
  
  # Atualizar o número de alternativas
  observeEvent(input$addAlt, {
    numAlt(numAlt() + 1)
  })
  
  observeEvent(input$removeAlt, {
    if (numAlt() > 2) {
      numAlt(numAlt() - 1)
    }
  })
  
  # Gerar campos de entrada de texto dinamicamente
  output$altInputs <- renderUI({
    n <- numAlt()
    altInputs <- lapply(1:n, function(i) {
      textInput(paste0("alt", i), paste("Alternativa", i, ":"), value = paste0("ALT", i))
    })
    do.call(tagList, altInputs)
  })
  
  # Dataframe para armazenar os resultados
  results <- reactiveVal(data.frame())
  
  # Observe o clique do botão para mudar a aba
  observeEvent(input$goToCompare, {
    updateTabsetPanel(session, "navBar", selected = "Comparar Alternativas")
  })
  
  # Renderiza a UI para comparação
  output$compareUI <- renderUI({
    n <- numAlt()
    altNames <- sapply(1:n, function(i) input[[paste0("alt", i)]])
    sliders <- lapply(1:(n-1), function(i) {
      lapply((i+1):n, function(j) {
        sliderInput(paste0("comp", i, j), paste(altNames[i], "vs", altNames[j]), min = 1, max = 5, value = 3)
      })
    })
    sliders <- unlist(sliders, recursive = FALSE)
    fluidPage(
      titlePanel("Comparação de Alternativas"),
      sidebarLayout(
        sidebarPanel(
          do.call(tagList, sliders),
          actionButton("save", "Salvar Comparações")
        ),
        mainPanel(
          tableOutput("results")
        )
      )
    )
  })
  
  # Função para salvar as comparações
  observeEvent(input$save, {
    n <- numAlt()
    altNames <- sapply(1:n, function(i) input[[paste0("alt", i)]])
    new_results <- matrix(NA, nrow = n, ncol = n, dimnames = list(altNames, altNames))
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        comp <- input[[paste0("comp", i, j)]]
        new_results[i, j] <- comp
        new_results[j, i] <- comp
      }
    }
    results(as.data.frame(new_results))
  })
  
  # Exibir os resultados na tabela
  output$results <- renderTable({
    results()
  }, rownames = TRUE)
}

# Crie a aplicação Shiny
shinyApp(ui = ui, server = server)
