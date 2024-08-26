
library(shiny)
library(shinythemes)
library(shinyMatrix)

page_test = div(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton("show", "Show modal dialog"),
      
    ),
    mainPanel(
      #tableOutput("table")
      verbatimTextOutput("results")
    )
  )
)

ui = navbarPage(
  title = "test",
  tabPanel("matrix", page_test),
  id = "navbarID",
  theme = shinythemes::shinytheme("journal"), # may want to change theme
)

server = function(input, output, session) {
  # actual modal
  dataModal <- function(failed = FALSE) {
    modalDialog(
      
      matrixInput(
        inputId = "matrix",
        value = diag(5),
        class = "numeric",
        cols = list(
          names = TRUE,
          extend = TRUE,
          #editableNames = TRUE,
          delta = 2,
          delete = TRUE
        ),
        rows = list(
          names = TRUE,
          extend = TRUE,
          #editableNames = TRUE,
          delta = 1,
          delete = TRUE
        )
      ),
      
      footer = tagList(
        modalButton("Close"),
        actionButton("ok", "OK")
      )
    ) # modual dialogue
  } # end of modal
  
  # Show modal when button is clicked.
  observeEvent(input$show, {
    showModal(dataModal())
  })
  
  observeEvent(input$ok, {
    #output$table = renderTable(input$matrix, rownames = TRUE)
    
    output$results = renderPrint({
      input$matrix
    })
  })
}

shinyApp(ui = ui, server = server)

