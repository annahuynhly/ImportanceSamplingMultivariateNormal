page_home = div(
  titlePanel("Home Page"),
  
  p("The technical details underlying the algorithms implemented on this website can be found in the following paper:"),
  tags$div(
    tags$a(href="https://www.utstat.toronto.edu/mikevans/multnormregrees/paper.pdf", 
           "Prior Elicitation and Posterior Calculations for Linear Models with Normal Error"),
  ),
  br(),
  p("Note: currently the link does not work; it will be updated once the paper is finished."),
  
  disconnectMessage(
    text = "Your session timed out, reload the application.",
    refresh = "Reload now",
    background = "#4183e0",
    colour = "white",
    overlayColour = "#0c489c",
    overlayOpacity = 0.3,
    refreshColour = "#0c0c70"
  ),
  
  #actionButton("disconnect", "Disconnect the app")
)

