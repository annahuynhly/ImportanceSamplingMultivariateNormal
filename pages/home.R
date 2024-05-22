page_home = div(
  titlePanel("Home Page"),
  
  p("The technical details underlying the algorithms implemented on this website can 
    be found in the following paper:"),
  tags$div(
    tags$a(href="https://www.utstat.toronto.edu/mikevans/multnormregress/paper.pdf", 
           "Prior Elicitation and Relative Belief Inferences for Linear Models"),
  ),
  br(),
  p("Note: to navigate the site, use the tags at the top of the page. Using the back button on 
    the browser takes you out of the site and you will have to start again."),
  
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

