page_home = div(
  titlePanel("Home Page"),
  p("WEBSITE UNDER CONSTRUCTION!!"),
  
  disconnectMessage(
    text = "Your session timed out, reload the application.",
    refresh = "Reload now",
    background = "#4183e0",
    colour = "white",
    overlayColour = "#0c489c",
    overlayOpacity = 0.3,
    refreshColour = "#0c0c70"
  ),
  
  actionButton("disconnect", "Disconnect the app")
)

