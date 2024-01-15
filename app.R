################################################################
# LIBRARIES                                                    #
################################################################

# For Shiny & website support
library(shiny)  
library(shinycssloaders) # for loading screens
library(latex2exp) # for latex within graphs
library(shinydashboard)
library(shinyanimate)
library(shinyalert)
library(colourpicker)
library(shinydisconnect)

# For the functions
library(expm) # used for the onion method 
library(stringr)
library(varhandle)
#library(powerplus) # May be useful for matrix multiplication
library(MASS)
library(matrixcalc)
library(stats)

# Globally setting the spinner colour and type
options(spinner.type = 8, spinner.color = "#6990EE")

# Removing scientific notation
options(scipen = 999)

# Accessing other R-codes
#source("routes.R") # Note: re-add later once we have an abundance of pages.
source("./pages/home.R")
source("./pages/contact.R")
source("./pages/file_upload.R")
source("./pages/sampling/Description.R")
source("./pages/sampling/PagePriorElicitations.R")
source("./pages/sampling/PageSamplePriorPosterior.R")
source("./pages/algorithm_description.R")

source("./functions/HelperFunctions.R")
source("./functions/PriorElicitation.R")
source("./functions/Sampling.R")

################################################################
# FRONTEND                                                     #
################################################################

ui = navbarPage(title = "Importance Sampling for Multivariate Normal Calculations",
                tabPanel("Home", page_home),
                tabPanel("Prior Elicitation", page_prior_elicit),
                tabPanel("Sampling", page_sampling),
                #tabPanel("Algorithm", page_algorithm),
                tabPanel("Contact", page_contact),
                id = "navbarID",
                theme = shinythemes::shinytheme("journal"), # may want to change theme
)

################################################################
# BACKEND                                                      #
################################################################

server = function(input, output, session) {
  
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # convert the inputs into vectors to be used for computations
  source(file.path("server", "ServerPriorElicitation.R"),  local = TRUE)$value
  source(file.path("server", "ServerSampling.R"),  local = TRUE)$value
  
  observe(addHoverAnim(session, 'AnnaImg', 'rubberBand'))
  observe(addHoverAnim(session, 'MikeImg', 'tada'))
  observe(addHoverAnim(session, 'LuaiImg', 'flip'))
}

shinyApp(ui, server)
