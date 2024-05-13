################################################################
# TODO LIST                                                    #
################################################################

# 1. re-write some documentation for the new functions
# 2. re-check that all of the "download" options are working
# 3. delete all old code
# 4. deploy
# 5. contact claire and make sure the newest version is working
# 6. options for downloading the plot
# 7. add column support

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
library(DT) # for tables

# For the functions
library(dplyr) 
library(expm) # used for the onion method 
library(stringr)
library(varhandle)
#library(powerplus) # May be useful for matrix multiplication
library(MASS)
library(matrixcalc)
library(stats)
library(pracma)
library(reticulate)

# Globally setting the spinner colour and type
options(spinner.type = 8, spinner.color = "#6990EE")

# Removing scientific notation
options(scipen = 999)

# Accessing other R-codes
#source("routes.R") # Note: re-add later once we have an abundance of pages.
source("./pages/home.R")
source("./pages/contact.R")
source("./pages/file_upload.R")
#source("./pages/sampling/Description.R")

source("./pages/mutivariate_normal/PagePriorElicitations.R")
source("./pages/mutivariate_normal/PagePriorSampling.R")
source("./pages/mutivariate_normal/PagePosteriorComputations.R")

#source("./pages/algorithm_description.R")

source("./functions/HelperFunctions.R")
source("./functions/PriorElicitation.R")
source("./functions/PriorSample.R")
source("./functions/PosteriorComputations.R")

################################################################
# FRONTEND                                                     #
################################################################

# will remove the below once we work on the multivariate normal regression
page_ph1 = div(
  titlePanel("Place holder page"),
  p("WEBSITE UNDER CONSTRUCTION!!"),
)

page_ph2 = div(
  titlePanel("Place holder page"),
  p("WEBSITE UNDER CONSTRUCTION!!"),
)
  
ui = navbarPage(
  title = "Prior Elicitation and Posterior Calculations for Linear Models",
  tabPanel("Home", page_home),
  navbarMenu(
    "Multivariate Normal",
    tabPanel("Prior Elicitation", page_prior_elicit),
    tabPanel("Sampling from the Prior", page_prior_sample),
    tabPanel("Posterior Computations", page_sampling)
  ),
  navbarMenu(
    "Multivariate Normal Regression",
    tabPanel("placeholder1", page_ph1),
    tabPanel("placeholder2", page_ph2)
  ),
  tabPanel("Contact & Credits", page_contact),
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
  source(file.path("server", "ServerPriorInstructions.R"),  local = TRUE)$value
  source(file.path("server", "ServerPriorElicitation.R"),  local = TRUE)$value
  source(file.path("server", "ServerPriorSampling.R"),  local = TRUE)$value
  source(file.path("server", "ServerPosteriorComputations1.R"),  local = TRUE)$value
  source(file.path("server", "ServerPosteriorComputations2.R"),  local = TRUE)$value
  #source(file.path("server", "ServerPosteriorComputations3.R"),  local = TRUE)$value
  
  observe(addHoverAnim(session, 'AnnaImg', 'rubberBand'))
  observe(addHoverAnim(session, 'MikeImg', 'tada'))
  observe(addHoverAnim(session, 'LuaiImg', 'flip'))
}

shinyApp(ui, server)
