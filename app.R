################################################################
# TODO LIST                                                    #
################################################################

# 1. make it so there's latex as the dropdown option in the relative belief instances
# 2. check to see if there's anywhere else that requires latex
# 3. inplement the relative belief instances for the contrasts (see if Luai has checked this)

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
#library(data.table) # to transpose the table 
library(plyr)
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

source("./pages/multivariate_normal/PagePriorElicitations.R")
source("./pages/multivariate_normal/PagePosteriorComputations.R")
source("./pages/linear_regression/PageQualitative1.R")
source("./pages/linear_regression/PageQualitative2.R")
source("./pages/linear_regression/PageQualitative3.R")

#source("./pages/algorithm_description.R")

source("./functions/HelperFunctions.R")
source("./functions/BetaText.R")
source("./functions/multivariate_normal/PriorElicitation.R")
source("./functions/multivariate_normal/PriorSample.R")
source("./functions/multivariate_normal/PosteriorComputations.R")
source("./functions/linear_regression/QualComputations.R")

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
    tabPanel("Prior Elicitation and Sampling from the Prior", page_prior_elicit),
    tabPanel("Posterior Computations", page_sampling)
  ),
  navbarMenu(
    "Multivariate Normal Regression",
    tabPanel("Qualitative Factors Only", page_qualitative_reg),
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
  
  options(shiny.maxRequestSize=100*1024^2)
  
  observeEvent(input$disconnect, {
    session$close()
  })
  
  path1 = "server/multivariate_normal"
  path2 = "server/linear_regression"
  
  # convert the inputs into vectors to be used for computations
  source(file.path("server", "ServerPriorInstructions.R"),  local = TRUE)$value
  source(file.path(path1, "ServerPriorElicitation.R"),  local = TRUE)$value
  source(file.path(path1, "ServerPriorSampling.R"),  local = TRUE)$value
  source(file.path(path1, "ServerPosteriorComputations1.R"),  local = TRUE)$value
  source(file.path(path1, "ServerPosteriorComputations2.R"),  local = TRUE)$value
  source(file.path(path1, "ServerPosteriorComputations3.R"),  local = TRUE)$value
  source(file.path(path2, "ServerQualitative1.R"),  local = TRUE)$value
  source(file.path(path2, "ServerQualitative2.R"),  local = TRUE)$value
  source(file.path(path2, "ServerQualitative3.R"),  local = TRUE)$value
  
  observe(addHoverAnim(session, 'AnnaImg', 'rubberBand'))
  observe(addHoverAnim(session, 'MikeImg', 'tada'))
  observe(addHoverAnim(session, 'LuaiImg', 'flip'))
}

shinyApp(ui, server)
