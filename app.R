################################################################
# LIBRARIES                                                    #
################################################################

# For Shiny & website support
library(shiny)
library(shinycssloaders) # for loading screens

# For the functions
library(expm) # used for the onion method 
library(stringr)
library(varhandle)
#library(powerplus) # May be useful for matrix multiplication
library(MASS)
library(matrixcalc)
library(stats)

# Globally setting the spinner colour and type # TODO: find a new one
options(spinner.type = 8, spinner.color = "#6990EE")

# Removing scientific notation
options(scipen = 999)

# Accessing other R-codes
#source("routes.R") # Note: re-add later.
source("./pages/sampling/samplepriorposterior.R")
source("./pages/algorithm_description.R")
source("./pages/contact.R")
source("./pages/home.R")

source("./functions/SamplePrior.R")
source("./functions/HelperFunctions.R")
source("./functions/HelperFunctions.R")

################################################################
# FRONTEND                                                     #
################################################################

# still a placeholder before adding a true home page.
page_home = div(
  titlePanel("Home Page"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_samplingdescription),
              tabPanel("Sampling from the Prior", page_priorsample),
              tabPanel("Sampling from the Posterior", page_posteriorsample)
  )
)

ui = navbarPage(title = "Importance Sampling for Multivariate Normal Calculations",
                tabPanel("Home", page_home),
                tabPanel("Algorithm", page_algorithm),
                tabPanel("Contact", page_contact),
                id = "navbarID",
                theme = shinythemes::shinytheme("journal"), # may want to change theme
)

################################################################
# BACKEND                                                      #
################################################################

server = function(input, output, session) {
  # convert the inputs into vectors to be used for computations
  alpha01_list = reactive({create_necessary_vector(input$alpha01)})
  alpha02_list = reactive({create_necessary_vector(input$alpha02)})
  
  output$sample_prior_computation = renderPrint({
    sample_prior(alpha01 = alpha01_list(), 
                 alpha02 = alpha02_list(), 
                 mu_0 = input$mu_0, 
                 sigma_0 = input$sigma_0)
  })
  
  output$sample_posterior_computation = renderPrint({
    sample_posterior(alpha01 = alpha01_list(), 
                     alpha02 = alpha02_list(), 
                     n = input$n, 
                     N = input$bigN, 
                     mu_0 = input$mu_0, 
                     sigma_0 = input$sigma_0)
  })
}

shinyApp(ui, server)
