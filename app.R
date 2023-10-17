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
              tabPanel("Elicitating the Prior", page_elicitprior),
              tabPanel("Sampling from the Prior", page_priorsample),
              tabPanel("Graph of the Prior", page_priorgraph),
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
  const_list = reactive({create_necessary_vector(input$const_s)})
  m1_list = reactive({create_necessary_vector(input$m1)})
  m2_list = reactive({create_necessary_vector(input$m2)})
  
  alpha01_list = reactive({create_necessary_vector(input$alpha01)})
  alpha02_list = reactive({create_necessary_vector(input$alpha02)})
  alpha01_list_ver2 = reactive({create_necessary_vector(input$alpha01_ver2)})
  alpha02_list_ver2 = reactive({create_necessary_vector(input$alpha02_ver2)})
  

  prior_elicitation_values = reactive({
    prior_elicitation(gamma = input$virtual_uncertainty, 
                      m1 = m1_list(), 
                      m2 = m2_list(), 
                      const = const_list(), 
                      s1 = FALSE, s2 = FALSE)
  })
  
  prior_sample_values = reactive({
    if(input$priorsample_use == "y"){
      sample_multiple_prior(
        n = input$prior_bigN,
        alpha01 = prior_elicitation_values()$alphas, 
        alpha02 = prior_elicitation_values()$betas, 
        mu_0 = prior_elicitation_values()$mu0, 
        sigma_0 = prior_elicitation_values()$sigma0
      )
    } else if (input$priorsample_use == "n"){
      sample_multiple_prior(
        n = input$prior_bigN,
        alpha01 = alpha01_list(), 
        alpha02 = alpha02_list(), 
        mu_0 = input$mu_0, 
        sigma_0 = input$sigma_0
      )
    }
  })
  
  output$elicit_prior_computation = renderPrint({
    prior_elicitation_values()
  })
  
  output$sample_prior_computation = renderPrint({
    list(
      "mu" = head(prior_sample_values()$mu, 10),
      "sigma" = head(prior_sample_values()$sigma, 10)
    )
  })
  
  # downloading the data
  output$priorsample_download_mu = downloadHandler(
    filename = "priorsample_mu.csv",
    content = function(file) {
      write.csv(prior_sample_values()$mu, file, row.names = FALSE)
    }
  )
  
  output$priorsample_download_sigma = downloadHandler(
    filename = "priorsample_sigma.csv",
    content = function(file) {
      write.csv(prior_sample_values()$sigma, file, row.names = FALSE)
    }
  )
  
  post_sample_values = reactive({
    if(input$postsample_use == 1){ # input values
      sample_post(alpha01 = alpha01_list_ver2(), 
                       alpha02 = alpha02_list_ver2(), 
                       n = input$post_n, N = input$post_bigN, 
                       mu_0 = input$mu_0_ver2, 
                       sigma_0 = input$sigma_0_ver2)
    } else if (input$postsample_use == 2){ # same as elicit
      sample_post(alpha01 = prior_elicitation_values()$alphas, 
                       alpha02 = prior_elicitation_values()$betas,
                       n = input$post_n, N = input$post_bigN, 
                       mu_0 = prior_elicitation_values()$mu0, 
                       sigma_0 = prior_elicitation_values()$sigma0)
    } else if (input$postsample_use == 3){ # same as prior
      sample_post(alpha01 = alpha01_list(), 
                       alpha02 = alpha02_list(), 
                       n = input$post_n, N = input$post_bigN, 
                       mu_0 = input$mu_0, 
                       sigma_0 = input$sigma_0)
    }
  })
  
  output$sample_post_computation = renderPrint({
    list(
      "mu" = head(post_sample_values()$mu, 5),
      "sigma" = head(post_sample_values()$sigma, 5),
      "k_zeta" = post_sample_values()$k_zeta[1:25]
    )
  })
  
  output$postsample_download_mu = downloadHandler(
    filename = "postsample_mu.csv",
    content = function(file) {
      write.csv(post_sample_values()$mu, file, row.names = FALSE)
    }
  )
  output$postsample_download_sigma = downloadHandler(
    filename = "postsample_sigma.csv",
    content = function(file) {
      write.csv(post_sample_values()$sigma, file, row.names = FALSE)
    }
  )
  output$postsample_download_k_zeta = downloadHandler(
    filename = "postsample_k_zeta.csv",
    content = function(file) {
      write.csv(post_sample_values()$k_zeta, file, row.names = FALSE)
    }
  )
  
  
  #################################### graphing
  # this is for the GRAPH of the prior.
  output$sample_prior_graph = renderPlot({
    prior_mu_graph(
      prior_mu = prior_sample_values()$mu, 
      col_num = input$mu_col,
      colour_choice = c(convert_to_hex(input$prior_colour_hist),
                        convert_to_hex(input$prior_colour_line)),
      lty_type = as.numeric(input$prior_lty_type),
      transparency = input$prior_transparency)
  })
  
  # this is for the GRAPH of the posterior.
  output$sample_post_graph = renderPlot({
    prior_mu_graph(
      prior_mu = post_sample_values()$mu, 
      col_num = input$mu_col,
      colour_choice = c(convert_to_hex(input$prior_colour_hist),
                        convert_to_hex(input$prior_colour_line)),
      lty_type = as.numeric(input$prior_lty_type),
      transparency = input$prior_transparency)
  })
  
}

shinyApp(ui, server)
