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

# Globally setting the spinner colour and type # TODO: find a new one
options(spinner.type = 8, spinner.color = "#6990EE")

# Removing scientific notation
options(scipen = 999)

# Accessing other R-codes
#source("routes.R") # Note: re-add later.
source("./Functions/SamplePrior.R")
source("./Functions/HelperFunctions.R")

################################################################
# FRONTEND                                                     #
################################################################

page_contact = div(
  mainPanel(
    titlePanel("Contact Page"),
    p("PLACEHOLDER. WILL ADD LATER.")
  )
)

page_algorithm = div(
  mainPanel(
    titlePanel("Algorithm Page"),
    p("PLACEHOLDER. WILL ADD LATER.")
  )
)

page_home = div(
  
  # Code for adding latex
  tags$head(
    tags$link(rel="stylesheet", 
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
    HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
  ),
  
  withMathJax(),
  # end of adding latex
  
  titlePanel("Home Page"),
  sidebarLayout(
    sidebarPanel(
      p("For the following, let:"),
      p("$\\frac{1}{\\delta_{i}} \\sim Gamma(\\alpha_{i}, \\beta_{i})$ for $i = 1, 2, .., p$ where $p$ is the sample size"),
      p("and $\\mu \\sim N_{p}(\\mu_{0}, \\sigma_{0}^{2} \\sum)$."),
      p(""),
      textInput(
        inputId = "alphas",
        label = "Insert the vector of $\\alpha_{1}, ..., \\alpha_{p}$.",
        value = "1, 1, 1, 1, 1"),
      textInput(
        inputId = "betas",
        label = "Insert the vector of $\\beta_{1}, ..., \\beta_{p}$",
        value = "1, 1, 1, 1, 1"),
      numericInput(inputId = "mu_0",
                   label = 'Insert $\\mu_{0}$',
                   value = 2),
      numericInput(inputId = "sigma_0",
                   label = 'Insert $\\sigma_{0}$',
                   value = 2),
    ),
    mainPanel(
      withSpinner(verbatimTextOutput("sample_prior_computation"))
    ),
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
  alphas_list = reactive({create_necessary_vector(input$alphas)})
  betas_list = reactive({create_necessary_vector(input$betas)})
  
  output$sample_prior_computation = renderPrint({
    sample_prior(alphas_list(), betas_list(), input$mu_0, input$sigma_0)
  })
}

shinyApp(ui, server)
