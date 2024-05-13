################################################################
# DESCRIPTION FOR THE SAMPLING FROM THE PRIOR                  #
################################################################

page_sample_description = div(
  titlePanel("Description"),
  p("Please refer to section 3.1 of the paper for this section."),
  p("under construction...")
)

################################################################
# SAMPLING FROM THE MU                                         #
################################################################

page_sample_computation = div(
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(inputId = "submit_prior_sampling", 
                   label = "Submit Data"),
      
      numericInput(inputId = "prior_seed",
                   label = "Insert the seed",
                   value = 1),
      
      numericInput(inputId = "prior_sample_bigN",
                   label = 'Insert the Monte Carlo sample size',
                   value = 20000),
      
    ),
    mainPanel(
      downloadButton(outputId = 'download_prior_sample', 
                     label = 'Download Values'),
      
      p("The downloaded values above are formatted as follows, where each row in the file 
        contains:"),
      p("$\\mu_{1}, \\mu_{2}, ..., \\mu_{p}, 
        \\frac{1}{\\sigma^{2}_{1}}, \\frac{1}{\\sigma^{2}_{2}}, \\frac{1}{\\sigma^{2}_{3}}, ..., 
        \\frac{1}{\\sigma^{2}_{p}}, 
        \\rho_{12}, \\rho_{13}, ..., \\rho_{1p}, \\rho_{23}, 
        \\rho_{24}, ..., \\rho_{2p}, ..., \\rho_{(p-1)(p)}$"),
      p("Here, the $\\sigma^{2}_{i}$ are the variances, the $\\rho_{ij}$ are the 
        correlations and the $\\mu_{i}$ are the means."),
      p("Below is the first few lines of the file and contains the first set of values 
        generated from the prior."),
      p("Below are used to view different columns within the dataframe."),
      actionButton('prior_prev_five', 'Previous Cols'),
      actionButton('prior_next_five', 'Next Cols'),
      withSpinner(DTOutput('prior_sample_table'))
      #withSpinner(verbatimTextOutput("prior_sample_sample"))
    )
  )
)


################################################################
# TAB ORGANIZATION                                             #
################################################################

page_prior_sample = div(
  titlePanel("Sampling from the Prior"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_sample_description),
              tabPanel("Sample", page_sample_computation)
  )
)
