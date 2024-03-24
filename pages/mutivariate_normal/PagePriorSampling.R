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

################################################################
# OLD!!                                                        #
################################################################

page_sample_hist = div(
  # NOTE: this is the old example that isnt being used anymore. It is kept here
  # just in case.
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(inputId = "submit_sample_prior", 
                   label = "Submit Data (for the effective range)"),
      
      numericInput(inputId = "prior_seed",
                   label = "Insert the seed",
                   value = 1),
      
      numericInput(inputId = "prior_sample_bigN",
                   label = 'Insert the Monte Carlo sample size',
                   value = 1000),
      
      numericInput(inputId = "prior_sample_m",
                   label = 'Insert the number of desired subintervals for the effective range',
                   value = 50),
      
      p("Below is for denoting the smaller and larger quantiles for computing the effective range."),
      
      fluidRow(box(width = 12,
                   splitLayout(
                     numericInput(inputId = "prior_sample_small_quantile", 
                                  label = "Small Quantile", value = 0.005),
                     numericInput(inputId = "prior_sample_large_quantile", 
                                  label = "Large Quantile", value = 0.995),
                   )
      )),
      
      numericInput(inputId = "prior_sample_col", 
                   label = 'Which $\\mu_{i}$ for the graph?',
                   value = 1),
      
      colourInput(inputId = "prior_sample_colour",
                  label = 'Input colour of the graph',
                  value = "FF00FF"), 
      
      selectInput(inputId = "prior_sample_lty", 
                  label = 'Select line type of the graph', 
                  choices = list("0" = 0, "1" = 1, "2" = 2, 
                                 "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                  selected = 2),
      
    ),
    mainPanel(
      
      downloadButton(outputId = 'download_prior_sample', 
                     label = 'Download Values'),
      #downloadButton(outputId = 'download_prior_sample_sigma', 
      #               label = 'Download $\\Sigma$'),
      # not added below until we get confirmation
      #downloadButton(outputId = 'download_prior_elicit_variance', label = 'Download variance'),
      #downloadButton(outputId = "download_prior_sample_correlation", 
      #               label = "Download Correlation Matrix"),
      #withSpinner(plotOutput("prior_sample_histogram")),
      
      withSpinner(verbatimTextOutput("prior_sample_sample")),
      
      withSpinner(verbatimTextOutput("prior_sample_delta")),
      
      fluidRow(
        column(4,
               fluidRow(box(width = 12,
                            splitLayout(
                              numericInput(inputId = "prior_sample_xlim_min", 
                                           label = "Lower x limit", value = -10),
                              numericInput(inputId = "prior_sample_xlim_max", 
                                           label = "Upper x limit", value = 10),
                            )
               )),
               #sliderInput(inputId = "prior_sample_delta",
               #            label = "Length of the bins",
               #           min = 0.01, max = 1, value = 0.1),
        ),
        column(4, 
               sliderInput(inputId = "prior_sample_smoother", 
                           label = "# of Average Points (Smoother)", 
                           min = 1, max = 15, value = 3, step = 2),
        ),
        column(4, 
               sliderInput(inputId = "prior_sample_transparency", 
                           label = "Scale for colour transparency",
                           min = 0, max = 1, value = 0.2), 
        )
      ),
    )
  )
)

