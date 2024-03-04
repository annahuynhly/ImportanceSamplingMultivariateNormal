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
  titlePanel("Sampling from the Priors on the $\\mu_{i}$"),
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
                   value = 400),
      
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
                   label = 'The column of $\\mu$ for the graph.',
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


################################################################
# TAB ORGANIZATION                                             #
################################################################

page_prior_sample = div(
  titlePanel("Sampling from the Prior"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_sample_description),
              tabPanel("Sample", page_sample_computation),
  )
)

