################################################################
# SAMPLING FROM THE PRIOR                                      #
################################################################

page_priorsample = div(
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(inputId = "submit_sample_prior", label = "Submit Data"),
      
      numericInput(inputId = "prior_bigN",
                   label = 'Insert N, the monte carlo sample size.',
                   value = 100),
      
      selectInput(inputId = "priorsample_use", 
                  label = 'Do you want to use the values from the Prior Elicitation calculations?', 
                  choices = list("Yes" = "y", "No" = "n"), 
                  selected = "n"
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use == 'yes'",
        p("The values from the previous section will be used!")
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use == 'n'",
      
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "alpha01",
              label = "$\\alpha_{011}, ..., \\alpha_{01p}$",
              value = "1, 1, 1, 1, 1"),
            textInput(
              inputId = "alpha02",
              label = "$\\alpha_{021}, ..., \\alpha_{02p}$",
              value = "1, 1, 1, 1, 1"),
          )
        )),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            numericInput(inputId = "mu_0",
                         label = 'Insert $\\mu_{0}$',
                         value = 0),
            numericInput(inputId = "sigma_0",
                         label = 'Insert $\\sigma_{0}$',
                         value = 1),
          )
        )),
      ),
      
    ),
    mainPanel(
      p("The values shown display limited information, but can be downloaded:"),
      downloadButton("priorsample_download_mu", "Download $\\mu$"),
      downloadButton("priorsample_download_sigma", "Download $\\Sigma$"),
      downloadButton("priorsample_download_R", "Download $R$"),
      p(""),
      withSpinner(verbatimTextOutput("sample_prior_computation"))
    ),
  )
)

################################################################
# USING IMPORTANCE SAMPLING FOR THE POSTERIOR                  #
################################################################

page_posteriorsample = div(

  titlePanel("Integrating with Respect to the Posterior"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      fileInput(inputId = "sample_post_Y", 
                label = "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      actionButton(inputId = "submit_sample_post", label = "Submit Data"),
      
      numericInput(inputId = "post_bigN",
                   label = 'Insert N, the monte carlo sample size',
                   value = 1000),
      
      selectInput(inputId = "postsample_use", 
                  label = 'Which kind of values do you want to use?', 
                  choices = list("Input values" = 1, 
                                 "Same values as elicitation" = 2,
                                 "Same values as prior" = 3), 
                  selected = 1),
      
      conditionalPanel(
        condition = "input.postsample_use == 2",
        p("The values from the elicitation of the prior will be used!")
      ),
      conditionalPanel(
        condition = "input.postsample_use == 3",
        p("The values inputted in the sampling of the prior will be used!")
      ),
      
      conditionalPanel(
        condition = "input.postsample_use == 1",

        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "alpha01_ver2",
              label = "$\\alpha_{011}, ..., \\alpha_{01p}$",
              value = "1, 1, 1, 1, 1"),
            textInput(
              inputId = "alpha02_ver2",
              label = "$\\alpha_{021}, ..., \\alpha_{02p}$",
              value = "1, 1, 1, 1, 1"),
          )
        )),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            numericInput(inputId = "mu_0_ver2",
                         label = 'Insert $\\mu_{0}$',
                         value = 0),
            numericInput(inputId = "sigma_0_ver2",
                         label = 'Insert $\\sigma_{0}$',
                         value = 1),
          )
        )),
      ),
      
    ),
    mainPanel(
      file_upload_example,
      downloadButton(outputId = "sample_post_example_file", label = "Download Sample"),
      p("The values shown display limited information, but can be downloaded. (Please 
        press the buttons after you submit, or nothing will load.)"),
      downloadButton("postsample_download_mu", "Download $\\mu$"),
      downloadButton("postsample_download_sigma", "Download $\\Sigma$"),
      downloadButton("postsample_download_k_zeta", "Download $K(\\zeta)$"),
      p(""),
      withSpinner(verbatimTextOutput("sample_post_computation"))
    ),
  )
)

################################################################
# PORTION FOR GRAPH BUILDING                                   #
################################################################

page_priorgraph = div(
  titlePanel("Plots for $\\mu$"), 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "mu_col", 
                   label = 'The column of $\\mu$ used to generate the graph.',
                   value = 1),
      sliderInput(
        inputId = "graph_delta",
        label = "The meaningful difference (length of the bins)",
        min = 0.01, max = 1, value = 0.1,
      ),
      
      #numericInput(inputId = "graph_delta",
      #             label = 'The meaningful difference (length of the bins).',
      #             value = 0.1),
      textInput(inputId = "prior_colour_hist",
                label = 'Input the colour of the histogram',
                value = "6699FF"
      ), 
      textInput(inputId = "prior_colour_line",
                label = 'Input the colour of the line',
                value = "FF6666"
      ), 
      selectInput(inputId = "prior_lty_type", 
                  label = 'Select a line type', 
                  choices = list("0" = 0, "1" = 1, "2" = 2, 
                                 "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                  selected = 2
      ),
      sliderInput(inputId = "prior_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.2
      ), 
      
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("33%", "33%"), 
            withSpinner(plotOutput("sample_prior_graph")), 
            withSpinner(plotOutput("sample_post_graph")),
            withSpinner(plotOutput("sample_rbr_graph"))
          )
        ),
      ),
    ),
  ),
)

################################################################
# TAB ORGANIZATION                                             #
################################################################

page_sampling = div(
  titlePanel("Sampling"),
  tabsetPanel(type = "tabs",
              tabPanel("Sampling from the Prior", page_priorsample),
              tabPanel("Sampling from the Posterior", page_posteriorsample),
              tabPanel("Graphs", page_priorgraph),
              tabPanel("Description", page_samplingdescription)
  )
)



