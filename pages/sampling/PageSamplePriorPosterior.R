################################################################
# SAMPLING FROM THE PRIOR                                      #
################################################################

page_priorsample_new = div(
  # note: this is a new way to sample. may need to replace the other page.
  titlePanel("Sampling from the Prior (MODIFIED)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(inputId = "submit_sample_prior_NEW", label = "Submit Data"),
      
      downloadButton(outputId = 'plot_prior_mu', label = 'Download Plot'),
      
      numericInput(inputId = "prior_bigN_NEW",
                   label = 'Insert N, the monte carlo sample size.',
                   value = 100),
      
      selectInput(inputId = "priorsample_use_NEW", 
                  label = 'Do you want to use the values from the Prior Elicitation calculations?', 
                  choices = list("Yes" = "y", "No" = "n"), 
                  selected = "n"),
      
      conditionalPanel(
        condition = "input.priorsample_use_NEW == 'yes'",
        p("The values from the previous section will be used!")
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use_NEW == 'n'",
        
        numericInput(inputId = "virtual_uncertainty_prior",
                     label = 'Insert the virtual uncertainty, $\\gamma$.',
                     value = 0.99),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "alpha01_prior",
              label = "$\\alpha_{011}, ..., \\alpha_{01p}$",
              value = "3.15, 3.15, 3.15"),
            textInput(
              inputId = "alpha02_prior",
              label = "$\\alpha_{021}, ..., \\alpha_{02p}$",
              value = "5.75, 5.75, 5.75"),
          )
        )),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "m1_prior",
              label = "$m_{11}, m_{12}, ..., m_{1p}$",
              value = "-5,-5,-5"),
            textInput(
              inputId = "m2_prior",
              label = "$m_{21}, m_{22}, ..., m_{2p}$",
              value = "5,5,5"),
          )
        )),
        
      ),
      
      numericInput(inputId = "prior_graph_num",
                   label = "The column of $\\mu$ used to generate the graph.",
                   value = 1)
      
    ),
    mainPanel(
      withSpinner(plotOutput(outputId = "sample_prior_computations_graph_NEW")), 
      #withSpinner(verbatimTextOutput("sample_prior_computation_NEW"))
      #fluidRow(
      #  splitLayout(
      #    cellWidths = c("50%", "50%"), 
      #    withSpinner(plotOutput(outputId = "sample_prior_computations_graph_NEW")), 
      #    withSpinner(verbatimTextOutput("sample_prior_computation_NEW"))
      #  ),
      #)
    ),
  )
)

################################################################
# USING IMPORTANCE SAMPLING FOR THE POSTERIOR                  #
################################################################

page_postsample_new = div(

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
                  label = 'Which values do you want to use?', 
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

        numericInput(inputId = "virtual_uncertainty_post",
                     label = 'Insert the virtual uncertainty, $\\gamma$.',
                     value = 0.99),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "alpha01_post",
              label = "$\\alpha_{011}, ..., \\alpha_{01p}$",
              value = "3.15, 3.15, 3.15"),
            textInput(
              inputId = "alpha02_post",
              label = "$\\alpha_{021}, ..., \\alpha_{02p}$",
              value = "5.75, 5.75, 5.75"),
          )
        )),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "m1_post",
              label = "$m_{11}, m_{12}, ..., m_{1p}$",
              value = "-5,-5,-5"),
            textInput(
              inputId = "m2_post",
              label = "$m_{21}, m_{22}, ..., m_{2p}$",
              value = "5,5,5"),
          )
        )),
      ),
      
      numericInput(inputId = "post_graph_num",
                   label = "The column of $\\mu$ used to generate the graph.",
                   value = 1),
      
      numericInput(inputId = "post_graph_delta",
                   label = "The meaningful difference (length of the bins)",
                   value = 0.05),
      
      sliderInput(inputId = "post_graph_smoother", 
                  label = "Number of Average Points (Smoother)", 
                  min = 1, max = 15, value = 3, step = 2)
      
    ),
    mainPanel(
      file_upload_example,
      downloadButton(outputId = "sample_post_example_file", label = "Download Sample"),
      p("The values shown display limited information, but can be downloaded. (Please 
        press the buttons after you submit, or nothing will load.)"),
      downloadButton(outputId = 'plot_post_mu', label = 'Download Plot'),
      downloadButton(outputId = "postsample_download_mu", label = "Download $\\mu$"),
      downloadButton(outputId = "postsample_download_xi", label = "Download $\\Xi$"),
      p(""),
      
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("60%", "40%"), 
                withSpinner(plotOutput("sample_post_graph")),
                withSpinner(verbatimTextOutput("sample_post_computation"))
          )
        ),
      ),
    ),
  )
)

################################################################
# PORTION FOR GRAPH BUILDING                                   #
################################################################

page_comparison_graphs = div(
  titlePanel("Plots for $\\mu$"), 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "comparison_mu_col", 
                   label = 'The column of $\\mu$ used to generate the graph.',
                   value = 1),
      
      sliderInput(inputId = "comparison_graph_delta",
                  label = "The meaningful difference (length of the bins)",
                  min = 0.01, max = 1, value = 0.1),
      
      sliderInput(inputId = "comparison_smoother", 
                  label = "Number of Average Points (Smoother)", 
                  min = 1, max = 15, value = 3, step = 2),
      
      selectInput(inputId = "comparison_modify_which",
                  label = 'Select line to modify',
                  choices = list("Prior" = 'prior', "Posterior" = 'post',
                                 "Relative belief ratio" = 'rbr'),
                  selected = 'prior'), 
      
      conditionalPanel(
        condition = "input.comparison_modify_which == 'prior'",
        textInput(inputId = "comparison_prior_col",
                  label = 'Input colour of the prior',
                  value = "FF007F"), 
        # NOTE: lty type not added yet.
        selectInput(inputId = "comparison_prior_lty", 
                    label = 'Select line type of the prior', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      conditionalPanel(
        condition = "input.comparison_modify_which == 'post'",
        textInput(inputId = "comparison_post_col",
                  label = 'Input colour of the posterior',
                  value = "FF00FF"), 
        selectInput(inputId = "comparison_post_lty", 
                    label = 'Select line type of the posterior', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      conditionalPanel(
        condition = "input.comparison_modify_which == 'rbr'",
        textInput(inputId = "comparison_rbr_col",
                  label = 'Input colour of the relative belief ratio',
                  value = "7F00FF"), 
        selectInput(inputId = "comparison_rbr_lty", 
                    label = 'Select line type of the relative belief ratio', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      
      sliderInput(inputId = "comparison_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.2
      ), 
      
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("sample_priorpost_graph")), 
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
              tabPanel("Sampling from the Prior", page_priorsample_new),
              tabPanel("Sampling from the Posterior", page_postsample_new),
              tabPanel("Comparison Plots for Mu", page_comparison_graphs),
              #tabPanel("Sampling from the Prior", page_priorsample),
              #tabPanel("Sampling from the Posterior", page_posteriorsample),
              #tabPanel("Graphs", page_priorgraph),
              tabPanel("Description", page_samplingdescription)
  )
)



