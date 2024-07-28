
page_qualitativeinputs = div(
  titlePanel("Computed Sufficient Statistics"),
  mainPanel(
    fluidRow(
      splitLayout(
        cellWidths = c("30%", "30%", "30%"), 
        h4("X Matrix"),
        h4("$\\underset{\\sim}{b}$ and $s^{2}$"),
        h4("C Matrix"),
      )
    ),
    
    fluidRow(
      splitLayout(
        cellWidths = c("30%", "30%", "30%"), 
        withSpinner(verbatimTextOutput(outputId = "qual_X_output")), 
        withSpinner(verbatimTextOutput(outputId = "qual_sufficient_statistics1")), 
        withSpinner(verbatimTextOutput(outputId = "qual_sufficient_statistics2"))
      )
    ),
    
  ),
)

page_qualitative_elicitsigma = div(
  titlePanel("Elicitation of the Prior on $\\sigma^{2}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("You must press \"submit\" for the information to load properly."),
      
      actionButton(inputId = "qual_submit_prior_elicit_sigma", label = "Submit Data"),
      
      downloadButton(outputId = 'qual_download_prior_elicit_sigma', label = 'Download Plot'),
      
      numericInput(inputId = "qual_virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      
      selectInput(inputId = "qual_elicit_sigma_input_type",
                  label = 'How do you want to enter the data?',
                  choices = list("Manually" = "manual",
                                 "Text file" = "txt",
                                 "CSV file" = "csv")),
      
      p("Below, insert $s_{1i} \\leq \\sigma \\cdot z_{0} \\leq s_{2i}$ 
        holds with virtual certainty."),
      
      conditionalPanel(
        condition = "input.qual_elicit_sigma_input_type == 'manual'",
        fluidRow(box(width = 12,
                     splitLayout(
                       numericInput(inputId = "qual_elicit_s1", label = "Insert $s_{1}$", value = "1"),
                       numericInput(inputId = "qual_elicit_s2", label = "Insert $s_{2}$", value = "7"),
                     )
        )),
        
        p("Bounds for the iterative processes determining values for $\\alpha_{01}$ and 
          $\\alpha_{02}$:"),
        
        fluidRow(box(width = 12,
                     splitLayout(
                       numericInput(inputId = "qual_alphalow", label = "Lower bound", value = "0"),
                       numericInput(inputId = "qual_alphaup", label = "Upper bound", value = "50"),
                     )
        )),
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.qual_elicit_sigma_input_type == 'txt'",
        
        p("Note: the $\\sigma$ is one dimensional. For an example of the file format, consult 
                      the description page."),
        
        fileInput(inputId = "qual_elicit_sigma_inputs_txt", 
                  label = "Choose .txt file", multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
        
      conditionalPanel(
        condition = "input.qual_elicit_sigma_input_type == 'csv'",
        
        p("Note: the $\\sigma$ is one dimensional. For an example of the file format, consult 
                      the description page."),
        
        fileInput(inputId = "qual_elicit_sigma_inputs_csv", 
                  label = "Choose .csv file", multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      selectInput(inputId = "qual_elicit_sigma_graph_type",
                  label = "Select which type of graph to view.",
                  choices = list("Prior Density of Sigma" = 1,
                                 "Prior Density of Sigma * z" = 2)),
      
    ),
    mainPanel(
      #withSpinner(verbatimTextOutput(outputId = "qual_debug"))
      tabPanel("elicitation of the prior graphs",
               fluidRow(
                 splitLayout(
                   cellWidths = c("50%", "50%"), 
                   withSpinner(plotOutput(outputId = "qual_elicit_prior_sigma")), 
                   withSpinner(tableOutput(outputId = "qual_prior_elicit_sigma_table"))
                 ),
               )
      )
    ),
  )
)

page_qualitative_elicitmu = div(
  titlePanel("Determining the Priors on the $\\beta_{j1,...,jm}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("You must press \"submit\" for the information to load properly."),
      
      actionButton(inputId = "qual_submit_prior_elicit_mu", label = "Submit Data"),
      
      downloadButton(outputId = 'qual_download_prior_elicit_mu_plot', label = 'Download Plot'),
      
      # TODO: implement later!
      selectInput(inputId = "qual_prior_mu_input_type",
                  label = 'How do you want to enter the data?',
                  choices = list("Manually" = "manual",
                                 "Text file" = "txt",
                                 "CSV file" = "csv")),
      
      conditionalPanel(
        condition = "input.qual_prior_mu_input_type == 'manual'",
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "qual_m1",
              label = "$m_{11}, m_{12}, ..., m_{1p}$",
              value = "0, 2, 4, 6, 8, 10"),
            textInput(
              inputId = "qual_m2",
              label = "$m_{21}, m_{22}, ..., m_{2p}$",
              value = "4, 6, 8, 10, 12, 14"),
          )
        )),
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.qual_prior_mu_input_type == 'txt'",
        
        fileInput(inputId = "qual_prior_mu_file_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      conditionalPanel(
        condition = "input.qual_prior_mu_input_type == 'csv'",
        
        fileInput(inputId = "qual_prior_mu_file_csv", 
                  label = "Choose .csv File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      p("Please select the number on the top associated with the desired $\\beta_{ji}$ to 
        view the graph."),
      
      tableOutput(outputId = "beta_order_output1"),
      
      numericInput(inputId = "qual_prior_elicit_mu_graphnum",
                   label = "The index of $\\beta_{ji}$ for the graph.",
                   value = 1),
      
    ), # end of sidebarPanel
    mainPanel(
      #verbatimTextOutput(outputId = "qualdebug123"),
      fluidRow(
        splitLayout(
          cellWidths = c("60%", "50%"), 
          #withSpinner(verbatimTextOutput(outputId = "qual_debugging")),
          withSpinner(plotOutput(outputId = "qual_prior_elicit_mu_graph")), 
          withSpinner(tableOutput(outputId = "qual_prior_elicit_mu_table"))
        ),
      )
    ), # end of mainPanel
  ) # end of sidebarLayout
)

page_qualitative_sampleprior = div(
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("This generates a sample of values from the prior."),
      
      actionButton(inputId = "qual_submit_prior_sampling", 
                   label = "Submit Data"),
      
      numericInput(inputId = "qual_prior_seed",
                   label = "Insert the seed",
                   value = 1),
      
      numericInput(inputId = "qual_prior_sample_bigN",
                   label = 'Insert the Monte Carlo sample size',
                   value = 10000),
      
      selectInput(inputId = "qual_prior_comp_use", 
                  label = 'What values of the hyperparameters do you want to use?', 
                  choices = list("Same values as elicitation" = 1,
                                 "Input values manually" = 2,
                                 "Input values via txt file" = 3,
                                 "Input values via csv file" = 4), 
                  selected = 1),
      
      conditionalPanel(
        condition = "input.qual_prior_comp_use == 2",
        
        numericInput(inputId = "qual_alpha01", label = "$\\alpha_{01}$", value = 2.32),
        numericInput(inputId = "qual_alpha02", label = "$\\alpha_{02}$", value = 1.21),
        
        textInput(inputId = "qual_betas",
                  label = "$\\beta_{0j1}, \\beta_{0j2}, ..., \\beta_{0jm}$",
                  value = "2, 4, 6, 8, 10, 12"),
        
        textInput(inputId = "qual_lambda0",
                  label = "$\\lambda_{0j1}, \\lambda_{0j2}, ..., \\lambda_{0jm}$",
                  value = "0.65, 0.65, 0.65, 0.65, 0.65, 0.65"),
      ),
      
      conditionalPanel(
        condition = "input.qual_prior_comp_use == 3",
        
        fileInput(inputId = "qual_elicit_hyperparameters_prior_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      conditionalPanel(
        condition = "input.qual_prior_comp_use == 4",
        
        fileInput(inputId = "qual_elicit_hyperparameters_prior_csv", 
                  label = "Choose .csv File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
    ),
    mainPanel(
      
      downloadButton(outputId = 'qual_download_prior_sample', 
                     label = 'Download Values'),
      
      p("The downloaded values above are formatted as follows, where each row in the file 
        contains:"),
      
      uiOutput(outputId = "beta_order_output_prior"),
      
      p("Below are the first few lines of the file and contains the first set of values 
        generated from the prior."),
      p("Below are used to view different columns within the dataframe."),
      actionButton('qual_prior_prev_five', 'Previous Cols'),
      actionButton('qual_prior_next_five', 'Next Cols'),
      
      # TEST DEBUGGING BELOW
      #withSpinner(verbatimTextOutput(outputId = "debuggingpriorqual123")),
      withSpinner(DTOutput(outputId = 'qual_prior_sample_table'))
      
    ),
  )
)

page_qualitative_samplepost = div(
  titlePanel("Sampling from the Posterior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("This generates a sample of values from the posterior."),
      
      actionButton(inputId = "qual_submit_post_sampling", 
                   label = "Submit Data"),
      
      numericInput(inputId = "qual_post_seed",
                   label = "Insert the seed",
                   value = 1),
      
      numericInput(inputId = "qual_post_sample_bigN",
                   label = 'Insert the Monte Carlo sample size',
                   value = 10000),
      
      selectInput(inputId = "qual_post_comp_use", 
                  label = 'What values of the hyperparameters do you want to use?', 
                  choices = list("Same values as elicitation" = 1,
                                 "Same inputted values as the prior" = 2,
                                 "Input values manually" = 3,
                                 "Input values via txt file" = 4,
                                 "Input values via csv file" = 5), 
                  selected = 1),
      
      conditionalPanel(
        condition = "input.qual_post_comp_use == 3",
        
        numericInput(inputId = "qual_alpha01", label = "$\\alpha_{01}$", value = 2.32),
        numericInput(inputId = "qual_alpha02", label = "$\\alpha_{02}$", value = 1.21),
        
        textInput(inputId = "qual_betas",
                  label = "$\\beta_{0j1}, \\beta_{0j2}, ..., \\beta_{0jm}$",
                  value = "2, 4, 6, 8, 10, 12"),
        
        textInput(inputId = "qual_lambda0",
                  label = "$\\lambda_{0j1}, \\lambda_{0j2}, ..., \\lambda_{0jm}$",
                  value = "0.65, 0.65, 0.65, 0.65, 0.65, 0.65"),
      ),
      
      conditionalPanel(
        condition = "input.qual_post_comp_use == 4",
        
        fileInput(inputId = "qual_elicit_hyperparameters_post_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      conditionalPanel(
        condition = "input.qual_post_comp_use == 5",
        
        fileInput(inputId = "qual_elicit_hyperparameters_post_csv", 
                  label = "Choose .csv File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      
    ),
    mainPanel(
      
      downloadButton(outputId = 'qual_download_post_sample', 
                     label = 'Download Values'),
      
      p("The downloaded values above are formatted as follows, where each row in the file 
        contains:"),
      
      # adding the latex
      withMathJax(),
      
      # Custom JavaScript to handle MathJax reprocessing
      tags$head(
        tags$script(HTML("
      Shiny.addCustomMessageHandler('mathjax_reprocess3', function(message) {
        MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
      });
    "))
      ),
      # end of adding the latex
      
      uiOutput(outputId = "beta_order_output_post"),
      
      p("Below are the first few lines of the file and contains the first set of values 
        generated from the prior."),
      p("Below are used to view different columns within the dataframe."),
      actionButton('qual_post_prev_five', 'Previous Cols'),
      actionButton('qual_post_next_five', 'Next Cols'),
      withSpinner(DTOutput('qual_post_sample_table'))
      
    ),
  )
)



