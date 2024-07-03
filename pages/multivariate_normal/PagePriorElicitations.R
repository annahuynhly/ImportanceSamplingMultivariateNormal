################################################################
# DESCRIPTION FOR THE PRIOR ELICITATION                        #
################################################################

page_elicitdescription = div(
  titlePanel("Description"),
  
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
  
  p("See Sections 2 and 3.1 of the paper for descriptions of the parameters necessary for eliciting the priors on the $\\sigma_{i}$ and the $\\mu_{i}$, namely, ($s_{1i}, s_{2i}$) for $\\sigma_{i}$ and ($m_{1i}, m_{2i}$) for $\\mu_{i}$ for $i = 1, ..., p$."),
  p("Also specify the lower and upper bounds for the iterative process that determines the values of ($\\alpha_{01i}, \\alpha_{02i}$), $i= 1, ..., p.$"),
  hr(),
  h4("How to Submit .txt or .csv files"),
  p("Although you can manually enter values such as $(s_{1i}, s_{2i}), (m_{1i}, m_{2i})$, for large p it may be more convenient for the user to input .txt or .csv files."),
  p("You may download the samples below for the acceptable format of .txt or .csv files to get an idea of how to upload."),
  downloadButton(outputId = "prior_sigma_csv_example", label = "Download .csv Sample"),
  downloadButton(outputId = "prior_sigma_txt_example", label = "Download .txt Sample"),
  p("The headers must exist and must be exactly formatted as is, or the computations will not work properly."),
  hr(),
  p("Esentially, each item must be separated by commas instead of spaces. Alternatively, the .csv file is formatted as follows:"),
  DTOutput('prior_sigma_txt_example_table')
  
)

################################################################
# ELICITING FROM THE PRIOR (SIGMA)                             #
################################################################

page_elicitsigma = div(
  titlePanel("Determining the Priors on the $\\sigma_{i}^{2}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("You must press \"submit\" for the information to load properly."),
      
      actionButton(inputId = "submit_prior_elicit_sigma", label = "Submit Data"),
      
      downloadButton(outputId = 'download_prior_elicit_sigma', label = 'Download Plot'),
      
      numericInput(inputId = "num_dimensions",
                   label = 'Insert the number of dimensions, $p$.',
                   min = 1, max = 10000000, step = 1, value = 5),
      
      numericInput(inputId = "virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      
      selectInput(inputId = "prior_sigma_input_type",
                  label = 'How do you want to enter the data?',
                  choices = list("Manually" = "manual",
                                 "Text file" = "txt",
                                 "CSV file" = "csv")),
      
      p("Below, insert $s_{1i} \\leq \\sigma \\cdot z_{0} \\leq s_{2i}$ 
        holds with virtual certainty."),
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'manual'",
        fluidRow(box(width = 12,
          splitLayout(
            textInput(inputId = "elicit_s1", label = "Insert $s_{11}, ..., s_{1p}$", 
                      value = "1,0.5,0.2,0.5,1"),
            textInput(inputId = "elicit_s2", label = "Insert $s_{21}, ..., s_{2p}$", 
                      value = "7,4,3,4,7"),
          )
        )),
        
        p("Bounds for the iterative processes determining values for $\\alpha_{011}, ..., \\alpha_{01p}$ and $\\alpha_{021}, ..., \\alpha_{02p}$:"),
        
        fluidRow(box(width = 12,
          splitLayout(
            textInput(inputId = "alphalow", label = "Lower bound",
                      value = "0,0,0,0,0"),
            textInput(inputId = "alphaup", label = "Upper bound",
                      value = "50,50,50,50,50"),
          )
        )),
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'txt'",
        
        fileInput(inputId = "prior_sigma_file_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        #actionButton(inputId = "prior_sigma_submit_info_txt", label = "How to Submit")
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'csv'",
        
        fileInput(inputId = "prior_sigma_file_csv", 
                  label = "Choose .csv File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        #actionButton(inputId = "prior_sigma_submit_info_csv", label = "How to Submit")
      ), # end of conditional panel
      
      selectInput(inputId = "elicit_sigma_graph_type",
                  label = "Select which type of graph to view.",
                  choices = list("Prior Density of Sigma" = 1,
                                 "Prior Density of Sigma * z" = 2)),
      
      numericInput(inputId = "prior_elicit_sigma_graphnum", 
                   label = 'The index of $\\alpha_{01}, \\alpha_{02}$ for the graph.',
                   value = 1),
    ),
    # want to add two tab panels
    mainPanel(
      tabPanel("elicitation of the prior graphs",
        fluidRow(
          splitLayout(
            cellWidths = c("60%", "50%"), 
              #withSpinner(verbatimTextOutput("elicit_prior_debugging")),
              withSpinner(plotOutput(outputId = "elicit_prior_graph")), 
              withSpinner(tableOutput(outputId = "prior_elicit_sigma_table"))
              #withSpinner(verbatimTextOutput("elicit_prior_calculation"))
          ),
        )
      )
    )
  )
)



################################################################
# ELICITING FROM THE PRIOR (MU)                                #
################################################################

page_elicitmu = div(
  
  titlePanel("Determining the Priors on the $\\mu_{i}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("You must press \"submit\" for the information to load properly."),
      
      actionButton(inputId = "submit_prior_elicit_mu", label = "Submit Data"),
      
      downloadButton(outputId = 'download_prior_elicit_mu_plot', label = 'Download Plot'),
      
      selectInput(inputId = "prior_mu_input_type",
                  label = 'How do you want to enter the data?',
                  choices = list("Manually" = "manual",
                                 "Text file" = "txt",
                                 "CSV file" = "csv")),
      
      conditionalPanel(
        condition = "input.prior_mu_input_type == 'manual'",
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "m1",
              label = "$m_{11}, m_{12}, ..., m_{1p}$",
              value = "-5,-3,-2,-1,0"),
            textInput(
              inputId = "m2",
              label = "$m_{21}, m_{22}, ..., m_{2p}$",
              value = "0,1,2,3,5"),
          )
        )),
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.prior_mu_input_type == 'txt'",
        
        fileInput(inputId = "prior_mu_file_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        #actionButton(inputId = "prior_mu_submit_info_txt", label = "How to Submit")
        
      ),
      
      conditionalPanel(
        condition = "input.prior_mu_input_type == 'csv'",
        
        fileInput(inputId = "prior_mu_file_csv", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        #actionButton(inputId = "prior_mu_submit_info_csv", label = "How to Submit")
      ),
    
      numericInput(inputId = "prior_elicit_mu_graphnum",
                   label = "The index of $\\mu$ for the graph.",
                   value = 1)
      
    ),
    mainPanel(
      #withSpinner(plotOutput(outputId = "sample_prior_computations_graph_NEW")), 
      #withSpinner(verbatimTextOutput("sample_prior_computation_NEW"))
      fluidRow(
        splitLayout(
          cellWidths = c("60%", "50%"), 
          withSpinner(plotOutput(outputId = "prior_elicit_mu_graph")), 
          withSpinner(tableOutput(outputId = "prior_elicit_mu_table"))
        ),
      )
    ),
  )
)

################################################################
# SAMPLING FROM THE PRIOR                                      #
################################################################

page_sample_computation = div(
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("This generates a sample of values from the prior."),
      
      actionButton(inputId = "submit_prior_sampling", 
                   label = "Submit Data"),
      
      numericInput(inputId = "prior_seed",
                   label = "Insert the seed",
                   value = 1),
      
      numericInput(inputId = "prior_sample_bigN",
                   label = 'Insert the Monte Carlo sample size',
                   value = 20000),
      
      selectInput(inputId = "post_comp_use", 
                  label = 'What values of the hyperparameters do you want to use?', 
                  choices = list("Same values as elicitation" = 1,
                                 "Input values" = 2), 
                  selected = 1),
      
      conditionalPanel(
        condition = "input.post_comp_use == 2",
        p("Values from the elicitation of the prior will be used!")
      ),
      
      conditionalPanel(
        condition = "input.post_comp_use == 2",
        
        numericInput(inputId = "num_dimensions_post",
                     label = 'Insert the number of dimensions, $p$.',
                     min = 1, max = 10000000, step = 1, value = 5),
        
        textInput(inputId = "mu0_post",
                  label = "$\\mu_{01}, ..., \\mu_{0p}$",
                  value = "-2.5, -1, 0, 1, 2.5"),
        
        textInput(inputId = "lambda0_post",
                  label = "$\\lambda_{021}, ..., \\lambda_{02p}$",
                  value = "0.82, 1.21, 1.97, 1.21, 0.82"),
        
        textInput(inputId = "alpha01_post",
                  label = "$\\alpha_{011}, ..., \\alpha_{01p}$",
                  value = "2.32, 2.09, 1.41, 2.09, 2.32"),
        
        textInput(inputId = "alpha02_post",
                  label = "$\\alpha_{021}, ..., \\alpha_{02p}$",
                  value = "1.21, 0.29, 0.04, 0.29, 1.21"),
      ),
      
    ),
    mainPanel(
      downloadButton(outputId = 'download_prior_sample', 
                     label = 'Download Values'),
      
      p("The downloaded values above are formatted as follows, where each row in the file 
        contains:"),
      p("$\\mu_{1}, \\mu_{2}, ..., \\mu_{p}, 
        \\sigma_{11}, \\sigma_{12}, ..., \\sigma_{1p}, 
        \\sigma_{22}, \\sigma_{23}, ..., \\sigma_{2p}, ...,
        \\sigma_{(p-1)(p-1)}, \\sigma_{(p-1)p}, \\sigma_{pp},
        \\sigma_{11}^{'}, \\sigma_{12}^{'}, ..., \\sigma_{1p}^{'}, 
        \\sigma_{22}^{'}, \\sigma_{23}^{'}, ..., \\sigma_{2p}^{'}, ...,
        \\sigma_{(p-1)(p-1)}^{'}, \\sigma_{(p-1)p}^{'}, \\sigma_{pp}^{'}$"),
      p("Here, the $\\mu_{i}$ are the means, $\\sigma_{ij}$ are the correlations of the 
        $\\Sigma$ matrix, and $\\sigma_{ij}^{'}$ are the entries of the $\\Xi$ matrix."),
      p("Below are the first few lines of the file and contains the first set of values 
        generated from the prior."),
      p("Below are used to view different columns within the dataframe."),
      actionButton('prior_prev_five', 'Previous Cols'),
      actionButton('prior_next_five', 'Next Cols'),
      withSpinner(DTOutput('prior_sample_table'))
      #verbatimTextOutput(outputId = "debug_debug1")
    )
  )
)

################################################################
# DEBUGGING                                                    #
################################################################

page_debugging1 = div(
  p("blank for now!")
  #verbatimTextOutput(outputId = "debug_debug")
)


################################################################
# TAB ORGANIZATION                                             #
################################################################

page_prior_elicit = div(
  titlePanel("Prior Elicitation and Sampling from the Prior"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_elicitdescription),
              tabPanel("Sigma", page_elicitsigma),
              tabPanel("Mu", page_elicitmu),
              tabPanel("Sampling from the Prior", page_sample_computation)
  )
)

