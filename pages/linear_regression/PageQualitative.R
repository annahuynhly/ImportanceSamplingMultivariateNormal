# qualitative regression

page_qualitativedesc = div(
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
  ), # End of Latex Code
  
  p("Insert a description here for the qualitative factors case."),
  hr(),
  
  fluidRow(
    column(3, numericInput(inputId = "qual_num_factors",
                        label = 'Insert the # of factors. This should the number of inputted 
                        $l_{i}$.',
                        min = 1, max = 10000000, value = 2),
    ),
    column(3, textInput(inputId = "qual_num_levels", 
                        label = "Insert $l_{i}$, the number of levels 
                of factor $i$, $1 \\leq i \\leq m$", 
                        value = "2, 3"),
    ),
    column(3, textInput(inputId = "qual_num_n", 
                     label = "Insert $n_{j1}, ..., n_{jm}$ which is the sample size at factor $i$
                     at level $j_{i}$", 
                     value = "5, 5, 5, 5, 5, 5"),
    )
  ), # end fluidRow
  
  
  hr(),
  
  h4("How to Submit .txt or .csv files Containing the Data"),
  p("At the moment, this website exclusively accepts .csv or .txt files with a specific structure. 
    Ensure that your file has one header, Y, which has the following format:"),
  p("$Y = $ TODO: write the format here"),
  
  selectInput(inputId = "qual_input_type",
              label = 'How do you want to upload the data?',
              choices = list("Use default data" = "default",
                             "Text file" = "txt",
                             "CSV file" = "csv"),
              selected = "default"),
  
  conditionalPanel(
    condition = "input.qual_input_type == 'default'",
    p("The data is based off of the generated sample detailed in 
          the description section.")
  ),
  
  conditionalPanel(
    condition = "input.qual_input_type == 'csv'",
    fileInput(inputId = "qual_Y_input_csv", 
              label = "Upload File for Y",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", 
                         ".csv")),
  ),
  
  conditionalPanel(
    condition = "input.qual_input_type == 'txt'",
    fileInput(inputId = "qual_Y_input_txt", 
              label = "Upload File for Y",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", 
                         ".txt")),
  ),
  
  hr(),
  h4("Default Example"),
  
  p("There is a default example specified to illustrate the implementation of the analysis. 
    We generated a sample of $n = 30$ from a $\\mathcal{N}_{5}(\\mu, \\Sigma)$ distribution
    where:"),
  p("$\\mu = (2, 4, 6, 8, 10, 12)^{'}, \\sigma = 2$"),
  p("The generated sample can be downloaded below."),
  
  numericInput(inputId = "qual_default_example_seed",
               label = "Insert the seed for generating the default example.",
               value = 1),
  
  downloadButton(outputId = "qual_Y_input_example_csv", label = "Download .csv"),
  downloadButton(outputId = "qual_Y_example_txt", label = "Download .txt"),
  
  p("Alternatively, the data appears as follows: TODO: format below..."),
  
  verbatimTextOutput(outputId = "sample_Y_text_output")
  
)

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
      
      # I believe the number of dimensions is fixed as 1
      #numericInput(inputId = "qual_num_dimensions",
      #             label = 'Insert the number of dimensions, $p$.',
      #             min = 1, max = 10000000, step = 1, value = 5),
      
      numericInput(inputId = "qual_virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      
      p("Below, insert $s_{1i} \\leq \\sigma \\cdot z_{0} \\leq s_{2i}$ 
        holds with virtual certainty."),
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'manual'",
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
      
      selectInput(inputId = "qual_elicit_sigma_graph_type",
                  label = "Select which type of graph to view.",
                  choices = list("Prior Density of Sigma" = 1,
                                 "Prior Density of Sigma * z" = 2)),
      
    ),
    mainPanel(
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
                  label = 'How do you want to enter the data? (TODO: need to edit)',
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
        
        p("TODO: NEED TO IMPLEMENT. NOT WORKING YET"),
        
        fileInput(inputId = "qual_prior_mu_file_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      conditionalPanel(
        condition = "input.qual_prior_mu_input_type == 'csv'",
        
        p("TODO: NEED TO IMPLEMENT. NOT WORKING YET"),
        
        fileInput(inputId = "qual_prior_mu_file_csv", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      ),
      
      numericInput(inputId = "qual_prior_elicit_mu_graphnum",
                   label = "The index of $\\beta_{ji}$ for the graph.",
                   value = 1),
      
    ), # end of sidebarPanel
    mainPanel(
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
                   value = 20000),
      
      # implement the below later
      #selectInput(inputId = "qual_post_comp_use", 
      #            label = 'What values of the hyperparameters do you want to use?', 
      #            choices = list("Same values as elicitation" = 1,
      #                           "Input values" = 2), 
      #            selected = 1),
    ),
    mainPanel(
      
      downloadButton(outputId = 'qual_download_prior_sample', 
                     label = 'Download Values'),
      
      p("The downloaded values above are formatted as follows, where each row in the file 
        contains:"),
      p("TODO: change the formatting"),
      p("Below are the first few lines of the file and contains the first set of values 
        generated from the prior."),
      p("Below are used to view different columns within the dataframe."),
      actionButton('qual_prior_prev_five', 'Previous Cols'),
      actionButton('qual_prior_next_five', 'Next Cols'),
      withSpinner(DTOutput('qual_prior_sample_table'))
      
      
      
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
                   value = 20000),
      
      # implement the below later
      #selectInput(inputId = "qual_post_comp_use", 
      #            label = 'What values of the hyperparameters do you want to use?', 
      #            choices = list("Same values as elicitation" = 1,
      #                           "Same inputted values as the prior" = 2,
      #                           "Input values" = 3), 
      #            selected = 1),
    ),
    mainPanel(
      
      downloadButton(outputId = 'qual_download_post_sample', 
                     label = 'Download Values'),
      
      p("The downloaded values above are formatted as follows, where each row in the file 
        contains:"),
      p("TODO: change the formatting"),
      p("Below are the first few lines of the file and contains the first set of values 
        generated from the prior."),
      p("Below are used to view different columns within the dataframe."),
      actionButton('qual_post_prev_five', 'Previous Cols'),
      actionButton('qual_post_next_five', 'Next Cols'),
      withSpinner(DTOutput('qual_post_sample_table'))
      
    ),
  )
)



################################################################
# TAB ORGANIZATION                                             #
################################################################

page_qualitative_reg = div(
  titlePanel("Qualitative Factors Only - Linear Regression"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_qualitativedesc),
              tabPanel("Sufficient Statistics", page_qualitativeinputs),
              tabPanel("Sigma", page_qualitative_elicitsigma),
              tabPanel("Mu", page_qualitative_elicitmu),
              # need to check if names should be shortened?
              tabPanel("Sampling from the Prior", page_qualitative_sampleprior),
              tabPanel("Sampling from the Posterior", page_qualitative_samplepost)
  )
)
