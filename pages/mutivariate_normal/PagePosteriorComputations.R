################################################################
# DESCRIPTION FOR THE POSTERIOR COMPUTATIONS                   #
################################################################

page_post_comp_description = div(
  titlePanel("Description"),
  p("currently a placeholder; will add more details later."),
  hr(),
  h4("How to Submit .txt or .csv files"),
  p("At the moment, this website exclusively accepts .csv or .txt files with a specific structure. Ensure that your file includes a header, where each column corresponds to a distinct variable:"),
  p("$Y = (Y_{1}, Y_{2}, Y_{3}, ..., Y_{N})$"),
  p("You may download the samples below for the acceptable format of .txt or .csv files to get an idea of how to upload."),
  downloadButton(outputId = "post_computation_input_example_csv", label = "Download .csv"),
  downloadButton(outputId = "post_computation_input_example_txt", label = "Download .txt"),
  hr(),
  p("If you cannot download the following samples above, note that the .txt file appears as is:"),
  p("\"Y1\",\"Y2\",\"Y3\""),
  p("1.76312851911246,2.45276003285563,0.661806566932632"),
  p("2.89995319903158,2.63143675558349,0.0285393909422083"),
  p("1.28504391922971,2.94688870456367,3.34204091589147"),
  br(),
  p("Esentially, each item must be separated by commas instead of spaces. Alternatively, the .csv file is formatted as follows:"),
  DTOutput('post_comp_example_csv_table'),
  
  # the below is just for testing
  #verbatimTextOutput(outputId = "testing123")
)

################################################################
# USING IMPORTANCE SAMPLING FOR THE POSTERIOR                  #
################################################################

page_posteriorcomputations = div(

  titlePanel("Integrating with Respect to the Posterior"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      fileInput(inputId = "sample_post_Y", 
                label = "Upload File for Y",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      p("Note: you will need to resubmit if you make any changes with the inputs below."),
      
      actionButton(inputId = "submit_sample_post", label = "Submit Data"),
      #actionButton(inputId = "post_download_info", label = "How to Submit"),
    
      numericInput(inputId = "post_bigN",
                   label = 'Insert N, the Monte Carlo sample size',
                   value = 1000),
    
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
                     min = 1, max = 10000000, step = 1, value = 3),
        
        textInput(inputId = "mu0_post",
                  label = "$\\mu_{01}, ..., \\mu_{0p}$",
                  value = "0, 0, 0"),
        
        textInput(inputId = "lambda0_post",
                  label = "$\\lambda_{021}, ..., \\lambda_{02p}$",
                  value = "1, 1, 1"),
        
      ),
      
      numericInput(inputId = "post_graph_num",
                   label = "The column of $\\mu$ for the graph.",
                   value = 1),
      
      colourInput(inputId = "post_line_col",
                  label = 'Input colour of the line plot',
                  value = "#4287f5"), 
      
    ),
    mainPanel(
      downloadButton(outputId = 'plot_post_mu', label = 'Plot'),
      downloadButton(outputId = "postsample_download_mu", label = "$\\mu$"),
      downloadButton(outputId = "postsample_download_xi", label = "$\\Xi$"),
      
      #withSpinner(verbatimTextOutput(outputId = "testing_post")),
      withSpinner(plotOutput("sample_post_graph")),

      fluidRow(
        column(4, 
               fluidRow(box(width = 12,
                 splitLayout(
                    numericInput(inputId = "post_xlim_min", 
                                 label = "Lower x limit", value = -10),
                    numericInput(inputId = "post_xlim_max", 
                                 label = "Upper x limit", value = 10),
                  )
               )),
              #numericInput(inputId = "post_graph_delta",
              #              label = "Length of the bins)",
              #              value = 0.05),
        ),
        
        column(4, 
               sliderInput(inputId = "post_transparency", 
                           label = "Scale for colour transparency",
                           min = 0, max = 1, value = 0.2), 
        ),
        
        column(4, 
               sliderInput(inputId = "post_graph_smoother", 
                           label = "# of Average Points (Smoother)", 
                           min = 1, max = 15, value = 3, step = 2)
        )
      ),
      
      
    ),
  )
)

################################################################
# PORTION FOR GRAPH BUILDING                                   #
################################################################

page_comparison_graphs = div(
  titlePanel("Plots for $\\mu$"), 
  p("Note: will not work unless the user inputs the data from the previous section."),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      downloadButton(outputId = 'comparison_download_plot', 
                     label = 'Prior/Posterior Plot'),
      
      downloadButton(outputId = 'rbr_download_plot', 
                     label = 'RBR Plot'),
      
      numericInput(inputId = "comparison_mu_col", 
                   label = 'The column of $\\mu$ for the graph.',
                   value = 1),
      
      selectInput(inputId = "comparison_modify_which",
                  label = 'Select line to modify',
                  choices = list("Prior" = 'prior', "Posterior" = 'post',
                                 "Relative belief ratio" = 'rbr'),
                  selected = 'prior'), 
      
      conditionalPanel(
        condition = "input.comparison_modify_which == 'prior'",
        
        colourInput(inputId = "comparison_prior_col",
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
        colourInput(inputId = "comparison_post_col",
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
        colourInput(inputId = "comparison_rbr_col",
                    label = 'Input colour of the relative belief ratio',
                    value = "7F00FF"), 
        selectInput(inputId = "comparison_rbr_lty", 
                    label = 'Select line type of the relative belief ratio', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
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
      
      fluidRow(
        column(4,
               fluidRow(box(width = 12,
                  splitLayout(
                    numericInput(inputId = "comparison_xlim_min", 
                                 label = "Lower x limit", value = -10),
                    numericInput(inputId = "comparison_xlim_max", 
                                 label = "Upper x limit", value = 10),
                  )
               )),
               #sliderInput(inputId = "comparison_graph_delta",
               #             label = "Length of the bins",
               #            min = 0.01, max = 1, value = 0.1),
        ),
        column(4, 
               sliderInput(inputId = "comparison_smoother", 
                           label = "# of Average Points (Smoother)", 
                           min = 1, max = 15, value = 3, step = 2),
        ),
        column(4, 
               sliderInput(inputId = "comparison_transparency", 
                           label = "Scale for colour transparency",
                           min = 0, max = 1, value = 0.2), 
        )
      ),
      
    ),
  ),
)

################################################################
# TAB ORGANIZATION                                             #
################################################################

page_sampling = div(
  titlePanel("Posterior Computations"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_post_comp_description),
              tabPanel("Sampling from the Posterior", page_posteriorcomputations),
              tabPanel("Comparison Plots for Mu", page_comparison_graphs),
              #tabPanel("Graphs", page_priorgraph),
  )
)

