################################################################
# DESCRIPTION FOR THE POSTERIOR COMPUTATIONS                   #
################################################################

page_post_comp_description = div(
  titlePanel("Description"),
  p("Please refer to section 3.2 of the paper for this section."),
  p("currently a placeholder; will add more details later."),
  hr(),
  h4("How to Submit .txt or .csv files"),
  p("At the moment, this website exclusively accepts .csv or .txt files with a specific structure. 
    Ensure that your file includes a header, where each column corresponds to a distinct variable:"),
  p("$Y = (Y_{1}, Y_{2}, Y_{3}, ..., Y_{P})$"),
  hr(),
  p("There is a default example specified to illustrate the implementation of the analysis. 
    We generated a sample of $n = 50$ from a $\\mathcal{N}_{5}(\\mu, \\Sigma)$ distribution
    where:"),
  p("$\\mu = (-2, -1, 0, 1, 2)^{'}, \\sigma_{1} = 2, \\sigma_{2} = 1, \\sigma_{3} = 0.5, 
     \\sigma_{4} = 1, \\sigma_{5} = 2, R = \\frac{1}{2} I_{5} + \\frac{1}{2} 
     \\underset{\\sim}{1_{5}} \\underset{\\sim}{1_{5}^{'}}$ 
    where $\\underset{\\sim}{1_{5}} = (1, 1, 1, 1, 1)^{'}$ and"),
  p("$\\Sigma = $ diag($\\sigma_{1}, ..., \\sigma_{p}$)$ \\enspace R \\enspace 
     $diag($\\sigma_{1}, ..., \\sigma_{p}$)"),
  p("The generated sample can be downloaded below. Note that the format of the file consists of $n$ rows
     where each row is of the form $y_{1}, y_{2}, ..., y_{p}$."),
  numericInput(inputId = "post_default_example_seed",
               label = "Insert the seed for generating the default example.",
               value = 1),
  downloadButton(outputId = "post_computation_input_example_csv", label = "Download .csv"),
  downloadButton(outputId = "post_computation_input_example_txt", label = "Download .txt"),
  hr(),
  p("Esentially, each item must be separated by commas instead of spaces. 
    Alternatively, the .csv file is formatted as follows:"),
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
      
      p("Note: you must press \"submit\" to specify the prior for this page to \
        load properly."),
      
      selectInput(inputId = "post_input_type",
                  label = 'How do you want to upload the data?',
                  choices = list("Use default data" = "default",
                                 "Text file" = "txt",
                                 "CSV file" = "csv"),
                  selected = "default"),
      
      conditionalPanel(
        condition = "input.post_input_type == 'csv'",
        p("The data is based off of the generated sample detailed in 
          the description section.")
      ),
      
      conditionalPanel(
        condition = "input.post_input_type == 'csv'",
        fileInput(inputId = "sample_post_Y", 
                  label = "Upload File for Y",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", 
                             ".csv")),
      ),
      
      conditionalPanel(
        condition = "input.post_input_type == 'txt'",
        fileInput(inputId = "sample_post_Y_txt", 
                  label = "Upload File for Y",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", 
                             ".txt")),
      ),
      
      p("Note: you will need to resubmit if you make any changes with the inputs below."),
      
      actionButton(inputId = "submit_imp_sampler", label = "Submit Data"),
    
      numericInput(inputId = "post_seed",
                   label = "Insert the seed",
                   value = 1),
      
      numericInput(inputId = "post_bigN",
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
                     min = 1, max = 10000000, step = 1, value = 3),
        
        # should probably change the following values lol
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
      downloadButton(outputId = "imp_computation_download", label = "Download Values"),
      
      p("Below are used to view different columns within the dataframe."),
      
      actionButton('imp_prev_five', 'Previous Cols'),
      actionButton('imp_next_five', 'Next Cols'),
      
      withSpinner(DTOutput(outputId = 'imp_display_table')),
      #withSpinner(verbatimTextOutput(outputId = "testing_post")),

    ),
  )
)

################################################################
# SIR ALGORITHM                                                #
################################################################

page_SIR_algorithm = div(
  titlePanel("SIR Algorithm"), # placeholder title
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("Note: you must press \"submit\" to specify the prior for this page to \
        load properly."),
      
      actionButton(inputId = "submit_sample_post", label = "Submit Data"),
      
      # hope the below name isn't a repeat.
      numericInput(inputId = "post_sample_N",
                   label = 'Insert the Monte Carlo sample size',
                   value = 20000),
      
      
    ), # end sidebarPanel
    mainPanel(
      
      downloadButton(outputId = "post_computation_download", label = "Download Values"),
      
      p("Below are used to view different columns within the dataframe."),
      
      actionButton('post_prev_five', 'Previous Cols'),
      actionButton('post_next_five', 'Next Cols'),
      
      withSpinner(DTOutput(outputId = 'post_display_table')),
      #withSpinner(verbatimTextOutput(outputId = "SIR_algorithm_output")),
      
    ), # end mainPanel
  ),
  
)

################################################################
# RBR                                                          #
################################################################

page_rbr_comparison = div(
  titlePanel("Relative Belief Ratio of Psi"), # placeholder title
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      numericInput(inputId = "rbr_numcells",
                   label = 'Insert the number of subintervals for the density histogram',
                   value = 100),
      
      p("Below is the # of average points for the samples (smoother)"),
      
      fluidRow(box(width = 12,
        splitLayout(
          numericInput(inputId = "rbr_mprior", label = 'Prior', value = 7),
          numericInput(inputId = "rbr_mpost", label = 'Posterior', value = 5),
      ))),
      
      selectInput(inputId = "comparison_modify_which",
                  label = 'Select line to modify',
                  choices = list("Prior" = 'prior', "Posterior" = 'post',
                                 "Relative belief ratio" = 'rbr'),
                  selected = 'prior'), 
      
      conditionalPanel(
        condition = "input.comparison_modify_which == 'prior'",
        
        colourInput(inputId = "comparison_prior_col",
                    label = 'Input colour of the prior',
                    value = "FF6666"),
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
                    value = "6699FF"), 
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
                    value = "05DEB2"), 
        selectInput(inputId = "comparison_rbr_lty", 
                    label = 'Select line type of the relative belief ratio', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      
    ), # end of sidebarPanel
    mainPanel(
      
      conditionalPanel(
        # layout where prior and posterior are on different plots
        condition = "input.rbr_graph_layout == 1",
        tabPanel("Plots",
          fluidRow(
            splitLayout(
              cellWidths = c("33%", "33%", "33%"), 
                withSpinner(plotOutput("prior_psi_plot")), 
                withSpinner(plotOutput("post_psi_plot")),
                withSpinner(plotOutput("rbr_psi_plot"))
            )
          ),
        ), 
      ), # end conditional Panel
      
      conditionalPanel(
        # layout where prior and posterior are on the same plot
        condition = "input.rbr_graph_layout == 2",
        tabPanel("Plots",
          fluidRow(
            splitLayout(
              cellWidths = c("50%", "50%"), 
              withSpinner(plotOutput("priorpost_psi_plot")), 
              withSpinner(plotOutput("rbr_psi_plot_duplicate"))
            )
          ),
        ), 
      ), # end conditionalPanel

      fluidRow(
        column(4,
               selectInput(inputId = "rbr_graph_layout", 
                           label = 'What graph layout would you prefer?', 
                           choices = list("Prior and posterior on different plots" = 1,
                                          "Prior and posterior on the same plot" = 2), 
                           selected = 1),
        ),
        column(4, 
               fluidRow(box(width = 12,
                splitLayout(
                  numericInput(inputId = "psi_plot_xmin", label = 'Min x-axis', value = -10),
                  numericInput(inputId = "psi_plot_xmax", label = 'Max x-axis', value = 10),
                ))),
        ),
        column(4, 
               sliderInput(inputId = "comparison_transparency", 
                           label = "Scale for colour transparency",
                           min = 0, max = 1, value = 0.2), 
        )
      ), # end fluidRow
      
    ) # end of mainPanel
    
  )
)


################################################################
# HYPOTHESIS TESTING                                           #
################################################################

page_psi_hypo_test = div(
  titlePanel("Hypothesis Testing for Psi"), # placeholder title
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      p("Below is for accessing the hypothesis $H_{0} : \\psi = \\psi_{0}$"),
      
      numericInput(inputId = "psi0",
                   label = 'Insert the value of $\\psi_{0}$',
                   value = -2),
      
    ), # end of sidebarPanel
    mainPanel(
      
      withSpinner(verbatimTextOutput(outputId = "psi_hypo_test_output")),
      
    ) # end of mainPanel
    
  )
)

################################################################
# TAB ORGANIZATION                                             #
################################################################

page_sampling = div(
  titlePanel("Posterior Computations"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_post_comp_description),
              tabPanel("Integrating with Respect to the Posterior", page_posteriorcomputations),
              tabPanel("SIR Algorithm", page_SIR_algorithm), # will probably re-name
              tabPanel("Relative Belief Ratio of Psi", page_rbr_comparison),
              tabPanel("Hypothesis Testing for Psi", page_psi_hypo_test)
              #tabPanel("Comparison Plots for Mu", page_comparison_graphs),
  )
)

