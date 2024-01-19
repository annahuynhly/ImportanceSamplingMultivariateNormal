################################################################
# ELICITING FROM THE PRIOR FOR MU                              #
################################################################

page_elicitdescription = div(
  titlePanel("Description"),
  p("UNDER CONSTRUCTION! Description will be added soon...")
)

################################################################
# ELICITING FROM THE PRIOR (SIGMA)                             #
################################################################

page_elicitsigma = div(
  titlePanel("Determining the Prior on $\\sigma_{i}^{2}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(inputId = "submit_prior_elicit_sigma", label = "Submit Data"),
      
      downloadButton(outputId = 'download_prior_elicit_sigma', label = 'Download Plot'),
      
      numericInput(inputId = "num_dimensions",
                   label = 'Insert the number of dimensions, $p$.',
                   min = 1, max = 10000000, step = 1, value = 3),
      
      numericInput(inputId = "virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      
      selectInput(inputId = "prior_sigma_input_type",
                  label = 'How do you want to enter the data?',
                  choices = list("Manually" = "manual",
                                 "Text file" = "txt",
                                 "CSV file" = "csv")),
      
      p("For below, you need to insert $s_{1} \\leq \\sigma z_{0} \\leq s_{2}$ 
        holds with virtual certainty."),
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'manual'",
        fluidRow(box(width = 12,
          splitLayout(
            textInput(inputId = "elicit_s1", label = "Insert $s_{1}$", value = "2,2,2"),
            textInput(inputId = "elicit_s2", label = "Insert $s_{2}$", value = "10,10,10"),
          )
        )),
        
        p("Bounds for the iterative processes determining these values for $\\alpha_{0i}$:"),
        
        fluidRow(box(width = 12,
          splitLayout(
            textInput(inputId = "alphalow", label = "Lower bound",
                      value = "0,0,0"),
            textInput(inputId = "alphaup", label = "Upper bound",
                      value = "50,50,50"),
          )
        )),
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'txt'",
        
        fileInput(inputId = "prior_sigma_file_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        actionButton(inputId = "prior_sigma_submit_info_txt", label = "How to Submit")
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.prior_sigma_input_type == 'csv'",
        
        fileInput(inputId = "prior_sigma_file_csv", 
                  label = "Choose .csv File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        actionButton(inputId = "prior_sigma_submit_info_csv", label = "How to Submit")
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
      tabPanel("Relative Belief Plot of w0",
        fluidRow(
          splitLayout(
            cellWidths = c("60%", "50%"), 
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
  
  titlePanel("Prior of the $\\mu_{i}'s$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
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
              value = "-5,-5,-5"),
            textInput(
              inputId = "m2",
              label = "$m_{21}, m_{22}, ..., m_{2p}$",
              value = "5,5,5"),
          )
        )),
      ), # end of conditional panel
      
      conditionalPanel(
        condition = "input.prior_mu_input_type == 'txt'",
        
        fileInput(inputId = "prior_mu_file_txt", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        actionButton(inputId = "prior_mu_submit_info_txt", label = "How to Submit")
        
      ),
      
      conditionalPanel(
        condition = "input.prior_mu_input_type == 'csv'",
        
        fileInput(inputId = "prior_mu_file_csv", 
                  label = "Choose .txt File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        actionButton(inputId = "prior_mu_submit_info_csv", label = "How to Submit")
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
# TAB ORGANIZATION                                             #
################################################################

page_prior_elicit = div(
  titlePanel("Prior Elicitation"),
  tabsetPanel(type = "tabs",
              tabPanel("Sigma", page_elicitsigma),
              tabPanel("Mu", page_elicitmu),
              tabPanel("Description", page_elicitdescription)
  )
)

