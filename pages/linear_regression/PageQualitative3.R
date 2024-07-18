################################################################
# RBR                                                          #
################################################################

page_qualitative_rbr = div(
  titlePanel("Relative Belief Ratio"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      downloadButton(outputId = "qual_download_psi_plots", label = "Download Plots"),
      
      # todo: display the order here for reference?
      
      numericInput(inputId = "qual_column_num_rbr",
                   label = "TODO: change tne name, but essentially pick the column number to use",
                   value = 1),
      
      numericInput(inputId = "qual_rbr_delta",
                   label = 'Insert $\\delta$, the width of the bins of the histogram.',
                   value = 0.5),
      
      p("Below is the # of average points for the samples (smoother)"),
      
      fluidRow(box(width = 12,
        splitLayout(
          numericInput(inputId = "qual_rbr_mprior", label = 'Prior', value = 7),
          numericInput(inputId = "qual_rbr_mpost", label = 'Posterior', value = 7),
      ))),
      
      selectInput(inputId = "qual_comparison_modify_which",
                  label = 'Select line to modify',
                  choices = list("Prior" = 'prior', "Posterior" = 'post',
                                 "Relative belief ratio" = 'rbr'),
                  selected = 'prior'), 
      
      conditionalPanel(
        condition = "input.qual_comparison_modify_which == 'prior'",
        
        colourInput(inputId = "qual_comparison_prior_col",
                    label = 'Input colour of the prior',
                    value = "FF6666"),
        selectInput(inputId = "qual_comparison_prior_lty", 
                    label = 'Select line type of the prior', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      conditionalPanel(
        condition = "input.qual_comparison_modify_which == 'post'",
        colourInput(inputId = "qual_comparison_post_col",
                    label = 'Input colour of the posterior',
                    value = "6699FF"), 
        selectInput(inputId = "qual_comparison_post_lty", 
                    label = 'Select line type of the posterior', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      conditionalPanel(
        condition = "input.qual_comparison_modify_which == 'rbr'",
        colourInput(inputId = "qual_comparison_rbr_col",
                    label = 'Input colour of the relative belief ratio',
                    value = "05DEB2"), 
        selectInput(inputId = "qual_comparison_rbr_lty", 
                    label = 'Select line type of the relative belief ratio', 
                    choices = list("0" = 0, "1" = 1, "2" = 2, 
                                   "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                    selected = 2),
      ),
      
    ), # end of sidebarPanel
    mainPanel(
      
      # debugging
      #verbatimTextOutput(outputId = "qualdebugging1234"),
      
      conditionalPanel(
        # layout where prior and posterior are on different plots
        condition = "input.qual_rbr_graph_layout == 1",
        tabPanel("Plots",
          fluidRow(
            splitLayout(
              cellWidths = c("33%", "33%", "33%"), 
              withSpinner(plotOutput("qual_prior_alpha_plot")), 
              withSpinner(plotOutput("qual_post_alpha_plot")),
              withSpinner(plotOutput("qual_rbr_alpha_plot1"))
            )
          ),
        ), 
      ), # end conditional Panel
      
      conditionalPanel(
        # layout where prior and posterior are on the same plot
        condition = "input.qual_rbr_graph_layout == 2",
        tabPanel("Plots",
          fluidRow(
            splitLayout(
              cellWidths = c("50%", "50%"), 
              withSpinner(plotOutput("qual_priorpost_alpha_plot")), 
              withSpinner(plotOutput("qual_rbr_alpha_plot2"))
            )
          ),
        ), 
      ), # end conditionalPanel
      
      fluidRow(
        column(4,
               selectInput(inputId = "qual_rbr_graph_layout", 
                           label = 'What graph layout would you prefer?', 
                           choices = list("Prior and posterior on different plots" = 1,
                                          "Prior and posterior on the same plot" = 2), 
                           selected = 1),
        ),
        column(4, 
               fluidRow(box(width = 12,
                  splitLayout(
                  numericInput(inputId = "qual_psi_plot_xmin", label = 'Min x-axis', value = 35),
                  numericInput(inputId = "qual_psi_plot_xmax", label = 'Max x-axis', value = 55),
               ))),
        ),
        column(4, 
               sliderInput(inputId = "qual_comparison_transparency", 
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

page_qualitative_rbr_inf = div(
  titlePanel("Relative Belief Inferences"), # placeholder title
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(inputId = "qual_alpha_null",
                  label = 'Would you like to access the evidence for a hypothesis  
                  $H_{0} : \\alpha = \\alpha_{0}$?',
                  choices = list("Yes" = 1,
                                 "No" = 2),
                  selected = 2),
      
      conditionalPanel(
        condition = "input.qual_alpha_null == 1",
        p("Below is for accessing the hypothesis $H_{0} : \\alpha = \\alpha_{0}$"),
        
        numericInput(inputId = "qual_alpha0",
                     label = 'Insert the value of $\\alpha_{0}$',
                     value = 40),
      ),
      
    ), # end of sidebarPanel
    mainPanel(
      p("Estimate of the true value of $\\alpha_{0}$ from the relative belief ratio:"),
      withSpinner(verbatimTextOutput(outputId = "qual_psi_hypo_test_output1")),
      p("Plausible region:"),
      withSpinner(verbatimTextOutput(outputId = "qual_psi_hypo_test_output2")),
      p("Posterior content of the plausible region:"),
      withSpinner(verbatimTextOutput(outputId = "qual_psi_hypo_test_output3")),
      conditionalPanel(
        condition = "input.qual_alpha_null == 1",
        p("The evidence concerning strength $H_{0} : \\alpha = \\alpha_{0}$:"),
        withSpinner(verbatimTextOutput(outputId = "qual_psi_hypo_test_output4")),
        p("The strength of the evidence concerning $H_{0} : \\alpha = \\alpha_{0}$:"),
        withSpinner(verbatimTextOutput(outputId = "qual_psi_hypo_test_output5")),
      )
      #withSpinner(verbatimTextOutput(outputId = "qual_psi_hypo_test_output")),
    ) # end of mainPanel
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
              tabPanel("Beta", page_qualitative_elicitmu),
              # need to check if names should be shortened?
              tabPanel("Sampling from the Prior", page_qualitative_sampleprior),
              tabPanel("Sampling from the Posterior", page_qualitative_samplepost),
              tabPanel("Relative Belief Ratio", page_qualitative_rbr),
              tabPanel("Relative Belief Inferences", page_qualitative_rbr_inf),
  )
)