################################################################
# ELICITING FROM THE PRIOR FOR MU                              #
################################################################

page_elicitdescription = div(
  titlePanel("Description"),
  p("UNDER CONSTRUCTION! Description will be added soon...")
)

################################################################
# ELICITING FROM THE PRIOR                                     #
################################################################

page_elicitsigma = div(
  titlePanel("Determining the prior on $\\frac{1}{\\sigma^{2}}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      
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
      
      p("For below, you need to insert $s_{1} \\leq \\sigma z_{0} \\leq s_{2}$ 
        holds with virtual certainty."),
      fluidRow(box(
        width = 12,
        splitLayout(
          textInput(
            inputId = "elicit_s1",
            label = "Insert $s_{1}$",
            value = "2, 2, 2"),
          textInput(
            inputId = "elicit_s2",
            label = "Insert $s_{2}$",
            value = "10, 10, 10"),
        )
      )),
      
      fluidRow(box(
        width = 12,
        splitLayout(
          textInput(
            inputId = "alphalow",
            label = "Lower bd of $\\alpha_{0i}$",
            value = "0, 0, 0"),
          textInput(
            inputId = "alphaup",
            label = "Upper bd of $\\alpha_{0i}$",
            value = "50, 50, 50"),
        )
      )),
      
      selectInput(inputId = "elicit_sigma_graph_type",
        label = "Select which type of graph to view.",
        choices = list("Prior Density of Sigma" = 1,
                       "Prior Density of Sigma * z" = 2)),
    ),
    # want to add two tab panels
    mainPanel(
      tabPanel("Relative Belief Plot of w0",
        fluidRow(
          splitLayout(
            cellWidths = c("65%", "35%"), 
              withSpinner(plotOutput(outputId = "elicit_prior_graph")), 
              withSpinner(verbatimTextOutput("elicit_prior_calculation"))
          )
        )
      )
    )
  )
)

################################################################
# TAB ORGANIZATION                                             #
################################################################

page_prior_elicit = div(
  titlePanel("Prior Elicitation"),
  tabsetPanel(type = "tabs",
              tabPanel("Specify the Prior", page_elicitsigma),
              tabPanel("Description", page_elicitdescription)
  )
)

