################################################################
# ELICITING FROM THE PRIOR FOR MU                              #
################################################################

page_elicitdescription = div(
  titlePanel("Description"),
  p("UNDER CONSTRUCTION! Description will be added soon...")
)

page_elicitprior = div(
  titlePanel("Specify the Prior of $\\mu$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      textInput(
        inputId = "m1",
        label = "Insert the vector of $m_{11}, m_{12}, ..., m_{1p}$",
        value = "-5,-5,-5"),
      textInput(
        inputId = "m2",
        label = "Insert the vector of $m_{21}, m_{22}, ..., m_{2p}$",
        value = "5,5,5"),
      textInput(
        inputId = "const_s",
        label = "Insert the upper range of $s_{2}$, where $\\delta_{i}$ 
                need to be contained.",
        value = "2,2,2"),
    ),
    mainPanel(
      withSpinner(verbatimTextOutput("elicit_prior_computation"))
    ),
  )
)

################################################################
# ELICITING FROM THE PRIOR FOR SIGMA                           #
################################################################

page_elicitsigma = div(
  titlePanel("Specify the Prior of $\\sum$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "virtual_uncertainty_sigma",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
      p("For below, you need to insert $s_{1} \\leq \\sigma z_{0} \\leq s_{2}$ 
        holds with virtual certainty."),
      numericInput(inputId = "elicit_sigma_s1",
                   label = "Insert $s_{1}$", value = 1),
      numericInput(inputId = "elicit_sigma_s2",
                   label = "Insert $s_{2}$", value = 1),
      numericInput(inputId = "alphalow_sigma",
                   label = "Lower bound for $\\alpha_{0i}$", value = 0),
      numericInput(inputId = "alphaup_sigma",
                   label = "Upper bound for $\\alpha_{0i}$", value = 50)
    ),
    mainPanel(
      tabPanel(
        "sigmawhatever", withSpinner(verbatimTextOutput("sigma_elicit_prior_calculation"))
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
              tabPanel("Description", page_elicitdescription),
              tabPanel("Specify the Prior for mu", page_elicitprior),
              tabPanel("Specify the Prior for sigma", page_elicitsigma)
  )
)

