page_priorsample = div(
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(inputId = "submit_sample_prior", label = "Submit Data"),
      
      numericInput(inputId = "prior_bigN",
                   label = 'Insert N, the monte carlo sample size.',
                   value = 100),
      
      selectInput(inputId = "priorsample_use", 
                  label = 'Do you want to use the values from the Prior Elicitation calculations?', 
                  choices = list("Yes" = "y", "No" = "n"), 
                  selected = "n"
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use == 'yes'",
        p("The values from the previous section will be used!")
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use == 'n'",
        
        fluidRow(box(
          width = 12,
          splitLayout(
            textInput(
              inputId = "alpha01",
              label = "$\\alpha_{011}, ..., \\alpha_{01p}$",
              value = "1, 1, 1, 1, 1"),
            textInput(
              inputId = "alpha02",
              label = "$\\alpha_{021}, ..., \\alpha_{02p}$",
              value = "1, 1, 1, 1, 1"),
          )
        )),
        
        fluidRow(box(
          width = 12,
          splitLayout(
            numericInput(inputId = "mu_0",
                         label = 'Insert $\\mu_{0}$',
                         value = 0),
            numericInput(inputId = "sigma_0",
                         label = 'Insert $\\sigma_{0}$',
                         value = 1),
          )
        )),
      ),
      
    ),
    mainPanel(
      p("The values shown display limited information, but can be downloaded:"),
      downloadButton("priorsample_download_mu", "Download $\\mu$"),
      downloadButton("priorsample_download_sigma", "Download $\\Sigma$"),
      downloadButton("priorsample_download_R", "Download $R$"),
      p(""),
      withSpinner(verbatimTextOutput("sample_prior_computation"))
    ),
  )
)