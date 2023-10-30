
page_samplingdescription = div(
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
  
  withMathJax(),
  # end of adding latex
  
  h4("Generation of the Prior:"),
  p("Generate: $\\frac{1}{\\sigma_{ii}} \\sim \\gamma(\\alpha_{01i}, \\alpha_{02i}), i = 1, 2, ..., p$. 
    Let $\\triangle = diag(\\sqrt{\\sigma_{11}}, ..., \\sqrt{\\sigma_{pp}})$"),
  p("$R \\sim uniform$ on the set of all $p \\times p$ correlation matrices, and 
    $\\Sigma = \\triangle^{1/2} R \\triangle^{1/2} $, 
    $\\mu | \\Sigma \\sim N_{p}(\\mu_{0}, \\sigma^{2}_{0} \\Sigma)$"),
  p("So the hyperparameters as determied by the elicitation are 
    $(\\alpha_{01i}, \\alpha_{02i}), i = 1, ..., p$, $\\mu_{0}$, and $\\sigma^{2}_{0}$."),
  
  h4("Generation of the Posterior:"),
  p("Suppose $y_{1}, ..., y_{n}$ is a sample from a distribution on $\\mathbb{R}^{n}$ 
        with mean $\\underset{\\sim}{\\mu} \\in \\mathbb{R}^{n}$ and variance matrix 
        $\\sum \\in \\mathbb{R}^{p \\times p}$"),
  p("Let $Y = \\begin{pmatrix}
                  \\underset{\\sim}{y_{1}^{'}} \\\\
                  \\underset{\\sim}{y_{2}^{'}} \\\\
                  ...\\\\
                  \\underset{\\sim}{y_{n}^{'}} \\\\
                  \\end{pmatrix} = (y_{ij}) \\in \\mathbb{R}^{n \\times p}$ then, 
    $\\bar{\\underset{\\sim}{y}} = Y^{'}\\underset{\\sim}{I_{n}}/n =
        \\begin{pmatrix}
                  \\sum_{i=1}^{n} y_{i1}/n \\\\
                  \\sum_{i=1}^{n} y_{i2}/n \\\\
                  ...\\\\
                  \\sum_{i=1}^{n} y_{in}/n \\\\
        \\end{pmatrix} \\in \\mathbb{R}^{p}$ is the sample mean vector, where 
        $\\underset{\\sim}{I_{n}} = \\begin{pmatrix}
                  1 \\\\
                  1 \\\\
                  ...\\\\
                  1 \\\\
        \\end{pmatrix} \\in \\mathbb{R}^{n}$"),
  p("$S = \\frac{1}{n-1}(Y - \\underset{\\sim}{I_{n}} \\bar{\\underset{\\sim}{y}})^{'}
    (Y - \\underset{\\sim}{I_{n}} \\bar{\\underset{\\sim}{y}})
    = \\frac{1}{n-1} \\sum_{i=1}^{n} (y_{ki} - \\bar{y_{i}})(y_{kj} - \\bar{y_{j}})$"),
  
)

page_elicitprior = div(
  titlePanel("Elicitation from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      numericInput(inputId = "virtual_uncertainty",
                   label = 'Insert the virtual uncertainty, $\\gamma$.',
                   value = 0.99),
    ),
    mainPanel(
      withSpinner(verbatimTextOutput("elicit_prior_computation"))
    ),
  )
)

page_priorsample = div(
  titlePanel("Sampling from the Prior"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "prior_bigN",
                   label = 'Insert N, the length of vectors $\\mu$ and $\\sigma$.',
                   value = 100),
      
      p("Remark: the values show only the first 10 rows for mu and sigma, and then only 1 matrix 
      for R. You may download them below by pressing their respective icons below."),
      
      downloadButton("priorsample_download_mu", "$\\mu$"),
      downloadButton("priorsample_download_sigma", "$\\Sigma$"),
      downloadButton("priorsample_download_R", "$R$"),
      
      selectInput(inputId = "priorsample_use", 
                  label = 'Do you want to use the values from the previous section?', 
                  choices = list("Yes" = "y", "No" = "n"), 
                  selected = "n"
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use == 'yes'",
        p("The values from the previous section will be used!")
      ),
      
      conditionalPanel(
        condition = "input.priorsample_use == 'n'",
        textInput(
          inputId = "alpha01",
          label = "Insert the vector of $\\alpha_{011}, ..., \\alpha_{01p}$",
          value = "1, 1, 1, 1, 1"),
        textInput(
          inputId = "alpha02",
          label = "Insert the vector of $\\alpha_{021}, ..., \\alpha_{02p}$",
          value = "1, 1, 1, 1, 1"),
        numericInput(inputId = "mu_0",
                     label = 'Insert $\\mu_{0}$',
                     value = 0),
        numericInput(inputId = "sigma_0",
                     label = 'Insert $\\sigma_{0}$',
                     value = 1),
      ),
      
    ),
    mainPanel(
      withSpinner(verbatimTextOutput("sample_prior_computation"))
    ),
  )
)

page_posteriorsample = div(

  titlePanel("Integrating with Respect to the Posterior"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "post_n",
                   label = 'Insert n, the length of Y',
                   value = 100),
      numericInput(inputId = "post_bigN",
                   label = 'Insert N, the monte carlo sample size',
                   value = 1000),
      
      p("Remark: select values are shown. You may download to see the rest."),
      
      downloadButton("postsample_download_mu", "$\\mu$"),
      downloadButton("postsample_download_sigma", "$\\Sigma$"),
      downloadButton("postsample_download_k_zeta", "$K(\\zeta)$"),
      
      selectInput(inputId = "postsample_use", 
                  label = 'Which kind of values do you want to use?', 
                  choices = list("Input values" = 1, 
                                 "Same values as elicitation" = 2,
                                 "Same values as prior" = 3), 
                  selected = 1),
      
      conditionalPanel(
        condition = "input.postsample_use == 2",
        p("The values from the elicitation of the prior will be used!")
      ),
      conditionalPanel(
        condition = "input.postsample_use == 3",
        p("The values inputted in the sampling of the prior will be used!")
      ),
      
      conditionalPanel(
        condition = "input.postsample_use == 1",
        textInput(
          inputId = "alpha01_ver2",
          label = "Insert the vector of $\\alpha_{011}, ..., \\alpha_{01p}$",
          value = "1, 1, 1, 1, 1"),
        textInput(
          inputId = "alpha02_ver2",
          label = "Insert the vector of $\\alpha_{021}, ..., \\alpha_{02p}$",
          value = "1, 1, 1, 1, 1"),
        numericInput(inputId = "mu_0_ver2",
                     label = 'Insert $\\mu_{0}$',
                     value = 0),
        numericInput(inputId = "sigma_0_ver2",
                     label = 'Insert $\\sigma_{0}$',
                     value = 1),
      ),
      
    ),
    mainPanel(
      withSpinner(verbatimTextOutput("sample_post_computation"))
    ),
  )
)

################################################
# graph portion.

page_priorgraph = div(
  titlePanel("Plots for $\\mu$ Sampled from the Prior"), 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "mu_col", 
                   label = 'The column of $\\mu$ used to generate the graph.',
                   value = 1),
      sliderInput(
        inputId = "graph_delta",
        label = "The meaningful difference (length of the bins)",
        min = 0.01, max = 1, value = 0.1,
      ),
      
      #numericInput(inputId = "graph_delta",
      #             label = 'The meaningful difference (length of the bins).',
      #             value = 0.1),
      textInput(inputId = "prior_colour_hist",
                label = 'Input the colour of the histogram',
                value = "6699FF"
      ), 
      textInput(inputId = "prior_colour_line",
                label = 'Input the colour of the line',
                value = "FF6666"
      ), 
      selectInput(inputId = "prior_lty_type", 
                  label = 'Select a line type', 
                  choices = list("0" = 0, "1" = 1, "2" = 2, 
                                 "3" = 3, "4" = 4, "5" = 5, "6" = 6),
                  selected = 2
      ),
      sliderInput(inputId = "prior_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.2
      ), 
      
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("33%", "33%"), 
            withSpinner(plotOutput("sample_prior_graph")), 
            withSpinner(plotOutput("sample_post_graph")),
            withSpinner(plotOutput("sample_rbr_graph"))
          )
        ),
      ),
    ),
  ),
)



