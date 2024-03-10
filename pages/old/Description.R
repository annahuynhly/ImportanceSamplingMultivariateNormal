

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
