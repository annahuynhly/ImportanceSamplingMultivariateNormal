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
  
  p("You can choose to manually input values such as the number of factors, $l_{i}$ (the 
    number of levels) and $n_{j1}, ..., n_{jm}$, the sample size. Alternatively, you can upload 
    a .csv or .txt file."),
  
  selectInput(inputId = "qual_initial_input_type",
              label = 'How do you want to enter the data?',
              choices = list("Manually" = "manual",
                             "Text file" = "txt",
                             "CSV file" = "csv"),
              selected = "manual"),
  
  conditionalPanel(
    condition = "input.qual_initial_input_type == 'manual'",
    fluidRow(
      column(3, numericInput(inputId = "qual_num_factors",
                             label = 'Insert the # of factors. This should the number of inputted 
                        $l_{i}$',
                             min = 1, max = 10000000, value = 2),
      ),
      column(3, textInput(inputId = "qual_num_levels", 
                          label = "Insert $l_{i}$, the number of levels 
                of factor $i$, $1 \\leq i \\leq m$", 
                          value = "2, 3"),
      ),
      column(3, textInput(inputId = "qual_num_n", 
                          label = "Insert $n_{j1}, ..., n_{jm}$, the sample size. The order 
                     is shown below after inputting $l_{i}$", 
                          value = "5, 5, 5, 5, 5, 5"),
      )
    ), # end fluidRow
  ), # end of conditionalPanel
  
  conditionalPanel(
    condition = "input.qual_initial_input_type != 'manual'",
    
    p("There is a specific format for the dataset. You may also download an example here:"),
    downloadButton(outputId = "qual_initial_input_example_csv", label = "Download .csv"),
    downloadButton(outputId = "qual_initial_input_example_txt", label = "Download .txt"),
    
    conditionalPanel(
      condition = "input.qual_initial_input_type == 'txt'",
      fileInput(inputId = "qual_initial_input_file_txt", 
                label = "Choose a .txt file to upload",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    ),
    
    conditionalPanel(
      condition = "input.qual_initial_input_type == 'csv'",
      fileInput(inputId = "qual_initial_input_file_csv", 
                label = "Choose a .csv file to upload",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    ),
    
  ), # end of conditionalPanel
  
  p("Given the information, the order in which you input values for $\\beta$'s and 
  $n_{j1}, ..., n_{jm}$ and is the following:"),
  verbatimTextOutput(outputId = "beta_order_output"),
  
  hr(),
  
  h4("How to Submit .txt or .csv files Containing the Data"),
  p("At the moment, this website exclusively accepts .csv or .txt files with a specific structure. 
    Ensure that your file has one header, Y, which has the following format:"),
  
  fluidRow(
    column(3, selectInput(inputId = "qual_input_type",
                          label = 'How do you want to upload the data?',
                          choices = list("Use default data" = "default",
                                         "Text file" = "txt",
                                         "CSV file" = "csv"),
                          selected = "default")),
    column(3, 
           conditionalPanel(
             condition = "input.qual_input_type == 'default'",
             p("The data is based off of the generated sample detailed in 
          the description section.")
           ),
           
           conditionalPanel(
             condition = "input.qual_input_type == 'csv'",
             fileInput(inputId = "qual_Y_input_csv", 
                       label = "Upload .csv file for Y",
                       multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain", 
                                  ".csv")),
           ),
           
           conditionalPanel(
             condition = "input.qual_input_type == 'txt'",
             fileInput(inputId = "qual_Y_input_txt", 
                       label = "Upload .txt file for Y",
                       multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain", 
                                  ".txt")),
           )),
  ),
  
  
  hr(),
  h4("Default Example"),
  
  p("There is a default example specified to illustrate the implementation of the analysis. 
    We generated a sample of $n = 30$ from a $\\mathcal{N}_{5}(\\mu, \\Sigma)$ distribution
    where:"),
  p("$\\mu = (2, 4, 6, 8, 10, 12)^{'}, \\sigma = 2$"),
  p("The generated sample can be downloaded below."),
  
  
  fluidRow(
    column(3, numericInput(inputId = "qual_default_example_seed",
                           label = "Insert the seed for generating the default example.",
                           value = 1)),
    column(3, downloadButton(outputId = "qual_Y_example_csv", label = "Download .csv"),
              downloadButton(outputId = "qual_Y_example_txt", label = "Download .txt"),),
  ),
  
  p("Alternatively, the data is just a Y vector, with the following data:"),
  
  verbatimTextOutput(outputId = "sample_Y_text_output")
  
)
