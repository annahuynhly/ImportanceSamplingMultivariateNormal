# qualitative regression

page_qualitativedesc = div(
  titlePanel("Description"),
  p("Insert a description here for the qualitative factors case.")
)

page_qualitativeinputs = div(
  titlePanel("Inputs"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      numericInput(inputId = "qual_num_factors",
                   label = 'Insert the # of factors.',
                   min = 1, max = 10000000, value = 3),
      
      textInput(inputId = "qual_num_levels", 
                label = "Insert $l_{i}$, the number of levels 
                of factor $i$, $1 \\leq i \\leq m$", 
                value = "2, 3, 4"),
      
      textInput(inputId = "qual_num_n", 
                label = "Insert $n_{j1}, ..., $, ", 
                value = "2, 3, 4"),
      
    ),
    mainPanel(
      p("mwahahaha nothing yet...")
    ),
  )
)


################################################################
# TAB ORGANIZATION                                             #
################################################################

page_qualitative_reg = div(
  titlePanel("Qualitative Factors Only - Linear Regression"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_qualitativedesc),
              tabPanel("Inputs", page_qualitativeinputs),
  )
)