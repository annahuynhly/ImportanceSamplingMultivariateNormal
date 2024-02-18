################################################################
# DESCRIPTION FOR THE SAMPLING FROM THE PRIOR                  #
################################################################

page_sample_description = div(
  titlePanel("Description"),
  p("add some descriptions here too..."),
  p("under construction...")
)

################################################################
# SAMPLING FROM THE PRIOR (SIGMA)                              #
################################################################

page_sample_sigma = div(
  titlePanel("Sampling from the Priors on the $\\sigma_{i}^{2}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("under construction..."),
    ),
    mainPanel(
      p("under construction...")
    )
  )
)

################################################################
# SAMPLING FROM THE PRIOR (MU)                                 #
################################################################

page_sample_mu = div(
  titlePanel("Sampling from the Priors on the $\\mu_{i}$"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("under construction..."),
    ),
    mainPanel(
      p("under construction...")
    )
  )
)


################################################################
# TAB ORGANIZATION                                             #
################################################################

page_prior_sample = div(
  titlePanel("Sampling from the Prior"),
  tabsetPanel(type = "tabs",
              tabPanel("Description", page_sample_description),
              tabPanel("Sigma", page_sample_sigma),
              tabPanel("Mu", page_sample_mu)
  )
)

