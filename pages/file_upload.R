################################################################
# FILE UPLOAD INFORMATION - INSTRUCTIONS FOR USER              #
################################################################

# instructions for prior elicitation - sigma (text file)
prior_sigma_upload_instructions = div(
  p("In order for this to work, the file must contain the following headers:"),
  br(),
  p("s1: as defined in the description."),
  p("s2: as defined in the description."),
  p("lower_bd: this is the lower bound value for the initial value of the iteration to determine alpha0i's."),
  p("upper_bd: this is the upper bound value for the initial value of the iteration to determine alpha0i's."),
  br(),
  p("Check below for a working sample for how the upload file should be uploaded.")
)

# instructions for prior elicitation - mu (text file)
prior_mu_upload_instructions = div(
  p("In order for this to work, the file must contain the following headers:"),
  br(),
  p("m1: as defined in the description."),
  p("m2: as defined in the description."),
  br(),
  p("Check below for a working sample for how the upload file should be uploaded.")
)

# this is the original one - for the posterior
file_upload_example = div(
  p("At the moment, this website exclusively accepts .csv files with a specific structure.  
    Ensure that your file includes a header, where each column corresponds to a distinct 
    variable:"),
  br(),
  p("Y = (Y1, Y2, Y3, ..., YN)"),
  br(),
  p("You can download an example below of an example for the format:"),
)
