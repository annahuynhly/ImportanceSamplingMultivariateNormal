# contact page

page_contact = fluidPage(
  
  
  tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
  withAnim(),
  
  #div(id = "title_test", titlePanel("Contact & Credits")),
  titlePanel("Contact & Credits"),
  tags$div(
    "We constructed this website using ",
    tags$a(href="https://www.r-project.org/about.html", "R."),
    "Specifically, we used the ",
    tags$a(href="https://shiny.rstudio.com/", "R Shiny "),
    "package. The website theme is journal from ",
    tags$a(href="https://rstudio.github.io/shinythemes/", "shinythemes."),
  ),
  p(""),
  p('This website is maintained by Anna Ly. If you find any bugs on this website, please contact 
    [firstname]huynh.[lastname]@mail.utoronto.ca.'),
  p(""),
  p("The weird formatting of the email is to avoid spam."),
  p(""),
  
  tags$style("#project-grid {
                      display: grid;
                      grid-template-columns: 120px 1fr;
                      grid-gap: 10px;
                      }"),
  div(id = "project-grid",
      div(id = "AnnaImg", img(src = "anna_ly.jpg", style = 'border-radius: 50%', width = '120px')),
      div(h3('Anna Ly'),
          h4('Research Assistant @ University of Toronto'),
          p('Main programmer & maintainer of this website. Applied statistics specialist and 
            mathematical sciences major ', style = "color:#61646b"),
          p("alumni from the University of Toronto.", style = "color:#61646b"),
          #tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
          tags$div(
            tags$i(class = "fa-brands fa-github"),
            tags$a(href="https://github.com/annahuynhly", "Github"), " | ",
            tags$i(class = "fa-brands fa-linkedin"),
            tags$a(href="https://www.linkedin.com/in/anna-ly-statistics-specialist/", "Linkedin"), " | ",
            tags$i(class = "fa-solid fa-graduation-cap"),
            tags$a(href="https://scholar.google.ca/citations?user=9w41oS8AAAAJ&hl=en", "Google Scholar")
          ),
      ),
      div(id = "MikeImg", img(src = "mike_evans.PNG", style = 'border-radius: 50%', width = '120px')),
      div(h3('Michael Evans'),
          h4('Department Chair of Statistical Sciences & Professor @ University of Toronto'),
          p('The main author of the paper. Supplied most of the R codes for graph generation & computations.
            Assisted with', style = "color:#61646b"),
          p('designing the user layout of the website.', style = "color:#61646b"),
          tags$div(
            tags$i(class = "fa-solid fa-user"),
            tags$a(href="https://utstat.toronto.edu/mikevans/", "Personal Website"), " | ",
            tags$i(class = "fa-solid fa-graduation-cap"),
            tags$a(href="https://scholar.google.ca/citations?user=i4Z5iW4AAAAJ&hl=en", "Google Scholar"),
          ),
      ),
      
      div(id = "LuaiImg", img(src = "luai_labadi.png", style = 'border-radius: 50%', width = '120px')),
      div(h3('Luai Al Labadi'),
          h4('Associate Chair & Assistant Professor @ University of Toronto'),
          p('Assisted with codes and overseeing the website.', 
            style = "color:#61646b"),
          tags$div(
            tags$i(class = "fa-solid fa-building-columns"),
            tags$a(href="https://www.utm.utoronto.ca/math-cs-stats/people/luai-al-labadi", "UofT Webpage"),
            tags$i(class = "fa-solid fa-graduation-cap"), " | ",
            tags$a(href="https://scholar.google.ca/citations?user=DIin_xEAAAAJ&hl=en", "Google Scholar"),
          ),
      ),
      
  ),
  
)