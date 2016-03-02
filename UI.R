library(shiny)
library(plotly)
library(markdown)



shinyUI(fluidPage(theme = "bootstrap.css",
  tags$head(
    tags$meta(name = "description",
            content = "Densy - Visualise probability distributions intuitively and interactively."),

    tags$meta(name = "keywords",
            content = "Probability Statistics Distributions Density Visualisation Interactive"),

    tags$meta(name = "robots",
            content = "index,follow"),

    tags$script(src = "webgl.js"),

    tags$script(src = "mobile.js")
  ),

  footer = "(c) Erik-Jan van Kesteren",

  navbarPage(title=div(img(src="logo.gif", height = 20),
             "Densy // Visualise Distributions"),

    tabPanel("About",
      fluidRow(
        column(8, offset = 2,
          h1("Densy", align = "center"),
          h4("an application for distribution visualisations",
            align = "center"
            ),
          hr(),
          h2(img(src="logo2.gif", height = 50), align = "center"),
          br(),
          includeMarkdown("mkdwn/about.txt"))
        )
    ),

    navbarMenu("Univariate",

      tabPanel("Beta",
        fluidRow(
            column(8, offset = 2,
              h1("Beta", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/unibeta.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
                 sliderInput("sh1", "Shape Parameter 1", min = 0, max = 20,
                             value = 3, step = 0.1),
                 sliderInput("sh2", "Shape Parameter 2", min = 0, max = 20,
                             value = 2, step = 0.1),
               checkboxInput("cib", "Show Confidence Interval", FALSE),
               conditionalPanel("input.cib == 1",
                sliderInput("cinb", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("beta")
           )
        )
      ),

      tabPanel("Binomial",
        fluidRow(
            column(8, offset = 2,
              h1("Binomial", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/unibinom.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
                 sliderInput("nb", "Amount of Trials", min = 1, max = 100,
                             value = 20, step = 1),
                 sliderInput("thb", "Success Probability", min = 0, max = 1,
                             value = 0.5, step = 0.01),
               checkboxInput("cibi", "Show Confidence Interval", FALSE),
               conditionalPanel("input.cibi == 1",
                sliderInput("cinbi", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("binom")
           )
        )
      ),

      tabPanel("Cauchy",
        fluidRow(
            column(8, offset = 2,
              h1("Cauchy", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/unicauchy.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
                 sliderInput("locc", "Location", min = -4, max = 4,
                             value = 0, step = 0.1),
                 sliderInput("scc", "Scale", min = 0.1, max = 5,
                             value = 1, step = 0.1),
               checkboxInput("cic", "Show Confidence Interval", FALSE),
               conditionalPanel("input.cic == 1",
                sliderInput("cinc", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("cauchy")
           )
        )
      ),

      tabPanel("Chi-Squared",
        fluidRow(
            column(8, offset = 2,
              h1("Chi-Squared", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/unichisq.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
                 sliderInput("df", "Degrees of Freedom", min = 1, max = 30,
                             value = 5, step = 1),
               checkboxInput("cich", "Show Confidence Interval", FALSE),
               conditionalPanel("input.cich == 1",
                sliderInput("cinch", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("chisq")
           )
        )
      ),

      tabPanel("F",
        fluidRow(
          column(8, offset = 2,
            h1("under construction", align = "center"),
            br(),
            hr(),
            br(),
            h1(img(src = "load256.gif", height = 55),
            align = "center")
            )
        )
      ),

      tabPanel("Gamma",
        fluidRow(
            column(8, offset = 2,
              h1("Gamma", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/unigamma.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
                 sliderInput("shg", "Shape", min = 0.1, max = 25,
                             value = 1.5, step = 0.1),
                 sliderInput("scg", "Scale", min = 0.1, max = 5,
                             value = 1.1, step = 0.1),
               checkboxInput("cig", "Show Confidence Interval", FALSE),
               conditionalPanel("input.cig == 1",
                sliderInput("cing", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("gamma")
           )
        )
      ),

      tabPanel("Normal",
        fluidRow(
            column(8, offset = 2,
              h1("Univariate Normal", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/uninorm.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("mean", "Mean", min = -2.5, max = 2.5,
                           value = 0, step = 0.1),
               sliderInput("sd", "Standard Deviation", min = 0.1,
                           max = 2.5, value = 1.0, step = 0.1),
               checkboxInput("ci", "Show Confidence Interval", FALSE),
               conditionalPanel("input.ci == 1",
                sliderInput("cin", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("uninorm")
           )
        )
      ),

      tabPanel("Normal Bimodal",
        fluidRow(
            column(8, offset = 2,
              h1("Normal Bimodal Mixture", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/uninormb.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("delta", "Delta", min = 0,
                           max = 8, value = 3.2, step = 0.1),
               sliderInput("mix", "Mixture", min = 0,
                           max = 1, value = 0.3, step = 0.01),
               br(),
               "Note: no CI yet! this is kind of complicated.",
               width = 12
             )
           ),
           column(9,
             plotlyOutput("uninormb")
           )
        )
      ),

      tabPanel("Student's T",
        fluidRow(
            column(8, offset = 2,
              h1("Student's T", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/unistut.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("df", "Degrees of Freedom", min = 1, max = 100,
                           value = 1, step = 1),
               sliderInput("ncp", "Non-Centrality Parameter", min = -3,
                           max = 3, value = 0, step = 0.1),
               checkboxInput("cit", "Show Confidence Interval", FALSE),
               conditionalPanel("input.cit == 1",
                sliderInput("cint", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("stut")
           )
        )
      ),

      tabPanel("Uniform",
        fluidRow(
            column(8, offset = 2,
              h1("Uniform", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/uniuni.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
                 sliderInput("min", "Minimum", min = -10, max = 10,
                             value = -2, step = 0.1),
                 sliderInput("wid", "Width", min = 0, max = 10,
                             value = 4, step = 0.1),
               checkboxInput("ciu", "Show Confidence Interval", FALSE),
               conditionalPanel("input.ciu == 1",
                sliderInput("cinu", "Confidence Interval", min = 0.01,
                            max = 0.99, value = 0.95)),
               width = 12
             )
           ),
           column(9,
             plotlyOutput("uniform")
           )
        ))

    ),

    navbarMenu("Bivariate",

      tabPanel("Beta",
        conditionalPanel("webgl_detect() == 1 && window.mobilecheck() == 0",
        fluidRow(
            column(8, offset = 2,
              h1("Bivariate Beta", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/bivbeta.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("bsh1", "Shape X", min = 1, max = 12,
                           value = 5, step = 0.1),
               sliderInput("bsh2", "Shape Y", min = 1, max = 12,
                           value = 2, step = 0.1),
               sliderInput("bsh3", "Shape XY", min = 1, max = 12,
                           value = 3, step = 0.1),
               selectInput("bprec", "Precision:",
                           choices = c("Normal", "High")),
               width = 12,
               br(),
               "Note: High precision is slower."
             )
           ),
           column(9,
             plotlyOutput("bivbeta", height = 600)
           )
           )
           ),
           conditionalPanel("webgl_detect() == 0 || window.mobilecheck() == 1",
             h1(img(src = "load256.gif", height = 55),
                align = "center"),
             br(),
             h4("Your device does not support Bivariate Distributions!",
                align = "center")
           )
      ),


      tabPanel("Gamma",
        conditionalPanel("webgl_detect() == 1 && window.mobilecheck() == 0",
        fluidRow(
            column(8, offset = 2,
              h1("Bivariate Gamma", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/bivgamma.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("ga1", "Shape X", min = 1, max = 20,
                           value = 5, step = 0.1),
               sliderInput("ga2", "Shape Y", min = 1, max = 20,
                           value = 3, step = 0.1),
               sliderInput("gb", "Scale", min = 1, max = 10,
                           value = 2, step = 0.1),
               selectInput("gprec", "Precision:",
                           choices = c("Normal", "High")),
               br(),
               "Note: This distribution is not normalised.
                High precision is slower.",
               width = 12
             )
           ),
           column(9,
             plotlyOutput("bivgamma", height = 600)
           )
        )
        ),
        conditionalPanel("webgl_detect() == 0 || window.mobilecheck() == 1",
          h1(img(src = "load256.gif", height = 55),
             align = "center"),
          br(),
          h4("Your device does not support Bivariate Distributions!",
          align = "center")
        )
      ),

      tabPanel("Normal",
        conditionalPanel("webgl_detect() == 1 && window.mobilecheck() == 0",
        fluidRow(
            column(8, offset = 2,
              h1("Bivariate Normal", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/bivnorm.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("corr", "Correlation", min = 0, max = 0.99,
                           value = 0.3),
               sliderInput("s", "Standard Deviation", min = 0.0,
                           max = 2.5, value = 1.0),
               selectInput("prec", "Precision:", choices = c("Normal", "High")),
               width = 12,
               br(),
               "Note: SD of X and Y are assumed equal.
               High precision is slower."
             )
           ),
           column(9,
             plotlyOutput("bivnorm", height = 600)
           )
        )
        ),
        conditionalPanel("webgl_detect() == 0 || window.mobilecheck() == 1",
          h1(img(src = "load256.gif", height = 55),
             align = "center"),
          br(),
          h4("Your device does not support Bivariate Distributions!",
          align = "center")
        )
      ),

      tabPanel("Uniform",
        conditionalPanel("webgl_detect() == 1 && window.mobilecheck() == 0",
        fluidRow(
            column(8, offset = 2,
              h1("Bivariate Uniform", align = "center"),
              hr(),
              h5(includeMarkdown("mkdwn/bivunif.txt"), align = "center"),
              br()
            )
        ),

        fluidRow(
           column(3,
             sidebarPanel(
               sliderInput("bux", "X range", min = -3.5, max = 3.5,
                           value = c(-1,1)),
               sliderInput("buy", "Y range", min = -3.5,
                           max = 3.5, value = c(-1,1)),
               selectInput("uprec", "Precision:", choices = c("Normal", "High")),
               width = 12,
               br(),
               "Note: High precision is slower."
             )
           ),
           column(9,
             plotlyOutput("bivunif", height = 600)
           )
        )
        ),
        conditionalPanel("webgl_detect() == 0 || window.mobilecheck() == 1",
          h1(img(src = "load256.gif", height = 55),
             align = "center"),
          br(),
          h4("Your device does not support Bivariate Distributions!",
          align = "center")
        )
      )
    ),

    tabPanel("Version",
      fluidRow(
        column(8, offset = 2,
          h1("Densy", align = "center"),
          h4("an application for distribution visualisations",
            align = "center"
            ),
          hr(),
          h1(img(src = "load256.gif", height = 55),
          align = "center"),
          br(),
          h5(includeMarkdown("mkdwn/version.txt"), align = "center")
        )
      )
    ),

    windowTitle = "Densy",

    collapsible = TRUE
  )


))
