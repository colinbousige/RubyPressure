library(shiny)
library(plotly)

# library(rsconnect);deployApp()

lambda <- 532
Pin    <- 0
w0in   <- NA
win    <- NA


fluidPage(
  titlePanel("Ruby Pressure"),
  sidebarLayout(
    sidebarPanel(
      numericInput("lambda", 'Laser wavelength [nm]', lambda),
      numericInput("Pin", 'Pressure [GPa]', Pin),
      numericInput("win", 'Raman Shift [1/cm]', win),
      numericInput("w0in", 'Experimental Raman Shift at 0 GPa', w0in),
      # tags$hr(),
      fileInput("file1", "Choose Ruby File for Experimental Value",
                 accept = c("text",".txt",".dat","text/plain",".csv") )
    ),
    mainPanel(
      verbatimTextOutput('pressure'),
      plotlyOutput('plot')
    )
  )
)

