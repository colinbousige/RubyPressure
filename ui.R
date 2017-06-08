library(shiny)

# library(rsconnect);deployApp()

lambda <- 532
Pin    <- 0
w0in   <- NA
win    <- NA


fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Ruby Pressure"),
      numericInput("lambda", 'Laser wavelength [nm]', lambda),
      numericInput("Pin", 'Pressure [GPa]', Pin),
      numericInput("win", 'Raman Shift [1/cm]', win),
      numericInput("w0in", 'Experimental Raman Shift at 0 GPa', w0in),
      # tags$hr(),
      fileInput("file1", "Choose Ruby File for Experimental Value",
                 accept = c("text",".txt",".dat","text/plain",".csv") )
    ),
    mainPanel(
      h3("Manual tweaks:"),
      # verbatimTextOutput('pressure1'),
      tableOutput('pressure1'),
      h3("Fit result:"),
      # verbatimTextOutput('pressure2'),
      tableOutput('pressure2'),
      # plotOutput('plot', height="auto")
      plotOutput('plot')
    )
  )
)

