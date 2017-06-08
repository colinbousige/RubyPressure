library(shiny)

# library(rsconnect);deployApp()

function(input, output, session) {

   FitRes <- reactive({
      require(FME)
      inFile <- input$file1
      if (is.null(inFile))return(NULL)
      ruby <- read.table(inFile$datapath, header = FALSE,col.names=c("Energy","Intensity"))
      guess_c1 <- ruby[which.max(ruby[,2]),1]
      params <- c(A1=max(ruby[,2]),
                  A2=max(ruby[,2])*.7,
                  c1=guess_c1,
                  c2=guess_c1-50, 
                  fwhm1=10,
                  fwhm2=10,
                  y0=0)
      ModelCost <- function(P) {
         out <- sumLor(ruby[,1],P)
         return(ruby[,2]-out)  # residuals
      }
      fit <- modFit(f = ModelCost, p = params, 
                    lower = c(0,0,guess_c1-10,guess_c1-100,0,0,-Inf),
                    upper = c(Inf,Inf,guess_c1+10,guess_c1-5,50,50,Inf))
      A1    <- as.numeric(fit$par["A1"])
      A2    <- as.numeric(fit$par["A2"])
      c1    <- as.numeric(fit$par["c1"])
      c2    <- as.numeric(fit$par["c2"])
      fwhm1 <- as.numeric(fit$par["fwhm1"])
      fwhm2 <- as.numeric(fit$par["fwhm2"])
      y0    <- as.numeric(fit$par["y0"])
      cc    <- as.numeric(max(c1,c2))
      fw    <- as.numeric(c(fwhm1,fwhm2)[which.max(c(c1,c2))])
      c(A1=A1,A2=A2,c1=c1,c2=c2,fwhm1=fwhm1,fwhm2=fwhm2,y0=y0,cc=cc,fw=fw)
   })

   output$plot <- renderPlot({

      inFile <- input$file1
      if (is.null(inFile))return(NULL) 
      ruby <- read.table(inFile$datapath, header = FALSE,col.names=c("Energy","Intensity"))
      x <- seq(min(ruby[,1]),max(ruby[,1]),.01)
      A1    <- FitRes()["A1"];    A2    <- FitRes()["A2"]
      c1    <- FitRes()["c1"];    c2    <- FitRes()["c2"]
      fwhm1 <- FitRes()["fwhm1"]; fwhm2 <- FitRes()["fwhm2"]
      y0    <- FitRes()["y0"]
      cc    <- FitRes()["cc"]

      par(cex.lab=2, cex.axis=2, mgp = c(2.5, .8, 0), tck=0.02, mar=c(4, 4, .5, 2), lwd=3)
      plot(ruby,pch=16,cex=1, xlim=c(c1-100,c1+100),
           xlab="Raman Shift [1/cm]",
           ylab="Intensity [arb. units]")
      curvearea(x,y0+A1*Lorentzian(x,c1,fwhm1),col="seagreen")
      curvearea(x,y0+A2*Lorentzian(x,c2,fwhm2),col="blue")
      lines(x,sumLor(x,FitRes()[1:7]),col="red",lwd=2)
      abline(v=cc,lwd=2,lty=2)
   # }, height = function() {.5*session$clientData$output_plot_width}
   })

   Pw1 <- reactive({
      if(is.na(input$win)){
         P <- ifelse(is.na(input$Pin),0,input$Pin)
         w <- round(Pruby(laser=input$lambda,
                          P=P,
                          w0=ifelse(is.na(input$w0in),-1,input$w0in)
                         ),4)
      }
      else{ 
         w <- input$win
         P <- round(Pruby(laser=input$lambda,
                          w0=ifelse(is.na(input$w0in),-1,input$w0in),
                          w=ifelse(is.na(input$win),NA,input$win)
                          ),4)
      }
      data.frame("Pressure [GPa]"=P,"Raman Shift [1/cm]"=w, check.names=FALSE)
   })

   Pw2 <- reactive({
      inFile <- input$file1
      if (is.null(inFile)){
         cc <- fw <- P <- dP <- NA
      }else{
         cc <- FitRes()["cc"]
         fw <- FitRes()["fw"]      
         P  <- Pruby(cc,
                     laser=input$lambda,
                     w0=ifelse(is.na(input$w0in),-1,input$w0in)
                     )
         dP <- P*0.05
      }
      data.frame("Raman Shift [1/cm]"= round(cc,4), 
                 "FWHM [1/cm]"       = round(fw,4),
                 "Pressure [GPa]"    = round(P,4),
                 "dP [GPa]"          = round(dP,4),
                 check.names=FALSE
      )
   })

   output$pressure1 <- renderTable( Pw1() )
   output$pressure2 <- renderTable( Pw2() )

}

