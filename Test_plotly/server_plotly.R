library(shiny)
library(plotly)
library(FME)
library(ggplot2)
source("user_func.R")

# library(rsconnect);deployApp()

function(input, output, session) {

   RUBY <- reactive({
      inFile <- input$file1
      if (is.null(inFile))return(NULL) 
      read.table(inFile$datapath, header = FALSE,col.names=c("Energy","Intensity"))
   # ruby <- read.table("/Users/colin/Travail/Data/PostdocILM/Raman/15-09-22-Graphene_Sapphire/P_up/ruby_1_a.txt", header = FALSE,col.names=c("Energy","Intensity"))
   })

   FitRes <- reactive({
      ruby <- RUBY()
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

   output$plot <- renderPlotly({

      inFile <- input$file1
      if (is.null(inFile))return(NULL) 
      ruby <- RUBY()
      x <- seq(min(ruby[,1]),max(ruby[,1]),.5)
      A1    <- FitRes()["A1"]
      A2    <- FitRes()["A2"]
      c1    <- FitRes()["c1"]
      c2    <- FitRes()["c2"]
      fwhm1 <- FitRes()["fwhm1"]
      fwhm2 <- FitRes()["fwhm2"]
      y0    <- FitRes()["y0"]
      cc    <- FitRes()["cc"]
      
      Lor1 <- data.frame(x=x,y=y0+A1*Lorentzian(x,c1,fwhm1))
      Lor2 <- data.frame(x=x,y=y0+A2*Lorentzian(x,c2,fwhm2))
      Total <- data.frame(x=x,y=sumLor(x,FitRes()[1:7]))
      # Total <- data.frame(x=x,y=sumLor(x,fit$par))
      GP <- ggplot() + xlim(c1-150,c1+150) +
         theme(axis.text = element_text(size = 14, colour="black"),
               axis.title = element_text(size = 14),
               panel.grid.major = element_line(colour = add.alpha("black",0.2),linetype=2),
               panel.grid.minor = element_line(colour = add.alpha("black",0.1),linetype=2),
               panel.background = element_blank(),
               panel.border = element_rect(colour = "black", fill=NA, size=2)
              )+
         labs(x = "Raman Shift [1/cm]",y="Intensity [arb. units]") +
         geom_point(data=ruby,aes(Energy, Intensity)) +
         geom_area(data=Lor1,aes(x,y),fill =add.alpha("orange",0.3))+
         geom_area(data=Lor2,aes(x,y),fill =add.alpha("royalblue",0.3))+
         geom_line(data=Total,aes(x,y),col="red")+
         geom_vline(aes(xintercept = cc),linetype = "dashed")
      ggplotly()
   })

   output$pressure <- renderText({
      inFile <- input$file1
      if (is.null(inFile)){
         if(is.na(input$win))
            paste(" Expected Raman Shift at",input$Pin,"GPa :\n",
                  round(Pruby(laser=input$lambda,P=input$Pin,w0=ifelse(is.na(input$w0in),-1,input$w0in)),3),
                  "cm-1")
         else{ 
            paste(" Pressure for",input$win,"cm-1 Raman Shift :\n",
            round(Pruby(laser=input$lambda,w0=ifelse(is.na(input$w0in),-1,input$w0in),w=ifelse(is.na(input$win),NA,input$win)),3),
            "GPa")
         }
      }
      else{
         cc <- FitRes()["cc"]
         fw <- FitRes()["fw"]
         
         P  <- Pruby(cc,laser=input$lambda,w0=ifelse(is.na(input$w0in),-1,input$w0in))
         dP <- P*0.05
         paste(" Raman shift :",round(cc,3),"cm-1\n",
               "Width peak  :", round(fw,3),"cm-1\n",
               "Pressure    :", round(P,3),"+/-", round(dP,3),"GPa")
      }
  })
}

