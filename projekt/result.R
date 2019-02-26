library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
ui <- navbarPage("Quanto",
                 tabPanel(" ",
                          tags$img(src='drugi.png', height = 730, width = 1300)),
                 tabPanel(" ",
                          tags$img(src='trzeci.png', height = 730, width = 1300)),
                 tabPanel(" ",
                          tags$img(src='piaty.png', height = 730, width = 1300)),
                 tabPanel(" ",
                          tags$img(src='szosty.png', height = 730, width = 1300)),
                 tabPanel("Histogramy część A",
                          wellPanel(
                            fluidRow(
                              column(3,
                                     #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #525354")),
                                     #tags$style(HTML(".control-label{color:white}")),
                                     sliderInput(inputId = "reh", label = "Ilość rehedgingów", 
                                                 value = 1, min = 1, max = 252),
                                     sliderInput(inputId = "fi", label = "Korelacja",
                                                 value = fiA, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigma1", label = "Zmienność złota",
                                                 value = sigma_gold, min = 0, max = 1),
                                     sliderInput(inputId = "sigma2", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r1", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r2", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "mi1", label = "Dryf złota",
                                                 value = mi_gold, min = 0, max = 1),
                                     sliderInput(inputId = "mi2", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          actionButton("Update", "Update!"),
                          fluidRow(
                            column(6, plotOutput("hist1", height = "400px")),
                            column(6, plotOutput("hist2", height = "400px"))
                            #column(4, fluidRow(plotOutput("analiza1"), width = "100%", height = "50%"), fluidRow(plotOutput("analiza2"), width = "100%"))
                          )
                          
                          
                          ),
                 tabPanel("Analiza Wrażliwości",
                          wellPanel(
                            fluidRow(
                              column(3, 
                                     checkboxInput(inputId = "ch1", label = "Dodaj wykres", value = FALSE, width = NULL),
                                     sliderInput(inputId = "fi2", label = "Korelacja",
                                                 value = fiA, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigma12", label = "Zmienność złota",
                                                 value = sigma_gold, min = 0, max = 1),
                                     sliderInput(inputId = "sigma22", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r12", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r22", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "mi12", label = "Dryf złota",
                                                 value = mi_gold, min = 0, max = 1),
                                     sliderInput(inputId = "mi22", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          actionButton("Update2", "Update!"),
                          fluidRow(
                            column(6, plotOutput("plot", height = "400px")),
                            column(6, plotOutput("plot2", height = "400px"))
                          )
                          
                          #fluidRow(
                            #column(9, plotOutput("plot", height = "500px"), plotOutput("plot2"), height = "500px"),
                            #column(3, fluidRow(plotOutput("hist3"), height = "50px"), fluidRow(plotOutput("hist4"), height = "50px"))
                            #column(3,plotOutput("hist3"), height = "25px",plotOutput("hist4"), height = "25px")
                          #)
                          
                          
                 ),
                 tabPanel("Skład portfela",
                          wellPanel(
                            fluidRow(
                              column(3, 
                                     sliderInput(inputId = "reh3", label = "Ilość rehedgingów", 
                                                 value = 1, min = 1, max = 252),
                                     sliderInput(inputId = "fi3", label = "Korelacja",
                                                 value = fiA, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigma13", label = "Zmienność złota",
                                                 value = sigma_gold, min = 0, max = 1),
                                     sliderInput(inputId = "sigma23", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r13", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r23", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "mi13", label = "Dryf złota",
                                                 value = mi_gold, min = 0, max = 1),
                                     sliderInput(inputId = "mi23", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          actionButton("Update3", "Update!"),
                          fluidRow(
                            column(6, plotOutput("skladopcje", height = "220px"), plotOutput("skladgotowka", height = "220px")),
                            column(6, plotOutput("skladdeltaz", height = "220px"), plotOutput("skladdeltadol", height = "220px"))
                          )
                          
                          
                 ),
                 tabPanel(" ",
                          tags$img(src='siodmy.png', height = 730, width = 1300)),
                 tabPanel(" ",
                          tags$img(src='osmy.png', height = 730, width = 1300)),
                 tabPanel("Histogramy część B",
                          wellPanel(
                            fluidRow(
                              column(3, 
                                     sliderInput(inputId = "reh4", label = "Ilość rehedgingów", 
                                                 value = 1, min = 1, max = 252),
                                     sliderInput(inputId = "fi4", label = "Korelacja",
                                                 value = fiB, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigma14", label = "Zmienność złota",
                                                 value = sigma_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "sigma24", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r14", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r24", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "mi14", label = "Dryf złota",
                                                 value = mi_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "mi24", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          actionButton("Update4", "Update!"),
                          fluidRow(
                            column(6, plotOutput("histB1", height = "400px")),
                            column(6, plotOutput("histB2", height = "400px"))
                            #column(2, fluidRow(plotOutput("plot1")), fluidRow(plotOutput("plot2")))
                          )
                          
                          
                 ),
                 tabPanel("Analiza Wrażliwości",
                          wellPanel(
                            fluidRow(
                              column(3, 
                                     checkboxInput(inputId = "ch2", label = "Dodaj wykres", value = FALSE, width = NULL),
                                     sliderInput(inputId = "fi5", label = "Korelacja",
                                                 value = fiB, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigma15", label = "Zmienność złota",
                                                 value = sigma_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "sigma25", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r15", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r25", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "mi15", label = "Dryf złota",
                                                 value = mi_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "mi25", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          actionButton("Update5", "Update!"),
                          fluidRow(
                                   column(6, plotOutput("analizaB1", height = "350px")),
                                   column(6, plotOutput("analizaB2", height = "350px"))
                                   #column(2, fluidRow(plotOutput("plot1")), fluidRow(plotOutput("plot2")))
                          )
                          
                          
                 ),
                 tabPanel("Skład portfela",
                          wellPanel(
                            fluidRow(
                              column(3, 
                                     sliderInput(inputId = "reh6", label = "Ilość rehedgingów", 
                                                 value = 1, min = 1, max = 252),
                                     sliderInput(inputId = "fi6", label = "Korelacja",
                                                 value = fiB, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigma16", label = "Zmienność złota",
                                                 value = sigma_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "sigma26", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r16", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r26", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "mi16", label = "Dryf złota",
                                                 value = mi_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "mi26", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          actionButton("Update6", "Update!"),
                          fluidRow(
                            column(6, plotOutput("skladopcjeB", height = "220px"), plotOutput("skladgotowkaB", height = "220px")),
                            column(6, plotOutput("skladdeltazB", height = "220px"), plotOutput("skladdeltadolB", height = "220px"))
                          )
                          
                          
                 ),
                
                 tabPanel("Porównanie A i B",
                          wellPanel(
                            fluidRow(
                              column(3, 
                                     sliderInput(inputId = "reh7", label = "Ilość rehedgingów", 
                                                 value = 1, min = 1, max = 252),
                                     sliderInput(inputId = "fiA7", label = "Korelacja A",
                                                 value = fiA, min = -0.99, max = 0.99),
                                     sliderInput(inputId = "fiB7", label = "Korelacja B",
                                                 value = fiB, min = -0.99, max = 0.99)
                              ),
                              column(3,
                                     sliderInput(inputId = "sigmaA17", label = "Zmienność złota USD",
                                                 value = sigma_gold, min = 0, max = 1),
                                     sliderInput(inputId = "sigmaB17", label = "Zmienność złota PLN",
                                                 value = sigma_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "sigma27", label = "Zmienność USDPLN",
                                                 value = sigma_USDPLN, min = 0, max = 1)),
                              column(3,
                                     sliderInput(inputId = "r17", label = "Oprocentowanie złotówki",
                                                 value = r_PL, min = 0.00, max = 0.1129), #Od 0 do 2 razy dzisiejsze oprocentowanie
                                     sliderInput(inputId = "r27", label = "Oprocentowanie dolara",
                                                 value = r_USA, min = 0, max = 0.04142)),
                              column(3,
                                     sliderInput(inputId = "miA17", label = "Dryf złota USD",
                                                 value = mi_gold, min = 0, max = 1),
                                     sliderInput(inputId = "miB17", label = "Dryf złota PLN",
                                                 value = mi_gold_PLN, min = 0, max = 1),
                                     sliderInput(inputId = "mi27", label = "Dryf USDPLN",
                                                 value = mi_USDPLN, min = 0, max = 1))
                            )
                          ),
                          fluidRow(column(6, plotOutput("hist5", height = "300px")),
                                   column(6, plotOutput("hist6", height = "300px"))
                                   #column(2, fluidRow(plotOutput("plot1")), fluidRow(plotOutput("plot2")))
                          )
                          
                          
                 )
                 
                 
                 )



server <- function(input, output) {
  output$hist1 <- renderPlot({
    PiA <- c()
    for (i in 1:1000) PiA[i]<-Pi(gold[length(gold)], 253/253, input$fi, input$sigma1,
                                 input$sigma2, input$r1, input$r2, input$reh, input$mi1, input$mi2, gold[length(gold)], USDPLN[length(USDPLN)])
    
    data_frame(v1 = PiA) %>%
      ggplot(., aes(v1)) + 
      geom_histogram(color='#000000',fill='#428bca', binwidth=0.3)+
      labs(x='zysk/strata', y='liczebnosc')+xlim(c(-10, 10))+
      ylim(c(0,600))
  })
  output$hist2 <- renderPlot({
    input$Update
    rehedging <- isolate(input$reh)
    PiA2 <- c()
    for (i in 1:1000) PiA2[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold,
                                  sigma_USDPLN, r_PL, r_USA, rehedging, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])
    data_frame(v2 = PiA2) %>%
      ggplot(., aes(v2)) + 
      geom_histogram(color='#000000',fill='#cc2100', binwidth=0.3)+
      labs(x='zysk/strata', y='liczebnosc')+xlim(c(-10, 10))+
      ylim(c(0,600))
  })
  
   #DRUGI SLAJD 
  
  data <- eventReactive(input$Update2, {
    quantoA2 <- matrix(0,6,1000)
    for (i in 1:1000){
      quantoA2[1,i] <- Pi(gold[length(gold)], 253/253, input$fi2, input$sigma12,
                          input$sigma22, input$r12, input$r22, 1, input$mi12, input$mi22, gold[length(gold)], USDPLN[length(USDPLN)])
      quantoA2[2,i] <- Pi(gold[length(gold)], 253/253, input$fi2, input$sigma12,
                          input$sigma22, input$r12, input$r22, 10, input$mi12, input$mi22, gold[length(gold)], USDPLN[length(USDPLN)])
      quantoA2[3,i] <- Pi(gold[length(gold)], 253/253, input$fi2, input$sigma12,
                          input$sigma22, input$r12, input$r22, 50, input$mi12, input$mi22, gold[length(gold)], USDPLN[length(USDPLN)])
      quantoA2[4,i] <- Pi(gold[length(gold)], 253/253, input$fi2, input$sigma12,
                          input$sigma22, input$r12, input$r22, 100, input$mi12, input$mi22, gold[length(gold)], USDPLN[length(USDPLN)])
      quantoA2[5,i] <- Pi(gold[length(gold)], 253/253, input$fi2, input$sigma12,
                          input$sigma22, input$r12, input$r22, 126, input$mi12, input$mi22, gold[length(gold)], USDPLN[length(USDPLN)])
      quantoA2[6,i] <- Pi(gold[length(gold)], 253/253, input$fi2, input$sigma12,
                          input$sigma22, input$r12, input$r22, 252, input$mi12, input$mi22, gold[length(gold)], USDPLN[length(USDPLN)])}
    
    kwantyleA2 <- matrix(0, 6, 5)
    for (i in 1:6){
      kwantyleA2[i,1]<-quantile(quantoA2[i,], probs = 0.1)
      kwantyleA2[i,2]<-quantile(quantoA2[i,], probs = 0.25)
      kwantyleA2[i,3]<-quantile(quantoA2[i,], probs = 0.5)
      kwantyleA2[i,4]<-quantile(quantoA2[i,], probs = 0.75)
      kwantyleA2[i,5]<-quantile(quantoA2[i,], probs = 0.9)
    }
    return(kwantyleA2)
  })
  output$plot <- renderPlot({
    if (input$ch1){
      if (data()[1,1] < kwantyleA[1,1]){
        matplot(data(), type = "l", lwd = 2, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
        matlines(kwantyleA, type = "l", lwd = 1, lty = 1, col = c("black", "black", "green", "black", "black"))
        axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
        grid(NULL, NULL, col = "gray")}
      else {
        matplot(kwantyleA, type = "l", lwd = 1, lty = 1, col = c("black", "black", "green", "black", "black"), xaxt = "n", ylab = "zysk/strata")
        matlines(data(), type = "l", lwd = 2, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"))
        axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
        grid(NULL, NULL, col = "gray")
      }
    }
    else{
      matplot(data(), type = "l", lwd = 2, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
      axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
      grid(NULL, NULL, col = "gray")
    }
    
  })
  output$plot2 <- renderPlot({
    #matplot(data()[c(4,5,6),], type = "l", lwd = 2, lty = 1)
    if (input$ch1){
      matplot(data()[c(4,5,6),], type = "l", lwd = 3, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
      matlines(kwantyleA[c(4,5,6),], type = "l", lwd = 1, lty = 1, col = c("black", "black", "green", "black", "black"))
      axis(1,at = c(1,2,3), labels =c(100,126,252), las = 1) 
      grid(NULL, NULL, col = "gray")
    }
    else{
      matplot(data()[c(4,5,6),], type = "l", lwd = 3, lty = 1,  col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
      axis(1,at = c(1,2,3), labels =c(100,126,252), las = 1) 
      grid(NULL, NULL, col = "gray")
    }
    
  })
  

  #TRZECI SLAJD
  data2 <- eventReactive(input$Update3, {
    Analiza <- Portfel(gold[length(gold)], 253/253, input$fi3, input$sigma13,
                       input$sigma23, input$r13, input$r23, input$reh3, input$mi13, input$mi23, gold[length(gold)], USDPLN[length(USDPLN)])
    return(Analiza)
  })
  output$skladopcje<- renderPlot({
    matplot(data2()[,1], type = "l", lwd = 2, col = "darkred", ylab  = "wartosc", xlab = " ", main = "Suma aktywow")
    grid(NULL, NULL, col = "gray")
  })
  output$skladgotowka<- renderPlot({
     matplot(data2()[,2], type = "l", lwd = 2, col = "coral3", ylab  = "wartosc", xlab = " ", main = "Gotowka")
    grid(NULL, NULL, col = "gray")
  })
  output$skladdeltaz<- renderPlot({
    matplot(data2()[,3], type = "l", lwd =2, col = "darkorange", ylab  = "wartosc", xlab = " ", main = "Delta zlota")
    grid(NULL, NULL, col = "gray")
  })
  output$skladdeltadol<- renderPlot({
    matplot(data2()[,4], type = "l", lwd = 2, col = "brown", ylab  = "wartosc", xlab = " ", main = "Delta dolara")
    grid(NULL, NULL, col = "gray")
  })
  #CZWARTY SLAJD
  output$histB1 <- renderPlot({
    PiB <- c()
    for (i in 1:1000) PiB[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi4, input$sigma14,
                                 input$sigma24, input$r14, input$r24, input$reh4, input$mi14, input$mi24, USDPLN[length(USDPLN)])+2.5
    
    data_frame(v1 = PiB) %>%
      ggplot(., aes(v1)) + 
      geom_histogram(color='#000000',fill='#428bca', binwidth=0.3)+
      labs(x='zysk/strata', y='liczebnosc')+xlim(c(-10, 10))+
      ylim(c(0,600))
  })
  output$histB2 <- renderPlot({
    input$Update4
    rehedging <- isolate(input$reh4)
    PiB2 <- c()
    for (i in 1:1000) PiB2[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN,
                                  sigma_USDPLN, r_PL, r_USA, rehedging, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
    data_frame(v2 = PiB2) %>%
      ggplot(., aes(v2)) + 
      geom_histogram(color='#000000',fill='#cc2100', binwidth=0.3)+
      labs(x='zysk/strata', y='liczebnosc')+xlim(c(-10, 10))+
      ylim(c(0,600))
  })
  #PIATY SLAJD
  dataB <- eventReactive(input$Update5, {
    quantoB2 <- matrix(0,6,1000)
    for (i in 1:1000){
      quantoB2[1,i] <-Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi5, input$sigma15,
                          input$sigma25, input$r15, input$r25, 1, input$mi15, input$mi25, USDPLN[length(USDPLN)])+2.5
      quantoB2[2,i] <-Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi5, input$sigma15,
                          input$sigma25, input$r15, input$r25, 10, input$mi15, input$mi25, USDPLN[length(USDPLN)])+2.5
      quantoB2[3,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi5, input$sigma15,
                           input$sigma25, input$r15, input$r25, 50, input$mi15, input$mi25, USDPLN[length(USDPLN)])+2.5
      quantoB2[4,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi5, input$sigma15,
                           input$sigma25, input$r15, input$r25, 100, input$mi15, input$mi25, USDPLN[length(USDPLN)])+2.5
      quantoB2[5,i] <-Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi5, input$sigma15,
                          input$sigma25, input$r15, input$r25, 126, input$mi15, input$mi25, USDPLN[length(USDPLN)])+2.5
      quantoB2[6,i] <-Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fi5, input$sigma15,
                          input$sigma25, input$r15, input$r25, 252, input$mi15, input$mi25, USDPLN[length(USDPLN)])+2.5}
    
    kwantyleB2 <- matrix(0, 6, 5)
    for (i in 1:6){
      kwantyleB2[i,1]<-quantile(quantoB2[i,], probs = 0.1)
      kwantyleB2[i,2]<-quantile(quantoB2[i,], probs = 0.25)
      kwantyleB2[i,3]<-quantile(quantoB2[i,], probs = 0.5)
      kwantyleB2[i,4]<-quantile(quantoB2[i,], probs = 0.75)
      kwantyleB2[i,5]<-quantile(quantoB2[i,], probs = 0.9)
    }
    return(kwantyleB2)
  })
  output$analizaB1 <- renderPlot({
    if (input$ch2){
      if (dataB()[1,1] < kwantyleB[1,1]){
        matplot(dataB(), type = "l", lwd = 2, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
        matlines(kwantyleB, type = "l", lwd = 1, lty = 1, col = c("black", "black", "green", "black", "black"))
        axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
        grid(NULL, NULL, col = "gray")}
      else {
        matplot(kwantyleB, type = "l", lwd = 1, lty = 1, col = c("black", "black", "green", "black", "black"), xaxt = "n", ylab = "zysk/strata")
        matlines(dataB(), type = "l", lwd = 2, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"))
        axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
        grid(NULL, NULL, col = "gray")
      }
    }
    else{
      matplot(dataB(), type = "l", lwd = 2, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
      axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
      grid(NULL, NULL, col = "gray")
    }
      
  })
  output$analizaB2 <- renderPlot({
    if (input$ch2){
      matplot(dataB()[c(4,5,6),], type = "l", lwd = 3, lty = 1, col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
      matlines(kwantyleB[c(4,5,6),], type = "l", lwd = 1, lty = 1, col = c("black", "black", "green", "black", "black"))
      axis(1,at = c(1,2,3), labels =c(100,126,252), las = 1) 
      grid(NULL, NULL, col = "gray")
    }
    else{
      matplot(dataB()[c(4,5,6),], type = "l", lwd = 3, lty = 1,  col = c("#1571a3", "#151ea2", "red", "#151ea2", "#1571a3"), xaxt = "n", ylab = "zysk/strata")
      axis(1,at = c(1,2,3), labels =c(100,126,252), las = 1)
      grid(NULL, NULL, col = "gray")
    }
  })
  
  #SZOSTY SLAJD
  dataB2 <- eventReactive(input$Update6, {
    AnalizaB <- PortfelB(gold_PLN[length(gold_PLN)], 253/253, input$fi6, input$sigma16,
                        input$sigma26, input$r16, input$r26, input$reh6, input$mi16, input$mi26, USDPLN[length(USDPLN)])
    return(AnalizaB)
  })
  output$skladopcjeB<- renderPlot({
    matplot(dataB2()[,1], type = "l", lwd = 2, col = "darkred", ylab  = "wartosc", xlab = " ", main = "Suma aktywow")
    grid(NULL, NULL, col = "gray")
  })
  output$skladgotowkaB<- renderPlot({
    matplot(dataB2()[,2], type = "l", lwd = 2, col = "coral3", ylab  = "wartosc", xlab = " ", main = "Gotowka")
    grid(NULL, NULL, col = "gray")
  })
  output$skladdeltazB<- renderPlot({
    matplot(dataB2()[,3], type = "l", lwd =2, col = "darkorange", ylab  = "wartosc", xlab = " ", main = "Delta zlota")
    grid(NULL, NULL, col = "gray")
  })
  output$skladdeltadolB<- renderPlot({
    matplot(dataB2()[,4], type = "l", lwd = 2, col = "brown", ylab  = "wartosc", xlab = " ", main = "Delta dolara")
    grid(NULL, NULL, col = "gray")
  })
  
  
  
  #Ostatni SLAJD
  output$hist5 <- renderPlot({
    PiA5 <- c()
    for (i in 1:1000) PiA5[i]<-Pi(gold[length(gold)], 253/253, input$fiA7, input$sigmaA17,
                                 input$sigma27, input$r17, input$r27, input$reh7, input$miA17, input$mi27, gold[length(gold)], USDPLN[length(USDPLN)])
    
    data_frame(v1 = PiA5) %>%
      ggplot(., aes(v1)) + 
      geom_histogram(color='darkblue',fill='lightblue', binwidth=0.3)+
      labs(x='zysk/strata', y='liczebnosc')+xlim(c(-10, 10))+
      ylim(c(0,600))
  })
  output$hist6 <- renderPlot({
    PiB5 <- c()
    for (i in 1:1000) PiB5[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, input$fiB7, input$sigmaB17,
                                  input$sigma27, input$r17, input$r27, input$reh7, input$miB17, input$mi27, USDPLN[length(USDPLN)])+2.5
    
    data_frame(v1 = PiB5) %>%
      ggplot(., aes(v1)) + 
      geom_histogram(color='#000000',fill='#428bca', binwidth=0.3)+
      labs(x='zysk/strata', y='liczebnosc')+xlim(c(-10, 10))+
      ylim(c(0,600))
  })
}




shinyApp(server = server, ui = ui)
