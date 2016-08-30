#
library(devtools)
library(shiny)
library(googleVis)
library(ggplot2)


library(data.table)
library(DT)

#MSRP in 1974
X<-c(4100,4295,2880,2298,2449,2960,2822,9811,
     9172,12756,13520,18333,17653,22053,7537,7719,
     7190,3640,2545,2790,2280,3200,3100,3570,
     3343,3920,9999,7290,10300,13900,28900,3020)

cars <- mtcars
cars$MoMa<-rownames(mtcars)
cars$carb<-as.factor(cars$carb)
cars$am<-as.factor(cars$am)
cars$vs<-as.factor(cars$vs)
cars$cyl<-as.factor(cars$cyl)

Xy<-cars

cars$MSRP<-X
MPG<-cars
HP<-cars
Displacement<-cars
QuarterMileSec<-cars

#Build the Physics Stuff
phy<-Xy
phy$d<-round((5280/4)*.3048,1)
phy$Vavg<-round(phy$d/phy$qsec,1)
phy$a<-round((2*phy$d)/(phy$qsec)**2,1)
phy$m<-round(phy$wt/2.2,2)*1000
phy$F<-round(phy$m*phy$a,1)
phy$Vf<-round(2*phy$Vavg,2)
phy$p<-round(phy$m*phy$Vf,1)
phy$KE<-round((.5*phy$m)*(phy$Vf)**2,2)
phy$t<-round(phy$p/phy$F,2)
phy$Pow<-round(phy$KE/phy$qsec,1)
phy$HPeff<-round(phy$Pow/746,1)
phy$Eff<-round(phy$HPeff/phy$hp,1)*100
phy$G<-round(phy$a/9.8,2)

phy<-phy[which(phy$Eff<=100),]
phy<-phy[-c(1:11)]



shinyServer(function(input, output) {

 output$shorttable = renderDataTable({
      tail(mtcars,3)
    })
 output$shortphy = renderDataTable({
      tail(phy[-c(1)],3)
    })

  #Output new slider depending on dataset chosen
  output$slider<- renderUI({

    if(input$data_choice== 'Mass'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(phy$m),
                  value= c(0, max(phy$m)))

    }else if(input$data_choice== 'AverageSpeed'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$Vavg,2)),
                  value= c(0, max(round(phy$Vavg,2))))

    }else if(input$data_choice== 'TopSpeed'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$Vf,2)),
                  value= c(0, max(round(phy$Vf,2))))


    }else if(input$data_choice== 'Acceleration'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$a,2)),
                  value= c(0, max(round(phy$a,2))))

    }else if(input$data_choice== 'Force'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$F,2)),
                  value= c(0, max(round(phy$F,2))))

    }else if(input$data_choice== 'g\'s'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$G,2)),
                  value= c(0, max(round(phy$G,2))))


    }else if(input$data_choice== 'KineticEnergy'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$KE,2)),
                  value= c(0, max(round(phy$KE,2))))

    }else if(input$data_choice== 'Momentum'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$p,2)),
                  value= c(0, max(round(phy$p,2))))

    }else if(input$data_choice== 'Power'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$Pow,2)),
                  value= c(0, max(round(phy$Pow,2))))

    }else if(input$data_choice== 'Efficiency'){
      sliderInput('rangeSlider', 'Choose data range',
                  min= 0, max= max(round(phy$Eff,2)),
                  value= c(0, max(round(phy$Eff,2))))

    }


  })

 output$slider_a<- renderUI({
   force<-phy[which(phy$MoMa == input$MoMa_a), ]$F
   sliderInput('integer', 'Choose a Braking Force Value',
                  min= 0, max= as.integer(force*3.5),
                  value= c(0))



 })

  #plot adjust
  adjustPlot<- reactive({
    #Adjust dataframes based on input from slider

    if(input$data_choice== 'Force'){
      adjusted_Force<- subset(phy, 
                                 F >= input$rangeSlider[1] & F <= input$rangeSlider[2])

    }else if(input$data_choice== 'AverageSpeed'){
      adjusted_Vavg<- subset(phy, 
                                 Vavg >= input$rangeSlider[1] & Vavg <= input$rangeSlider[2])

    }else if(input$data_choice== 'TopSpeed'){
      adjusted_Vf<- subset(phy, 
                                 Vf >= input$rangeSlider[1] & Vf <= input$rangeSlider[2])


    }else if(input$data_choice== 'Acceleration'){
      adjusted_Acceleration<- subset(phy, 
                                 a >= input$rangeSlider[1] & a <= input$rangeSlider[2])

    }else if(input$data_choice== 'Mass'){
      adjusted_Mass<- subset(phy, 
                                 m >= input$rangeSlider[1] & m <= input$rangeSlider[2])

    }else if(input$data_choice== 'g\'s'){
      adjusted_G<- subset(phy,  
                                G >= input$rangeSlider[1] & G <= input$rangeSlider[2])


    }else if(input$data_choice== 'KineticEnergy'){
      adjusted_KE<- subset(phy, 
                                 KE >= input$rangeSlider[1] & KE <= input$rangeSlider[2])

    }else if(input$data_choice== 'Momentum'){
      adjusted_Momentum<- subset(phy, 
                                 p >= input$rangeSlider[1] & p <= input$rangeSlider[2])

    }else if(input$data_choice== 'Power'){
      adjusted_Power<- subset(phy, 
                                 Pow >= input$rangeSlider[1] & Pow <= input$rangeSlider[2])

    }else if(input$data_choice== 'Efficiency'){
      adjusted_Effiency<- subset(phy, 
                                 Eff >= input$rangeSlider[1] & Eff <= input$rangeSlider[2])

    }



  })
#prices now and then

 msrp1974 <- eventReactive(input$calculate, {
    msrp<-cars[which(rownames(cars) == input$model_choice), ]$MSRP
    currentprice<-round((4.88*msrp),0)
    x<- paste(input$model_choice,"- 1974 MSRP  $", msrp," which is $", currentprice," in today's dollars")
    x    
  })
  
  output$result <- renderText({

    msrp1974()

  })

 ans_a <- eventReactive(input$calc_a, {
       friction_a<-input$integer
       ke_a=phy[which(phy$MoMa == input$MoMa_a), ]$KE
       m_a=phy[which(phy$MoMa == input$MoMa_a), ]$m
       Vf_a=phy[which(phy$MoMa == input$MoMa_a), ]$Vf 
       id_a=which(phy$MoMa == input$MoMa_a)
       dist_a<-ke_a/friction_a
       p_a<-phy[which(phy$MoMa == input$MoMa_a), ]$p
       time_a<-p_a/friction_a
       g_a<-friction_a/(m_a*9.8)
       vmph_a <- (Vf_a*60)/(.3048*88)

       answ_a<-rbind(as.integer("1974"),round(as.numeric(dist_a),2),round(as.numeric(time_a),2),as.numeric(friction_a),
        round(as.numeric(g_a),3),as.numeric(m_a),as.numeric(Vf_a),round(as.numeric(vmph_a),2))
       answ_a<-as.data.frame(answ_a)
       colnames(answ_a)<-c("Values")
       answ_a$Units<-c("  -  ","  m  ","  s  ","  N  "," none","  Kg "," m/s "," MPH ")
       answ_a$Quantity<-c(" - ","braking distance","braking time","braking force","g\'s","mass","velocity","velocity")
       answ_a$Description<-c(input$MoMa_a,"total distance traveled during breaking event ","time elapsed during breaking event",
       "Force applied by the brakes","ratio of braking deceleration to acceleration of gravity","mass of the automobile","velocity of car when the brakes are applied","same as 7")
       data.table(answ_a)





  })
  
  output$result_a <- renderDataTable({


    ans_a()

  })



  output$view <- renderPlot({


    if(input$data_choice=='Force'){
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=F))
      p<-p + geom_bar(stat='identity', fill='navyblue') + coord_flip()

    }else if(input$data_choice=='AverageSpeed') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=Vavg)) 
      p<-p + geom_bar(stat='identity', fill= 'forestgreen') + coord_flip()

    }else if(input$data_choice=='TopSpeed') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=Vf)) 
      p<-p + geom_bar(stat='identity', fill= 'pink') + coord_flip()

    } else if(input$data_choice=='Acceleration') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=a))
      p<-p + geom_bar(stat='identity', fill= 'red') + coord_flip()

    } else if(input$data_choice=='Mass') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=m)) 
      p<-p + geom_bar(stat='identity', fill= 'orange') + coord_flip()

    } else if(input$data_choice=='g\'s') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=G)) 
      p<-p + geom_bar(stat='identity', fill= 'black') + coord_flip()

    } else if(input$data_choice=='KineticEnergy') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=KE)) 
      p<-p + geom_bar(stat='identity', fill= 'purple') + coord_flip()

    } else if(input$data_choice=='Momentum') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=p)) 
      p<-p + geom_bar(stat='identity', fill= 'yellow') + coord_flip()

    } else if(input$data_choice=='Power') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=Pow)) 
      p<-p + geom_bar(stat='identity', fill= 'brown') + coord_flip()

    } else if(input$data_choice=='Efficiency') {
      p<-ggplot(adjustPlot(), aes(x=MoMa, y=Eff)) 
      p<-p + geom_bar(stat='identity', fill= 'salmon') + coord_flip()

    } 



    print(p)

  }, height=550)

})