library(shiny)

library(devtools)
library(shiny)
library(googleVis)
library(ggplot2)
library(data.table)
library(DT)


cars<-mtcars
phy<-cars
phy$d<-round((5280/4)*.3048,1)
phy$Vavg<-round(phy$d/phy$qsec,1)
phy$a<-round((2*phy$d)/(phy$qsec)**2,1)
phy$m<-round(phy$wt/2.2,2)*1000
phy$F<-round(phy$m*phy$a,1)
phy$Vf<-round(2*phy$Vavg,1)
phy$p<-round(phy$m*phy$Vf,1)
phy$KE<-round((.5*phy$m)*(phy$Vf)**2,1)
phy$t<-round(phy$p/phy$F,2)
phy$Pow<-round(phy$KE/phy$qsec,1)
phy$HPeff<-round(phy$Pow/746,1)
phy$Eff<-round(phy$HPeff/phy$hp,1)*100
phy$G<-round(phy$a/9.8,2)
phy<-phy[which(phy$Eff<=100),]


shinyUI(fluidPage(
    titlePanel("Time for a Bit of Physics:  Calculate the Braking Distance and Braking Time for the 1974 Motor Trend Cars."),
    helpText(h6('                                                 By ACGII')),
    helpText(h5('The 1974 Motor Trend Cars data structure is a compilation of 32 different 1974 car models and makes. A 3 model subset of the 32 models 
      is provided, making it easy to identify each structure member.  The elements that are most critical for our calculations are: ')), 
   
  
    tags$ol(
        tags$li(h5("qsec - elapsed time, in seconds, for the quarter mile")),
        tags$li(h5("wt - automobile weight in thousands of pounds")),
        tags$li(h5("hp - Horse Power rating for the cars engine"))
    ),

    dataTableOutput('shorttable'),

    helpText(h5('We must make a few assumptions before making our calculations.  If we didn\'t do this we could not use the equations of kinematics to analyze the data.
      We will assume that:')),
    tags$ol(
        tags$li(h5("Acceleration is uniform over the quarter mile.")),
        tags$li(h5("The road is level, like the Bonneville Salt Flats ")),
        tags$li(h5("Air Resistance is Negligible")),
        tags$li(h5("Deceleration is uniform during the breaking event"))
    ),
    helpText(h5('The second table, below, was formed by using the equations of kinematics and dynamics on the structure members qsec, wt, and hp from the first table. 
      Examining the second table from left to right, we see d is the first physical quantity and G is the last.  
        A description of each physical quantity in the structure is provided, below, along with the
          proper units and method of calculation.')),

    dataTableOutput('shortphy'),
  
    tags$ol(
        tags$li(h5("d - distance in meters (m) for a quarter mile, all entries are 402.3 m")),
        tags$li(h5("Vavg - Average velocity measured in meter per second (m/s) - d/qsec")),
        tags$li(h5("a - Average acceleration in meter per second squared (m/s2) - 2*d/qsec**2")),
        tags$li(h5("m - mass of the automobile in Kilograms (Kg) - wt/2.2")),
        tags$li(h5("F - Force of the automobile, measured in Newtons, during acceleration over the quarter mile.")),
        tags$li(h5("Vf - Final velocity or top speed in meter per second  (m/s) - 2*Vavg")),
        tags$li(h5("p - Momentum of the automobile, measured in (Kgm/s), at top speed - m*Vf")), 
        tags$li(h5("KE - Kinetic Energy measured in Joules (J = kgm2/s2) - 1/2*m*Vf**2")),
        tags$li(h5("t - Calculated time in seconds (s) as a check for correctness and should be the same as qsec - p/F")),
        tags$li(h5("Pow - Power, in Watts (J/s), used in attaining top speed - KE/qsec ")),
        tags$li(h5("HPeff - Effective Horse Power that is demonstrated in the quarter mile - Pow/746")),
        tags$li(h5("Eff - Efficiency measured in % is the ratio of HPeff to the manufacturers Horse Power (hp) - (HPeff/hp)*100%")),
        tags$li(h5("G -  A ratio, no units, of the acceleration to the acceleration of gravity - a/9.8"))
    ),


    helpText(h5('Below are barcharts plotted sideways indicating the values for most of the physical quantities in table 2.  The are plotted so 
       that car model is depicted as a function of physical quantitiy.
         In order to view a particular physical quantity, select the radio button associated with it.  
           The dual ended slider bar allows you to select 
             subsets of the automobiles by limiting the range of the physical quantity.')),
   
    sidebarLayout(
        sidebarPanel(
  
            radioButtons('data_choice', 'Physical Quantities, Values and Units', 
               choices= c('Mass','AverageSpeed','TopSpeed','Acceleration', 'Force','g\'s','KineticEnergy','Momentum','Power', 'Efficiency')),
            uiOutput('slider'),
            tags$br(),
     
            helpText(h3('The Analysis of the Braking Distance and Breaking Time')),
            helpText(h5('Calculate both the Braking Distance and Braking Time for a selected automobile and breaking force.  We assume the automobile is traveling at its top speed in the quarter mile run.' )),
            selectInput('MoMa_a', 'Select a Model and Make', rownames(phy), selected = NULL,
              multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
            br(),
            uiOutput('slider_a'),
            actionButton("calc_a", label = "Calculate"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            helpText(h3('One Final Point of Interest')),

            selectInput('model_choice', 'Find the Original MSRP', rownames(cars), selected = NULL,
              multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
            br(),
            actionButton("calculate", label = "Calculate"),        
            br(),
            br(),
            textOutput('result')
    ),
    mainPanel(
       plotOutput('view'),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
	 br(),
       dataTableOutput('result_a'),
       helpText(h5('The braking distance is defined as the distance the automobile travels, after the brakes have been applied, until it 
         comes to rest. The braking time is the amount of time, after the brakes have been applied, it takes for the automobile 
           to come to rest.  The physical quantities displayed above, coorespond to the braking event, which comes immediately after 
            the quarter mile run.  These values quantify the braking event.  An explanation of each item is provided in the description'))
    )
  )
))