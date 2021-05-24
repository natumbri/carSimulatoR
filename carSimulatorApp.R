if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(shinyWidgets)) install.packages('shinyWidgets')
library(shinyWidgets)

setwd("/home/natumbri/cloud-projects/205si")
source("carSimulator.R")

# Engine
# eng.tqcurve <- read.csv(file = '205si_torquecurve.csv')
# eng.tqFullLoad <- eng.tqcurve$Nm  # c(0, 306, 385, 439, 450, 450, 367) # engine torque curve at full load [Nm]
# eng.NtqFullLoad <- eng.tqcurve$RPM  # c(0, 1000, 2020, 2990, 3500, 5000, 6500) # engine speed axis [rpm]
# eng.kWFullLoad <- (1 / 1000) * eng.tqFullLoad * (eng.NtqFullLoad * (pi / 30)) # engine power curve at full load [kW] 
# eng.NmaxTq <- eng.tqcurve$RPM[which.max(eng.tqcurve$Nm)] # engine speed for maximum torque [rpm]
# eng.NmaxPwr <- eng.tqcurve$RPM[which.max(eng.kWFullLoad)] # engine speed for maximum power [rpm]
# eng.Nmax <- max(eng.tqcurve$RPM) # maximum engine speed [rpm]
# eng.Nmin <- min(eng.tqcurve$RPM) # minimum engine speed [rpm]

# Transmission
gbx.eff <- 0.85 # driveline efficiency [-]
gbx.gearMin <- 1 # lowest gear [-]
gbx.gearMax <- 5 # highest gear [-]
# gbx.gearRat <- c(3.45, 1.85, 1.28, 0.969, 0.76)   # gearbox gear ratios [-]
# gbx.i0 <-  3.94 # final drive ratio (differential) [-]

# Tires: 165/70R-13
# tire.W <- 0.165 # tire width [m]
# tire.ratio <- 70
# tire.rimd <- 13  # rim diameter [inch]

# Tires: 185/60R-14
# tire.W <- 0.185 # tire width [m]
# tire.ratio <- 60
# tire.rimd <- 14  # rim diameter [inch]

# tire.D <- tire.rimd * 0.0254 # rim diameter [m]

tire.miua <- 1.1 # wheel (tire) friction coefficient [-]
tire.load <- 0.65 # front axle load coeeficient [-] ?not understand?

# Vehicle
# veh.mass_curb <- 870 # [kg]
veh.mass_driver <- 80 # [kg]
veh.mass_fm <- 1.05 # mass factor [-]
veh.g <- 9.81 # gravitational acceleration [m/s2]
veh.cd <- 0.34 # drag coefficient [-]
veh.fa <- 2.195 # frontal area [m2]
veh.ro <- 1.25 # air density [kg/m3]
road.slope <- 0 # road slope angle [rad]
road.cr <- 0.011 # road load coefficient [-] ?not understand?

# # wheel dynamic radius
# tire.H <- tire.ratio * tire.W / 100 # tire height [m]
# tire.rws <- tire.D / 2 + tire.H # wheel static radius [m]
# tire.rwd <- 0.98 * tire.rws # wheel dynamic radius [m]

# vehicle total mass
# veh.mass <- veh.mass_curb * veh.mass_fm + veh.mass_driver # [kg]

# simulation
dT <- 0.03 # sample (plot) time [s]

first <- c(3.45, 3.25, 2.92)
second <- c(1.85)
third <- c(1.36, 1.280)
fourth <- c(1.069, 0.969)
fifth <- c(0.865, 0.810, 0.760)
final <- c(3.62, 3.94, 4.06, 4.43, 5.1)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "model",
        label = h4("model"),
        choices = c("si", "gti-93kW", "mi16-160hp"),
        selected = "si",
        inline=T
      ),
      sliderTextInput(inputId="first", 
                      label = h5("First"),
                      choices = first,
                      width = "100%"),
      p(strong("second"), ": ", second),
      sliderTextInput(inputId="third", 
                      label = h4("third"),
                      choices = third,
                      width = "100%"),
      sliderTextInput(inputId="fourth", 
                      label = h4("fourth"),
                      choices = fourth,
                      width = "100%"),
      sliderTextInput(inputId="fifth", 
                      label = h4("fifth"),
                      choices = fifth,
                      selected = fifth[3],
                      width = "100%"),
      sliderTextInput(inputId="final", 
                      label = h4("final"),
                      choices = final,
                      selected = final[2],
                      width = "100%"),
      selectInput(inputId="graphy",
                  label = h4("graph y"),
                  choices = c("engine torque" = "tq", 
                              "power" = "kW",
                              "transmission torque" = "trn_tq",
                              "engine rpm" = "eng_rpm",
                              "gear selected" = "gear",
                              "vehicle speed (km/h)" = "whl_spd",
                              "vehicle speed (m/s)" = "vehSpeed",
			                        "acceleration (m/s2)" = "vehAcc_mps2"),
                  selected = "whl_spd"
      ),
      selectInput(inputId="graphx",
                  label = h4("graph x"),
                  choices = c("engine torque" = "tq", 
                              "power" = "kW",
                              "transmission torque" = "trn_tq",
                              "engine rpm" = "eng_rpm",
                              "gear selected" = "gear",
                              "vehicle speed (km/h)" = "whl_spd",
                              "vehicle speed (m/s)" = "vehSpeed",
                              "time (s)" = "time",
			                        "acceleration (m/s2)" = "vehAcc_mps2"),
                  selected = "time"
      ),
      
      div(style="display: inline-block;vertical-align:bottom; width: 50px;",textInput(inputId="tireW", label="tire width (mm)", value = 165)),
      div(style="display: inline-block;vertical-align:bottom; width: 50px;",textInput(inputId="tireRatio", label="ratio", value = 70)),
      div(style="display: inline-block;vertical-align:bottom; width: 50px;",textInput(inputId="rimD", label="rim diameter [inch]", value = 13)),
      div(style="display: inline-block;vertical-align:bottom; width: 100px;",HTML("<br>")),
      div(style="display: inline-block;vertical-align:bottom; width: 50px;",textInput(inputId="weight", label="weight [kg]", value = 870))
      
      
    ),
    mainPanel(
      plotOutput("plot"),
      htmlOutput("text")
    )
  )
)


server = function(input, output, session){
  v <- reactiveValues(x = NULL)
  
  observeEvent(input$third, {
    v$x <- which(third == input$third)
  })
  
  observeEvent(input$fourth, {
    v$x <- which(fourth == input$fourth)
  })
  
  observeEvent(v$x, {
    if (v$x != which(third == input$third)) {
      updateSliderTextInput(session, "third", selected = third[v$x])
    }
    
    if (v$x != which(fourth == input$fourth)) {
      updateSliderTextInput(session, "fourth", selected = fourth[v$x])
    }
  })
  
  simulation <- reactive({
    
    eng.tqcurve <- read.csv(file = paste0('205',input$model,'_torquecurve.csv'))
    eng.tqFullLoad <- eng.tqcurve$Nm  # c(0, 306, 385, 439, 450, 450, 367) # engine torque curve at full load [Nm]
    eng.NtqFullLoad <- eng.tqcurve$RPM  # c(0, 1000, 2020, 2990, 3500, 5000, 6500) # engine speed axis [rpm]
    eng.kWFullLoad <- (1 / 1000) * eng.tqFullLoad * (eng.NtqFullLoad * (pi / 30)) # engine power curve at full load [kW] 
    eng.NmaxTq <- eng.tqcurve$RPM[which.max(eng.tqcurve$Nm)] # engine speed for maximum torque [rpm]
    eng.NmaxPwr <- eng.tqcurve$RPM[which.max(eng.kWFullLoad)] # engine speed for maximum power [rpm]
    eng.Nmax <- max(eng.tqcurve$RPM) # maximum engine speed [rpm]
    eng.Nmin <- min(eng.tqcurve$RPM) # minimum engine speed [rpm]
    
    gbx.gearRat <- c(input$first, second, input$third, input$fourth, input$fifth)
    gbx.i0 <- input$final
    
    tire.W <- as.numeric(input$tireW)/1000
    tire.ratio <- as.numeric(input$tireRatio)
    tire.rimd <- as.numeric(input$rimD)
    tire.D <- tire.rimd * 0.0254
    
    # wheel dynamic radius
    tire.H <- tire.ratio * tire.W / 100 # tire height [m]
    tire.rws <- tire.D / 2 + tire.H # wheel static radius [m]
    tire.rwd <- 0.98 * tire.rws # wheel dynamic radius [m]
    
    veh.mass_curb <- as.numeric(input$weight)
    veh.mass <- veh.mass_curb * veh.mass_fm + veh.mass_driver # [kg]
    
    
    # simulation
    # initial values
    engineSpeed <- eng.Nmin  # start at idle [rpm]
    gear <- gbx.gearMin  # start in first
    vehSpeed <- 0  # start stationary [m/s]
    trn_spd <-
      (gbx.i0 * gbx.gearRat[gear] / engineSpeed) * (30 / pi)   # transmission speed [rad/s] at idle in first
    
    result <- data.frame("time", "tq", "kW", "engineSpeed", "trn_tq", "eng_rpm", "gear", "whl_spd", "vehSpeed", "vehAcc_mps2")
    names(result) <- result[1,]
    
    for (i in seq(0, 100/dT, 1)) {
      engine_output <-
        engine(eng.NtqFullLoad,
               eng.tqFullLoad,
               engineSpeed,
               i,
               eng.Nmin,
               eng.Nmax)
      
      eng_trq <- engine_output[1]
      
      transmission_output <-
        transmission(
          eng_trq,
          trn_spd,
          gear,
          gbx.gearMin,
          gbx.gearMax,
          eng.NmaxTq,
          eng.NmaxPwr,
          gbx.gearRat,
          gbx.i0,
          gbx.eff,
          eng.Nmin,
          eng.Nmax
        )
      
      trn_tq <- transmission_output[1]
      engineSpeed <- transmission_output[2]
      gear <- transmission_output[3]
      
      vehSpeed.max <- (eng.Nmax*2*pi*tire.rwd) / (60*gbx.gearRat[gear]*gbx.i0) 
        
      vehicle_output <-
        vehicle(
          veh.mass,
          veh.g,
          tire.miua,
          tire.load,
          road.slope,
          road.cr,
          veh.ro,
          veh.cd,
          veh.fa,
          trn_tq,
          vehSpeed,
          vehSpeed.max,
          dT,
          tire.rwd
        )
      
      trn_spd <- vehicle_output[1]
      vehSpeed <- vehicle_output[2]
      result <- rbind(result, setNames(c(as.numeric(i)*dT, engine_output, transmission_output, vehicle_output), names(result)))  
    }
    result <- result[-1,]
  })
  
  output$plot <- renderPlot({
    # plot(simulation()[,"gear"], col="blue", type="l")
    plot(simulation()[,input$graphx], simulation()[,input$graphy], col="blue", type="l", xlab = input$graphx, ylab=input$graphy)
  })
  output$text <- renderUI({
    # redline <- paste0("redline: ", max(as.integer((simulation()[,"eng_rpm"]))))
    top_speed_N <- simulation()[which.max(as.integer((simulation()[,"vehSpeed"]))), ]
    top_speed <- paste0("top speed: ", as.integer(as.numeric(top_speed_N["vehSpeed"])*3.6), " Km/h at ", as.integer(top_speed_N["eng_rpm"]), " RPM, in ", top_speed_N["gear"], " gear, in ", top_speed_N["time"], " seconds")
    noughtto100 <- paste0("0-100: ", Position(function(x) x > 100, as.numeric(simulation()[,"vehSpeed"])*3.6)*dT)
    onehundredto150 <- paste0("100-150: ", (Position(function(x) x > 150, as.numeric(simulation()[,"vehSpeed"])*3.6) - Position(function(x) x > 100, as.numeric(simulation()[,"vehSpeed"])*3.6))*dT)
    HTML(paste(top_speed, noughtto100, onehundredto150, sep = '<br/>'))  })
}

shinyApp(ui = ui, server = server)
