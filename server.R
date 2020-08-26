source("functions.R", local = TRUE)

x <- 1
t <- 0

while(t == 0) {
  x <- x + 1
  tmax <- ncdc(datasetid = 'GHCND', 
               stationid = "GHCND:USC00051959", 
               token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
               startdate = Sys.Date() - x, 
               enddate = Sys.Date() - x, 
               datatypeid = "TMAX")
  t <- dim(tmax$data)[1]
}

tmin <- ncdc(datasetid = 'GHCND', 
             stationid = "GHCND:USC00051959", 
             token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
             startdate = Sys.Date() - x, 
             enddate = Sys.Date() - x, 
             datatypeid = "TMIN")

zenith <- zenith_angle(doy = day_of_year(Sys.Date() - x), lat = 38.9, lon = -107.0, hour = c(0:23))




shinyServer <- function(input, output, session) {

  airTemp <- reactive({
    if(input$data == "recent") {

    
      diurnal_temp_variation_sine(tmax$data$value / 10 , tmin$data$value / 10, c(0:23))
    } else {
      validate(need(input$tmax, ""),
               need(input$tmin, ""))
      diurnal_temp_variation_sine(input$tmax, input$tmin, c(0:23))
    }
  })

  
   
  output$manual <- renderUI({
    if(!input$data == "recent") {
      list(
        numericInput("tmax", "Tmax (°C)", value = 30),
        numericInput("tmin", "Tmin (°C)", value = 10)
      )
    } else {
      list(
        strong(div("Data from", Sys.Date() - x), style = "color:black"),
        div(paste0("Tmax: ", tmax$data$value / 10, "°C"), style = "color:black"),
        div(paste0("Tmin: ", tmin$data$value / 10, "°C"), style = "color:black"),
        hr()
      )
    }
  })
  
  output$plot <- renderPlot({

    colors <- c("Environmental" = "black", "Operative" = "blue")
    
    ggplot() + geom_line(aes(x = c(0:23), y = airTemp(), color = "Environmental"), size = 1.3) + geom_line(aes(x = c(0:23), y = bodyTemp(), color = "Operative"), size = 1.3) +
      xlab("Day") + ylab("Temperature (°C)") + theme_bw() + ggtitle("C. eriphyle body temperatures") +
      scale_color_manual(values = colors) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 13), axis.title = element_text(size = 16), legend.text = element_text(size = 13), 
            legend.title = element_blank())
    
  })
  
  bodyTemp <- reactive({
    
    if (input$weather == "Clear") {
      dir <- 1012  # assuming total radiation is 1100 and 92% is direct
      dif <- 98
    } else if (input$weather == "Partly sunny") {
      dir <- 350
      dif <- 350
    } else {
      dir <- 0
      dif <- 300
    }
    
    if(input$shade) {
      dir <- 0
      dif <- 0
    }

    Tb <- mapply(Tb_butterfly, 
                 T_a = airTemp(),
                 Tg = airTemp() + input$ground, 
                 Tg_sh = airTemp() - 5, 
                 u = input$wind, 
                 H_sdir = dir, 
                 H_sdif = dif, 
                 z = zenith, 
                 D = input$diam / 10, 
                 delta = input$fur, 
                 alpha=as.numeric(input$abs), 
                 r_g=0.3, 
                 shade = input$shade)
  })
  
  output$plotly <- renderPlotly({

    d <- event_data("plotly_click")
    if (input$data == "recent") {
      title <- paste0("Butterfly temperature on ", Sys.Date() - x, " in Colorado")
    } else {
      title <- "Butterfly temperature"
    }
    fig <- 
      plot_ly(x = ~c(0:23), y = ~airTemp(), name = "Environmental", type = "scatter", mode = "lines") %>%
        # add_trace() %>%
        add_trace(y = ~bodyTemp(), name = "Operative") %>%
        layout(title = title,
               xaxis = list(title = "Hour"),
               yaxis = list(title = "Temperature (°C)")) %>% 
      layout(legend = list(x = 100, y = 0.5))
    

    if (!is.null(d)) {
      fig <- fig %>% add_segments(x = d[,"x"], xend = d[,"x"], y = min(airTemp()) - 1, yend = max(max(bodyTemp()), max(airTemp())) + 1, line = list(color = "black"), name = "selected", showlegend = FALSE)
    }
    fig
  })
  
  direct <- reactive({
    if (input$weather == "Clear") {
      dir <- 1012  # assuming total radiation is 1100 and 92% is direct
    } else if (input$weather == "Partly sunny") {
      dir <- 350
    } else {
      dir <- 0
    }
    if(input$shade) {
      dir <- 0
    }
    dir
  })
  
  diffuse <- reactive({
    if (input$weather == "Clear") {
      dif <- 98
    } else if (input$weather == "Partly sunny") {
      dif <- 350
    } else {
      dif <- 300
    }
    if(input$shade) {
      dif <- 0
    }
    dif
  })
  
  output$heat <- renderUI({
    d <- event_data("plotly_click")
    hour <- d[,"x"]
    clock <- ifelse(is.null(d), "Click an hour on the plot", paste0(hour, ":00"))
    air <- ifelse(is.null(d), "", round(airTemp()[hour+1], digits = 1))
    body <- ifelse(is.null(d), "", round(bodyTemp()[hour+1], digits = 1))

    D <- input$diam / 10
    fur <- input$fur / 10
    A <- pi * D * 2  # surface area
    dir <- direct()
    dif <- diffuse()
    
    if(!is.null(d)) {
      Tsky = (1.22 * air - 20.4) + 273.15
      atemp <- air + 273.15
      btemp <- body + 273.15
    }
    
    abs <- as.numeric(input$abs)
    solar <- ifelse(is.null(d), "", round(abs * (A / 2) * dir * cos(zenith[hour+1] * pi / 180) + 
                                            abs * (A / 2) * dif + abs * 0.3 * (A / 2) * (dir + dif), digits = 1))
    if(!is.null(d) && input$shade == TRUE) {
      solar <- solar / 2
    }
    thermal <- ifelse(is.null(d), "", round(0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - Tsky^4) + 
                                              0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - (atemp + input$ground)^4), digits = 1))
    
    R_e = input$wind * D / (15.68 * 10^-2)
    N_u = 0.6 * R_e^0.5
    h_c = N_u * 0.25 / D
    convection <- ifelse(is.null(d), "", round((1 / h_c + (0.15 + fur) * log((0.15 + fur) / 0.15) / 1.3)^-1 * A * (btemp - atemp), digits = 1))
    
    HTML("Selected hour: ", clock,
         "</br>Air temperature (°C): ", air, 
         "</br>Body temperature (°C): ", body, 
         "</br>Solar radiative heat flux (mW): ", solar, 
         "</br>Thermal raditive flux (mW): ", thermal,
         "</br>Convective heat flux (mW): ", convection)
  })
  
  output$summary <- renderUI({
    d <- event_data("plotly_click")
    hour <- d[,"x"]
    ground <- ifelse(is.null(d), paste("Air temp +", input$ground), round(airTemp()[hour+1], digits = 1) + input$ground)
    
    if(input$weather == "Clear") {
      weather <- "Clear (Direct)"
    }
    weeather <- 
    HTML("Wind speed (u): ", input$wind,
         "m/s</br>Weather: ", input$weather,
         "</br>Shade: ", input$shade,
         "</br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8594;Direct solar radiation: ", direct(),
         "W/m<sup>2</sup></br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8594;Diffuse solar radiation: ", diffuse(),
         "W/m<sup>2</sup></br>Wing absorptivity (&alpha;): ", input$abs,
         "</br>Thoracic diameter (D): ", input$diam,
         "mm </br>Fur thickness (&delta;): ", input$fur,
         "mm </br>Ground temperature: ", ground, "°C"
         )
  })
  
  output$details <- renderUI({
    d <- event_data("plotly_click")
    validate(
      need(!is.null(d), "Click an hour on the plot")
    )
    hour <- d[,"x"]
    clock <- ifelse(is.null(d), "None", paste0(hour, ":00"))
    air <- ifelse(is.null(d), "", round(airTemp()[hour+1], digits = 1))
    body <- ifelse(is.null(d), "", round(bodyTemp()[hour+1], digits = 1))
    dir <- direct()
    dif <- diffuse()
    
    D <- input$diam / 10
    fur <- input$fur / 10
    A <- pi * D * 2  # surface area
    
    if(!is.null(d)) {
      Tsky = (1.22 * air - 20.4) + 273.15
      atemp <- air + 273.15
      btemp <- body + 273.15
      
    }
    
    abs <- as.numeric(input$abs)
    
    R_e = input$wind * D / (15.68 * 10^-2)
    N_u = 0.6 * R_e^0.5
    h_c = N_u * 0.25 / D
    
    HTML(.noWS = "before", "<b>Solar radiative</b>", 
         "<br> &alpha; &times; A<sub>s,dir</sub> &times; H<sub>s,dir</sub> &times; cos(z) + 
                                &alpha; &times; A<sub>s,ref</sub> &times; H<sub>s,dif</sub> + &alpha; &times; r<sub>g</sub> &times; A<sub>s,ref</sub> &times; H<sub>s,total</sub>",
         "<br> = ", input$abs, "&times; (&pi; &times;", input$diam / 10, " / 2) &times;", dir, "&times; cos(", round(zenith[hour+1], digits = 1), "°) + ", input$abs, "&times; (&pi; &times;", input$diam / 10, "/ 2)", "&times; ",
         dif, "+ ", input$abs, "&times; 0.3 &times; (&pi; &times;", input$diam / 10, "/ 2) &times;", (dir + dif),
         "<br> =", round(abs * (A / 2) * dir * cos(zenith[hour+1] * pi / 180) + 
                           abs * (A / 2) * dif + abs * 0.3 * (A / 2) * (dir + dif), digits = 1),
         "<br><br><b>Thermal radiative</b>",
         "<br> 0.5A<sub>t</sub> &times; &epsilon;&sigma;(T<sub>b</sub><sup>4</sup> - T<sub>sky</sub><sup>4</sup>) + 
                                              0.5A<sub>t</sub> &times; &epsilon;&sigma;(T<sub>b</sub><sup>4</sup> - T<sub>g</sub><sup>4</sup>)",
         "<br> = 0.5 &times; (&pi; &times;", input$diam / 10, ") &times; 1 &times; (5.67 &times; 10<sup>-9</sup>) &times; ((", body, " + 273.15)<sup>4</sup> - ((1.22 &times; ", air, " - 20.4) + 273.15)<sup>4</sup>) + 
         0.5 &times; (&pi; &times;", input$diam / 10, ") &times; 1 &times; (5.67 &times; 10<sup>-9</sup>) &times; ((", body, " + 273.15)<sup>4</sup> - (", air + input$ground, "+ 273.15)<sup>4</sup>)
         <br> =", round(0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - Tsky^4) + 
                          0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - (atemp + input$ground)^4), digits = 1),
         "<br><br><b>Convective</b>",
         "<br> (1 / (0.6 &times; (uD / &nu;)<sup>0.5</sup>) &times; k<sub>a</sub> / D + (r<sub>i</sub> + &delta;) &times; ln((r<sub>i</sub> + &delta;) / r<sub>i</sub>) / k<sub>e</sub>)<sup>-1</sup> &times; A<sub>c</sub> (T<sub>b</sub> - T<sub>a</sub>)
         <br> = (1 / (0.6 &times; (", input$wind * 100, "&times; ", D, "/ 15.68 &times; 10<sup>-2</sup>)<sup>0.5</sup>) &times; 0.25 / ", D,
         "+ (0.15 + ", fur, ") &times; ln((0.15 +", fur, ") / 1.3)<sup>-1</sup> &times; (&pi; &times;", D, ") &times; (", body, "-", air, ")
         <br> = ", round((1 / h_c + (0.15 + fur) * log((0.15 + fur) / 0.15) / 1.3)^-1 * A * (btemp - atemp), digits = 1)
    )
  })
  
  output$variables <- renderUI({
    if(input$switch == "Text") {
      HTML("<b>Solar</b>
           <br>A<sub>s,dir</sub>: Area subject to direct solar radiation
           <br>H<sub>s,dir</sub>: Direct solar radiation
           <br>z: Zenith angle
           <br>A<sub>s,ref</sub>: Area subject to reflected solar radiation
           <br>H<sub>s,dif</sub>: Diffuse solar radiation
           <br>r<sub>g</sub>: Substrate solar reflectivity
           <br>H<sub>s,total</sub>: Total solar radiation
           <br><b>Thermal</b>
           <br>A<sub>t</sub>: Thermal radiative heat transfer surface area
           <br>&epsilon;: Butterfly thermal emissivity
           <br>&sigma;: Stefan-Boltzman constant 
           <br>T<sub>sky</sub>: Equivalent black body sky temperature
           <br><b>Convective</b>
           <br>&nu;: Kinematic viscosity of air
           <br>k<sub>a</sub>: Thermal conductivity of air
           <br>r<sub>i</sub>: Body radius
           <br>k<sub>e</sub>: Thermal conductivity of the fur
           <br>A<sub>c</sub>: Convective heat transfer surface area"
      )
    } else {
      d <- event_data("plotly_click")
      validate(
        need(!is.null(d), "Click an hour on the plot")
      )
      hour <- d[,"x"]
      clock <- ifelse(is.null(d), "None", paste0(hour, ":00"))
      air <- ifelse(is.null(d), "", round(airTemp()[hour+1], digits = 1))
      body <- ifelse(is.null(d), "", round(bodyTemp()[hour+1], digits = 1))
      dir <- direct()
      dif <- diffuse()
      
      D <- input$diam / 10
      fur <- input$fur / 10
      A <- pi * D * 2  # surface area
      
      if(!is.null(d)) {
        Tsky = (1.22 * air - 20.4) + 273.15
        atemp <- air + 273.15
        btemp <- body + 273.15
        
      }
      
      abs <- as.numeric(input$abs)
      
      R_e = input$wind * D / (15.68 * 10^-2)
      N_u = 0.6 * R_e^0.5
      h_c = N_u * 0.25 / D
      HTML("<b>Solar</b>
           <br>A<sub>s,dir</sub>:", round(pi * input$diam / 10 / 2, digits = 1), "cm<sup>2</sup> (Half of total surface area)
           <br>H<sub>s,dir</sub>:", dir, "mW/cm<sup>2</sup>
           <br>z:", round(zenith[hour+1], digits = 1), "°
           <br>A<sub>s,ref</sub>:", round(pi * input$diam / 10 / 2, digits = 1), "cm<sup>2</sup> (Half of total surface area)
           <br>H<sub>s,dif</sub>:", dif, "mW/cm<sup>2</sup>
           <br>r<sub>g</sub>: 0.3 for grassland
           <br>H<sub>s,total</sub>:", dir + dif, "mW/cm<sup>2</sup>
           <br><b>Thermal</b>
           <br>A<sub>t</sub>:", round(pi * input$diam / 10, digits = 1), "cm<sup>2</sup> (Total surface area)
           <br>&epsilon;: 1
           <br>&sigma;: 5.67 &times; 10<sup>-9</sup> mW cm<sup>-2</sup>K<sup>-4</sup>
           <br>T<sub>sky</sub>:", (1.22 * air - 20.4) + 273.15, "K
           <br><b>Convective</b>
           <br>&nu;: 15.68 &times; 10<sup>-2</sup> cm<sup>2</sup>/s
           <br>k<sub>a</sub>: 0.25 mW cm<sup>-1</sup>K<sup>-1</sup>
           <br>r<sub>i</sub>: 0.15 cm
           <br>k<sub>e</sub>: 1.3 mW cm<sup>-1</sup>K<sup>-1</sup>
           <br>A<sub>c</sub>:", round(pi * input$diam / 10, digits = 1), "cm<sup>2</sup> (Total surface area)"
      )
    }
  })
}