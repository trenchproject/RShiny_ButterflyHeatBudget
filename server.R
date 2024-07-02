source("functions.R", local = TRUE)

# Find the most recent day with tmax and tmin data.
x <- 1
t <- 0

while (t == 0) {
  x <- x + 1
  tmax <- ncdc(
    datasetid = "GHCND",
    stationid = "GHCND:USC00051959",
    token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq",
    startdate = Sys.Date() - x,
    enddate = Sys.Date() - x,
    datatypeid = "TMAX"
  )
  t <- dim(tmax$data)[1]
}

tmin <- ncdc(
  datasetid = "GHCND",
  stationid = "GHCND:USC00051959",
  token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq",
  startdate = Sys.Date() - x,
  enddate = Sys.Date() - x,
  datatypeid = "TMIN"
)

zenith <- zenith_angle(doy = day_of_year(Sys.Date() - x), lat = 38.9, lon = -107.0, hour = c(0:23))

doy <- day_of_year(Sys.Date() - x)
t0 <- solar_noon(lon = -107, doy = doy)
hour <- c(0:23)

shinyServer <- function(input, output, session) {
  # Calculate hourly air temperatures based on tmax and tmin
  airTemp <- reactive({
    if (input$data == "recent") {
      diurnal_temp_variation_sine(tmax$data$value / 10, tmin$data$value / 10, c(0:23))
    } else {
      validate(
        need(input$tmax, ""),
        need(input$tmin, "")
      )
      diurnal_temp_variation_sine(input$tmax, input$tmin, c(0:23))
    }
  })

  # When input$data is recent, show tmin and tmax from the station,
  # otherwise provide numericInputs for users to manually enter.
  output$manual <- renderUI({
    if (input$data == "recent") {
      list(
        strong(div("Data from", Sys.Date() - x), style = "color:black"),
        div(paste0("Tmax: ", tmax$data$value / 10, "°C"), style = "color:black"),
        div(paste0("Tmin: ", tmin$data$value / 10, "°C"), style = "color:black"),
        hr()
      )
    } else {
      list(
        numericInput("tmax", "Tmax (°C)", value = 30),
        numericInput("tmin", "Tmin (°C)", value = 10)
      )
    }
  })

  output$plot <- renderPlot({
    colors <- c("T air" = "black", "T body" = "blue")

    ggplot() +
      geom_line(aes(x = c(0:23), y = airTemp(), color = "T air"), size = 1.3) +
      geom_line(aes(x = c(0:23), y = bodyTemp(), color = "T body"), size = 1.3) +
      xlab("Day") +
      ylab("Temperature (°C)") +
      theme_bw() +
      ggtitle("C. eriphyle body temperatures") +
      scale_color_manual(values = colors) +
      theme(
        plot.title = element_text(size = 18), axis.text = element_text(size = 13), axis.title = element_text(size = 16), legend.text = element_text(size = 13),
        legend.title = element_blank()
      )
  })

  # Define body temperature
  bodyTemp <- reactive({
    # set zs to plant height
    Tas <- mapply(air_temp_profile, airTemp(), T_s = airTemp() + 5, u_r = input$wind, zr = 2, z0 = 0.02, z = 0.02)

    # estimate radiation
    rad <- mapply(direct_solar_radiation, hour, lat = 38.9, doy = doy, elev = 2700, t0 = t0, method = "Campbell 1977")

    # partition radiation
    if (input$weather == "Clear") {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.8, lat = 38.9)
    } else if (input$weather == "Partly sunny") {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.7, lat = 38.9)
    } else {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.5, lat = 38.9)
      shade <- FALSE
    }

    dir <- rad * (1 - diff)
    dif <- rad * (diff)

    Tb <- mapply(Tb_butterfly,
      T_a = airTemp(),
      Tg = airTemp() + 5,
      Tg_sh = airTemp(),
      u = input$wind,
      H_sdir = dir,
      H_sdif = dif,
      z = zenith,
      D = input$diam / 10,
      delta = input$fur,
      alpha = as.numeric(input$abs),
      r_g = 0.3,
      shade = FALSE
    )
  })


  output$plotly <- renderPlotly({
    d <- event_data("plotly_click")
    if (input$data == "recent") {
      title <- paste0("Butterfly temperature on ", Sys.Date() - x, " in Colorado")
    } else {
      title <- "Butterfly temperature"
    }
    fig <-
      plot_ly(x = ~ c(0:23), y = ~ airTemp(), name = "T air", type = "scatter", mode = "lines") %>%
      # add_trace() %>%
      add_trace(y = ~ bodyTemp(), name = "T body") %>%
      layout(
        title = title,
        xaxis = list(title = "Hour"),
        yaxis = list(title = "Temperature (°C)")
      ) %>%
      layout(legend = list(x = 100, y = 0.5))


    if (!is.null(d)) {
      fig <- fig %>% add_segments(x = d[, "x"], xend = d[, "x"], y = min(airTemp()) - 1, yend = max(max(bodyTemp()), max(airTemp())) + 1, line = list(color = "black"), name = "selected", showlegend = FALSE)
    }
    fig
  })

  direct <- reactive({
    # estimate radiation
    rad <- mapply(direct_solar_radiation, hour, lat = 38.9, doy = doy, elev = 2700, t0 = t0, method = "Campbell 1977")

    # partition radiation
    if (input$weather == "Clear") {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.8, lat = 38.9)
    } else if (input$weather == "Partly sunny") {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.7, lat = 38.9)
    } else {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.5, lat = 38.9)
      shade <- FALSE
    }

    dir <- rad * (1 - diff)
  })

  diffuse <- reactive({
    # estimate radiation
    rad <- mapply(direct_solar_radiation, hour, lat = 38.9, doy = doy, elev = 2700, t0 = t0, method = "Campbell 1977")

    # partition radiation
    if (input$weather == "Clear") {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.8, lat = 38.9)
    } else if (input$weather == "Partly sunny") {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.7, lat = 38.9)
    } else {
      diff <- partition_solar_radiation(method = "Erbs", kt = 0.5, lat = 38.9)
      shade <- FALSE
    }

    dif <- rad * (diff)
  })

  output$heat <- renderUI({
    d <- event_data("plotly_click")
    hour <- d[, "x"]
    clock <- ifelse(is.null(d), "Click an hour on the plot", paste0(hour, ":00"))
    air <- ifelse(is.null(d), "", round(airTemp()[hour + 1], digits = 1))
    body <- ifelse(is.null(d), "", round(bodyTemp()[hour + 1], digits = 1))

    D <- input$diam / 10
    fur <- input$fur / 10
    A <- pi * D * 2 # surface area

    dir <- ifelse(is.null(d), "", round(direct()[hour + 1], digits = 1))
    dif <- ifelse(is.null(d), "", round(diffuse()[hour + 1], digits = 1))

    if (!is.null(d)) {
      Tsky <- (1.22 * air - 20.4) + 273.15
      atemp <- air + 273.15
      btemp <- body + 273.15
    }

    abs <- as.numeric(input$abs)
    solar <- ifelse(is.null(d), "", round(abs * (A / 2) * dir * cos(zenith[hour + 1] * pi / 180) +
      abs * (A / 2) * dif + abs * 0.3 * (A / 2) * (dir + dif), digits = 1))

    thermal <- ifelse(is.null(d), "", round(0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - Tsky^4) +
      0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - (atemp + 5)^4), digits = 1))

    R_e <- input$wind * D / (15.68 * 10^-2)
    N_u <- 0.6 * R_e^0.5
    h_c <- N_u * 0.25 / D
    convection <- ifelse(is.null(d), "", round((1 / h_c + (0.15 + fur) * log((0.15 + fur) / 0.15) / 1.3)^-1 * A * (btemp - atemp), digits = 1))

    HTML(
      "Selected hour: ", clock,
      "</br>Air temperature (°C): ", air,
      "</br>Body temperature (°C): ", body,
      "</br>Solar radiative heat flux (mW): ", solar,
      "</br>Thermal raditive flux (mW): ", thermal,
      "</br>Convective heat flux (mW): ", convection
    )
  })

  output$summary <- renderUI({
    d <- event_data("plotly_click")
    hour <- d[, "x"]
    ground <- ifelse(is.null(d), paste("Air temp +", 5), round(airTemp()[hour + 1], digits = 1) + 5)

    dir <- ifelse(is.null(d), "", round(direct()[hour + 1], digits = 1))
    dif <- ifelse(is.null(d), "", round(diffuse()[hour + 1], digits = 1))

    if (input$weather == "Clear") {
      weather <- "Clear (Direct)"
    }
    weeather <-
      HTML(
        "Wind speed (u): ", input$wind,
        "m/s</br>Weather: ", input$weather,
        "</br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8594;Direct solar radiation: ", dir,
        "W/m<sup>2</sup></br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8594;Diffuse solar radiation: ", dif,
        "W/m<sup>2</sup></br>Wing absorptivity (&alpha;): ", input$abs,
        "</br>Thoracic diameter (D): ", input$diam,
        "mm </br>Fur thickness (&delta;): ", input$fur,
        "mm </br>Ground temperature: ", ground, "°C"
      )
  })

  output$details <- renderUI({
    d <- event_data("plotly_click")
    validate(
      need(!is.null(d), "Select an hour on the plot for detailed analysis.")
    )
    hour <- d[, "x"]
    clock <- ifelse(is.null(d), "None", paste0(hour, ":00"))
    air <- ifelse(is.null(d), "", round(airTemp()[hour + 1], digits = 1))
    body <- ifelse(is.null(d), "", round(bodyTemp()[hour + 1], digits = 1))

    dir <- ifelse(is.null(d), "", round(direct()[hour + 1], digits = 1))
    dif <- ifelse(is.null(d), "", round(diffuse()[hour + 1], digits = 1))

    D <- input$diam / 10
    fur <- input$fur / 10
    A <- pi * D * 2 # surface area

    if (!is.null(d)) {
      Tsky <- (1.22 * air - 20.4) + 273.15
      atemp <- air + 273.15
      btemp <- body + 273.15
    }

    abs <- as.numeric(input$abs)

    R_e <- input$wind * D / (15.68 * 10^-2)
    N_u <- 0.6 * R_e^0.5
    h_c <- N_u * 0.25 / D

    HTML(
      .noWS = "before", "<b>Solar radiative</b>",
      "<br> &alpha; &times; A<sub>s,dir</sub> &times; H<sub>s,dir</sub> &times; cos(z) +
                                &alpha; &times; A<sub>s,ref</sub> &times; H<sub>s,dif</sub> + &alpha; &times; r<sub>g</sub> &times; A<sub>s,ref</sub> &times; H<sub>s,total</sub>",
      "<br> = ", input$abs, "&times; (&pi; &times;", input$diam / 10, " / 2) &times;", dir, "&times; cos(", round(zenith[hour + 1], digits = 1), "°) + ", input$abs, "&times; (&pi; &times;", input$diam / 10, "/ 2)", "&times; ",
      dif, "+ ", input$abs, "&times; 0.3 &times; (&pi; &times;", input$diam / 10, "/ 2) &times;", (dir + dif),
      "<br> =", round(abs * (A / 2) * dir * cos(zenith[hour + 1] * pi / 180) +
        abs * (A / 2) * dif + abs * 0.3 * (A / 2) * (dir + dif), digits = 1),
      "<br><br><b>Thermal radiative</b>",
      "<br> 0.5A<sub>t</sub> &times; &epsilon;&sigma;(T<sub>b</sub><sup>4</sup> - T<sub>sky</sub><sup>4</sup>) +
                                              0.5A<sub>t</sub> &times; &epsilon;&sigma;(T<sub>b</sub><sup>4</sup> - T<sub>g</sub><sup>4</sup>)",
      "<br> = 0.5 &times; (&pi; &times;", input$diam / 10, ") &times; 1 &times; (5.67 &times; 10<sup>-9</sup>) &times; ((", body, " + 273.15)<sup>4</sup> - ((1.22 &times; ", air, " - 20.4) + 273.15)<sup>4</sup>) +
         0.5 &times; (&pi; &times;", input$diam / 10, ") &times; 1 &times; (5.67 &times; 10<sup>-9</sup>) &times; ((", body, " + 273.15)<sup>4</sup> - (", air + 5, "+ 273.15)<sup>4</sup>)
         <br> =", round(0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - Tsky^4) +
        0.5 * A * 1 * (5.67 * 10^-9) * (btemp^4 - (atemp + 5)^4), digits = 1),
      "<br><br><b>Convective</b>",
      "<br> (1 / (0.6 &times; (uD / &nu;)<sup>0.5</sup>) &times; k<sub>a</sub> / D + (r<sub>i</sub> + &delta;) &times; ln((r<sub>i</sub> + &delta;) / r<sub>i</sub>) / k<sub>e</sub>)<sup>-1</sup> &times; A<sub>c</sub> (T<sub>b</sub> - T<sub>a</sub>)
         <br> = (1 / (0.6 &times; (", input$wind * 100, "&times; ", D, "/ 15.68 &times; 10<sup>-2</sup>)<sup>0.5</sup>) &times; 0.25 / ", D,
      "+ (0.15 + ", fur, ") &times; ln((0.15 +", fur, ") / 1.3)<sup>-1</sup> &times; (&pi; &times;", D, ") &times; (", body, "-", air, ")
         <br> = ", round((1 / h_c + (0.15 + fur) * log((0.15 + fur) / 0.15) / 1.3)^-1 * A * (btemp - atemp), digits = 1)
    )
  })

  output$variables <- renderUI({
    if (input$switch == "Text") {
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
           <br>A<sub>c</sub>: Convective heat transfer surface area")
    } else {
      d <- event_data("plotly_click")
      validate(
        need(!is.null(d), "Click an hour on the plot")
      )
      hour <- d[, "x"]
      clock <- ifelse(is.null(d), "None", paste0(hour, ":00"))
      air <- ifelse(is.null(d), "", round(airTemp()[hour + 1], digits = 1))
      body <- ifelse(is.null(d), "", round(bodyTemp()[hour + 1], digits = 1))
      dir <- ifelse(is.null(d), "", round(direct()[hour + 1], digits = 1))
      dif <- ifelse(is.null(d), "", round(diffuse()[hour + 1], digits = 1))

      D <- input$diam / 10
      fur <- input$fur / 10
      A <- pi * D * 2 # surface area

      if (!is.null(d)) {
        Tsky <- (1.22 * air - 20.4) + 273.15
        atemp <- air + 273.15
        btemp <- body + 273.15
      }

      abs <- as.numeric(input$abs)

      R_e <- input$wind * D / (15.68 * 10^-2)
      N_u <- 0.6 * R_e^0.5
      h_c <- N_u * 0.25 / D
      HTML("<b>Solar</b>
           <br>A<sub>s,dir</sub>:", round(pi * input$diam / 10 / 2, digits = 1), "cm<sup>2</sup> (Half of total surface area)
           <br>H<sub>s,dir</sub>:", dir, "mW/cm<sup>2</sup>
           <br>z:", round(zenith[hour + 1], digits = 1), "°
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
           <br>A<sub>c</sub>:", round(pi * input$diam / 10, digits = 1), "cm<sup>2</sup> (Total surface area)")
    }
  })

  observeEvent(input$showSidebar, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  })

  observeEvent(input$hideSidebar, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })

  observeEvent(input$sidebar, {
    if (input$sidebar) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
}
