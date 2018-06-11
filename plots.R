source("trends.R")
histogram <- function(temp_data){
  return(plot_ly(x=temp_data$parameter_value, type='histogram', color=I('steelblue')) %>% layout(bargap=0.001)%>%
           layout(xaxis = list(title = unique(temp_data$parameter_name)),
                  yaxis = list (title = "Frequency"))%>%
           config(displaylogo = FALSE, collaborate = FALSE) )
}

normal_distribution <- function(temp_data, setup_output){
  par(mar=c(1,1,1,1))
  h <- hist(temp_data$parameter_value, breaks = 10, density = 10,
            col = "lightgray", xlab = "Accuracy", main = "Overall") 
  xfit <- seq(min(temp_data$parameter_value), max(temp_data$parameter_value)) 
  yfit <- dnorm(xfit, mean = mean(temp_data$parameter_value), sd = sd(temp_data$parameter_value)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(temp_data$parameter_value) 
  return(
    # plot_ly(x = xfit, y = yfit, type = 'scatter', marker = list(color = I('steelblue'), size = 1, showlegend = FALSE))%>%
    plot_ly(x = xfit, y = yfit)%>%
      add_trace(name = 'Distribution', mode='lines', color = I('steelblue'))%>%
      add_trace(x=mean(temp_data$parameter_value), mode = 'lines', line = list(dash = "dash"), color = I('red'), name='Mean')%>%
      add_trace(x=setup_output$plus_one_sigma, mode = 'lines', line = list(dash = "dash"), color = I('darkgreen'), name='+1 sigma')%>%
      add_trace(x=setup_output$minus_one_sigma, mode = 'lines', line = list(dash = "dash"), color = I('darkgreen'), name='-1 sigma')%>%
      add_trace(x=setup_output$plus_two_sigma, mode = 'lines', line = list(dash = "dash"), color = I('orange'), name='+2 sigma')%>%
      add_trace(x=setup_output$minus_two_sigma, mode = 'lines', line = list(dash = "dash"), color = I('orange'), name='-2 sigma')%>%
      layout(legend=list(orientation = 'h', x=0, y=max(yfit)),xaxis = list(title = unique(temp_data$parameter_name)),yaxis = list (title = "Frequency"))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
  )
}

run_chart_fun <- function(monitor_data, setup_frame){
  machine_name <- monitor_data[monitor_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- monitor_data[monitor_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f2,
    exponentformat = "E"
  )
  
  monitor_data$colors <- "steelblue"
  monitor_data$symbols <- "circle"
  monitor_data$sizes <- 5
  monitor_data$colors[monitor_data$parameter_value > setup_frame$ucl_value_r] <- "#FF5722"
  monitor_data$symbols[monitor_data$parameter_value > setup_frame$ucl_value_r] <- "square"
  monitor_data$sizes[monitor_data$parameter_value > setup_frame$ucl_value_r] <- 8
  
  monitor_data$colors[monitor_data$parameter_value < setup_frame$lcl_value_r] <- "#FF5722"
  monitor_data$symbols[monitor_data$parameter_value < setup_frame$lcl_value_r] <- "square"
  monitor_data$sizes[monitor_data$parameter_value < setup_frame$lcl_value_r] <- 8
  
  monitor_data$colors[monitor_data$parameter_value > setup_frame$usl_value] <- "#D50000"
  monitor_data$symbols[monitor_data$parameter_value > setup_frame$usl_value] <- "square"
  monitor_data$sizes[monitor_data$parameter_value > setup_frame$usl_value] <- 8
  
  monitor_data$colors[monitor_data$parameter_value < setup_frame$lsl_value] <- "#D50000"
  monitor_data$symbols[monitor_data$parameter_value > setup_frame$usl_value] <- "square"
  monitor_data$sizes[monitor_data$parameter_value > setup_frame$usl_value] <- 8
  
  monitor_data$local_date_time <- as.POSIXct(monitor_data$local_date_time, format="%Y-%m-%d %H:%M:%S")
  print('monitor_data :::::::::::::: ')
  print(monitor_data)
  ucl <- mean(monitor_data$parameter_value) + (3 * sd(monitor_data$parameter_value))
  lcl <- mean(monitor_data$parameter_value) - (3 * sd(monitor_data$parameter_value))
  return(
    if(is.na(setup_frame$pci_value)){
      plot_ly(monitor_data, x = monitor_data$local_date_time, y = monitor_data$parameter_value) %>%
        add_markers(name = paste0(parameter_name), marker = list(color = ~I(as.character(colors)), symbol = ~I(as.character(symbols)), size = monitor_data$sizes)) %>%
        add_trace(type = 'scatter', mode = 'lines', color = I('steelblue'), showlegend = FALSE, hoverinfo="none",opacity=1) %>%
        add_trace(y=monitor_data$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
        # add_trace(y=setup_frame$ucl_value_r,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
        add_trace(y=monitor_data$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('#FFE0B2')) %>%
        # add_trace(y=setup_frame$lcl_value_r,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
        add_trace(y=monitor_data$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
        layout(xaxis = a, title = paste0(machine_name), yaxis = list(title = paste0(parameter_name), titlefont = f1, tickfont = f2)) %>%
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "bottom",  # use center of legend as anchor
                             x = "middle", y = -0.3,
                             bordercolor = "#333",
                             borderwidth = 2))%>%
        config(displaylogo = FALSE, collaborate = FALSE)
      # , xaxis = a margin = list(b = 160)
    }
    else{
      plot_ly(monitor_data, x = monitor_data$local_date_time, y = monitor_data$parameter_value) %>% 
        add_markers(name = paste0(parameter_name), marker = list(color = ~I(as.character(colors)),opacity=1, symbol = ~I(as.character(symbols)), size = monitor_data$sizes)) %>% 
        add_trace(type = 'scatter', mode = 'lines', color = I('steelblue'), showlegend = FALSE, hoverinfo="none",opacity=1) %>%
        add_trace(y=monitor_data$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('#90A4AE')) %>% 
        add_trace(y=setup_frame$ucl_value_r,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
        add_trace(y=monitor_data$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('#FFE0B2')) %>%
        add_trace(y=setup_frame$lcl_value_r,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
        add_trace(y=monitor_data$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
        layout(xaxis = a, title = paste0(machine_name), yaxis = list(title = paste0(parameter_name), titlefont = f1, tickfont = f2)) %>%
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5, y = -0.3,
                             bordercolor = "#333",
                             borderwidth = 2))%>%
        config(displaylogo = FALSE, collaborate = FALSE)
      # , xaxis = a margin = list(b = 160)
      # , marker = list(color = ~I(as.character(colors)), symbol = ~I(as.character(symbols)), size = monitor_data$sizes)
    }
  )
}

up_down_trend_fun <- function(monitor_data, setup_frame){
  print('setup_frame up_down_trend_fun @@@:: ')
  print(setup_frame)
  machine_name <- monitor_data[monitor_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- monitor_data[monitor_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f2,
    exponentformat = "E"
  )
  
  monitor_data$colors <- "steelblue"
  monitor_data$symbols <- "circle"
  monitor_data$sizes <- 5
  # monitor
  monitor_data <- up_down_trend(monitor_data)
  ucl <- mean(monitor_data$parameter_value) + (3 * sd(monitor_data$parameter_value))
  lcl <- mean(monitor_data$parameter_value) - (3 * sd(monitor_data$parameter_value))
  monitor_data$local_date_time <- as.POSIXct(monitor_data$local_date_time, format="%Y-%m-%d %H:%M:%S")
  print('monitor_data :::::::::::::: ')
  print(monitor_data)
  return(
    plot_ly(monitor_data, x = monitor_data$local_date_time, y = monitor_data$parameter_value) %>%
      add_markers(name = paste0(parameter_name), marker = list(color = ~I(as.character(colors)),opacity=1, symbol = ~I(as.character(symbols)), size = monitor_data$sizes)) %>%
      add_trace(type = 'scatter', mode = 'lines', color = I('steelblue'), showlegend = FALSE, hoverinfo="none",opacity=1) %>%
      add_trace(y=monitor_data$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
      add_trace(y=setup_frame$ucl_value_r,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
      add_trace(y=monitor_data$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('#FFE0B2')) %>%
      add_trace(y=setup_frame$lcl_value_r,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
      add_trace(y=monitor_data$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
      layout(xaxis = a, title = paste0(machine_name), yaxis = list(title = paste0(parameter_name), titlefont = f1, tickfont = f2)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5, y = -0.3,
                           bordercolor = "#333",
                           borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
    # , xaxis = a margin = list(b = 160)
  )
}

shift_fun <- function(monitor_data, setup_frame){
  machine_name <- monitor_data[monitor_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- monitor_data[monitor_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f2,
    exponentformat = "E"
  )
  
  monitor_data$colors <- "steelblue"
  monitor_data$symbols <- "circle"
  monitor_data$sizes <- 5
  monitor_data <- shift(monitor_data, unique(monitor_data$central_limit_value)) 
  
  monitor_data$local_date_time <- as.POSIXct(monitor_data$local_date_time, format="%Y-%m-%d %H:%M:%S")
  print('monitor_data :::::::::::::: ')
  print(monitor_data)
  ucl <- mean(monitor_data$parameter_value) + (3 * sd(monitor_data$parameter_value))
  lcl <- mean(monitor_data$parameter_value) - (3 * sd(monitor_data$parameter_value))
  return(
    plot_ly(monitor_data, x = monitor_data$local_date_time, y = monitor_data$parameter_value) %>%
      add_markers(name = paste0(parameter_name), marker = list(color = ~I(as.character(colors)),opacity=1, symbol = ~I(as.character(symbols)), size = monitor_data$sizes)) %>%
      add_trace(type = 'scatter', mode = 'lines', color = I('steelblue'), showlegend = FALSE, hoverinfo="none",opacity=1) %>%
      add_trace(y=monitor_data$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
      add_trace(y=setup_frame$ucl_value_r,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
      add_trace(y=monitor_data$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('#FFE0B2')) %>%
      add_trace(y=setup_frame$lcl_value_r,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
      add_trace(y=monitor_data$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
      layout(xaxis = a, title = paste0(machine_name), yaxis = list(title = paste0(parameter_name), titlefont = f1, tickfont = f2)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5, y = -0.3,
                           bordercolor = "#333",
                           borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
    # , xaxis = a margin = list(b = 160)
  )
}

range_chart_fun <- function(monitor_data, setup_frame, grouping){
  machine_name <- monitor_data[monitor_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- monitor_data[monitor_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 45,
    tickfont = f2,
    exponentformat = "E"
  )
  return(
    plot_ly(grouping, x = grouping$groups, y = grouping$Range) %>%
      add_markers(name = paste0(parameter_name), color = I('#00BCD4')) %>%
      add_trace(type = 'scatter', mode = 'lines', color = I('#00BCD4'), showlegend = FALSE, hoverinfo="none") %>%
      # add_trace(y=setup_frame$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('red')) %>%
      add_trace(y=setup_frame$ucl_value_r,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('darkgreen')) %>%
      add_trace(y=setup_frame$r_bar,name = "R - BAR", mode ='lines', line = list(dash = "solid"), color = I('orange')) %>%
      add_trace(y=setup_frame$lcl_value_r,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('darkgreen')) %>%
      # add_trace(y=setup_frame$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('red')) %>%
      layout(xaxis = a, title = paste0(machine_name), yaxis = list(title = paste0(parameter_name), titlefont = f1, tickfont = f2)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "bottom",  # use center of legend as anchor
                           x = 0.25, y = -0.3,
                           bordercolor = "#333",
                           borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
    # , xaxis = a margin = list(b = 160)
  )
}

x_bar_chart <- function(monitor_data, setup_frame, grouping){
  machine_name <- monitor_data[monitor_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- monitor_data[monitor_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 45,
    tickfont = f2,
    exponentformat = "E"
  )
  return(
    plot_ly(grouping, x = grouping$groups, y = grouping$Mean) %>%
      add_markers(name = paste0(parameter_name), color = I('#00BCD4')) %>%
      add_trace(type = 'scatter', mode = 'lines', color = I('#00BCD4'), showlegend = FALSE, hoverinfo="none") %>%
      add_trace(y=setup_frame$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('red')) %>%
      add_trace(y=setup_frame$ucl_value_x,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('darkgreen')) %>%
      add_trace(y=setup_frame$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('orange')) %>%
      add_trace(y=setup_frame$lcl_value_x,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('darkgreen')) %>%
      add_trace(y=setup_frame$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('red')) %>%
      layout(xaxis = a, title = paste0(machine_name), yaxis = list(title = paste0(parameter_name), titlefont = f1, tickfont = f2)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "bottom",  # use center of legend as anchor
                           x = 0.25, y = -0.3,
                           bordercolor = "#333",
                           borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
    # , xaxis = a margin = list(b = 160)
  )
}

bar_chart_proc_ac_fun <- function(ac_data, setup_frame, setup_data){
  machine_name <- ac_data[ac_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- ac_data[ac_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  return(
    plot_ly(setup_data, x = ~setup_name, y = ~ucl_value_r, type = 'bar', name = 'UCL') %>% 
      # add_lines() %>% add_markers() %>%
      add_trace(y = ~lcl_value_r, name = 'LCL') %>% 
      # add_lines(y = ~lcl_value_r, name = "LCL", color = I('gold1')) %>% add_markers(y = ~lcl_value_r, name = "LCL", color = I('gold1')) %>%
      layout(autosize = T, margin = list(b = 160), yaxis = list(title = 'Control Limits',titlefont = f1, tickfont = f2), 
             xaxis = list(title = 'Setups',titlefont = f1, tickfont = f2, tickangle = 90), barmode = 'group', title = paste0(machine_name)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "bottom",  # use center of legend as anchor
                           x = 0.25, y = -0.3,
                           bordercolor = "#333",
                           borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
  )
}

run_chart_ac_fun <- function(ac_data, setup_frame, last_setup){
  machine_name <- ac_data[ac_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- ac_data[ac_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f2,
    exponentformat = "E"
  )
  ac_mean <- mean(ac_data$parameter_value)
  ac_sigma <- sd(ac_data$parameter_value)
  ac_ucl <- ac_mean + (3*ac_sigma)
  ac_lcl <- ac_mean - (3*ac_sigma)
  return(
    plot_ly(ac_data, x = ac_data$local_date_time, y = ac_data$parameter_value) %>%
      add_markers(name = paste0(unique(ac_data$parameter_name)), color = I('#00BCD4')) %>%
      add_trace(type = 'scatter', mode = 'lines', color = I('#00BCD4'), showlegend = FALSE, hoverinfo="none") %>%
      add_trace(y=unique(ac_data$usl_value),name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
      add_trace(y=ac_ucl,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
      add_trace(y=ac_mean,name =  "Average", mode ='lines', line = list(dash = "dashed"), color = I('#ff5c5c')) %>%
      add_trace(y=ac_data$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('#FFE0B2')) %>%
      add_trace(y=ac_lcl,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('#B0BEC5')) %>%
      add_trace(y=unique(ac_data$lsl_value),name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('#90A4AE')) %>%
      layout(autosize = T, margin = list(b = 160),title = paste0(machine_name), xaxis = a, yaxis = list(title = paste0(unique(ac_data$parameter_name)), titlefont = f1, tickfont = f2))
    %>% layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "bottom",  # use center of legend as anchor
                             x = 0.25, y = -0.3,
                             bordercolor = "#333",
                             borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
  )
}

bar_chart_prod_ac_fun <- function(ac_data, setup_frame, setup_data){
  machine_name <- ac_data[ac_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- ac_data[ac_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  return(
    plot_ly(setup_data, x = ~setup_name, y = ~ucl_value_x, type = 'bar', name = 'UCL') %>% add_lines() %>% add_markers() %>%
      add_trace(y = ~lcl_value_x, name = 'LCL') %>% add_lines(y = ~lcl_value_x, name = "LCL", color = I('gold1')) %>% add_markers(y = ~lcl_value_x, name = "LCL", color = I('gold1')) %>%
      layout(autosize = T, margin = list(b = 160), yaxis = list(title = 'Control Limits',titlefont = f1, tickfont = f2), xaxis = list(title = 'Setups',titlefont = f1, tickfont = f2, tickangle = 0), barmode = 'group', title = paste0(machine_name))
    %>% layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "bottom",  # use center of legend as anchor
                             x = 0.25, y = -0.3,
                             bordercolor = "#333",
                             borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
  )
}

x_bar_chart_ac_fun <- function(ac_data, setup_frame, last_setup, grouping){
  machine_name <- ac_data[ac_data$machine_id == setup_frame$machine_id, ]
  machine_name <- unique(machine_name$machine_name)
  parameter_name <- ac_data[ac_data$parameter_id == setup_frame$parameter_id, ]
  parameter_name <- unique(parameter_name$parameter_name)
  
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "darkgrey"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  a <- list(
    title = "Time Stamp",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 45,
    tickfont = f2,
    exponentformat = "E"
  )
  
  return(
    plot_ly(grouping, x = as.character(grouping$groups), y = grouping$Mean) %>%
      add_markers(name = paste0(parameter_name), color = I('#00BCD4')) %>%
      add_trace(type = 'scatter', mode = 'lines', color = I('#00BCD4'), showlegend = FALSE, hoverinfo="none") %>%
      add_trace(y=last_setup$usl_value,name = "USL", mode = 'lines', line = list(dash = "dash"), color = I('red')) %>%
      add_trace(y=last_setup$ucl_value_x,name = "UCL", mode ='lines', line = list(dash = "dot"), color = I('darkgreen')) %>%
      add_trace(y=last_setup$central_limit_value,name = "Central limit", mode ='lines', line = list(dash = "solid"), color = I('orange')) %>%
      add_trace(y=last_setup$lcl_value_x,name = "LCL", mode ='lines', line = list(dash = "dot"), color = I('darkgreen')) %>%
      add_trace(y=last_setup$lsl_value,name = "LSL", mode ='lines', line = list(dash = "dash"), color = I('red')) %>%
      layout(autosize = T, margin = list(b = 160),title = paste0(machine_name), xaxis = a, yaxis = list(title = paste0(parameter_name, " Values"), titlefont = f1, tickfont = f2))
    %>% layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "bottom",  # use center of legend as anchor
                             x = 0.25, y = -0.3,
                             bordercolor = "#333",
                             borderwidth = 2))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
  )
}