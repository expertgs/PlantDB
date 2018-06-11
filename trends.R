up_down_trend <- function(monitor_data){
  i<-0
  count_up <- 0
  count_down <- 0
  indices_up <- c()
  indices_down <- c()
  indices_up1 <- c()
  indices_down1 <- c()
  for(data_point in monitor_data$parameter_value){
    i <- i+1
    if((monitor_data$parameter_value[i+1] > data_point && !is.na(monitor_data$parameter_value[i+1]))  ){
      count_up <- count_up + 1
      indices_up <- c(indices_up, i)
    }
    else if(count_up > 0 && count_up >= 7 ){
      monitor_data$colors[i] <- "#00BCD4"
      monitor_data$symbols[i] <- "circle"
      monitor_data$sizes[i] <- 5
      indices_up <- c(indices_up, i)
      count_up <- 0
      indices_up1 <- c(indices_up1, indices_up)
      # break
    }
    else{
      count_up <- 0
      indices_up <- c()
    }
    if((monitor_data$parameter_value[i+1] < data_point && !is.na(monitor_data$parameter_value[i+1]))  ){
      count_down <- count_down + 1
      indices_down <- c(indices_down, i)
    }
    else if(count_down > 0 && count_down >= 7 ){
      monitor_data$colors[i] <- "#00BCD4"
      monitor_data$symbols[i] <- "circle"
      monitor_data$sizes[i] <- 5
      indices_down <- c(indices_down, i)
      count_down <- 0
      indices_down1 <- c(indices_down1, indices_down)
      # break
    }
    else{
      count_down <- 0
      indices_down <- c()
    }
  }
  if(count_up > 0 && count_up >= 7){
    indices_up1 <- c(indices_up1, indices_up)
  }
  print(paste('updown indices_up1 ::::', indices_up1))
  monitor_data$colors[indices_up1] <- "#FF000F"
  monitor_data$symbols[indices_up1] <- "square"
  monitor_data$sizes[indices_up1] <- 8
  
  if(count_down > 0 && count_down >= 7){
    indices_down1 <- c(indices_down1, indices_down)
  }
  print(paste('updown indices_down1 ::::', indices_down1))
  monitor_data$colors[indices_down1] <- "#031257"
  monitor_data$symbols[indices_down1] <- "square"
  monitor_data$sizes[indices_down1] <- 8
  return(monitor_data)
}

shift <- function(monitor_data, central_limit_value){
  i<-0
  count_up <- 0
  count_down <- 0
  indices_up <- c()
  indices_down <- c()
  indices_up1 <- c()
  indices_down1 <- c()
  for(data_point in monitor_data$parameter_value){
    i <- i+1
    if(data_point > central_limit_value){
      count_up <- count_up + 1
      indices_up <- c(indices_up, i)
    }
    else if(count_up > 0 && count_up >= 8 ){
      monitor_data$colors[i] <- "#00BCD4"
      monitor_data$symbols[i] <- "circle"
      monitor_data$sizes[i] <- 5
      count_up <- 0
      indices_up1 <- c(indices_up1, indices_up)
      # break
    }
    else{
      count_up <- 0
      indices_up <- c()
    }
    if(data_point < central_limit_value){
      count_down <- count_down + 1
      indices_down <- c(indices_down, i)
    }
    else if(count_down > 0 && count_down >= 8 ){
      monitor_data$colors[i] <- "#00BCD4"
      monitor_data$symbols[i] <- "circle"
      monitor_data$sizes[i] <- 5
      count_down <- 0
      indices_down1 <- c(indices_down1, indices_down)
      # break
    }
    else{
      count_down <- 0
      indices_down <- c()
    }
  }
  if(count_up > 0 && count_up >= 8){
    indices_up1 <- c(indices_up1, indices_up)
  }
    monitor_data$colors[indices_up1] <- "#FF000F"
    monitor_data$symbols[indices_up1] <- "square"
    monitor_data$sizes[indices_up1] <- 8
    
  if(count_down > 0 && count_down >= 8){
    indices_down1 <- c(indices_down1, indices_down)
  }
  monitor_data$colors[indices_down1] <- "#031257"
  monitor_data$symbols[indices_down1] <- "square"
  monitor_data$sizes[indices_down1] <- 8
  return(monitor_data)
}