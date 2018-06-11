source("database.R", local = TRUE)
library(stringr)
library(tidyr)
library(dplyr)
# param_data <- read.csv("machine_data.csv")

param_data <- merge(x = param_data, y = machine_master[ , c("machine_id", "machine_name")], by = "machine_id", all.x=TRUE)
param_data <- merge(x = param_data, y = mt_device_master[ , c("mt_device_id", "mt_device_name")], by = "mt_device_id", all.x=TRUE, sort = FALSE)
param_data <- merge(x = param_data, y = parameter_master[ , c("parameter_id", "parameter_name", "parameter_type", "usl_value", "central_limit_value", "lsl_value")], by = "parameter_id", all.x=TRUE, sort = FALSE)
# param_data <- merge(x = param_data, y = production_plan_master[ , c("machine_id", "plant_id", "cell_id", "shift_id", "job_id")], by = "machine_id", all.x=TRUE, sort = FALSE)

product_param <- param_data[param_data$parameter_type == 'Product',]
process_param <- param_data[param_data$parameter_type == 'Process',]

product_param$local_date_time <- as.POSIXct(product_param$local_date_time, format="%m/%d/%Y %H:%M")
process_param$local_date_time <- as.POSIXct(process_param$local_date_time, format="%m/%d/%Y %H:%M")
# # tempDate <- read.csv(text=sub("\\s+(\\S+)$", ",\\1", process_param$local_date_time), 
# #                      col.names=c('local_date', 'local_time'), header=FALSE)
# # process_param <- cbind(process_param, tempDate)
# # process_param$local_date <- as.Date(process_param$local_date)
# colnames(process_param)[colnames(process_param) == 'ï..machine_id'] <- 'machine_id'
# colnames(product_param)[colnames(product_param) == 'ï..machine_id'] <- 'machine_id'
# process_param$job_type_id <- NULL
# process_param$job_type_name <- NULL
# process_param$local_date_time <- NULL
product_job_choices = setNames(as.character(product_param$job_type_id),product_param$job_type_name)
product_parameter_choices = setNames(as.character(product_param$parameter_id),product_param$parameter_name)
product_machine_choices = setNames(as.character(product_param$machine_id),product_param$machine_name)
product_job_choices_monitor = setNames(as.character(product_param$job_type_id),product_param$job_type_name)
product_parameter_choices_monitor = setNames(as.character(product_param$parameter_id),product_param$parameter_name)
product_machine_choices_monitor = setNames(as.character(product_param$machine_id),product_param$machine_name)
product_job_choices_ac = setNames(as.character(product_param$job_type_id),product_param$job_type_name)
product_parameter_choices_ac = setNames(as.character(product_param$parameter_id),product_param$parameter_name)
product_machine_choices_ac = setNames(as.character(product_param$machine_id),product_param$machine_name)

process_machine_choices = setNames(as.character(process_param$machine_id),process_param$machine_name)
process_parameter_choices = setNames(as.character(process_param$parameter_id),process_param$parameter_name)
process_machine_choices_monitor = setNames(as.character(process_param$machine_id),process_param$machine_name)
process_parameter_choices_monitor = setNames(as.character(process_param$parameter_id),process_param$parameter_name)
process_setup_choices_monitor = c()
process_machine_choices_ac = setNames(as.character(process_param$machine_id),process_param$machine_name)
process_parameter_choices_ac = setNames(as.character(process_param$parameter_id),process_param$parameter_name)

# ########################## setup out put ##########################
setup_output <- data.frame("setup_name"=NA,"setup_description"=NA,"parameter_type"=NA, "job_id"=NA, "machine_id"=NA, "parameter_id"=NA, "start_time"=NA, "mean"=NA, "sigma"=NA, "plus_one_sigma"=NA, "minus_one_sigma"=NA, "plus_two_sigma"=NA, "minus_two_sigma"=NA, "plus_three_sigma"=NA, "minus_three_sigma"=NA, "r_bar"=NA, "x_bar"=NA, "pci_value"=NA, "usl_value"=NA, "ucl_value_r"=NA, "ucl_value_x"=NA, "central_limit_value"=NA, "lcl_value_x"=NA, "lcl_value_r"=NA, "lsl_value"=NA)
setup_frame <- data.frame("setup_id"=NA,"setup_name"=NA,"setup_description"=NA,"parameter_type"=NA, "job_id"=NA, "machine_id"=NA, "parameter_id"=NA, "start_time"=NA, "mean"=NA, "sigma"=NA, "plus_one_sigma"=NA, "minus_one_sigma"=NA, "plus_two_sigma"=NA, "minus_two_sigma"=NA, "plus_three_sigma"=NA, "minus_three_sigma"=NA, "r_bar"=NA, "x_bar"=NA, "pci_value"=NA, "usl_value"=NA, "ucl_value_r"=NA, "ucl_value_x"=NA, "central_limit_value"=NA, "lcl_value_x"=NA, "lcl_value_r"=NA, "lsl_value"=NA)
# setup_output <-data.frame("parameter_type"=NA)
# setup_frame <- data.frame()
# ########################## constants for x-bar calculation ##########################
n <- 4
k <- 15
d2 <- 2.059
D4 <- 2.282
D3 <- 0
A2 <- 0.729
#Frequency Ditribution one sigma
area_one_sd <- round(pnorm(1) - pnorm(-1), 4)
area_one_sd

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x, mean = mean(x),sd=sd(x))
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -1 | x >= 1] <- NA
  return(norm_one_sd)
}

#Frequency Ditribution two sigma
area_two_sd <- round(pnorm(2) - pnorm(-2), 4)
area_two_sd

dnorm_two_sd <- function(x){
  norm_two_sd <- dnorm(x, mean = mean(x),sd=sd(x))
  # Have NA values outside interval x in [-2, 2]:
  norm_two_sd[x <= -2 | x >= 2] <- NA
  return(norm_two_sd)
}

#Frequency Ditribution three sigma
area_three_sd <- round(pnorm(3) - pnorm(-3), 4)
area_three_sd

dnorm_three_sd <- function(x){
  norm_three_sd <- dnorm(x, mean = mean(x),sd=sd(x))
  # Have NA values outside interval x in [-3, 3]:
  norm_three_sd[x <= -3 | x >= 3] <- NA
  return(norm_three_sd)
}

load_data_body <- function() {
  Sys.sleep(1)
  hideElement("loading_body_page")
  showElement("main_body_content")
  # hide(id = "loading-content", anim = TRUE, animType = "fade")    
  # show("app-content")
}

load_data_sideBar <- function() {
  Sys.sleep(0)
  hideElement("loading_sidebar_page")
  showElement("main_sidebar_content")
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}