library(RPostgreSQL)
library(stringr)
library(tidyr)
# library(RCassandra)
psql <- dbDriver("PostgreSQL")
# ########################## Fetch data from PostGreSQL ##########################
con <- dbConnect(psql, dbname = "machinetalkspc",
                 host = "192.168.10.51", port = 5432,
                 user = "postgres", password = 'egsadmin')
# rm(pw) # removes the password

plant_master <- dbGetQuery(con, "SELECT * from plant_master")
cell_master <- dbGetQuery(con, "SELECT * from cell_master")
machine_master <- dbGetQuery(con, "SELECT * from machine_master")
mt_device_master <- dbGetQuery(con, "SELECT * from mt_device_master")
unit_of_measurement <- dbGetQuery(con, "SELECT * from unit_of_measurement")
assembly_master <- dbGetQuery(con, "SELECT * from assembly_master")
# assembly_machine_master <- dbGetQuery(con, "SELECT * from assembly_machine_master")
# machine_mt_device <- dbGetQuery(con, "SELECT * from machine_mt_device")
parameter_master <- dbGetQuery(con, "SELECT * from parameter_master")
shift_master <- dbGetQuery(con, "SELECT * from shift_master")
production_plan_master <- dbGetQuery(con, "SELECT * from production_plan_master")
machine_data <- dbGetQuery(con, "SELECT * from machine_data")
Sensor_Engine_Master <- dbGetQuery(con, "SELECT * from Sensor_Engine_Master")
# param_data <- dbGetQuery(con, "SELECT machine_data.machine_id, machine_data.mt_device_id, machine_data.parameter_id, machine_data.parameter_value, machine_data.local_date_time,
#                          machine_master.machine_name, mt_device_master.mt_device_name, parameter_master.parameter_name,
#                          parameter_master.parameter_type, parameter_master.usl_value, parameter_master.lsl_value, parameter_master.central_limit_value,
#                          production_plan_master.job_id, job_master.job_name
#                          from machine_data
#                          inner join machine_master on machine_master.machine_id = machine_data.machine_id
#                          inner join mt_device_master on mt_device_master.mt_device_id = machine_data.mt_device_id
#                          inner join parameter_master on parameter_master.parameter_id = machine_data.parameter_id
#                          inner join production_plan_master on production_plan_master.machine_id = machine_data.machine_id
#                          inner join job_master on job_master.job_id = production_plan_master.job_id")
param_data <- machine_data
dbDisconnect(con)

# ########################## Fetch data from Cassandra ##########################
# cas_con <- RC.connect(host ="192.168.10.51", port = 9042)
# RC.use(cas_con, "machinetalk")
# RC.login(cas_con, username = "root", password = "root")
# 
# machine_data <- RC.read.table(c, "machine_data")
# 
# RC.close(cas_con)
# ########################## Store setup to database ##########################

addQuotes <- function(x) sprintf("'%s'", paste(x, collapse = "','"))
# addQuotes(vec1)

saveData <- function(data) {
  print("save data :: ")
  # data$start_time <- as.Date(data$start_time)
  # data$start_time <- str_replace_all(string=data$start_time, pattern=" ", repl="")
  # params=data["setup_name","setup_description","parameter_type","job_id","machine_id","parameter_id","start_time","mean"]
  data$setup_name <- addQuotes(data$setup_name)
  data$setup_description <- addQuotes(data$setup_description)
  data$parameter_type <- addQuotes(data$parameter_type)
  data$start_time <- addQuotes(data$start_time)
  data[is.na(data)]<- 0
  print(data)
  data$setup_description <- as.character(data$setup_description)
  tryCatch({
    # Connect to the database
    pcon <- dbConnect(psql, dbname = "machinetalkspc", host = "192.168.10.51", port = 5432, user
                      = "postgres", password = "egsadmin")
    # Construct the update query by looping over the data fields
    
    write_sql <- paste("Insert into spc_setup_master (setup_name,setup_description,parameter_type,job_id,machine_id,parameter_id,start_time,mean,sigma,plus_one_sigma,minus_one_sigma,plus_two_sigma,minus_two_sigma,plus_three_sigma,minus_three_sigma,r_bar,x_bar,pci_value,usl_value,ucl_value_r,ucl_value_x,central_limit_value,lcl_value_x,lcl_value_r,lsl_value) values (",data$setup_name,",",data$setup_description,",",data$parameter_type,",",data$job_id,",",data$machine_id,",",data$parameter_id,",",data$start_time,",",data$mean,",",data$sigma,",",data$plus_one_sigma,",",data$minus_one_sigma,",",data$plus_two_sigma,",",data$minus_two_sigma,",",data$plus_three_sigma,",",data$minus_three_sigma,",",data$r_bar,",",data$x_bar,",",data$pci_value,",",data$usl_value,",",data$ucl_value_r,",",data$ucl_value_x,",",data$central_limit_value,",",data$lcl_value_x,",",data$lcl_value_r,",",data$lsl_value,")",sep="")
    dbSendQuery(pcon,write_sql)
    dbDisconnect(pcon)
    return(TRUE)
  },
  error=function(e){
    return(FALSE)
  }
  )
}

loadData <- function(){
  pcon <- dbConnect(psql, dbname = "machinetalkspc", host = "192.168.10.51", port = 5432, user
                    = "postgres", password = "egsadmin")
  setup_frame <- dbGetQuery(pcon, "SELECT * from spc_setup_master")
  print('setup from database :: ')
  print(setup_frame)
  dbDisconnect(pcon)
  return(setup_frame)
}