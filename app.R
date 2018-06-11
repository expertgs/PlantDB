# https://github.com/emitanaka/shinycustomloader
# https://emitanaka.github.io/shinycustomloader/
# https://loading.io/spinner/square/-square-grid-loader
# https://loading.io/spinner/
source("airDatepickerInput.R")
## app.R ##
library(shinydashboard)
library(devtools)
library(shiny)

library(shinyTime)
# devtools::install_github("luca-scr/qcc")
library(qcc)
# devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
# devtools::install_github("ropensci/plotly")
library(plotly)
# devtools::install_github("hadley/tidyverse")
# library(tidyverse)
library(shinythemes)
library(shinycssloaders)
# devtools::install_github("nik01010/dashboardthemes")
library(dashboardthemes)
# devtools::install_github("daattali/shinyjs")
library(shinyjs)
# devtools::install_github("daattali/shinyalert")
library(shinyalert)
# devtools::install_github("jbkunst/highcharter")
#library(highcharter)
library(DT)
# library(shinyWidgets)
library(shinydashboardPlus)

source("app_global.R")
source("plots.R")
source("override.R", local = TRUE)
source("database.R", local = TRUE)
# source("airDatepickerInput.R")

header<- dashboardHeader(title = "Statistical Process Control", titleWidth = 280,
                         
                         
                         tags$li(class = "dropdown",
                           tags$span(Sys.Date() + Sys.time())
                    
                           # tags$img(src = "spc_trans_white.png",
                           #            id = "spc_icon")
                         )
                         # tags$li(class = "dropdown",
                         #         tags$li(class = "dropdown", actionLink("setup_page", "Setup")),
                         #         tags$li(class = "dropdown", actionLink("monitor_page", "Monitor")),
                         #         tags$li(class = "dropdown", actionLink("ac_page", "Average Charts")) )
)

sidebar<- dashboardSidebar(width = 280,
                           tags$head(
                             includeCSS(path = "styles.css")
                             # tags$link(href="bootstrap-timepicker/css/bootstrap-timepicker.min.css", rel = "stylesheet"),
                             # tags$script(type="text/javascript", src="bootstrap-timepicker/js/bootstrap-timepicker.min.js")
                            
                             # tags$link(href="bootstrap-datetimepicker-master/build/css/bootstrap-datetimepicker.min.css", rel = "stylesheet"),
                             # # tags$script(type="text/javascript", src="jquery.min.js"),
                             # tags$script(type="text/javascript", src="moment/min/moment.min.js"),
                             # tags$script(type="text/javascript", src="bootstrap-datetimepicker-master/build/js/bootstrap-datetimepicker.min.js")
                           ),
                           useShinyjs(),
                           div(
                             id = "loading_sidebar_page"
                             # h1("Loading...")
                           ),
                           hidden(
                             div(
                               id = "main_sidebar_content",
                               sidebarMenu(id = 'selection',
                                           menuItem("Setup", icon = icon("cogs", lib = "font-awesome"), tabName = "setup_page",expandedName = "setup",
                                                    menuItem('Process', tabName = 'tabset_process',expandedName = "setup_process", startExpanded = FALSE,
                                                                span(
                                                                selectInput("process_machine", label = paste0("Choose Machine"),
                                                                                                             process_machine_choices),
                                                                                                 selectInput("process_parameter", label = paste0("Choose Parameter"),
                                                                                                             process_parameter_choices),
                                                                # tags$label(id='control-label', 'Start Time'),
                                                                # tags$div(class = "input-group bootstrap-timepicker timepicker",
                                                                #          tags$input(id = "time_input_process",
                                                                #                     type = "text",
                                                                #                     class = "form-control input-small"),
                                                                #          tags$span(class = "input-group-addon",
                                                                #                    tags$i(class= "glyphicon glyphicon-time"))),
                                                                # dateInput('date_input_process',
                                                                #           label = 'Start Date ',
                                                                #           value = Sys.Date(),
                                                                #           format = "mm/dd/yyyy"
                                                                # ),
                                                                # timeInput("time_input_process", "Start time "),
                                                                 # tags$div(class='input-group date',id = "time_input_process",
                                                                 #          tags$input(id = "time_input_process",type = "text",class="form-control"),
                                                                 #          tags$span(class = "input-group-addon",
                                                                 #                    tags$i(class= "glyphicon glyphicon-calendar"))),
                                                                airDatepickerInput(
                                                                  inputId = "time_input_process",
                                                                  label = "Start date and time",
                                                                  timepicker = TRUE,
                                                                  timepickerOpts = timepickerOptions(timeFormat = 'hh:ii'),
                                                                  position = 'top left'
                                                                ),
                                                                # tags$input(id = "time_input_process",class="datepicker-here", "data-timepicker"="true", "data-language"='en'),
                                                                actionButton("plot_process", "Plot"),
                                                                useShinyalert(),
                                                                conditionalPanel("output.plot_process_click_reactive",
                                                                                 textInput("setup_name", label = paste0("Setup Name "), value = ""),
                                                                                 textInput("setup_description", label = paste0("Setup Description "), value = ""),
                                                                                 actionButton("save_setup", "Save setup")
                                                                )
                                                                )),
                                                    menuItem('Product', tabName = 'tabset_product',expandedName = "setup_product", startExpanded = FALSE,
                                                             span(
                                                               selectInput("product_job", label = paste0("Choose Job Type"), 
                                                                                                            product_job_choices),
                                                                                                selectInput("product_parameter", label = paste0("Select parameter"),
                                                                                                            product_parameter_choices),
                                                                                                selectInput("product_machine", label = paste0("Choose Machine"),
                                                                                                            product_machine_choices),
                                                                                                dateInput('date_input_product',
                                                                                                          label = 'Start Date ',
                                                                                                          value = Sys.Date(),
                                                                                                          format = "mm/dd/yyyy"
                                                                                                ),
                                                                                                timeInput("time_input_product", "Start time ", value = Sys.time()),
                                                               actionButton("plot_product", "Plot"),
                                                               useShinyalert(),
                                                               conditionalPanel("output.plot_product_click_reactive",
                                                                                textInput("setup_name", label = paste0("Setup Name "), value = ""),
                                                                                textInput("setup_description", label = paste0("Setup Description "), value = ""),
                                                                                actionButton("save_setup", "Save setup")
                                                               )
                                                             )),
                                        startExpanded = FALSE),
                                      menuItem("Monitoring", icon = icon("line-chart", lib = "font-awesome"), tabName = "monitor_page",expandedName = "monitor",
                                        useShinyjs(),
                                        menuItem('Process', tabName = 'tabset_process_monitor',expandedName = "monitor_process", startExpanded = FALSE,
                                                 span(
                                                   radioButtons("radio_Process_monitor", label = paste0("Choose settings"),
                                                                choices = list("Default Selection" = 1, "Custom Selection" = 2), inline = FALSE,
                                                                selected = 1),
                                                   conditionalPanel("input.radio_Process_monitor == 1",
                                                                    selectInput("process_machine_monitor_default", label = paste0("Choose Machine"),
                                                                                process_machine_choices_monitor),
                                                                    selectInput("process_parameter_monitor_default", label = paste0("Select Parameter"),
                                                                                process_parameter_choices_monitor),
                                                                    disabled(textInput("no_of_readings", label = paste0("Number of Readings "), value = "30"))
                                                   ),
                                                   conditionalPanel("input.radio_Process_monitor == 2",
                                                                    selectInput("process_machine_monitor_manual", label = paste0("Choose Machine"),
                                                                                process_machine_choices_monitor),
                                                                    selectInput("process_parameter_monitor_manual", label = paste0("Select Parameter"),
                                                                                process_parameter_choices_monitor),
                                                                    selectInput("process_setup_monitor_manual", label = paste0("Select Setup"),
                                                                                process_setup_choices_monitor),
                                                                    # dateInput('start_date_input_process_monitor',
                                                                    #           label = 'Start Date ',
                                                                    #           value = Sys.Date(),
                                                                    #           format = "mm/dd/yyyy"
                                                                    # ),
                                                                    # timeInput("start_time_input_process_monitor", "Start Time"),
                                                                    # tags$label(id='control-label', 'Start Time'),
                                                                    # tags$div(class = "input-group bootstrap-timepicker timepicker",
                                                                    #          tags$input(id = "start_time_input_process_monitor",
                                                                    #                     type = "text",
                                                                    #                     class = "form-control input-small"),
                                                                    #          tags$span(class = "input-group-addon",
                                                                    #                    tags$i(class= "glyphicon glyphicon-time"))),
                                                                    # dateInput('end_date_input_process_monitor',
                                                                    #           label = 'End Date ',
                                                                    #           value = Sys.Date(),
                                                                    #           format = "mm/dd/yyyy"
                                                                    # ),
                                                                    # timeInput("end_time_input_process_monitor", "End Time")
                                                                    # tags$label(id='control-label', 'End Time'),
                                                                    # tags$div(class = "input-group bootstrap-timepicker timepicker",
                                                                    #          tags$input(id = "end_time_input_process_monitor",
                                                                    #                     type = "text",
                                                                    #                     class = "form-control input-small"),
                                                                    #          tags$span(class = "input-group-addon",
                                                                    #                    tags$i(class= "glyphicon glyphicon-time")))
                                                                    airDatepickerInput(
                                                                      inputId = "start_time_input_process_monitor",
                                                                      label = "Start date and time",
                                                                      timepicker = TRUE,
                                                                      timepickerOpts = timepickerOptions(timeFormat = 'hh:ii'),
                                                                      position = 'top left'
                                                                    ),
                                                                    airDatepickerInput(
                                                                      inputId = "end_time_input_process_monitor",
                                                                      label = "End date and time",
                                                                      timepicker = TRUE,
                                                                      timepickerOpts = timepickerOptions(timeFormat = 'hh:ii'),
                                                                      position = 'top left'
                                                                    )
                                                   ),
                                                   actionButton("monitor_process", "Monitor")
                                                   # conditionalPanel("input.monitor_process && output.monitor_process_click_reactive"
                                                   #                  selectInput("trend", "Trends:",
                                                   #                              c("All",
                                                   #                                "Control Chart" ,
                                                   #                                "Up_Down" ,
                                                   #                                "Shift"))
                                                   #                  )
                                                 )),
                                        menuItem('Product', tabName = 'tabset_product_monitor',expandedName = "monitor_product", startExpanded = FALSE,
                                                 span(
                                                   radioButtons("radio_Product_monitor", label = paste0("Choose settings"),
                                                                choices = list("Default" = 1, "Manual" = 2),inline = FALSE,
                                                                selected = 1),
                                                   conditionalPanel("input.radio_Product_monitor == 1",
                                                                    selectInput("product_job_monitor_default", label = paste0("Choose Job Type"),
                                                                                product_job_choices_monitor),
                                                                    selectInput("product_parameter_monitor_default", label = paste0("Select Parameter"),
                                                                                product_parameter_choices_monitor),
                                                                    selectInput("product_machine_monitor_default", label = paste0("Select Machine"),
                                                                                product_machine_choices_monitor),
                                                                    disabled(textInput("no_of_readings", label = paste0("Number of Readings : "), value = "32"))
                                                   ),
                                                   actionButton("monitor_product", "Monitor")
                                                 )),
                                        startExpanded = FALSE),
                                      menuItem("Average Charts", icon = icon("bar-chart", lib = "font-awesome"), tabName = "ac_page",expandedName = "ac",
                                               useShinyjs(),
                                               menuItem('Process', tabName = 'tabset_process_ac',expandedName = "ac_process", startExpanded = FALSE,
                                                        span(
                                                          selectInput("process_machine_ac", label = paste0("Choose Machine"),
                                                                      process_machine_choices_ac),
                                                          selectInput("process_parameter_ac", label = paste0("Choose Parameter"),
                                                                      process_parameter_choices_ac),
                                                          actionButton("ac_plot_process", "Plot")
                                                        )),
                                               menuItem('Product', tabName = 'tabset_process_ac',expandedName = "ac_process", startExpanded = FALSE,
                                                        span(
                                                          selectInput("product_job_ac", label = paste0("Choose Job Type"), 
                                                                      product_job_choices_ac),
                                                          selectInput("product_parameter_ac", label = paste0("Select parameter"),
                                                                      product_parameter_choices_ac),
                                                          selectInput("product_machine_ac", label = paste0("Choose Machine"),
                                                                      product_machine_choices_ac),
                                                          actionButton("ac_plot_process", "Plot")
                                                        )),
                                               startExpanded = FALSE
                                      )
                               
                             )
                             ,
                             div(id="imgs",
                                 tags$footer(
                                   #tags$li(class = "dropdown", tags$img(src = "MT1.png",
                                   #  id = "mt_icon"))
                                   tags$p(class = "dropdown",tags$img(id = "mt_icon",src = "m1.png"), style = "
                                      position:fixed;
                                      bottom:0;
                                      width:400px;
                                      height:90px;
                                      padding: 5px;
                                      z-index: 1000;")
                                 )
                                 
                                 )
                            
                             )
                           
                             ),
                           tags$script(src = "timepicker.js")
                           )
renderBodyFunction <- function(id){
  ns <- NS(id)
  useShinyjs()
  div(
      conditionalPanel("(output.plot_process_click_reactive || output.plot_product_click_reactive) && !output.hide_setup_page && output.plot_click_reactive",
                       
                       tabsetPanel(id='tab_body_setup',
                                   tabPanel(id='tab_sertup_plot',icon = icon("bar-chart-o"),'Plot',
                                            fluidRow(    
                                              gradientBox(title = p(id='histogram_id',"Histogram"), footer = "Histogram demonstrates density of a data. Frequancy of the values of data is calculated and binned up.",
                                                          withSpinner(plotlyOutput("histogram_plot", height = 300), type = getOption("spinner.type", default = 8)) ),
                                              gradientBox(title = span(id='nd_id',textOutput('nd_title')), footer = textOutput('nd_description'),
                                                          withSpinner(  plotlyOutput("normal_distribution_plot", height = 300), type = getOption("spinner.type", default = 8))),
                                              uiOutput(ns('histogram_modal')),
                                              uiOutput(ns('nd_modal'))
                                              
                                            ),
                                            fluidRow(
                                              fluidRow(
                                                box(
                                                  title = "Summary",
                                                  background = NULL,
                                                  width = 12,
                                                  valueBoxOutput("sigma_value"),
                                                  valueBoxOutput("plus_one_sigma"),
                                                  valueBoxOutput("plus_two_sigma"),
                                                  valueBoxOutput("pci_value"),
                                                  valueBoxOutput("minus_one_sigma"),
                                                  valueBoxOutput("minus_two_sigma"))
                                              )
                                            )),
                                   tabPanel(icon = icon("file-text-o"),'Data',
                                            fluidRow(
                                              gradientBox(title = 'Master data', htmlOutput('setup_data_constants'),width = 4),
                                              column(8,DT::dataTableOutput("setup_data"))
                                            )
                                   )
                       )
      )
      ,        
      
      conditionalPanel("input.monitor_process && !output.hide_monitor_page && output.monitor_process_click_reactive" ,
                       tabsetPanel(id='tab_body_monitor_process',
                                   tabPanel(icon = icon("bar-chart-o"),'Plot',
                                            fluidRow(
                                              # conditionalPanel("input.trend == 'All'",
                                                               gradientBox(id='control_chart',title = p(id='control_chart',"Control Chart"), withSpinner(plotlyOutput("run_chart_all"), type = getOption("spinner.type", default = 8)), width = 12),
                                                               fluidRow(
                                                                 gradientBox(title = p(id='up_down_trend_id',"Up - Down Trend"),withSpinner(plotlyOutput("up_down_trend_all"), type = getOption("spinner.type", default = 8))),
                                                                 gradientBox(title = p(id='shift_trend',"Shift"),withSpinner(plotlyOutput("shift_all"), type = getOption("spinner.type", default = 8)))
                                                               ),
                                              # ),
                                              uiOutput(ns('control_chart_ui')),
                                              uiOutput(ns('up_down_trend_ui')),
                                              uiOutput(ns('shift_trend_ui'))
                                              # conditionalPanel("input.trend == 'Control Chart'",
                                              #                  gradientBox(title = "Control Chart", withSpinner(plotlyOutput("run_chart"), type = getOption("spinner.type", default = 8)), width = 12)
                                              # ),
                                              # conditionalPanel("input.trend == 'Up_Down'",
                                              #                  gradientBox(title = "Up - Down Trend",withSpinner(plotlyOutput("up_down_trend"), type = getOption("spinner.type", default = 8)), width = 12)
                                              # ),
                                              # conditionalPanel("input.trend == 'Shift'",
                                              #                  gradientBox(title = "Shift",withSpinner(plotlyOutput("shift"), type = getOption("spinner.type", default = 8)), width = 12)
                                              # )
                                            )
                                   ),
                                   tabPanel(icon = icon("file-text-o"),'Data',
                                            fluidRow(
                                              gradientBox(title = 'Master data', htmlOutput('monitor_process_data_constants'),width = 4),
                                              column(8,DT::dataTableOutput("monitor_process_data"))
                                            )
                                   )
                       )
      ),
      conditionalPanel("input.monitor_process && !output.hide_monitor_page && output.monitor_product_click_reactive",
                       tabsetPanel(id='tab_body_monitor_product',
                                   tabPanel(icon = icon("bar-chart-o"), 'Plot',
                                            fluidRow(
                                              box(title = "R chart", status = "primary", solidHeader = TRUE, withSpinner(plotlyOutput("R_chart", height = 450), type = getOption("spinner.type", default = 8)), width = 900, height = 500),
                                              box(title = "X Bar chart", status = "primary", solidHeader = TRUE, withSpinner(plotlyOutput("X_chart", height = 450), type = getOption("spinner.type", default = 8)), width = 900, height = 500)
                                            )),
                                   tabPanel(icon = icon("file-text-o"),'Data',
                                            fluidRow(
                                              gradientBox(title = 'Master data', htmlOutput('monitor_product_data_constants'),width = 4),
                                              column(8,DT::dataTableOutput("monitor_product_data"))
                                            )
                                   )
                       )
      ),
      conditionalPanel("input.ac_plot_process && !output.hide_ac_page",
                       fluidRow(
                         gradientBox(title = "Average Charts", width = 12,
                                     icon = "fa fa-th", boxToolSize = "xs",  color = "aqua-active",
                                     column(width = 5, withSpinner(plotlyOutput("bar_chart_proc", height = 600, width = "auto"), type = getOption("spinner.type", default = 8)), collapsible = FALSE),
                                     column(width = 7, withSpinner(plotlyOutput("run_chart_ac", height = 600, width = "auto"), type = getOption("spinner.type", default = 8)), collapsible = FALSE)
                                     
                         )
                         
                         #box(title = "R chart", status = "primary", solidHeader = TRUE, plotlyOutput("bar_chart", height = 300), plotlyOutput("run_chart_ac", height = 300))
                       )
      ),
      
      conditionalPanel("input.ac_plot && input.ac_page && !output.hide_ac_page && input.tabset_ac == 'Product'",
                       fluidRow(
                         gradientBox(title = "Average Charts", width = 12,
                                     icon = "fa fa-th", boxToolSize = "xs",  color = "aqua-active",
                                     column(width = 5, withSpinner(plotlyOutput("bar_chart_prod", height = 600, width = "auto"), type = getOption("spinner.type", default = 8)), collapsible = FALSE)  ,
                                     column(width = 7, withSpinner(plotlyOutput("x_bar_chart_ac", height = 600, width = "auto"), type = getOption("spinner.type", default = 8)), collapsible = FALSE)
                                     
                         )
                         
                         #box(title = "R chart", status = "primary", solidHeader = TRUE, plotlyOutput("bar_chart", height = 300), plotlyOutput("run_chart_ac", height = 300))
                       )
      ))
}

body<- dashboardBody(
  div(
    tags$img(src = "gear_gif_edited.gif",
             id = "loading_body_page")
    
    # h3('Loading ...')
    # tags$img(src = "ND.png",
    #     id = "loading-spinner")
  ),
  hidden(
    div(
      id = "main_body_content",
  renderBodyFunction('body')))
)
ui<- dashboardPage(
  skin = "black", 
  header = header, sidebar = sidebar, body = body
)

server <- function(session, input, output) {
  
  load_data_body()
  load_data_sideBar()
  print(' 1111 ')
  # >>>>>>>>>>>>>>>>>>>>>>>> BUTTON CLICK DECLARATIONS >>>>>>>>>>>>>>>>>>>>>>>>> #
  
  setup_page_click <- reactiveValues(setup_page_click_event = FALSE)
  plot_click <- reactiveValues(plot_click_event = FALSE)
  plot_process_click <- reactiveValues(plot_process_click_event = FALSE)
  plot_product_click <- reactiveValues(plot_product_click_event = FALSE)
  save_setup_click <- reactiveValues(save_setup_click_event = FALSE)
  monitor_page_click <- reactiveValues(monitor_page_click_event = FALSE)
  monitor_process_click <- reactiveValues(monitor_process_click_event = FALSE)
  monitor_product_click <- reactiveValues(monitor_product_click_event = FALSE)
  hide_setup_page <- reactiveValues(hide_setup_page_event = FALSE)
  hide_monitor_page <- reactiveValues(hide_monitor_page_event = FALSE)
  setup <- reactiveValues(temp_setup = NULL)
  setup_frame <- reactiveValues(setup_frame_temp = NULL)
  ac_page_click <- reactiveValues(ac_page_click_event = FALSE)
  ac_plot_process_click <- reactiveValues(ac_plot_process_click_event = FALSE)
  ac_plot_product_click <- reactiveValues(ac_plot_product_click_event = FALSE)
  check_sigma <- reactiveValues(temp_sigma = NULL, mean=NULL, median= NULL, mode=NULL)
  temp_data <- reactiveValues(temp_data1 = NULL)
  monitor_data <- reactiveValues(monitor_data1 = NULL)
  ac_data <- reactiveValues(ac_data1 = NULL)
  grouping <- reactiveValues(reactive_groups = NULL)
  # <-------------------------------- SETUP BUTTON CODE --------------------------------> #
  observe({
    print(paste0('selection :::', input$selection))
    print(paste0('expanded selection :::', input$sidebarItemExpanded))
    # print(paste0('tab :::', input$tabset))
    
    if(sum(length(input$sidebarItemExpanded))!=0){
      if(input$sidebarItemExpanded == 'setup'){
        print(' 2222 ')
        save_setup_click$save_setup_click_event = FALSE
        setup_page_click$setup_page_click_event = TRUE
        monitor_page_click$monitor_page_click_event = FALSE
        ac_page_click$ac_page_click_event = FALSE
        output$hide_setup_page = eventReactive("",FALSE)
        output$hide_monitor_page = eventReactive("",TRUE)
        output$hide_ac_page = eventReactive("",TRUE)
        outputOptions(output, "hide_setup_page", suspendWhenHidden = FALSE)
        outputOptions(output, "hide_monitor_page", suspendWhenHidden = FALSE)
        outputOptions(output, "hide_ac_page", suspendWhenHidden = FALSE)
        plot_click$plot_click_event = FALSE
        plot_process_click$plot_process_click_event = FALSE
        plot_product_click$plot_product_click_event = FALSE
        output$plot_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_click_reactive", suspendWhenHidden = FALSE)

        monitor_process_click$monitor_process_click_event = FALSE
        output$monitor_process_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
        
        monitor_product_click$monitor_product_click_event = FALSE
        output$monitor_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_product_click_reactive", suspendWhenHidden = FALSE)
        # DATABASE CALL TO GET ALL PARAMETERS
        setup_frame <- loadData()
        # setup_frame <- read.csv('monitor.csv')
      }
      if(input$sidebarItemExpanded == 'setup_process'){
        setup$temp_setup$parameter_type <- 'Process'
        output$plot_process_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_process_click_reactive", suspendWhenHidden = FALSE)
        output$plot_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_product_click_reactive", suspendWhenHidden = FALSE)
      } 
      else if(input$sidebarItemExpanded == 'setup_product'){
        setup$temp_setup$parameter_type <- 'Product'
        output$plot_process_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_process_click_reactive", suspendWhenHidden = FALSE)
        output$plot_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_product_click_reactive", suspendWhenHidden = FALSE)
      }
      if(input$sidebarItemExpanded == 'monitor'){
        print(' 4444 ')
        plot_click$plot_click_event = FALSE
        setup_page_click$setup_page_click_event = FALSE
        monitor_page_click$monitor_page_click_event = TRUE
        ac_page_click$ac_page_click_event = FALSE
        output$plot_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_click_reactive", suspendWhenHidden = FALSE)
        plot_click$plot_click_event = FALSE
        plot_process_click$plot_process_click_event = FALSE
        plot_product_click$plot_product_click_event = FALSE
        
        output$hide_setup_page = eventReactive("",TRUE)
        output$hide_monitor_page = eventReactive("",FALSE)
        output$hide_ac_page = eventReactive("",TRUE)
        
        outputOptions(output, "hide_setup_page", suspendWhenHidden = FALSE)
        outputOptions(output, "hide_monitor_page", suspendWhenHidden = FALSE)
        outputOptions(output, "hide_ac_page", suspendWhenHidden = FALSE)
        
        monitor_process_click$monitor_process_click_event = FALSE
        output$monitor_process_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
        
        monitor_product_click$monitor_product_click_event = FALSE
        output$monitor_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_product_click_reactive", suspendWhenHidden = FALSE)
        
        # DATABASE CALL TO GET ALL PARAMETERS
        setup_frame$setup_frame_temp <- loadData()
        # setup_frame$setup_frame_temp <- read.csv('monitor.csv')
        print("monitor ::::")
        print(setup_frame$setup_frame_temp)
        
        setup_param_process <- process_param[process_param$machine_id == setup_frame$setup_frame_temp$machine_id, ]
        setup_param_product <- product_param[product_param$job_type_id == setup_frame$setup_frame_temp$job_id, ]
        
        process_machine_choices_monitor = setNames(as.character(setup_param_process$machine_id),setup_param_process$machine_name)
        product_job_choices_monitor = setNames(as.character(setup_param_product$job_type_id),setup_param_product$job_type_name)
        
      }
      if(input$sidebarItemExpanded == 'ac'){
        setup_page_click$setup_page_click_event = FALSE
        monitor_page_click$monitor_page_click_event = FALSE
        ac_page_click$ac_page_click_event = TRUE
        
        output$hide_setup_page = eventReactive("",TRUE)
        output$hide_monitor_page = eventReactive("",TRUE)
        output$hide_ac_page = eventReactive("",FALSE)
        
        outputOptions(output, "hide_setup_page", suspendWhenHidden = FALSE)
        outputOptions(output, "hide_monitor_page", suspendWhenHidden = FALSE)
        outputOptions(output, "hide_ac_page", suspendWhenHidden = FALSE)
        
        output$plot_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_click_reactive", suspendWhenHidden = FALSE)
        
        monitor_process_click$monitor_process_click_event = FALSE
        output$monitor_process_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
        
        monitor_product_click$monitor_product_click_event = FALSE
        output$monitor_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_product_click_reactive", suspendWhenHidden = FALSE)
        # DATABASE CALL TO GET ALL PARAMETERS
        setup_frame$setup_frame_temp <- loadData()
        # setup_frame$setup_frame_temp <- read.csv('monitor.csv')
        # process_machine_choices_ac = setNames(as.character(setup_param$machine_id),setup_param$machine_name)
        # 
        # product_job_choices_ac = setNames(as.character(product_param$job_type_id),product_param$job_type_name)
      }
    }
  })
  
  observeEvent(input$product_job,{
    temp_data <- product_param[product_param$job_type_id == input$product_job, ]
    product_parameter_choices = setNames(as.character(temp_data$parameter_id),temp_data$parameter_name)
    updateSelectInput(session, 'product_parameter', label = paste0("Select Parameter"),
                      product_parameter_choices)
  })
  observeEvent(input$product_parameter,{
    temp_data <- product_param[product_param$job_type_id == input$product_job, ]
    temp_data <- temp_data[temp_data$parameter_id == input$product_parameter, ]
    product_machine_choices = setNames(as.character(temp_data$machine_id),temp_data$machine_name)
    updateSelectInput(session, 'product_machine', label = paste0("Select Machine"),
                      product_machine_choices)
  })
  observeEvent(input$process_machine,{
    temp_data <- process_param[process_param$machine_id == input$process_machine, ]
    process_parameter_choices = setNames(as.character(temp_data$parameter_id),temp_data$parameter_name)
    updateSelectInput(session, 'process_parameter', label = paste0("Choose Parameter"),
                      process_parameter_choices)
  })
  
  # observeEvent(input$tabset,{
  #   output$plot_click_reactive = eventReactive("",FALSE)
  #   outputOptions(output, "plot_click_reactive", suspendWhenHidden = FALSE)
  # })
  # <------------------------- SETUP :: PLOT BUTTON CODE -------------------------> #
  observeEvent(input$plot_process,{
    print(' 3333 ')
    print('plot_click')
    setup_output$parameter_type = 'Process'
    plot_click$plot_click_event = TRUE
    plot_process_click$plot_process_click_event = TRUE
    plot_product_click$plot_product_click_event = FALSE
    setup_page_click$setup_page_click_event = TRUE
    monitor_page_click$monitor_page_click_event = FALSE
    output$plot_process_click_reactive = eventReactive("",TRUE)
    outputOptions(output, "plot_process_click_reactive", suspendWhenHidden = FALSE)
    output$plot_product_click_reactive = eventReactive("",FALSE)
    outputOptions(output, "plot_product_click_reactive", suspendWhenHidden = FALSE)
    output$plot_click_reactive = eventReactive("",TRUE)
    outputOptions(output, "plot_click_reactive", suspendWhenHidden = FALSE)
    
    # PROCESSING USER INPUTS
    tryCatch({
      setup_output$machine_id <- input$process_machine
      setup_output$parameter_id <- input$process_parameter
      date <- input$date_input_process
      print(paste('input time ::',input$time_input_process))
      # time <- strptime(input$time_input_process,format="%m/%d/%Y %I:%M:%S %p")
      # time <- strptime(input$time_input_process,format="%I:%M %p")
      # time <- strftime(time,"%T",format="%H:%M:%S")
      # dateAndTime <- paste(date, time, sep = " ")
      dateAndTime <- input$time_input_process
      setup_output$start_time <- dateAndTime
      print(paste('dateAndTime', dateAndTime))
      
      # DYNAMIC DATA FRAME CREATION : data frame is filtered based on user inputs and 60 records are collected
      temp_data_inprocess <- process_param[process_param$machine_id == input$process_machine, ]
      temp_data_inprocess <- temp_data_inprocess[temp_data_inprocess$parameter_id == input$process_parameter, ]
      temp_data_inprocess <- temp_data_inprocess[which(temp_data_inprocess$local_date_time >= dateAndTime), ]
      temp_data$temp_data1 <- head(temp_data_inprocess, 60)
      print('temp_data')
      print(temp_data$temp_data1)
      print(paste('nrow temp_data :: ', nrow(temp_data$temp_data1)))
      
      # CALCULATIONS: calculations for mean, sigma, UCL, LCL and PCI
      
      #$ MEAN & SIGMA $#
      setup_output$sigma <- sd(temp_data$temp_data1$parameter_value)
      # print(paste('sigma', setup_output$sigma))
      setup_output$mean <- mean(temp_data$temp_data1$parameter_value)
      # print(paste('mean', setup_output$mean))
      
      #$ 3-SIGMA $#
      setup_output$plus_one_sigma <- setup_output$mean + setup_output$sigma
      setup_output$minus_one_sigma <- setup_output$mean - setup_output$sigma
      setup_output$plus_two_sigma <- setup_output$mean + 2 * setup_output$sigma
      setup_output$minus_two_sigma <- setup_output$mean - 2 * setup_output$sigma
      setup_output$plus_three_sigma <- setup_output$mean + 3 * setup_output$sigma
      setup_output$minus_three_sigma <- setup_output$mean - 3 * setup_output$sigma
      
      #$ TOLERANCE :: USL & LSL $#
      setup_output$usl_value <- unique(temp_data$temp_data1$usl_value)
      setup_output$lsl_value <- unique(temp_data$temp_data1$lsl_value)
      
      #$ Central Limit $#
      setup_output$central_limit_value <- unique(temp_data$temp_data1$central_limit_value)
      
      check_sigma$mean = mean(temp_data$temp_data1$parameter_value)
      check_sigma$median = median(temp_data$temp_data1$parameter_value)
      check_sigma$mode = getmode(temp_data$temp_data1$parameter_value)
      # check_sigma$temp_sigma <- c()
      check_sigma$temp_sigma = NULL
      for (value in temp_data$temp_data1$parameter_value) {
        if(value >= setup_output$minus_one_sigma && value <= setup_output$plus_one_sigma){
          check_sigma$temp_sigma <- c(check_sigma$temp_sigma, value)
        }
      }
      print(paste('check_sigma ::::::::', sum(length(check_sigma$temp_sigma))))
      sigma_percent <- (sum(length(check_sigma$temp_sigma))/60)*100
      if(sigma_percent >= 68 && check_sigma$mean == check_sigma$median){
        #$ PCI :: Process Capability Index $#
        setup_output$pci_value <- (setup_output$usl_value - setup_output$lsl_value) / (6 * setup_output$sigma)
        
        #$ UCL & LCL $#
        setup_output$ucl_value_r <- setup_output$mean + 3 * setup_output$sigma
        setup_output$lcl_value_r <- setup_output$mean - 3 * setup_output$sigma
      }        
      
      print('setup_output :: ')
      print(setup_output)
    },
    error = function(e){
      if(plot_process_click$plot_process_click_event == FALSE || setup_page_click$setup_page_click_event == FALSE) return()
      shinyalert(
        title = "Error",text = "No records found for this time",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
      )
      plot_process_click$plot_process_click_event = FALSE
      plot_click$plot_click_event = FALSE
      output$plot_process_click_reactive = eventReactive("",FALSE)
      outputOptions(output, "plot_process_click_reactive", suspendWhenHidden = FALSE)
    })
    
    # setup_output$parameter_type <- input$tabset
    setup$temp_setup <- setup_output
  })
  
  observeEvent(input$plot_product,{
    print(' 3333 ')
    print('plot_click')
    setup_output$parameter_type = 'Product'
    plot_click$plot_click_event = TRUE
    plot_process_click$plot_process_click_event = FALSE
    plot_product_click$plot_product_click_event = TRUE
    setup_page_click$setup_page_click_event = TRUE
    monitor_page_click$monitor_page_click_event = FALSE
    # save_setup_click$save_setup_click_event = FALSE
    output$plot_process_click_reactive = eventReactive("",FALSE)
    outputOptions(output, "plot_process_click_reactive", suspendWhenHidden = FALSE)
    output$plot_product_click_reactive = eventReactive("",TRUE)
    outputOptions(output, "plot_product_click_reactive", suspendWhenHidden = FALSE)
    
    # setup_output$parameter_type <- input$tabset
    
    
    # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON CODE :: Product ++++++++++++++++++++++++++> #
      tryCatch({
        # PROCESSING USER INPUTS
        setup_output$job_id <- input$product_job
        setup_output$parameter_id <- input$product_parameter
        setup_output$machine_id <- input$product_machine
        # date <- format(strptime(input$date_input_product, format = "%Y-%m-%d"), "%m/%d/%Y")
        # if(substring(date, 1, 1) == '0'){
        #   date <- substring(date, 2)
        # }
        # time <- strftime(input$time_input_product, "%T", format = "%H:%M")
        # if(substring(time, 1, 1) == '0'){
        #   time <- substring(time, 2)
        # }        date <- input$date_input_process
        date <- input$date_input_product
        print(paste('date', date))
        time <- strftime(input$time_input_product, "%T", format = "%H:%M:%S")
        print(paste('time', time))
        dateAndTime <- paste(date, time, sep = " ")

        setup_output$start_time <- dateAndTime

        # DYNAMIC DATA FRAME CREATION : data frame is filtered based on user inputs and 60 records are collected
        temp_data_inprocess <- product_param[product_param$job_type_id == input$product_job, ]
        temp_data_inprocess <- temp_data_inprocess[temp_data_inprocess$parameter_id == input$product_parameter, ]
        temp_data_inprocess <- temp_data_inprocess[which(temp_data_inprocess$local_date_time >= dateAndTime), ]
        temp_data$temp_data1 <- head(temp_data_inprocess, 60)
        print('temp_data :: ')
        print(temp_data$temp_data1)
        print(paste('nrow temp_data :: ', nrow(temp_data$temp_data1)))

        # GROUP CREATION FOR PRODUCT SPC: 60 readings are grouped in total 15 groups with group size of 4 (4 readings/group)
        temp_data$temp_data1$sample <- rep(1:15, each=4)
        parameter_value <- qcc.groups(temp_data$temp_data1$parameter_value, temp_data$temp_data1$sample)
        grouping <- data.frame(parameter_value)
        grouping$Range <- apply(grouping,1,max) - apply(grouping,1,min)
        grouping$Mean <- (grouping$X1+grouping$X2+grouping$X3+grouping$X4)/4

        # CALCULATIONS: calculations for mean, sigma, R-bar, X-bar, UCL, LCL and PCI

        #$ R-bar & X-bar $#
        setup_output$r_bar <- sum(grouping$Range)/k
        setup_output$x_bar <- sum(grouping$Mean)/k

        #$ MEAN & SIGMA $#
        setup_output$sigma <- setup_output$r_bar/d2
        setup_output$mean <- mean(temp_data$temp_data1$parameter_value)

        #$ 3-SIGMA $#
        setup_output$plus_one_sigma <- setup_output$mean + setup_output$sigma
        setup_output$minus_one_sigma <- setup_output$mean - setup_output$sigma
        setup_output$plus_two_sigma <- setup_output$mean + 2 * setup_output$sigma
        setup_output$minus_two_sigma <- setup_output$mean - 2 * setup_output$sigma
        setup_output$plus_three_sigma <- setup_output$mean + 3 * setup_output$sigma
        setup_output$minus_three_sigma <- setup_output$mean - 3 * setup_output$sigma

        #$ TOLERANCE :: USL & LSL $#
        setup_output$usl_value <- unique(temp_data$temp_data1$usl_value)
        setup_output$lsl_value <- unique(temp_data$temp_data1$lsl_value)

        #$ Central Limit $#
        setup_output$central_limit_value <- unique(temp_data$temp_data1$central_limit_value)

        #$ PCI :: Process Capability Index $#
        setup_output$pci_value <- (setup_output$usl_value - setup_output$lsl_value) / (6 * setup_output$sigma)

        #$ UCL & LCL $#
        if(setup_output$pci_value >= 1){
          setup_output$ucl_value_r <- D4 * setup_output$r_bar
          setup_output$lcl_value_r <- D3 * setup_output$r_bar
          setup_output$ucl_value_x <- setup_output$x_bar + (A2 * setup_output$r_bar)
          setup_output$lcl_value_x <- setup_output$x_bar - (A2 * setup_output$r_bar)
        }
        print('setup_output :: ')
        print(setup_output)
      },
      error = function(e){
        if(plot_product_click$plot_product_click_event == FALSE || setup_page_click$setup_page_click_event == FALSE) return()
        shinyalert(
          title = "Error",text = "No records found for this time",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
        )
        plot_product_click$plot_product_click_event = FALSE
        plot_click$plot_click_event = FALSE
        output$plot_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "plot_product_click_reactive", suspendWhenHidden = FALSE)
      }
      )
    setup$temp_setup <- setup_output
  })
  
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT ++++++++++++++++++++++++++> #
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT :: HISTOGRAM ++++++++++++++++++++++++++> #
  output$histogram_plot <- renderPlotly({
    if(plot_click$plot_click_event == FALSE) return()
    chart <- histogram(temp_data$temp_data1)
    chart
  })
  # 
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT :: NORMAL DISTRIBUTION ++++++++++++++++++++++++++> #
  output$normal_distribution_plot <- renderPlotly({
    if(plot_click$plot_click_event == FALSE) return()
    FD <- normal_distribution(temp_data$temp_data1, setup$temp_setup)
    # ggplotly(FD, width = 490, height = 300)
    FD
  })
  
  output$Histogram_info <- renderText({
    if(plot_click$plot_click_event == FALSE) return()
    "The histogram is plotted on continuous 60 readings collected from the start time that you entered"
  })
  
  output$ND_info <- renderText({
    if(plot_click$plot_click_event == FALSE) return()
    "All the 60 readings are normally distributed and 68% of data fits into 1-Sigma"
  })
  
  output$setup_data_constants <- renderUI({
    machine_name <- paste0('Machine Name : ', unique(temp_data$temp_data1$machine_name))
    parameter_name <- paste0('Parameter Name : ', unique(temp_data$temp_data1$parameter_name))
    usl_value <- paste0('USL : ', unique(temp_data$temp_data1$usl_value))
    lsl_value <- paste0('LSL : ', unique(temp_data$temp_data1$lsl_value))
    central_limit_value <- paste0('Central Limit : ', unique(temp_data$temp_data1$central_limit_value))
    if(plot_click$plot_click_event == FALSE) return()
    HTML(paste(machine_name, parameter_name, usl_value, lsl_value, central_limit_value, sep="<br/>"))
  })
  
  output$setup_data <- DT::renderDataTable({
    if(plot_click$plot_click_event == FALSE) return()
    temp_data$temp_data1$local_date_time <- as.character(temp_data$temp_data1$local_date_time)
    if(nrow(temp_data$temp_data1) > 0){
      row.names(temp_data$temp_data1) <- seq(1:nrow(temp_data$temp_data1))
    }
    DT::datatable(temp_data$temp_data1[ ,c("local_date_time","parameter_value")], options = list(
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    ), class = 'cell-border stripe')
  })
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT :: SIGMA value BOX ++++++++++++++++++++++++++> #
  output$sigma_value <- renderValueBox({
    if(plot_click$plot_click_event == FALSE) return()
    x <- "sigma.png"
    valueBox( 
      paste0(round(setup$temp_setup$sigma, 2)), "Sigma",
      icon=icon(list(src=x, width="50px", height = "20px"), lib="local"),
      color="aqua", width=NULL)
    # 
  })
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT :: PCI value BOX ++++++++++++++++++++++++++> #
  output$pci_value <- renderValueBox({
    if(plot_click$plot_click_event == FALSE) return()
    x <- "pci.png"
    valueBox(
      paste0(round(setup$temp_setup$pci_value, 2)), "Process Capability Index",
      icon=icon(list(src=x, width="50px", height = "20px"), lib="local"),
      color="green", width=NULL)
  })
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT ::plus one sigma value BOX ++++++++++++++++++++++++++> #
  output$plus_one_sigma <- renderValueBox({
    if(plot_click$plot_click_event == FALSE) return()
    x <- "plus1sigma.png"
    valueBox(
      paste0(round(setup$temp_setup$plus_one_sigma, 2)), "Plus 1 Sigma",
      icon=icon(list(src=x, width="50px", height = "20px"), lib="local"),
      color="orange", width=NULL)
  })
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT ::minus one sigma value BOX ++++++++++++++++++++++++++> #
  output$minus_one_sigma <- renderValueBox({
    if(plot_click$plot_click_event == FALSE) return()
    x <- "minus1sigma.png"
    valueBox(
      paste0(round(setup$temp_setup$minus_one_sigma, 2)), "Minus 1 Sigma",
      icon=icon(list(src=x, width="50px", height = "20px"), lib="local"),
      color="orange", width=NULL)
  })
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT ::plus two sigma value BOX ++++++++++++++++++++++++++> #
  output$plus_two_sigma <- renderValueBox({
    if(plot_click$plot_click_event == FALSE) return()
    x <- "plus2sigma.png"
    valueBox(
      paste0(round(setup$temp_setup$plus_two_sigma, 2)), "Plus 2 Sigma",
      icon=icon(list(src=x, width="50px", height = "20px"), lib="local"),
      color="red", width=NULL)
  })
  
  # <++++++++++++++++++++++++++ SETUP :: PLOT BUTTON OUTPUT ::minus two sigma value BOX ++++++++++++++++++++++++++> #
  output$minus_two_sigma <- renderValueBox({
    if(plot_click$plot_click_event == FALSE) return()
    x <- "minus2sigma.png"
    valueBox(
      paste0(round(setup$temp_setup$minus_two_sigma, 2)), "Minus 2 Sigma",
      icon=icon(list(src=x, width="50px", height = "20px"), lib="local"),
      color="red", width=NULL)
  })
  
  output$nd_description <- renderText({
    if(plot_click$plot_click_event == FALSE) return()
    percent_data_fit <- (length(check_sigma$temp_sigma)/60)*100
    if(check_sigma$mean < check_sigma$median){
      text <- paste0('Mean = ',check_sigma$mean, ' Median = ', check_sigma$median, '. It is left-skewed.')
    }
    else if(check_sigma$mean > check_sigma$median){
      text <- paste0('Mean = ',check_sigma$mean, ' Median = ', check_sigma$median, '. It is right-skewed.')
    }
    else if(check_sigma$mean == check_sigma$median){
      text <- paste0('Mean = Median = Mode = ', check_sigma$mean, '. It is symmetric.')
    }
    if(is.na(setup$temp_setup$pci_value)){
      text <- paste0(text,' ', paste0(round(percent_data_fit, 2), '% of data fits into one sigma. The standard fit is 68%.'))
    }
    else{
      text <- paste0(text, ' ', paste0(round(percent_data_fit, 2), '% of data fits into one sigma.'))
    }
    text
  })
  output$nd_title <- renderText({
    if(plot_click$plot_click_event == FALSE) return()
    if(check_sigma$mean < check_sigma$median){
      text <- paste0('Left - Skewed')
    }
    else if(check_sigma$mean > check_sigma$median){
      text <- paste0('Right - Skewed')
    }
    else if(check_sigma$mean == check_sigma$median){
      text <- paste0('Normal Distribution')
    }
    text
  })
  
  onevent('mouseenter', 'histogram_id', showModal(modalDialog(
    title = "Histogram",
    output$histogram_modal <- renderUI({
      if(plot_click$plot_click_event == FALSE) return()
      output$histogram_plot_modal <- renderPlotly({
        if(plot_click$plot_click_event == FALSE) return()
        chart <- histogram(temp_data$temp_data1)
        chart
      })
      withSpinner(plotlyOutput(ns("histogram_plot_modal"), height = 800), type = getOption("spinner.type", default = 8))
    }),
    size = c("l"),
    easyClose = TRUE
  )))
  
  onevent('mouseenter', 'nd_id', showModal(modalDialog(
    output$nd_title1 <- renderText({
      if(plot_click$plot_click_event == FALSE) return()
      if(check_sigma$mean < check_sigma$median){
        text <- paste0('Left - Skewed')
      }
      else if(check_sigma$mean > check_sigma$median){
        text <- paste0('Right - Skewed')
      }
      else if(check_sigma$mean == check_sigma$median){
        text <- paste0('Normal Distribution')
      }
      text
    }),
    title = textOutput(ns('nd_title1')),
    output$nd_modal <- renderUI({
      if(plot_click$plot_click_event == FALSE) return()
      output$normal_distribution_plot_modal <- renderPlotly({
        if(plot_click$plot_click_event == FALSE) return()
        FD <- normal_distribution(temp_data$temp_data1, setup$temp_setup)
        FD
      })
      withSpinner(plotlyOutput(ns("normal_distribution_plot_modal"), height = 800), type = getOption("spinner.type", default = 8))
    }),
    size = c("l"),
    easyClose = TRUE
  )))
  
  # <------------------------- SETUP :: SAVE_SETUP BUTTON CODE -------------------------> #
  observeEvent(input$save_setup,{
    print('setup$temp_setup :: ')
    print(setup$temp_setup$parameter_type)
    if(input$setup_name=='' || is.na(input$setup_name)){
      
    }
    else{
      print(paste0('save setup click :: ',save_setup_click$save_setup_click_event))
      tryCatch({
        save_setup_click$save_setup_click_event = TRUE
        setup$temp_setup$setup_name <- input$setup_name
        setup$temp_setup$setup_description <- input$setup_description
        if(setup$temp_setup$setup_name == '' || is.na(setup$temp_setup$setup_name)){
          # disabled(save_setup)
          shinyalert(
            title = "Error",text = "Enter Setup name",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,animation = TRUE
          )
        } else {
          if(save_setup_click$save_setup_click_event == FALSE) return()
          # enable(save_setup)
          if(setup$temp_setup$parameter_type == 'Product'){
            if(!is.na(setup$temp_setup$pci_value) && setup$temp_setup$pci_value >= 1){
              save_response <- saveData(setup$temp_setup)
              if(save_response == TRUE && save_setup_click$save_setup_click_event == TRUE){
                if(save_setup_click$save_setup_click_event == FALSE) return()
                print(paste0('save setup click :: ',save_setup_click$save_setup_click_event))
                print(paste0('save_response :: ',save_response))
                shinyalert(
                  title = "Done",text = "Setup saved successfully!",closeOnEsc = TRUE,closeOnClickOutside = TRUE,html = FALSE,type = "success",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
                )
                save_response <- FALSE
                save_setup_click$save_setup_click_event = FALSE
                updateTextInput(session,"setup_name", value="")
                updateTextInput(session,"setup_description", value="")
              }
              else{
                shinyalert(
                  title = "Error",text = "Save Failed",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
                )
              }
            }
            else{
              shinyalert(
                title = "PCI", text = "PCI is less than one",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",animation = TRUE
              )
              save_response <- FALSE
              save_setup_click$save_setup_click_event = FALSE
              updateTextInput(session,"setup_name", value="")
              updateTextInput(session,"setup_description", value="")
            }
          }
          else if(setup$temp_setup$parameter_type == 'Process'){
            if(!is.na(setup$temp_setup$pci_value)){
              save_response <- saveData(setup$temp_setup)
              if(save_response == TRUE && save_setup_click$save_setup_click_event == TRUE){
                if(save_setup_click$save_setup_click_event == FALSE) return()
                print(paste0('save setup click :: ',save_setup_click$save_setup_click_event))
                print(paste0('save_response :: ',save_response))
                shinyalert(
                  title = "Done",text = "Setup saved successfully!",closeOnEsc = TRUE,closeOnClickOutside = TRUE,html = FALSE,type = "success",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
                )
                save_response <- FALSE
                save_setup_click$save_setup_click_event = FALSE
                updateTextInput(session,"setup_name", value="")
                updateTextInput(session,"setup_description", value="")
              }
              else{
                shinyalert(
                  title = "Error",text = "Save Failed",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
                )
              }
            }
            else{
              shinyalert(
                title = "Error",text = "Data does not fall under normal distribution. Therefore, unfit for furthur calculations.",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
              )
              save_response <- FALSE
              save_setup_click$save_setup_click_event = FALSE
              updateTextInput(session,"setup_name", value="")
              updateTextInput(session,"setup_description", value="")
            }
          }
        }
        print('Setup DATABASE :: ')
        # print(loadData())
        
      }
      )
    }
  })
  
  # <-------------------------------- MONITOR BUTTON CODE --------------------------------> #
  
  # <-------------------------------- MONITOR UPDATE INPUT --------------------------------> #
  observeEvent(input$process_machine_monitor_default,{
    temp_data1 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$process_machine_monitor_default, ]
    temp_data2 <- process_param[process_param$parameter_id == temp_data1$parameter_id, ]
    process_parameter_choices_monitor = setNames(as.character(temp_data2$parameter_id),temp_data2$parameter_name)
    updateSelectInput(session, 'process_parameter_monitor_default', label = paste0("Select Parameter"),
                      process_parameter_choices_monitor)
  })
  observeEvent(input$process_machine_monitor_manual,{
    temp_data1 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$process_machine_monitor_manual, ]
    temp_data2 <- process_param[process_param$parameter_id == temp_data1$parameter_id, ]
    process_parameter_choices_monitor = setNames(as.character(temp_data2$parameter_id),temp_data2$parameter_name)
    updateSelectInput(session, 'process_parameter_monitor_manual', label = paste0("Select Parameter"),
                      process_parameter_choices_monitor)
  })
  observeEvent(input$product_job_monitor_default,{
    temp_data1 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$job_id == input$product_job_monitor_default, ]
    temp_data2 <- product_param[product_param$parameter_id == temp_data1$parameter_id, ]
    product_parameter_choices_monitor = setNames(as.character(temp_data2$parameter_id),temp_data2$parameter_name)
    updateSelectInput(session, 'product_parameter_monitor_default', label = paste0("Select Parameter"),
                      product_parameter_choices_monitor)
  })
  observeEvent(input$product_parameter_monitor_default,{
    temp_data1 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$job_id == input$product_job_monitor_default, ]
    temp_data2 <- product_param[product_param$parameter_id == input$product_parameter_monitor_default, ]
    product_machine_choices_monitor = setNames(as.character(temp_data2$machine_id),temp_data2$machine_name)
    updateSelectInput(session, 'product_machine_monitor_default', label = paste0("Select Machine"),
                      product_machine_choices_monitor)
  })
  observeEvent(input$process_parameter_monitor_manual,{
    temp_data1 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$process_machine_monitor_manual, ]
    temp_data2 <- temp_data1[temp_data1$parameter_id == input$process_parameter_monitor_manual, ]
    process_setup_choices_monitor = setNames(as.character(temp_data2$setup_id),temp_data2$setup_name)
    process_setup_choices_monitor <- sort(process_setup_choices_monitor, decreasing = TRUE)
    updateSelectInput(session, 'process_setup_monitor_manual', label = paste0("Select Setup"),
                      process_setup_choices_monitor)
  })
  
  # <------------------------- MONITOR :: START MONITORING BUTTON CODE : PROCESS -------------------------> #
  observeEvent(input$monitor_process,{
    print(' 5555 ')
    monitor_process_click$monitor_process_click_event = TRUE
    output$monitor_process_click_reactive = eventReactive("",TRUE)
    outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
    
    monitor_product_click$monitor_product_click_event = FALSE
    output$monitor_product_click_reactive = eventReactive("",FALSE)
    outputOptions(output, "monitor_product_click_reactive", suspendWhenHidden = FALSE)
      if(input$radio_Process_monitor == 1){
        
        tryCatch({
          print('inside default')
          
          # FILTER DATA FRAME AND KEEP ONLY Process PARAMETERS
          setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$process_machine_monitor_default, ]
          setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$parameter_id == input$process_parameter_monitor_default, ]
          # setup_frame <- setup_frame[setup_frame$parameter_type == 'Process', ]
          setup_frame$setup_frame_temp <- tail(setup_frame$setup_frame_temp, 1)
          print('Setup Frame Default:: ')
          print(setup_frame$setup_frame_temp)
          
          # DYNAMIC DATA FRAME CREATION : data frame is filtered based on last setup in database and last 30 readings are collected
          monitor_data_inprocess <- process_param[process_param$machine_id == setup_frame$setup_frame_temp$machine_id, ]
          monitor_data_inprocess <- monitor_data_inprocess[monitor_data_inprocess$parameter_id == setup_frame$setup_frame_temp$parameter_id, ]
          monitor_data$monitor_data1 <- tail(monitor_data_inprocess, 30) 
          print('Monitor Data default nrows:: ')
          print(nrow(monitor_data$monitor_data1))
        },error = function(e){
          shinyalert(
            title = "Error",text = "No records found",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
          )
          monitor_process_click$monitor_process_click_event = FALSE
          output$monitor_process_click_reactive = eventReactive("",FALSE)
          outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
        })
        
      }
      
      if(input$radio_Process_monitor == 2){
        
        tryCatch({
          print('inside manual')
          
          # FILTER DATA FRAME AND KEEP ONLY Process PARAMETERS
          setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$setup_id == input$process_setup_monitor_manual, ]
          print('Setup Frame Manual:: ')
          print(setup_frame$setup_frame_temp)
          
          # DYNAMIC DATA FRAME CREATION : data frame is filtered based on last setup in database and readings between start and end time are collected
          monitor_data_inprocess <- process_param[process_param$machine_id == setup_frame$setup_frame_temp$machine_id, ]
          monitor_data_inprocess <- monitor_data_inprocess[monitor_data_inprocess$parameter_id == setup_frame$setup_frame_temp$parameter_id, ]
          # start_date <- input$start_date_input_process_monitor
          # start_time <- strftime(input$start_time_input_process_monitor, "%T", format = "%H:%M:%S")
          # start_time <- strptime(input$start_time_input_process_monitor,format="%I:%M %p")
          # start_time <- strftime(start_time,"%T",format="%H:%M:%S")
          # start_dateAndTime <- paste(start_date, start_time, sep = " ")
          start_dateAndTime <- input$start_time_input_process_monitor
          print(paste('start_dateAndTime', start_dateAndTime))
          
          # end_date <- input$end_date_input_process_monitor
          # end_time <- strftime(input$end_time_input_process_monitor, "%T", format = "%H:%M:%S")
          # end_time <- strptime(input$end_time_input_process_monitor,format="%I:%M %p")
          # end_time <- strftime(end_time,"%T",format="%H:%M:%S")
          # end_dateAndTime <- paste(end_date, end_time, sep = " ")
          end_dateAndTime <- input$end_time_input_process_monitor
          print(paste('end_dateAndTime', end_dateAndTime))
          if(end_dateAndTime > start_dateAndTime){
            # monitor_data$monitor_data1 <- monitor_data_inprocess[c(which(monitor_data_inprocess$local_date_time >= start_dateAndTime):which(monitor_data_inprocess$local_date_time >= end_dateAndTime)), ]
            monitor_data$monitor_data1 <- monitor_data_inprocess[which(monitor_data_inprocess$local_date_time >= start_dateAndTime), ]
            monitor_data$monitor_data1 <- monitor_data$monitor_data1[which(monitor_data$monitor_data1$local_date_time <= end_dateAndTime), ]
          }
          
          else{
            shinyalert(
              title = "Error",text = "Start date and time should be less than end date and time",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
            )
            monitor_process_click$monitor_process_click_event = FALSE
            output$monitor_process_click_reactive = eventReactive("",FALSE)
            outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
          }
          print('Monitor Data manual nrows:: ')
          print(nrow(monitor_data$monitor_data1))
          
        },error = function(e){
          shinyalert(
            title = "Error",text = "No records found",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
          )
          monitor_process_click$monitor_process_click_event = FALSE
          output$monitor_process_click_reactive = eventReactive("",FALSE)
          outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
        })
      }
  })
  
  # <------------------------- MONITOR :: START MONITORING BUTTON CODE : PRODUCT -------------------------> #
  observeEvent(input$monitor_product,{
    monitor_process_click$monitor_process_click_event = FALSE
    output$monitor_process_click_reactive = eventReactive("",FALSE)
    outputOptions(output, "monitor_process_click_reactive", suspendWhenHidden = FALSE)
    
    monitor_product_click$monitor_product_click_event = TRUE
    output$monitor_product_click_reactive = eventReactive("",TRUE)
    outputOptions(output, "monitor_product_click_reactive", suspendWhenHidden = FALSE)
    if(input$radio_Product_monitor == 1){
      print('inside default')
      tryCatch({
        # FILTER DATA FRAME AND KEEP ONLY Process PARAMETERS
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$job_id == input$product_job_monitor_default, ]
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$parameter_id == input$product_parameter_monitor_default, ]
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$product_machine_monitor_default, ]
        setup_frame$setup_frame_temp <- tail(setup_frame$setup_frame_temp, 1)
        print('Setup Frame Default:: ')
        print(setup_frame$setup_frame_temp)
        
        # DYNAMIC DATA FRAME CREATION : data frame is filtered based on last setup in database and last 30 readings are collected
        monitor_data_inprocess <- product_param[product_param$job_type_id == setup_frame$setup_frame_temp$job_id, ]
        monitor_data_inprocess <- monitor_data_inprocess[monitor_data_inprocess$parameter_id == setup_frame$setup_frame_temp$parameter_id, ]
        monitor_data_inprocess <- monitor_data_inprocess[monitor_data_inprocess$machine_id == setup_frame$setup_frame_temp$machine_id, ]
        monitor_data$monitor_data1 <- tail(monitor_data_inprocess, 32) 
        print('Monitor Data :: ')
        print(monitor_data$monitor_data1)
        
        # GROUP CREATION FOR PRODUCT SPC: 60 readings are grouped in total 15 groups with group size of 4 (4 readings/group)
        monitor_data$monitor_data1$sample <- rep(1:8, each=4)
        parameter_value <- qcc.groups(monitor_data$monitor_data1$parameter_value, monitor_data$monitor_data1$sample)
        grouping$reactive_groups <- data.frame(parameter_value)
        grouping$reactive_groups$Range <- apply(grouping$reactive_groups,1,max) - apply(grouping$reactive_groups,1,min)
        grouping$reactive_groups$Mean <- (grouping$reactive_groups$X1+grouping$reactive_groups$X2+grouping$reactive_groups$X3+grouping$reactive_groups$X4)/4
        grouping$reactive_groups$groups <- rep(1:8, each=1)
        print('groupings :: ')
        print(grouping$reactive_groups)
      },error = function(e){
        monitor_product_click$monitor_product_click_event = FALSE
        output$monitor_product_click_reactive = eventReactive("",FALSE)
        outputOptions(output, "monitor_product_click_reactive", suspendWhenHidden = FALSE)
      })
    }
    
    if(input$radio_Product_monitor == 2){
      print('inside manual')
    }
  })
  
  # <++++++++++++++++++++++++++ MONITOR :: START MONITORING BUTTON OUTPUT : PROCESS MANUAL ++++++++++++++++++++++++++> #
  # library(quantmod)
  
  output$run_chart_all <- renderPlotly({
    if(monitor_process_click$monitor_process_click_event == FALSE) return()
    chart <- run_chart_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp)
    chart
  })
  
  output$up_down_trend_all <- renderPlotly({
    if(monitor_process_click$monitor_process_click_event == FALSE) return()
    chart1 <- up_down_trend_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp)
    chart1
  })
  
  output$shift_all <- renderPlotly({
    if(monitor_process_click$monitor_process_click_event == FALSE) return()
    chart2 <- shift_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp)
    chart2
  })
  
  output$monitor_process_data_constants <- renderUI({
    machine_name <- paste0('Machine Name : ', unique(monitor_data$monitor_data1$machine_name))
    parameter_name <- paste0('Parameter Name : ', unique(monitor_data$monitor_data1$parameter_name))
    usl_value <- paste0('USL : ', unique(monitor_data$monitor_data1$usl_value))
    lsl_value <- paste0('LSL : ', unique(monitor_data$monitor_data1$lsl_value))
    central_limit_value <- paste0('Central Limit : ', unique(monitor_data$monitor_data1$central_limit_value))
    if(monitor_process_click$monitor_process_click_event == FALSE) return()
    HTML(paste(machine_name, parameter_name, usl_value, lsl_value, central_limit_value, sep="<br/>"))
  })
  
  output$monitor_process_data <- DT::renderDataTable({
    if(monitor_process_click$monitor_process_click_event == FALSE) return()
    monitor_data$monitor_data1$local_date_time <- as.character(monitor_data$monitor_data1$local_date_time)
    row.names(monitor_data$monitor_data1) <- seq(1:nrow(monitor_data$monitor_data1))
    DT::datatable(monitor_data$monitor_data1[ ,c("local_date_time","parameter_value")], options = list(
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    ), class = 'cell-border stripe')
  })
  
  
  output$R_chart <- renderPlotly({
    if(monitor_product_click$monitor_product_click_event == FALSE) return()
    print('R chart')
    chart <- range_chart_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp, grouping$reactive_groups)
    chart
  })
  
  output$X_chart <- renderPlotly({
    if(monitor_product_click$monitor_product_click_event == FALSE) return()
    print('run chart')
    chart <- x_bar_chart(monitor_data$monitor_data1, setup_frame$setup_frame_temp, grouping$reactive_groups)
    chart
  })
  
  output$monitor_product_data_constants <- renderUI({
    machine_name <- paste0('Machine Name : ', unique(monitor_data$monitor_data1$machine_name))
    parameter_name <- paste0('Parameter Name : ', unique(monitor_data$monitor_data1$parameter_name))
    usl_value <- paste0('USL : ', unique(monitor_data$monitor_data1$usl_value))
    lsl_value <- paste0('LSL : ', unique(monitor_data$monitor_data1$lsl_value))
    central_limit_value <- paste0('Central Limit : ', unique(monitor_data$monitor_data1$central_limit_value))
    if(monitor_product_click$monitor_product_click_event == FALSE) return()
    HTML(paste(machine_name, parameter_name, usl_value, lsl_value, central_limit_value, sep="<br/>"))
  })
  
  output$monitor_product_data <- DT::renderDataTable({
    if(monitor_product_click$monitor_product_click_event == FALSE) return()
    monitor_data$monitor_data1$local_date_time <- as.character(monitor_data$monitor_data1$local_date_time)
    row.names(monitor_data$monitor_data1) <- seq(1:nrow(monitor_data))
    DT::datatable(monitor_data$monitor_data1[ ,c("local_date_time","parameter_value")], options = list(
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    ), class = 'cell-border stripe')
  })
  
  ns <- session$ns
  onevent('mouseenter', 'control_chart', showModal(modalDialog(
    title = "Control Chart",
    output$control_chart_ui <- renderUI({
      if(monitor_process_click$monitor_process_click_event == FALSE) return()
      output$run_chart <- renderPlotly({
        if(monitor_process_click$monitor_process_click_event == FALSE) return()
        chart <- run_chart_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp)
        chart
      })
      withSpinner(plotlyOutput(ns("run_chart"), height = 800), type = getOption("spinner.type", default = 8))
    }),
    size = c("l"),
    easyClose = TRUE
  )))
  
  onevent('mouseenter', 'up_down_trend_id', showModal(modalDialog(
    title = "Up - Down Trend",
    output$up_down_trend_ui <- renderUI({
      if(monitor_process_click$monitor_process_click_event == FALSE) return()
      output$up_down_trend <- renderPlotly({
        if(monitor_process_click$monitor_process_click_event == FALSE) return()
        chart1 <- up_down_trend_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp)
        chart1
      })
      withSpinner(plotlyOutput(ns("up_down_trend"), height = 800), type = getOption("spinner.type", default = 8))
    }),
    size = c("l"),
    easyClose = TRUE
  )))
  
  onevent('mouseenter', 'shift_trend', showModal(modalDialog(
    title = "Shift Trend",
    output$shift_trend_ui <- renderUI({
      if(monitor_process_click$monitor_process_click_event == FALSE) return()
      output$shift <- renderPlotly({
        if(monitor_process_click$monitor_process_click_event == FALSE) return()
        chart2 <- shift_fun(monitor_data$monitor_data1, setup_frame$setup_frame_temp)
        chart2
      })
      withSpinner(plotlyOutput(ns("shift"), height = 800), type = getOption("spinner.type", default = 8))
    }),
    size = c("l"),
    easyClose = TRUE
  )))
  
  # <-------------------------------- Average Chart BUTTON CODE Process and Product--------------------------------> #
  # observeEvent(input$ac_page,{
  #   setup_page_click$setup_page_click_event = FALSE
  #   monitor_page_click$monitor_page_click_event = FALSE
  #   ac_page_click$ac_page_click_event = TRUE
  #   
  #   output$hide_setup_page = eventReactive("",TRUE)
  #   output$hide_monitor_page = eventReactive("",TRUE)
  #   output$hide_ac_page = eventReactive("",FALSE)
  #   
  #   outputOptions(output, "hide_setup_page", suspendWhenHidden = FALSE)
  #   outputOptions(output, "hide_monitor_page", suspendWhenHidden = FALSE)
  #   outputOptions(output, "hide_ac_page", suspendWhenHidden = FALSE)
  #   
  #   # DATABASE CALL TO GET ALL PARAMETERS
  #   setup_frame$setup_frame_temp <- loadData()
  #   
  #   # process_machine_choices_ac = setNames(as.character(setup_param$machine_id),setup_param$machine_name)
  #   # 
  #   # product_job_choices_ac = setNames(as.character(product_param$job_type_id),product_param$job_type_name)
  # })
  
  observeEvent(input$product_job_ac,{
    setup_prod_param <- product_param[product_param$job_type_id == setup_frame$setup_frame_temp$job_id, ]
    temp_data3 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$job_id == input$product_job_ac, ]
    temp_data4 <- setup_prod_param[setup_prod_param$parameter_id == temp_data3$parameter_id, ]
    product_parameter_choices_ac = setNames(as.character(temp_data4$parameter_id),temp_data4$parameter_name)
    updateSelectInput(session, 'product_parameter_ac', label = paste0("Select Parameter"),
                      product_parameter_choices_ac)
  })
  observeEvent(input$product_parameter_ac,{
    setup_prod_param <- product_param[product_param$job_type_id == setup_frame$setup_frame_temp$job_id, ]
    temp_data5 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$job_id == input$product_job_ac, ]
    temp_data5 <- temp_data5[temp_data5$parameter_id == input$product_parameter_ac, ]
    temp_data6 <- setup_prod_param[setup_prod_param$machine_id == temp_data5$machine_id, ]
    product_machine_choices_ac = setNames(as.character(temp_data6$machine_id),temp_data6$machine_name)
    updateSelectInput(session, 'product_machine_ac', label = paste0("Select Machine"),
                      product_machine_choices_ac)
  })
  observeEvent(input$process_machine_ac,{
    setup_param <- process_param[process_param$machine_id == setup_frame$setup_frame_temp$machine_id, ]
    temp_data1 <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$process_machine_ac, ]
    temp_data2 <- setup_param[setup_param$parameter_id == temp_data1$parameter_id, ]
    process_parameter_choices_ac = setNames(as.character(temp_data2$parameter_id),temp_data2$parameter_name)
    updateSelectInput(session, 'process_parameter_ac', label = paste0("Select Parameter"),
                      process_parameter_choices_ac)
  })
  
  observeEvent(input$ac_plot_process,{
    ac_plot_process_click$ac_plot_process_click_event = TRUE
    ac_plot_product_click$ac_plot_product_click_event = TRUE
    print(paste('input$process_machine_ac :: ', input$process_machine_ac))
    print(paste('input$process_parameter_ac :: ', input$process_parameter_ac))
    
      tryCatch({
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$process_machine_ac, ]
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$parameter_id == input$process_parameter_ac, ]
        last_setup <- tail(setup_frame$setup_frame_temp, 1)
        ac_data_inprocess <- process_param[process_param$machine_id == last_setup$machine_id, ]
        ac_data_inprocess <- ac_data_inprocess[ac_data_inprocess$parameter_id == last_setup$parameter_id, ]
        ac_data$ac_data1 <- tail(ac_data_inprocess, 60) 
        
        print("acdata :: ")
        print(ac_data$ac_data1)
        print(paste('nrow ac_data :: ', nrow(ac_data$ac_data1)))
        
        print("setup_data :: ")
        print(setup_frame$setup_frame_temp)
      }, error = function(){
        shinyalert(
          title = "Error",text = "No records found",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
        )
      })
  })
  observeEvent(input$ac_plot_product,{
      tryCatch({
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$job_id == input$product_job_ac, ]
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$parameter_id == input$product_parameter_ac, ]
        setup_frame$setup_frame_temp <- setup_frame$setup_frame_temp[setup_frame$setup_frame_temp$machine_id == input$product_machine_ac, ]
        last_setup <- tail(setup_frame$setup_frame_temp, 1)
        ac_data_inprocess <- product_param[product_param$job_type_id == last_setup$job_id, ]
        ac_data_inprocess <- ac_data_inprocess[ac_data_inprocess$parameter_id == last_setup$parameter_id, ]
        ac_data_inprocess <- ac_data_inprocess[ac_data_inprocess$machine_id == last_setup$machine_id, ]
        ac_data$ac_data1 <- tail(ac_data_inprocess, 32) 
        
        # GROUP CREATION FOR PRODUCT SPC: 60 readings are grouped in total 15 groups with group size of 4 (4 readings/group)
        ac_data$ac_data1$sample <- rep(1:8, each=4)
        parameter_value <- qcc.groups(ac_data$ac_data1$parameter_value, ac_data$ac_data1$sample)
        grouping$reactive_groups <- data.frame(parameter_value)
        grouping$reactive_groups$Range <- apply(grouping$reactive_groups,1,max) - apply(grouping$reactive_groups,1,min)
        grouping$reactive_groups$Mean <- (grouping$reactive_groups$X1+grouping$reactive_groups$X2+grouping$reactive_groups$X3+grouping$reactive_groups$X4)/4
        grouping$reactive_groups$groups <- rep(1:8, each=1)
        print('groupings :: ')
        print(grouping$reactive_groups)
        
        print("acdata :: ")
        print(ac_data$ac_data1)
        print(paste('nrow ac_data :: ', nrow(ac_data$ac_data1)))
        
        print("setup_data :: ")
        print(setup_frame$setup_frame_temp)
      }, error = function(){
        shinyalert(
          title = "Error",text = "No records found",closeOnEsc = TRUE,closeOnClickOutside = FALSE,html = FALSE,type = "error",showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "#18AE68",timer = 0,imageUrl = "",animation = TRUE
        )
      })
    })
  output$bar_chart_proc <- renderPlotly({
    if(ac_page_click$ac_page_click_event == FALSE) return()
    chart <- bar_chart_proc_ac_fun(ac_data$ac_data1, setup_frame$setup_frame_temp, setup_frame$setup_frame_temp)
    chart
  })
  output$run_chart_ac <- renderPlotly({
    if(ac_page_click$ac_page_click_event == FALSE) return()
    chart <- run_chart_ac_fun(ac_data$ac_data1, setup_frame$setup_frame_temp, tail(setup_frame$setup_frame_temp, 1))
    chart
  })
  output$bar_chart_prod <- renderPlotly({
    if(ac_page_click$ac_page_click_event == FALSE) return()
    chart <- bar_chart_prod_ac_fun(ac_data$ac_data1, setup_frame$setup_frame_temp, setup_frame$setup_frame_temp)
    chart
  })
  output$x_bar_chart_ac <- renderPlotly({
    if(ac_page_click$ac_page_click_event == FALSE) return()
    chart <- x_bar_chart_ac_fun(ac_data$ac_data1, setup_frame$setup_frame_temp, tail(setup_frame$setup_frame_temp, 1), grouping$reactive_groups)
    chart
  })
  
}

shinyApp(ui, server)