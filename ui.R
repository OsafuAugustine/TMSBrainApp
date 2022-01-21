library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)
library(readxl)
library(tidyverse)
# library(plyr)
library(plotly)
library(colorRamps)
library(lme4)
library(lmerTest)

# INLA REQUIRE
library(foreach)
library(sp)
library(splancs)
library(rgdal)
library(INLA)
library(INLABMA)
library(geosphere)
library(igraph)
library(rmarkdown)
# source(file = 'Global.R', local = T, encoding = 'UTF8')

shinyUI(fluidPage(
  headerPanel(
    windowTitle = 'TMS BRAIN LEARNING MAP',
    fluidRow(
      column(
        width = 6,
        HTML('<p style="font-size:40px;"><b>TMS BRAIN LEARNING MAP</b></p>'),
        HTML('<p style="font-size:35px;"> NIBS Learning Project </p>')
      ),
      column(
        width = 4,
        HTML('<img src="LOGOS.jpg", height="130px"/>')
      )
    )
  ),
  tags$head(tags$style(HTML(".small-box {height: 130px}"))),
  useShinydashboard(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fluidRow(
        column(
          width = 12,
          column(
            width = 8,
            style = 'padding-left: 0px; padding-right: 10px;',
            fileInput(
              inputId = 'FileInput',
              label = 'File Input',
              width = '100%'
            )
          ),
          column(
            width = 4,
            style = 'padding-left: 10px; padding-right: 0px;',
            actionButton(
              style = "display: inline-block; vertical-align: -32px;",
              inputId = 'SampleData',
              label = 'Sample Data',
              width = '100%'
            )
          ),
          hr(),
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            selectInput(
              inputId = 'SummaryResponse',
              label = 'Summary for Signal',
              choices = list('Range','Standard Deviation','Median','Mean'),
              selected = 'Range'
            )
          ),
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            selectInput(
              inputId = 'Model',
              label = 'Spatial Model',
              choices = list('SPDE','ICAR','BYM','LEROUX'),
              selected = 'SPDE'
            )
          ),
          uiOutput('ParModels'),
          fluidRow(),
          hr(),
          fluidRow(),
          # 
          column(
            width = 3,
            style = 'padding-left: 0px; padding-right: 10px;',
            div(style = "display: inline-block; margin-left: 20px; margin-right: 20px; vertical-align: -40px;",
                prettyCheckbox(
                  inputId = 'Rep1',
                  label = 'Rep 1',
                  value = TRUE,
                  width = '100%',
                  icon = icon("check"),
                  bigger=T,
                  status = "primary",
                  inline = F
                )
            )
          ),
          column(
            width = 9,
            style = 'padding-left: 10px; padding-right: 0px;',
            # div(style = "display: inline-block;",
            sliderInput(
              inputId = 'Rep',
              label = 'Replication',
              min = 1,
              max = 10,
              value = 1,
              step = 1
            )
            # )
          ),
          fluidRow(),
          hr(),
          fluidRow(),
          actionButton(
            inputId = 'Calculate',
            label = 'Fit Model',
            width = '100%'
          ),fluidRow(),
          hr(),
          fluidRow(
            column(
              width=5,
              numericInput(
                inputId = "quantile",
                label = "MEP",
                value = NULL,
                width = '100%')
            ),
            hr(),
            column(
              width = 7,
              actionButton(
                inputId = 'Probability',
                label = 'Tail Probability',
                width = '100%'
              )
            )
          ),
          
          hr(),
          fluidRow(
            column(
              width=12,
              downloadButton(
                outputId = "downloadReport",
                label = "Generate Report"
              )
            )
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        tabsetPanel(
          selected = 'ADJUST',
          type = 'pills',
          tabPanel(
            title = 'ADJUST',
            wellPanel(
              fluidRow(
                column(
                  width = 4,
                  fluidRow(
                    column(
                      width = 12,
                      style = 'padding-left:15px; padding-right:5px, padding-bottom:7.5px, padding-top:0px',
                      bsCollapse(
                        open = 'BS.HOTSPOT.HEAD',
                        bsCollapsePanel(
                          title = '3D HOTSPOT LOCATION',
                          value = 'BS.HOTSPOT.HEAD',
                          # rglwidgetOutput('PLOT3D', width = '100%', height = 300)
                          plotlyOutput('PLOT3D', width = '100%', height = 250)
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      # style = 'padding-left:15px; padding-right:5px, padding-bottom:-15px, padding-top:7.5px',
                      valueBoxOutput(
                        outputId = 'CARD',
                        width = '100%'
                      )
                    ),
                    column(
                      width = 6,
                      # style = 'padding-left:15px; padding-right:5px, padding-bottom:-15px, padding-top:7.5px',
                      valueBoxOutput(
                        outputId = 'CARD2',
                        width = '100%'
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      style = 'padding-left:15px; padding-right:5px, padding-bottom:-15px, padding-top:7.5px',
                      bsCollapse(
                        open = 'BS.REPLICATION',
                        bsCollapsePanel(
                          title = 'REPLICATION EFFECT',
                          value = 'BS.REPLICATION',
                          plotOutput('REPLICATION', width = '100%', height = 200)
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  fluidRow(
                    column(
                      width = 6,
                      style = 'padding-left:5px; padding-right:5px, padding-bottom:7.5px, padding-top:0px',
                      bsCollapse(
                        open = 'BS.TMS.BRAIN.MAPPING',
                        bsCollapsePanel(
                          title = 'PIXEL MAP',
                          value = 'BS.TMS.BRAIN.MAPPING',
                          plotOutput('TMS.BRAIN.MAPPING', width = '100%', height = 300)
                        )
                      )
                    ),
                    column(
                      width = 6,
                      style = 'padding-left:5px; padding-right:15px, padding-bottom:5px, padding-top:0px',
                      bsCollapse(
                        open = 'BS.HOTSPOT',
                        bsCollapsePanel(
                          title = 'HOTSPOT MAP',
                          value = 'BS.HOTSPOT',
                          plotOutput('HOTSPOT', width = '100%', height = 300)
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:15px, padding-bottom:-15px, padding-top:7.5px',
                      bsCollapse(
                        open = 'BS.TESTING.ORDERS',
                        bsCollapsePanel(
                          title = 'TESTING ORDERS',
                          value = 'BS.TESTING.ORDERS',
                          plotOutput('TESTING.ORDERS', width = '100%', height = 300)
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            title = 'MODEL SUMMARY',
            wellPanel(
              fluidRow(
                column(
                  width = 12,
                  verbatimTextOutput('SUMMARY.OUTPUT')
                )
              )
            )
          ),
          tabPanel(
            title = 'VIEW DATA',
            wellPanel(
              fluidRow(
                column(
                  width = 12,
                  dataTableOutput('datatable')
                )
              )
            )
          )
        )
      )
    )
  ),
  hr(),
  HTML('<p style="text-align:center; color:gray; font-size:18px"><b>JOINT WORK</b><br>Dylan EDWARDS, Onno van der GROEN, Diego NASCIMENTO, Oilson GONZATTO, Osafu EGBON, Francisco LOUZADA</p>'),
  useShinyalert()
))
