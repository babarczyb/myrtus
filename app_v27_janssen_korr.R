
### Installing and loading packages

#install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, data.table, ggthemes, shiny, here, truncnorm, shinydashboard, shinyWidgets,
               randomizr, jph, stats, scales, shinyjs, renv)

renv::isolate()



### Input settings

# Sidebar with tabs
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Model", tabName = "tab_model"),
    menuItem("Treatment algorithm", tabName = "tab_treatment"),
    menuItem("DSA", tabName = "tab_dsa",
             badgeLabel = "extension", badgeColor = "red")
  )
)


# Main panel with tabs and boxes
body <- dashboardBody(
  
  tags$head(        
    #in line styling
    tags$style(HTML(
      
      #siderbar background
      '.skin-red .main-sidebar {
          background-color: white;}',
      
      
      #sidebar menu background
      '.skin-red .main-sidebar .sidebar-menu > li.active > a {
        background-color: red;
      }',
      
      '.skin-red .main-sidebar .sidebar-menu > li:hover > a {
        background-color: red;
      }',
      
      #siderbar text color
      '.skin-red .main-sidebar .sidebar{
          color: red;}',
      
      #sidebar text size
      '.skin-red .main-sidebar .sidebar{
          font-size: 18px; }',
      
      #radio button option labels
      '.radio label { font-size: 16px; }',
      
      # select box option labels
      '.selectize-dropdown { font-size: 16px; }',
      '.selectize-input { font-size: 16px; }',
      
      # numeric input label (N)
      '#N { font-size: 16px; }',
      
      # action button text
      '.btn { font-size: 16px; }',
      
      # checkbox input label (dsa_vars)
      '#dsa_vars { font-size: 16px; }'
      
    )),
    
  ),
  
  
  tabItems(
    
    
    tabItem(tabName = "tab_model", # Tab 1: model
            
            
            fluidRow( # Organising content into boxes
              
              column(width = 4,
                     
                     #Box 0 (logo)
                     box(width = '25%', tags$img(src = "janssen_logo.png"),
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE),
                     
                     #Box 1.1.1 (dara2l - select box)
                     box(status="primary", solidHeader = TRUE, width = 500,
                         selectInput(inputId ="dara2l", label =h4(strong("Daratumumab available in 2L?"), style = "font-size:18px;"),
                                     choices = list(
                                       "Yes" = 1, "No" = 0
                                     ))),
                     
                     
                     #Box 1.1.2 (dara retreatment - select box)
                     box(status="primary", solidHeader = TRUE, width = 500,
                         selectInput(inputId ="dara_retreat", label =h4(strong("Daratumumab available in 3L, if used in 1L?"), style = "font-size:18px;"),
                                     choices = list(
                                       "Yes" = 1, "No" = 0
                                     ))),
                     
                     
                     
                     #Box 1.2 (scenario - radio button)
                     box(status="primary", solidHeader = TRUE,width = 500,
                         radioButtons(inputId ="scenario", label =h4(strong("Scenario"), style = "font-size:18px;"),
                                      choices = list(
                                        "1: Daratumumab in 1L & 3L vs. daratumumab only in 2L/3L" = 1,
                                        "2: Daratumumab in 1L & 3L vs. no daratumumab" = 2,
                                        "3: Daratumumab only in 2L/3L vs. no daratumuab" = 3))),
                     
                     
                     #Box 1.3 (ASCT rate - slider input)
                     box(status="primary", solidHeader = TRUE,
                         width = 500,
                         sliderInput(inputId ="asct_rate",
                                     label = h4(strong("ASCT ratio (%)"), style = "font-size:18px;"),
                                     value = 35,
                                     min= 0,
                                     max= 100)),
                     
                     
                     #Box 1.4 (VenVd rate - slider input)
                     box(status="primary", solidHeader = TRUE,
                         width = 500,
                         sliderInput(inputId ="venvd_rate",
                                     label = h4(strong("VenVd eligibility ratio (%)"), style = "font-size:18px;"),
                                     value = 20,
                                     min= 0,
                                     max= 100)),
                     
                     
                     #Box 1.5 (cytrisk rate - slider input)
                     box(status="primary", solidHeader = TRUE,
                         width = 500,
                         sliderInput(inputId ="cytrisk_rate",
                                     label = h4(strong("High cytologic risk ratio (%)"), style = "font-size:18px;"),
                                     value = 30,
                                     min= 0,
                                     max= 100)),
                     
                     
                     #Box 1.6 (discount rate - slider input)
                     box(status="primary", solidHeader = TRUE,
                         width = 500,
                         sliderInput(inputId ="disc_cost",
                                     label = h4(strong("Cost discount rate (%)"), style = "font-size:18px;"),
                                     value = 3.7,
                                     min= 1,
                                     max= 20,
                                     step = 0.1)),
                     
                     
                     #Box 1.7 (N - numeric input)
                     box(title=h4(strong("Population size")),
                         status = "primary", solidHeader = TRUE,
                         width = 500,
                         
                         numericInputIcon(inputId ="N",
                                          label = "N",
                                          value = 10000,
                                          min= 0,
                                          max= 10000)),
                     
                     
                     #Box 1.x (action button runs model when pressed)
                     box(status="primary", solidHeader = TRUE,
                         width = 500,
                         actionButton(inputId ="run_model",
                                      label   ="Run model")),
                     
                     
                     #Box 1.xx (action button writes results in csv)
                     box(status="primary", solidHeader = TRUE,
                         width = 500,
                         actionButton(inputId ="save_csv",
                                      label   ="Export patient level results")),
                     
                     
                     
              ), #end of column
              
              
              
              
              
              column(width = 8, # Output panel of Tab 1
                     
                     # Explanation
                     helpText(h3(strong("Model outputs"))),
                     
                     #Box O1 (Output graph)
                     box(title=h3(strong("Graph")),
                         status = "danger",
                         width = 700,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         plotOutput(outputId ="barChart"),
                         color="yellow"),
                     
                     
                     #Box O2 (Output table)
                     box(title=h3(strong("ICER table (cost per life years, without quality of life)")),
                         status = "danger",
                         width = 300,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         tableOutput(outputId ="icer_table"),
                         useShinyjs(),
                         inlineCSS(list("table" = "font-size: 18px")),
                         color="yellow")
                     
              ), # end of column
              
              
              
              
              
              
              column(width = 8,  # Treatment cost inputs
                     
                     
                     # Explanation
                     helpText(h3(strong("Treatments costs by medicine"))),
                     
                     
                     #Box 3.1 (Daratumumab)
                     box(title=h4(strong("Daratumumab (D)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_D",
                                          label = h4(strong("Darzalex injection 1800 mg 1x15 ml (HUF)"), style = "font-size:18px;"),
                                          value = 1567782,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.2 (Carfilzomib)
                     box(title=h4(strong("Carfilzomib (K)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_K",
                                          label = h4(strong("Kyprolis powder 60 mg 1x (HUF)"), style = "font-size:18px;"),
                                          value = 385400,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     #Box 3.3 (Melphalan)
                     box(title=h4(strong("Melphalan (M)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_M",
                                          label = h4(strong("Zentiva powder 50 mg (HUF)"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.4 (Pomalidomide)
                     box(title=h4(strong("Pomalidomide (Pom)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_Pom",
                                          label = h4(strong("Imnovid capsule 21x4 mg (HUF)"), style = "font-size:18px;"),
                                          value =  2348243,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.5 (Lenalidomide)
                     box(title=h4(strong("Lenalidomide (R)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_R",
                                          label = h4(strong("Lenalidomide Sandoz capsule 21x10 mg (HUF)"), style = "font-size:18px;"),
                                          value =  509472,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.6 (Selinexor)
                     box(title=h4(strong("Selinexor (S)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_S",
                                          label = h4(strong("Selinexor"), style = "font-size:18px;"),
                                          value =  0,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.7 (Bortezomib)
                     box(title=h4(strong("Bortezomib (V)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_V",
                                          label = h4(strong("Bortezomib MSN powder 3.5 mg 1x (HUF)"), style = "font-size:18px;"),
                                          value =  102081,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.8 (Venetoclax)
                     box(title=h4(strong("Venetoclax (Ven)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_Ven",
                                          label = h4(strong("Venclyxto tablet 14x100 mg (HUF)"), style = "font-size:18px;"),
                                          value =  210470,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     
                     #Box 3.9 (Ixazomib)
                     box(title=h4(strong("Ixazomib (Ixa)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_Ixa",
                                          label = h4(strong("Ninlaro capsule tablet 3x4 mg (HUF)"), style = "font-size:18px;"),
                                          value =  1309335,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     
                     #Box 3.10 (Isatuximab)
                     box(title=h4(strong("Isatuximab (Isa)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_Isa",
                                          label = h4(strong("Isatuximab (HUF)"), style = "font-size:18px;"),
                                          value =  0,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.11 (Dexamethasone)
                     box(title=h4(strong("Dexamethasone (d)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_dd",
                                          label = h4(strong("Dexamethasone Krka tablet 50x20 mg (HUF)"), style = "font-size:18px;"),
                                          value =  20311,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.12 (Prednisone)
                     box(title=h4(strong("Prednisolone (P)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_P",
                                          label = h4(strong("Prednisolon-Richter tablet 100x5 mg (HUF)"), style = "font-size:18px;"),
                                          value =  1339,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     
                     #Box 3.13 (Thalidomide)
                     box(title=h4(strong("Thalidomide (T)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_T",
                                          label = h4(strong("Thalidomide Accord capsule 28x50 mg (HUF)"), style = "font-size:18px;"),
                                          value =  63709,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")),
                     
                     
                     
                     
                     #Box 3.14 (Cyclophosphamide)
                     box(title=h4(strong("Cyclophosphamide (C)")),
                         status = "info",
                         solidHeader=T,
                         collapsible=T,
                         width = 400,
                         
                         autonumericInput(inputId ="cost_C",
                                          label = h4(strong("Cyclophosphamide (HUF)"), style = "font-size:18px;"),
                                          value =  0,
                                          min= 0,
                                          max= 100000000,
                                          decimalPlaces = 0,
                                          digitGroupSeparator = " ")) 
                     
                     
                     
              ) # Close column
              
            ) # Close fluidRow
            
    ), # Close Tab 1
    
    
    
    
    tabItem(tabName = "tab_treatment", # Tab 2: treatments
            
            fluidRow(
              
              column(width = 8, # Treatment rates
                     
                     
                     # Explanation
                     helpText(h3(strong("Distribution of different treatments at specific points of the treatment algorithm"))),
                     
                     #Box 4.1 (1L/2L - ASCT: induction, no Dara)
                     box(title=h4(strong("1L/2L, ASCT: induction, with no daratumumab")),
                         status = "warning", 
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_1", # VRd
                                          label = h4(strong("VRd"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_2", # VRT
                                          label = h4(strong("VTd"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_3", # VCd
                                          label = h4(strong("VCd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     #Box 4.2 (1L - ASCT: maintenance)
                     box(title=h4(strong("1L, ASCT: maintenance")),
                         status = "warning",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_35", # V maintenance
                                          label = h4(strong("bortezomib miantenance"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_36", # R maintenance
                                          label = h4(strong("lenalidomide maintenance"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     #Box 4.3 (1L - No ASCT, with Dara)
                     box(title=h4(strong("1L, no ASCT, with daratumumab")),
                         status = "warning",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_4", # DRd
                                          label = h4(strong("DRd"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_5", # DVMP
                                          label = h4(strong("DVMP"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     #Box 4.4 (1L - No ASCT, no Dara)
                     box(title=h4(strong("1L, no ASCT, with no daratumumab")),
                         status = "warning",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_6", # VRd
                                          label = h4(strong("VRd"), style = "font-size:18px;"),
                                          value = 33,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_7", # VMP
                                          label = h4(strong("VMP"), style = "font-size:18px;"),
                                          value = 33,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_8", # Rd
                                          label = h4(strong("Rd"), style = "font-size:18px;"),
                                          value = 34,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     #Box 4.5 (2L - 1L no dara, 2L Dara)
                     box(title=h4(strong("2L, first line no daratumumab, 2L daratumumab present")),
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_34", # DRd
                                          label = h4(strong("DRd"), style = "font-size:18px;"),
                                          value = 100,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_32", # DVd
                                          label = h4(strong("DVd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_33", # DKd
                                          label = h4(strong("DKd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     
                     #Box 4.6 (2L - 1L bortezomib, no Dara)
                     box(title=h4(strong("2L, first line bortezomib, no daratumumab")),
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_9", # KRd
                                          label = h4(strong("KRd"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_10", # IxaRd
                                          label = h4(strong("IxaRd"), style = "font-size:18px;"),
                                          value = 50,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     
                     #Box 4.7 (2L - 1L lenalidomide, no Dara)
                     box(title=h4(strong("2L, first line lenalidomide, no daratumumab (treatment rates in addition to VenVd)")),
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_11", # KRd
                                          label = h4(strong("KRd"), style = "font-size:18px;"),
                                          value = 33,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_12", # IsaKd
                                          label = h4(strong("IsaKd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_13", # PomVd
                                          label = h4(strong("PomVd"), style = "font-size:18px;"),
                                          value = 33,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_14", # IxaRd
                                          label = h4(strong("IxaRd"), style = "font-size:18px;"),
                                          value = 34,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_15", # SVd
                                          label = h4(strong("SVd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     
                     #Box 4.8 (2L - 1L DVTd/DVMP)
                     box(title=h4(strong("2L, first line DVTd or DVMP (treatment rates in addition to VenVd)")),
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_16", # KRd
                                          label = h4(strong("KRd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_17", # Kd
                                          label = h4(strong("Kd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_18", # VRd
                                          label = h4(strong("VRd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_19", # IxaRd
                                          label = h4(strong("IxaRd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_20", # SVd
                                          label = h4(strong("SVd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     
                     #Box 4.9 (2L - 1L DRd)
                     box(title=h4(strong("2L, first line DRd (treatment rates in addition to VenVd)")),
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_21", # PomVd
                                          label = h4(strong("PomVd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_22", # Kd
                                          label = h4(strong("Kd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_23", # KRd
                                          label = h4(strong("KRd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_24", # IxaRd
                                          label = h4(strong("IxaRd"), style = "font-size:18px;"),
                                          value = 25,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_25", # SVd
                                          label = h4(strong("SVd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     
                     #Box 4.10 (3L - Dara)
                     box(title=h4(strong("3L, daratumumab present")),
                         status = "success",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_26", # DPd
                                          label = h4(strong("DPd"), style = "font-size:18px;"),
                                          value = 33,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_27", # DVd
                                          label = h4(strong("DVd"), style = "font-size:18px;"),
                                          value = 33,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_28", # DKd
                                          label = h4(strong("DKd"), style = "font-size:18px;"),
                                          value = 34,
                                          min= 0,
                                          max= 100,
                                          icon= list(NULL, icon("percent")))
                     ),
                     
                     
                     
                     #Box 4.11 (3L - no Dara)
                     box(title=h4(strong("3L, no daratumumab (treatment rates in addition to VenVd)")),
                         status = "success",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         
                         numericInputIcon(inputId ="rate_29", # IsaKd
                                          label = h4(strong("IsaKd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_30", # IsaPd
                                          label = h4(strong("IsaPd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent"))),
                         
                         numericInputIcon(inputId ="rate_31", # SVd
                                          label = h4(strong("SVd"), style = "font-size:18px;"),
                                          value = 0,
                                          min= 0,
                                          max= 0,
                                          icon= list(NULL, icon("percent")))
                     )
                     
              ) # Close column
              
            )#Close fluidRow
            
    ), # Close Tab 2
    
    
    
    
    tabItem(tabName = "tab_dsa", # Tab 3: DSA
            
            fluidRow( # Organising content into boxes
              
              
              # Title: DSA
              helpText(h3(strong("Deterministic sensitivity analysis (DSA)")),
                       width = 12),
              
              
              helpText(h4(strong("Attention: this may take 1-72 hours to run at N=10,000,
                                depending on the number of parameters selected for DSA
                                and on computing performance available.")),
                       width = 12),
              
              
              column(width = 4, # DSA inputs
                     
                     #Box 2.1 (DSA rate - slider input)
                     box(status="danger",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         sliderInput(inputId ="dsa_rate",
                                     label = h4(strong("DSA correction rate (%)"), style = "font-size:18px;"),
                                     value = 10,
                                     min= 1,
                                     max= 20,
                                     step = 1)),
                     
                     
                     # Box 2.2 (Variables selected for DSA)
                     box(status="danger",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 400,
                         checkboxGroupInput(inputId = "dsa_vars",
                                            label = h4(strong("Variables selected for DSA"), style = "font-size:18px;"),
                                            choices = list("Maximum length of treatment cycles" = "max_cycles_dsa",
                                                           "The extent of maintenance improving treatment response" = "maint_better_dsa",
                                                           "Adverse event rate of daratumumab treatments" = "adverse_dara_dsa",
                                                           "Adverse event rate of other treatments" = "adverse_other_dsa",
                                                           "Complete response (CR) rate of daratumumab treatments" = "cr_dara_dsa",
                                                           "Complete response (CR) rate of other treatments" = "cr_other_dsa",
                                                           "Very good partial response (VGPR) rate of daratumumab treatments" = "vgpr_dara_dsa",
                                                           "Very good partial response (VGPR) rate of other treatments" = "vgpr_other_dsa",
                                                           "Partial response (PR) rate of daratumumab treatments" = "pr_dara_dsa",
                                                           "Partial response (PR) rate of other treatments" = "pr_other_dsa",
                                                           "Minimal response (MR) rate of daratumumab treatments" = "mr_dara_dsa",
                                                           "Minimal response (MR) rate of other treatments" = "mr_other_dsa",
                                                           "Stable disease (SD) rate of daratumumab treatments" = "sd_dara_dsa",
                                                           "Stable disease (SD) rate of other treatments" = "sd_other_dsa",
                                                           "Progressive disease (PD) rate of daratumumab treatments" = "pd_dara_dsa",
                                                           "Progressive disease (PD) rate of other treatments" = "pd_other_dsa",
                                                           "Cost of daratumumab treatments" = "costs_dara_dsa",
                                                           "Cost of other treatments" = "costs_other_dsa",
                                                           "Probability of progression per quarter \nat response CR" = "cr_pfs_dsa",
                                                           "Probability of progression per quarter \nat response VGPR" = "vgpr_pfs_dsa",
                                                           "Probability of progression per quarter \nat response PR" = "pr_pfs_dsa",
                                                           "Probability of progression per quarter \nat response MR" = "mr_pfs_dsa",
                                                           "Probability of progression per quarter \nat response SD" = "sd_pfs_dsa",
                                                           "Probability of progression per quarter \nat response PD" = "pd_pfs_dsa",
                                                           "Probability of death per quarter \nat response CR" = "cr_os_dsa",
                                                           "Probability of death per quarter \nat response VGPR" = "vgpr_os_dsa",
                                                           "Probability of death per quarter \nat response PR" = "pr_os_dsa",
                                                           "Probability of death per quarter \nat response MR" = "mr_os_dsa",
                                                           "Probability of death per quarter \nat response SD" = "sd_os_dsa",
                                                           "Probability of death per quarter \nat response PD" = "pd_os_dsa"))),
                     
                     
                     
                     #Box 2.x (action button runs DSA)
                     box(status="danger",
                         width = 400,
                         actionButton(inputId ="run_dsa",
                                      label   ="Run DSA with the above inputs")),
                     
              ), # Column ends
              
              
              
              
              column(width = 8, # DSA output
                     
                     
                     #Box O3 (DSA output)
                     box(title=h3(strong("DSA results")),
                         status = "danger",
                         width = 400,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         plotOutput(outputId ="tornadoPlot"),
                         color="yellow")
                     
              ) # Column ends
              
              
            ) # Close fuidRow
            
    ) # Close Tab 2
    
  ) # Close tabItems
  
  
)#close dashboardBody



### User interface

ui <- dashboardPage (
  
  skin = "red",
  
  # title of app
  dashboardHeader(title = h2(strong("Daratumumab for the treatment of patients with multiple myeloma"), style = "font-size:24px;"), titleWidth = 800),
  
  sidebar,
  body
  
) # Close ui dashboardPage


  
 





### Output functions

server <- function(input, output, session) {

  ### Output: Dynamic UI functions
  
    # Treatment rates 1L/2L, ASCT, no dara
  
  observeEvent(input$rate_1, {
    updateNumericInput(inputId = "rate_2", value = 100 - input$rate_1)
  }) 
  
  
  observeEvent(input$rate_2, {
    updateNumericInput(inputId = "rate_1", value = 100 - input$rate_2)
  }) 
  
  
  #observeEvent(input$rate_3, {
  #  updateNumericInput(inputId = "rate_1", value = 100 - input$rate_2 - input$rate_3)
  #  updateNumericInput(inputId = "rate_2", value = 100 - input$rate_1 - input$rate_3)
  #})
  
  
  
  # Treatments rates 1L, maintenance
  
  observeEvent(input$rate_35, {
    updateNumericInput(inputId = "rate_36", value = 100 - input$rate_35)
  }) 
  
  
  observeEvent(input$rate_36, {
    updateNumericInput(inputId = "rate_35", value = 100 - input$rate_36)
  }) 
  
  
  # Treatments rates 1L, no ASCT, with dara
  
  observeEvent(input$rate_4, {
    updateNumericInput(inputId = "rate_5", value = 100 - input$rate_4)
  }) 
  
  
  observeEvent(input$rate_5, {
    updateNumericInput(inputId = "rate_4", value = 100 - input$rate_5)
  }) 
  
  
  # Treatment rates 1L, no ASCT, no dara
  
  observeEvent(input$rate_6, {
    updateNumericInput(inputId = "rate_8", value = 100 - input$rate_6 - input$rate_7)
  }) 
  
  
  observeEvent(input$rate_7, {
    updateNumericInput(inputId = "rate_8", value = 100 - input$rate_6 - input$rate_7)
  }) 
  
  
  observeEvent(input$rate_8, {
    updateNumericInput(inputId = "rate_6", value = 100 - input$rate_7 - input$rate_8)
    updateNumericInput(inputId = "rate_7", value = 100 - input$rate_6 - input$rate_8)
  })
  
  
  # Treatment rates 2L, fist line no dara, 2L dara
  
  observeEvent(input$rate_34, {
    updateNumericInput(inputId = "rate_33", value = 100 - input$rate_34 - input$rate_32)
  }) 
  
  
  observeEvent(input$rate_32, {
    updateNumericInput(inputId = "rate_33", value = 100 - input$rate_34 - input$rate_32)
  }) 
  
  
  observeEvent(input$rate_33, {
    updateNumericInput(inputId = "rate_34", value = 100 - input$rate_32 - input$rate_33)
    updateNumericInput(inputId = "rate_32", value = 100 - input$rate_34 - input$rate_33)
  })
  
  
  
  # Treatments rates 2L, fist line bortezomib, no dara
  
  observeEvent(input$rate_9, {
    updateNumericInput(inputId = "rate_10", value = 100 - input$rate_9)
  }) 
  
  
  observeEvent(input$rate_10, {
    updateNumericInput(inputId = "rate_9", value = 100 - input$rate_10)
  })
  
  
  
  # Treatment rates 2L, first line lenalidomide, no dara
  
  observeEvent(input$rate_11, {
    updateNumericInput(inputId = "rate_14", value = 100 - input$rate_11 - input$rate_13)
  }) 
  
  
  #observeEvent(input$rate_12, {
  #  updateNumericInput(inputId = "rate_15", value = 100 - input$rate_11 - input$rate_12 - input$rate_13 - input$rate_14)
  #})
  
  
  observeEvent(input$rate_13, {
    updateNumericInput(inputId = "rate_14", value = 100 - input$rate_11 - input$rate_13)
  })
  
  
  observeEvent(input$rate_14, {
    updateNumericInput(inputId = "rate_11", value = 100 - input$rate_13 - input$rate_14)
    updateNumericInput(inputId = "rate_13", value = 100 - input$rate_11 - input$rate_14)
  })
  
  
  #observeEvent(input$rate_15, {
  #  updateNumericInput(inputId = "rate_11", value = 100 - input$rate_15 - input$rate_12 - input$rate_13 - input$rate_14)
  #  updateNumericInput(inputId = "rate_12", value = 100 - input$rate_11 - input$rate_15 - input$rate_13 - input$rate_14)
  # updateNumericInput(inputId = "rate_13", value = 100 - input$rate_11 - input$rate_12 - input$rate_15 - input$rate_14)
  #  updateNumericInput(inputId = "rate_14", value = 100 - input$rate_11 - input$rate_12 - input$rate_13 - input$rate_15)
  #})
  
  
  
  # Treatment rates 2L, first line DVTd/DVMP
  
  observeEvent(input$rate_16, {
    updateNumericInput(inputId = "rate_19", value = 100 - input$rate_16 - input$rate_17 - input$rate_18)
  }) 
  
  
  observeEvent(input$rate_17, {
    updateNumericInput(inputId = "rate_19", value = 100 - input$rate_16 - input$rate_17 - input$rate_18)
  })
  
  
  observeEvent(input$rate_18, {
    updateNumericInput(inputId = "rate_19", value = 100 - input$rate_16 - input$rate_17 - input$rate_18)
  })
  
  
  observeEvent(input$rate_19, {
    updateNumericInput(inputId = "rate_16", value = 100 - input$rate_17 - input$rate_18 - input$rate_19)
    updateNumericInput(inputId = "rate_17", value = 100 - input$rate_16 - input$rate_18 - input$rate_19)
    updateNumericInput(inputId = "rate_18", value = 100 - input$rate_16 - input$rate_17 - input$rate_19)
  })
  
  
  #observeEvent(input$rate_20, {
  #  updateNumericInput(inputId = "rate_16", value = 100 - input$rate_20 - input$rate_17 - input$rate_18 - input$rate_19)
  #  updateNumericInput(inputId = "rate_17", value = 100 - input$rate_16 - input$rate_20 - input$rate_18 - input$rate_19)
  #  updateNumericInput(inputId = "rate_18", value = 100 - input$rate_16 - input$rate_17 - input$rate_20 - input$rate_19)
  #  updateNumericInput(inputId = "rate_19", value = 100 - input$rate_16 - input$rate_17 - input$rate_18 - input$rate_20)
  #})
  
  
  
  # Treatment rates 2L, first line DRd
  
  observeEvent(input$rate_21, {
    updateNumericInput(inputId = "rate_24", value = 100 - input$rate_21 - input$rate_22 - input$rate_23)
  }) 
  
  
  observeEvent(input$rate_22, {
    updateNumericInput(inputId = "rate_24", value = 100 - input$rate_21 - input$rate_22 - input$rate_23)
  })
  
  
  observeEvent(input$rate_23, {
    updateNumericInput(inputId = "rate_24", value = 100 - input$rate_21 - input$rate_22 - input$rate_23)
  })
  
  
  observeEvent(input$rate_24, {
    updateNumericInput(inputId = "rate_21", value = 100 - input$rate_22 - input$rate_23 - input$rate_24)
    updateNumericInput(inputId = "rate_22", value = 100 - input$rate_21 - input$rate_23 - input$rate_24)
    updateNumericInput(inputId = "rate_23", value = 100 - input$rate_21 - input$rate_22 - input$rate_24)
  })
  
  
  #observeEvent(input$rate_25, {
  #  updateNumericInput(inputId = "rate_21", value = 100 - input$rate_25 - input$rate_22 - input$rate_23 - input$rate_24)
  #  updateNumericInput(inputId = "rate_22", value = 100 - input$rate_21 - input$rate_25 - input$rate_23 - input$rate_24)
  #  updateNumericInput(inputId = "rate_23", value = 100 - input$rate_21 - input$rate_22 - input$rate_25 - input$rate_24)
  #  updateNumericInput(inputId = "rate_24", value = 100 - input$rate_21 - input$rate_22 - input$rate_23 - input$rate_25)
  #})
  
  
  # Treatment rates 3L, dara present
  
  observeEvent(input$rate_26, {
    updateNumericInput(inputId = "rate_28", value = 100 - input$rate_26 - input$rate_27)
  }) 
  
  
  observeEvent(input$rate_27, {
    updateNumericInput(inputId = "rate_28", value = 100 - input$rate_26 - input$rate_27)
  }) 
  
  
  observeEvent(input$rate_28, {
    updateNumericInput(inputId = "rate_26", value = 100 - input$rate_27 - input$rate_28)
    updateNumericInput(inputId = "rate_27", value = 100 - input$rate_26 - input$rate_28)
  })
  
  
  # Treatment rates 3L, no dara
  
  #observeEvent(input$rate_29, {
  #  updateNumericInput(inputId = "rate_31", value = 100 - input$rate_29 - input$rate_30)
  #}) 
  
  
  #observeEvent(input$rate_30, {
  #  updateNumericInput(inputId = "rate_31", value = 100 - input$rate_29 - input$rate_30)
  #}) 
  
  
  #observeEvent(input$rate_31, {
  #  updateNumericInput(inputId = "rate_29", value = 100 - input$rate_31 - input$rate_30)
  #  updateNumericInput(inputId = "rate_30", value = 100 - input$rate_29 - input$rate_31)
  #})
  
  
  
 
   
  
  
  ### Output: Model code
  
  #when action button "Run model" pressed ...
  observeEvent(input$run_model,
               ignoreNULL = T, {
  
  
  ##### I.A. Source treatment and outcome functions ###############################
  
  source(here::here("model_functions_janssen_korr.R"))
                 
                 
  ##### II. Inputs and population generation ######################################
  #library(dplyr)
  #library(randomizr)
  #library(data.table)
  
  options(scipen=999)
  
  #setwd("C:Users/gyorb/OneDrive - SYREON Kft/Myeloma - code")
  #setwd('C:/Users/david.nagy/SYREON Kft/David GYORBIRO - Myeloma - code')
  input_0 <- read.csv(here::here("input_1.csv"), header=FALSE, fileEncoding="UTF-8-BOM")
  input_0_frame <- data.frame(input_0)
  
  input1 <- t(input_0_frame[-1])
  colnames(input1) <- input_0_frame[, 1]
  input_final <- data.table(input1)
  
  #vektorokba gyujt?tt inputok
  max_cycles_vector <- c(100, input_final$max_cycles_vector[!is.na(input_final$max_cycles_vector)])
  p_adverse_tx_vector <- input_final$p_adverse_tx_vector[!is.na(input_final$p_adverse_tx_vector)]
  p_cr_vector <- input_final$p_cr_vector[!is.na(input_final$p_cr_vector)]
  p_vgpr_vector <- input_final$p_vgpr_vector[!is.na(input_final$p_vgpr_vector)]
  p_pr_vector <- input_final$p_pr_vector[!is.na(input_final$p_pr_vector)]
  p_mr_vector <- input_final$p_mr_vector[!is.na(input_final$p_mr_vector)]
  p_sd_vector <- input_final$p_sd_vector[!is.na(input_final$p_sd_vector)]
  p_pd_vector <- input_final$p_pd_vector[!is.na(input_final$p_pd_vector)]
  
  
  #m?trixok is vannak
  pfs_matrix <- cbind(input_final$l1_pfs_vector[!is.na(input_final$l1_pfs_vector)], input_final$l2_pfs_vector[!is.na(input_final$l2_pfs_vector)], input_final$l3_pfs_vector[!is.na(input_final$l3_pfs_vector)])
  os_matrix <- cbind(input_final$l1_os_vector[!is.na(input_final$l1_os_vector)], input_final$l2_os_vector[!is.na(input_final$l2_os_vector)], input_final$l3_os_vector[!is.na(input_final$l3_os_vector)])
  
  #marad?k
  number_of_cycles <- input_final$number_of_cycles[!is.na(input_final$number_of_cycles)]
  maint_better <- input_final$maint_better[!is.na(input_final$maint_better)]
  
  
  #egyeb shiny inputok
  dara2l <- input$dara2l
  scenario <- input$scenario
  asct_rate <- input$asct_rate/100
  venvd_rate <- input$venvd_rate/100
  cytrisk_rate <- input$cytrisk_rate/100
  N <- input$N
  disc_cost <- input$disc_cost/100
  dara_retr <- input$dara_retreat
  
  
  
  #treatment rates (shiny)
  rates_vector <- NA
  rates_vector[1] <- input$rate_1
  rates_vector[2] <- input$rate_2
  rates_vector[3] <- input$rate_3
  rates_vector[4] <- input$rate_4
  rates_vector[5] <- input$rate_5
  rates_vector[6] <- input$rate_6
  rates_vector[7] <- input$rate_7
  rates_vector[8] <- input$rate_8
  rates_vector[9] <- input$rate_9
  rates_vector[10] <- input$rate_10
  rates_vector[11] <- input$rate_11
  rates_vector[12] <- input$rate_12
  rates_vector[13] <- input$rate_13
  rates_vector[14] <- input$rate_14
  rates_vector[15] <- input$rate_15
  rates_vector[16] <- input$rate_16
  rates_vector[17] <- input$rate_17
  rates_vector[18] <- input$rate_18
  rates_vector[19] <- input$rate_19
  rates_vector[20] <- input$rate_20
  rates_vector[21] <- input$rate_21
  rates_vector[22] <- input$rate_22
  rates_vector[23] <- input$rate_23
  rates_vector[24] <- input$rate_24
  rates_vector[25] <- input$rate_25
  rates_vector[26] <- input$rate_26
  rates_vector[27] <- input$rate_27
  rates_vector[28] <- input$rate_28
  rates_vector[29] <- input$rate_29
  rates_vector[30] <- input$rate_30
  rates_vector[31] <- input$rate_31
  rates_vector[32] <- input$rate_32
  rates_vector[33] <- input$rate_33
  rates_vector[34] <- input$rate_34
  rates_vector[35] <- input$rate_35
  rates_vector[36] <- input$rate_36
  
  rates_vector <- rates_vector/100
  
  
  
  
  #cost matrix (shiny)
  cost_D <- input$cost_D
  cost_K <- input$cost_K
  cost_M <- input$cost_M
  cost_Pom <- input$cost_Pom
  cost_R <- input$cost_R
  cost_S <- input$cost_S
  cost_V <- input$cost_V
  cost_Ven <- input$cost_Ven
  cost_Ixa <- input$cost_Ixa
  cost_Isa <- input$cost_Isa
  cost_dd <- input$cost_dd
  cost_P <- input$cost_P
  cost_T <- input$cost_T
  cost_C <- input$cost_C
  
  box_dose_D <- 1
  box_dose_K <- 1.68
  box_dose_Pom <- 0.0476
  box_dose_R <- 0.1905
  box_dose_V <- 0.6686
  box_dose_Ven <- 0.2857
  box_dose_Ixa <- 0.3333
  box_dose_dd <- 0.04
  box_dose_P <- 0.216
  box_dose_T <- 0.1429
  box_dose_M <- 1
  box_dose_S <- 1
  box_dose_Isa <- 1
  box_dose_C <- 1
  
  number_of_doses_1 <- read.csv("no_admin_1.csv") #number of administration in the first cycle
  number_of_doses_2 <- read.csv("no_admin_2.csv") #number of adminsitration in the second cycle
  number_of_doses_3 <- read.csv("no_admin_3.csv") #number of adminsitration in the third cycle
  number_of_doses_4 <- read.csv("no_admin_4.csv") #number of adminsitration in the fourth cycle
  
  
  comp_costs_1 <- c(cost_D, 0, 0, 0, cost_D, cost_D, 0, 0, 0, 0, 0, 0, 0, cost_D, cost_D, cost_D, 0, 0, 0, 0, 0, 0, cost_D, 0)
  comp_costs_2 <- c(cost_V, cost_V, cost_V, cost_V, 0, cost_V, cost_K, cost_Ixa, cost_Ven, cost_Isa, cost_Pom, cost_S, cost_K, cost_V, cost_K, cost_Pom, cost_Isa, cost_V, cost_R, cost_V, 0, 0, 0, cost_V)
  comp_costs_3 <- c(cost_T, cost_R, cost_T, cost_C, cost_R, cost_M, cost_R, cost_R, cost_V, cost_K, cost_V, cost_V, 0, 0, 0, 0, cost_Pom, 0, 0, cost_M, cost_R, 0, cost_R, cost_R)
  comp_costs_4 <- c(cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_P, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, 0, 0, cost_P, cost_dd, 0, cost_dd, cost_dd)
  
  comp_costs_1 <- comp_costs_1*box_dose_D
  comp_costs_2 <- c(cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, 0, cost_V*box_dose_V, cost_K*box_dose_K, cost_Ixa*box_dose_Ixa, cost_Ven*box_dose_Ven, cost_Isa*box_dose_Isa, 
                    cost_Pom*box_dose_Pom, cost_S*box_dose_S, cost_K*box_dose_K, cost_V*box_dose_V, cost_K*box_dose_K, cost_Pom*box_dose_Pom, cost_Isa*box_dose_Isa, cost_V*box_dose_V, cost_R*box_dose_R, cost_V*box_dose_V, 0, 0, 0, cost_V*box_dose_V)
  comp_costs_3 <- c(cost_T*box_dose_T, cost_R*box_dose_R, cost_T*box_dose_T, cost_C*box_dose_C, cost_R*box_dose_R, cost_M*box_dose_M, cost_R*box_dose_R, cost_R*box_dose_R, 
                    cost_V*box_dose_V, cost_K*box_dose_K, cost_V*box_dose_V, cost_V*box_dose_V, 0, 0, 0, 0, cost_Pom*box_dose_Pom, 0, 0, cost_M*box_dose_M, cost_R*box_dose_R, 0, cost_R*box_dose_R, cost_R*box_dose_R)
  comp_costs_4 <- c(cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_P*box_dose_P, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 
                    cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 0, 0, cost_P*box_dose_P, cost_dd*box_dose_dd, 0, cost_dd*box_dose_dd, cost_dd*box_dose_dd)
  
  cost_cycle1 <- number_of_doses_1[1,]*comp_costs_1 + number_of_doses_1[2,]*comp_costs_2 + number_of_doses_1[3,]*comp_costs_3 + number_of_doses_1[3,]*comp_costs_3
  cost_cycle2 <- number_of_doses_2[1,]*comp_costs_1 + number_of_doses_2[2,]*comp_costs_2 + number_of_doses_2[3,]*comp_costs_3 + number_of_doses_2[3,]*comp_costs_3
  cost_cycle3 <- number_of_doses_3[1,]*comp_costs_1 + number_of_doses_3[2,]*comp_costs_2 + number_of_doses_3[3,]*comp_costs_3 + number_of_doses_3[3,]*comp_costs_3
  cost_cycle4 <- number_of_doses_4[1,]*comp_costs_1 + number_of_doses_4[2,]*comp_costs_2 + number_of_doses_4[3,]*comp_costs_3 + number_of_doses_4[3,]*comp_costs_3 
  cost_cycle5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0)
  cost_cycle6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
  cost_cycle7 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
  cost_cycle8 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
  
  cost_matrix <- rbind(cost_cycle1, cost_cycle2, cost_cycle3, cost_cycle4, cost_cycle5, cost_cycle6, cost_cycle7, cost_cycle8)
  
  cost_nodara <- cost_matrix[ ,-c(1,5,6,14,15,16,22,23)]
  cost_matrix$dummy <- rowMeans(cost_nodara) #amit a negyedik line-t?l kap, az a daratumumab n?lk?li ter?pi?k ?tlaga
  
  
  
  
  
  #---- I.1. Population ----
  set.seed(20220411)
  
  population <- data.frame(asct_elig=numeric(), venvd_elig=numeric(), cytrisk=numeric())
  
  #rbinom(N,1,rate)
  for (n in 1:N){
    rand_asct <- runif(1)
    rand_venvd <- runif(1) 
    rand_cytrisk <- runif(1)
    
    asct_elig <- ifelse(rand_asct<asct_rate, 1, 0)
    venvd_elig <- ifelse(rand_venvd<venvd_rate, 1, 0)
    cytrisk <- ifelse(rand_cytrisk<cytrisk_rate, 1, 0)
    
    population[n,] <- c(asct_elig,venvd_elig,cytrisk)
  }
  
  
  #patient seedek
  patient_seed_all<- sample(10000:99999, N*2, replace = FALSE, prob = NULL)
  patient_seed_split <- complete_ra(N = N, num_arms = 2)
  table_seed = data.table("seeds" = patient_seed_all, "split"= patient_seed_split)
  seed1 = table_seed[table_seed$split=='T1']
  seed2 = table_seed[table_seed$split=='T2']
  
  population$seed1 <- seed1$seeds
  population$seed2 <- seed2$seeds
  
  #cycle seedek (ebbol el?g egy darab)
  cycle_seed <- sample(100:999, number_of_cycles, replace= FALSE, prob = NULL)
  
  

  ##### III. Model start #########################################################
  
  
  #---- III.1. Patient level simulation ----
  per_patient <- data.frame(n=numeric(), asct_elig=numeric(), venvd_elig=numeric(), cytrisk=numeric(),
                            l1_tx_int=numeric(), asct2_tx_int=numeric(), maint_tx_int=numeric(), l2_tx_int=numeric(), l3_tx_int=numeric(),
                            date_prog_l1_int=numeric(), date_prog_asct2_int=numeric(), date_prog_maint_int=numeric(), date_prog_l2_int=numeric(), date_prog_l3_int=numeric(),
                            date_relapse_l1_int=numeric(), date_relapse_asct2_int=numeric(), date_relapse_maint_int=numeric(), date_relapse_l2_int=numeric(), date_relapse_l3_int=numeric(), date_death_int=numeric(), cost_int=numeric(), cost_int_disc=numeric(),
                            l1_tx_comp=numeric(), asct2_tx_comp=numeric(), maint_tx_comp=numeric(), l2_tx_comp=numeric(), l3_tx_comp=numeric(),
                            date_prog_l1_comp=numeric(), date_prog_asct2_comp=numeric(), date_prog_int_comp=numeric(), date_prog_l2_comp=numeric(), date_prog_l3_comp=numeric(),
                            date_relapse_l1_comp=numeric(), date_relapse_asct2_comp=numeric(), date_relapse_maint_comp=numeric(), date_relapse_l2_comp=numeric(), date_relapse_l3_comp=numeric(), date_death_comp=numeric(), cost_comp=numeric(), cost_comp_disc=numeric(),
                            date_adverse_l1_int=numeric(), date_adverse_asct2_int=numeric(), date_adverse_maint_int=numeric(), date_adverse_l2_int=numeric(), date_adverse_l3_int=numeric(),
                            date_adverse_l1_comp=numeric(), date_adverse_asct2_comp=numeric(), date_adverse_maint_comp=numeric(), date_adverse_l2_comp=numeric(), date_adverse_l3_comp=numeric())
  
  model_start<-Sys.time()
  
  for (n in 1:N) {
    
    #patient data per cycle
    per_cycle <- data.frame(cycle=0, 
                            treatment_int=0, tracker_int=0, cycle_track_int=0, current_line_int=0, toxic_int=1, cost_int=0,
                            treatment_comp=0, tracker_comp=0, cycle_track_comp=0, current_line_comp=0, toxic_comp=1, cost_comp=0,
                            l1_tx_int=0, l1_tx_comp=0, 
                            date_adverse_l1_comp=0, date_adverse_2asct_comp=0, date_adverse_maint_comp=0, date_adverse_l2_comp=0, date_adverse_l3_comp=0,
                            date_adverse_l1_int=0, date_adverse_2asct_int=0, date_adverse_maint_int=0, date_adverse_l2_int=0, date_adverse_l3_int=0,
                            response_int=0, response_comp=0, relapse_int=1, relapse_comp=1, 
                            date_death_int=0, date_prog_l1_int=0, date_prog_2asct_int=0, date_prog_l2_int=0, date_prog_l3_int=0, date_relapse_l1_int=0, date_relapse_2asct_int=0, date_relapse_l2_int=0, date_relapse_l3_int=0,
                            date_death_comp=0, date_prog_l1_comp=0, date_prog_2asct_comp=0, date_prog_l2_comp=0, date_prog_l3_comp=0, date_relapse_l1_comp=0, date_relapse_2asct_comp=0, date_relapse_l2_comp=0, date_relapse_l3_comp=0,
                            asct2_int=0, asct2_comp=0 ) 
    
    #Baseline Patient data
    asct_elig <- population[n,1]
    venvd_elig <- population[n,2]
    cytrisk <- population[n,3]
    patient_seed_int <- population[n, 4]
    patient_seed_comp <- population[n, 5]
    
    #==== III.1.1. Cycle level simulation====
    for (i in 1:number_of_cycles) {
      
      seed_int <- as.integer(paste0(patient_seed_int, cycle_seed[i]))
      seed_comp <- as.integer(paste0(patient_seed_comp, cycle_seed[i]))
      seed_int_2 <- as.integer(paste0(patient_seed_int, cycle_seed[i],2))
      seed_comp_2 <- as.integer(paste0(patient_seed_comp, cycle_seed[i],2))
      
      #Treatment decision algoritm
      treatment_vector <- treatment(asct_elig, venvd_elig, cytrisk, i, 
                                    response_int = per_cycle[i,26], response_comp = per_cycle[i,27], 
                                    treatment_int = per_cycle[i,2], treatment_comp = per_cycle[i,8], 
                                    prev_line_int = per_cycle[i,5], prev_line_comp = per_cycle[i,11], 
                                    tracker_int= per_cycle[i,3], tracker_comp = per_cycle[i,9], 
                                    cycle_track_int = per_cycle[i,4], cycle_track_comp = per_cycle[i,10], 
                                    toxic_int= per_cycle[i,6], toxic_comp= per_cycle[i,12],
                                    relapse_int= per_cycle[i,28], relapse_comp= per_cycle[i,29], 
                                    l1_tx_int= per_cycle[i,14], l1_tx_comp= per_cycle[i,15], 
                                    date_adverse_l1_comp=per_cycle[i,16], date_adverse_2asct_comp=per_cycle[i,17], date_adverse_maint_comp=per_cycle[i,18],date_adverse_l2_comp=per_cycle[i,19],date_adverse_l3_comp=per_cycle[i,20],
                                    date_adverse_l1_int=per_cycle[i,21],date_adverse_2asct_int=per_cycle[i,22],date_adverse_maint_int=per_cycle[i,23],date_adverse_l2_int=per_cycle[i,24],date_adverse_l3_int=per_cycle[i,25],
                                    asct2_int=per_cycle[i,48], asct2_comp=per_cycle[i,49], 
                                    seed_int, seed_comp, 
                                    max_cycles_vector, cost_matrix, p_adverse_tx_vector, p_cr_vector, p_vgpr_vector, p_pr_vector, p_mr_vector, p_sd_vector, p_pd_vector, maint_better, scenario, dara2l,  rates_vector, dara_retr)
      
      #translate current_line from treatment to current line used in outcome
      current_line_int <- ifelse (
                                    #condition
                                    treatment_vector[4]==1 | treatment_vector[4]==2 |treatment_vector[4]==5,
                                    #if true
                                    1,
                                    #if false
                                    ifelse (
                                      #condition
                                      treatment_vector[4]==3,
                                      #if true
                                      2,
                                      #if false
                                      3)
                                  )
      
      current_line_comp <- ifelse (
                                    #condition
                                    treatment_vector[11]==1 | treatment_vector[11]==2 |treatment_vector[11]==5,
                                    #if true
                                    1,
                                    #if false
                                    ifelse (
                                      #condition
                                      treatment_vector[11]==3,
                                      #if true
                                      2,
                                      #if false
                                      3)
                                    )
      
      outcome_vector <- outcome(i, current_line_int, current_line_comp, treatment_vector_line_int=treatment_vector[4], treatment_vector_line_comp=treatment_vector[11], 
                                response_int=treatment_vector[5], response_comp=treatment_vector[12], cytrisk,
                                pfs_matrix, os_matrix, 
                                seed_int_2, seed_comp_2)
      
      per_cycle[i+1,] <- c(i, treatment_vector[-c(5,12,27,28)], outcome_vector, treatment_vector[c(27,28)])
    }#cycle simulation end
    
    #discounting
    per_cycle <- per_cycle %>% mutate(cost_int_disc = cost_int/((1+disc_cost)^(floor(cycle*0.25))),cost_comp_disc = cost_comp/((1+disc_cost)^(floor(cycle*0.25)))) 
    
    #takes per_patient table variables one_by_one
    l1_tx_int <- max(per_cycle$l1_tx_int)
    
    asct2_tx_int <- ifelse(
      #condition
      nrow(filter(per_cycle, current_line_int==5))==0,
      #if true
      0,
      #if false
      filter(per_cycle, current_line_int==5)$treatment_int[1])
    
    maint_tx_int <- ifelse(
      #condition
      nrow(filter(per_cycle, current_line_int==2))==0,
      #if true
      0,
      #if false
      filter(per_cycle, current_line_int==2)$treatment_int[1])
  
    l2_tx_int <- ifelse(
      #condition
      nrow(filter(per_cycle, current_line_int==3))==0,
      #if true
      0,
      #if false
      filter(per_cycle, current_line_int==3)$treatment_int[1])
    
    l3_tx_int <- ifelse(
                        #condition
                        nrow(filter(per_cycle, current_line_int==4))==0,
                        #if true
                        0,
                        #if false
                        filter(per_cycle, current_line_int==4)$treatment_int[1])
    
    date_prog_l1_int <- ifelse(
                              #condition
                              nrow(filter(per_cycle, current_line_int==1))==0,
                              #if true
                              0,
                              #if false
                              max(filter(per_cycle, current_line_int==1)$date_prog_l1_int))
    
    date_prog_asct2_int <- ifelse(
                            #condition
                            nrow(filter(per_cycle, current_line_int==5))==0,
                            #if ture
                            0,
                            #if false
                            max(filter(per_cycle, current_line_int==5)$date_prog_2asct_int))
    
    date_prog_maint_int <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==2))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==2)$date_prog_l1_int))
    
    date_prog_l2_int <- ifelse(
                              #condition
                              nrow(filter(per_cycle, current_line_int==3))==0,
                              #if true
                              0,
                              #if false
                              max(filter(per_cycle, current_line_int==3)$date_prog_l2_int))
    
    date_prog_l3_int <- ifelse(
                              #condition
                              nrow(filter(per_cycle, current_line_int==4))==0,
                              #if true
                              0,
                              #if false
                              max(filter(per_cycle, current_line_int==4)$date_prog_l3_int))
    
    date_relapse_l1_int <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==1))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==1)$date_relapse_l1_int))
    
    date_relapse_asct2_int <- ifelse(
                                    #condition
                                    nrow(filter(per_cycle, current_line_int==5))==0,
                                    #if ture
                                    0,
                                    #if false
                                    max(filter(per_cycle, current_line_int==5)$date_relapse_2asct_int))
    
    date_relapse_maint_int <- ifelse(
                                    #condition
                                    nrow(filter(per_cycle, current_line_int==2))==0,
                                    #if true
                                    0,
                                    #if false
                                    max(filter(per_cycle, current_line_int==2)$date_relapse_l1_int))
  
    date_relapse_l2_int<- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==3))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==3)$date_relapse_l2_int))
    
    date_relapse_l3_int <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==4))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==4)$date_relapse_l3_int))
    
    date_death_int <- max(per_cycle$date_death_int)
    cost_int <- sum(per_cycle$cost_int)
    cost_int_disc <- sum(per_cycle$cost_int_disc)
    
    #Adverse Event date int
    date_adverse_l1_int <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==1))==0,
                                  #if ture
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==1)$date_adverse_l1_int))
    
    date_adverse_asct2_int <- ifelse(
                                    #condition
                                    nrow(filter(per_cycle, current_line_int==5))==0,
                                    #if true
                                    0,
                                    #if false
                                    max(filter(per_cycle, current_line_int==5)$date_adverse_2asct_int))
    
    date_adverse_maint_int <- ifelse(
                                    #condition
                                    nrow(filter(per_cycle, current_line_int==2))==0,
                                    #if true
                                    0,
                                    #if false
                                    filter(per_cycle, current_line_int==2)$date_adverse_maint_int[1])
    
    date_adverse_l2_int<- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==3))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==3)$date_adverse_l2_int))
    
    date_adverse_l3_int <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_int==4))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_int==4)$date_adverse_l3_int))
    
    l1_tx_comp <- max(unique(per_cycle$l1_tx_comp))
    
    asct2_tx_comp <- ifelse(
                            #condition
                            nrow(filter(per_cycle, current_line_comp==5))==0,
                            #if true
                            0,
                            #if false
                            filter(per_cycle, current_line_comp==5)$treatment_comp[1])
    
    maint_tx_comp <- ifelse(
                            #condition
                            nrow(filter(per_cycle, current_line_comp==2))==0,
                            #if true
                            0,
                            #if false
                            filter(per_cycle, current_line_comp==2)$treatment_comp[1])
    
    l2_tx_comp <- ifelse(
                          #condition
                          nrow(filter(per_cycle, current_line_comp==3))==0,
                          #if true
                          0,
                          #if false
                          filter(per_cycle, current_line_comp==3)$treatment_comp[1])
    
    l3_tx_comp <- ifelse(
                        #condition
                        nrow(filter(per_cycle, current_line_comp==4))==0,
                        #if true
                        0,
                        #if false
                        filter(per_cycle, current_line_comp==4)$treatment_comp[1])
    
    date_prog_l1_comp <- ifelse(
                                #condition
                                nrow(filter(per_cycle, current_line_comp==1))==0,
                                #if true
                                0,
                                #if false
                                max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp))
    
    date_prog_asct2_comp <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==5))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==5)$date_prog_2asct_comp))
    
    date_prog_maint_comp <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==2))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp))
    
    date_prog_l2_comp <- ifelse(
                                #condition
                                nrow(filter(per_cycle, current_line_comp==3))==0,
                                #if true
                                0,
                                #if false
                                max(filter(per_cycle, current_line_comp==3)$date_prog_l2_comp))
    
    date_prog_l3_comp <- ifelse(
                                #condition
                                nrow(filter(per_cycle, current_line_comp==4))==0,
                                #if true
                                0,
                                #if true
                                max(filter(per_cycle, current_line_comp==4)$date_prog_l3_comp))
    
    date_relapse_l1_comp <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==1))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==1)$date_relapse_l1_comp))
    
    date_relapse_asct2_comp <- ifelse(
                                      #condition
                                      nrow(filter(per_cycle, current_line_comp==5))==0,
                                      #if true
                                      0,
                                      #if false
                                      max(filter(per_cycle, current_line_comp==5)$date_relapse_2asct_comp))
    
    date_relapse_maint_comp <- ifelse(
                                      #condition
                                      nrow(filter(per_cycle, current_line_comp==2))==0,
                                      #if true
                                      0,
                                      #if false
                                      max(filter(per_cycle, current_line_comp==2)$date_relapse_l1_comp))
    
    date_relapse_l2_comp<- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==3))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==3)$date_relapse_l2_comp))
    
    date_relapse_l3_comp <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==4))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==4)$date_relapse_l3_comp))
    
    date_death_comp <- max(per_cycle$date_death_comp)
    cost_comp <- sum(per_cycle$cost_comp)
    cost_comp_disc <- sum(per_cycle$cost_comp_disc)
    
    #Adverse Event date comp
    date_adverse_l1_comp <- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==1))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==1)$date_adverse_l1_comp))
    
    date_adverse_asct2_comp <- ifelse(
                                      #condition
                                      nrow(filter(per_cycle, current_line_comp==5))==0,
                                      #if true
                                      0,
                                      #if false
                                      max(filter(per_cycle, current_line_comp==5)$date_adverse_2asct_comp))
    
    date_adverse_maint_comp <- ifelse(
                                      #condition
                                      nrow(filter(per_cycle, current_line_comp==2))==0,
                                      #if true
                                      0,
                                      #if false
                                      filter(per_cycle, current_line_comp==2)$date_adverse_maint_comp[1])
    
    date_adverse_l2_comp<- ifelse(
                                  #condition
                                  nrow(filter(per_cycle, current_line_comp==3))==0,
                                  #if true
                                  0,
                                  #if false
                                  max(filter(per_cycle, current_line_comp==3)$date_adverse_l2_comp))
    
    date_adverse_l3_comp <- ifelse(
                                    #condition
                                    nrow(filter(per_cycle, current_line_comp==4))==0,
                                    #if true
                                    0,
                                    #if false
                                    max(filter(per_cycle, current_line_comp==4)$date_adverse_l3_comp))
    
    
    # a per cycle-bol k?sz?t egy per patient sort, ezt m?g ki k?ne tal?lnom.
    per_patient[n, ] <- c(n, population[n,1],population[n,2],population[n,3],
                          l1_tx_int, asct2_tx_int, maint_tx_int, l2_tx_int, l3_tx_int,
                          date_prog_l1_int, date_prog_asct2_int, date_prog_maint_int, date_prog_l2_int, date_prog_l3_int,
                          date_relapse_l1_int, date_relapse_asct2_int, date_relapse_maint_int, date_relapse_l2_int, date_relapse_l3_int, date_death_int, cost_int, cost_int_disc,
                          l1_tx_comp, asct2_tx_comp, maint_tx_comp, l2_tx_comp, l3_tx_comp,
                          date_prog_l1_comp, date_prog_asct2_comp, date_prog_maint_comp, date_prog_l2_comp, date_prog_l3_comp,
                          date_relapse_l1_comp, date_relapse_asct2_comp, date_relapse_maint_comp, date_relapse_l2_comp, date_relapse_l3_comp, date_death_comp, cost_comp, cost_comp_disc,
                          date_adverse_l1_int, date_adverse_asct2_int, date_adverse_maint_int, date_adverse_l2_int, date_adverse_l3_int,
                          date_adverse_l1_comp, date_adverse_asct2_comp, date_adverse_maint_comp, date_adverse_l2_comp, date_adverse_l3_comp
    )
  }
  
  model_finish <- Sys.time()
  runtime <- model_finish - model_start
  
  ##### IV. Outputs ##################################################################
  
  LY_int <- (sum(per_patient$date_death_int)*0.25)/N
  LY_comp <- (sum(per_patient$date_death_comp)*0.25)/N
  delta_LY <- LY_int - LY_comp
  
  total_cost_int <- (sum(per_patient$cost_int_disc))/N
  total_cost_comp <- (sum(per_patient$cost_comp_disc))/N
  delta_cost <- total_cost_int-total_cost_comp
  
  ICER <- delta_cost/delta_LY
  
  
  
  
  
  
  ### Output for plot
  
  treatment <- NA
  
  treatment <- if (input$scenario == 1) {
    c("Daratumumab in 1L & 3L", "Daratumumab only in 2L/3L")
    }
    else if (input$scenario == 2) {
    c("Daratumumab in all 1L & 3L", "No daratumumab")
    }
    else {
    c("Daratumumab only in 2L/3L", "No daratumumab")
    }
  
  life_years <- rbind(LY_int, LY_comp)
  cost <- rbind(total_cost_int, total_cost_comp)
  df <- data.frame(cbind(treatment, life_years, cost))
  colnames(df) <- c("treatment", "life_years", "cost")
  
  df2 <- df %>%
    pivot_longer(cols=c("life_years", "cost"), names_to="variable", values_to="values")
  
  df2$variable <- factor(df2$variable)
  levels(df2$variable) <- c("Cost", "Life years")
  df2$values <- as.numeric(as.character(df2$values))
  
 
  
  
  ### Output for ICER table
  
  df_res_table <- data.frame( #create dataframe
    
    Option = c("Intervention", "Comparator"),
    
    LifeYears  = c(LY_int, LY_comp),
    d_LifeYears = c(LY_int - LY_comp, NA),
    Cost  = c(total_cost_int, total_cost_comp),
    d_Cost = c(total_cost_int - total_cost_comp, NA),
    
    ICER = c(ICER, NA)
    
  ) #close data—frame
  
  #round the data—frame to two digits
  df_res_table[,2:6] = format(round(df_res_table[,2:6],digits = 2), big.mark=" ")
  
  
  
  
 
  ### Bar chart
  
  output$barChart <- renderPlot({
    
    #df2 <- read.csv2(here::here("wd", "df2.csv"))
  
    ggplot(df2) +
          geom_col(aes(x=treatment, y=round(values,2), fill=treatment), alpha=0.9, width=0.9) +
          scale_fill_manual(values = c("dodgerblue2", "coral2")) +
          facet_wrap(~variable, scales="free") +
          scale_y_continuous(labels = function(x) format(x, big.mark = " ")) +
          theme_economist_white() +
          theme(strip.text = element_text(size=16, face="bold")) +
          theme(legend.position = "none") +
          theme(axis.title = element_blank()) +
          theme(axis.text = element_text(size=12),
                axis.text.x = element_text(face="bold"))
    
    }) #RenderPlot ends
  

  
  
  
  ### ICER table
  
   output$icer_table <- renderTable({
    
    #LY_int <- read.csv2(here::here("wd", "LY_int.csv"))
    #LY_comp <- read.csv2(here::here("wd", "LY_comp.csv"))
    #total_cost_int <- read.csv2(here::here("wd", "total_cost_int.csv"))
    #total_cost_comp <- read.csv2(here::here("wd", "total_cost_comp.csv"))
    #ICER <- read.csv2(here::here("wd", "icer.csv"))
    
    #print the results table
    df_res_table
    
  }) #RenderTable ends
   

write.csv2(per_patient, here::here("wd", "per_patient.csv"))

}) #Observe event ends (action button)
  
  
 
  

  
### Save output
  
observeEvent(input$save_csv,
             ignoreNULL = T, {
               per_patient <- read.csv2(here::here("wd", "per_patient.csv"))
               write.csv2(per_patient, here::here("results", "mm_model_patient_table.csv"))
             })







### Run DSA

observeEvent(input$run_dsa,
             ignoreNULL = T, {
               
               output$tornadoPlot <- renderPlot({
               
               ##### I.A. Source treatment and outcome functions ###############################
               
               source(here::here("model_functions.R"))
               
               
                 options(scipen=999)
                 
                 ##### Load the original inputs ################################################
                 ###############################################################################
                 
                 input_0 <- read.csv(here::here("input_1.csv"), header=FALSE, fileEncoding="UTF-8-BOM")
                 input_0_frame <- data.frame(input_0)
                 
                 input1 <- t(input_0_frame[-1])
                 colnames(input1) <- input_0_frame[, 1]
                 input_final <- data.table(input1)
                 
                 #vektorokba gyujt?tt inputok
                 max_cycles_vector <- c(100, input_final$max_cycles_vector[!is.na(input_final$max_cycles_vector)])
                 p_adverse_tx_vector <- input_final$p_adverse_tx_vector[!is.na(input_final$p_adverse_tx_vector)]
                 p_cr_vector <- input_final$p_cr_vector[!is.na(input_final$p_cr_vector)]
                 p_vgpr_vector <- input_final$p_vgpr_vector[!is.na(input_final$p_vgpr_vector)]
                 p_pr_vector <- input_final$p_pr_vector[!is.na(input_final$p_pr_vector)]
                 p_mr_vector <- input_final$p_mr_vector[!is.na(input_final$p_mr_vector)]
                 p_sd_vector <- input_final$p_sd_vector[!is.na(input_final$p_sd_vector)]
                 p_pd_vector <- input_final$p_pd_vector[!is.na(input_final$p_pd_vector)]
                 
                 #treatment rates (shiny)
                 rates_vector <- rep(NA, 36)
                 rates_vector[1] <- input$rate_1
                 rates_vector[2] <- input$rate_2
                 rates_vector[3] <- input$rate_3
                 rates_vector[4] <- input$rate_4
                 rates_vector[5] <- input$rate_5
                 rates_vector[6] <- input$rate_6
                 rates_vector[7] <- input$rate_7
                 rates_vector[8] <- input$rate_8
                 rates_vector[9] <- input$rate_9
                 rates_vector[10] <- input$rate_10
                 rates_vector[11] <- input$rate_11
                 rates_vector[12] <- input$rate_12
                 rates_vector[13] <- input$rate_13
                 rates_vector[14] <- input$rate_14
                 rates_vector[15] <- input$rate_15
                 rates_vector[16] <- input$rate_16
                 rates_vector[17] <- input$rate_17
                 rates_vector[18] <- input$rate_18
                 rates_vector[19] <- input$rate_19
                 rates_vector[20] <- input$rate_20
                 rates_vector[21] <- input$rate_21
                 rates_vector[22] <- input$rate_22
                 rates_vector[23] <- input$rate_23
                 rates_vector[24] <- input$rate_24
                 rates_vector[25] <- input$rate_25
                 rates_vector[26] <- input$rate_26
                 rates_vector[27] <- input$rate_27
                 rates_vector[28] <- input$rate_28
                 rates_vector[29] <- input$rate_29
                 rates_vector[30] <- input$rate_30
                 rates_vector[31] <- input$rate_31
                 rates_vector[32] <- input$rate_32
                 rates_vector[33] <- input$rate_33
                 rates_vector[34] <- input$rate_34
                 rates_vector[35] <- input$rate_35
                 rates_vector[36] <- input$rate_36
                 
                 rates_vector <- rates_vector/100
                 
                 
                 #m?trixok is vannak
                 pfs_matrix <- cbind(input_final$l1_pfs_vector[!is.na(input_final$l1_pfs_vector)], input_final$l2_pfs_vector[!is.na(input_final$l2_pfs_vector)], input_final$l3_pfs_vector[!is.na(input_final$l3_pfs_vector)])
                 os_matrix <- cbind(input_final$l1_os_vector[!is.na(input_final$l1_os_vector)], input_final$l2_os_vector[!is.na(input_final$l2_os_vector)], input_final$l3_os_vector[!is.na(input_final$l3_os_vector)])
                 
                 
                 #egyeb shiny inputok
                 dara2l <- input$dara2l
                 scenario <- input$scenario
                 asct_rate <- input$asct_rate/100
                 venvd_rate <- input$venvd_rate/100
                 cytrisk_rate <- input$cytrisk_rate/100
                 N <- input$N
                 disc_cost <- input$disc_cost/100
                 dara_retr <- input$dara_retreat
                 
                 
                 #marad?k
                 number_of_cycles <- input_final$number_of_cycles[!is.na(input_final$number_of_cycles)]
                 maint_better <- input_final$maint_better[!is.na(input_final$maint_better)]
                 
                 #cost matrix (shiny)
                 cost_D <- input$cost_D
                 cost_K <- input$cost_K
                 cost_M <- input$cost_M
                 cost_Pom <- input$cost_Pom
                 cost_R <- input$cost_R
                 cost_S <- input$cost_S
                 cost_V <- input$cost_V
                 cost_Ven <- input$cost_Ven
                 cost_Ixa <- input$cost_Ixa
                 cost_Isa <- input$cost_Isa
                 cost_dd <- input$cost_dd
                 cost_P <- input$cost_P
                 cost_T <- input$cost_T
                 cost_C <- input$cost_C
                 
                 box_dose_D <- 1
                 box_dose_K <- 1.68
                 box_dose_Pom <- 0.0476
                 box_dose_R <- 0.1905
                 box_dose_V <- 0.6686
                 box_dose_Ven <- 0.2857
                 box_dose_Ixa <- 0.3333
                 box_dose_dd <- 0.04
                 box_dose_P <- 0.216
                 box_dose_T <- 0.1429
                 box_dose_M <- 1
                 box_dose_S <- 1
                 box_dose_Isa <- 1
                 box_dose_C <- 1
                 
                 number_of_doses_1 <- read.csv("no_admin_1.csv") #number of administration in the first cycle
                 number_of_doses_2 <- read.csv("no_admin_2.csv") #number of adminsitration in the second cycle
                 number_of_doses_3 <- read.csv("no_admin_3.csv") #number of adminsitration in the third cycle
                 number_of_doses_4 <- read.csv("no_admin_4.csv") #number of adminsitration in the fourth cycle
                 
                 
                 comp_costs_1 <- c(cost_D, 0, 0, 0, cost_D, cost_D, 0, 0, 0, 0, 0, 0, 0, cost_D, cost_D, cost_D, 0, 0, 0, 0, 0, 0, cost_D, 0)
                 comp_costs_2 <- c(cost_V, cost_V, cost_V, cost_V, 0, cost_V, cost_K, cost_Ixa, cost_Ven, cost_Isa, cost_Pom, cost_S, cost_K, cost_V, cost_K, cost_Pom, cost_Isa, cost_V, cost_R, cost_V, 0, 0, 0, cost_V)
                 comp_costs_3 <- c(cost_T, cost_R, cost_T, cost_C, cost_R, cost_M, cost_R, cost_R, cost_V, cost_K, cost_V, cost_V, 0, 0, 0, 0, cost_Pom, 0, 0, cost_M, cost_R, 0, cost_R, cost_R)
                 comp_costs_4 <- c(cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_P, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, 0, 0, cost_P, cost_dd, 0, cost_dd, cost_dd)
                 
                 comp_costs_1 <- comp_costs_1*box_dose_D
                 comp_costs_2 <- c(cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, 0, cost_V*box_dose_V, cost_K*box_dose_K, cost_Ixa*box_dose_Ixa, cost_Ven*box_dose_Ven, cost_Isa*box_dose_Isa, 
                                   cost_Pom*box_dose_Pom, cost_S*box_dose_S, cost_K*box_dose_K, cost_V*box_dose_V, cost_K*box_dose_K, cost_Pom*box_dose_Pom, cost_Isa*box_dose_Isa, cost_V*box_dose_V, cost_R*box_dose_R, cost_V*box_dose_V, 0, 0, 0, cost_V*box_dose_V)
                 comp_costs_3 <- c(cost_T*box_dose_T, cost_R*box_dose_R, cost_T*box_dose_T, cost_C*box_dose_C, cost_R*box_dose_R, cost_M*box_dose_M, cost_R*box_dose_R, cost_R*box_dose_R, 
                                   cost_V*box_dose_V, cost_K*box_dose_K, cost_V*box_dose_V, cost_V*box_dose_V, 0, 0, 0, 0, cost_Pom*box_dose_Pom, 0, 0, cost_M*box_dose_M, cost_R*box_dose_R, 0, cost_R*box_dose_R, cost_R*box_dose_R)
                 comp_costs_4 <- c(cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_P*box_dose_P, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 
                                   cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 0, 0, cost_P*box_dose_P, cost_dd*box_dose_dd, 0, cost_dd*box_dose_dd, cost_dd*box_dose_dd)
                 
                 
                 ###############################################################################
                 ###### MAke the lower and upper bounds ########################################
                 ###############################################################################
                 
                 matrix_replace <- function(line, multiplier, matrix){
                   result <- matrix
                   result[line,] <- matrix[line,]*multiplier
                   return(result)
                 }
                 
                 vector_replace <- function(line, multiplier, vector){
                   
                 }
                 
                 dsa_rate <- input$dsa_rate/100 #Shiny input
                 
                 #selectable variables for DSA, Shiny tick-boxes
                 max_cycles_dsa <- ifelse("max_cycles_dsa" %in% input$dsa_vars, 1, 0)
                 maint_better_dsa <- ifelse("maint_better_dsa" %in% input$dsa_vars, 1, 0)
                 adverse_dara_dsa <- ifelse("adverse_dara_dsa" %in% input$dsa_vars, 1, 0)
                 adverse_other_dsa <- ifelse("adverse_other_dsa" %in% input$dsa_vars, 1, 0)
                 cr_dara_dsa <- ifelse("cr_dara_dsa" %in% input$dsa_vars, 1, 0)
                 cr_other_dsa <- ifelse("cr_other_dsa" %in% input$dsa_vars, 1, 0)
                 vgpr_dara_dsa <- ifelse("vgpr_dara_dsa" %in% input$dsa_vars, 1, 0)
                 vgpr_other_dsa <- ifelse("vgpr_other_dsa" %in% input$dsa_vars, 1, 0)
                 pr_dara_dsa <- ifelse("pr_dara_dsa" %in% input$dsa_vars, 1, 0)
                 pr_other_dsa <- ifelse("pr_other_dsa" %in% input$dsa_vars, 1, 0)
                 mr_dara_dsa <- ifelse("mr_dara_dsa" %in% input$dsa_vars, 1, 0)
                 mr_other_dsa <- ifelse("mr_other_dsa" %in% input$dsa_vars, 1, 0)
                 sd_dara_dsa <- ifelse("sd_dara_dsa" %in% input$dsa_vars, 1, 0)
                 sd_other_dsa <- ifelse("sd_other_dsa" %in% input$dsa_vars, 1, 0)
                 pd_dara_dsa <- ifelse("pd_dara_dsa" %in% input$dsa_vars, 1, 0)
                 pd_other_dsa <- ifelse("pd_other_dsa" %in% input$dsa_vars, 1, 0)
                 cr_pfs_dsa <- ifelse("cr_pfs_dsa" %in% input$dsa_vars, 1, 0)
                 vgpr_pfs_dsa <- ifelse("vgpr_pfs_dsa" %in% input$dsa_vars, 1, 0)
                 pr_pfs_dsa <- ifelse("pr_pfs_dsa" %in% input$dsa_vars, 1, 0)
                 mr_pfs_dsa <- ifelse("mr_pfs_dsa" %in% input$dsa_vars, 1, 0)
                 sd_pfs_dsa <- ifelse("sd_pfs_dsa" %in% input$dsa_vars, 1, 0)
                 pd_pfs_dsa <- ifelse("pd_pfs_dsa" %in% input$dsa_vars, 1, 0)
                 cr_os_dsa <- ifelse("cr_os_dsa" %in% input$dsa_vars, 1, 0)
                 vgpr_os_dsa <- ifelse("vgpr_os_dsa" %in% input$dsa_vars, 1, 0)
                 pr_os_dsa <- ifelse("pr_os_dsa" %in% input$dsa_vars, 1, 0)
                 mr_os_dsa <- ifelse("mr_os_dsa" %in% input$dsa_vars, 1, 0)
                 sd_os_dsa <- ifelse("sd_os_dsa" %in% input$dsa_vars, 1, 0)
                 pd_os_dsa <- ifelse("pd_os_dsa" %in% input$dsa_vars, 1, 0)
                 price_D <- ifelse("costs_dara_dsa" %in% input$dsa_vars, 1, 0)
                 price_other <- ifelse("costs_other_dsa" %in% input$dsa_vars, 1, 0)
                 
                 dsa_vector <- c(max_cycles_dsa, 
                                 maint_better_dsa, 
                                 adverse_dara_dsa, 
                                 adverse_other_dsa, 
                                 cr_dara_dsa, 
                                 cr_other_dsa, 
                                 vgpr_dara_dsa, 
                                 vgpr_other_dsa, 
                                 pr_dara_dsa, 
                                 pr_other_dsa,
                                 mr_dara_dsa,
                                 mr_other_dsa,
                                 sd_dara_dsa, 
                                 sd_other_dsa,  
                                 pd_dara_dsa, 
                                 pd_other_dsa, 
                                 cr_pfs_dsa, 
                                 vgpr_pfs_dsa, 
                                 pr_pfs_dsa, 
                                 mr_pfs_dsa, 
                                 sd_pfs_dsa, 
                                 pd_pfs_dsa, 
                                 cr_os_dsa, 
                                 vgpr_os_dsa, 
                                 pr_os_dsa, 
                                 mr_os_dsa, 
                                 sd_os_dsa, 
                                 pd_os_dsa, 
                                 price_D, 
                                 price_other)
                 
                 dsa_vector_2 <- which(dsa_vector==1)
                 
                 no_variables <- length(dsa_vector[dsa_vector==1])
                 
                 per_variable_dsa <- data.frame(number=dsa_vector_2, lower_dsa = NA, upper_dsa = NA)
                 
                 var_list <- c("Maximum length of treatment cycles",
                               "The extent of maintenance \nimproving treatment response",
                               "Adverse event \nrate of daratumumab treatments",
                               "Adverse event \nrate of other treatments",
                               "Complete response (CR) \nrate of daratumumab treatments",
                               "Complete response (CR) \nrate of other treatments",
                               "Very good partial response (VGPR) \nrate of daratumumab treatments",
                               "Very good partial response (VGPR) \nrate of other treatments",
                               "Partial response (PR) \nrate of daratumumab treatments",
                               "Partial response (PR) \nrate of other treatments",
                               "Minimal response (MR) \nrate of daratumumab treatments",
                               "Minimal response (MR) \nrate of other treatments",
                               "Stable disease (SD) \nrate of daratumumab treatments",
                               "Stable disease (SD) \nrate of other treatments",
                               "Progressive disease (PD) \nrate of daratumumab treatments",
                               "Progressive disease (PD) \nrate of other treatments",
                               "Probability of progression per quarter \nat response CR",
                               "Probability of progression per quarter \nat response VGPR",
                               "Probability of progression per quarter \nat response PR",
                               "Probability of progression per quarter \nat response MR",
                               "Probability of progression per quarter \nat response SD",
                               "Probability of progression per quarter \nat response PD",
                               "Probability of death per quarter \nat response CR",
                               "Probability of death per quarter \nat response VGPR",
                               "Probability of death per quarter \nat response PR",
                               "Probability of death per quarter \nat response MR",
                               "Probability of death per quarter \nat response SD",
                               "Probability of death per quarter \nat response PD",
                               "Cost of daratumumab treatments",
                               "Cost of other treatments")
                 
                 
                 var_list_dsa <- var_list[dsa_vector_2]
                 
                 #All inputs - lower bound =========================================
                 max_cycles_dsa_lower <- floor(max_cycles_vector*(1-dsa_rate)) #always integer
                 maint_better_dsa_lower <- maint_better * (1-dsa_rate)
                 comp_costs_1_lower <- comp_costs_1 * (1-dsa_rate)
                 comp_costs_2_lower <- comp_costs_2 * (1-dsa_rate)
                 comp_costs_3_lower <- comp_costs_3 * (1-dsa_rate)
                 comp_costs_4_lower <- comp_costs_4 * (1-dsa_rate)
                 adverse_dara_dsa_lower <- replace(p_adverse_tx_vector, c(1,5,6,14,15,16,23),p_adverse_tx_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 adverse_other_dsa_lower <- replace(p_adverse_tx_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_adverse_tx_vector[c(1,5,6,14,15,16,23)])
                 cr_dara_dsa_lower <- replace(p_cr_vector, c(1,5,6,14,15,16,23),p_cr_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 cr_other_dsa_lower <- replace(p_cr_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_cr_vector[c(1,5,6,14,15,16,23)])
                 vgpr_dara_dsa_lower <- replace(p_vgpr_vector, c(1,5,6,14,15,16,23),p_vgpr_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 vgpr_other_dsa_lower <- replace(p_vgpr_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_vgpr_vector[c(1,5,6,14,15,16,23)])
                 pr_dara_dsa_lower <- replace(p_pr_vector, c(1,5,6,14,15,16,23),p_pr_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 pr_other_dsa_lower <- replace(p_pr_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_pr_vector[c(1,5,6,14,15,16,23)])
                 mr_dara_dsa_lower <- replace(p_mr_vector, c(1,5,6,14,15,16,23),p_mr_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 mr_other_dsa_lower <- replace(p_mr_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_mr_vector[c(1,5,6,14,15,16,23)])
                 sd_dara_dsa_lower <- replace(p_sd_vector, c(1,5,6,14,15,16,23),p_sd_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 sd_other_dsa_lower <- replace(p_sd_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_sd_vector[c(1,5,6,14,15,16,23)])
                 pd_dara_dsa_lower <- replace(p_pd_vector, c(1,5,6,14,15,16,23),p_pd_vector[c(1,5,6,14,15,16,23)]*(1-dsa_rate))
                 pd_other_dsa_lower <- replace(p_pd_vector*(1-dsa_rate), c(1,5,6,14,15,16,23),p_pd_vector[c(1,5,6,14,15,16,23)])
                 cr_pfs_dsa_lower <- matrix_replace(1, 1-dsa_rate, pfs_matrix)
                 vgpr_pfs_dsa_lower <- matrix_replace(2, 1-dsa_rate, pfs_matrix)
                 pr_pfs_dsa_lower <- matrix_replace(3, 1-dsa_rate, pfs_matrix)
                 mr_pfs_dsa_lower <- matrix_replace(4, 1-dsa_rate, pfs_matrix)
                 sd_pfs_dsa_lower <- matrix_replace(5, 1-dsa_rate, pfs_matrix)
                 pd_pfs_dsa_lower <- matrix_replace(6, 1-dsa_rate, pfs_matrix)
                 cr_os_dsa_lower <- matrix_replace(1, 1-dsa_rate, os_matrix)
                 vgpr_os_dsa_lower <- matrix_replace(2, 1-dsa_rate, os_matrix)
                 pr_os_dsa_lower <- matrix_replace(3, 1-dsa_rate, os_matrix)
                 mr_os_dsa_lower <- matrix_replace(4, 1-dsa_rate, os_matrix)
                 sd_os_dsa_lower <- matrix_replace(5, 1-dsa_rate, os_matrix)
                 pd_os_dsa_lower <- matrix_replace(6, 1-dsa_rate, os_matrix)
                 #==============================================================================
                 
                 #All inputs - upper bounds =========================================
                 max_cycles_dsa_upper <- ceiling(max_cycles_vector*(1+dsa_rate))
                 maint_better_dsa_upper <- maint_better * (1+dsa_rate)
                 comp_costs_1_upper <- comp_costs_1 * (1+dsa_rate) 
                 comp_costs_2_upper <- comp_costs_2 * (1+dsa_rate)
                 comp_costs_3_upper <- comp_costs_3 * (1+dsa_rate)
                 comp_costs_4_upper <- comp_costs_4 * (1+dsa_rate)
                 adverse_dara_dsa_upper <- replace(p_adverse_tx_vector, c(1,5,6,14,15,16,23),p_adverse_tx_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 adverse_other_dsa_upper <- replace(p_adverse_tx_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_adverse_tx_vector[c(1,5,6,14,15,16,23)])
                 cr_dara_dsa_upper <- replace(p_cr_vector, c(1,5,6,14,15,16,23),p_cr_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 cr_other_dsa_upper <- replace(p_cr_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_cr_vector[c(1,5,6,14,15,16,23)])
                 vgpr_dara_dsa_upper <- replace(p_vgpr_vector, c(1,5,6,14,15,16,23),p_vgpr_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 vgpr_other_dsa_upper <- replace(p_vgpr_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_vgpr_vector[c(1,5,6,14,15,16,23)])
                 pr_dara_dsa_upper <- replace(p_pr_vector, c(1,5,6,14,15,16,23),p_pr_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 pr_other_dsa_upper <- replace(p_pr_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_pr_vector[c(1,5,6,14,15,16,23)])
                 mr_dara_dsa_upper <- replace(p_mr_vector, c(1,5,6,14,15,16,23),p_mr_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 mr_other_dsa_upper <- replace(p_mr_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_mr_vector[c(1,5,6,14,15,16,23)])
                 sd_dara_dsa_upper <- replace(p_sd_vector, c(1,5,6,14,15,16,23),p_sd_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 sd_other_dsa_upper <- replace(p_sd_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_sd_vector[c(1,5,6,14,15,16,23)])
                 pd_dara_dsa_upper <- replace(p_pd_vector, c(1,5,6,14,15,16,23),p_pd_vector[c(1,5,6,14,15,16,23)]*(1+dsa_rate))
                 pd_other_dsa_upper <- replace(p_pd_vector*(1+dsa_rate), c(1,5,6,14,15,16,23),p_pd_vector[c(1,5,6,14,15,16,23)])
                 cr_pfs_dsa_upper <- matrix_replace(1, 1+dsa_rate, pfs_matrix)
                 vgpr_pfs_dsa_upper <- matrix_replace(2, 1+dsa_rate, pfs_matrix)
                 pr_pfs_dsa_upper <- matrix_replace(3, 1+dsa_rate, pfs_matrix)
                 mr_pfs_dsa_upper <- matrix_replace(4, 1+dsa_rate, pfs_matrix)
                 sd_pfs_dsa_upper <- matrix_replace(5, 1+dsa_rate, pfs_matrix)
                 pd_pfs_dsa_upper <- matrix_replace(6, 1+dsa_rate, pfs_matrix)
                 cr_os_dsa_upper <- matrix_replace(1, 1+dsa_rate, os_matrix)
                 vgpr_os_dsa_upper <- matrix_replace(2, 1+dsa_rate, os_matrix)
                 pr_os_dsa_upper <- matrix_replace(3, 1+dsa_rate, os_matrix)
                 mr_os_dsa_upper <- matrix_replace(4, 1+dsa_rate, os_matrix)
                 sd_os_dsa_upper <- matrix_replace(5, 1+dsa_rate, os_matrix)
                 pd_os_dsa_upper <- matrix_replace(6, 1+dsa_rate, os_matrix)
                 #==============================================================================
                 
                 #### Data frame needed for normalizing ######################################
                 response_matrix_dsa <- rbind(p_cr_vector, p_vgpr_vector, p_pr_vector, p_mr_vector, p_sd_vector, p_pd_vector)
                 normalise_cr <- 1-response_matrix_dsa[1,]
                 normalise_vgpr <- 1-response_matrix_dsa[2,]
                 normalise_pr <- 1-response_matrix_dsa[3,]
                 normalise_mr <- 1-response_matrix_dsa[4,]
                 normalise_sd <- 1-response_matrix_dsa[5,]
                 normalise_pd <- 1-response_matrix_dsa[6,]
                 
                 
                 response_matrix_dara_cr_lower  <- rbind(cr_dara_dsa_lower, 
                                                         (p_vgpr_vector/normalise_cr)*(1-cr_dara_dsa_lower), 
                                                         (p_pr_vector/normalise_cr)*(1-cr_dara_dsa_lower), 
                                                         (p_mr_vector/normalise_cr)*(1-cr_dara_dsa_lower), 
                                                         (p_sd_vector/normalise_cr)*(1-cr_dara_dsa_lower), 
                                                         (p_pd_vector/normalise_cr)*(1-cr_dara_dsa_lower))
                 response_matrix_other_cr_lower <- rbind(cr_other_dsa_lower, 
                                                         (p_vgpr_vector/normalise_cr)*(1-cr_other_dsa_lower), 
                                                         (p_pr_vector/normalise_cr)*(1-cr_other_dsa_lower), 
                                                         (p_mr_vector/normalise_cr)*(1-cr_other_dsa_lower), 
                                                         (p_sd_vector/normalise_cr)*(1-cr_other_dsa_lower), 
                                                         (p_pd_vector/normalise_cr)*(1-cr_other_dsa_lower))
                 response_matrix_dara_vgpr_lower <- rbind(vgpr_dara_dsa_lower, 
                                                          (p_cr_vector/normalise_vgpr)*(1-vgpr_dara_dsa_lower), 
                                                          (p_pr_vector/normalise_vgpr)*(1-vgpr_dara_dsa_lower), 
                                                          (p_mr_vector/normalise_vgpr)*(1-vgpr_dara_dsa_lower), 
                                                          (p_sd_vector/normalise_vgpr)*(1-vgpr_dara_dsa_lower), 
                                                          (p_pd_vector/normalise_vgpr)*(1-vgpr_dara_dsa_lower))
                 response_matrix_other_vgpr_lower <- rbind(vgpr_other_dsa_lower, 
                                                           (p_cr_vector/normalise_vgpr)*(1-vgpr_other_dsa_lower), 
                                                           (p_pr_vector/normalise_vgpr)*(1-vgpr_other_dsa_lower), 
                                                           (p_mr_vector/normalise_vgpr)*(1-vgpr_other_dsa_lower), 
                                                           (p_sd_vector/normalise_vgpr)*(1-vgpr_other_dsa_lower), 
                                                           (p_pd_vector/normalise_vgpr)*(1-vgpr_other_dsa_lower))
                 response_matrix_dara_pr_lower <- rbind(pr_dara_dsa_lower, 
                                                        (p_cr_vector/normalise_pr)*(1-pr_dara_dsa_lower), 
                                                        (p_vgpr_vector/normalise_pr)*(1-pr_dara_dsa_lower), 
                                                        (p_mr_vector/normalise_pr)*(1-pr_dara_dsa_lower), 
                                                        (p_sd_vector/normalise_pr)*(1-pr_dara_dsa_lower), 
                                                        (p_pd_vector/normalise_pr)*(1-pr_dara_dsa_lower))
                 response_matrix_other_pr_lower <- rbind(pr_other_dsa_lower, 
                                                         (p_cr_vector/normalise_pr)*(1-pr_other_dsa_lower), 
                                                         (p_vgpr_vector/normalise_pr)*(1-pr_other_dsa_lower), 
                                                         (p_mr_vector/normalise_pr)*(1-pr_other_dsa_lower), 
                                                         (p_sd_vector/normalise_pr)*(1-pr_other_dsa_lower), 
                                                         (p_pd_vector/normalise_pr)*(1-pr_other_dsa_lower))
                 response_matrix_dara_mr_lower <- rbind(mr_dara_dsa_lower, 
                                                        (p_cr_vector/normalise_mr)*(1-mr_dara_dsa_lower), 
                                                        (p_vgpr_vector/normalise_mr)*(1-mr_dara_dsa_lower), 
                                                        (p_pr_vector/normalise_mr)*(1-mr_dara_dsa_lower), 
                                                        (p_sd_vector/normalise_mr)*(1-mr_dara_dsa_lower), 
                                                        (p_pd_vector/normalise_mr)*(1-mr_dara_dsa_lower))
                 response_matrix_other_mr_lower<- rbind(mr_other_dsa_lower, 
                                                        (p_cr_vector/normalise_mr)*(1-mr_other_dsa_lower), 
                                                        (p_vgpr_vector/normalise_mr)*(1-mr_other_dsa_lower), 
                                                        (p_pr_vector/normalise_mr)*(1-mr_other_dsa_lower), 
                                                        (p_sd_vector/normalise_mr)*(1-mr_other_dsa_lower), 
                                                        (p_pd_vector/normalise_mr)*(1-mr_other_dsa_lower))
                 response_matrix_dara_sd_lower <- rbind(sd_dara_dsa_lower, 
                                                        (p_cr_vector/normalise_sd)*(1-sd_dara_dsa_lower), 
                                                        (p_vgpr_vector/normalise_sd)*(1-sd_dara_dsa_lower), 
                                                        (p_pr_vector/normalise_sd)*(1-sd_dara_dsa_lower), 
                                                        (p_mr_vector/normalise_sd)*(1-sd_dara_dsa_lower), 
                                                        (p_pd_vector/normalise_sd)*(1-sd_dara_dsa_lower))
                 response_matrix_other_sd_lower <- rbind(sd_other_dsa_lower, 
                                                         (p_cr_vector/normalise_sd)*(1-sd_other_dsa_lower), 
                                                         (p_vgpr_vector/normalise_sd)*(1-sd_other_dsa_lower), 
                                                         (p_pr_vector/normalise_sd)*(1-sd_other_dsa_lower), 
                                                         (p_mr_vector/normalise_sd)*(1-sd_other_dsa_lower), 
                                                         (p_pd_vector/normalise_sd)*(1-sd_other_dsa_lower))
                 response_matrix_dara_pd_lower <- rbind(pd_dara_dsa_lower, 
                                                        (p_cr_vector/normalise_pd)*(1-pd_dara_dsa_lower), 
                                                        (p_vgpr_vector/normalise_pd)*(1-pd_dara_dsa_lower), 
                                                        (p_pr_vector/normalise_pd)*(1-pd_dara_dsa_lower), 
                                                        (p_mr_vector/normalise_pd)*(1-pd_dara_dsa_lower), 
                                                        (p_sd_vector/normalise_pd)*(1-pd_dara_dsa_lower))
                 response_matrix_other_pd_lower <- rbind(pd_other_dsa_lower, 
                                                         (p_cr_vector/normalise_pd)*(1-pd_other_dsa_lower), 
                                                         (p_vgpr_vector/normalise_pd)*(1-pd_other_dsa_lower), 
                                                         (p_pr_vector/normalise_pd)*(1-pd_other_dsa_lower), 
                                                         (p_mr_vector/normalise_pd)*(1-pd_other_dsa_lower), 
                                                         (p_sd_vector/normalise_pd)*(1-pd_other_dsa_lower))
                 
                 ##### upper bound
                 
                 response_matrix_dara_cr_upper  <- rbind(cr_dara_dsa_upper, 
                                                         (p_vgpr_vector/normalise_cr)*(1-cr_dara_dsa_upper), 
                                                         (p_pr_vector/normalise_cr)*(1-cr_dara_dsa_upper), 
                                                         (p_mr_vector/normalise_cr)*(1-cr_dara_dsa_upper), 
                                                         (p_sd_vector/normalise_cr)*(1-cr_dara_dsa_upper), 
                                                         (p_pd_vector/normalise_cr)*(1-cr_dara_dsa_upper))
                 response_matrix_other_cr_upper <- rbind(cr_other_dsa_upper, 
                                                         (p_vgpr_vector/normalise_cr)*(1-cr_other_dsa_upper), 
                                                         (p_pr_vector/normalise_cr)*(1-cr_other_dsa_upper), 
                                                         (p_mr_vector/normalise_cr)*(1-cr_other_dsa_upper), 
                                                         (p_sd_vector/normalise_cr)*(1-cr_other_dsa_upper), 
                                                         (p_pd_vector/normalise_cr)*(1-cr_other_dsa_upper))
                 response_matrix_dara_vgpr_upper <- rbind(vgpr_dara_dsa_upper, 
                                                          (p_cr_vector/normalise_vgpr)*(1-vgpr_dara_dsa_upper), 
                                                          (p_pr_vector/normalise_vgpr)*(1-vgpr_dara_dsa_upper), 
                                                          (p_mr_vector/normalise_vgpr)*(1-vgpr_dara_dsa_upper), 
                                                          (p_sd_vector/normalise_vgpr)*(1-vgpr_dara_dsa_upper), 
                                                          (p_pd_vector/normalise_vgpr)*(1-vgpr_dara_dsa_upper))
                 response_matrix_other_vgpr_upper <- rbind(vgpr_other_dsa_upper, 
                                                           (p_cr_vector/normalise_vgpr)*(1-vgpr_other_dsa_upper), 
                                                           (p_pr_vector/normalise_vgpr)*(1-vgpr_other_dsa_upper), 
                                                           (p_mr_vector/normalise_vgpr)*(1-vgpr_other_dsa_upper), 
                                                           (p_sd_vector/normalise_vgpr)*(1-vgpr_other_dsa_upper), 
                                                           (p_pd_vector/normalise_vgpr)*(1-vgpr_other_dsa_upper))
                 response_matrix_dara_pr_upper <- rbind(pr_dara_dsa_upper, 
                                                        (p_cr_vector/normalise_pr)*(1-pr_dara_dsa_upper), 
                                                        (p_vgpr_vector/normalise_pr)*(1-pr_dara_dsa_upper), 
                                                        (p_mr_vector/normalise_pr)*(1-pr_dara_dsa_upper), 
                                                        (p_sd_vector/normalise_pr)*(1-pr_dara_dsa_upper), 
                                                        (p_pd_vector/normalise_pr)*(1-pr_dara_dsa_upper))
                 response_matrix_other_pr_upper <- rbind(pr_other_dsa_upper, 
                                                         (p_cr_vector/normalise_pr)*(1-pr_other_dsa_upper), 
                                                         (p_vgpr_vector/normalise_pr)*(1-pr_other_dsa_upper), 
                                                         (p_mr_vector/normalise_pr)*(1-pr_other_dsa_upper), 
                                                         (p_sd_vector/normalise_pr)*(1-pr_other_dsa_upper), 
                                                         (p_pd_vector/normalise_pr)*(1-pr_other_dsa_upper))
                 response_matrix_dara_mr_upper <- rbind(mr_dara_dsa_upper, 
                                                        (p_cr_vector/normalise_mr)*(1-mr_dara_dsa_upper), 
                                                        (p_vgpr_vector/normalise_mr)*(1-mr_dara_dsa_upper), 
                                                        (p_pr_vector/normalise_mr)*(1-mr_dara_dsa_upper), 
                                                        (p_sd_vector/normalise_mr)*(1-mr_dara_dsa_upper), 
                                                        (p_pd_vector/normalise_mr)*(1-mr_dara_dsa_upper))
                 response_matrix_other_mr_upper<- rbind(mr_other_dsa_upper, 
                                                        (p_cr_vector/normalise_mr)*(1-mr_other_dsa_upper), 
                                                        (p_vgpr_vector/normalise_mr)*(1-mr_other_dsa_upper), 
                                                        (p_pr_vector/normalise_mr)*(1-mr_other_dsa_upper), 
                                                        (p_sd_vector/normalise_mr)*(1-mr_other_dsa_upper), 
                                                        (p_pd_vector/normalise_mr)*(1-mr_other_dsa_upper))
                 response_matrix_dara_sd_upper <- rbind(sd_dara_dsa_upper, 
                                                        (p_cr_vector/normalise_sd)*(1-sd_dara_dsa_upper), 
                                                        (p_vgpr_vector/normalise_sd)*(1-sd_dara_dsa_upper), 
                                                        (p_pr_vector/normalise_sd)*(1-sd_dara_dsa_upper), 
                                                        (p_mr_vector/normalise_sd)*(1-sd_dara_dsa_upper), 
                                                        (p_pd_vector/normalise_sd)*(1-sd_dara_dsa_upper))
                 response_matrix_other_sd_upper <- rbind(sd_other_dsa_upper, 
                                                         (p_cr_vector/normalise_sd)*(1-sd_other_dsa_upper), 
                                                         (p_vgpr_vector/normalise_sd)*(1-sd_other_dsa_upper), 
                                                         (p_pr_vector/normalise_sd)*(1-sd_other_dsa_upper), 
                                                         (p_mr_vector/normalise_sd)*(1-sd_other_dsa_upper), 
                                                         (p_pd_vector/normalise_sd)*(1-sd_other_dsa_upper))
                 response_matrix_dara_pd_upper <- rbind(pd_dara_dsa_upper, 
                                                        (p_cr_vector/normalise_pd)*(1-pd_dara_dsa_upper), 
                                                        (p_vgpr_vector/normalise_pd)*(1-pd_dara_dsa_upper), 
                                                        (p_pr_vector/normalise_pd)*(1-pd_dara_dsa_upper), 
                                                        (p_mr_vector/normalise_pd)*(1-pd_dara_dsa_upper), 
                                                        (p_sd_vector/normalise_pd)*(1-pd_dara_dsa_upper))
                 response_matrix_other_pd_upper <- rbind(pd_other_dsa_upper, 
                                                         (p_cr_vector/normalise_pd)*(1-pd_other_dsa_upper), 
                                                         (p_vgpr_vector/normalise_pd)*(1-pd_other_dsa_upper), 
                                                         (p_pr_vector/normalise_pd)*(1-pd_other_dsa_upper), 
                                                         (p_mr_vector/normalise_pd)*(1-pd_other_dsa_upper), 
                                                         (p_sd_vector/normalise_pd)*(1-pd_other_dsa_upper))
                 
                 ################################################################################
                 
                 ##########################################################################################
                 ################### BASELINE MODEL RUN ###################################################
                 
                
                 cost_cycle1 <- number_of_doses_1[1,]*comp_costs_1 + number_of_doses_1[2,]*comp_costs_2 + number_of_doses_1[3,]*comp_costs_3 + number_of_doses_1[3,]*comp_costs_3
                 cost_cycle2 <- number_of_doses_2[1,]*comp_costs_1 + number_of_doses_2[2,]*comp_costs_2 + number_of_doses_2[3,]*comp_costs_3 + number_of_doses_2[3,]*comp_costs_3
                 cost_cycle3 <- number_of_doses_3[1,]*comp_costs_1 + number_of_doses_3[2,]*comp_costs_2 + number_of_doses_3[3,]*comp_costs_3 + number_of_doses_3[3,]*comp_costs_3
                 cost_cycle4 <- number_of_doses_4[1,]*comp_costs_1 + number_of_doses_4[2,]*comp_costs_2 + number_of_doses_4[3,]*comp_costs_3 + number_of_doses_4[3,]*comp_costs_3 
                 cost_cycle5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0)
                 cost_cycle6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                 cost_cycle7 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                 cost_cycle8 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                 
                 cost_matrix <- rbind(cost_cycle1, cost_cycle2, cost_cycle3, cost_cycle4, cost_cycle5, cost_cycle6, cost_cycle7, cost_cycle8)
                 
                 cost_nodara <- cost_matrix[ ,-c(1,5,6,14,15,16,22,23)]
                 cost_matrix$dummy <- rowMeans(cost_nodara) #amit a negyedik line-t?l kap, az a daratumumab n?lk?li ter?pi?k ?tlaga
                 
                 #---- I.1. Population ----
                 set.seed(20220411)
                 
                 population <- data.frame(asct_elig=numeric(), venvd_elig=numeric(), cytrisk=numeric())
                 
                 #rbinom(N,1,rate)
                 for (l in 1:N){
                   rand_asct <- runif(1)
                   rand_venvd <- runif(1) 
                   rand_cytrisk <- runif(1)
                   
                   asct_elig <- if (rand_asct<asct_rate){1}else{0}
                   venvd_elig <- if (rand_venvd<venvd_rate){1}else{0}
                   cytrisk <- if (rand_cytrisk<cytrisk_rate){1}else{0}
                   
                   population[l,] <- c(asct_elig,venvd_elig,cytrisk)
                 }
                 
                 
                 #patient seedek
                 patient_seed_all<- sample(10000:99999, N*2, replace = FALSE, prob = NULL)
                 patient_seed_split <- complete_ra(N = N, num_arms = 2)
                 table_seed = data.table("seeds" = patient_seed_all, "split"= patient_seed_split)
                 seed1 = table_seed[table_seed$split=='T1']
                 seed2 = table_seed[table_seed$split=='T2']
                 
                 population$seed1 <- seed1$seeds
                 population$seed2 <- seed2$seeds
                 
                 #cycle seedek (ebbol el?g egy darab)
                 cycle_seed <- sample(100:999, number_of_cycles, replace= FALSE, prob = NULL)
                 
                 
                 ##### III. Model start #########################################################
                 
                 
                 #---- III.1. Patient level simulation ----
                 per_patient <- data.frame(n=numeric(), asct_elig=numeric(), venvd_elig=numeric(), cytrisk=numeric(),
                                           l1_tx_int=numeric(), asct2_tx_int=numeric(), maint_tx_int=numeric(), l2_tx_int=numeric(), l3_tx_int=numeric(),
                                           date_prog_l1_int=numeric(), date_prog_asct2_int=numeric(), date_prog_maint_int=numeric(), date_prog_l2_int=numeric(), date_prog_l3_int=numeric(),
                                           date_relapse_l1_int=numeric(), date_relapse_asct2_int=numeric(), date_relapse_maint_int=numeric(), date_relapse_l2_int=numeric(), date_relapse_l3_int=numeric(), date_death_int=numeric(), cost_int=numeric(), cost_int_disc=numeric(),
                                           l1_tx_comp=numeric(), asct2_tx_comp=numeric(), maint_tx_comp=numeric(), l2_tx_comp=numeric(), l3_tx_comp=numeric(),
                                           date_prog_l1_comp=numeric(), date_prog_asct2_comp=numeric(), date_prog_int_comp=numeric(), date_prog_l2_comp=numeric(), date_prog_l3_comp=numeric(),
                                           date_relapse_l1_comp=numeric(), date_relapse_asct2_comp=numeric(), date_relapse_maint_comp=numeric(), date_relapse_l2_comp=numeric(), date_relapse_l3_comp=numeric(), date_death_comp=numeric(), cost_comp=numeric(), cost_comp_disc=numeric(),
                                           date_adverse_l1_int=numeric(), date_adverse_asct2_int=numeric(), date_adverse_maint_int=numeric(), date_adverse_l2_int=numeric(), date_adverse_l3_int=numeric(),
                                           date_adverse_l1_comp=numeric(), date_adverse_asct2_comp=numeric(), date_adverse_maint_comp=numeric(), date_adverse_l2_comp=numeric(), date_adverse_l3_comp=numeric())
                 
                 model_start<-Sys.time()
                 
                 for (n in 1:N) {
                   
                   #patient data per cycle
                   per_cycle <- data.frame(cycle=0, 
                                           treatment_int=0, tracker_int=0, cycle_track_int=0, current_line_int=0, toxic_int=1, cost_int=0,
                                           treatment_comp=0, tracker_comp=0, cycle_track_comp=0, current_line_comp=0, toxic_comp=1, cost_comp=0,
                                           l1_tx_int=0, l1_tx_comp=0, 
                                           date_adverse_l1_comp=0, date_adverse_2asct_comp=0, date_adverse_maint_comp=0, date_adverse_l2_comp=0, date_adverse_l3_comp=0,
                                           date_adverse_l1_int=0, date_adverse_2asct_int=0, date_adverse_maint_int=0, date_adverse_l2_int=0, date_adverse_l3_int=0,
                                           response_int=0, response_comp=0, relapse_int=1, relapse_comp=1, 
                                           date_death_int=0, date_prog_l1_int=0, date_prog_2asct_int=0, date_prog_l2_int=0, date_prog_l3_int=0, date_relapse_l1_int=0, date_relapse_2asct_int=0, date_relapse_l2_int=0, date_relapse_l3_int=0,
                                           date_death_comp=0, date_prog_l1_comp=0, date_prog_2asct_comp=0, date_prog_l2_comp=0, date_prog_l3_comp=0, date_relapse_l1_comp=0, date_relapse_2asct_comp=0, date_relapse_l2_comp=0, date_relapse_l3_comp=0,
                                           asct2_int=0, asct2_comp=0 ) 
                   
                   #Baseline Patient data
                   asct_elig <- population[n,1]
                   venvd_elig <- population[n,2]
                   cytrisk <- population[n,3]
                   patient_seed_int <- population[n, 4]
                   patient_seed_comp <- population[n, 5]
                   
                   #==== III.1.1. Cycle level simulation====
                   for (i in 1:number_of_cycles) {
                     
                     seed_int <- as.integer(paste0(patient_seed_int, cycle_seed[i]))
                     seed_comp <- as.integer(paste0(patient_seed_comp, cycle_seed[i]))
                     seed_int_2 <- as.integer(paste0(patient_seed_int, cycle_seed[i],2))
                     seed_comp_2 <- as.integer(paste0(patient_seed_comp, cycle_seed[i],2))
                     
                     #Treatment decision algoritm
                     treatment_vector <- treatment(asct_elig, venvd_elig, cytrisk, i, 
                                                   response_int = per_cycle[i,26], response_comp = per_cycle[i,27], 
                                                   treatment_int = per_cycle[i,2], treatment_comp = per_cycle[i,8], 
                                                   prev_line_int = per_cycle[i,5], prev_line_comp = per_cycle[i,11], 
                                                   tracker_int= per_cycle[i,3], tracker_comp = per_cycle[i,9], 
                                                   cycle_track_int = per_cycle[i,4], cycle_track_comp = per_cycle[i,10], 
                                                   toxic_int= per_cycle[i,6], toxic_comp= per_cycle[i,12],
                                                   relapse_int= per_cycle[i,28], relapse_comp= per_cycle[i,29], 
                                                   l1_tx_int= per_cycle[i,14], l1_tx_comp= per_cycle[i,15], 
                                                   date_adverse_l1_comp=per_cycle[i,16], date_adverse_2asct_comp=per_cycle[i,17], date_adverse_maint_comp=per_cycle[i,18],date_adverse_l2_comp=per_cycle[i,19],date_adverse_l3_comp=per_cycle[i,20],
                                                   date_adverse_l1_int=per_cycle[i,21],date_adverse_2asct_int=per_cycle[i,22],date_adverse_maint_int=per_cycle[i,23],date_adverse_l2_int=per_cycle[i,24],date_adverse_l3_int=per_cycle[i,25],
                                                   asct2_int=per_cycle[i,48], asct2_comp=per_cycle[i,49], 
                                                   seed_int, seed_comp, 
                                                   max_cycles_vector, cost_matrix, p_adverse_tx_vector, p_cr_vector, p_vgpr_vector, p_pr_vector, p_mr_vector, p_sd_vector, p_pd_vector, maint_better, scenario, dara2l,  rates_vector, dara_retr)
                     
                     #translate current_line from treatment to current line used in outcome
                     current_line_int <- if (treatment_vector[4]==1 | treatment_vector[4]==2 |treatment_vector[4]==5) {1} 
                     else if (treatment_vector[4]==3) {2} 
                     else {3}
                     
                     current_line_comp <- if (treatment_vector[11]==1 | treatment_vector[11]==2 |treatment_vector[11]==5) {1} 
                     else if (treatment_vector[11]==3) {2} 
                     else {3}
                     
                     outcome_vector <- outcome(i, current_line_int, current_line_comp, treatment_vector_line_int=treatment_vector[4], treatment_vector_line_comp=treatment_vector[11], 
                                               response_int=treatment_vector[5], response_comp=treatment_vector[12], cytrisk,
                                               pfs_matrix, os_matrix, 
                                               seed_int_2, seed_comp_2)
                     
                     per_cycle[i+1,] <- c(i, treatment_vector[-c(5,12,27,28)], outcome_vector, treatment_vector[c(27,28)])
                   }#cycle simulation end
                   
                   #discounting
                   per_cycle <- per_cycle %>% mutate(cost_int_disc = cost_int/((1+disc_cost)^(floor(cycle*0.25))),cost_comp_disc = cost_comp/((1+disc_cost)^(floor(cycle*0.25)))) 
                   
                   #takes per_patient table variables one_by_one
                   l1_tx_int <- max(per_cycle$l1_tx_int)
                   asct2_tx_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else {filter(per_cycle, current_line_int==5)$treatment_int[1]}
                   maint_tx_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else {filter(per_cycle, current_line_int==2)$treatment_int[1]}
                   l2_tx_int <- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{filter(per_cycle, current_line_int==3)$treatment_int[1]}
                   l3_tx_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{filter(per_cycle, current_line_int==4)$treatment_int[1]}
                   date_prog_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_prog_l1_int)}
                   date_prog_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_prog_2asct_int)}
                   date_prog_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{max(filter(per_cycle, current_line_int==2)$date_prog_l1_int)}
                   date_prog_l2_int <- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_prog_l2_int)}
                   date_prog_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_prog_l3_int)}
                   
                   date_relapse_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_relapse_l1_int)}
                   date_relapse_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_relapse_2asct_int)}
                   date_relapse_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{max(filter(per_cycle, current_line_int==2)$date_relapse_l1_int)}
                   date_relapse_l2_int<- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_relapse_l2_int)}
                   date_relapse_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_relapse_l3_int)}
                   date_death_int <- max(per_cycle$date_death_int)
                   cost_int <- sum(per_cycle$cost_int)
                   cost_int_disc <- sum(per_cycle$cost_int_disc)
                   #Adverse Event date int
                   date_adverse_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_adverse_l1_int)}
                   date_adverse_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_adverse_2asct_int)}
                   date_adverse_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{filter(per_cycle, current_line_int==2)$date_adverse_maint_int[1]}
                   date_adverse_l2_int<- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_adverse_l2_int)}
                   date_adverse_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_adverse_l3_int)}
                   
                   l1_tx_comp <- max(unique(per_cycle$l1_tx_comp))
                   asct2_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{filter(per_cycle, current_line_comp==5)$treatment_comp[1]}
                   maint_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{filter(per_cycle, current_line_comp==2)$treatment_comp[1]}
                   l2_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{filter(per_cycle, current_line_comp==3)$treatment_comp[1]}
                   l3_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{filter(per_cycle, current_line_comp==4)$treatment_comp[1]}
                   date_prog_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp)}
                   date_prog_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_prog_2asct_comp)}
                   date_prog_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp)}
                   date_prog_l2_comp <- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_prog_l2_comp)}
                   date_prog_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_prog_l3_comp)}
                   
                   date_relapse_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_relapse_l1_comp)}
                   date_relapse_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_relapse_2asct_comp)}
                   date_relapse_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{max(filter(per_cycle, current_line_comp==2)$date_relapse_l1_comp)}
                   date_relapse_l2_comp<- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_relapse_l2_comp)}
                   date_relapse_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_relapse_l3_comp)}
                   date_death_comp <- max(per_cycle$date_death_comp)
                   cost_comp <- sum(per_cycle$cost_comp)
                   cost_comp_disc <- sum(per_cycle$cost_comp_disc)
                   
                   #Adverse Event date comp
                   date_adverse_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_adverse_l1_comp)}
                   date_adverse_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_adverse_2asct_comp)}
                   date_adverse_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{filter(per_cycle, current_line_comp==2)$date_adverse_maint_comp[1]}
                   date_adverse_l2_comp<- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_adverse_l2_comp)}
                   date_adverse_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_adverse_l3_comp)}
                   
                   
                   # a per cycle-bol k?sz?t egy per patient sort, ezt m?g ki k?ne tal?lnom.
                   per_patient[n, ] <- c(n, population[n,1],population[n,2],population[n,3],
                                         l1_tx_int, asct2_tx_int, maint_tx_int, l2_tx_int, l3_tx_int,
                                         date_prog_l1_int, date_prog_asct2_int, date_prog_maint_int, date_prog_l2_int, date_prog_l3_int,
                                         date_relapse_l1_int, date_relapse_asct2_int, date_relapse_maint_int, date_relapse_l2_int, date_relapse_l3_int, date_death_int, cost_int, cost_int_disc,
                                         l1_tx_comp, asct2_tx_comp, maint_tx_comp, l2_tx_comp, l3_tx_comp,
                                         date_prog_l1_comp, date_prog_asct2_comp, date_prog_maint_comp, date_prog_l2_comp, date_prog_l3_comp,
                                         date_relapse_l1_comp, date_relapse_asct2_comp, date_relapse_maint_comp, date_relapse_l2_comp, date_relapse_l3_comp, date_death_comp, cost_comp, cost_comp_disc,
                                         date_adverse_l1_int, date_adverse_asct2_int, date_adverse_maint_int, date_adverse_l2_int, date_adverse_l3_int,
                                         date_adverse_l1_comp, date_adverse_asct2_comp, date_adverse_maint_comp, date_adverse_l2_comp, date_adverse_l3_comp
                   )
                 }
                 
                 model_finish <- Sys.time()
                 runtime <- model_finish - model_start
                 
                 ##### IV. Outputs ##################################################################
                 
                 LY_int <- (sum(per_patient$date_death_int)*0.25)/N
                 LY_comp <- (sum(per_patient$date_death_comp)*0.25)/N
                 delta_LY <- LY_int - LY_comp
                 
                 total_cost_int <- (sum(per_patient$cost_int_disc))/N
                 total_cost_comp <- (sum(per_patient$cost_comp_disc))/N
                 delta_cost <- total_cost_int-total_cost_comp
                 
                 ICER_baseline <- delta_cost/delta_LY
                 
                 
                 
                 ####################################################################################
                 ###################### LOWER BOUND MODEL RUN #######################################
                 ####################################################################################
                 
                 for (v in 1:no_variables){
                   
                   #vektorokba gyujt?tt inputok
                   max_cycles_vector <- c(100, input_final$max_cycles_vector[!is.na(input_final$max_cycles_vector)])
                   p_adverse_tx_vector <- input_final$p_adverse_tx_vector[!is.na(input_final$p_adverse_tx_vector)]
                   p_cr_vector <- input_final$p_cr_vector[!is.na(input_final$p_cr_vector)]
                   p_vgpr_vector <- input_final$p_vgpr_vector[!is.na(input_final$p_vgpr_vector)]
                   p_pr_vector <- input_final$p_pr_vector[!is.na(input_final$p_pr_vector)]
                   p_mr_vector <- input_final$p_mr_vector[!is.na(input_final$p_mr_vector)]
                   p_sd_vector <- input_final$p_sd_vector[!is.na(input_final$p_sd_vector)]
                   p_pd_vector <- input_final$p_pd_vector[!is.na(input_final$p_pd_vector)]
                   
                   #treatment rates (shiny)
                   rates_vector <- rep(NA, 36)
                   rates_vector[1] <- input$rate_1
                   rates_vector[2] <- input$rate_2
                   rates_vector[3] <- input$rate_3
                   rates_vector[4] <- input$rate_4
                   rates_vector[5] <- input$rate_5
                   rates_vector[6] <- input$rate_6
                   rates_vector[7] <- input$rate_7
                   rates_vector[8] <- input$rate_8
                   rates_vector[9] <- input$rate_9
                   rates_vector[10] <- input$rate_10
                   rates_vector[11] <- input$rate_11
                   rates_vector[12] <- input$rate_12
                   rates_vector[13] <- input$rate_13
                   rates_vector[14] <- input$rate_14
                   rates_vector[15] <- input$rate_15
                   rates_vector[16] <- input$rate_16
                   rates_vector[17] <- input$rate_17
                   rates_vector[18] <- input$rate_18
                   rates_vector[19] <- input$rate_19
                   rates_vector[20] <- input$rate_20
                   rates_vector[21] <- input$rate_21
                   rates_vector[22] <- input$rate_22
                   rates_vector[23] <- input$rate_23
                   rates_vector[24] <- input$rate_24
                   rates_vector[25] <- input$rate_25
                   rates_vector[26] <- input$rate_26
                   rates_vector[27] <- input$rate_27
                   rates_vector[28] <- input$rate_28
                   rates_vector[29] <- input$rate_29
                   rates_vector[30] <- input$rate_30
                   rates_vector[31] <- input$rate_31
                   rates_vector[32] <- input$rate_32
                   rates_vector[33] <- input$rate_33
                   rates_vector[34] <- input$rate_34
                   rates_vector[35] <- input$rate_35
                   rates_vector[36] <- input$rate_36
                   
                   rates_vector <- rates_vector/100
                   
                   
                   #m?trixok is vannak
                   pfs_matrix <- cbind(input_final$l1_pfs_vector[!is.na(input_final$l1_pfs_vector)], input_final$l2_pfs_vector[!is.na(input_final$l2_pfs_vector)], input_final$l3_pfs_vector[!is.na(input_final$l3_pfs_vector)])
                   os_matrix <- cbind(input_final$l1_os_vector[!is.na(input_final$l1_os_vector)], input_final$l2_os_vector[!is.na(input_final$l2_os_vector)], input_final$l3_os_vector[!is.na(input_final$l3_os_vector)])
                   
                   
                   #egyeb shiny inputok
                   dara2l <- input$dara2l
                   scenario <- input$scenario
                   asct_rate <- input$asct_rate/100
                   venvd_rate <- input$venvd_rate/100
                   cytrisk_rate <- input$cytrisk_rate/100
                   N <- input$N
                   disc_cost <- input$disc_cost/100
                   dara_retr <- input$dara_retreat
                   
                   
                   #marad?k
                   number_of_cycles <- input_final$number_of_cycles[!is.na(input_final$number_of_cycles)]
                   maint_better <- input_final$maint_better[!is.na(input_final$maint_better)]
                   
                   #cost matrix (shiny)
                   cost_D <- input$cost_D
                   cost_K <- input$cost_K
                   cost_M <- input$cost_M
                   cost_Pom <- input$cost_Pom
                   cost_R <- input$cost_R
                   cost_S <- input$cost_S
                   cost_V <- input$cost_V
                   cost_Ven <- input$cost_Ven
                   cost_Ixa <- input$cost_Ixa
                   cost_Isa <- input$cost_Isa
                   cost_dd <- input$cost_dd
                   cost_P <- input$cost_P
                   cost_T <- input$cost_T
                   cost_C <- input$cost_C
                   
                   box_dose_D <- 1
                   box_dose_K <- 1.68
                   box_dose_Pom <- 0.0476
                   box_dose_R <- 0.1905
                   box_dose_V <- 0.6686
                   box_dose_Ven <- 0.2857
                   box_dose_Ixa <- 0.3333
                   box_dose_dd <- 0.04
                   box_dose_P <- 0.216
                   box_dose_T <- 0.1429
                   box_dose_M <- 1
                   box_dose_S <- 1
                   box_dose_Isa <- 1
                   box_dose_C <- 1
                   
                   number_of_doses_1 <- read.csv("no_admin_1.csv") #number of administration in the first cycle
                   number_of_doses_2 <- read.csv("no_admin_2.csv") #number of adminsitration in the second cycle
                   number_of_doses_3 <- read.csv("no_admin_3.csv") #number of adminsitration in the third cycle
                   number_of_doses_4 <- read.csv("no_admin_4.csv") #number of adminsitration in the fourth cycle
                   
                   
                   comp_costs_1 <- c(cost_D, 0, 0, 0, cost_D, cost_D, 0, 0, 0, 0, 0, 0, 0, cost_D, cost_D, cost_D, 0, 0, 0, 0, 0, 0, cost_D, 0)
                   comp_costs_2 <- c(cost_V, cost_V, cost_V, cost_V, 0, cost_V, cost_K, cost_Ixa, cost_Ven, cost_Isa, cost_Pom, cost_S, cost_K, cost_V, cost_K, cost_Pom, cost_Isa, cost_V, cost_R, cost_V, 0, 0, 0, cost_V)
                   comp_costs_3 <- c(cost_T, cost_R, cost_T, cost_C, cost_R, cost_M, cost_R, cost_R, cost_V, cost_K, cost_V, cost_V, 0, 0, 0, 0, cost_Pom, 0, 0, cost_M, cost_R, 0, cost_R, cost_R)
                   comp_costs_4 <- c(cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_P, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, 0, 0, cost_P, cost_dd, 0, cost_dd, cost_dd)
                   
                   comp_costs_1 <- comp_costs_1*box_dose_D
                   comp_costs_2 <- c(cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, 0, cost_V*box_dose_V, cost_K*box_dose_K, cost_Ixa*box_dose_Ixa, cost_Ven*box_dose_Ven, cost_Isa*box_dose_Isa, 
                                     cost_Pom*box_dose_Pom, cost_S*box_dose_S, cost_K*box_dose_K, cost_V*box_dose_V, cost_K*box_dose_K, cost_Pom*box_dose_Pom, cost_Isa*box_dose_Isa, cost_V*box_dose_V, cost_R*box_dose_R, cost_V*box_dose_V, 0, 0, 0, cost_V*box_dose_V)
                   comp_costs_3 <- c(cost_T*box_dose_T, cost_R*box_dose_R, cost_T*box_dose_T, cost_C*box_dose_C, cost_R*box_dose_R, cost_M*box_dose_M, cost_R*box_dose_R, cost_R*box_dose_R, 
                                     cost_V*box_dose_V, cost_K*box_dose_K, cost_V*box_dose_V, cost_V*box_dose_V, 0, 0, 0, 0, cost_Pom*box_dose_Pom, 0, 0, cost_M*box_dose_M, cost_R*box_dose_R, 0, cost_R*box_dose_R, cost_R*box_dose_R)
                   comp_costs_4 <- c(cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_P*box_dose_P, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 
                                     cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 0, 0, cost_P*box_dose_P, cost_dd*box_dose_dd, 0, cost_dd*box_dose_dd, cost_dd*box_dose_dd)
                   
                   #changes the right input
                   if (dsa_vector_2[v]==1) {
                     max_cycles_vector <- max_cycles_dsa_lower
                   } else if (dsa_vector_2[v]==2) {
                     maint_better <- maint_better_dsa_lower
                   } else if (dsa_vector_2[v]==3) {
                     p_adverse_tx_vector <- adverse_dara_dsa_lower
                   } else if (dsa_vector_2[v]==4) {
                     p_adverse_tx_vector <- adverse_other_dsa_lower
                   } else if (dsa_vector_2[v]==5) {
                     p_cr_vector <- cr_dara_dsa_lower
                     p_vgpr_vector <- response_matrix_dara_cr_lower[2,]
                     p_pr_vector <- response_matrix_dara_cr_lower[3,]
                     p_mr_vector <- response_matrix_dara_cr_lower[4,]
                     p_sd_vector <- response_matrix_dara_cr_lower[5,]
                     p_pd_vector <- response_matrix_dara_cr_lower[6,]
                   } else if (dsa_vector_2[v]==6) {
                     p_cr_vector <- cr_other_dsa_lower
                     p_vgpr_vector <- response_matrix_other_cr_lower[2,]
                     p_pr_vector <- response_matrix_other_cr_lower[3,]
                     p_mr_vector <- response_matrix_other_cr_lower[4,]
                     p_sd_vector <- response_matrix_other_cr_lower[5,]
                     p_pd_vector <- response_matrix_other_cr_lower[6,]
                   } else if (dsa_vector_2[v]==7) {
                     p_vgpr_vector <- vgpr_dara_dsa_lower
                     p_cr_vector <- response_matrix_dara_vgpr_lower[2,]
                     p_pr_vector <- response_matrix_dara_vgpr_lower[3,]
                     p_mr_vector <- response_matrix_dara_vgpr_lower[4,]
                     p_sd_vector <- response_matrix_dara_vgpr_lower[5,]
                     p_pd_vector <- response_matrix_dara_vgpr_lower[6,]
                   } else if (dsa_vector_2[v]==8) {
                     p_vgpr_vector <- vgpr_other_dsa_lower
                     p_cr_vector <- response_matrix_other_vgpr_lower[2,]
                     p_pr_vector <- response_matrix_other_vgpr_lower[3,]
                     p_mr_vector <- response_matrix_other_vgpr_lower[4,]
                     p_sd_vector <- response_matrix_other_vgpr_lower[5,]
                     p_pd_vector <- response_matrix_other_vgpr_lower[6,]
                   } else if (dsa_vector_2[v]==9) {
                     p_pr_vector <- pr_dara_dsa_lower
                     p_cr_vector <- response_matrix_dara_pr_lower[2,]
                     p_vgpr_vector <- response_matrix_dara_pr_lower[3,]
                     p_mr_vector <- response_matrix_dara_pr_lower[4,]
                     p_sd_vector <- response_matrix_dara_pr_lower[5,]
                     p_pd_vector <- response_matrix_dara_pr_lower[6,]
                   } else if (dsa_vector_2[v]==10) {
                     p_pr_vector <- pr_other_dsa_lower
                     p_cr_vector <- response_matrix_other_pr_lower[2,]
                     p_vgpr_vector <- response_matrix_other_pr_lower[3,]
                     p_mr_vector <- response_matrix_other_pr_lower[4,]
                     p_sd_vector <- response_matrix_other_pr_lower[5,]
                     p_pd_vector <- response_matrix_other_pr_lower[6,]
                   } else if (dsa_vector_2[v]==11) {
                     p_mr_vector <- mr_dara_dsa_lower
                     p_cr_vector <- response_matrix_dara_mr_lower[2,]
                     p_vgpr_vector <- response_matrix_dara_mr_lower[3,]
                     p_pr_vector <- response_matrix_dara_mr_lower[4,]
                     p_sd_vector <- response_matrix_dara_mr_lower[5,]
                     p_pd_vector <- response_matrix_dara_mr_lower[6,]
                   } else if (dsa_vector_2[v]==12) {
                     p_mr_vector <- mr_other_dsa_lower
                     p_cr_vector <- response_matrix_other_mr_lower[2,]
                     p_vgpr_vector <- response_matrix_other_mr_lower[3,]
                     p_pr_vector <- response_matrix_other_mr_lower[4,]
                     p_sd_vector <- response_matrix_other_mr_lower[5,]
                     p_pd_vector <- response_matrix_other_mr_lower[6,]
                   } else if (dsa_vector_2[v]==13) {
                     p_sd_vector <- sd_dara_dsa_lower
                     p_cr_vector <- response_matrix_dara_sd_lower[2,]
                     p_vgpr_vector <- response_matrix_dara_sd_lower[3,]
                     p_pr_vector <- response_matrix_dara_sd_lower[4,]
                     p_mr_vector <- response_matrix_dara_sd_lower[5,]
                     p_pd_vector <- response_matrix_dara_sd_lower[6,]
                   } else if (dsa_vector_2[v]==14) {
                     p_sd_vector <- sd_other_dsa_lower
                     p_cr_vector <- response_matrix_other_sd_lower[2,]
                     p_vgpr_vector <- response_matrix_other_sd_lower[3,]
                     p_pr_vector <- response_matrix_other_sd_lower[4,]
                     p_mr_vector <- response_matrix_other_sd_lower[5,]
                     p_pd_vector <- response_matrix_other_sd_lower[6,]
                   } else if (dsa_vector_2[v]==15) {
                     p_pd_vector <- pd_dara_dsa_lower
                     p_cr_vector <- response_matrix_dara_pd_lower[2,]
                     p_vgpr_vector <- response_matrix_dara_pd_lower[3,]
                     p_pr_vector <- response_matrix_dara_pd_lower[4,]
                     p_mr_vector <- response_matrix_dara_pd_lower[5,]
                     p_sd_vector <- response_matrix_dara_pd_lower[6,]
                   } else if (dsa_vector_2[v]==16) {
                     p_pd_vector <- pd_other_dsa_lower
                     p_cr_vector <- response_matrix_other_pd_lower[2,]
                     p_vgpr_vector <- response_matrix_other_pd_lower[3,]
                     p_pr_vector <- response_matrix_other_pd_lower[4,]
                     p_mr_vector <- response_matrix_other_pd_lower[5,]
                     p_sd_vector <- response_matrix_other_pd_lower[6,]
                   } else if (dsa_vector_2[v]==17) {
                     pfs_matrix <- cr_pfs_dsa_lower
                   } else if (dsa_vector_2[v]==18) {
                     pfs_matrix <- vgpr_pfs_dsa_lower
                   } else if (dsa_vector_2[v]==19) {
                     pfs_matrix <- pr_pfs_dsa_lower
                   } else if (dsa_vector_2[v]==20) {
                     pfs_matrix <- mr_pfs_dsa_lower
                   } else if (dsa_vector_2[v]==21) {
                     pfs_matrix <- sd_pfs_dsa_lower
                   } else if (dsa_vector_2[v]==22) {
                     pfs_matrix <- pd_pfs_dsa_lower
                   } else if (dsa_vector_2[v]==23) {
                     os_matrix <- cr_os_dsa_lower
                   } else if (dsa_vector_2[v]==24) {
                     os_matrix <- vgpr_os_dsa_lower
                   } else if (dsa_vector_2[v]==25) {
                     os_matrix <- pr_os_dsa_lower
                   } else if (dsa_vector_2[v]==26) {
                     os_matrix <- mr_os_dsa_lower
                   } else if (dsa_vector_2[v]==27) {
                     os_matrix <- sd_os_dsa_lower
                   } else if (dsa_vector_2[v]==28) {
                     os_matrix <- pd_os_dsa_lower
                   } else if (dsa_vector_2[v]==29) {
                     comp_costs_1 <- comp_costs_1_lower
                   } else if (dsa_vector_2[v]==30) {
                     comp_costs_2 <- comp_costs_2_lower
                     comp_costs_3 <- comp_costs_3_lower
                     comp_costs_4 <- comp_costs_4_lower
                   }
                   else {
                     pfs_matrix <- "ERROR in DSA variable selection"
                     os_matrix <- "ERROR in DSA variable selection"
                   }
                   
                   
                   cost_cycle1 <- number_of_doses_1[1,]*comp_costs_1 + number_of_doses_1[2,]*comp_costs_2 + number_of_doses_1[3,]*comp_costs_3 + number_of_doses_1[3,]*comp_costs_3
                   cost_cycle2 <- number_of_doses_2[1,]*comp_costs_1 + number_of_doses_2[2,]*comp_costs_2 + number_of_doses_2[3,]*comp_costs_3 + number_of_doses_2[3,]*comp_costs_3
                   cost_cycle3 <- number_of_doses_3[1,]*comp_costs_1 + number_of_doses_3[2,]*comp_costs_2 + number_of_doses_3[3,]*comp_costs_3 + number_of_doses_3[3,]*comp_costs_3
                   cost_cycle4 <- number_of_doses_4[1,]*comp_costs_1 + number_of_doses_4[2,]*comp_costs_2 + number_of_doses_4[3,]*comp_costs_3 + number_of_doses_4[3,]*comp_costs_3 
                   cost_cycle5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0)
                   cost_cycle6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                   cost_cycle7 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                   cost_cycle8 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                   
                   cost_matrix <- rbind(cost_cycle1, cost_cycle2, cost_cycle3, cost_cycle4, cost_cycle5, cost_cycle6, cost_cycle7, cost_cycle8)
                   
                   cost_nodara <- cost_matrix[ ,-c(1,5,6,14,15,16,22,23)]
                   cost_matrix$dummy <- rowMeans(cost_nodara) #amit a negyedik line-t?l kap, az a daratumumab n?lk?li ter?pi?k ?tlaga
                   
                   per_patient <- data.frame(n=numeric(), asct_elig=numeric(), venvd_elig=numeric(), cytrisk=numeric(),
                                             l1_tx_int=numeric(), asct2_tx_int=numeric(), maint_tx_int=numeric(), l2_tx_int=numeric(), l3_tx_int=numeric(),
                                             date_prog_l1_int=numeric(), date_prog_asct2_int=numeric(), date_prog_maint_int=numeric(), date_prog_l2_int=numeric(), date_prog_l3_int=numeric(),
                                             date_relapse_l1_int=numeric(), date_relapse_asct2_int=numeric(), date_relapse_maint_int=numeric(), date_relapse_l2_int=numeric(), date_relapse_l3_int=numeric(), date_death_int=numeric(), cost_int=numeric(), cost_int_disc=numeric(),
                                             l1_tx_comp=numeric(), asct2_tx_comp=numeric(), maint_tx_comp=numeric(), l2_tx_comp=numeric(), l3_tx_comp=numeric(),
                                             date_prog_l1_comp=numeric(), date_prog_asct2_comp=numeric(), date_prog_int_comp=numeric(), date_prog_l2_comp=numeric(), date_prog_l3_comp=numeric(),
                                             date_relapse_l1_comp=numeric(), date_relapse_asct2_comp=numeric(), date_relapse_maint_comp=numeric(), date_relapse_l2_comp=numeric(), date_relapse_l3_comp=numeric(), date_death_comp=numeric(), cost_comp=numeric(), cost_comp_disc=numeric(),
                                             date_adverse_l1_int=numeric(), date_adverse_asct2_int=numeric(), date_adverse_maint_int=numeric(), date_adverse_l2_int=numeric(), date_adverse_l3_int=numeric(),
                                             date_adverse_l1_comp=numeric(), date_adverse_asct2_comp=numeric(), date_adverse_maint_comp=numeric(), date_adverse_l2_comp=numeric(), date_adverse_l3_comp=numeric())
                   
                   
                   for (n in 1:N) {
                     
                     #patient data per cycle
                     per_cycle <- data.frame(cycle=0, 
                                             treatment_int=0, tracker_int=0, cycle_track_int=0, current_line_int=0, toxic_int=1, cost_int=0,
                                             treatment_comp=0, tracker_comp=0, cycle_track_comp=0, current_line_comp=0, toxic_comp=1, cost_comp=0,
                                             l1_tx_int=0, l1_tx_comp=0, 
                                             date_adverse_l1_comp=0, date_adverse_2asct_comp=0, date_adverse_maint_comp=0, date_adverse_l2_comp=0, date_adverse_l3_comp=0,
                                             date_adverse_l1_int=0, date_adverse_2asct_int=0, date_adverse_maint_int=0, date_adverse_l2_int=0, date_adverse_l3_int=0,
                                             response_int=0, response_comp=0, relapse_int=1, relapse_comp=1, 
                                             date_death_int=0, date_prog_l1_int=0, date_prog_2asct_int=0, date_prog_l2_int=0, date_prog_l3_int=0, date_relapse_l1_int=0, date_relapse_2asct_int=0, date_relapse_l2_int=0, date_relapse_l3_int=0,
                                             date_death_comp=0, date_prog_l1_comp=0, date_prog_2asct_comp=0, date_prog_l2_comp=0, date_prog_l3_comp=0, date_relapse_l1_comp=0, date_relapse_2asct_comp=0, date_relapse_l2_comp=0, date_relapse_l3_comp=0,
                                             asct2_int=0, asct2_comp=0 ) 
                     
                     #Baseline Patient data
                     asct_elig <- population[n,1]
                     venvd_elig <- population[n,2]
                     cytrisk <- population[n,3]
                     patient_seed_int <- population[n, 4]
                     patient_seed_comp <- population[n, 5]
                     
                     #==== III.1.1. Cycle level simulation====
                     for (i in 1:number_of_cycles) {
                       
                       seed_int <- as.integer(paste0(patient_seed_int, cycle_seed[i]))
                       seed_comp <- as.integer(paste0(patient_seed_comp, cycle_seed[i]))
                       seed_int_2 <- as.integer(paste0(patient_seed_int, cycle_seed[i],2))
                       seed_comp_2 <- as.integer(paste0(patient_seed_comp, cycle_seed[i],2))
                       
                       #Treatment decision algoritm
                       treatment_vector <- treatment(asct_elig, venvd_elig, cytrisk, i, 
                                                     response_int = per_cycle[i,26], response_comp = per_cycle[i,27], 
                                                     treatment_int = per_cycle[i,2], treatment_comp = per_cycle[i,8], 
                                                     prev_line_int = per_cycle[i,5], prev_line_comp = per_cycle[i,11], 
                                                     tracker_int= per_cycle[i,3], tracker_comp = per_cycle[i,9], 
                                                     cycle_track_int = per_cycle[i,4], cycle_track_comp = per_cycle[i,10], 
                                                     toxic_int= per_cycle[i,6], toxic_comp= per_cycle[i,12],
                                                     relapse_int= per_cycle[i,28], relapse_comp= per_cycle[i,29], 
                                                     l1_tx_int= per_cycle[i,14], l1_tx_comp= per_cycle[i,15], 
                                                     date_adverse_l1_comp=per_cycle[i,16], date_adverse_2asct_comp=per_cycle[i,17], date_adverse_maint_comp=per_cycle[i,18],date_adverse_l2_comp=per_cycle[i,19],date_adverse_l3_comp=per_cycle[i,20],
                                                     date_adverse_l1_int=per_cycle[i,21],date_adverse_2asct_int=per_cycle[i,22],date_adverse_maint_int=per_cycle[i,23],date_adverse_l2_int=per_cycle[i,24],date_adverse_l3_int=per_cycle[i,25],
                                                     asct2_int=per_cycle[i,48], asct2_comp=per_cycle[i,49], 
                                                     seed_int, seed_comp, 
                                                     max_cycles_vector, cost_matrix, p_adverse_tx_vector, p_cr_vector, p_vgpr_vector, p_pr_vector, p_mr_vector, p_sd_vector, p_pd_vector, maint_better, scenario, dara2l,  rates_vector, dara_retr)
                       
                       #translate current_line from treatment to current line used in outcome
                       current_line_int <- if (treatment_vector[4]==1 | treatment_vector[4]==2 |treatment_vector[4]==5) {1} 
                       else if (treatment_vector[4]==3) {2} 
                       else {3}
                       
                       current_line_comp <- if (treatment_vector[11]==1 | treatment_vector[11]==2 |treatment_vector[11]==5) {1} 
                       else if (treatment_vector[11]==3) {2} 
                       else {3}
                       
                       outcome_vector <- outcome(i, current_line_int, current_line_comp, treatment_vector_line_int=treatment_vector[4], treatment_vector_line_comp=treatment_vector[11], 
                                                 response_int=treatment_vector[5], response_comp=treatment_vector[12], cytrisk,
                                                 pfs_matrix, os_matrix, 
                                                 seed_int_2, seed_comp_2)
                       
                       per_cycle[i+1,] <- c(i, treatment_vector[-c(5,12,27,28)], outcome_vector, treatment_vector[c(27,28)])
                     }#cycle simulation end
                     
                     #discounting
                     per_cycle <- per_cycle %>% mutate(cost_int_disc = cost_int/((1+disc_cost)^(floor(cycle*0.25))),cost_comp_disc = cost_comp/((1+disc_cost)^(floor(cycle*0.25)))) 
                     
                     #takes per_patient table variables one_by_one
                     l1_tx_int <- max(per_cycle$l1_tx_int)
                     asct2_tx_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else {filter(per_cycle, current_line_int==5)$treatment_int[1]}
                     maint_tx_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else {filter(per_cycle, current_line_int==2)$treatment_int[1]}
                     l2_tx_int <- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{filter(per_cycle, current_line_int==3)$treatment_int[1]}
                     l3_tx_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{filter(per_cycle, current_line_int==4)$treatment_int[1]}
                     date_prog_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_prog_l1_int)}
                     date_prog_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_prog_2asct_int)}
                     date_prog_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{max(filter(per_cycle, current_line_int==2)$date_prog_l1_int)}
                     date_prog_l2_int <- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_prog_l2_int)}
                     date_prog_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_prog_l3_int)}
                     
                     date_relapse_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_relapse_l1_int)}
                     date_relapse_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_relapse_2asct_int)}
                     date_relapse_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{max(filter(per_cycle, current_line_int==2)$date_relapse_l1_int)}
                     date_relapse_l2_int<- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_relapse_l2_int)}
                     date_relapse_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_relapse_l3_int)}
                     date_death_int <- max(per_cycle$date_death_int)
                     cost_int <- sum(per_cycle$cost_int)
                     cost_int_disc <- sum(per_cycle$cost_int_disc)
                     #Adverse Event date int
                     date_adverse_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_adverse_l1_int)}
                     date_adverse_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_adverse_2asct_int)}
                     date_adverse_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{filter(per_cycle, current_line_int==2)$date_adverse_maint_int[1]}
                     date_adverse_l2_int<- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_adverse_l2_int)}
                     date_adverse_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_adverse_l3_int)}
                     
                     l1_tx_comp <- max(unique(per_cycle$l1_tx_comp))
                     asct2_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{filter(per_cycle, current_line_comp==5)$treatment_comp[1]}
                     maint_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{filter(per_cycle, current_line_comp==2)$treatment_comp[1]}
                     l2_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{filter(per_cycle, current_line_comp==3)$treatment_comp[1]}
                     l3_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{filter(per_cycle, current_line_comp==4)$treatment_comp[1]}
                     date_prog_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp)}
                     date_prog_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_prog_2asct_comp)}
                     date_prog_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp)}
                     date_prog_l2_comp <- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_prog_l2_comp)}
                     date_prog_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_prog_l3_comp)}
                     
                     date_relapse_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_relapse_l1_comp)}
                     date_relapse_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_relapse_2asct_comp)}
                     date_relapse_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{max(filter(per_cycle, current_line_comp==2)$date_relapse_l1_comp)}
                     date_relapse_l2_comp<- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_relapse_l2_comp)}
                     date_relapse_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_relapse_l3_comp)}
                     date_death_comp <- max(per_cycle$date_death_comp)
                     cost_comp <- sum(per_cycle$cost_comp)
                     cost_comp_disc <- sum(per_cycle$cost_comp_disc)
                     
                     #Adverse Event date comp
                     date_adverse_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_adverse_l1_comp)}
                     date_adverse_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_adverse_2asct_comp)}
                     date_adverse_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{filter(per_cycle, current_line_comp==2)$date_adverse_maint_comp[1]}
                     date_adverse_l2_comp<- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_adverse_l2_comp)}
                     date_adverse_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_adverse_l3_comp)}
                     
                     
                     # a per cycle-bol k?sz?t egy per patient sort, ezt m?g ki k?ne tal?lnom.
                     per_patient[n, ] <- c(n, population[n,1],population[n,2],population[n,3],
                                           l1_tx_int, asct2_tx_int, maint_tx_int, l2_tx_int, l3_tx_int,
                                           date_prog_l1_int, date_prog_asct2_int, date_prog_maint_int, date_prog_l2_int, date_prog_l3_int,
                                           date_relapse_l1_int, date_relapse_asct2_int, date_relapse_maint_int, date_relapse_l2_int, date_relapse_l3_int, date_death_int, cost_int, cost_int_disc,
                                           l1_tx_comp, asct2_tx_comp, maint_tx_comp, l2_tx_comp, l3_tx_comp,
                                           date_prog_l1_comp, date_prog_asct2_comp, date_prog_maint_comp, date_prog_l2_comp, date_prog_l3_comp,
                                           date_relapse_l1_comp, date_relapse_asct2_comp, date_relapse_maint_comp, date_relapse_l2_comp, date_relapse_l3_comp, date_death_comp, cost_comp, cost_comp_disc,
                                           date_adverse_l1_int, date_adverse_asct2_int, date_adverse_maint_int, date_adverse_l2_int, date_adverse_l3_int,
                                           date_adverse_l1_comp, date_adverse_asct2_comp, date_adverse_maint_comp, date_adverse_l2_comp, date_adverse_l3_comp
                     )
                   }
                   
                   
                   ##### IV. Outputs ##################################################################
                   
                   LY_int <- (sum(per_patient$date_death_int)*0.25)/N
                   LY_comp <- (sum(per_patient$date_death_comp)*0.25)/N
                   delta_LY_lower <- LY_int - LY_comp
                   
                   total_cost_int <- (sum(per_patient$cost_int_disc))/N
                   total_cost_comp <- (sum(per_patient$cost_comp_disc))/N
                   delta_cost_lower <- total_cost_int-total_cost_comp
                   
                   per_variable_dsa[v,2] <- delta_cost_lower/delta_LY_lower
                   
                 }
                 
                 
                 ####################################################################################
                 ###################### UPPER BOUND MODEL RUN #######################################
                 ####################################################################################
                 
                 for (v in 1:no_variables){
                   
                   #vektorokba gyujt?tt inputok
                   max_cycles_vector <- c(100, input_final$max_cycles_vector[!is.na(input_final$max_cycles_vector)])
                   p_adverse_tx_vector <- input_final$p_adverse_tx_vector[!is.na(input_final$p_adverse_tx_vector)]
                   p_cr_vector <- input_final$p_cr_vector[!is.na(input_final$p_cr_vector)]
                   p_vgpr_vector <- input_final$p_vgpr_vector[!is.na(input_final$p_vgpr_vector)]
                   p_pr_vector <- input_final$p_pr_vector[!is.na(input_final$p_pr_vector)]
                   p_mr_vector <- input_final$p_mr_vector[!is.na(input_final$p_mr_vector)]
                   p_sd_vector <- input_final$p_sd_vector[!is.na(input_final$p_sd_vector)]
                   p_pd_vector <- input_final$p_pd_vector[!is.na(input_final$p_pd_vector)]
                   
                   #treatment rates (shiny)
                   rates_vector <- rep(NA, 36)
                   rates_vector[1] <- input$rate_1
                   rates_vector[2] <- input$rate_2
                   rates_vector[3] <- input$rate_3
                   rates_vector[4] <- input$rate_4
                   rates_vector[5] <- input$rate_5
                   rates_vector[6] <- input$rate_6
                   rates_vector[7] <- input$rate_7
                   rates_vector[8] <- input$rate_8
                   rates_vector[9] <- input$rate_9
                   rates_vector[10] <- input$rate_10
                   rates_vector[11] <- input$rate_11
                   rates_vector[12] <- input$rate_12
                   rates_vector[13] <- input$rate_13
                   rates_vector[14] <- input$rate_14
                   rates_vector[15] <- input$rate_15
                   rates_vector[16] <- input$rate_16
                   rates_vector[17] <- input$rate_17
                   rates_vector[18] <- input$rate_18
                   rates_vector[19] <- input$rate_19
                   rates_vector[20] <- input$rate_20
                   rates_vector[21] <- input$rate_21
                   rates_vector[22] <- input$rate_22
                   rates_vector[23] <- input$rate_23
                   rates_vector[24] <- input$rate_24
                   rates_vector[25] <- input$rate_25
                   rates_vector[26] <- input$rate_26
                   rates_vector[27] <- input$rate_27
                   rates_vector[28] <- input$rate_28
                   rates_vector[29] <- input$rate_29
                   rates_vector[30] <- input$rate_30
                   rates_vector[31] <- input$rate_31
                   rates_vector[32] <- input$rate_32
                   rates_vector[33] <- input$rate_33
                   rates_vector[34] <- input$rate_34
                   rates_vector[35] <- input$rate_35
                   rates_vector[36] <- input$rate_36
                   
                   rates_vector <- rates_vector/100
                   
                   
                   #m?trixok is vannak
                   pfs_matrix <- cbind(input_final$l1_pfs_vector[!is.na(input_final$l1_pfs_vector)], input_final$l2_pfs_vector[!is.na(input_final$l2_pfs_vector)], input_final$l3_pfs_vector[!is.na(input_final$l3_pfs_vector)])
                   os_matrix <- cbind(input_final$l1_os_vector[!is.na(input_final$l1_os_vector)], input_final$l2_os_vector[!is.na(input_final$l2_os_vector)], input_final$l3_os_vector[!is.na(input_final$l3_os_vector)])
                   
                   
                   #egyeb shiny inputok
                   dara2l <- input$dara2l
                   scenario <- input$scenario
                   asct_rate <- input$asct_rate/100
                   venvd_rate <- input$venvd_rate/100
                   cytrisk_rate <- input$cytrisk_rate/100
                   N <- input$N
                   disc_cost <- input$disc_cost/100
                   dara_retr <- input$dara_retreat
                   
                   
                   #marad?k
                   number_of_cycles <- input_final$number_of_cycles[!is.na(input_final$number_of_cycles)]
                   maint_better <- input_final$maint_better[!is.na(input_final$maint_better)]
                   
                   #cost matrix (shiny)
                   cost_D <- input$cost_D
                   cost_K <- input$cost_K
                   cost_M <- input$cost_M
                   cost_Pom <- input$cost_Pom
                   cost_R <- input$cost_R
                   cost_S <- input$cost_S
                   cost_V <- input$cost_V
                   cost_Ven <- input$cost_Ven
                   cost_Ixa <- input$cost_Ixa
                   cost_Isa <- input$cost_Isa
                   cost_dd <- input$cost_dd
                   cost_P <- input$cost_P
                   cost_T <- input$cost_T
                   cost_C <- input$cost_C
                   
                   box_dose_D <- 1
                   box_dose_K <- 1.68
                   box_dose_Pom <- 0.0476
                   box_dose_R <- 0.1905
                   box_dose_V <- 0.6686
                   box_dose_Ven <- 0.2857
                   box_dose_Ixa <- 0.3333
                   box_dose_dd <- 0.04
                   box_dose_P <- 0.216
                   box_dose_T <- 0.1429
                   box_dose_M <- 1
                   box_dose_S <- 1
                   box_dose_Isa <- 1
                   box_dose_C <- 1
                   
                   number_of_doses_1 <- read.csv("no_admin_1.csv") #number of administration in the first cycle
                   number_of_doses_2 <- read.csv("no_admin_2.csv") #number of adminsitration in the second cycle
                   number_of_doses_3 <- read.csv("no_admin_3.csv") #number of adminsitration in the third cycle
                   number_of_doses_4 <- read.csv("no_admin_4.csv") #number of adminsitration in the fourth cycle
                   
                   comp_costs_1 <- c(cost_D, 0, 0, 0, cost_D, cost_D, 0, 0, 0, 0, 0, 0, 0, cost_D, cost_D, cost_D, 0, 0, 0, 0, 0, 0, cost_D, 0)
                   comp_costs_2 <- c(cost_V, cost_V, cost_V, cost_V, 0, cost_V, cost_K, cost_Ixa, cost_Ven, cost_Isa, cost_Pom, cost_S, cost_K, cost_V, cost_K, cost_Pom, cost_Isa, cost_V, cost_R, cost_V, 0, 0, 0, cost_V)
                   comp_costs_3 <- c(cost_T, cost_R, cost_T, cost_C, cost_R, cost_M, cost_R, cost_R, cost_V, cost_K, cost_V, cost_V, 0, 0, 0, 0, cost_Pom, 0, 0, cost_M, cost_R, 0, cost_R, cost_R)
                   comp_costs_4 <- c(cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_P, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, cost_dd, 0, 0, cost_P, cost_dd, 0, cost_dd, cost_dd)
                   
                   comp_costs_1 <- comp_costs_1*box_dose_D
                   comp_costs_2 <- c(cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, cost_V*box_dose_V, 0, cost_V*box_dose_V, cost_K*box_dose_K, cost_Ixa*box_dose_Ixa, cost_Ven*box_dose_Ven, cost_Isa*box_dose_Isa, 
                                     cost_Pom*box_dose_Pom, cost_S*box_dose_S, cost_K*box_dose_K, cost_V*box_dose_V, cost_K*box_dose_K, cost_Pom*box_dose_Pom, cost_Isa*box_dose_Isa, cost_V*box_dose_V, cost_R*box_dose_R, cost_V*box_dose_V, 0, 0, 0, cost_V*box_dose_V)
                   comp_costs_3 <- c(cost_T*box_dose_T, cost_R*box_dose_R, cost_T*box_dose_T, cost_C*box_dose_C, cost_R*box_dose_R, cost_M*box_dose_M, cost_R*box_dose_R, cost_R*box_dose_R, 
                                     cost_V*box_dose_V, cost_K*box_dose_K, cost_V*box_dose_V, cost_V*box_dose_V, 0, 0, 0, 0, cost_Pom*box_dose_Pom, 0, 0, cost_M*box_dose_M, cost_R*box_dose_R, 0, cost_R*box_dose_R, cost_R*box_dose_R)
                   comp_costs_4 <- c(cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_P*box_dose_P, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 
                                     cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, cost_dd*box_dose_dd, 0, 0, cost_P*box_dose_P, cost_dd*box_dose_dd, 0, cost_dd*box_dose_dd, cost_dd*box_dose_dd)
                   
                   #changes the right input
                   if (dsa_vector_2[v]==1) {
                     max_cycles_vector <- max_cycles_dsa_upper
                   } else if (dsa_vector_2[v]==2) {
                     maint_better <- maint_better_dsa_upper
                   } else if (dsa_vector_2[v]==3) {
                     p_adverse_tx_vector <- adverse_dara_dsa_upper
                   } else if (dsa_vector_2[v]==4) {
                     p_adverse_tx_vector <- adverse_other_dsa_upper
                   } else if (dsa_vector_2[v]==5) {
                     p_cr_vector <- cr_dara_dsa_upper
                     p_vgpr_vector <- response_matrix_dara_cr_upper[2,]
                     p_pr_vector <- response_matrix_dara_cr_upper[3,]
                     p_mr_vector <- response_matrix_dara_cr_upper[4,]
                     p_sd_vector <- response_matrix_dara_cr_upper[5,]
                     p_pd_vector <- response_matrix_dara_cr_upper[6,]
                   } else if (dsa_vector_2[v]==6) {
                     p_cr_vector <- cr_other_dsa_upper
                     p_vgpr_vector <- response_matrix_other_cr_upper[2,]
                     p_pr_vector <- response_matrix_other_cr_upper[3,]
                     p_mr_vector <- response_matrix_other_cr_upper[4,]
                     p_sd_vector <- response_matrix_other_cr_upper[5,]
                     p_pd_vector <- response_matrix_other_cr_upper[6,]
                   } else if (dsa_vector_2[v]==7) {
                     p_vgpr_vector <- vgpr_dara_dsa_upper
                     p_cr_vector <- response_matrix_dara_vgpr_upper[2,]
                     p_pr_vector <- response_matrix_dara_vgpr_upper[3,]
                     p_mr_vector <- response_matrix_dara_vgpr_upper[4,]
                     p_sd_vector <- response_matrix_dara_vgpr_upper[5,]
                     p_pd_vector <- response_matrix_dara_vgpr_upper[6,]
                   } else if (dsa_vector_2[v]==8) {
                     p_vgpr_vector <- vgpr_other_dsa_upper
                     p_cr_vector <- response_matrix_other_vgpr_upper[2,]
                     p_pr_vector <- response_matrix_other_vgpr_upper[3,]
                     p_mr_vector <- response_matrix_other_vgpr_upper[4,]
                     p_sd_vector <- response_matrix_other_vgpr_upper[5,]
                     p_pd_vector <- response_matrix_other_vgpr_upper[6,]
                   } else if (dsa_vector_2[v]==9) {
                     p_pr_vector <- pr_dara_dsa_upper
                     p_cr_vector <- response_matrix_dara_pr_upper[2,]
                     p_vgpr_vector <- response_matrix_dara_pr_upper[3,]
                     p_mr_vector <- response_matrix_dara_pr_upper[4,]
                     p_sd_vector <- response_matrix_dara_pr_upper[5,]
                     p_pd_vector <- response_matrix_dara_pr_upper[6,]
                   } else if (dsa_vector_2[v]==10) {
                     p_pr_vector <- pr_other_dsa_upper
                     p_cr_vector <- response_matrix_other_pr_upper[2,]
                     p_vgpr_vector <- response_matrix_other_pr_upper[3,]
                     p_mr_vector <- response_matrix_other_pr_upper[4,]
                     p_sd_vector <- response_matrix_other_pr_upper[5,]
                     p_pd_vector <- response_matrix_other_pr_upper[6,]
                   } else if (dsa_vector_2[v]==11) {
                     p_mr_vector <- mr_dara_dsa_upper
                     p_cr_vector <- response_matrix_dara_mr_upper[2,]
                     p_vgpr_vector <- response_matrix_dara_mr_upper[3,]
                     p_pr_vector <- response_matrix_dara_mr_upper[4,]
                     p_sd_vector <- response_matrix_dara_mr_upper[5,]
                     p_pd_vector <- response_matrix_dara_mr_upper[6,]
                   } else if (dsa_vector_2[v]==12) {
                     p_mr_vector <- mr_other_dsa_upper
                     p_cr_vector <- response_matrix_other_mr_upper[2,]
                     p_vgpr_vector <- response_matrix_other_mr_upper[3,]
                     p_pr_vector <- response_matrix_other_mr_upper[4,]
                     p_sd_vector <- response_matrix_other_mr_upper[5,]
                     p_pd_vector <- response_matrix_other_mr_upper[6,]
                   } else if (dsa_vector_2[v]==13) {
                     p_sd_vector <- sd_dara_dsa_upper
                     p_cr_vector <- response_matrix_dara_sd_upper[2,]
                     p_vgpr_vector <- response_matrix_dara_sd_upper[3,]
                     p_pr_vector <- response_matrix_dara_sd_upper[4,]
                     p_mr_vector <- response_matrix_dara_sd_upper[5,]
                     p_pd_vector <- response_matrix_dara_sd_upper[6,]
                   } else if (dsa_vector_2[v]==14) {
                     p_sd_vector <- sd_other_dsa_upper
                     p_cr_vector <- response_matrix_other_sd_upper[2,]
                     p_vgpr_vector <- response_matrix_other_sd_upper[3,]
                     p_pr_vector <- response_matrix_other_sd_upper[4,]
                     p_mr_vector <- response_matrix_other_sd_upper[5,]
                     p_pd_vector <- response_matrix_other_sd_upper[6,]
                   } else if (dsa_vector_2[v]==15) {
                     p_pd_vector <- pd_dara_dsa_upper
                     p_cr_vector <- response_matrix_dara_pd_upper[2,]
                     p_vgpr_vector <- response_matrix_dara_pd_upper[3,]
                     p_pr_vector <- response_matrix_dara_pd_upper[4,]
                     p_mr_vector <- response_matrix_dara_pd_upper[5,]
                     p_sd_vector <- response_matrix_dara_pd_upper[6,]
                   } else if (dsa_vector_2[v]==16) {
                     p_pd_vector <- pd_other_dsa_upper
                     p_cr_vector <- response_matrix_other_pd_upper[2,]
                     p_vgpr_vector <- response_matrix_other_pd_upper[3,]
                     p_pr_vector <- response_matrix_other_pd_upper[4,]
                     p_mr_vector <- response_matrix_other_pd_upper[5,]
                     p_sd_vector <- response_matrix_other_pd_upper[6,]
                   } else if (dsa_vector_2[v]==17) {
                     pfs_matrix <- cr_pfs_dsa_upper
                   } else if (dsa_vector_2[v]==18) {
                     pfs_matrix <- vgpr_pfs_dsa_upper
                   } else if (dsa_vector_2[v]==19) {
                     pfs_matrix <- pr_pfs_dsa_upper
                   } else if (dsa_vector_2[v]==20) {
                     pfs_matrix <- mr_pfs_dsa_upper
                   } else if (dsa_vector_2[v]==21) {
                     pfs_matrix <- sd_pfs_dsa_upper
                   } else if (dsa_vector_2[v]==22) {
                     pfs_matrix <- pd_pfs_dsa_upper
                   } else if (dsa_vector_2[v]==23) {
                     os_matrix <- cr_os_dsa_upper
                   } else if (dsa_vector_2[v]==24) {
                     os_matrix <- vgpr_os_dsa_upper
                   } else if (dsa_vector_2[v]==25) {
                     os_matrix <- pr_os_dsa_upper
                   } else if (dsa_vector_2[v]==26) {
                     os_matrix <- mr_os_dsa_upper
                   } else if (dsa_vector_2[v]==27) {
                     os_matrix <- sd_os_dsa_upper
                   } else if (dsa_vector_2[v]==28) {
                     os_matrix <- pd_os_dsa_upper
                   } else if (dsa_vector_2[v]==29) {
                     comp_costs_1 <- comp_costs_1_upper
                   } else if (dsa_vector_2[v]==30) {
                     comp_costs_2 <- comp_costs_2_upper
                     comp_costs_3 <- comp_costs_3_upper
                     comp_costs_4 <- comp_costs_4_upper
                   }
                   else {
                     pfs_matrix <- "ERROR in DSA variable selection"
                     os_matrix <- "ERROR in DSA variable selection"
                   }
                   
                   
                   cost_cycle1 <- number_of_doses_1[1,]*comp_costs_1 + number_of_doses_1[2,]*comp_costs_2 + number_of_doses_1[3,]*comp_costs_3 + number_of_doses_1[3,]*comp_costs_3
                   cost_cycle2 <- number_of_doses_2[1,]*comp_costs_1 + number_of_doses_2[2,]*comp_costs_2 + number_of_doses_2[3,]*comp_costs_3 + number_of_doses_2[3,]*comp_costs_3
                   cost_cycle3 <- number_of_doses_3[1,]*comp_costs_1 + number_of_doses_3[2,]*comp_costs_2 + number_of_doses_3[3,]*comp_costs_3 + number_of_doses_3[3,]*comp_costs_3
                   cost_cycle4 <- number_of_doses_4[1,]*comp_costs_1 + number_of_doses_4[2,]*comp_costs_2 + number_of_doses_4[3,]*comp_costs_3 + number_of_doses_4[3,]*comp_costs_3 
                   cost_cycle5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0)
                   cost_cycle6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                   cost_cycle7 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                   cost_cycle8 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2*cost_V, 2*cost_R,0,0,0,0,0) 
                   
                   cost_matrix <- rbind(cost_cycle1, cost_cycle2, cost_cycle3, cost_cycle4, cost_cycle5, cost_cycle6, cost_cycle7, cost_cycle8)
                   
                   cost_nodara <- cost_matrix[ ,-c(1,5,6,14,15,16,22,23)]
                   cost_matrix$dummy <- rowMeans(cost_nodara) #amit a negyedik line-t?l kap, az a daratumumab n?lk?li ter?pi?k ?tlaga
                   
                   per_patient <- data.frame(n=numeric(), asct_elig=numeric(), venvd_elig=numeric(), cytrisk=numeric(),
                                             l1_tx_int=numeric(), asct2_tx_int=numeric(), maint_tx_int=numeric(), l2_tx_int=numeric(), l3_tx_int=numeric(),
                                             date_prog_l1_int=numeric(), date_prog_asct2_int=numeric(), date_prog_maint_int=numeric(), date_prog_l2_int=numeric(), date_prog_l3_int=numeric(),
                                             date_relapse_l1_int=numeric(), date_relapse_asct2_int=numeric(), date_relapse_maint_int=numeric(), date_relapse_l2_int=numeric(), date_relapse_l3_int=numeric(), date_death_int=numeric(), cost_int=numeric(), cost_int_disc=numeric(),
                                             l1_tx_comp=numeric(), asct2_tx_comp=numeric(), maint_tx_comp=numeric(), l2_tx_comp=numeric(), l3_tx_comp=numeric(),
                                             date_prog_l1_comp=numeric(), date_prog_asct2_comp=numeric(), date_prog_int_comp=numeric(), date_prog_l2_comp=numeric(), date_prog_l3_comp=numeric(),
                                             date_relapse_l1_comp=numeric(), date_relapse_asct2_comp=numeric(), date_relapse_maint_comp=numeric(), date_relapse_l2_comp=numeric(), date_relapse_l3_comp=numeric(), date_death_comp=numeric(), cost_comp=numeric(), cost_comp_disc=numeric(),
                                             date_adverse_l1_int=numeric(), date_adverse_asct2_int=numeric(), date_adverse_maint_int=numeric(), date_adverse_l2_int=numeric(), date_adverse_l3_int=numeric(),
                                             date_adverse_l1_comp=numeric(), date_adverse_asct2_comp=numeric(), date_adverse_maint_comp=numeric(), date_adverse_l2_comp=numeric(), date_adverse_l3_comp=numeric())
                   
                   
                   for (n in 1:N) {
                     
                     #patient data per cycle
                     per_cycle <- data.frame(cycle=0, 
                                             treatment_int=0, tracker_int=0, cycle_track_int=0, current_line_int=0, toxic_int=1, cost_int=0,
                                             treatment_comp=0, tracker_comp=0, cycle_track_comp=0, current_line_comp=0, toxic_comp=1, cost_comp=0,
                                             l1_tx_int=0, l1_tx_comp=0, 
                                             date_adverse_l1_comp=0, date_adverse_2asct_comp=0, date_adverse_maint_comp=0, date_adverse_l2_comp=0, date_adverse_l3_comp=0,
                                             date_adverse_l1_int=0, date_adverse_2asct_int=0, date_adverse_maint_int=0, date_adverse_l2_int=0, date_adverse_l3_int=0,
                                             response_int=0, response_comp=0, relapse_int=1, relapse_comp=1, 
                                             date_death_int=0, date_prog_l1_int=0, date_prog_2asct_int=0, date_prog_l2_int=0, date_prog_l3_int=0, date_relapse_l1_int=0, date_relapse_2asct_int=0, date_relapse_l2_int=0, date_relapse_l3_int=0,
                                             date_death_comp=0, date_prog_l1_comp=0, date_prog_2asct_comp=0, date_prog_l2_comp=0, date_prog_l3_comp=0, date_relapse_l1_comp=0, date_relapse_2asct_comp=0, date_relapse_l2_comp=0, date_relapse_l3_comp=0,
                                             asct2_int=0, asct2_comp=0 ) 
                     
                     #Baseline Patient data
                     asct_elig <- population[n,1]
                     venvd_elig <- population[n,2]
                     cytrisk <- population[n,3]
                     patient_seed_int <- population[n, 4]
                     patient_seed_comp <- population[n, 5]
                     
                     #==== III.1.1. Cycle level simulation====
                     for (i in 1:number_of_cycles) {
                       
                       seed_int <- as.integer(paste0(patient_seed_int, cycle_seed[i]))
                       seed_comp <- as.integer(paste0(patient_seed_comp, cycle_seed[i]))
                       seed_int_2 <- as.integer(paste0(patient_seed_int, cycle_seed[i],2))
                       seed_comp_2 <- as.integer(paste0(patient_seed_comp, cycle_seed[i],2))
                       
                       #Treatment decision algoritm
                       treatment_vector <- treatment(asct_elig, venvd_elig, cytrisk, i, 
                                                     response_int = per_cycle[i,26], response_comp = per_cycle[i,27], 
                                                     treatment_int = per_cycle[i,2], treatment_comp = per_cycle[i,8], 
                                                     prev_line_int = per_cycle[i,5], prev_line_comp = per_cycle[i,11], 
                                                     tracker_int= per_cycle[i,3], tracker_comp = per_cycle[i,9], 
                                                     cycle_track_int = per_cycle[i,4], cycle_track_comp = per_cycle[i,10], 
                                                     toxic_int= per_cycle[i,6], toxic_comp= per_cycle[i,12],
                                                     relapse_int= per_cycle[i,28], relapse_comp= per_cycle[i,29], 
                                                     l1_tx_int= per_cycle[i,14], l1_tx_comp= per_cycle[i,15], 
                                                     date_adverse_l1_comp=per_cycle[i,16], date_adverse_2asct_comp=per_cycle[i,17], date_adverse_maint_comp=per_cycle[i,18],date_adverse_l2_comp=per_cycle[i,19],date_adverse_l3_comp=per_cycle[i,20],
                                                     date_adverse_l1_int=per_cycle[i,21],date_adverse_2asct_int=per_cycle[i,22],date_adverse_maint_int=per_cycle[i,23],date_adverse_l2_int=per_cycle[i,24],date_adverse_l3_int=per_cycle[i,25],
                                                     asct2_int=per_cycle[i,48], asct2_comp=per_cycle[i,49], 
                                                     seed_int, seed_comp, 
                                                     max_cycles_vector, cost_matrix, p_adverse_tx_vector, p_cr_vector, p_vgpr_vector, p_pr_vector, p_mr_vector, p_sd_vector, p_pd_vector, maint_better, scenario, dara2l,  rates_vector, dara_retr)
                       
                       #translate current_line from treatment to current line used in outcome
                       current_line_int <- if (treatment_vector[4]==1 | treatment_vector[4]==2 |treatment_vector[4]==5) {1} 
                       else if (treatment_vector[4]==3) {2} 
                       else {3}
                       
                       current_line_comp <- if (treatment_vector[11]==1 | treatment_vector[11]==2 |treatment_vector[11]==5) {1} 
                       else if (treatment_vector[11]==3) {2} 
                       else {3}
                       
                       outcome_vector <- outcome(i, current_line_int, current_line_comp, treatment_vector_line_int=treatment_vector[4], treatment_vector_line_comp=treatment_vector[11], 
                                                 response_int=treatment_vector[5], response_comp=treatment_vector[12], cytrisk,
                                                 pfs_matrix, os_matrix, 
                                                 seed_int_2, seed_comp_2)
                       
                       per_cycle[i+1,] <- c(i, treatment_vector[-c(5,12,27,28)], outcome_vector, treatment_vector[c(27,28)])
                     }#cycle simulation end
                     
                     #discounting
                     per_cycle <- per_cycle %>% mutate(cost_int_disc = cost_int/((1+disc_cost)^(floor(cycle*0.25))),cost_comp_disc = cost_comp/((1+disc_cost)^(floor(cycle*0.25)))) 
                     
                     #takes per_patient table variables one_by_one
                     l1_tx_int <- max(per_cycle$l1_tx_int)
                     asct2_tx_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else {filter(per_cycle, current_line_int==5)$treatment_int[1]}
                     maint_tx_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else {filter(per_cycle, current_line_int==2)$treatment_int[1]}
                     l2_tx_int <- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{filter(per_cycle, current_line_int==3)$treatment_int[1]}
                     l3_tx_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{filter(per_cycle, current_line_int==4)$treatment_int[1]}
                     date_prog_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_prog_l1_int)}
                     date_prog_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_prog_2asct_int)}
                     date_prog_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{max(filter(per_cycle, current_line_int==2)$date_prog_l1_int)}
                     date_prog_l2_int <- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_prog_l2_int)}
                     date_prog_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_prog_l3_int)}
                     
                     date_relapse_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_relapse_l1_int)}
                     date_relapse_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_relapse_2asct_int)}
                     date_relapse_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{max(filter(per_cycle, current_line_int==2)$date_relapse_l1_int)}
                     date_relapse_l2_int<- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_relapse_l2_int)}
                     date_relapse_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_relapse_l3_int)}
                     date_death_int <- max(per_cycle$date_death_int)
                     cost_int <- sum(per_cycle$cost_int)
                     cost_int_disc <- sum(per_cycle$cost_int_disc)
                     #Adverse Event date int
                     date_adverse_l1_int <- if(nrow(filter(per_cycle, current_line_int==1))==0) {0} else{max(filter(per_cycle, current_line_int==1)$date_adverse_l1_int)}
                     date_adverse_asct2_int <- if(nrow(filter(per_cycle, current_line_int==5))==0) {0} else{max(filter(per_cycle, current_line_int==5)$date_adverse_2asct_int)}
                     date_adverse_maint_int <- if(nrow(filter(per_cycle, current_line_int==2))==0) {0} else{filter(per_cycle, current_line_int==2)$date_adverse_maint_int[1]}
                     date_adverse_l2_int<- if(nrow(filter(per_cycle, current_line_int==3))==0) {0} else{max(filter(per_cycle, current_line_int==3)$date_adverse_l2_int)}
                     date_adverse_l3_int <- if(nrow(filter(per_cycle, current_line_int==4))==0) {0} else{max(filter(per_cycle, current_line_int==4)$date_adverse_l3_int)}
                     
                     l1_tx_comp <- max(unique(per_cycle$l1_tx_comp))
                     asct2_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{filter(per_cycle, current_line_comp==5)$treatment_comp[1]}
                     maint_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{filter(per_cycle, current_line_comp==2)$treatment_comp[1]}
                     l2_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{filter(per_cycle, current_line_comp==3)$treatment_comp[1]}
                     l3_tx_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{filter(per_cycle, current_line_comp==4)$treatment_comp[1]}
                     date_prog_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp)}
                     date_prog_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_prog_2asct_comp)}
                     date_prog_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_prog_l1_comp)}
                     date_prog_l2_comp <- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_prog_l2_comp)}
                     date_prog_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_prog_l3_comp)}
                     
                     date_relapse_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_relapse_l1_comp)}
                     date_relapse_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_relapse_2asct_comp)}
                     date_relapse_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{max(filter(per_cycle, current_line_comp==2)$date_relapse_l1_comp)}
                     date_relapse_l2_comp<- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_relapse_l2_comp)}
                     date_relapse_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_relapse_l3_comp)}
                     date_death_comp <- max(per_cycle$date_death_comp)
                     cost_comp <- sum(per_cycle$cost_comp)
                     cost_comp_disc <- sum(per_cycle$cost_comp_disc)
                     
                     #Adverse Event date comp
                     date_adverse_l1_comp <- if(nrow(filter(per_cycle, current_line_comp==1))==0) {0} else{max(filter(per_cycle, current_line_comp==1)$date_adverse_l1_comp)}
                     date_adverse_asct2_comp <- if(nrow(filter(per_cycle, current_line_comp==5))==0) {0} else{max(filter(per_cycle, current_line_comp==5)$date_adverse_2asct_comp)}
                     date_adverse_maint_comp <- if(nrow(filter(per_cycle, current_line_comp==2))==0) {0} else{filter(per_cycle, current_line_comp==2)$date_adverse_maint_comp[1]}
                     date_adverse_l2_comp<- if(nrow(filter(per_cycle, current_line_comp==3))==0) {0} else{max(filter(per_cycle, current_line_comp==3)$date_adverse_l2_comp)}
                     date_adverse_l3_comp <- if(nrow(filter(per_cycle, current_line_comp==4))==0) {0} else{max(filter(per_cycle, current_line_comp==4)$date_adverse_l3_comp)}
                     
                     
                     # a per cycle-bol k?sz?t egy per patient sort, ezt m?g ki k?ne tal?lnom.
                     per_patient[n, ] <- c(n, population[n,1],population[n,2],population[n,3],
                                           l1_tx_int, asct2_tx_int, maint_tx_int, l2_tx_int, l3_tx_int,
                                           date_prog_l1_int, date_prog_asct2_int, date_prog_maint_int, date_prog_l2_int, date_prog_l3_int,
                                           date_relapse_l1_int, date_relapse_asct2_int, date_relapse_maint_int, date_relapse_l2_int, date_relapse_l3_int, date_death_int, cost_int, cost_int_disc,
                                           l1_tx_comp, asct2_tx_comp, maint_tx_comp, l2_tx_comp, l3_tx_comp,
                                           date_prog_l1_comp, date_prog_asct2_comp, date_prog_maint_comp, date_prog_l2_comp, date_prog_l3_comp,
                                           date_relapse_l1_comp, date_relapse_asct2_comp, date_relapse_maint_comp, date_relapse_l2_comp, date_relapse_l3_comp, date_death_comp, cost_comp, cost_comp_disc,
                                           date_adverse_l1_int, date_adverse_asct2_int, date_adverse_maint_int, date_adverse_l2_int, date_adverse_l3_int,
                                           date_adverse_l1_comp, date_adverse_asct2_comp, date_adverse_maint_comp, date_adverse_l2_comp, date_adverse_l3_comp
                     )
                   }
                   
                   
                   ##### IV. Outputs ##################################################################
                   
                   LY_int <- (sum(per_patient$date_death_int)*0.25)/N
                   LY_comp <- (sum(per_patient$date_death_comp)*0.25)/N
                   delta_LY_upper <- LY_int - LY_comp
                   
                   total_cost_int <- (sum(per_patient$cost_int_disc))/N
                   total_cost_comp <- (sum(per_patient$cost_comp_disc))/N
                   delta_cost_upper <- total_cost_int-total_cost_comp
                   
                   per_variable_dsa[v,3] <- delta_cost_upper/delta_LY_upper
                   
                 }
               
               
               
               ### Tornado plot
               
              
                 
               # Inputs
               data <- data.frame(
                 var    = rep(var_list_dsa, 2),
                 level  = c(rep('ICER at lower bound', length(dsa_vector_2)), rep('ICER at upper bound', length(dsa_vector_2))),
                 result = c(per_variable_dsa[,2], per_variable_dsa[,3])
               )
               
               
               # "Center" the result around the baseline result (so baseline is at 0)
               df <- data %>%
                 mutate(result2 = result - ICER_baseline,
                        resultRange = stats::ave(abs(result2), var, FUN = sum))
               
               
              ten <- function(x) {
                 return(x[10:1])
               }
               
               
               # Compute labels for the x-axis
               lb0        <- floor(22*min(df$result2))/10
               ub0        <- ceiling(22*max(df$result2))/10
               breaks0    <- seq(lb0, ub0, (ub0 - lb0) / 5)
               breakLabs <- round(breaks0 + ICER_baseline, 0)
               
               
               
               ggplot(df, aes(x = result2,
                              y = stats::reorder(var, resultRange),
                              fill = level)) +
                 geom_col(width = 0.45, position = "identity") +
                 scale_fill_manual(values=c("deepskyblue2", "coral2")) +
                 geom_text(aes(label = scales::number(round(result,1)), big.mark=" "),
                           hjust = ifelse(df$result < ICER_baseline, 1.5, -0.4), vjust = 0, size = 7) +
                 geom_vline(xintercept = 0) +
                 scale_y_discrete(limits=levels(fct_reorder(df$var, -df$resultRange))) +
                 scale_x_continuous(
                   limits = c(lb0, ub0),
                   breaks = breaks0,
                   labels = scales::number(breakLabs, big.mark = " ")) +
                 labs(x = 'ICER vs. baseline', y = 'Variable') +
                 theme(legend.title = element_blank()) +
                 theme_economist_white() +
                 theme(axis.text.y = element_text(size=20),
                       axis.text.x = element_text(size=20)) +
                 theme(axis.title.x = element_text(face="bold", size=20, vjust=-2.5),
                       axis.title.y = element_blank()) +
                 theme(legend.position = 'top',
                       legend.title=element_blank(),
                       legend.text = element_text(size=20))
               
                   
                
               }) #RenderPlot ends
               

               
               
             }) #Observe event ends



 

  
} #Server function ends

               
### Run the application 
shinyApp(ui = ui, server = server)
