
# 1.0 LOAD PACKAGES & LIBRARIES ----
# install.packages("lpsolve")
library(lpSolve)
library(bslib) 
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(tibble)
library(dplyr)
library(billboarder)
library(tidyverse)
library(tidyquant)
library(kableExtra)
library(scales)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggdensity)
library(shinythemes)
library(shinydashboard)
# install.packages("shinycssloaders")
library(shinycssloaders)
# install.packages("shinydashboardPlus")
# library(shinydashboardPlus)
#install.packages("shinyBS")
library(shinyBS)
library(reactable)
library(formattable)
library(shinyalert)
library(collapsibleTree)

library(tidyverse)
library(tidyquant)
library(plotly)
library(ggplot2)
#install.packages("ggExtra")
library(ggExtra)
library(hrbrthemes)
library(viridis)

library(highcharter) #interactive visualization

library(readxl)
library(scales)

library(imputeTS) # replace NA's with a value in df
library(fresh)
# install.packages("colorDF")
library(colorDF)
# install.packages("DT")

# install.packages("kableExtra")
# install.packages("devtools")
# devtools::install_github("haozhu233/kableExtra")
library(kableExtra)
# install.packages("flextable")
library(flextable)
# install.packages("pacman")
library(pacman)

# install.packages("formattable")                         
library("formattable")

library(billboarder) # https://cran.r-project.org/web/packages/billboarder/vignettes/billboarder.html

# Map

library(osrm)
library(sf)
#library(mapview)
library(leaflet)
library(billboarder)

library(markdown)

# Modelling
# install.packages("Amelia")
library(Amelia)
# install.packages("caret")
library(caret)
library(broom)
library(modelr)
library(DT)

#

Sys.setlocale(locale = "Turkish")

# Data ----

data_tablo <- read_excel("../Erdi_App/All Tables.xlsx",sheet = "tablo")


# Function ----


tazminat_hesaplama <- function(Data, Gelir, Maluliyet_oran, Kusur_oran){
  
  data = Data
  
  # Sonuçları depolamak için boş vektörler oluşturun
  Yas_values <- numeric()
  Tablo_values <- character()
  Cinsiyet_values <- character()
  Tazminat_values <- numeric()
  ExpLife_values <- numeric()
  
  # Benzersiz tablo ve cinsiyet kombinasyonlarını bulun
  tablo_cinsiyet_combinations <- unique(data[, c("Tablo", "Cinsiyet")])
  
  # Dış döngü benzersiz tablo ve cinsiyet kombinasyonları üzerinde dönecek
  for (i in 1:nrow(tablo_cinsiyet_combinations)) {
    tablo <- as.character(tablo_cinsiyet_combinations$Tablo[i])
    cinsiyet <- as.character(tablo_cinsiyet_combinations$Cinsiyet[i])
    
    # İlgili tablo ve cinsiyet için veri alt kümesini alın
    subset_data <- subset(data, Tablo == tablo & Cinsiyet == cinsiyet)
    
    # İç döngü 0'dan ilgili alt kümedeki en büyük Yas değerine kadar dönecek
    for (Yas in 0:max(subset_data$Yas)) {
      # ExpLife değerini bulun
      ExpLife <- subset_data$ExpLife[subset_data$Yas == Yas]
      if (length(ExpLife) == 0) {
        next
      }
      # Kazanilan_Ay hesaplaması
      Kazanilan_Ay <- ExpLife * 12
      
      # Tazminat hesaplaması
      Tazminat <- round(Kazanilan_Ay * Gelir * Maluliyet_oran * Kusur_oran, digits = 2)
      
      Yas_values <- c(Yas_values, Yas)
      Tablo_values <- c(Tablo_values, tablo)
      Cinsiyet_values <- c(Cinsiyet_values, cinsiyet)
      Tazminat_values <- c(Tazminat_values, Tazminat)
      ExpLife_values <- c(ExpLife_values, ExpLife) # ExpLife değerlerini vektöre ekleyin
    }
  }
  
  # Sonuçları Yas, Tablo, Cinsiyet ve Tazminat adında dört kolondan oluşan bir dataframe yapın
  result_df <- data.frame(Yas = Yas_values, Tablo = Tablo_values, Cinsiyet = Cinsiyet_values, ExpLife = ExpLife_values, Tazminat = Tazminat_values, stringsAsFactors = FALSE)
  
  # Dataframe'i yazdırın
  # print(result_df)
}


# Define the UI ----

ui <- dashboardPage(
  
  
  dashboardHeader(title = "OPTIMIZATION APP"),
  dashboardSidebar(
    
    
    sidebarMenu(
      menuItem("Yaşam Tabloları Etki Analizi", tabName = "slider_app"),
      menuItem("Tazminat Etki Analizi", tabName = "slider_app2"),
      menuItem("Test Analizi", tabName = "slider_app3")
      
    )
  ),
  dashboardBody(
    
    
    tags$head(
      tags$style(HTML("
        /* Custom CSS for changing the sidebar color */
        .skin-blue .main-sidebar {
          background-color: #d7d8d6;
        }
        
        /* Custom CSS for changing the background color of the main dashboard */
        .content-wrapper {
          background-color: #26224c;
          color: #fff;
        }
        
        .div.content-wrapper {
          background-color: #031f30;
          color: #acc2dd;
        }
        
        /* Custom CSS for changing the text color of solution outputs */
        .solution-text {
          color: white !important;
        }
        
        /* Custom CSS for changing the text color of datatable */
        div.datatables {
          color: black !important;
          background-color: #D7D8D6;
        }
        }
        
        
        /* Make valuebox background transparent */
        /*.small-box { background-color: transparent!important; }*/
        

      "))
    ),
    
    
    tabItems(
      tabItem(
        tabName = "slider_app",
        
        
        wellPanel(
          
          style = "background: #26224c",
          
          fluidRow(
            
            column(
              width = 2,
              
              wellPanel(
                style = "background-color: #26224c ;",  # Change the background color here
                
                prettyCheckboxGroup(
                  inputId = "tablo",
                  label = "Yaşam Tablosu Seç:",
                  choices = unique(data_tablo$Tablo),
                  selected = "TRH2010",
                  icon = icon("user"),
                  animation = "tada"
                ),
                
                awesomeRadio(
                  inputId = "cinsiyet",
                  label = "Cinsiyet", 
                  choices = unique(data_tablo$Cinsiyet),
                  selected = "Erkek",
                  inline = TRUE, 
                  checkbox = TRUE
                ),
                
                br(),
                hr(),
                
                # p(tags$b("Location Diameter for Mapping"), style="color:#acc2dd"),
                # 
                # span(tags$i(h6("Haritada belirlemek istediğiniz çapı seçiniz.")), style="color:#acc2dd"),
                chooseSliderSkin("Flat", color = "#ff8da3"),
                sliderInput("kusur", label = "Kusur Oranı (%)", min = 0, 
                            max = 100, value = 50
                            # animate=animationOptions(interval = 300, loop = FALSE)
                ),
                sliderInput("maluliyet", label = "Maluliyet Oranı (%)", min = 0, 
                            max = 100, value = 50
                            # animate=animationOptions(interval = 300, loop = FALSE)
                )
              )

            ),
            
            column(
              width = 5,
              plotOutput("beklenen_omur_grafik",height = 400),
            ),
            column(
              width = 5,
              plotlyOutput("beklenen_omur_farki_grafik",height = 400) %>% withSpinner(color = "#0dc5c1")
            ),
            
            column(
              width = 5,
              plotOutput("tazminat_grafik",height = 400)
            ),
            
            column(
              width = 5,
              plotlyOutput("tazminat_farki_grafik", height = 400) %>% withSpinner(color = "#0dc5c1")
            )

            # column(
            #   width = 6,
            #   plotOutput("grafik2")
            # ),
            # column(
            #   width = 6,
            #   plotOutput("grafik3")
            # )
            
          ) # wellpanel
        ), # row
        
        
        fluidRow(
          
        ), # row

        # wellPanel(
        
        fluidRow(
          
        )
        
      ),
      
      tabItem(
        tabName = "slider_app2",
        fluidRow(
          box(
            title = "Slider Inputs2",
            width = 3,
            sliderInput("slider11", "ss", min = 0, max = 3000, step = 200, value = 500),
            sliderInput("slider22", "cc", min = 0, max = 5000, step = 200, value = 500),
            sliderInput("slider33", "bb", min = 0, max = 4000, step = 200, value = 500),
            sliderInput("slider44", "nn", min = 50000, max = 500000, step = 25000, value = 100000)
            
          ),
        )
      ),
      
      tabItem(
        tabName = "slider_app3",
        div(class = "container",style = "padding:30px;",
            h3("Indicator definitions and schedule", style = "font-weight:600;"),
            hr(),
            p("Use the filters below to search for indicators by profile and/or geography level. Alternatively you can search using key words (e.g. 'cancer'). You can then click on an indicator in the search results table to view metadata and links to quickly navigate to analysis in this tool for that particular indicator.",
              style = "font-size:16px; padding:5px"),
            div(class = "tech-doc-download",style = "display:flex; margin-bottom: 10px; justify-content:space-between;",
                p("To view technical information and updates schedule for all indicators at once, use the download button.",
                  style = "font-size:16px;"),
                downloadButton('btn_techdoc_download', "Download as CSV", class = "button")),
            fluidRow(style = "border-top: 1px solid #eee;",
                     column(3,
                            selectInput(
                              "profile_search", 
                              label = "Filter by profile",
                              choices = unique(data_tablo$Cinsiyet)
                              )),
                     column(3,
                            selectizeInput(
                              "geo_search",
                              label = "Filter by geography level",
                              choices = unique(data_tablo$Cinsiyet),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(placeholder = 'Select geography level(s)'))
                     )),
            
            fluidRow(reactableOutput("ind_search_results") %>% withSpinner(color = "#0dc5c1"))
        ) # close container 
      )

    )
  )
)


# Define the server logic
server <- function(input, output) {
  
  data_tablo2 <- data_tablo %>%
    filter(Cinsiyet == "Kadin")
  
  data_tablo3 <- data_tablo %>%
    filter(Cinsiyet == "Erkek")
  
  
  filtreli_veri <- reactive({
    data_tablo %>%
      filter(Tablo %in% c(input$tablo)) |>
      filter(Cinsiyet == input$cinsiyet)
  })
  
  output$beklenen_omur_grafik <- renderPlot({
    ggplot(filtreli_veri(), aes(x = Yas, y = ExpLife, color = Tablo, group = Tablo)) +
      geom_line(linewidth = 1.5) +
      labs(title = "Beklenen Ömür", x = "Yaş", y = "ExpLife") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA), # Change plot panel background
        plot.background = element_rect(fill = "lightgrey", color = NA), # Change entire plot background
        axis.title = element_text(size = 18), # Increase axis titles font size
        axis.text = element_text(size = 18)   # Increase axis text font size
      )
  })
  
  output$tazminat_grafik <- renderPlot({
    
    filtered_calculated_table <- tazminat_hesaplama(filtreli_veri(), 20002, input$maluliyet/100, input$kusur/100) 
    
    ggplot(filtered_calculated_table, aes(x = Yas, y = Tazminat, color = Tablo, group = Tablo)) +
      geom_point(size = 1) +
      geom_hline(yintercept = 1800000, linetype = "dashed", color = "black") +
      labs(title = "Hesaplanan Tazminat", x = "Yas", y = "Tazminat") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA), # Change plot panel background
        plot.background = element_rect(fill = "lightgrey", color = NA), # Change entire plot background
        axis.title = element_text(size = 18), # Increase axis titles font size
        axis.text = element_text(size = 18)   # Increase axis text font size
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous()
  })
  
  # output$grafik2 <- renderPlot({
  #   ggplot(data_tablo2, aes(x = Yas, y = ExpLife, color = Tablo, group = Tablo)) +
  #     geom_line() +
  #     labs(title = "Beklenen Ömür-Kadın", x = "Yaş", y = "ExpLife") +
  #     theme_minimal()
  # })
  # 
  # output$grafik3 <- renderPlot({
  #   ggplot(data_tablo3, aes(x = Yas, y = ExpLife, color = Tablo, group = Tablo)) +
  #     geom_line() +
  #     labs(title = "Beklenen Ömür-Erkek", x = "Yaş", y = "ExpLife") +
  #     theme_minimal()
  # })

  
  
  output$beklenen_omur_farki_grafik <- renderPlotly({
    
    selected_tablo <- input$tablo
    
    if (length(selected_tablo) >= 2) {
      tablo1 <- selected_tablo[length(selected_tablo) - 1]
      tablo2 <- selected_tablo[length(selected_tablo)]
      
      data1 <- data_tablo %>% filter(Tablo == tablo1, Cinsiyet == input$cinsiyet)
      data2 <- data_tablo %>% filter(Tablo == tablo2, Cinsiyet == input$cinsiyet)
      
      data_diff <- merge(data1, data2, by = "Yas")
      data_diff$ExpLife_Farki <- data_diff$ExpLife.y - data_diff$ExpLife.x
      
      e <- ggplot(data_diff, aes(x = Yas, y = ExpLife_Farki)) +
        geom_line(linewidth = 1, color = "red") +
        labs(title = paste("Beklenen Ömür Farkı (", tablo1, " ve ", tablo2, ")", sep = ""), x = "Yaş", y = "Beklenen Ömür Farkı") +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "lightgrey", color = NA),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
        ) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous()
      
      r <- ggplotly(e)
      r
 
    }
  })
  
  
  
  output$tazminat_farki_grafik <- renderPlotly({
    
    selected_tablo <- input$tablo
    
    if (length(selected_tablo) >= 2) {
      tablo1 <- selected_tablo[length(selected_tablo) - 1]
      tablo2 <- selected_tablo[length(selected_tablo)]
      
      data1 <- tazminat_hesaplama(data_tablo %>% filter(Tablo == tablo1, Cinsiyet == input$cinsiyet), 20002, input$maluliyet / 100, input$kusur / 100)
      data2 <- tazminat_hesaplama(data_tablo %>% filter(Tablo == tablo2, Cinsiyet == input$cinsiyet), 20002, input$maluliyet / 100, input$kusur / 100)
      
      data_diff <- merge(data1, data2, by = "Yas")
      data_diff$Tazminat_Farki <- data_diff$Tazminat.y - data_diff$Tazminat.x
      
      g <- ggplot(data_diff, aes(x = Yas, y = Tazminat_Farki)) +
        geom_line(linewidth = 1, color = "blue") +
        labs(title = paste("Tazminat Farkı (", tablo1, " ve ", tablo2, ")", sep = ""), x = "Yaş", y = "Tazminat Farkı") +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "lightgrey", color = NA),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
        ) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous()
      
      
      p <- ggplotly(g)
      
      # Modify layout to add a black framework line
      # p <- layout(p, plot_bgcolor = "#031f30", paper_bgcolor = "#031f30", margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
      #             xaxis = list(color = "white", showgrid = FALSE, showline = TRUE, linewidth = 1, linecolor = "black"),
      #             yaxis = list(color = "white", showgrid = FALSE, showline = TRUE, linewidth = 1, linecolor = "black"))
      
      p
      
      
    }
  })
  
  
}



# Create the Shiny app
shinyApp(ui, server)