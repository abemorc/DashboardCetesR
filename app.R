
library(shiny)
library(tidyverse)
library(shinydashboard)
library(highcharter)
library(xts)
library(readxl)
library(prophet)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8080)



#layout of the dashboard

#defining character vectors for select inputs
country<-c("India","United States","Mexico","Canada","China, People's Republic of","Japan","Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

unions<-c("Major advanced economies (G7)","European Union","Emerging and Developing Europe","ASEAN-5","Commonwealth of Independent States",
          "Emerging and Developing Asia","Latin America and the Caribbean",
          "Middle East, North Africa, Afghanistan, and Pakistan")

# aqui ira el nombre de los bonos

nBonos <- c("Cetes a 28 días", "Cetes a 91 días",
            "Cetes a 182 días", "Cetes a 364 días",
            "Cetes a 728 días",
            "Bonos series M a 3 años", "Bonos series M a 5 años", 
            "Bonos series M a 7 años","Bonos series M a 10 años",
            "Bonos series M a 20 años", "Bonos series M a 30 años",
            "Udibonos a 3 años", "Udibonos a 5 años",
            "Udibonos a 10 años", "Udibonos a 20 años",
            "Udibonos a 30 años")

selectNombresBonos <- c("cetes28", "cetes91", "cetes182", "cetes364", "cetes728", "bonosM3", 
                        "bonosM5", "bonosM7", "bonosM10", "bonosM20", "bonosM30", "udibonos3", 
                        "udibonos5", "udibonos10", "udibonos20", "udibonos30")
names(selectNombresBonos) <- nBonos

# selectNombresBonos



ui <- 

dashboardPage(
  #defines header
  skin = "green",
  dashboardHeader(
    title="Dashboard" #,
    # dropdownMenu()
  ),
  
  
  #defines sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Bonos Gubernamentales", tabName = "bonos", icon = icon("dashboard")),
      menuItem("Inflación por país", tabName = "dashboard", icon = icon("signal")),
      menuItem("Inflación mundial",tabName="world",icon=icon("globe"))
      
    )
  ),
  
  
  #defines bodys
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      tabItem(tabName = "bonos",
              
              fluidRow(
                
                
                column(12,
                       
                       box(selectInput("instrumentos",label="Seleciona un instrumento",
                                       choices=selectNombresBonos),width = 12) 
                       
                ),#end column
                
                #box for plotting the time series plot
                column(12,
                       
                       box(
                         
                         highchartOutput("graficabono"),
                         collapsible = T,
                         collapsed = T,
                         width="12") #end box2
                       
                ), #end column
                hr(),
                h2("Modelo de series de tiempo",align="center"),
                br(),
                p(style="font-size:20px",strong(
                  "Puede 'estimar' cuales seran tasas de
                rendimiento que se tendrán en las siguientes subastas 
                usando un modelo de regresión")),
                p(style="font-size:20px",
                  "Primero seleccione el rango de fechas sobre el cual desea 
                entrenar el modelo, posteriormente seleccione el número 
                de subastas que pretende estimar (pocas estimaciones serán 
                más precisas)"),
                
                
                
  
                
                
                column(12,
                       
                       box(
                         dateRangeInput("fechasmodelo",
                                        "Periodo de tiempo para analizar",
                                        language = "es",
                                        start = "2008-01-01",
                                        end = Sys.Date(),
                                        min = "1900-01-01",
                                        max = Sys.Date(),
                                        startview = "decade"),
                         sliderInput("numeropredicciones",
                                     "Número de subastas a predecir",
                                     value = 50,
                                     min = 0, max = 300),
                         actionButton("ejecutarmodelo", "Ejecutar",
                                      class = "btn-block"),
                         
                         width = 12)
                       
                ),#end column
                
                column(12,
                       
                       box(
                         verbatimTextOutput("datosresumen"),
                         width=11,
                         collapsible = T,
                         collapsed = T
                         
                       )
                       
                ),
                
                column(12,
                       
                       box(
                         verbatimTextOutput("datospredicted"),
                         width=11,
                         collapsible = T,
                         collapsed = T
                         
                       )
                       
                ),
                
                
                # este tambien lo debo quitar
                column(12,
                       
                       box(
                         verbatimTextOutput("datospredicted2"),
                         width=11,
                         collapsible = T,
                         collapsed = T
                         
                       )
                       
                ),
                
                
                column(12,
                       
                       box(
                         plotOutput("graficamodelobono"),
                         width=6,
                         collapsible = T
                         
                       ),
                       box(
                         plotOutput("graficamodelocomponentes"),
                         width = 6,
                         collapsible = T
                       )
                       
                )
                
              )
              
      ),
      
      
      
      
      
      #First TAB Menu-Dashboard
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                
                
                column(12,
                       
                       box(selectInput("country",label="Selecciona un país",choices=country),width = 12) 
                       
                ),#end column
                
                column(12,
                       
                       box(
                         
                         highchartOutput("hcontainer"),
                         
                         
                         
                         width="12") #end box2
                       
                ), #end column
                hr()#,
                
                
              ),#end row
              
      ),
      
      
      
      #second tab menu- ABOUT
      tabItem(tabName="about",
              
              h2("Acerca de",style="text-align:center"),
              br(),
              br(),
              box(width=12,height="600px",
                  p(style="font-size:20px",
                    "Monitorear los CETES (Certificados de la Tesorería de
                          la Federación) es importante por diversas razones, tanto
                          para los inversores como para el gobierno y la economía 
                          en general. Los CETES son instrumentos de deuda emitidos
                          por el gobierno de México, y su monitoreo puede proporcionar
                          información clave sobre la salud económica y las 
                          expectativas del mercado."),
                  
                  
                  
                  
                  p(style="font-size:20px",strong(""), 
                    "Los CETES reflejan las decisiones del gobierno sobre la política monetaria y las tasas de interés. El Banco de México (Banxico) utiliza los CETES para regular la cantidad de dinero en circulación, afectando la inflación y el crecimiento económico. El monitoreo de estos valores permite a los inversores y analistas identificar las expectativas del mercado sobre las decisiones de tasas de interés, lo cual puede influir en las decisiones de inversión.")
                  
                  ,
                  p(style="font-size:20px",
                    "Esto es esencial para una comprensión profunda de la economía mexicana y la toma de decisiones financieras informadas, tanto por parte de los gobiernos como de los inversionistas. A través del seguimiento de los rendimientos de estos instrumentos, es posible anticipar cambios en las políticas monetarias, evaluar el riesgo soberano y ajustar las estrategias de inversión según las condiciones económicas del momento."))
              
              
              
              
      ),
      
      
      tabItem(tabName = "world",
              
              h3("Tasa de inflación mundial",align="center") ,
              
              box(
                highchartOutput("hc4"),
                width=12)
              
      )
    )#end tabitems
    
    
  )#end body
  
)#end dashboard
  
  
  















inflation <- read_excel("inflacionfmi.xls")

year<-c(1980:2029) 
year<-as.character(year)


inf<-inflation %>% 
  gather(year,key = "Year",value="InflationRate")
inf<-na.omit(inf) #omitting NA values

names(inf)<-c("region","year","inflation")

inf$year<-as.integer(inf$year)

India<-filter(inf,region=="India")
India$inflation<-as.numeric(India$inflation)
India$year<-as.numeric(India$year)

China<-filter(inf,region=="China, People's Republic of")
Ger<-filter(inf,region=="Germany")
Japan<-filter(inf,region=="Japan")
US<-filter(inf,region=="United States")
EU<-filter(inf,region=="European Union")
UK<-filter(inf,region=="United Kingdom")
Fr<-filter(inf,region=="France")
uae<-filter(inf,region=="United Arab Emirates")





# leer la informacion de los bonos
lista_bonos <- read_rds(file = "lista_bonos.Rds")


server <- function(input, output) { 
  
  dfSelectedBono <- reactive({
    # return(list(mtcars))
    return(lista_bonos[[input$instrumentos]])
  })
  
  output$graficabono <- renderHighchart ({
    
    
    tsSelectedBono <- xts(dfSelectedBono()[,2:4], 
                          order.by = dfSelectedBono()[,1])
    
    
    highchart(type = "stock") |>  
      hc_add_series(tsSelectedBono$Tasa_rendimiento, 
                    type = "line", name = input$instrumentos) |> 
      hc_title(text="Grafica de serie de tiempo",align="center") |> 
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      # hc_title(text="Time series plot of Inflation Rates",align="center") %>%
      hc_subtitle(text="Fuente: Banco de México",align="center") 
    
    
  })
  
  
  dfSelectedBono_toModel <- reactive({
    req(input$fechasmodelo[1]<input$fechasmodelo[2])
    
    dfSelectedBono_toModel <- dfSelectedBono()[c(1,4)]
    names(dfSelectedBono_toModel) <- c("ds", "y")
    dfSelectedBono_toModel <-
      dfSelectedBono_toModel |>
      filter(ds > as.Date(input$fechasmodelo[1]),
             ds < as.Date(input$fechasmodelo[2]))
    
    return(dfSelectedBono_toModel)
  })
  
  model_SelectedBono <- eventReactive(input$ejecutarmodelo, {
    req(input$fechasmodelo[1]<input$fechasmodelo[2])
    model_SelectedBono <- prophet(dfSelectedBono_toModel())
    
    return(model_SelectedBono)
  })
  
  # 
  forecast_prophet <- reactive({
    
    future <- make_future_dataframe(model_SelectedBono(),
                                    periods = input$numeropredicciones)
    predict(model_SelectedBono(), future)
    
  })
  # 
  
  
  output$datosresumen <- renderPrint({
    summary(dfSelectedBono_toModel())
  })
  
  output$datospredicted <- renderPrint({
    summary(model_SelectedBono())
  })
  
  output$datospredicted2 <- renderPrint({
    summary(forecast_prophet())
  })
  
  output$graficamodelobono <- renderPlot({
    plot(model_SelectedBono(), forecast_prophet()) +
      ggtitle(paste0("Pronóstico de serie de tiempo para ", input$instrumentos) )
  })
  
  output$graficamodelocomponentes <- renderPlot({
    prophet_plot_components(model_SelectedBono(), forecast_prophet())
  })
  
  #   # Modelo de regresion -------------------------------------------------------------------
  #   # este es el chido
  # 
  
  
  output$hcontainer <- renderHighchart ({
    
    
    df<-inf %>% filter(region==input$country)#making is the dataframe of the country
    
    df$inflation<-as.numeric(df$inflation)
    df$year<-as.numeric(df$year)
    
    #plotting the data
    hchart(df, "line",color="#DC270C",hcaes(x=year,y=inflation))  %>%
      
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Tasas de inflación por país",align="center") %>%
      hc_subtitle(text="Fuente: Fondo Monetario Internacional",align="center") %>%
      hc_add_theme(hc_theme_elementary()) 
    
    
    
    
    
  })
  
  
  output$hc2<-renderHighchart({
    
    highchart() %>% 
      hc_xAxis(categories=inf$year) %>% 
      hc_add_series(name = "India", data = India$inflation) %>% 
      hc_add_series(name = "USA", data = US$inflation) %>%
      hc_add_series(name = "UK", data = UK$inflation) %>%
      hc_add_series(name = "China", data = China$inflation) %>%
      hc_add_series(name = "Germany", data = Ger$inflation) %>%
      hc_add_series(name="Japan",data=Japan$inflation) %>%
      #to add colors
      hc_colors(c("red","blue","green","purple","darkpink","orange")) %>%
      hc_add_theme(hc_theme_elementary())
    
    
    
    
    
  })
  
  output$hc3<-renderHighchart({
    
    union<-inf %>% filter(region==input$region)
    union$year<-as.numeric(union$year)
    union$inflation<-as.numeric(union$inflation)
    
    #plotting
    hchart(union,hcaes(x=year,y=inflation),type="area",color="#2B1F97") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Time series plot of Inflation Rates for Economic Unions",align="center") %>%
      hc_subtitle(text="Data Source: IMF",align="center") %>%
      hc_add_theme(hc_theme_elementary())
    
    
    
    
  })
  
  output$hc4<-renderHighchart({
    world<-inf %>% filter(region=="World")
    world$year<-as.numeric(world$year)
    world$inflation<-as.numeric(world$inflation)
    #plotting the plot
    hchart(world,hcaes(x=year,y=inflation),type="area",color="#B915A3") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Tasa de inflación mundial",align="center") %>%
      hc_subtitle(text="Fuente: Fondo Monetario Internacional",align="center") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  
  
}







# Run 
shinyApp(ui = ui, server = server)
