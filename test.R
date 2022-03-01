# https://appsilon.com/user-authentication-in-r-shiny-sneak-peek-of-shiny-users-and-shiny-admin-packages/ ->ekran logowania
# https://www.showmeshiny.com/shinyhome/
# https://plot.ly/r/#controls

# interkatywne wykresy z plotly
# https://medium.com/@abchen/increasing-interaction-through-coupled-events-with-shiny-and-plotly-4a253dd3be12
# https://bioinformatics-core-shared-training.github.io/shiny-bioinformatics/interactive-plots
# https://bioinformatics-core-shared-training.github.io/shiny-bioinformatics/interactive-plots
# https://plot.ly/r/shinyapp-plotly-events/   !!!!

options(shiny.port = 138) #500, 1900
options(shiny.host = "192.168.56.1")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)  
library(cluster)    
library(factoextra)
library(plotly)

ui <- dashboardPage(title = "CARS CLUSTER ANALYSIS",
  dashboardHeader(title = "CARS CLUSTER"),
  dashboardSidebar(sidebarMenu(id = "sbm",
                               menuItem("Promo Explorer", tabName = "PE", icon = icon("bar-chart"),
                                        menuSubItem('KPI / SLA',tabName = "KPI",icon = icon("line-chart")),
                                        menuSubItem('Data',tabName =  "r_data",icon = icon("bell-slash =")))
                              )
                  ),
  dashboardBody(
   tabItems(
     tabItem(tabName = 'r_data',
        fluidRow(
          column(width = 3, 
                 pickerInput(
                   inputId = "f_car",
                   label = "Car Name:", 
                   choices = unique(rownames(mtcars)),
                   multiple = TRUE
                 ),
                 sliderInput("f_mpg", label = "MPG Range:", min = min(mtcars$disp), 
                             max = max(mtcars$disp), value = c(50,200))
          ),
          column(width = 3, 
                 pickerInput(
                   inputId = "f_cyl",
                   label = "Liczba Cylindrów:", 
                   choices = unique(mtcars$cyl),
                   multiple = TRUE
                 ),
                 
                 sliderInput("f_disp", label = "Disp Range:", min = min(mtcars$disp), 
                             max = max(mtcars$disp), value = c(200,300))
             
                 ),
        
        column(width = 3, 
               pickerInput(
                 inputId = "f_carb",
                 label = "Liczba Carb:", 
                 choices = unique(mtcars$gear),
                 multiple = TRUE
               ),
               
               sliderInput("f_hp", label = "HP Range:", min = min(mtcars$disp), 
                           max = max(mtcars$disp), value = c(60,100))
        ),
      
      column(width = 3, 
             pickerInput(
               inputId = "f_gear",
               label = "Liczba Biegów:", 
               choices = unique(mtcars$gear),
               multiple = TRUE
             ),
             
             sliderInput("f_drat", label = "Drat Range:", min = min(mtcars$disp), 
                         max = max(mtcars$disp), value = c(1,3))
    
            )
      ),
     
      DT::dataTableOutput("Table1")
  
     ),
     tabItem(tabName ="KPI",
          fluidPage(
            column(width = 2, wellPanel(
                   awesomeRadio(
                     inputId = "l_aut",
                     label = "Lista Aut:", 
                     choices = unique(rownames(mtcars),
                     selected = "A",
                     inline = FALSE, 
                     checkbox = TRUE
                   ))
                   
            )),
            
            column(width = 10,
            box(title = "Cluster for mtcars dataset",status = "primary", solidHeader = TRUE,
            
                   pickerInput(
                     inputId = "c_number",
                     label = "Liczba Clustrów:", 
                     choices = c(1:7),
                     multiple = FALSE
                   ),
                   # plotOutput("plot1")
                   plotlyOutput("plot1")
                   ))
          )   
        )
     
   )
  )
)


server <- function(input,output,session){
  output$Table1 <- DT::renderDataTable({
    datatable(mtcars[mtcars$hp >= input$f_hp[1] & mtcars$hp <= input$f_hp[2],],filter = 'top')
  })
  
  output$plot1 <- renderPlotly({
    
    set.seed(12)
    df <- na.omit(mtcars)
    distance <- get_dist(df)
    k2 <- kmeans(df, centers = input$c_number)
    # fviz_cluster(k2, data = df,show.clust.cent = TRUE)
    ggplotly(fviz_cluster(k2, data = df,show.clust.cent = TRUE,labelsize = 8,ggtheme = theme_void())) %>% 
      layout(showlegend = FALSE, title = '', xaxis = list(title = '', tickfont = list(size = 8)), 
             yaxis = list(title='', tickfont = list(size = 8)))
    
  })
  
  

}

shinyApp(ui,server)


k2$cluster[k2$cluster==1]
mtcars[rownames(mtcars)%in%names(l),]





