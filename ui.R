# LIBRAIRIES
library(shinydashboard)
library(esquisse)
library(ggplot2)
library(dplyr)



# CODE INTERFACE UTILISATEUR
ui <- dashboardPage(
  dashboardHeader(title = "Recommandation d'animés",
                  titleWidth = 300),
  skin ="blue",
  
  dashboardSidebar(collapsed = T ,
                   menuItem("Dashboard", tabName = "data", icon = icon("poll")),
                   menuItem("Tableaux", tabName = "Graphiques", icon = icon("graph"))),
  
    dashboardBody(
      tabItem(
          tabName = "data",
          h1("Dashboard"),
    
          fluidRow(
      
            # Affichage des KPI
            valueBoxOutput("KPI1"),
            valueBoxOutput("KPI2"),
            valueBoxOutput("KPI3")),
          DT::dataTableOutput("import_data"),
          div(style = "display:inline-block; float:right"),
          
          # affichage du graphique top 3
          fluidRow(
            box(downloadButton("export"),plotOutput("plot1", height = 270),background = "blue"),
            box(
              title = "Données complémentaires",
              "Sélectionner un type et/ou un genre pour connaitre l'animé qui a le plus de succès !",br(),
              width = 6,
              selectInput("vv",label = "Choisir un type", choices =  c("Tout", "Movie", "Music", "ONA", "OVA", "Special", "TV")),
              selectInput("v",label = "Choisir un genre", choices =  c("Tout", "Action", "Drama", "Adventure","TV" )),
              height = 325,
              background = "yellow"
            ),
      
              box(downloadButton("export3"), plotOutput("plot3", height = 300,),background = "blue"),
              box(downloadButton("export2"),plotOutput("plot2", height = 300),background = "blue")),
        ),

    tabItem(
      tabName = "graphiques",
      h1("Tableaux"),
      fluidRow(
        box(tableOutput("tab1"),background = "blue"),
        box(tableOutput("tab2"),background = "blue"))
      )
)

)
