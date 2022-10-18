# INSTALLATION PACKAGES
# install.packages("esquisse")
library(esquisse)
# install.packages("shinydashboard")
library(shinydashboard)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr)
library(DT)

#IMPORTATION DES DONNEES
setwd("C:/Users/Ayat/Downloads")
anime = read.csv2("anime.csv", h=T, sep=";")
type = read.csv2("type.csv", h=T, sep=";")
genre = read.csv2("genre.csv", h=T, sep=";")

#CHANGER LES TYPES
str(anime)
anime$note_anime <- as.numeric(anime$note_anime)
anime$id_genre <- as.integer(anime$id_genre , na.rm = F)
anime$id_type <- as.integer(anime$id_type , na.rm = F)
str(genre)

# CREATION DES LIAISONS 
anime <- merge(x = anime, y = genre, by = "id_genre", all.x = TRUE)
anime <- merge(x = anime, y = type, by = "id_type", all.y = TRUE)

# test pour tableau
delete_col <- c(-1,-2,-3,-6,-7,-8)
anime_bis<- anime[,delete_col]

# Palette de couleur (theme :one piece)
couleur <- c( noir = "#412a1e", 
              jaune = "#f8de3c",
              rouge = "#c8472c",
              blanc = "#fefefe",
              bleu_c = "#58acf4",
              bleu_d = "#105edd",
              bleu_f = "#0b3075")

#CREATION DES GRAPHIQUES POUR LE TDB FAIT AVEC ESQUISSE OU PAS

top_3_graph <-  anime %>%  #TOP 3 DES ANIMES LES MIEUX NOTES
  filter(note_anime >= 9.2 & note_anime <= 9.4) %>%
  ggplot() +
  aes(x = nom_anime, y = note_anime) +
  geom_col(fill = couleur["bleu_f"]) +
  labs(
    x = "Titre animé",
    y = "Note animé",
    title = "Top 3 animé"
  ) +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

top_4_graph <- anime %>%  #TOP DES GENRES D ANIMES LES PLUS RECURRENTS
  filter(nom_genre %in% c("Action", "Comedy", "Adventure", "Drama")) %>%
  ggplot() +
  aes(x = nom_genre) +
  geom_bar(fill = couleur["noir"]) +
  labs(
    x = "Genre ",
    y = "Nombre d'apparition du genre",
    title = "Les genres les plus prÃ©sents "
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

graph_genre <- anime %>% #TOP DES GENRES LES MIEUX NOTES SIMILAIRE AU PRECEDENT (PERTINENT ?)
  filter(note_anime >= 7.45 & note_anime <= 9.4) %>%
  filter(nom_genre %in% c("Action", "Comedy",
                          "Adventure", "Drama")) %>%
  ggplot() +
  aes(x = nom_genre, y = note_anime) +
  geom_col(fill = couleur["bleu_f"]) +
  labs(
    x = "Genre",
    y = "Cumule des notes",
    title = "Genre les mieux notÃ©s"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

#GRAPHIQUE PIE CHART PLOT 3 répartition des genres
x <- prop.table(table(anime$nom_type))
lb <- round(100*x/sum(x), 1)
lb<-paste(lb,"%",sep="")

pie(x, labels = lb , main = "Répartition des genres sur le nombre total d'animés",col = couleur)
legend("topright", c("Movie", "Music", "ONA", "OVA", "Special", "TV"), cex = 0.8,
       fill = couleur)

#CODE POUR LE SHINY 
server <- function(input, output) { 
  output$import_data<-DT::renderDataTable({})
  
  # Calculs permettant de construire les KPI
  total.anime= nrow(x = anime) 
  moyenne.note = round(mean(anime$note_anime, na.rm = T),2)
  nb.moyen.votant = round(mean(anime$membres_anime))

  #KPI nb total d'anime
  output$KPI1 <- renderValueBox({
    valueBox(
      total.anime, "Nombre total d'animés", icon = icon("folder-open"),
      color = "red")
  })
  
  #KPI moyenne des notes
    output$KPI2 <- renderValueBox({
      valueBox(
        moyenne.note, "Moyenne des notes", icon = icon("signal"),
        color = "yellow")
    })
  
  #KPI nb de votant
    output$KPI3 <- renderValueBox({
      valueBox(
        nb.moyen.votant, "Nombre moyen de votant par animés", icon = icon("user"),
        color = "blue")
    })
  
    
  #GRAPHIQUE TOP 3  
    output$plot1 <- renderPlot({
      top_3_graph
    })
  #GRAPHIQUE TOP 4 GENRE
    output$plot2 <- renderPlot({
      d1 <- tail(table(anime$nom_genre),4)
      barplot(height = d1,
              xlab = "Genres",
              ylab = "Nombre d'occurence",
              main = "Top 4 des genres les plus présents",
              col = couleur["rouge"]
              )
    })
  #GRAPHIQUE TOP TYPE
    output$plot3 <- renderPlot({
      pie(x, labels = lb , main = "Répartition des types sur le nombre total d'animés",col = couleur)
      legend("bottomright", c("Movie", "Music", "ONA", "OVA", "Special", "TV"), cex = 0.8,
             fill = couleur)
    })
    
    #Tableaux top 3 animés
    output$tab1 <- renderTable(
      anime_bis %>%
        arrange(desc(anime_bis$note_anime)) %>%
        slice(1:3)
    )
    #Tableaux top 4 genre
    output$tab2 <- renderTable(
      table(anime$nom_genre)
    )

}


