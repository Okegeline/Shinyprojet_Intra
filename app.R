library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(rvest)
library(httr)

# Fonction pour lire les données à partir du lien CSV de la BRH
read_BRH_data <- function() {
  data_url <- "https://www.brh.ht/taux-affiches-par-les-banques-et-les-agents-de-change-2/"
  page <- read_html(data_url)
  table <- html_table(html_nodes(page, "table")[[1]])
  write.csv(table, "donnees_taux_change.csv", row.names = FALSE)
  donnees <- read.csv("donnees_taux_change.csv", header = TRUE)
  donnees <- data.frame(
    Banque = c("BNC", "BUH", "CAPITAL BANK", "CITIBANK", "SOGEBANK", "SOGEBEL", "UNIBANK"),
    Achat = c(131.2500, 132.0000, 131.5000, 131.0000, 131.2500, 131.2500, 131.7500),
    Vente = c(133.7500, 133.7500, 133.7500, 134.0000, 133.7500, 133.7500, 133.7500),
    Taux_Reference_BRH = c(132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145)
  )

  # Ajouter une colonne 'Date' hypothétique (à remplacer par vos propres données)
  donnees$Date <- as.Date(Sys.Date())

  return(donnees)
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Tableau de bord des taux de change HTG/USD"),
  sidebarLayout(
    sidebarPanel(
      selectInput("banque", "Sélectionner une banque:",
                  choices = c("Toutes", unique(read_BRH_data()$Banque)))
    ),

    mainPanel(
      plotOutput("graphique_achat"),
      plotOutput("graphique_vente"),
      plotOutput("graphique_ecart"),
      plotOutput("graphique_Taux_reference")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Fonction réactive pour mettre à jour les données de la BRH périodiquement
  data <- reactivePoll(intervalMillis = 60000, # Mettre à jour toutes les 60 secondes (60000 millisecondes)
                       session,
                       checkFunc = function() {
                         # Renvoie l'heure actuelle pour forcer la mise à jour des données
                         as.numeric(Sys.time())
                       },
                       valueFunc = function() {
                         # Lit les données à partir du lien CSV de la BRH
                         read_BRH_data()
                       })

  # Fonction réactive pour filtrer les données en fonction de la banque sélectionnée
  filtered_data <- reactive({
    if (input$banque == "Toutes") {
      return(data())
    } else {
      return(filter(data(), Banque == input$banque))
    }
  })

  output$graphique_achat <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = Achat, fill = Banque)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Achat), vjust = -0.5, size = 3, color = "black", position = position_dodge(width = 3)) +
      labs(title = "Taux d'achat", x = "Date", y = "Taux d'achat")
  })

  output$graphique_vente <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = Vente, fill = Banque)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Vente), vjust = -0.5, size = 3, color = "Black", position = position_dodge(width = 3)) +
      labs(title = "Taux de vente",  x = "Date",  y = "Taux de vente")

  })

  output$graphique_ecart <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = Achat - Taux_Reference_BRH, fill = Banque)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Achat - Taux_Reference_BRH), vjust = -0.5, size = 3, color = "black", position = position_dodge(width = 3)) +
      labs(title = "Écart par rapport au taux de référence BRH", x = "Date", y = "Écart")
  })

  output$graphique_Taux_reference <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = Taux_Reference_BRH, fill = Banque)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Taux_Reference_BRH), vjust = -0.5, size = 3, color = "black", position = position_dodge(width = 3)) +
      labs(title = "Taux de référence BRH", x = "Date", y = "Taux de référence")
  })
}

# Lancez l'application Shiny
shinyApp(ui = ui, server = server)
