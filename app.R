library(shiny)
library(ggplot2)
library(dplyr)

# Création du data frame avec les données
data <- data.frame(
  Banque = c("BNC", "BUH", "CAPITAL BANK", "CITIBANK", "SOGEBANK", "SOGEBEL", "UNIBANK"),
  Achat = c(131.2500, 132.0000, 131.5000, 131.0000, 131.2500, 131.2500, 131.7500),
  Vente = c(133.7500, 133.7500, 133.7500, 134.0000, 133.7500, 133.7500, 133.7500),
  BRH = c(132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145)
)

# Ajout de la date du jour
data$date <- Sys.Date()

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Tableau de bord des taux de change HTG/USD"),
  sidebarLayout(
    sidebarPanel(
      selectInput("banque", "Sélectionner une banque:",
                  choices = unique(data$Banque))
    ),
    mainPanel(
      plotOutput("graphique_achat"),
      plotOutput("graphique_vente"),
      plotOutput("graphique_ecart")

    )
  )
)

# Serveur
server <- function(input, output, session) {
  output$graphique_achat <- renderPlot({
    banqueSelectionnee <- input$banque
    dataBanque <- filter(data, Banque == banqueSelectionnee)

    ggplot(dataBanque, aes(x = date, y = Achat, color = Banque, label = Achat)) +
      geom_line() +
      geom_text(aes(y = Achat + 0.5), color = "blue", size = 3, vjust = 0) +
      labs(title = paste("Taux d'achat de", banqueSelectionnee),
           x = "Date", y = "Taux d'achat") +
      theme_minimal()
  })

  output$graphique_vente <- renderPlot({
    banqueSelectionnee <- input$banque
    dataBanque <- filter(data, Banque == banqueSelectionnee)

    ggplot(dataBanque, aes(x = date, y = Vente, color = Banque, label = Vente)) +
      geom_line() +
      geom_text(aes(y = Vente + 0.5), color = "Blue", size = 3, vjust = 0) +
      labs(title = paste("Taux de vente de", banqueSelectionnee),
           x = "Date", y = "Taux de vente") +
      theme_minimal()
  })

  output$graphique_ecart <- renderPlot({
    banqueSelectionnee <- input$banque
    dataBanque <- filter(data, Banque == banqueSelectionnee)

    ggplot(dataBanque, aes(x = date, y = Achat - BRH, color = Banque, label = Achat - BRH)) +
      geom_line() +
      geom_text(aes(y = Achat - BRH + 0.5), color = "Blue", size = 3, vjust = 0) +
      labs(title = paste("Écart par rapport au taux de référence BRH de", banqueSelectionnee),
           x = "Date", y = "Écart par rapport au taux de référence BRH") +
      theme_minimal()
  })

}

# Lancez l'application Shiny
shinyApp(ui = ui, server = server)









