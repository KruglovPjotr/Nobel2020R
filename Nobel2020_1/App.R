library(plyr)
library(shiny)
library(plotly)
library(summarytools)
library(magrittr)
library(ggiraph)
library(RColorBrewer)



# Load data
data <- read.csv("data/nobel2020.csv")
data$category <- as.factor(data$category)
data$born_country_code <- as.factor(data$born_country_code)
data$died_country_code <- as.factor(data$died_country_code)
data$gender <- as.factor(data$gender)

#UI
ui <- fluidPage(
  titlePanel("Nobeli preemia aastatel 1901–2020"),
  helpText("Õppeaine: Andmete visualiseerimine (Virumaa) - ITB8812"),
  helpText("Õppejõud: Olga Dunajeva"),
  helpText("Autor: Pjotr Kruglov"),
  
  tabsetPanel(
    tabPanel(
      "Rakenduse kirjeldus",
      sidebarPanel(img(
        src = "nobelprize.jpg",
        height = 400,
        width = 400
      )),
      mainPanel(
        h1("Tutvustus"),
        p(
          "Nobeli preemia on iga-aastaste rahvusvaheliste auhindade kogum, mida Rootsi ja Norra institutsioonid annavad mitmes kategoorias akadeemiliste, kultuuriliste või teaduslike edusammude tunnustamiseks."
        ),
        br(),
        p(
          "Projekt on tehtud Nobeli preemia aastast 1901 kuni 2020 andmestiku peal. Andmestik on allalaaditud Kaggle.com veebilelt csv formaadis."
        ),
        a(
          href = "https://www.kaggle.com/datasets/bahramjannesarr/nobel-prize-from-1901-till-2020?select=nobel_final.csv",
          "https://www.kaggle.com/datasets/bahramjannesarr/nobel-prize-from-1901-till-2020?select=nobel_final.csv"
        ),
        br(),
        h3("Projekti eesmärgid:"),
        p("1. Tutvuda andmetega."),
        p("2. Visualiseerida uurimisküsimustele vastused."),
        br(),
        h3("Uurimisküsimused:"),
        p("1. Milline on preemia saajate jagamine soo kaudu?"),
        p("2. Mis kategoorias on kõige rohkem preemiaid saanud?"),
        p("3. Millised olid top aastad Nobeli preemia saamiseks?"),
        p("4. Mis riigilt on pärit kõige rohkem Nobeli preemia saajat?")
      )
    ),
    
    tabPanel(
      "Andmed",
      sidebarPanel(
        h3("Nobeli preemia aastatel 1901–2020"),
        p("Algandmestikus on 923 vaatlus ja 14 tunnust."),
        br(),
        h3("Tunnused:"),
        p("firstname : eesnimi"),
        p("surname : perekonnanimi"),
        p("born_country_code : sündinud riigi kood"),
        p("died_country_code : suri riigikood"),
        p("gender : sugu"),
        p("year : auhinna saamise aasta"),
        p("category : kategooria"),
        p("share : mitme inimestega auhinne jagatud"),
        p("name_of_university : ülikooli nimetus"),
        p("city_of_university : ülikooli linn"),
        p("country_of_university : ülikooli riik"),
        p("born_month : sünnikuu"),
        p("age : vanus"),
        p("age_get_prize : vanus auhinna saamisel")
      ),
      mainPanel(
        br(),
        h2("Andmestiku kirjeldus"),
        htmlOutput("Summary"),
        br(),
        dataTableOutput("tabel")
      )
    ),
    
    tabPanel(
      "Soo järgi jagamine",
      sidebarPanel(
        h2("Jagamine"),
        p("Auhinna saajate soo jagamine kategooriate kaudu."),
        br(),
        selectInput(
          "select",
          label = h3("Vali kategooria"),
          choices = append("Kokku", levels(data$category)),
          selected = "Kokku"
        )
      ),
      mainPanel(br(),
                plotlyOutput("Plot"))
    ),
    
    tabPanel(
      "Kategooriad",
      sidebarPanel(
        h2("Ülevaade auhindade kategooriatest ajavahemikus"),
        br(),
        sliderInput(
          "slider",
          label = h3("Vali aastade ajavahemik"),
          min = 1901,
          max = 2020,
          value = c(1901, 2020)
        )
      ),
      mainPanel(br(),
                plotlyOutput("ggPlot"))
    ),
    
    tabPanel(
      "Top N aastat",
      sidebarPanel(
        h2("Parimad aastad Nobeli preemias"),
        p("Järjestatakse top N aastad Nobeli preeamia saadud arvu järgi"),
        radioButtons(
          "radio",
          label = h3("Vali variant:"),
          choices = list(
            "Top 5" = 5,
            "Top 10" = 10,
            "Top 50" = 50
          ),
          selected = 5
        ),
      ),
      mainPanel(br(),
                plotlyOutput("ggPlot2"))
    ),
    
    
  )
)

# Define server logic ----
server <- function(input, output) {
  #Andmed
  output$tabel <- renderDataTable(
    data,
    options =
      list(
        searching = FALSE,
        ordering = F,
        lengthMenu = c(5, 10, 20),
        pageLength = 5,
        scrollX = TRUE
      )
  )
  
  output$Summary <- renderUI({
    summarytools::view(summarytools::dfSummary(data), method = "render")
  })
  
  
  #Sugu
  output$Plot <- renderPlotly({
    if (input$select == "Kokku") {
      m1 <- count(data$year[data$gender == "male"])
      n1 <- count(data$year[data$gender == "female"])
    }
    else
    {
      m1 <-
        count(data$year[data$category == input$select &
                          data$gender == "male"])
      n1 <-
        count(data$year[data$category == input$select &
                          data$gender == "female"])
    }
    fig <-
      plot_ly(
        data,
        x = m1$x,
        y = m1$freq,
        name = "Mehed",
        type = 'bar'
      )
    fig <- fig %>% add_trace(x = n1$x,
                             y = n1$freq,
                             name = "Naised")
    fig <-
      fig %>% layout(title = paste(input$select, "jagamine soo järgi"))
    fig <-
      fig %>% layout(
        autosize = F,
        xaxis = list(title = 'Aasta'),
        yaxis = list(title = 'Arv'),
        barmode = 'group',
        bargap = 0.15
      )
    
    fig
  })
  
  # Kategooriad
  output$ggPlot <- renderPlotly({
    x1 <-
      count(data$category[data$year >= input$slider[1] &
                            data$year <= input$slider[2]])
    
    fig <-
      plot_ly(x1,
              labels = x1$x,
              values = x1$freq,
              type = 'pie')
    fig <-
      fig %>% layout(
        title = paste(
          "Preemiate jagamine aastates ",
          input$slider[1],
          "-",
          input$slider[2]
        ),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    
    fig
  })
  
  # Aastad
  output$ggPlot <- renderPlotly({
    x1 <-
      count(data$category[data$year >= input$slider[1] &
                            data$year <= input$slider[2]])
    
    fig <-
      plot_ly(x1,
              labels = x1$x,
              values = x1$freq,
              type = 'pie')
    fig <-
      fig %>% layout(
        title = paste(
          "Preemiate jagamine aastates ",
          input$slider[1],
          "-",
          input$slider[2]
        ),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    
    fig
  })
  
  # Top
  output$ggPlot2 <- renderPlotly({
    x2 <- count(data$year)
    x2 <- x2[with(x2, order(-freq)), ]
    
    x2 <- x2[1:input$radio, ]
    
    fig <- plot_ly(
      x = as.factor(x2$x),
      y = x2$freq,
      name = "Top N",
      type = "bar"
    )
    fig <-
      fig %>% layout(
        title = paste("Top ", input$radio, " aastat Nobeli preemia arvu järgi"),
        xaxis = list(title = "aasta", categoryorder = "total descending"),
        yaxis = list(title = "arv")
      )
    
    fig
  })
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)