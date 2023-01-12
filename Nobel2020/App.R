library(plyr)
library(shiny)
library(plotly)
library(summarytools)
library(magrittr)
library(rvest)
library(RColorBrewer)
library(ggiraph)

# Load data
data <- read.csv("data/nobel2020.csv")
data$category <- as.factor(data$category)
data$born_country_code <- as.factor(data$born_country_code)
data$died_country_code <- as.factor(data$died_country_code)
data$gender <- as.factor(data$gender)

# Map load
url <-
  "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Country-Codes-A-C"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][,-1]
iso_codes <-
  iso_codes[!apply(iso_codes, 1, function(x) {
    all(x == x[1])
  }),]

iso_codes2 <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Country-Codes-D-H"]') %>%
  html_table()
iso_codes2 <- iso_codes2[[1]][,-1]
iso_codes2 <-
  iso_codes2[!apply(iso_codes2, 1, function(x) {
    all(x == x[1])
  }),]

iso_codes3 <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCodes-I-L"]') %>%
  html_table()
iso_codes3 <- iso_codes3[[1]][,-1]
iso_codes3 <-
  iso_codes3[!apply(iso_codes3, 1, function(x) {
    all(x == x[1])
  }),]

iso_codes4 <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Country-Codes-M-P"]') %>%
  html_table()
iso_codes4 <- iso_codes4[[1]][,-1]
iso_codes4 <-
  iso_codes4[!apply(iso_codes4, 1, function(x) {
    all(x == x[1])
  }),]

iso_codes5 <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Country-Codes-Q-T"]') %>%
  html_table()
iso_codes5 <- iso_codes5[[1]][,-1]
iso_codes5 <-
  iso_codes5[!apply(iso_codes5, 1, function(x) {
    all(x == x[1])
  }),]

iso_codes6 <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Country-Codes-U-Z"]') %>%
  html_table()
iso_codes6 <- iso_codes6[[1]][,-1]
iso_codes6 <-
  iso_codes6[!apply(iso_codes6, 1, function(x) {
    all(x == x[1])
  }),]

total <-
  rbind(iso_codes,
        iso_codes2,
        iso_codes3,
        iso_codes4,
        iso_codes5,
        iso_codes6)
names(total) <- c("Country", "born_country_code", "ISO3", "UN")

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)

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
    
    tabPanel(
      "Preemiad kaardil",
      sidebarPanel(
        h2("Perioodi sisestamine"),
        p("Sisesta aastate vahemik - abil"),
        textInput("text", label = h3("Sisesta aastad:"), value = "1901-2020"),
      ),
      mainPanel
      (br(),
        girafeOutput("mapplot"))
    )
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
  
  #Map
  output$mapplot <- renderGirafe({
    y1 = as.integer(strsplit(input$text, split = "-")[[1]][1])
    y2 = as.integer(strsplit(input$text, split = "-")[[1]][2])
    if (y2 > y1) {
      x4 <- count(data$born_country_code[data$year >= y1 & data$year <= y2])
    }
    else
    {
      x4 <- count(data$born_country_code[data$year >= y2 & data$year <= y1])
    }
    
    total$Value <-
      x4$freq[match(total$born_country_code, x4$x, nomatch = NA_integer_)]
    
    
    total$Country[total$Country == 'United States of America'] <-
      'USA'
    total$Country[total$Country == 'United Kingdom'] <- 'UK'
    total$Country[total$Country == 'Viet Nam'] <- 'Vietnam'
    total$Country[total$Country == 'Virgin Islands, US'] <-
      'Virgin Islands'
    total$Country[total$Country == 'Wallis and Futuna Islands'] <-
      'Wallis and Futuna'
    total$Country[total$Country == 'Venezuela (Bolivarian Republic)'] <-
      'Venezuela'
    total$Country[total$Country == 'Tanzania, United Republic of'] <-
      'Tanzania'
    total$Country[total$Country == 'Taiwan, Republic of China'] <-
      'Taiwan'
    total$Country[total$Country == 'Syrian Arab Republic (Syria)'] <-
      'Syria'
    total$Country[total$Country == 'Saint-Martin (French part)'] <-
      'Saint Martin'
    total$Country[total$Country == 'Saint-Barthélemy'] <-
      'Saint Barthelemy'
    total$Country[total$Country == 'Russian Federation'] <- 'Russia'
    total$Country[total$Country == 'Réunion'] <- 'Reunion'
    total$Country[total$Country == 'Pitcairn'] <- 'Pitcairn Islands'
    total$Country[total$Country == 'Palestinian Territory'] <-
      'Palestine'
    total$Country[total$Country == 'Micronesia, Federated States of'] <-
      'Micronesia'
    total$Country[total$Country == 'Macedonia, Republic of'] <-
      'North Macedonia'
    total$Country[total$Country == 'Lao PDR'] <- 'Laos'
    total$Country[total$Country == 'Korea (South)'] <- 'South Korea'
    total$Country[total$Country == 'Korea (North)'] <- 'North Korea'
    total$Country[total$Country == 'Iran, Islamic Republic of'] <-
      'Iran'
    total$Country[total$Country == 'Holy See (Vatican City State)'] <-
      'Vatican'
    total$Country[total$Country == 'Heard and Mcdonald Islands'] <-
      'Heard Island'
    total$Country[total$Country == 'French Southern Territories'] <-
      'French Southern and Antarctic Lands'
    total$Country[total$Country == 'Falkland Islands (Malvinas)'] <-
      'Falkland Islands'
    total$Country[total$Country == 'Cocos (Keeling) Islands'] <-
      'Cocos Islands'
    total$Country[total$Country == 'Brunei Darussalam'] <- 'Brunei'
    total$Country[total$Country == 'Congo (Brazzaville)'] <-
      'Republic of Congo'
    total$Country[total$Country == 'Congo, (Kinshasa)'] <-
      'Democratic Republic of the Congo'
    
    world_data['ISO3'] <-
      total$ISO3[match(world_data$region, total$Country)]
    
    
    plotdf <- total
    plotdf <- plotdf[!is.na(plotdf$ISO3),]
    
    world_data$Value <-
      plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
    
    
    g <- ggplot() +
      geom_polygon_interactive(
        data = world_data,
        color = 'gray70',
        size = 0.1,
        aes(
          x = long,
          y = lat,
          fill = world_data$Value,
          group = group,
          tooltip = world_data$Value
        )
      ) +
      scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') +
      scale_y_continuous(limits = c(-60, 90), breaks = c()) +
      scale_x_continuous(breaks = c())
    
    map <- girafe(ggobj = g)
    map
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)