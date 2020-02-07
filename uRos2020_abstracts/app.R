library(shiny)
library(data.table)
library(wordcloud2)
library(tm)

# abst <- readxl::read_excel("abstracts.xlsx")
# setDT(abst)
abst <- readRDS("abstracts.Rds")
abst <- abst[sample(1:nrow(abst))]
setorderv(abst, c("Type of talk", "Title"))

titles <- 1:length(abst$Title)
names(titles) <- as.list(abst$Title)
types <- c("All", sort(unique(abst[["Type of talk"]])))

wordcloud_data <- function(abstracts) {
  corp <- Corpus(VectorSource(paste(unlist(abstracts), collapse = "")))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  corp <- tm_map(corp, stripWhitespace)
  m <- as.matrix(TermDocumentMatrix(corp))
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(
    word = names(v),
    freq = v,
    stringsAsFactors = FALSE)

  d <- d[!d$word %in% c("one", "new", "will", "can", "using", "used", "use"), ]
  d
}

suppressWarnings(wcd <- wordcloud_data(abst$Abstract))

ui <- fluidPage(
  titlePanel("uRos 2020 Abstracts"),
  tags$script(HTML("
    window.addEventListener('resize', function(){
      if ($(window).width()<=755) {
        $('#wordcloud').hide();
      } else {
        $('#wordcloud').show();
      }
    });
  ")),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", h3("Type of talk"), choices = types),
      selectInput("title", h3("Abstract title"), choices = titles),
      hr(),
      wordcloud2Output("wordcloud")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 12,
          align = "left",
          h3(code("Title")), h3(strong(textOutput("curTitle")))
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          h3(code("Authors")), h5(htmlOutput("curAuthor"))
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          h3(code("Content")), h5(htmlOutput("curAbstract"))
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "left",
          htmlOutput("curReferences")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  output$curTitle <- renderText({
    abst[as.numeric(input$title), Title]
  })

  output$curAuthor <- renderText({
    cur <- abst[as.numeric(input$title),]
    curAuthor <- vector()
    for (i in 1:6) {
      auth <- cur[[paste0("Author", i)]]
      if (!is.na(auth)) {
        curAuthor[i] <- paste0(auth, " (", cur[[paste0("Affiliation", i)]], ")")
      }
    }
    paste(curAuthor, collapse = "<br />")
  })

  output$curAbstract <- renderUI({
    cur <- abst[as.numeric(input$title), ]
    return(p(HTML(gsub("\\n", "<br/>", cur[["Abstract"]]))))
  })

  output$curReferences <- renderUI({
    out <- list()
    refs <- abst[as.numeric(input$title), References]
    if (!is.na(refs)) {
      out[[1]] <- h3(code("References"))
      out[[2]] <- p(HTML(gsub("\\n", "<br/>", refs)))
    }
    out
  })

  output$wordcloud <- renderWordcloud2({
    wordcloud2(data = wcd[wcd$freq > 3, ], "R", size = 0.75)
  })


  observeEvent(input$type, {
    if (input$type == "All") {
      vals <- titles
    } else {
      vals <- titles[abst[["Type of talk"]] == input$type]
    }
    updateSelectInput(session = session, inputId = "title", choices = vals)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
