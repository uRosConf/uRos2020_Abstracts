library(shiny)
library(data.table)

abst <- readxl::read_excel("abstracts.xlsx")
setDT(abst)
titles <- 1:length(abst$Title)
names(titles) <- as.list(abst$Title)
types <- c("All", sort(unique(abst[["Type of talk"]])))

ui <- fluidPage(
    titlePanel("uRos 2020 Abstracts"),
    sidebarLayout(
        sidebarPanel(
            selectInput("type", h3("Type of talk"), choices = types),
            selectInput("title", h3("Abstract title"), choices = titles),
        ),
        mainPanel(
            fluidRow(
                column(
                    width = 12,
                    align = "center",
                    h3(code("Title")), h3(strong(textOutput("curTitle")))
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    align = "center",
                    h3(code("Authors")), h5(htmlOutput("curAuthor"))
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    align = "center",
                    h3(code("Content")), h5(htmlOutput("curAbstract"))
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    align = "center",
                    h3(code("References")), h5(htmlOutput("curReferences"))
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
        refs <- abst[as.numeric(input$title), References]
        if (!is.na(refs)) {
            out <- p(HTML(gsub("\\n", "<br/>", refs)))
        } else {
            out <- "not-available"
        }
        out
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
