library(shiny)
library(data.table)

abst <- readxl::read_excel("abstracts.xlsx")
setDT(abst)
titles <- 1:length(abst$Title)
names(titles) <- as.list(abst$Title)
    

# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("uRos 2020 Abstracts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("title",
                        h3("Abstract title"),
                        choices = titles)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h2(textOutput("curTitle")),
           h3(textOutput("curAuthor")),
           htmlOutput("curAbstract")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$curTitle <- renderText({
        abst[as.numeric(input$title), Title]
    })
    output$curAuthor <- renderText({
        cur <- abst[as.numeric(input$title), ]    
        curAuthor <- vector()
        for(i in 1:6){
            if(!is.na(cur[[paste0("Author",i)]])){
                curAuthor[i] <- paste0(cur[[paste0("Author",i)]],", ",cur[[paste0("Affiliation",i)]])
            }
        }
        paste(curAuthor, collapse=";")
    })
    output$curAbstract <- renderUI({
        cur <- abst[as.numeric(input$title), ]
        out <- list()
        out[[length(out)+1]] <- p(HTML(gsub("\\n","<br/>",cur[["Abstract"]])))
        if(!is.na(cur$References)){
            out[[length(out)+1]] <- h3("References")
            out[[length(out)+1]] <- p(HTML(gsub("\\n","<br/>",cur[["References"]])))
        }
        out
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
