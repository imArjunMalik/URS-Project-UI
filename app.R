####################################
#
# Main file for the project's UI. 
# @author Arjun Malik.
#
####################################

# load the necessary packages.
library(shiny)
library(shinythemes)
library(bslib)
#library(igraph)
library(shinyjs)
library(data.table)
#library(org.Hs.eg.db)


data <- read.csv('random_data.csv', header = TRUE)
class(data)
# network <- fread("brain_top")
# network.df <- as.data.frame(network)
# 
# network.df <- mapIds(org.Hs.eg.db, keys = 'NAT1', keytype = "SYMBOL", column = "ENTREZID")

# I graph lab 


# Define UI (ui.R file)
ui <- fluidPage( # theme = shinytheme("yeti"), 
  theme = bs_theme(version = 5, bootswatch = "minty"),
                navbarPage(
                  "Project UI", 
                  tabPanel("Home",
                           sidebarPanel(
                             shinyjs::useShinyjs(),
                             div (
                               id = "form", 
                               tags$h3("Input:"), 
                               textInput("gene", "Gene Symbol:",""), # gene sent to server
                               actionButton("submitbutton", "Submit", class = "btn btn-primary"), 
                               actionButton("refresh", "Refresh", icon("refresh"))
                                 )  
                             ), # sidebar panel 
    
                  mainPanel( 
                    div ( #id = "form",
                    fluidRow(
                      column(12, h2("Results")),
                      column(4, verbatimTextOutput("txtout")),#txtout received from server
                      column(4, tableOutput('tabledata')) # Prediction results table received from server
                    ),
                    fluidRow(
                      id = "form",
                      column(6,  align = "right",
                             style = "margin-top: 95px",
                             plotOutput("barplot")) # received from server
                    )
                  ))
                  
                  ), #tab panel 1
                  tabPanel("Help"
                    
                  ) # Help Tab Panel
                ) #navbar page 
) # fluid page

# Server
server <- function(input, output, session) {
  
  observeEvent(input$refresh, {
    refresh() # Refresh the page
  })
  
  output$txtout <- renderText({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Enter Gene Symbol.")
    }
  })
  
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      prediction <- subset(data, data$geneID == input$gene)
    }
  })
  
  output$barplot <- renderPlot({
    if (input$submitbutton>0) {

    barplot(height = c(data[data$geneID == input$gene, "alz"],
                       data[data$geneID == input$gene, "scz"],
                       data[data$geneID == input$gene, "autism"]),
             main = input$gene,
             names.arg = c("alz", "scz", "autism"),
             xlab = "Disorders/Diseases",
             ylab = "Score")
    }
  })

} # server

# Run the application 
shinyApp(ui = ui, server = server)
