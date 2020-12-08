library(shiny)
library(dplyr)
library(shinycssloaders)
library(ggplot2)
library(lattice)

shinyUI(fluidPage(
    
   # titlePanel("Advance Text Analysis"),
    titlePanel(title=div(img(src="logo.png",align='right'),"Advance Text Analysis")),
    # Input in sidepanel:
    sidebarPanel(
        
        fileInput("file", "Upload text file"),
        fileInput("model","Upload model file"),
        htmlOutput('pos_select_ui'),
        textInput('stopw',label = "Enter stopwords seperated by comma"),
        sliderInput('pos_slider',"Select top k words to display",min = 1,max = 100,value = 20,step = 1),
        #progressBar(id = "pb4", value = 50, display_pct = TRUE)
        selectInput("key_algo",'Select keyword extraction algorithm',choices = c("RAKE","Noun-Verb Phrase")),
        sliderInput('key_slider',"Select top k keywords to display",min = 1,max = 100,value = 20,step = 1)
        
        ),
    
    # Main Panel:
    mainPanel( 
        tabsetPanel(type = "tabs",
                    #
                    tabPanel("Overview",h4(p("How to use this App")),
                             
                             p("To use this app you need two files one is document corpus in txt file format & other one is language specific model file."),
                             
                             tags$b("Step 1: Upload Corpus"),
                             p("Make sure each document is separated from another document with a new line character.
                                click on Browse in left-sidebar panel and upload the txt file."),
                             
                             tags$b("Step 2: Upload Model"),
                             p("Once the corpus is uploaded successfully, click on browse below file upload option in left-sidebar panel and upload the model.
                               when file & model are uploaded it will take some time to finish annotation in back-end.", align = "justify"),
                             
                             p("After annotaion is complete you can plot most frequent pos tag and keyword phrase")
                             #, height = 280, width = 400
                    ),
                    tabPanel("Example dataset", h4(p("Download Sample text file")), 
                             downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                             p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                             img(src = "example1.png")),
                    
                    tabPanel("POS TAG",h4("POS TAG"), dataTableOutput(outputId = "a_table"),
                             withSpinner(plotOutput("pos_plot"))
                             ),
                  
                    tabPanel("Keyword Extraction",
                             h4('Keyword Extraction'),
                             withSpinner(plotOutput('key_plot'))
                             
                    )
                    
        )
    )
)
)
