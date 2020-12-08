#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.maxRequestSize=100*1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
        else {
            Document = readLines(input$file$datapath)
            Doc.id=seq(1:length(Document))
            calib=data.frame(Doc.id,Document)
            return(calib)}
    })
    
    anotated_data <- reactive({
        if (is.null(input$model)) {return(NULL)}
        else{
            model <- udpipe_load_model(file = input$model$datapath)
            x <- udpipe_annotate(model, x = dataset()[,2])
            x <- as.data.frame(x)
           # updateProgressBar(session = session, id = "pb4", value = input$slider)
            
        }
        return(x)
        
    })
    
    
    # Select variables:
    output$pos_select_ui <- renderUI({
        if (is.null(input$file)) { return(NULL) }
        else{
            
            radioButtons("pos_select", "Display most frequent",
                               choiceNames = 
                                   list('Noun', 'Verb','Adjective', 'Adverb'),
                               choiceValues =
                                   list("NOUN", "VERB", "ADJ", "ADV")
            )
            
        
        }
    })
    
    stopw <- reactive({
        stpw <- unlist(strsplit(input$stopw,","))
        return(stpw)
    })

    output$pos_plot <- renderPlot({
        
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            stats <- subset(anotated_data(), upos %in% input$pos_select) 
            stats <- txt_freq(stats$token)
            
            if(length(stopw()!=0)){
            stats <- stats %>%
                    select(key, freq, freq_pct) %>% 
                    filter(!key %in% stopw())
            }
            
            stats$key <- factor(stats$key, levels = rev(stats$key))
            
            barchart(key ~ freq, data = head(stats, input$pos_slider), col = "cadetblue", 
                     main = paste0("Top 20 Most occurring"," ",input$pos_select), xlab = "Freq")
        }
        
    })
    
    
    output$key_plot <- renderPlot({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            if(input$key_algo=="RAKE"){
                stats <- keywords_rake(x = anotated_data(), term = "lemma", group = "doc_id", 
                                       relevant = anotated_data()$upos %in% c("NOUN", "ADJ"))
                stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                p1 <- barchart(key ~ rake, data = head(subset(stats, freq > 3), input$key_slider), col = "red", 
                               main = "Top Keywords identified by RAKE", 
                               xlab = "Rake")
                print(p1)
            }
            if(input$key_algo=="Noun-Verb Phrase"){
                phrase_tags <- as_phrasemachine(anotated_data()$upos, type = "upos")
                stats <- keywords_phrases(x = phrase_tags, term = tolower(anotated_data()$token),
                                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                                          is_regex = TRUE, detailed = FALSE)
                stats <- subset(stats, ngram > 1 & freq > 3)
                stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                p2<- barchart(key ~ freq, data = head(stats, input$key_slider), col = "magenta",
                              main = "Keywords - simple noun phrases", xlab = "Frequency")
                print(p2)
                
            }
        }
        
    })
    
})
