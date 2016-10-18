library(ggplot2)
source("predict.R")


drawPlot = function(predTable) {
    g = ggplot(predTable, aes(x = word, y = prob, fill = word))
    g = g + geom_bar(stat = "identity")
    print(g)
}

# Server
shinyServer(function(input, output) {
    txtInput = reactive({
        predictWord(input$termText, n = 15)
    })
    
    output$predPlot = renderPlot({
        drawPlot(txtInput())
    })

    output$prediction = renderPrint({ txtInput() })
    output$predText   = renderPrint({ cat(txtInput()[1,"word"]) })
})

