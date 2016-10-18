shinyUI(fluidPage(

    # Title
    titlePanel("Abby Word Prediction Engine"),

    # Layout
    sidebarLayout(
        sidebarPanel(
            h2("Input and Settings"),
            textInput( inputId = "termText",
                     , label   = "initial text",
                     , value   = ""
                     ),
            submitButton("submit")
        ),
        mainPanel(
            h2("Results"),
            textOutput("predText"),
            verbatimTextOutput("prediction"),            
            plotOutput("predPlot", height = "600px")
        )
    )

))
