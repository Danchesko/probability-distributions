server <- function(input,output){
  
  rand_samp <- reactive({
    rchisq(input$obs, input$df)
  })
  
  quantiles <- reactive({
    seq(0,max(rand_samp()),(max(rand_samp())/1000))
  })
  
  prob_dens <- reactive({
    dchisq(quantiles(), input$df) 
  })
  
  
  output$plot_sample <-renderPlot({
    hist(rand_samp(),main="",xlab="Quantiles",col="grey")
  },
  width=400,height=400
  )
  
  output$summary <- renderPrint({
    summary(rand_samp())
  })
  
  output$plot_prob_dens <-renderPlot({
    plot(quantiles(), prob_dens(), type="l", ylab="Probability density", xlab="Quantiles")
  },
  width=400,height=400
  )
}

ui <- shinyUI(pageWithSidebar(
  
  headerPanel(""),
  
  sidebarPanel(
    sliderInput("obs", 
                label = "Number of observations:", 
                min = 1, 
                max = 1000,
                value = 500,
                step=1),  
    
    sliderInput("df",
                "degrees of freedom:",
                min=1,
                max=100,
                value=50,
                step=1)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Random Sample",
               plotOutput("plot_sample"),
               h6("Summary:"),
               verbatimTextOutput("summary")
      ),
      tabPanel("Probability Density",
               plotOutput("plot_prob_dens")
      ) 
    )
  )
  
))