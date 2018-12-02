server <- function(input,output){
  
  rand_samp <- reactive({
    rnorm(input$obs, input$mean, input$sd)
  })

  quantiles <- reactive({
    seq(input$mean - 3*input$sd, input$mean + 3*input$sd, 0.01) 
  })
  
  prob_dens <- reactive({
    dnorm(quantiles(), input$mean, input$sd) 
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
    
    sliderInput("mean",
                "Mean:",
                min=-50,
                max=50,
                value=0,
                step=0.1),
    
    sliderInput("sd",
                "Standard deviation:",
                value=50,
                min=0.1,
                max=100,
                step=0.1)
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