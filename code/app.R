library(shiny)
library(shinyjs)
library(distr6)

ui <- fluidPage(
  sidebarPanel(width = 3,
               useShinyjs(),
               h4("Reset"),
               actionButton("reset", "Reset"),
               div(id = 'div_draw',
                   h4("Draw"),
                   selectInput("fun", "Function", c("p(T)", "F(T)", "S(T)", "h(T)", "H(T)"), "S(T)"),
                   fluidRow(
                     column(3, numericInput("xmin", "xmin", 0)),
                     column(3, numericInput("xmax", "xmax", 10)),
                     column(3, numericInput("ymin", "ymin", 0)),
                     column(3, numericInput("ymax", "ymax", 1))
                   ),
                   actionButton("draw", "Interpolate"),
               ),
               div(id = 'div_sim',
                   h4("Simulate", id = "sim"),
                   numericInput("simulate", "n", 100),
                   actionButton("btn_sim", "Simulate")
               )
  ),
  mainPanel(width = 9,
            h4("Click on plot to start drawing, click again to pause", id = "txt"),
            textOutput("f"),
            dataTableOutput("xy"),
            fluidRow(
              column(width = 4,
                     plotOutput("int_plot", width = "700px", height = "700px",
                                hover=hoverOpts(id = "hover", delay = 100,
                                                delayType = "throttle",
                                                clip = TRUE, nullOutside = TRUE),
                                click="click"),
                     plotOutput("static_plot", width = "400px", height = "400px")
              ),
              column(8, plotOutput("dplot", width = "400px", height = "400px"))
            ),
            div(id = 'div_samples',
                h3("Samples"),
                fluidRow(
                  column(3, plotOutput("dplot_dens", width = "300px", height = "300px")),
                  column(3, plotOutput("plt_dens", width = "300px", height = "300px")),
                  column(6, textOutput("samples"))
                )
            )
  )
)
server <- function(input, output, session) {
  shinyjs::hide(id = "dplot")
  shinyjs::hide(id = "div_samples")
  shinyjs::hide(id = "div_sim")
  shinyjs::hide(id = "draw")
  vals = reactiveValues(x=NULL, y=NULL)
  draw = reactiveVal(FALSE)
  x = cdf = 1
  output$dplot = NULL
  observeEvent(input$click, handlerExpr = {
    temp <- draw(); draw(!temp)
    if(!draw()) {
      vals$x <- c(vals$x, NA)
      vals$y <- c(vals$y, NA)
    }})
  observeEvent(input$reset, handlerExpr = {
    shinyjs::hide(id = "div_sim")
    shinyjs::hide(id = "div_samples")
    shinyjs::hide(id = "static_plot")
    shinyjs::hide(id = "dplot")
    shinyjs::show(id = "int_plot")
    shinyjs::show(id = "txt")
    shinyjs::show(id = "div_draw")
    vals$x <- vals$y <- NULL
    x <- cdf <- 1
  })
  observeEvent(input$hover, {
    shinyjs::show(id = "draw")
    if (draw()) {
      vals$x <- c(vals$x, input$hover$x)
      vals$y <- c(vals$y, input$hover$y)
    }})
  output$int_plot= renderPlot({
    plot(x=vals$x, y=vals$y, xlim=c(input$xmin, input$xmax),
         ylim=c(input$ymin, input$ymax), ylab=input$fun, xlab="T", type="l", lwd=1)
  })

  observeEvent(input$draw, handlerExpr = {
    shinyjs::hide(id = "int_plot")
    shinyjs::hide(id = "div_draw")
    shinyjs::hide(id = "txt")
    
    keep <- !(is.na(vals$x) | is.na(vals$y) | duplicated(vals$x))
    vals$x = vals$x[keep]
    vals$y = vals$y[keep]

    ord = order(vals$x)
    vals$x = vals$x[ord]
    vals$y = vals$y[ord]
    
    cdf = switch(input$fun,
                 "p(T)" = c(vals$y, cumsum(vals$y)),
                 "F(T)" = vals$y,
                 "S(T)" = 1 - vals$y,
                 "H(T)" = 1 - exp(-vals$y),
                 "h(T)" = 1 - exp(c(vals$y, cumsum(vals$y))))
    valid <- vals$x >= 0 & cdf <= 1 & cdf >= 0
    
    # the following code ensures that cdf is increasing. So that during simulation, there is no negative probabilty during simulation
    currCdf <- 0
    keep <- c()
    for(i in which(valid)){
      if(cdf[i] >= currCdf){
          keep <- c(keep, i)
          currCdf <- cdf[i]
      }
    }
    
    cdf = cdf[keep]
    vals$x  = vals$x[keep]
    vals$y  = vals$y[keep]
    
    output$xy = renderDataTable({matrix(x, cdf, nrow = 2)})
    shinyjs::show(id = "xy")
    distr <<-  distr6::dstr("WeightedDiscrete", cdf = cdf, x = vals$x)
    output$dplot = renderPlot({
      plot(distr, fun = "all")
    })
    output$dplot_dens = renderPlot({
      plot(distr, fun = "pdf")
    })

    output$static_plot = renderPlot({
      plot(x=vals$x, y=vals$y, xlim=c(input$xmin, input$xmax),
           ylim=c(input$ymin, input$ymax), ylab=input$fun, xlab="T", type="l", lwd=1)
    })

    shinyjs::show(id = "dplot")
    shinyjs::show(id = "div_sim")
    shinyjs::show(id = "static_plot")
  })

  observeEvent(input$btn_sim, handlerExpr = {
    samples = as.numeric(distr$rand(input$simulate))
    output$plt_dens = renderPlot({plot(density(samples))})
    output$samples = renderText(samples)

    shinyjs::show(id = "div_samples")
  })

}
shinyApp(ui, server)
