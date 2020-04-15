library(shiny)
library(ggplot2)
library(latex2exp)
library(magrittr)
library(ggpubr)

# N_0 <- 1
# L_0 <- 1
# birth_rate <- 0.03
# death_rate <- function(consumption){0.01-log(consumption)}
# g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
# G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
# tfp <- 1.0
# alpha_used <- 0.75

ui <- fluidPage(
  withMathJax(),
   titlePanel("Ein Malthusianisches Wachstumsmodell neoklassischer Art"),
  fluidRow(
    column(4,
       checkboxInput("compa_cases", "Komparativ-statische Analyse?", value = F),
       sliderInput("N_0", 
                   label='Bevölkerungsniveau \\( N_0 \\)',
                   min = 0.25, max = 2, step=0.05, value = 1.0),
       helpText("Startwert Faktor Bevölkerung (endogen)"),
       sliderInput("L_0", 
                   label='Parameter \\( L_0 \\)',
                 min = 0.5, max = 1.5, step=0.1, value = 1.0),
       helpText("Faktor Land (fix)"),
              sliderInput("birth_rate", 
                   label='Parameter \\( b \\)',
                   min = 0, max = 0.8, step=0.01, value = 0.03),
       helpText("Geburtenrate"),
       sliderInput("death_rate", 
                   label='Parameter \\( d \\)',
                   min = 0.0, max = 0.8, step=0.01, value = 0.04),
       helpText("Sterberate"),
       sliderInput("tfp", 
                   label='Parameter \\( A \\)',
                   min = 0.5, max = 1.5, step=0.01, value = 1.0),
       helpText("Totale Faktorproduktivität"),
       sliderInput("alpha_used", 
                   label='Parameter \\( \\alpha \\)',
                   min = 0.1, max = 0.9, step=0.1, value = 0.6),
       helpText("Outputelastizität für Faktor Land")
       ),
     column(4,
       plotOutput("pop_plot"),
       plotOutput("prod_plot")),
    column(4,
       plotOutput("consumption_plot"),
       plotOutput("population_plot")),
    column(4),
    column(8,
           downloadButton("downloadPlot", "Download der Abbildungen im PDF Format"),
           p("Dabei ergeben sich folgende Gleichgewichtswerte:"),
           tableOutput("equil_values"),
       #uiOutput('f1'),
       #uiOutput('f2'),
       p("Eine genaue Beschreibung des Modelles und der Implementierung in R finden Sie im Begleitdokument (Moodle oder Github).")
       )
     )
   )

server <- function(input, output) {
  # output$f1 <- renderUI({
  #   withMathJax(
  #     helpText('$$\\frac{\\partial N}{\\partial t} = \\gamma WN - \\delta N$$'))
  # })
  # output$f2 <- renderUI({
  #   withMathJax(
  #     helpText('$$\\frac{\\partial W}{\\partial t} = (1+\\alpha) W + \\beta WN - \\delta W$$'))
  # })
  production <- function(total_factor_productivity, land, population, alpha_value){
    total_factor_productivity * land**alpha_value * population**(1-alpha_value)
  }
  
  pop_plot <- reactive({
    birth_rate <- input$birth_rate
    death_rate <- function(consumption){input$death_rate-log(consumption)}
    g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
    G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
    
    ggplot(data.frame(x=c(0,5)), aes(x)) + 
      stat_function(fun=G_c, args = list(birthrate = birth_rate)) + 
      ggtitle(TeX("Die Bevölkerungswachstumsfunktion $G(c_t)$")) +
      xlab("Konsum") + ylab(TeX("$N_{t+1}/N_t$")) +
      scale_y_continuous(expand = expansion()) +
      theme_bw() + theme(panel.border = element_blank(), 
                         axis.line = element_line())
  })
  
  output$pop_plot <- renderPlot({
    pop_plot()
  })
  
  prod_plot <- reactive({
    L_0 <- input$L_0
    N_0 <- input$N_0
    tfp <- input$tfp
    alpha_used <- input$alpha_used

    ggplot(data.frame(x=c(0,2)), aes(x)) + 
      stat_function(fun=production, args = list(total_factor_productivity = tfp, 
                                                land=L_0, alpha_value=alpha_used)) + 
      ggtitle("Die Produktionsfunktion") +
      scale_y_continuous(expand = expansion()) +
      xlab("Bevölkerung") + ylab(TeX("$Y_t$")) +
      theme_bw() + theme(panel.border = element_blank(), 
                         axis.line = element_line())
  })
  output$prod_plot <- renderPlot({
    prod_plot()
  })
  
  consumption_plot <- reactive({
    birth_rate <- input$birth_rate
    death_rate <- function(consumption){input$death_rate-log(consumption)}
    g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
    G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
    
    get_c_ss <- function(birth_rate){
      g_c_1 <- function(consumption, birthrate){G_c(consumption, birthrate) - 1}
      uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate)$root
    }
    
    c_ss <- get_c_ss(birth_rate)
    
    ggplot(data.frame(x=c(0,5)), aes(x)) + 
      stat_function(fun=G_c, args = list(birthrate = birth_rate)) + 
      ggtitle("Gleichgewichtskonsum") +
      scale_y_continuous(expand = expansion()) +
      xlab("Konsum") + ylab(TeX("$N_{t+1}/N_t$")) +
      geom_segment(aes(x = -Inf, y = 1, xend = c_ss, yend = 1), linetype="dashed") +
      geom_segment(aes(x = c_ss, y = -Inf, xend = c_ss, yend = 1), linetype="dashed") +
      theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
  })
  output$consumption_plot <- renderPlot({
    consumption_plot()
  })
  population_plot <- reactive({
    L_0 <- input$L_0
    N_0 <- input$N_0
    tfp <- input$tfp
    alpha_used <- input$alpha_used
    birth_rate <- input$birth_rate
    
    death_rate <- function(consumption){input$death_rate-log(consumption)}
    g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
    G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
    
    get_c_ss <- function(birth_rate){
      g_c_1 <- function(consumption, birthrate){G_c(consumption, birthrate) - 1}
      uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate)$root
    }
    
    c_ss <- get_c_ss(birth_rate)
    
    interval_check <- seq(0, 2, 0.05)
    
    total_consumption <- function(pop_total, consumption_total){
      pop_total*consumption_total
    }
    
    n_ss <- uniroot(function(x) total_consumption(
      x, consumption_total=c_ss)-production(
        population=x, 
        total_factor_productivity=tfp, 
        land=L_0, 
        alpha_value = alpha_used), 
      c(0.5, 2.5), extendInt = "yes")$root
    
    y_ss <- production(
      population=n_ss, 
      total_factor_productivity=tfp, 
      land=L_0, 
      alpha_value = alpha_used)
    
    ggplot(data.frame(x=c(0, 2)), aes(x)) + 
      stat_function(fun=production, args = list(total_factor_productivity = tfp, 
                                                land=L_0, alpha_value=alpha_used)) + 
      ggtitle("Das Bevölkerungsgleichgewicht") +
      scale_y_continuous(expand = expansion()) +
      xlab("Bevölkerung") + ylab(TeX("$Y_t$")) +
      geom_segment(aes(x = -Inf, y = y_ss, xend = n_ss, yend = y_ss), linetype="dashed") +
      geom_segment(aes(x = n_ss, y = -Inf, xend = n_ss, yend = y_ss), linetype="dashed") +
      geom_abline(intercept = 0, slope = c_ss) +
      theme_bw() + theme(panel.border = element_blank(), 
                         axis.line = element_line())
  })
  output$population_plot <- renderPlot({
    population_plot()
  })
  
  output$equil_values <- renderTable({
    birth_rate <- input$birth_rate
    death_rate <- function(consumption){input$death_rate-log(consumption)}
    g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
    G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
    
    L_0 <- input$L_0
    N_0 <- input$N_0
    tfp <- input$tfp
    alpha_used <- input$alpha_used
    
    get_c_ss <- function(birth_rate){
      g_c_1 <- function(consumption, birthrate){G_c(consumption, birthrate) - 1}
      uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate)$root
    }
    c_ss <- get_c_ss(birth_rate)
    
    interval_check <- seq(0, 2, 0.05)
    
    total_consumption <- function(pop_total, consumption_total){
      pop_total*consumption_total
    }
    
    n_ss <- uniroot(function(x) total_consumption(
      x, consumption_total=c_ss)-production(
        population=x, 
        total_factor_productivity=tfp, 
        land=L_0, 
        alpha_value = alpha_used), 
      c(0.5, 1.5), extendInt = "yes")$root
    
    y_ss <- production(
      population=n_ss, 
      total_factor_productivity=tfp, 
      land=L_0, 
      alpha_value = alpha_used)
    
    get_w_ss <- function(alpha_values, y_t, n_t){
      (1-alpha_values) * (y_t/n_t)
    }
    
    get_r_ss <- function(alpha_values, y_t, l_t){
      alpha_values*(y_t/l_t)
    }

    w_ss <- get_w_ss(alpha_used, y_ss, n_ss)
    r_ss <- get_r_ss(alpha_used, y_ss, L_0)
    
    data.frame(
      Variable=c("Konsum", "Bevölkerung", "Produktion", "Lohn", "Landrente"),
      Gleichgewichtswert=c(c_ss, n_ss, y_ss, w_ss, r_ss))
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("malthus.pdf", sep = "")
    },
    content = function(file) {
      full_plot <- ggarrange(pop_plot(), consumption_plot(), 
                             prod_plot(), population_plot(), 
                             ncol = 2, nrow = 2,
                             labels = paste0(LETTERS[1:4], ")"), 
                             font.label = list(face="bold"))
      ggsave(file, plot = full_plot, width = 8, height = 6)
    }
  )
}

shinyApp(ui = ui, server = server)