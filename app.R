library(shiny)
library(ggplot2)
library(dplyr)
library(latex2exp)
library(magrittr)
library(ggpubr)

ui <- fluidPage(
  withMathJax(),
   titlePanel("Ein Malthusianisches Wachstumsmodell neoklassischer Art"),
  fluidRow(
    column(4,
       checkboxInput("compa_cases", "Komparativ-statische Analyse?", value = F),
       checkboxInput("show_init_pop", "Ausgangswert der Bevölkerung anzeigen?", 
                     value = F),
       h3("Ausgangsszenario"),
       sliderInput("N_0", 
                   label='Ausgangsniveau Bevölkerung \\( N_0 \\)',
                   min = 0.25, max = 2, step=0.05, value = 1.0),
       helpText("Ausgangsniveauwert Faktor Bevölkerung (endogen)"),
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
       helpText("Outputelastizität für Faktor Land"),
       conditionalPanel(
         h3("Alternativszenario"),
         condition = "input.compa_cases==1",
         sliderInput("N_2", 
                     label='Ausgangsniveau Bevölkerung \\( N_2 \\)',
                     min = 0.25, max = 2, step=0.05, value = 1.0),
         helpText("Ausgangsniveau Faktor Bevölkerung (endogen)"),
         sliderInput("L_2", 
                     label='Parameter \\( L_2 \\)',
                     min = 0.5, max = 1.5, step=0.1, value = 1.0),
         helpText("Faktor Land (fix)"),
         sliderInput("birth_rate_2", 
                     label='Parameter \\( b_2 \\)',
                     min = 0, max = 0.8, step=0.01, value = 0.03),
         helpText("Geburtenrate"),
         sliderInput("death_rate_2", 
                     label='Parameter \\( d_2 \\)',
                     min = 0.0, max = 0.8, step=0.01, value = 0.04),
         helpText("Sterberate"),
         sliderInput("tfp_2", 
                     label='Parameter \\( A_2 \\)',
                     min = 0.5, max = 1.5, step=0.01, value = 1.0),
         helpText("Totale Faktorproduktivität"),
         sliderInput("alpha_used_2", 
                     label='Parameter \\( \\alpha_2 \\)',
                     min = 0.1, max = 0.9, step=0.1, value = 0.6),
         helpText("Outputelastizität für Faktor Land")
       )
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
           h3("Beschreibung des Modells"),
       p("Eine genaue Beschreibung des Modelles und der Implementierung in R finden Sie im Begleitdokument (Moodle oder auf Github im Ordner 'beschreibung')."),
       h3("Benutzung der App"),
       p("Parameterwerte können auf der linken Seite geändert werden. Wenn Sie zwei Gleichgewichte in einer komparativ-statischen Analyse vergleichen wollen setzen Sie den Haken bei 'Komparativ-statische Analyse'. Dann können Sie mehrere Szenarien grafisch und quantitativ vergleichen. Der Download Button unter der Abbildung erlaubt Ihnen die aktuelle Version der Abbildungen als PDF herunterzuladen."),
       h3("Leitfragen"),
       p("1. Was impliziert eine Veränderung in der Sterberate, z.B. durch einen Virus?"),
       p("2. Was ist der Effekt eines Anstiegs der Geburtenrate?"),
       p("3. Wie zeigt sich eine Änderung der TFP durch eine technische Innovation?"),
       p("4. Welche Rolle spielt der Ausgangswert für die Bevölkerungsgröße? Warum?")
       )
     )
   )

server <- function(input, output) {
  production <- function(total_factor_productivity, land, population, alpha_value){
    total_factor_productivity * land**alpha_value * population**(1-alpha_value)
  }
  orig_color <- "#61a27c"
  alt_color <- "#b8103b"
  
  pop_plot <- reactive({
    if (input$compa_cases){
      birth_rate <- input$birth_rate
      birth_rate_2 <- input$birth_rate_2
      death_rate <- function(consumption){input$death_rate-log(consumption)}
      death_rate_2 <- function(consumption){input$death_rate_2-log(consumption)}
      g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
      g_c_2 <- function(consumption, birthrate){birth_rate_2 - death_rate_2(consumption)}
      G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
      G_c_2 <- function(consumption, birthrate){1 + g_c_2(consumption, birthrate)}
      
      ggplot(data.frame(x=c(0,5)), aes(x)) + 
        stat_function(fun=G_c, args = list(birthrate = birth_rate), 
                      aes(color="alt"), key_glyph = "rect") +
        stat_function(fun=G_c_2, args = list(birthrate = birth_rate_2), 
                      aes(color="neu"), key_glyph = "rect") +
        scale_color_manual(values = c(orig_color, alt_color)) +
        ggtitle(TeX("Die Bevölkerungswachstumsfunktion $G(c_t)$")) +
        xlab("Konsum") + ylab(TeX("Veränderungsrate der Bevölkerung: $N_{t+1}/N_t$")) +
        scale_y_continuous(expand = expansion()) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line(), 
                           legend.title = element_blank())
    } else{
      birth_rate <- input$birth_rate
      death_rate <- function(consumption){input$death_rate-log(consumption)}
      g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
      G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
      
      ggplot(data.frame(x=c(0,5)), aes(x)) + 
        stat_function(fun=G_c, args = list(birthrate = birth_rate)) + 
        ggtitle(TeX("Die Bevölkerungswachstumsfunktion $G(c_t)$")) +
        xlab("Konsum") + ylab(TeX("Veränderungsrate der Bevölkerung:$N_{t+1}/N_t$")) +
        scale_y_continuous(expand = expansion()) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line())
    }

  })
  
  output$pop_plot <- renderPlot({
    pop_plot()
  })
  
  prod_plot <- reactive({
    show_init_pop <- input$show_init_pop
    if (input$compa_cases){
      L_0 <- input$L_0
      L_2 <- input$L_2
      N_0 <- input$N_0
      N_2 <- input$N_2
      tfp <- input$tfp
      tfp_2 <- input$tfp_2
      alpha_used <- input$alpha_used
      alpha_used_2 <- input$alpha_used_2
      
      plot_object <- ggplot(data.frame(x=c(0,2)), aes(x)) + 
        stat_function(fun=production, 
                      args = list(total_factor_productivity = tfp, 
                                  land=L_0, alpha_value=alpha_used),
                      aes(color="alt"), key_glyph = "rect") + 
        stat_function(fun=production, 
                      args = list(total_factor_productivity = tfp_2, 
                                  land=L_2, alpha_value=alpha_used_2),
                      aes(color="neu"), key_glyph = "rect") + 
        scale_color_manual(values = c(orig_color, alt_color)) +
        ggtitle("Die Produktionsfunktion") +
        scale_y_continuous(expand = expansion()) +
        xlab("Bevölkerung") + ylab(TeX("Output $Y_t$")) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line(), 
                           legend.title = element_blank())
    } else{
      L_0 <- input$L_0
      N_0 <- input$N_0
      tfp <- input$tfp
      alpha_used <- input$alpha_used
      
      plot_object <- ggplot(data.frame(x=c(0,2)), aes(x)) + 
        stat_function(fun=production, args = list(total_factor_productivity = tfp, 
                                                  land=L_0, alpha_value=alpha_used)) + 
        ggtitle("Die Produktionsfunktion") +
        scale_y_continuous(expand = expansion()) +
        xlab("Bevölkerung") + ylab(TeX("Output $Y_t$")) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line())
    }
    
    if (show_init_pop){
      plot_object <- plot_object + 
        geom_vline(xintercept = N_0, color="#004c93") +
        annotate(geom="text", label=TeX("$N_0$"), 
                 x = N_0+0.05, y=0.125, color="#004c93")
    }
    plot_object
  })
  output$prod_plot <- renderPlot({
    prod_plot()
  })

  consumption_plot <- reactive({
    if (input$compa_cases){
      birth_rate <- input$birth_rate
      birth_rate_2 <- input$birth_rate_2
      death_rate <- function(consumption){input$death_rate-log(consumption)}
      death_rate_2 <- function(consumption){input$death_rate_2-log(consumption)}
      g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
      g_c_2 <- function(consumption, birthrate){birth_rate_2 - death_rate_2(consumption)}
      G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
      G_c_2 <- function(consumption, birthrate){1 + g_c_2(consumption, birthrate)}
      
      get_c_ss <- function(birth_rate){
        g_c_1 <- function(consumption, birthrate){G_c(consumption, birthrate) - 1}
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate)$root
      }
      
      get_c_ss_new <- function(birth_rate){
        g_c_1 <- function(consumption, birthrate){G_c_2(consumption, birthrate) - 1}
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate_2)$root
      }
      
      c_ss <- get_c_ss(birth_rate)
      c_ss_new <- get_c_ss_new(birth_rate_2)
      
      ggplot(data.frame(x=c(0,5)), aes(x)) + 
        stat_function(fun=G_c, args = list(birthrate = birth_rate),
                      aes(color="alt"), key_glyph = "rect") + 
        stat_function(fun=G_c_2, args = list(birthrate = birth_rate_2),
                      aes(color="neu"), key_glyph = "rect") + 
        scale_color_manual(values = c(orig_color, alt_color)) +
        ggtitle("Gleichgewichtskonsum") +
        scale_y_continuous(expand = expansion()) +
        xlab("Konsum") + ylab(TeX("Veränderungsrate der Bevölkerung: $N_{t+1}/N_t$")) +
        geom_segment(aes(x = -Inf, y = 1, xend = c_ss, yend = 1), 
                     linetype="dashed", color=orig_color) +
        geom_segment(aes(x = c_ss, y = -Inf, xend = c_ss, yend = 1), 
                     linetype="dashed", color=orig_color) +
        geom_segment(aes(x = -Inf, y = 1, xend = c_ss_new, yend = 1), 
                     linetype="dashed", color=alt_color) +
        geom_segment(aes(x = c_ss_new, y = -Inf, xend = c_ss_new, yend = 1), 
                     linetype="dashed", color=alt_color) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line(), 
                           legend.title = element_blank())
    } else{
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
        xlab("Konsum") + ylab(TeX("Veränderungsrate der Bevölkerung: $N_{t+1}/N_t$")) +
        geom_segment(aes(x = -Inf, y = 1, xend = c_ss, yend = 1), linetype="dashed") +
        geom_segment(aes(x = c_ss, y = -Inf, xend = c_ss, yend = 1), linetype="dashed") +
        theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
    }

  })
  output$consumption_plot <- renderPlot({
    consumption_plot()
  })
  population_plot <- reactive({
    show_init_pop <- input$show_init_pop
    if (input$compa_cases){
      L_0 <- input$L_0
      L_2 <- input$L_2
      N_0 <- input$N_0
      N_2 <- input$N_2
      tfp <- input$tfp
      tfp_2 <- input$tfp_2
      alpha_used <- input$alpha_used
      alpha_used_2 <- input$alpha_used_2
      
      birth_rate <- input$birth_rate
      birth_rate_2 <- input$birth_rate_2
      death_rate <- function(consumption){input$death_rate-log(consumption)}
      death_rate_2 <- function(consumption){input$death_rate_2-log(consumption)}
      g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
      g_c_2 <- function(consumption, birthrate){birth_rate_2 - death_rate_2(consumption)}
      G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
      G_c_2 <- function(consumption, birthrate){1 + g_c_2(consumption, birthrate)}
      
      get_c_ss <- function(birth_rate){
        g_c_1 <- function(consumption, birthrate){G_c(consumption, birthrate) - 1}
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate)$root
      }
      
      get_c_ss_new <- function(birth_rate){
        g_c_1 <- function(consumption, birthrate){G_c_2(consumption, birthrate) - 1}
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate_2)$root
      }
      
      c_ss <- get_c_ss(birth_rate)
      c_ss_new <- get_c_ss_new(birth_rate_2)
      
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
      
      n_ss_new <- uniroot(function(x) total_consumption(
        x, consumption_total=c_ss_new)-production(
          population=x, 
          total_factor_productivity=tfp_2, 
          land=L_2, 
          alpha_value = alpha_used_2), 
        c(0.5, 2.5), extendInt = "yes")$root
      
      y_ss <- production(
        population=n_ss, 
        total_factor_productivity=tfp, 
        land=L_0, 
        alpha_value = alpha_used)
      
      y_ss_new <- production(
        population=n_ss_new, 
        total_factor_productivity=tfp_2, 
        land=L_2, 
        alpha_value = alpha_used_2)
      
      plot_object <- ggplot(data.frame(x=c(0, 2)), aes(x)) + 
        stat_function(fun=production, 
                      args = list(total_factor_productivity = tfp, 
                                  land=L_0, alpha_value=alpha_used),
                      aes(color="alt"), key_glyph = "rect"
                      ) + 
        stat_function(fun=production, 
                      args = list(total_factor_productivity = tfp_2, 
                                  land=L_2, alpha_value=alpha_used_2),
                      aes(color="neu"), key_glyph = "rect"
        ) + 
        geom_abline(intercept = 0, slope = c_ss, 
                    color=orig_color, key_glyph = "rect") +
        geom_abline(intercept = 0, slope = c_ss_new, 
                    color=alt_color, key_glyph = "rect") +
        scale_color_manual(values = c(orig_color, alt_color)) +
        ggtitle("Das Bevölkerungsgleichgewicht") +
        scale_y_continuous(expand = expansion()) +
        xlab("Bevölkerung") + ylab(TeX("$Y_t$")) +
        geom_segment(aes(x = -Inf, y = y_ss, xend = n_ss, yend = y_ss), 
                     linetype="dashed", color=orig_color) +
        geom_segment(aes(x = n_ss, y = -Inf, xend = n_ss, yend = y_ss), 
                     linetype="dashed", color=orig_color) +
        geom_segment(aes(x = -Inf, y = y_ss_new, xend = n_ss_new, yend = y_ss_new), 
                     linetype="dashed", color=alt_color) +
        geom_segment(aes(x = n_ss_new, y = -Inf, xend = n_ss_new, yend = y_ss_new), 
                     linetype="dashed", color=alt_color) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line(), 
                           legend.title = element_blank())
    } else{
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
      
      plot_object <- ggplot(data.frame(x=c(0, 2)), aes(x)) + 
        stat_function(fun=production, args = list(total_factor_productivity = tfp, 
                                                  land=L_0, alpha_value=alpha_used)) + 
        ggtitle("Das Bevölkerungsgleichgewicht") +
        scale_y_continuous(expand = expansion()) +
        xlab("Bevölkerung") + ylab(TeX("Output: $Y_t$")) +
        geom_segment(aes(x = -Inf, y = y_ss, xend = n_ss, yend = y_ss), linetype="dashed") +
        geom_segment(aes(x = n_ss, y = -Inf, xend = n_ss, yend = y_ss), linetype="dashed") +
        geom_abline(intercept = 0, slope = c_ss) +
        theme_bw() + theme(panel.border = element_blank(), 
                           axis.line = element_line())
    }
    if (show_init_pop){
      plot_object <- plot_object + 
        geom_vline(xintercept = N_0, color="#004c93") +
        annotate(geom="text", label=TeX("$N_0$"), 
                 x = N_0+0.05, y=0.125, color="#004c93")
    }
    plot_object
  })
  output$population_plot <- renderPlot({
    population_plot()
  })
  
  equil_values <- reactive({
    if(input$compa_cases){
      L_0 <- input$L_0
      L_2 <- input$L_2
      N_0 <- input$N_0
      N_2 <- input$N_2
      tfp <- input$tfp
      tfp_2 <- input$tfp_2
      alpha_used <- input$alpha_used
      alpha_used_2 <- input$alpha_used_2
      
      birth_rate <- input$birth_rate
      birth_rate_2 <- input$birth_rate_2
      death_rate <- function(consumption){input$death_rate-log(consumption)}
      death_rate_2 <- function(consumption){input$death_rate_2-log(consumption)}
      g_c <- function(consumption, birthrate){birthrate - death_rate(consumption)}
      g_c_2 <- function(consumption, birthrate){birth_rate_2 - death_rate_2(consumption)}
      G_c <- function(consumption, birthrate){1 + g_c(consumption, birthrate)}
      G_c_2 <- function(consumption, birthrate){1 + g_c_2(consumption, birthrate)}
      
      get_c_ss <- function(birth_rate){
        g_c_1 <- function(consumption, birthrate){G_c(consumption, birthrate) - 1}
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate)$root
      }
      
      get_c_ss_new <- function(birth_rate){
        g_c_1 <- function(consumption, birthrate){G_c_2(consumption, birthrate) - 1}
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", birthrate=birth_rate_2)$root
      }
      
      get_w_ss <- function(alpha_values, y_t, n_t){
        (1-alpha_values) * (y_t/n_t)
      }
      
      get_r_ss <- function(alpha_values, y_t, l_t){
        alpha_values*(y_t/l_t)
      }
      
      c_ss <- get_c_ss(birth_rate)
      c_ss_new <- get_c_ss_new(birth_rate_2)
      
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
      
      n_ss_new <- uniroot(function(x) total_consumption(
        x, consumption_total=c_ss_new)-production(
          population=x, 
          total_factor_productivity=tfp_2, 
          land=L_2, 
          alpha_value = alpha_used_2), 
        c(0.5, 1.5), extendInt = "yes")$root
      
      y_ss_new <- production(
        population=n_ss_new, 
        total_factor_productivity=tfp_2, 
        land=L_2, 
        alpha_value = alpha_used_2)
      
      w_ss <- get_w_ss(alpha_used, y_ss, n_ss)
      r_ss <- get_r_ss(alpha_used, y_ss, L_0)
      w_ss_new <- get_w_ss(alpha_used_2, y_ss_new, n_ss_new)
      r_ss_new <- get_r_ss(alpha_used_2, y_ss_new, L_2)
      
      data.frame(
        "Variable"=c("Konsum", "Bevölkerung", "Produktion", "Lohn", "Landrente"),
        "Gleichgewichtswert_alt"=c(c_ss, n_ss, y_ss, w_ss, r_ss),
        "Gleichgewichtswert_neu"=c(c_ss_new, n_ss_new, y_ss_new, w_ss_new, r_ss_new)
        ) %>%
        dplyr::mutate(Differenz=Gleichgewichtswert_neu-Gleichgewichtswert_alt)
    } else{
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
        uniroot(g_c_1, interval = c(0, 10), extendInt = "yes", 
                birthrate=birth_rate)$root
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
    } 
  })
  
  output$equil_values <- renderTable({
    equil_values()
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
