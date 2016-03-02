shinyServer(function(input, output) {


output$beta <- renderPlotly({
  x <- seq(0, 1, length = 1000)
  density <- dbeta(x, shape1 = input$sh1, shape2 = input$sh2)

  p <- plot_ly(x = x, y = density, name = "Beta Distribution")

  if (input$cib == T){
    p <- add_trace(x = c(qbeta((1 - input$cinb)/2, shape1 = input$sh1,
                               shape2 = input$sh2),
                         qbeta((1 - input$cinb)/2, shape1 = input$sh1,
                               shape2 = input$sh2)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ", (1 - input$cinb)/2)) %>%
    add_trace(x = c(qbeta(0.5 + (input$cinb)/2, shape1 = input$sh1,
                          shape2 = input$sh2),
                    qbeta(0.5 + (input$cinb)/2, shape1 = input$sh1,
                          shape2 = input$sh2)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ",
                               0.5 + (input$cinb)/2)) %>%
    layout(annotations = list(list(x = qbeta(0.5 + (input$cinb)/2,
                                            shape1 = input$sh1,
                                            shape2 = input$sh2
                                            ),
                                  y = max(density)*0.75,
                                  text = round(qbeta(0.5 + (input$cinb)/2,
                                              shape1 = input$sh1,
                                              shape2 = input$sh2), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0),
                              list(x = qbeta((1 - input$cinb)/2,
                                            shape1 = input$sh1,
                                            shape2 = input$sh2),
                                  y = max(density)*0.75,
                                  text = round(qbeta((1 - input$cinb)/2,
                                              shape1 = input$sh1,
                                              shape2 = input$sh2), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$binom <- renderPlotly({
  Successes <- seq(0, input$nb)
  Probability <- dbinom(Successes, size = input$nb, prob = input$thb)

  p <- plot_ly(x = Successes, y = Probability,
               name = "Binomial Distribution",
               mode = "markers")

  if (input$cibi == T){
    p <- add_trace(x = c(qbinom((1 - input$cinbi)/2, size = input$nb,
                                prob = input$thb),
                         qbinom((1 - input$cinbi)/2, size = input$nb,
                                prob = input$thb)),
                   y= c(0, max(Probability)), mode = "lines",
                   name = paste("CI lower bound: ",
                                (1 - input$cinbi)/2)) %>%
    add_trace(x = c(qbinom(0.5 + (input$cinbi)/2, size = input$nb,
                           prob = input$thb),
                    qbinom(0.5 + (input$cinbi)/2, size = input$nb,
                           prob = input$thb)),
                  y= c(0, max(Probability)), mode = "lines",
                  name = paste("CI upper bound: ", 0.5 + (input$cinbi)/2)) %>%
    layout(annotations = list(list(x = qbinom(0.5 + (input$cinbi)/2,
                                            size = input$nb,
                                            prob = input$thb
                                            ),
                                  y = max(Probability)*0.75,
                                  text = round(qbinom(0.5 +
                                                      (input$cinbi)/2,
                                                      size = input$nb,
                                                      prob = input$thb),
                                               2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0),
                              list(x = qbinom((1 - input$cinbi)/2,
                                              size = input$nb,
                                              prob = input$thb),
                                  y = max(Probability)*0.75,
                                  text = round(qbinom((1 - input$cinbi)/2,
                                                      size = input$nb,
                                                      prob = input$thb),
                                               2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$cauchy <- renderPlotly({
  x <- seq(input$locc - 5,
           input$locc + 5,
           length = 1000)
  density <- dcauchy(x, location = input$locc, scale = input$scc)

  p <- plot_ly(x = x, y = density, name = "Cauchy Distribution")

  if (input$cic == T){
    p <- add_trace(x = c(qcauchy((1 - input$cinc)/2, location = input$locc,
                                 scale = input$scc),
                         qcauchy((1 - input$cinc)/2, location = input$locc,
                                 scale = input$scc)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ", (1 - input$cinc)/2)) %>%
    add_trace(x = c(qcauchy(0.5 + (input$cinc)/2, location = input$locc,
                            scale = input$scc),
                    qcauchy(0.5 + (input$cinc)/2, location = input$locc,
                            scale = input$scc)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ",
                               0.5 + (input$cinc)/2)) %>%
    layout(annotations = list(list(x = qcauchy(0.5 + (input$cinc)/2,
                                               location = input$locc,
                                               scale = input$scc
                                               ),
                                  y = max(density)*0.75,
                                  text = round(qcauchy(0.5 + (input$cinc)/2,
                                                       location = input$locc,
                                                       scale = input$scc), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0),
                              list(x = qcauchy((1 - input$cinc)/2,
                                               location = input$locc,
                                               scale = input$scc),
                                  y = max(density)*0.75,
                                  text = round(qcauchy((1 - input$cinc)/2,
                                                       location = input$locc,
                                                       scale = input$scc), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12),
              yaxis = list(rangemode = "tozero"))

})


output$chisq <- renderPlotly({
  x <- seq(0, qchisq(0.999, df = input$df), length = 1000)
  density <- dchisq(x, df = input$df)

  p <- plot_ly(x = x, y = density, name = "Chi-Squared Distribution")

  if (input$cich == T){
    p <- add_trace(x = c(qchisq((1 - input$cinch)/2, df = input$df),
                         qchisq((1 - input$cinch)/2, df = input$df)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ", (1 - input$cinch)/2)) %>%
    add_trace(x = c(qchisq(0.5 + (input$cinch)/2, df = input$df),
                    qchisq(0.5 + (input$cinch)/2, df = input$df)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ",
                               0.5 + (input$cinch)/2)) %>%
    layout(annotations = list(list(x = qchisq(0.5 + (input$cinch)/2,
                                             df = input$df),
                                  y = max(density)*0.75,
                                  text = round(qchisq(0.5 + (input$cinch)/2,
                                                     df = input$df), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0),
                              list(x = qchisq((1 - input$cinch)/2,
                                             df = input$df),
                                  y = max(density)*0.75,
                                  text = round(qchisq((1 - input$cinch)/2,
                                                     df = input$df), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$gamma <- renderPlotly({
  x <- seq(0, qgamma(0.999, shape = input$shg,
                     scale = input$scg),
                     length = 1000)
  density <- dgamma(x, shape = input$shg, scale = input$scg)

  p <- plot_ly(x = x, y = density, name = "Gamma Distribution")

  if (input$cig == T){
    p <- add_trace(x = c(qgamma((1 - input$cing)/2, shape = input$shg,
                                scale = input$scg),
                         qgamma((1 - input$cing)/2, shape = input$shg,
                                scale = input$scg)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ", (1 - input$cing)/2)) %>%
    add_trace(x = c(qgamma(0.5 + (input$cing)/2, shape = input$shg,
                           scale = input$scg),
                    qgamma(0.5 + (input$cing)/2, shape = input$shg,
                           scale = input$scg)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ",
                               0.5 + (input$cing)/2)) %>%
    layout(annotations = list(list(x = qgamma(0.5 + (input$cing)/2,
                                              shape = input$shg,
                                              scale = input$scg
                                              ),
                                  y = max(density)*0.75,
                                  text = round(qgamma(0.5 + (input$cing)/2,
                                              shape = input$shg,
                                              scale = input$scg), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0),
                              list(x = qgamma((1 - input$cing)/2,
                                            shape = input$shg,
                                            scale = input$scg),
                                  y = max(density)*0.75,
                                  text = round(qgamma((1 - input$cing)/2,
                                              shape = input$shg,
                                              scale = input$scg), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$uninorm <- renderPlotly({
  x <- seq(input$mean - 5, input$mean + 5, length = 1000)
  density <- dnorm(x, mean = input$mean, sd = input$sd)

  p <- plot_ly(x = x, y = density, name = "Normal Distribution")

  if (input$ci == T){
    p <- add_trace(x = c(qnorm((1 - input$cin)/2, mean = input$mean,
                               sd = input$sd),
                         qnorm((1 - input$cin)/2, mean = input$mean,
                               sd = input$sd)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ", (1 - input$cin)/2)) %>%
    add_trace(x = c(qnorm(0.5 + (input$cin)/2, mean = input$mean,
                          sd = input$sd),
                    qnorm(0.5 + (input$cin)/2, mean = input$mean,
                          sd = input$sd)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ", 0.5 + (input$cin)/2)) %>%
    layout(annotations = list(list(x = qnorm(0.5 + (input$cin)/2,
                                             mean = input$mean,
                                             sd = input$sd),
                                  y = max(density)*0.75,
                                  text = round(qnorm(0.5 + (input$cin)/2,
                                                     mean = input$mean,
                                                     sd = input$sd), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0),
                              list(x = qnorm((1 - input$cin)/2,
                                             mean = input$mean,
                                             sd = input$sd),
                                  y = max(density)*0.75,
                                  text = round(qnorm((1 - input$cin)/2,
                                                     mean = input$mean,
                                                     sd = input$sd), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$uninormb <- renderPlotly({

  dbmnorm <- function(x, mu, sigma, d, p){
    p*dnorm(x, mu, sigma)+(1-p)*dnorm(x, (mu+(d*sigma)), sigma)
  }
  m <- -(0.5 * input$delta)

  x <- seq(m-3,
           m+input$delta+3,
           length.out = 1000)

  density <- dbmnorm(x,m,1,input$delta,input$mix)

  p <- plot_ly(x = x, y = density, name = "Normal Mixture")

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$uniform <- renderPlotly({
  x <- seq(-10, 10, length = 1000)
  density <- dunif(x, min = input$min, max = input$min + input$wid)

  p <- plot_ly(x = x, y = density, name = "Uninform Distribution")

  if (input$ciu == T){
    p <- add_trace(x = c(qunif((1 - input$cinu)/2, min = input$min,
                               max = input$min + input$wid),
                         qunif((1 - input$cinu)/2, min = input$min,
                               max = input$min + input$wid)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ", (1 - input$cinu)/2)) %>%
    add_trace(x = c(qunif(0.5 + (input$cinu)/2, min = input$min,
                          max = input$min + input$wid),
                    qunif(0.5 + (input$cinu)/2, min = input$min,
                          max = input$min + input$wid)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ",
                               0.5 + (input$cinu)/2)) %>%
    layout(annotations = list(list(x = qunif(0.5 + (input$cinu)/2,
                                             min = input$min,
                                             max = input$min + input$wid
                                            ),
                                  y = max(density)*0.75,
                                  text = round(qunif(0.5 + (input$cinu)/2,
                                                     min = input$min,
                                                     max = input$min +
                                                     input$wid), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0),
                              list(x = qunif((1 - input$cinu)/2,
                                             min = input$min,
                                             max = input$min + input$wid),
                                  y = max(density)*0.75,
                                  text = round(qunif((1 - input$cinu)/2,
                                                     min = input$min,
                                                     max = input$min +
                                                     input$wid), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12))

})


output$stut <- renderPlotly({
  x <- seq(input$ncp - 20, input$ncp + 20, length = 4000)
  density <- dt(x, df = input$df, ncp = input$ncp)

  p <- plot_ly(x = x, y = density, name = "T Distribution")

  if (input$cit == T){
    p <- add_trace(x = c(qt((1 - input$cint)/2, df = input$df,
                            ncp = input$ncp),
                         qt((1 - input$cint)/2, df = input$df,
                            ncp = input$ncp)),
                   y= c(0, max(density)), mode = "lines",
                   name = paste("CI lower bound: ",
                                (1 - input$cint)/2)) %>%
    add_trace(x = c(qt(0.5 + (input$cint)/2, df = input$df,
                       ncp = input$ncp),
                    qt(0.5 + (input$cint)/2, df = input$df,
                       ncp = input$ncp)),
                  y= c(0, max(density)), mode = "lines",
                  name = paste("CI upper bound: ",
                               0.5 + (input$cint)/2)) %>%
    layout(annotations = list(list(x = qt(0.5 + (input$cint)/2,
                                          df = input$df,
                                          ncp = input$ncp),
                                  y = max(density)*0.75,
                                  text = round(qt(0.5 + (input$cint)/2,
                                                  df = input$df,
                                                  ncp = input$ncp), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = 20,
                                  ay = 0),
                              list(x = qt((1 - input$cint)/2,
                                          df = input$df,
                                          ncp = input$ncp),
                                  y = max(density)*0.75,
                                  text = round(qt((1 - input$cint)/2,
                                                  df = input$df,
                                                  ncp = input$ncp), 2),
                                  showarrow = T,
                                  arrowhead = 7,
                                  ax = -20,
                                  ay = 0)))
  }

  l <- layout(p = p, font = list(family = "sans-serif", size = 12),
              xaxis = list(range = c(input$ncp - 5, input$ncp + 5)))

})



output$bivbeta <- renderPlotly({
    if (input$bprec == "High"){
      p <- 200
    }
    else {
      p <- 100
    }
    x <- seq(0, 1, length = p)
    y <- x

    a = input$bsh2
    b = input$bsh1
    c = input$bsh3

    f <- function(x,y){
      ((x^(a - 1)) * (y^(b - 1)) * ((1 - x)^(b+c-1)) * ((1 - y)^(a+c-1))) /
        (((gamma(a) * gamma(b) * gamma(c)) / gamma(a + b + c)) *
           ((1 - x * y)^(a + b + c)))
    }

    density = outer(x, y, f)
    density[is.na(density)] = 0

    p <- plot_ly(x = x, y = y, z = density,
                 type = "surface", opacity = 1,
                 hoverinfo = "z",
                 colorscale = "Portland",
                 showscale = F,
                 lighting = list(ambient = 0.9,
                                 diffuse = 0.7,
                                 specular = 0.07,
                                 roughness = 0.3,
                                 fresnel = 0.2))
     l <- layout(p = p, font = list(family = "sans-serif",
                    size = 12))

  })


output$bivgamma <- renderPlotly({
    if (input$gprec == "High"){
      p <- 100
    }
    else {
      p <- 45
    }
    f <- function(x,y){
      (1/(gamma(a1)*gamma(a2))) *
        b^(a1+a2)
        x^(a1-1) *
        y^(a2-1) *
        exp(-(x*b)-(y*b))
    }

    a1 = input$ga1
    a2 = input$ga2
    b = 1/input$gb

    x <- seq(0, qgamma(0.999, shape = max(a1,a2),
                       scale = 1/b), length = p)
    y <- x

    density = outer(x, y, f)
    density[is.na(density)] = 1

    p <- plot_ly(x = x, y = y, z = density,
                 type = "surface", opacity = 1,
                 hoverinfo = "z",
                 colorscale = "Portland",
                 showscale = F,
                 lighting = list(ambient = 0.9,
                                 diffuse = 0.7,
                                 specular = 0.07,
                                 roughness = 0.3,
                                 fresnel = 0.2))
     l <- layout(p = p, font = list(family = "sans-serif",
                    size = 12))

  })


output$bivnorm <- renderPlotly({
    if (input$prec == "High"){
      p <- 100
    }
    else {
      p <- 45
    }
    x <- seq(-3.5, 3.5, length = p)
    y <- x

    s <- input$s
    c <- input$corr
    f <- function(x,y){
      1/(2*pi*s^2*sqrt(1-c^2)) *
      exp(
        -(
          ((x - mean(x))^2-
          2*c*(x - mean(x))*(y - mean(y))+
          (y - mean(y))^2)/s^2)
          /(2*(1-c^2)))
    }
    density = outer(x, y, f)
    density[is.na(density)] = 1

    p <- plot_ly(x = x, y = y, z = density,
                 type = "surface", opacity = 1,
                 hoverinfo = "z",
                 colorscale = "Portland",
                 showscale = F,
                 lighting = list(ambient = 0.9,
                                 diffuse = 0.7,
                                 specular = 0.07,
                                 roughness = 0.3,
                                 fresnel = 0.2))
     l <- layout(p = p, font = list(family = "sans-serif",
                    size = 12))

  })



output$bivunif <- renderPlotly({
    if (input$uprec == "High"){
      p <- 100
    }
    else {
      p <- 45
    }
    x <- seq(3.5, -3.5, length = p)
    y <- x

    xl <- input$bux[1]
    xh <- input$bux[2]
    yl <- input$buy[1]
    yh <- input$buy[2]

    f <- function(x,y){
      dunif(y,min = xl,max = xh)*dunif(x, min = yl, max = yh)
    }
    density = outer(x, y, f)
    density[is.na(density)] = 1

    p <- plot_ly(x = x, y = y, z = density,
                 type = "surface", opacity = 1,
                 hoverinfo = "z",
                 colorscale = "Portland",
                 showscale = F,
                 lighting = list(ambient = 0.9,
                                 diffuse = 0.7,
                                 specular = 0.07,
                                 roughness = 0.3,
                                 fresnel = 0.2))
     l <- layout(p = p, font = list(family = "sans-serif",
                 size = 12), camera = list(eye = list(x = -1.25,
                                                      y = -1.25,
                                                      z = -1.25)))

  })

})
