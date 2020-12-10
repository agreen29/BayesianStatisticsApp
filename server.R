library(shiny)
library(tidyverse)
library(ggplot2)
library(car)
library(DT)
library(broom)
library(shinythemes)

source("functions.R")

function(input, output) {

  output$BayesTable <- renderTable({

    if (input$likelihood == "Binomial") {
      beta_binom(input$prior_p_b1, input$prior_p_b2, input$like_p_bin1, input$like_p_bin2)[1]
    }

    else if (input$likelihood == "Normal") {
      normal(input$prior_p_n2, input$prior_p_n1, input$like_p_n1, input$like_p_n2, input$like_sigma)[1]
    }

    else if (input$likelihood == "Poisson") {
      poisson_gamma(input$prior_p_g1, input$prior_p_g2, input$like_p_p2, input$like_p_p1)[1]
    }
    else if (input$likelihood == "Exponential"){
      expon_gamma(input$prior_p_g3, input$prior_p_g4, input$like_p_e1, input$like_p_e2)[1]

    }

  })


  output$Graphs <- renderPlot({
    if (input$likelihood == "Binomial") {

      output <- beta_binom(input$prior_p_b1, input$prior_p_b2, input$like_p_bin1, input$like_p_bin2)
      prior <- output[2]
      scale_l <- output[3]
      post <- output[4]
      theta <- output[6]
      ymax <- output[5]

      plot(unlist(theta), unlist(prior), type='l', col='orange', xlim=c(0, 1), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xlab = "theta")
      par(new=T)
      plot(unlist(theta), unlist(scale_l), type='l', col='skyblue', xlim=c(0, 1), ylim=c(0, unlist(ymax)), ylab='',  yaxt='n', xlab = "theta")
      par(new=T)
      plot(unlist(theta), unlist(post), type='l', col='seagreen', xlim=c(0, 1), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xlab = "theta")
      legend("topleft", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("orange", "skyblue", "seagreen"))
    }
    else if (input$likelihood == "Normal") {
      output <- normal(input$prior_p_n2, input$prior_p_n1, input$like_p_n1, input$like_p_n2, input$like_sigma)
      prior <- output[2]
      scale_l <- output[3]
      post <- output[4]
      theta <- output[6]
      ymax <- output[5]

      plot(unlist(theta), unlist(prior), type='l', col='orange', xlim=range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xlab = "theta")
      par(new=T)
      plot(unlist(theta), unlist(scale_l), type='l', col='skyblue', xlim=range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='',  yaxt='n', xlab = "theta")
      par(new=T)
      plot(unlist(theta), unlist(post), type='l', col='seagreen', xlim=range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xlab = "theta")
      legend("topleft", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("orange", "skyblue", "seagreen"))
    }
    else if (input$likelihood == "Poisson"){
      output <- poisson_gamma(input$prior_p_g1, input$prior_p_g2, input$like_p_p2, input$like_p_p1)
      prior <- output[2]
      scale_l <- output[3]
      post <- output[4]
      theta <- output[6]
      ymax <- output[5]

      plot(unlist(theta), unlist(prior), type='l', xlim=range(unlist(theta))+c(-0.01, 0.01), ylim=c(0, unlist(ymax)), col="orange", xlab='theta', ylab='')
      par(new=T)
      plot(unlist(theta), unlist(scale_l), type='l', xlim=range(unlist(theta))+c(-0.01, 0.01), ylim=c(0, unlist(ymax)), col="skyblue", xlab='theta', ylab='')
      par(new=T)
      plot(unlist(theta), unlist(post), type='l', col='seagreen', xlim=range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xaxt='n', xlab = "theta")
      legend("topleft", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("orange", "skyblue", "seagreen"))
    }
    else if (input$likelihood == "Exponential"){
      output <- expon_gamma(input$prior_p_g3, input$prior_p_g4, input$like_p_e1, input$like_p_e2)
      prior <- output[2]
      scale_l <- output[3]
      post <- output[4]
      theta <- output[6]
      ymax <- output[5]
      post_lambda <- output[7]
      timescale <- c(1, 60)

      plot(unlist(theta), unlist(prior), type='l', col='orange', xlim= range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xaxt='s', xlab = "theta")
      par(new=T)
      plot(unlist(theta), unlist(scale_l), type='l', col='skyblue', xlim= range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xaxt='s', xlab = "theta")
      par(new=T)

      plot(unlist(theta), unlist(post), type='l', col='seagreen', xlim=range(unlist(theta)), ylim=c(0, unlist(ymax)), ylab='', yaxt='n', xaxt='s', xlab = "theta")

      legend("topright", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("orange", "skyblue", "seagreen"))

    }
  })
}

