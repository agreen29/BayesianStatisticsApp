library(markdown)
library(shiny)
library(tidyverse)
library(ggplot2)
library(car)
library(DT)
library(shinythemes)

navbarPage(theme = shinytheme("cosmo"), "Bayesian Methods: Conjugate Families!",
           #First tab iniated of the website
           tabPanel("Looking at Posterior Distributions",
                    sidebarLayout(
                      sidebarPanel(
                        fluidRow(
                          column(10,
                                 selectInput("likelihood",
                                             "Choose your distribution of your data:",
                                             c("Binomial", "Normal", "Poisson", "Exponential")))
                        ),

                        #If data is Binomial
                        conditionalPanel(
                          condition = "input.likelihood == 'Binomial'",
                          numericInput("like_p_bin1",
                                       "Enter the sample size for your data:", 0, min = 1
                          ),
                          numericInput("like_p_bin2",
                                       "Enter the amount of sucesses in your data:", 1, min = 0
                          ),
                          selectInput("prior",
                                      "Choose your prior distribution",
                                      c("Beta")),
                          conditionalPanel(
                            condition = "input.prior == 'Beta'",
                            numericInput("prior_p_b1",
                                         "Enter your prior value of alpha", 2, min = 0
                            ),
                            numericInput("prior_p_b2",
                                         "Enter your prior value of beta", 2, min = 0))
                        ),

                        #if data is normal
                        conditionalPanel(
                          condition = "input.likelihood == 'Normal'",
                          numericInput("like_p_n1",
                                       "Enter the sample size of your data:", 1, min = 1
                          ),
                          numericInput("like_p_n2",
                                       "Enter the mean of your data:", 0),
                          numericInput("like_sigma",
                                       "Enter your known standard deviation about the population (sigma):", 1, min=1),
                          selectInput("prior_n",
                                      "Choose your prior distribution",
                                      c("Normal")),
                          conditionalPanel(
                            condition = "input.prior_n == 'Normal'",
                            numericInput("prior_p_n1",
                                         "Enter your prior value of mu", 0
                            ),
                            numericInput("prior_p_n2",
                                         "Enter your prior value of tau", 1, min = 1))
                        ),

                        #if data follows a poisson distribution
                        conditionalPanel(
                          condition = "input.likelihood == 'Poisson'",
                          numericInput("like_p_p1",
                                       "Enter the sample size of your data:", 1, min = 1
                          ),
                          numericInput("like_p_p2",
                                       "Enter the number of sucesses in your data:", 1, min = 0),
                          selectInput("prior_p",
                                      "Your prior distribution",
                                      "Gamma"),
                          conditionalPanel(
                            condition = "input.prior_p =='Gamma'",
                            numericInput("prior_p_g1",
                                         "Enter your prior value of alpha", 1, min = 1
                            ),
                            numericInput("prior_p_g2",
                                         "Enter your prior value of lambda", 1, min = 1))
                        ),


                        #if data follows an exponential distribution
                        conditionalPanel(
                          condition = "input.likelihood == 'Exponential'",
                          numericInput("like_p_e1",
                                       "Enter the sample size of your data:", 1, min = 1),
                          numericInput("like_p_e2",
                                       "Enter the mean of your data:", 0, min = 0),
                          selectInput("prior_e",
                                      "Your prior distribution",
                                      "Gamma"),
                          conditionalPanel(
                            condition = "input.prior_e =='Gamma'",
                            numericInput("prior_p_g3",
                                         "Enter your prior value of alpha", 1, min = 0
                            ),
                            numericInput("prior_p_g4",
                                         "Enter your prior value of lambda", 1, min = 1))
                        )
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Bayes Table",
                                   tableOutput(outputId = "BayesTable"))),
                        tabsetPanel(
                          tabPanel("Plot",
                                   plotOutput(outputId = "Graphs"))
                        )
                      )

                    )

           )
)



