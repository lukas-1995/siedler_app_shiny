library(tictoc)
library(scales)
library(grid)
library(ggplot2)
library(mise)
library(shiny)
library(shinyjs)
library(rdrop2)
library(stringi)
library(shinyWidgets)
library(shinydashboard)
# library(odbc)
# library(RPostgreSQL)


Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
mise()


dashboardPage(skin="yellow",
              dashboardHeader(title = "Siedler-Würfelverteilung",
                              titleWidth = '100%',
                              dropdownMenuOutput("messageMenu")),
              dashboardSidebar(disable=T),
              dashboardBody(setBackgroundImage(
                src = "catan.jpg", shinydashboard = T
              ),
              fluidRow(column(3,
                              box(title = 'Einstellungen', width = '100%', #background="black",
                                  solidHeader = T, status = 'warning',
                                  useShinyjs(),
                                  textOutput("spielerconf"),
                                  selectInput("version", "Siedler-Version:",
                                              choices = c("Standardspiel (Version 1)"="1",
                                                          "Städte und Ritter (Version 2)"="2",
                                                          "Seefahrer (Version 3)"="3",
                                                          "Städte und Ritter & Seefahrer (Version 4)"="4")),
                                  uiOutput('playerpickchoice'),
                                  tags$div(style="margin-bottom:10px;",
                                           actionButton("newplayer", "Spieler bearbeiten")),
                                  materialSwitch(inputId = "onl", label = "Online", status = "warning"),
                                  tags$div(id="versionconf", textOutput("versionconft")),
                                  tags$div(id="onlineconf", textOutput("onlineconft")),
                                  tags$div(id="playconf", textOutput("playconft"),
                                           hr(style="border-color: grey;")),
                                  fluidRow(
                                    column(6, align="center", uiOutput("startbutton")),
                                    column(6, align="center", uiOutput("endbutton"))
                                  ),
                                  helpText("Beachte: Durch Drücken des 'Los'-Knopfes beginnt die Zeitnahme.",
                                           "Des Weiteren kann erst nach dem Drücken ein Wurf eingegeben werden."),
                                  hr(style="border-color: grey;"),
                                  fluidRow(column(4, align="center",actionButton("two", "2", width='100%')),
                                           column(4, align="center",actionButton("three", "3", width='100%')),
                                           column(4, align="center",actionButton("four", "4", width='100%'))),
                                  fluidRow(column(4, align="center",actionButton("five", "5", width='100%')),
                                           column(4, align="center",actionButton("six", "6", width='100%')),
                                           column(4, align="center",actionButton("seven", "7", width='100%'))),
                                  fluidRow(column(4, align="center",actionButton("eight", "8", width='100%')),
                                           column(4, align="center",actionButton("nine", "9", width='100%')),
                                           column(4, align="center",actionButton("ten", "10", width='100%'))),
                                  fluidRow(column(4, align="center",actionButton("eleven", "11", width='100%')),
                                           column(4, align="center",actionButton("twelve", "12", width='100%')),
                                           column(4, align="center", actionButton("delete", "Löschen", width='100%'))),
                                  tags$div(id="txt", hr(style="border-color: grey;"),
                                           uiOutput('winnerpickchoice'),
                                           uiOutput("confirm")),
                                  tags$div(id="neus", hr(style="border-color: grey;"),
                                           actionButton("new", "Neues Spiel starten", width='100%'))),
                              tags$head(tags$style("#spielerconf{color: black;
                                    font-size: 25px;
                                    font-style: bold}",
                                                   "#versionconf{color: black;
                                    font-size: 25px;
                                    font-style: bold}"
                                                   
                              )
                              )),
                       column(9,
                              tabBox(width='100%',
                                     tabPanel("Aktuelle Grafik",  
                                              fluidRow(column(12, align="center", 
                                                              h3("Histogram", align="center"),
                                                              plotOutput("siedler"),
                                                              helpText("Die dunklen Balken stellen das Histogram der tatsächlichen Würfe dar, während die hellblauen
                                          die theoretische Verteilung zeigen."),
                                                              h3("Statistiken", align="center"), #   tableOutput("view")),
                                                              tableOutput("summary")))
                                              
                                     ),
                                     tabPanel("Historische Grafik",
                                              dropdown(
                                                tags$h3("Filter"),
                                                selectInput(inputId = 'time', label = 'Anzahl Spiele', 
                                                            choices = c("Zeige Alles"="1",
                                                                        "Letzte 5 Spiele"="5",
                                                                        "Letzte 10 Spiele"="10",
                                                                        "Letzte 20 Spiele"="20",
                                                                        "Letzte 50 Spiele"="50")),
                                                selectInput("versionhist", "Siedler-Version:",
                                                            choices = c("Zeige alle Versionen"="10",
                                                                        "Standardspiel (Version 1)"="1",
                                                                        "Städte und Ritter (Version 2)"="2",
                                                                        "Seefahrer (Version 3)"="3",
                                                                        "Städte und Ritter & Seefahrer (Version 4)"="4")),
                                                uiOutput('playerpickhistout'),
                                                style = "gradient", circle = TRUE, status = "warning", icon = icon("cog"), width = "300px",
                                                tooltip = tooltipOptions(title = "Klicke um die Grafik zu verändern!")
                                              ),
                                              fluidRow(column(12, align="center",
                                                              h3("Histogram"),
                                                              plotOutput("stathist"),
                                                              helpText("Die dunklen Balken stellen das Histogram der tatsächlichen Würfe dar, während die hellblauen
                                          die theoretische Verteilung zeigen."),
                                                              fluidRow(column(1, align="center"),
                                                                       column(7, align="center",h3("Statistiken", align="center"),
                                                                              tableOutput("summaryhist")),
                                                                       column(3, align="center",
                                                                              h3("Ewige Tabelle", align="center"),
                                                                              tableOutput("summaryhistwinner")))))
                                              
                                     ),
                                     tabPanel("Historische Daten",
                                              dropdown(
                                                tags$h3("Filter"),
                                                selectInput(inputId = 'timet', label = 'Anzahl Spiele', 
                                                            choices = c("Zeige Alles"="1",
                                                                        "Letzte 5 Spiele"="5",
                                                                        "Letzte 10 Spiele"="10",
                                                                        "Letzte 20 Spiele"="20",
                                                                        "Letzte 50 Spiele"="50")),
                                                selectInput("versionhistt", "Siedler-Version:",
                                                            choices = c("Zeige alle Versionen"="10",
                                                                        "Standardspiel (Version 1)"="1",
                                                                        "Städte und Ritter (Version 2)"="2",
                                                                        "Seefahrer (Version 3)"="3",
                                                                        "Städte und Ritter & Seefahrer (Version 4)"="4")),
                                                uiOutput('playerpickhisttout'),
                                                style = "gradient", circle = TRUE, status = "warning", icon = icon("cog"), width = "300px",
                                                tooltip = tooltipOptions(title = "Klicke um die Tabelle zu verändern!")
                                              ),
                                              fluidRow(column(12, align="center",
                                                              h3("Daten"),
                                                              tableOutput("statdata"))))
                                     
                              )
                       ))
              ))