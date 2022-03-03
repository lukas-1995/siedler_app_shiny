##################################################################################################################################
###################################### SERVER-FILE ###############################################################################
##################################################################################################################################

# What the indicator stands for:
# 
# 0 <- nothing has been decided yet
# 1 <- a user was chosen, all initialisations can be done
# 2 <- the game has started 
# 3 <- the game has ended
# 4 <- the winner was confirmed, everything is over



##################################
######## Load libraries ##########
##################################

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
library(shinyalert)
library(odbc)

con <- dbConnect(odbc(),
                 Driver   = "SQL server",
                 Server   = "v2202104145393150207.luckysrv.de",
                 #Database = "siedler_app",
                 UID      = "SA",
                 PWD      = "nzdpXjmJaeh^X*5k3u^G2quZ6H@N3P",
                 Port     = 1433)



#############################################
######## Initialise server funtion ##########
############################################# 

function(input,output, session){
  
  
  # session$onSessionEnded(function(){
  #   dbDisconnect(con)
  # })
  
########################################################
######## Load data for theoretic distribution ##########
########################################################
  

  dist <- data.frame(c(2,3,4,5,6,7,8,9,10,11,12), c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36))
  names(dist) <- c("num", "prob")
  
  dhms <- function(t){
    paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
          ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
          ,formatC(t %% 60, width = 2, format = "d", flag = "0")
          ,sep = ":"
    )
  }
  
##############################################
######## Initialise reactive values ##########
##############################################
  
  waits <- reactiveValues() # reactive to store all reactive variables
  waits$resetindicator<-0   # used to change button labels
  waits$temp <- 0
  waits$inp <- 0
  waits$m <- 0
  waits$n <- 1
  waits$t1 <- 0
  waits$t2 <- 0
  waits$t3 <- 0
  waits$t4 <- 0
  waits$t5 <- 0
  waits$text <- 0
  waits$finish <- 0
  waits$players <- 0
  waits$data <- 0
  waits$online <- 0
  waits$statistik_new <- 0
  waits$user <- 0
  waits$spieler <- 0
  waits$pre_user <- 0
  waits$newpl <- 0
  waits$version <- 0
  waits$player_names <- 0
  waits$user_datafr <- 0
  use <- reactive
  lang <- reactiveValues()
  lang$t1 <- 0
  lang$t2 <- 0
  lang$t3 <- 0
  lang$t4 <- 0
  lang$t5 <- 0
  lang$histtable <- 0
  df <- reactiveValues()
  df$temp <- 0
  df$inp <- 0
  

#############################
######## Load data ##########
#############################
  
  user <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[user]")
  names(user) <- "Users"
  
  
  query_modal <- modalDialog(
    title = "Login",
    selectInput('user_choose','Konto wählen:', user$Users),
    textInput("new_user", "Neuen Benutzer anlegen und einloggen:"),
    easyClose = F,
    footer = tagList(
      actionButton("run", "Einloggen")
    )
  )
  
  showModal(query_modal)
  
  observeEvent(input$run,{
    removeModal()
    waits$user <- ifelse(input$new_user!="", input$new_user, input$user_choose)
    if (waits$user == input$new_user) {
      user <- data.frame(c(user$Users, waits$user))
      names(user) <- "Users"
      dbWriteTable(con, "[siedler_app].[dbo].[user]", user, overwrite=T, row.names=F)
    }
    player_names <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Name]")
    names(player_names) <- c("Name", "Nutzer")
    player_names <- data.frame("Name" = player_names[player_names$Nutzer == waits$user,1])
    
    # Pick all players involved in the game
    output$playerpickchoice<-renderUI({
      pickerInput(inputId = "playerpick", 
                  label = "Spieler", 
                  choices = player_names$Name, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 5",
                    title = "Bitte wähle alle Mitspieler aus",
                    deselectAllText = "Alle abwählen",
                    selectAllText = "Alle auswählen"
                  ), 
                  multiple = TRUE
      )
    })
    
    # Pick all players you want to filter in the historical diagramm
    output$playerpickhistout <- renderUI({
      pickerInput(inputId = "playerpickhist", 
                  label = "Spieler", 
                  choices = player_names$Name, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 5",
                    title = "Zeige alle Spieler",
                    deselectAllText = "Alle abwählen",
                    selectAllText = "Alle auswählen"
                  ), 
                  multiple = TRUE)
    })
    
    # Pick all players you want to filter in the historical table
    output$playerpickhisttout <- renderUI({
      pickerInput(inputId = "playerpickhistt", 
                  label = "Spieler", 
                  choices = player_names$Name, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 5",
                    title = "Zeige alle Spieler",
                    deselectAllText = "Alle abwählen",
                    selectAllText = "Alle auswählen"
                  ), 
                  multiple = TRUE)
    })
    
    # Determine what is shown in the delete player option
    output$delete_player <- renderUI({
      pickerInput(inputId = "del_player", 
                  label = "Spieler löschen", 
                  choices = player_names$Name, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 5",
                    title = "Zeige alle Mitspieler"
                  ), 
                  multiple = F)
    })
    
    gsiedler <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Siedler]")
    gsiedler_user <- gsiedler[,length(gsiedler)]
    gsiedler <- data.frame(gsiedler[gsiedler$Nutzer == waits$user,1:length(gsiedler)-1])
    names(gsiedler) <- c("Datum", "Spieleranzahl", "Spieldauer", "Sieger", "Version", "Online", "2", "3", "4", "5", "6", "7",
                         "8", "9", "10", "11", "12", "Spieler 1", "Spieler 2", "Spieler 3", "Spieler 4", "Spieler 5", "Spieler 6")
    gsiedler$`Spieler 1` <- stri_replace_all_regex(gsiedler$`Spieler 1`, "^\\s+|\\s+$", "")
    gsiedler$`Spieler 2` <- stri_replace_all_regex(gsiedler$`Spieler 2`, "^\\s+|\\s+$", "")
    gsiedler$`Spieler 3` <- stri_replace_all_regex(gsiedler$`Spieler 3`, "^\\s+|\\s+$", "")
    gsiedler$`Spieler 4` <- stri_replace_all_regex(gsiedler$`Spieler 4`, "^\\s+|\\s+$", "")
    gsiedler$`Spieler 5` <- stri_replace_all_regex(gsiedler$`Spieler 5`, "^\\s+|\\s+$", "")
    gsiedler$`Spieler 6` <- stri_replace_all_regex(gsiedler$`Spieler 6`, "^\\s+|\\s+$", "")
    waits$statistik_new <- gsiedler
    
    waits$resetindicator<-1
    

  })
    
    
    
    # Change User
    observeEvent(input$change,{session$reload()
                 # {
                 #   user <- dbGetQuery(con, "SELECT * FROM user")
                 # names(user) <- "Users"
                 # waits$user <- 0
                 # showModal(modalDialog(title = "Login",
                 #                       selectInput('user_choose','Konto wählen:', user$Users),
                 #                       textInput("new_user", "Neuen Benutzer anlegen und einloggen:"),
                 #                       easyClose = F,
                 #                       footer = tagList(
                 #                         actionButton("run", "Einloggen")
                 #                       )))
                 # 
                 })
    
######################################
######## When to show stuff ##########
######################################
    
    # Confirmations of details of chosen game properties
    observe({
      if (waits$resetindicator==0 | waits$resetindicator==1) {
        shinyjs::hide("spielerconf")
        shinyjs::hide("versionconf")
        shinyjs::hide("playconf")
        shinyjs::hide("onlineconf")
      } else {
        shinyjs::show("spielerconf")
        shinyjs::show("versionconf")
        shinyjs::show("playconf")
        shinyjs::show("onlineconf")
      }
    })
    
    # Buttons to choose properties
    observe({
      if (waits$resetindicator==1) {
        shinyjs::show("version")
        shinyjs::show("onl")
        shinyjs::show("playerpick")
        shinyjs::show("newplayer")
      } else {
        shinyjs::hide("version")
        shinyjs::hide("onl")
        shinyjs::hide("playerpick")
        shinyjs::hide("newplayer")
      }
    })
    
    # When to show end-button
    observe({
      if (waits$resetindicator==0 | waits$resetindicator==1) {
        shinyjs::hide("endbutton")
      } else {
        shinyjs::show("endbutton")
      }
    })
    
    # When to show button for new game
    observe({
      if (waits$resetindicator==4) {
        shinyjs::show("neus")
      } else {
        shinyjs::hide("neus")
      }
    })
    
    # When to show button to choose winner
    observe({
      if (waits$resetindicator==3 | waits$resetindicator==4) {
        shinyjs::show("txt")
      } else {
        shinyjs::hide("txt")
      }
    })
    
    # When to show button to delete input
    observe({
      if(length(df$inp)==0){
        shinyjs::hide("delete")
      } else {
        shinyjs::show("delete")
      }
      
    })
    
    
###############################################################
######## Determine what happens with reactive output ##########
###############################################################
    
    # Show user in header
    output$messageMenu <- renderMenu({
      dropdownMenu(type = "messages", badgeStatus = NULL, headerText="",
                   messageItem(from = "Benutzer", 
                               message = tags$div(paste0("Der aktuelle Nutzer ist ", waits$user, "."),
                                                  style = "display: inline-block; vertical-align: middle; color:grey")),
                   tags$li( fluidRow(column(12, align="center",actionButton("change", label = "Konto wechseln", width='100%'))),
                            class = "dropdown"),
                   messageItem(
                     from = "Support",
                     message = tags$div("Falls ein Problem auftritt, wende",
                                        tags$br(),
                                        "dich an lukasf1310@gmail.com.",
                                        style = "display: inline-block; vertical-align: middle; color:grey"),
                     icon = icon("life-ring"),
                     href = "mailto:lukasf1310@gmail.com"))
    })
    

    # Let start Button show different things at different times
    output$startbutton<-renderUI({
      if(waits$resetindicator==0 | waits$resetindicator==1){
        lbl<-"Los"
      }else{
        lbl<-"Begonnen"
      }
      actionButton("start",label=lbl, width='100%')
    })
    
    # Determine output of Confirm winner button
    output$confirm<-renderUI({
      if(waits$resetindicator==0 | waits$resetindicator==1 | waits$resetindicator==2 | waits$resetindicator==3){
        lbl<-"Bestätigen"
      }
      if(waits$resetindicator==4){
        lbl<-paste0("Herzlichen Glückwunsch ", input$sieger, "!")
      }
      actionButton("confirm",label=lbl, width='100%')
    })
    
    # Determine output of property texts, they will be shown once the game has started
    output$spielerconf <- renderText({ paste0("Anzahl der Spieler: ", waits$spieler) })
    output$versionconft <- renderText({ paste0("Siedler-Version: ", waits$version) })
    output$onlineconft <- renderText({ paste0("Online: ", waits$online) })
    output$playconft <- renderText({waits$players  })
    
    # # Pick all players involved in the game
    # output$playerpickchoice<-renderUI({
    #   pickerInput(inputId = "playerpick", 
    #               label = "Spieler", 
    #               choices = player_names$Name, 
    #               options = pickerOptions(
    #                 actionsBox = TRUE, 
    #                 size = 10,
    #                 `selected-text-format` = "count > 5",
    #                 title = "Bitte wähle alle Mitspieler aus",
    #                 deselectAllText = "Alle abwählen",
    #                 selectAllText = "Alle auswählen"
    #               ), 
    #               multiple = TRUE
    #   )
    # })
    # 
    # # Pick all players you want to filter in the historical diagramm
    # output$playerpickhistout <- renderUI({
    #   pickerInput(inputId = "playerpickhist", 
    #               label = "Spieler", 
    #               choices = player_names$Name, 
    #               options = pickerOptions(
    #                 actionsBox = TRUE, 
    #                 size = 10,
    #                 `selected-text-format` = "count > 5",
    #                 title = "Zeige alle Spieler",
    #                 deselectAllText = "Alle abwählen",
    #                 selectAllText = "Alle auswählen"
    #               ), 
    #               multiple = TRUE)
    # })
    # 
    # # Pick all players you want to filter in the historical table
    # output$playerpickhisttout <- renderUI({
    #   pickerInput(inputId = "playerpickhistt", 
    #               label = "Spieler", 
    #               choices = player_names$Name, 
    #               options = pickerOptions(
    #                 actionsBox = TRUE, 
    #                 size = 10,
    #                 `selected-text-format` = "count > 5",
    #                 title = "Zeige alle Spieler",
    #                 deselectAllText = "Alle abwählen",
    #                 selectAllText = "Alle auswählen"
    #               ), 
    #               multiple = TRUE)
    # })
    # 
    
    
    # Pick the winner among those involved in the game
    output$winnerpickchoice <- renderUI({
      pickerInput(inputId = "sieger", 
                  label = "Wer hat das Spiel gewonnen?", 
                  choices = input$playerpick, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 5",
                    title = "Zeige alle Mitspieler"
                    ), 
                  multiple = F)
    })
    
    
    # Determine what is shown on the end button
    output$endbutton<-renderUI({
      if(waits$resetindicator==0 | waits$resetindicator==1 | waits$resetindicator==2){
        lbl<-"Spiel beenden"
      }
      if(waits$resetindicator==3 | waits$resetindicator==4){
        lbl="Spiel beendet"
      }
      actionButton("end",label=lbl, width='100%')
    })
    
    # # Determine what is shown in the delete player option
    # output$delete_player <- renderUI({
    #   pickerInput(inputId = "del_player", 
    #               label = "Spieler löschen", 
    #               choices = player_names$Name, 
    #               options = pickerOptions(
    #                 actionsBox = TRUE, 
    #                 size = 10,
    #                 `selected-text-format` = "count > 5",
    #                 title = "Zeige alle Mitspieler"
    #               ), 
    #               multiple = F)
    # })
    # 
    
########################################
######## Initialise functions ##########
########################################
    
    # Function that is executed after game starts, initialises properties to show
    forward<-function(){
      
      #req(input$spieler)#shows data that is required
      req(input$version)
      
      if (input$onl==1) {
        waits$online <- "Ja"
      } else{
        waits$online <- "Nein"
      }
      
      if (input$version==1) {
        waits$version <- "Standard"
      }
      if (input$version==2) {
        waits$version <- "Städte und Ritter"
      }
      if (input$version==3) {
        waits$version <- "Seefahrer"
      }
      if (input$version==4) {
        waits$version <- "Beides"
      }
      
      waits$spieler <- length(input$playerpick)
      
      waits$resetindicator<-2 # change button label
      tic.clearlog()
      tic()
    }
    
    # Calculate the data for the current histogram
    calc <- function(){
      
      req(waits$resetindicator==2)
      df$inp <- c(df$inp, df$temp[!is.na(df$temp<13 & df$temp>1)])
      use <- as.data.frame(df$inp[df$inp<13 & df$inp>1])
      names(use)[1] <- "count"
      
      waits$m <- names(sort(table(use$count), decreasing = T))[1]
      waits$n <- max(length(use$count[use$count==waits$m]),1/6*length(use$count))
      waits$t1 <- round(mean(use$count),2)
      waits$t2 <- median(use$count)
      waits$t3 <- as.integer(waits$m)
      waits$t4 <- length(use$count)
      waits$data <- use$count
    }
    
    
    # Function that is executed once the game ends
    ende<-function(){
      waits$resetindicator<-3 
      toc(log=T, quiet = T)
      time <- tic.log(format = T)
      
      waits$t5 <- dhms(as.numeric(stri_replace_all_regex(time[[1]], " sec elapsed", "")))
      
    }
    
    # Function that is executed once the winner has been confirmed, makes the datawork
    siegerconf<-function(){
      req(input$sieger!="")
      show_alert(
        title = "Herzlichen Glückwunsch!",
        text = paste0("Herzlichen Glückwunsch ", input$sieger, " ! Das hast du klasse gemacht!"),
        type = "success"
      )
      
      waits$resetindicator<-4# change button label
      gsiedler <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Siedler]")
      gsiedler <- data.frame(gsiedler[gsiedler$Nutzer == waits$user,])
      gsiedler_user <- gsiedler[,length(gsiedler)]
      gsiedler <- data.frame(gsiedler[,1:length(gsiedler)-1])
      names(gsiedler) <- c("Datum", "Spieleranzahl", "Spieldauer", "Sieger", "Version", "Online", "2", "3", "4", "5", "6", "7",
                           "8", "9", "10", "11", "12", "Spieler 1", "Spieler 2", "Spieler 3", "Spieler 4", "Spieler 5", "Spieler 6")
      gsiedler$`Spieler 1` <- stri_replace_all_regex(gsiedler$`Spieler 1`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 2` <- stri_replace_all_regex(gsiedler$`Spieler 2`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 3` <- stri_replace_all_regex(gsiedler$`Spieler 3`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 4` <- stri_replace_all_regex(gsiedler$`Spieler 4`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 5` <- stri_replace_all_regex(gsiedler$`Spieler 5`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 6` <- stri_replace_all_regex(gsiedler$`Spieler 6`, "^\\s+|\\s+$", "")
      Datum <- c(unlist(gsiedler$Datum), format(Sys.Date(), "%d.%m.%Y"))
      Spieleranzahl <- c(gsiedler$Spieleranzahl, paste(waits$spieler))
      Spieldauer <- c(unlist(gsiedler$Spieldauer), paste(waits$t5))
      Sieger <- c(as.character(gsiedler$Sieger), paste(input$sieger))
      Version <- c(as.character(gsiedler$Version), paste(input$version))
      Online <- c(as.character(gsiedler$Online), paste(1*input$onl))
      a <- table(df$inp)
      a <- data.frame(data.matrix(a))
      a$num <- rownames(a)
      for (i in 2:12) {
        if (!is.element(i, a$num)) {
          a[nrow(a) + 1,] = c(0,paste(i))
        }
      }
      a$data.matrix.a. <- as.numeric(a$data.matrix.a.)
      X2 <- as.integer(c(gsiedler$`2`, a$data.matrix.a.[a$num==2]))
      X3 <- as.integer(c(gsiedler$`3`, a$data.matrix.a.[a$num==3]))
      X4 <- as.integer(c(gsiedler$`4`, a$data.matrix.a.[a$num==4]))
      X5 <- as.integer(c(gsiedler$`5`, a$data.matrix.a.[a$num==5]))
      X6 <- as.integer(c(gsiedler$`6`, a$data.matrix.a.[a$num==6]))
      X7 <- as.integer(c(gsiedler$`7`, a$data.matrix.a.[a$num==7]))
      X8 <- as.integer(c(gsiedler$`8`, a$data.matrix.a.[a$num==8]))
      X9 <- as.integer(c(gsiedler$`9`, a$data.matrix.a.[a$num==9]))
      X10 <- as.integer(c(gsiedler$`10`, a$data.matrix.a.[a$num==10]))
      X11 <- as.integer(c(gsiedler$`11`, a$data.matrix.a.[a$num==11]))
      X12 <- as.integer(c(gsiedler$`12`, a$data.matrix.a.[a$num==12]))
      Spieler1 <- c(as.character(gsiedler$`Spieler 1`), paste(input$playerpick[1]))
      Spieler2 <- c(as.character(gsiedler$`Spieler 2`), paste(input$playerpick[2]))
      Spieler3 <- c(as.character(gsiedler$`Spieler 3`), paste(input$playerpick[3]))
      Spieler4 <- c(as.character(gsiedler$`Spieler 4`), paste(input$playerpick[4]))
      Spieler5 <- c(as.character(gsiedler$`Spieler 5`), paste(input$playerpick[5]))
      Spieler6 <- c(as.character(gsiedler$`Spieler 6`), paste(input$playerpick[6]))
      
      tempset <- data.frame(Datum, Spieleranzahl, Spieldauer, Sieger, Version, Online, X2, X3, X4, X5, X6, X7,
                            X8, X9, X10, X11, X12, Spieler1, Spieler2, Spieler3, Spieler4, Spieler5, Spieler6)
      names(tempset) <- c("Datum", "Spieleranzahl", "Spieldauer", "Sieger", "Version", "Online", "2", "3", "4", "5", "6", "7",
                          "8", "9", "10", "11", "12", "Spieler 1", "Spieler 2", "Spieler 3", "Spieler 4", "Spieler 5", "Spieler 6")
      waits$statistik_new <- tempset
      gsiedler <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Siedler]")
      tempset1 <- rbind(gsiedler, setNames(data.frame(c(tempset[nrow(tempset),], waits$user)), names(gsiedler)))
      names(tempset1) <- c("Datum", "Spieleranzahl", "Spieldauer", "Sieger", "Version", "Online", "2", "3", "4", "5", "6", "7",
                          "8", "9", "10", "11", "12", "Spieler 1", "Spieler 2", "Spieler 3", "Spieler 4", "Spieler 5", "Spieler 6", "Nutzer")
      dbWriteTable(con, "[siedler_app].[dbo].[Siedler]", tempset1, overwrite=T, row.names=F, col.names=T)
   
    }
    
####################################
######## React to actions ##########
####################################
    
    # What happens when the start button is pressed
    observeEvent(input$start,{
      waits$players <- paste0("Mitspieler: ", input$playerpick[1])
      for (i in 2:length(input$playerpick)) {
        waits$players <- paste0(waits$players, ", ", input$playerpick[i])
      }
      
      if (length(input$playerpick)<2) {
        show_alert(
          title = "Zu wenige Spieler",
          text = "Es wurden zu wenige Spieler ausgewählt, bitte alle Spieler auswählen.",
          type = "error"
        )
      }
      if (length(input$playerpick)>6) {
        show_alert(
          title = "Zu viele Spieler",
          text = "Es wurden zu viele Spieler ausgewählt, bitte wenige Spieler auswählen.",
          type = "error"
        )
      }
      
      req(length(input$playerpick)>1)
      req(length(input$playerpick)<7)
      
      if (waits$resetindicator==1) {
        forward()
        
      }
    })
    
    # What happens when any of the number buttons is pressed
    observeEvent(input$two,{
      df$temp=2
      calc()
    })
    observeEvent(input$three,{
      df$temp=3
      calc()
    })
    observeEvent(input$four,{
      df$temp=4
      calc()
    })
    observeEvent(input$five,{
      df$temp=5
      calc()
    })
    observeEvent(input$six,{
      df$temp=6
      calc()
    })
    observeEvent(input$seven,{
      df$temp=7
      calc()
    })
    observeEvent(input$eight,{
      df$temp=8
      calc()
    })
    observeEvent(input$nine,{
      df$temp=9
      calc()
    })
    observeEvent(input$ten,{
      df$temp=10
      calc()
    })
    observeEvent(input$eleven,{
      df$temp=11
      calc()
    })
    observeEvent(input$twelve,{
      df$temp=12
      calc()
    })
    
    observeEvent(input$delete,{
      req(waits$resetindicator==2)
      df$inp <- df$inp[1:length(df$inp)-1]
      use <- as.data.frame(df$inp[df$inp<13 & df$inp>1])
      names(use)[1] <- "count"
      
      waits$m <- names(sort(table(use$count), decreasing = T))[1]
      waits$n <- max(length(use$count[use$count==waits$m]),1/6*length(use$count))
      waits$t1 <- round(mean(use$count),2)
      waits$t2 <- median(use$count)
      waits$t3 <- as.integer(waits$m)
      waits$t4 <- length(use$count)
      waits$data <- use$count
    })
    
    
    # What happens when the end button is pressed
    observeEvent(input$end,{
      req(waits$resetindicator==2)
      ende()
    })
    
    # What happens when the winner confirm button is pressed
    observeEvent(input$confirm,{
      if (waits$finish==0) {
        if (input$sieger=="") {
          show_alert(
            title = "Kein Sieger ausgwählt",
            text = "Du hast keinen Sieger ausgewählt. Bitte wähle einen Sieger aus!",
            type = "error"
          )
        } else{
          siegerconf()
          waits$finish <- 1
        }
        
      }
    })
    
    # What happens when the new player button is pressed
    observeEvent(input$newplayer,{
      showModal(modalDialog(
        title = "Spieler bearbeiten",
        textInput("new_user", "Neuen Spieler hinzufügen:"),
        actionButton("newplconf", "Bestätigen"),
        uiOutput("delete_player"),
        actionButton("delplconf", "Löschen"),
        easyClose = T,
        footer = tagList(
          actionButton("close", "Schließen")
        ))
      )
    })
    
    # What happens when the pop up window to work on players is klicked
    observeEvent(input$close,{
      removeModal()
    })
    
    # What happens when a new player is confirmed
    observeEvent(input$newplconf,{
      
      req(input$new_user!="")
      
            
      player_names <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Name]")
      player_names_users <- player_names[,2] 
      player_names <- data.frame("Name" = player_names[,1])
      
      player_names <- data.frame("Name" = c(player_names$Name, input$new_user), "Nutzer" = c(player_names_users, waits$user))
      dbWriteTable(con, "[siedler_app].[dbo].[Name]", player_names, overwrite=T, row.names=F, col.names=T)
      
      player_names <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Name]")
      names(player_names) <- c("Name", "Nutzer")
      player_names <- data.frame("Name" = player_names[player_names$Nutzer == waits$user,1])
      
      output$playerpickchoice<-renderUI({
        pickerInput(inputId = "playerpick", 
                    label = "Spieler", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Bitte wähle alle Mitspieler aus",
                      deselectAllText = "Alle abwählen",
                      selectAllText = "Alle auswählen"
                    ), 
                    multiple = TRUE
        )
      })
      output$playerpickhistout <- renderUI({
        pickerInput(inputId = "playerpickhist", 
                    label = "Spieler", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Zeige alle Mitspieler",
                      deselectAllText = "Alle abwählen",
                      selectAllText = "Alle auswählen"
                    ), 
                    multiple = TRUE)
      })
      
      output$playerpickhisttout <- renderUI({
        pickerInput(inputId = "playerpickhistt", 
                    label = "Spieler", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Zeige alle Mitspieler",
                      deselectAllText = "Alle abwählen",
                      selectAllText = "Alle auswählen"
                    ), 
                    multiple = TRUE)
        
      })
      
      output$delete_player <- renderUI({
        pickerInput(inputId = "del_player", 
                    label = "Spieler löschen", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Zeige alle Mitspieler"
                    ), 
                    multiple = F)
      })
      
      show_alert(
        title = "Spieler hinzugefügt",
        text = paste0(input$new_user, " wurde erfolgreich hinzugefügt."),
        type = "success"
      )
      
      
    })
    
    # What happens when a player is deleted
    observeEvent(input$delplconf,{
      
      player_names <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Name]")
      num_kick <- which(player_names$Name==input$del_player & player_names$Nutzer == waits$user)
      player_names <- player_names[-num_kick,]

      dbWriteTable(con, "[siedler_app].[dbo].[Name]", player_names, overwrite=T, row.names=F, col.Names=T)
      player_names <- player_names[(player_names$Nutzer == waits$user),]
      output$playerpickchoice<-renderUI({
        pickerInput(inputId = "playerpick", 
                    label = "Spieler", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Bitte wähle alle Mitspieler aus",
                      deselectAllText = "Alle abwählen",
                      selectAllText = "Alle auswählen"
                    ), 
                    multiple = TRUE
        )
      })
      output$playerpickhistout <- renderUI({
        pickerInput(inputId = "playerpickhist", 
                    label = "Spieler", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Zeige alle Mitspieler",
                      deselectAllText = "Alle abwählen",
                      selectAllText = "Alle auswählen"
                    ), 
                    multiple = TRUE)
      })
      
      output$playerpickhisttout <- renderUI({
        pickerInput(inputId = "playerpickhistt", 
                    label = "Spieler", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Zeige alle Mitspieler",
                      deselectAllText = "Alle abwählen",
                      selectAllText = "Alle auswählen"
                    ), 
                    multiple = TRUE)
        
      })
      
      output$delete_player <- renderUI({
        pickerInput(inputId = "del_player", 
                    label = "Spieler löschen", 
                    choices = player_names$Name, 
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 5",
                      title = "Zeige alle Mitspieler"
                    ), 
                    multiple = F)
      })
      
      show_alert(
        title = "Spieler entfernt",
        text = paste0(input$del_player, " wurde erfolgreich entfernt."),
        type = "success"
      )
      
      
    })
    
    # What is all resetted once the new game button is pressed
    observeEvent(input$new,{
      waits$resetindicator<-1   # used to change button labels
      waits$temp <- 0
      waits$inp <- 0
      waits$m <- 0
      waits$n <- 1
      waits$t1 <- 0
      waits$t2 <- 0
      waits$t3 <- 0
      waits$t4 <- 0
      waits$t5 <- 0
      waits$text <- 0
      waits$finish <- 0
      waits$players <- 0
      waits$data <- 0
      waits$version <- 0
      waits$online <- 0
      waits$statistik_new <- 0
      waits$spieler <- 0
      waits$newpl <- 0
      lang$t1 <- 0
      lang$t2 <- 0
      lang$t3 <- 0
      lang$t4 <- 0
      lang$t5 <- 0
      lang$histtable <- 0
      df$temp <- 0
      df$inp <- 0
      
      
      gsiedler <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Siedler]")
      gsiedler_user <- gsiedler[,length(gsiedler)]
      gsiedler <- data.frame(gsiedler[gsiedler$Nutzer == waits$user,1:length(gsiedler)-1])
      names(gsiedler) <- c("Datum", "Spieleranzahl", "Spieldauer", "Sieger", "Version", "Online", "2", "3", "4", "5", "6", "7",
                           "8", "9", "10", "11", "12", "Spieler 1", "Spieler 2", "Spieler 3", "Spieler 4", "Spieler 5", "Spieler 6")
      gsiedler$`Spieler 1` <- stri_replace_all_regex(gsiedler$`Spieler 1`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 2` <- stri_replace_all_regex(gsiedler$`Spieler 2`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 3` <- stri_replace_all_regex(gsiedler$`Spieler 3`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 4` <- stri_replace_all_regex(gsiedler$`Spieler 4`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 5` <- stri_replace_all_regex(gsiedler$`Spieler 5`, "^\\s+|\\s+$", "")
      gsiedler$`Spieler 6` <- stri_replace_all_regex(gsiedler$`Spieler 6`, "^\\s+|\\s+$", "")
      waits$statistik_new <- gsiedler
      
      player_names <- dbGetQuery(con, "SELECT * FROM [siedler_app].[dbo].[Name]")
      names(player_names) <- c("Name", "Nutzer")
      player_names <- data.frame("Name" = player_names[player_names$Nutzer == waits$user,1])
      
    })
    
    

    
    
#####################################
######## Graphical outputs ##########
#####################################
    
    # Current histogram
    output$siedler<-renderPlot({
      if(sum(waits$data)==0 | waits$resetindicator==1){
        return() # no error if no data
      }
      ggplot(mapping = aes(waits$data)) +
        scale_x_continuous(breaks=2:12, limits = c(1.6, 12.4))+
        xlab("Würfelsumme")+
        ylab("Anzahl")+
        geom_bar(dist, mapping = aes(x=num, y=prob*length(waits$data)), stat="identity", fill="steelblue", width=0.75, alpha = .3)+
        geom_histogram(binwidth = 0.5, alpha = .8)+
        geom_vline(aes(xintercept=mean(waits$data)),
                   color="blue", linetype="dashed", size=1)+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(breaks= 0:waits$n)
    })
    
    # Table under current histogram
    output$summary <- renderTable({
      if (waits$resetindicator==3 | waits$resetindicator==4) {
        if (all(waits$t2==as.integer(waits$t2))==TRUE) {
          data.frame("Mittelwert"=waits$t1, 
                     "Median"=as.integer(waits$t2),
                     "Modalwert"=as.integer(waits$t3),
                     "Anzahl der Würfe"=as.integer(waits$t4),
                     "Zeit"=waits$t5,
                     check.names=FALSE)
        }
        else{
          data.frame("Mittelwert"=waits$t1, 
                     "Median"=waits$t2,
                     "Modalwert"=as.integer(waits$t3),
                     "Anzahl der Würfe"=as.integer(waits$t4),
                     "Zeit"=waits$t5,
                     check.names=FALSE)
          
        }
        
      }else{
        if (all(waits$t2==as.integer(waits$t2))==TRUE) {
          data.frame("Mittelwert"=waits$t1, 
                     "Median"=as.integer(waits$t2),
                     "Modalwert"=as.integer(waits$t3),
                     "Anzahl der Würfe"=as.integer(waits$t4),
                     check.names=FALSE)
        }
        else{
          data.frame("Mittelwert"=waits$t1, 
                     "Median"=waits$t2,
                     "Modalwert"=as.integer(waits$t3),
                     "Anzahl der Würfe"=as.integer(waits$t4),
                     check.names=FALSE)
          
        }
      }
    })
    
    
    # Historical histogram
    output$stathist<-renderPlot({
      stat_tot_use <- waits$statistik_new
      if (input$versionhist!=10) {
        stat_tot_use <- subset(stat_tot_use, as.numeric(stat_tot_use$Version)==as.numeric(input$versionhist))
      }
      if (input$time!=1) {
        n <- nrow(stat_tot_use)
        stat_tot_use <- stat_tot_use[max(n-as.numeric(input$time)+1,1):n,]
      }
      
      for (name in input$playerpickhist) {
        for (row in nrow(stat_tot_use):1) {
          if (!is.element(name, stat_tot_use[row,18:23])) {
            stat_tot_use <- stat_tot_use[-row,] 
          }
        }
      }
      
      stat_tot <- stat_tot_use[,7:17]
      req(nrow(stat_tot)>=1)
      #      stat_tot <- data.frame(lapply(stat_tot, as.numeric))
      stat_tot <- data.frame(colname = names(stat_tot),colSums_demo=colSums(stat_tot))
      m <- which.max(stat_tot$colSums_demo)
      n <- max((stat_tot[m,2]),1/6*sum(stat_tot$colSums_demo))
      vec_version <- NULL
      for (i in 2:12) {
        vec_version <- c(vec_version, rep(i, stat_tot$colSums_demo[i-1]))
      }
      hist_table <- data.frame(table(stat_tot_use[,4]))
      names(hist_table) <- c("Gewinner", "Anzahl")
      hist_table <- hist_table[order(-hist_table$Anzahl),]
      lang$t1 <- round(mean(vec_version),2)
      lang$t2 <- round(median(vec_version),2)
      lang$t3 <- m+1
      lang$t4 <- length(vec_version)
      lang$t5 <- nrow(stat_tot_use)
      lang$histtable <- hist_table
      print(ggplot(stat_tot, mapping = aes(colSums_demo)) + 
              scale_x_continuous(breaks=2:12, limits = c(1.6, 12.4))+
              ggtitle("Historisches Histogramm der Siedler Würfel-Würfe")+
              xlab("Würfelsumme")+
              ylab("Anzahl")+
              geom_bar(dist, mapping = aes(x=2:12, y=prob*length(vec_version)), stat="identity", fill="steelblue", width=0.75, alpha = .3)+
              geom_bar(stat_tot, mapping = aes(x=2:12, y=colSums_demo), stat="identity",  width=0.5, alpha = .8)+
              geom_vline(aes(xintercept=mean(vec_version)),
                         color="blue", linetype="dashed", size=1)+
              theme(plot.title = element_text(hjust = 0.5)))
      
    })
    
    # Historical table
    output$statdata <- renderTable({
      stat_tot_use <- waits$statistik_new
      if (input$versionhistt!=10) {
        stat_tot_use <- subset(stat_tot_use, as.numeric(stat_tot_use$Version)==as.numeric(input$versionhistt))
      }
      if (input$timet!=1) {
        n <- nrow(stat_tot_use)
        stat_tot_use <- stat_tot_use[max(n-as.numeric(input$timet)+1,1):n,]
      }
      
      for (name in input$playerpickhistt) {
        for (row in nrow(stat_tot_use):1) {
          if (!is.element(name, stat_tot_use[row,18:23])) {
            stat_tot_use <- stat_tot_use[-row,] 
          }
        }
      }
      stat_tot_use <- stat_tot_use[,1:17]
      stat_tot_use[,7:17] <- lapply(stat_tot_use[,7:17], as.integer)
      stat_tot_use[,-6]
    })
    
    # Table under historical histogram
    output$summaryhist <- renderTable({
      data.frame("Mittelwert"=lang$t1, 
                 "Median"=as.integer(lang$t2),
                 "Modalwert"=as.integer(lang$t3),
                 "Anzahl der Würfe"=as.integer(lang$t4),
                 "Anzahl der Spiele"=lang$t5,
                 check.names=FALSE)
      
    })
    
    # Leaderboard
    output$summaryhistwinner <- renderTable({
      
      lang$histtable
      
    })
    
}
