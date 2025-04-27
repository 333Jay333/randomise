#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(rio)

# Data & variables
df.exercises <- import("data/Übungen_PV_Ziele.csv")
choices.exercises <- df.exercises$Name
choices.pv <- c("Bauchlage mit gestreckten Beinen (flache Bauchlage)","Bauchlage mit angewinkelten Beinen","Seitenlage","Rückenlage mit angewinkelten Beinen",
                "Brüggli-Position (z.B. Bridging)","Kniebeugen (z.B. Squats)","einseitiger Unterarmstütz mit viel Körpergewicht","beidseitiger Unterarmstütz mit wenig Körpergewicht",
                "beidseitiger Unterarmstütz mit viel Körpergewicht","Andere Position (z.B. Vierfüssler, Fersensitz, Kniestand, Rückenlage mit gestreckten Beinen (flache Rückenlage), vorgebeugtes Stehen (z.B. gehaltene Flexion), beidseitig Stützen mit gestreckten Armen)")


# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Übungen mit Positionsverboten ersetzen"),
  
  # Input Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput("toReplace", label = "Welche Übungen im unpersonalisierten Übungsprogramm müssen ersetzt werden?", choices = choices.exercises, multiple = TRUE),
      selectInput("pv", label = "Welche Positionsverbote gibt es?", choices = choices.pv, multiple = TRUE),
      selectInput("toKeep", label = "Welche Übungen sollen im unpersonalisierten Übungsprogramm beibehalten werden?", choices = choices.exercises, multiple = TRUE),
      actionButton("go","Übungen randomisiert ersetzen"),
      p(""),
      actionButton("reset","Reset"),
      p(""),
      p("Firefox Browser: Reset Button funktioniert nicht. Bitte Seite neu laden und dabei Shift gedrückt halten.")
    ),
    
    # Show text
    mainPanel(
      span(textOutput("header"), style = "font-size:20px; font-style:bold" ),
      verbatimTextOutput("generated"),
      span(textOutput("header_2"), style = "font-size:20px; font-style:bold" ),
      verbatimTextOutput("final_program")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # randomising logic
  # I create a list of eligible exercises that can be randomised from
  
  # Criteria 1: If there is an alternative exercise with the same goal, I try to choose that one
  # Criteria 2: The new exercise must not have a Positionsverbot that has been specified
  # Criteria 3: The new exercise must not already be part of the program (toKeep)
  # Criteria 4: The new exercise must not have a same goal as one that is already part of the program (toKeep.goal)
  # Criteria 5: The new exercise must not have been picked as a replacement exercise before
  # Criteria 6: If an exercise with the same goal can't be picked because of one criterium, it still needs to belong to the same Gruppe
  
  # I create a list of indices that fit these criteria (randomise.choices)
  # Then, I can randomly select from this list to get my replacement exercises
  # And I can repeat the whole process in a for loop if I have multiple exercises to replace
  
  randomising <- eventReactive(input$go, {
    
    # empty vector to hold the replaced exercises that will be returned at the end
    randomised <- c()
    
    # Next, I need to get the indices for the exercises that need to be replaced as well as that will be kept in the program
    toReplace.index <- df.exercises[df.exercises$Name %in% input$toReplace,]$Index
    # thanks for this solution https://www.geeksforgeeks.org/select-rows-from-a-dataframe-based-on-values-in-a-vector-in-r/
    toKeep.index <- df.exercises[df.exercises$Name %in% input$toKeep,]$Index
    
    # if any of the toKeep exercises have exercises with the same goal, I don't want them to be part of randomise.choice
    # therefore, I make a list of the indices of the exercises with the same goal, such that I can remove those.
    toKeep.goal.index <- c()
    for (index in toKeep.index) {
      if (df.exercises$BOOL_Ziel[index]) {
        toKeep.goal.index <- c(toKeep.goal.index, df.exercises[df.exercises$Ziel %in% df.exercises$Ziel[index],]$Index)
      }
    }
    
    # Now I enter the for loop which iterates over all the exercises in input$toReplace to find a suitable replacement
    for (v.replace in input$toReplace) {
      
      #get index of exercise to replace
      v.index <- df.exercises$Index[df.exercises$Name==v.replace]
      
      #check if there are other exercises with the same goal
      if (df.exercises$BOOL_Ziel[v.index]) {
        # get indices of exercises with the same goal
        v.index.ziel <- df.exercises$Index[df.exercises$Ziel==df.exercises$Ziel[v.index]]
        
        # remove index of exercise to be replaced
        v.index.ziel <- v.index.ziel[!v.index.ziel %in% toReplace.index]
        
        # remove indices of exercises to keep
        v.index.ziel <- v.index.ziel[!v.index.ziel %in% toKeep.index]
        
        # remove index of exercises that are already in the program -> in case it's not the first iteration of the for loop
        if (length(randomised) > 0) {
          index <- df.exercises$Index[df.exercises$Name==randomised]
          v.index.ziel <- v.index.ziel[!v.index.ziel %in% index]
        }
        
        # vector to hold choices for randomise
        randomise.choices <- c()
        
        # for each exercise with same goal, check if it includes a pv. IF NOT, add that exercise as a choice for randomise
        for (index in v.index.ziel) {
          if (df.exercises$BOOL_PV[index]) {
            pv.exercises.ziel <- c(df.exercises$PV1[index], df.exercises$PV2[index])
            pv.matching <- pv.exercises.ziel %in% input$pv
            # if there is no true -> add to randomise.choices -> thanks ChatGPT for the syntax
            if (!TRUE %in% pv.matching) {
              randomise.choices <- c(randomise.choices,df.exercises$Name[index])
            }
          }
        }
        
        # Now, if a suitable alternative exercise with the same goal has been found, this next code will not be triggered (randomise.choices will have at least one element)
        # However, if the exercises with the same goal were not possible, I need to broaden my search to the Gruppe of the exercise
        if (length(randomise.choices) == 0) {
          # if the exercises with the goal had to be excluded because of pv, choose exercises of the same group
          v.group <- df.exercises$Gruppe[v.index]
          
          # get indices of exercises in same group
          v.index.group <- df.exercises$Index[df.exercises$Gruppe==v.group]
          
          # remove index of exercises to be replaced
          v.index.group <- v.index.group[!v.index.group %in% toReplace.index]
          
          # remove index of exercises that are kept in the program -> also the ones with the same goals if applicable
          if (length(toKeep.goal.index) > 0) {
            v.index.group <- v.index.group[!v.index.group %in% toKeep.goal.index]
          } else {
            v.index.group <- v.index.group[!v.index.group %in% toKeep.index]
          }
          
          # remove index of exercises that are already in the program -> again, if not first iteration
          if (length(randomised) > 0) {
            index <- df.exercises$Index[df.exercises$Name==randomised]
            v.index.group <- v.index.group[!v.index.group %in% index]
          }
          
          # remove indices of exercises with same goal -> already checked above, not eligible
          v.index.group <- v.index.group[!v.index.group %in% v.index.ziel]
          
          # for each exercise in the same group, check if it includes a pv. IF NOT, add that exercise as a choice for randomise
          for (index in v.index.group) {
            if (df.exercises$BOOL_PV[index]) {
              pv.exercises.group <- c(df.exercises$PV1[index], df.exercises$PV2[index])
              pv.matching <- pv.exercises.group %in% input$pv
              # if there is no true -> add to randomise.choices
              if (!TRUE %in% pv.matching) {
                randomise.choices <- c(randomise.choices,df.exercises$Name[index])
              } # here I also need the else in order to add exercises that don't have a pv to the options
              # before I was removing from the list, here I am adding to the list
            } else {
              randomise.choices <- c(randomise.choices,df.exercises$Name[index])
            }
          }
        }
        
        # RANDOMISE
        # Now that my randomise.choices has been completed, I can sample one from the options
        # This will be returned after all the iterations of the for loop
        randomised <- c(randomised, sample(randomise.choices,1))
        
      } else {
        # In case there are no eligible exercises with the same goals, I can immediately start with the Gruppe,
        # but I also need to go through the whole checking process as before
        
        # vector to hold choices for randomise
        randomise.choices <- c()
        
        # get the same group
        v.group <- df.exercises$Gruppe[v.index]
        
        # get indices of exercises in same group
        v.index.group <- df.exercises$Index[df.exercises$Gruppe==v.group]
        
        # remove index of exercises to be replaced
        v.index.group <- v.index.group[!v.index.group %in% toReplace.index]
        
        # remove index of exercises that are kept in the program -> also the ones with the same goals if applicable
        if (length(toKeep.goal.index) > 0) {
          v.index.group <- v.index.group[!v.index.group %in% toKeep.goal.index]
        } else {
          v.index.group <- v.index.group[!v.index.group %in% toKeep.index]
        }
        
        # remove index of exercises that are already in the program
        if (length(randomised) > 0) {
          index <- df.exercises$Index[df.exercises$Name==randomised]
          v.index.group <- v.index.group[!v.index.group %in% index]
        }
        
        # for each exercise in the same group, check if it includes a pv. IF NOT, add that exercise as a choice for randomise
        for (index in v.index.group) {
          if (df.exercises$BOOL_PV[index]) {
            pv.exercises.group <- c(df.exercises$PV1[index], df.exercises$PV2[index])
            pv.matching <- pv.exercises.group %in% input$pv
            # if there is no true -> add to randomise.choices
            if (!TRUE %in% pv.matching) {
              randomise.choices <- c(randomise.choices,df.exercises$Name[index])
            }
          } else { #same as before, I am adding instead of removing, therefore I need the else
            randomise.choices <- c(randomise.choices,df.exercises$Name[index])
          }
        }
        
        # RANDOMISE
        # same as before
        randomised <- c(randomised, sample(randomise.choices,1))
      }
      
    }
    
    # because this gets called as a function, I need to return the results of the randomisation after the for loop
    return(randomised)
  })
  
  
  # Lastly, I can display my final program in the mainPanel
  finalProgram <- eventReactive(input$go, {
    output <- input$toKeep
    output <- c(output, randomising())
    output.sorted <- output[order(factor(output, levels = choices.exercises))] #thanks ChatGPT -> sort according to order of choices
    return(output.sorted)
  })
  
  output$generated <- renderText({ 
    paste("Die Übung ", paste(" ",input$toReplace), "wird mit der Übung ", paste(" ",randomising()),"ersetzt.\n\n", sep = "\n")
  })
  
  # Such that it looks nice
  output$final_program <- renderText({
    paste(finalProgram(), "", collapse = "\n")
  })
  
  # Only show once the program gets generated, not before
  observeEvent(input$go, {
    output$header <- renderText({"Ersetzte Übungen"})
    output$header_2 <- renderText({"Neues, unpersonalisiertes Übungsprogramm"})
  })
  
  # Reset Button
  observeEvent(input$reset, {
    reset("toKeep")
    updateSelectInput(session, "toKeep", choices = choices, selected = "")
    reset("toReplace")
    updateSelectInput(session, "toReplace", choices = choices, selected = "")
    reset("generated")
    updateSelectInput(session, "generated", choices = choices, selected = "")
    session$reload()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
