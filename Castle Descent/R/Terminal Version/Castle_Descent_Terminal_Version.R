#Script to play the actual game
#Objective is to get to the bottom of the castle and find the exit while avoiding the zombie
#Keypress only works in the commandline/terminal. It allows you to input a key without having to  press enter
library(keypress)  
source('~/Terminal Version/Castle_Create.R')
source('~/Terminal Version/Events.R')
source('~/Terminal Version/Classes.R')
#The dot hides variable from global environment
#Variable used to determine if the player is greeted with the 
#welcome screen or not
.iteration <<- 0

castle_descent = function(){
  
  #Using castle-create to generate a new random game board
  castle_data = castle_create()
  #creating a class for the player
  player = player_class(coordinate = which(castle_data$castle=='\U1F93A', arr.ind = T),
                        player_hp = 100,
                        attack_range=5:10,
                        #wasd controls will be added to the players current 3d coordinate to 
                        #obtain a new coordinate corresponding to the direction
                        movement_dict = list('w' = c(-1,0,0), 'a' = c(0,-1,0), 's' = c(1,0,0),  'd' = c(0,1,0)),
                        floor = 1, 
                        encountered_object = '\u2800')
  
  zombie = zombie_class(current_coordinate = which(castle_data$castle=='\U1F9DF', arr.ind = T),
                        initial_coordinate = which(castle_data$castle=='\U1F9DF', arr.ind = T),
                        movement_dict = list('w' = c(-1,0,0), 'a' = c(0,-1,0), 's' = c(1,0,0),  'd' = c(0,1,0),
                                             'diag_up_left' = c(-1,-1,0), 'diag_up_right' = c(-1,1,0),
                                             'diag_down_left' = c(1,-1,0), 'diag_down_right' = c(1,1,0)),
                        floor = 1)
  zombie$distance_to_player = zombie$euclidean_distance(zombie$initial_coordinate,player$coordinate)
  
  
  #######################################Setup Complete########################################
  #if the player decides to play a new game, 
  #they do not need to be greeted with the welcome screen and objective agin
  if(.iteration == 0){
    print('Welcome to Castle Descent!', quote = F)
    Sys.sleep(1)
    print('The objective of the game is to descend the bottom of the castle while avoiding the zombie.', quote = F)
    Sys.sleep(2)
    print('You will be starting at the top of the castle!', quote = F)
    Sys.sleep(2)
  }
  else{
    cat(rep("\n", 50))
    print('New game.', quote = F)
    Sys.sleep(1)
  }
  while(!(player$encountered_object=='\U2395' | player$player_hp <= 0 | zombie$distance_to_player == 0)){
    #Add multiple spaces so that only a single print out of the game board is on the screen 
    cat(rep("\n", 50))
    #Inform player about the current floor they are on
    print(paste('Floor',player$floor, 'of',length(castle_data$castle)/(nrow(castle_data$castle)*ncol(castle_data$castle))), quote = F)
    #print current floor the player is on
    print(castle_data$castle[,,player$floor], quote = F)
    #player directional inputs
    print('w (up), a (left), s (down), d (right): ', quote = F)
    player_action = tolower(keypress(block = T))  
    while(!(player_action %in% c('w','a','s','d'))){
      player_action = tolower(keypress(block = T))  
    }
    #Take player current coordinate and add
    player$movement_coordinate = player$coordinate + player$movement_dict[[player_action]]
    if(castle_data$castle[player$movement_coordinate] == '\u2800'){
      player$encountered_object = castle_data$castle[player$movement_coordinate]
      # Add emoji to new coordinate is the object is empty
      castle_data$castle[player$coordinate] = '\u2800'
      #Make previous player coordinate empty
      player$coordinate = player$movement_coordinate
      #Movement coordinate now the new player coordinate
      castle_data$castle[player$coordinate] = '\U1F93A'
      #Zombie event
      event_output = zombie$pathfinding(castle_data = castle_data, player = player)
      castle_data = event_output[1:2]
      player = event_output[[3]]
    }
    else if(castle_data$castle[player$movement_coordinate] =='\U1F6AA'){
      #Look into dataframe to see what encountered object is supposed to be
      player$castle_dataframe_row = which(castle_data$dataframe$x == player$movement_coordinate[1] & castle_data$dataframe$y == player$movement_coordinate[2] & castle_data$dataframe$z == player$movement_coordinate[3])
      number = castle_data$dataframe[player$castle_dataframe_row,'hp']
      player$encountered_object = castle_data$dataframe[player$castle_dataframe_row,4]
      
      if(number > 0){
        #Switch statement for events
        switch(player$encountered_object,
               '\U1F9DA'= {event_output = fairy_event(castle_data = castle_data, player = player)},
               '\U1F9DE'= {event_output = genie_event(castle_data = castle_data, player = player)},
               '\U1F479'= {event_output = monster_event(castle_data = castle_data, player = player)},
               'DS'= {event_output = move_to_next_floor_event(castle_data = castle_data, player = player, zombie = zombie)}
        )
        #Collect the outputs from each function
        castle_data = event_output[1:2]
        player = event_output[[3]]
      }
      else if(number == 0){
        cat(rep("\n", 50))
        if(castle_data$dataframe[player$castle_dataframe_row,'object'] == 'AS'){
          print('You already came from upstairs.', quote = F)
        }
        else{
          print('There is nothing behind this door.', quote = F)
        }
        print(castle_data$castle[,,player$floor], quote = F)
        Sys.sleep(2)
        
      }
      
    }
  }
  #if statement to continue or quit when they die
  if(player$player_hp <= 0 | zombie$distance_to_player == 0){
    print('Want to play again? Yes (y) or No (n): ', quote = F)
    player_retry = tolower(keypress(block = T))                       
    while(!(player_retry %in% c('yes','y','no','n'))){
      player_retry = tolower(keypress(block = T))  
    }
    if(player_retry=='yes'| player_retry=='y'){
      .iteration <<- .iteration + 1
      castle_descent()
    }
    else{
      print('Thank you for playing Castle Descent!', quote = F)
    }
  }
  else{
    #else statement o replay if they win
    cat(rep("\n", 50))
    print('You found the exit!', quote = F)
    castle_data$castle[player$movement_coordinate] = player$encountered_object
    print(castle_data$castle[,,player$floor], quote = F)
    print('Want to play again? Yes (y) or No (n): ', quote = F)
    player_retry = tolower(keypress(block = T))                       
    while(!(player_retry %in% c('yes','y','no','n'))){
      player_retry = tolower(keypress(block = T))  
    }
    if(player_retry=='yes'| player_retry=='y'){
      .iteration <<- .iteration + 1
      castle_descent()
    }
    else{
      print('Thank you for playing Castle Descent!', quote = F)
    }
  }
}

castle_descent()
