#Objective is to get to the bottom of the castle and find the exit
#This version can be played in RStudio and the terminal. It runs on base R packages
#Functional; still a work in progress

#Function for player movement

#Function for fairy event
fairy_event = function(castle_data,player){
  cat('You encountered a fairy!\nYour HP increased by 2 points.')
  '\U1F9DE'
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print(castle_data$castle[,,player$floor], quote = F)
  player$player_health = player$player_health + 10
  print(paste('New HP:',player$player_health), quote = F)
  Sys.sleep(2)
  #Clear object from castle and dataframe
  castle_data$castle[player$movement_coordinate] = '\u2800'
  castle_data$dataframe = castle_data$dataframe[-c(player$castle_dataframe_row),]
  fairy_event_output = c(castle_data,player)
  return(fairy_event_output)
  
}
#Function for genie event
genie_event = function(castle_data,player){
  cat('You encountered a genie!\nYour attack range increased by 2 points.', quote = F)
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print(castle_data$castle[,,player$floor], quote = F)
  player$player_attack_range = player$player_attack_range + 2
  print(paste('New attack range: ', paste0(min(player$player_attack_range),':',max(player$player_attack_range))), quote = F)
  Sys.sleep(2)
  #Clear object from castle and dataframe
  castle_data$castle[player$movement_coordinate] = '\u2800'
  castle_data$dataframe = castle_data$dataframe[-c(player$castle_dataframe_row),]
  genie_event_output = c(castle_data,player)
  return(genie_event_output)
  
}
#Function for monster event
monster_event = function(castle_data,player){
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print('You encountered a monster!', quote = F)
  print(castle_data$castle[,,player$floor], quote = F)
  print(paste('Your HP: ',player$player_health), quote = F)
  print(paste('Your attack range: ', paste0(min(player$player_attack_range),':',max(player$player_attack_range))), quote = F)
  monster_hp = castle_data$dataframe[player$castle_dataframe_row,5]
  print(paste('Monster HP: ', monster_hp),quote = F)
  
  player_action = 'a'
  
  while(!(player_action %in% c('r','run') | monster_hp <= 0)){
    player_action = tolower(noquote(readline('Would you like to attack(a) or run(r)? ')))   
    while(!(player_action %in% c('r','run','attack','a'))){
      player_action = tolower(noquote(readline('Would you like to attack(a) or run(r)? ')))   
    }
    if(player_action == 'attack' | player_action == 'a'){
      print(castle_data$castle[,,player$floor], quote = F)
      print('You decided to attack',quote = F)
      player$attack_power = sample( player$player_attack_range,1)
      print(paste('You dealt',player$attack_power, 'points of damage'),quote = F)
      
      if(monster_hp - player$attack_power <= 0){
        monster_hp = 0
        print('The monster fainted. You won!',quote = F)
        #Clear object from castle and dataframe
        castle_data$castle[player$movement_coordinate] = '\u2800'
        castle_data$dataframe = castle_data$dataframe[-c(player$castle_dataframe_row),]
      }
      else{
        monster_hp =  monster_hp - player$attack_power
        print(paste('Monster HP: ', monster_hp), quote = F)
        monster_attack = sample(1:5,1)
        
        if(player$player_health - monster_attack <= 0){
          print(paste('Monster dealt',monster_attack, 'points of damage'),quote = F)
          print(paste('Your HP: ',player$player_health), quote = F)
          print('You died.', quote = F)
        }
        else{
          player$player_health = player$player_health - monster_attack
          print(paste('Monster dealt', monster_attack, 'points of damage'),quote = F)
          print(paste('Your HP: ', player$player_health), quote = F)
                  }
      }
    }
  }
  if(isTRUE(player_action %in% c('r','run')) == T){
    print('You decided to run.')
  }
  monster_event_output = c(castle_data,player)
  return(monster_event_output)
  
}

move_to_next_floor_event = function(castle_data,player){
  print('You found the stars!', quote = F)
  print(castle_data$castle, quote = F)
  print(paste('You can now advance to floor', player$floor + 1,'!'), quote = F)
  Sys.sleep(2)
  player$floor = player$floor + 1
  player$movement_coordinate[3] = player$floor
  player$player_coordinate = player$movement_coordinate
  castle_data$castle[player$player_coordinate] = '\U1F93A'
  move_to_next_floor_event_output = c(castle_data,player)
  return(move_to_next_floor_event_output)
}

#Function to create castle and castle dataframe containing information about castle

castle_create = function(){
  #Dataframe to contain information of gamestate
  castle_dataframe = data.frame('x' = NA, 'y' = NA, 'z' = NA, 'object' = NA,
                                'hp_usage' = NA)
  # Area of the castle floor will equal castle_width* castle_length
  castle_x_length = sample(8:13, 1)
  castle_y_length = sample(8:13, 1)
  castle_z_length = sample(3:6, 1)
  
  castle = array( '\u2800',dim = c(castle_x_length,castle_y_length,castle_z_length))
  #Spawn walls;These walls be used to ensure players don't go out of bounds in the array - basically boundary detection
  castle[1:nrow(castle),c(1,ncol(castle)),] ='\U2591'
  castle[c(1,nrow(castle)),1:ncol(castle),]='\U2591'
  #One is the top of the castle; this is where the player spawns
  castle[,,1][sample(which(castle[,,1]=='\u2800'),1)] = '\U1F93A'
  #Three is the bottom of the castle; this is where the exit spawns
  castle[,,castle_z_length][sample(which(castle[,,castle_z_length]=='\u2800'),1)] = '\U2395'
  
  castle_dataframe[,1:4] = data.frame(which(castle=='\U2395', arr.ind = T), '\U2395')
  
  #Spawn stairs
  for(grid in 1:(castle_z_length-1)){
    castle[,,grid][sample(which(castle[,,grid]=='\u2800'),1)] = 'S'
  }
  castle_dataframe[2:(castle_dataframe_rows <- nrow(castle_dataframe) + nrow(which(castle=='S', arr.ind = T))),1:4] = data.frame(which(castle=='S', arr.ind = T), 
                                                                                                                                 rep('S',nrow(which(castle=='S', arr.ind = T)) ))
  #Proportion of the empty spaces that will be monsters, genies, or fairies
  monster_proportion = round(length(which(castle == '\u2800'))*0.15,0)
  fairy_proportion = round(length(which(castle == '\u2800'))*0.025,0)
  genie_proportion = round(length(which(castle == '\u2800'))*0.025,0)
  
  #Spawning objects randomly through castle and storing information in dataframe
  castle[,,1:castle_z_length][sample(which(castle == '\u2800'),monster_proportion)] = '\U1F479'
  castle_dataframe[(nrow(castle_dataframe) + 1):(nrow(castle_dataframe) + nrow(which(castle == '\U1F479', arr.ind = T))),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle == '\U1F479', arr.ind = T), rep('\U1F479', nrow(which(castle =='\U1F479', arr.ind = T))),sample(c(10,20,30,40),nrow(which(castle =='\U1F479', arr.ind = T)),replace = T))
  
  castle[,,1:castle_z_length][sample(which(castle == '\u2800'),fairy_proportion)] = '\U1F9DA'
  castle_dataframe[nrow(castle_dataframe) + nrow(which(castle == '\U1F9DA', arr.ind = T)),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle == '\U1F9DA', arr.ind = T), rep('\U1F9DA', nrow(which(castle =='\U1F9DA', arr.ind = T))), rep(1, nrow(which(castle =='\U1F9DA', arr.ind = T))))
  
  castle[,,1:castle_z_length][sample(which(castle == '\u2800'),genie_proportion)] = '\U1F9DE'
  castle_dataframe[nrow(castle_dataframe) + nrow(which(castle == '\U1F9DE', arr.ind = T)),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle == '\U1F9DE', arr.ind = T), rep('\U1F9DE', nrow(which(castle =='\U1F9DE', arr.ind = T))), rep(1, nrow(which(castle =='\U1F9DE', arr.ind = T))))

  #Spawning doors at coordinates that are objects
  for(coordinate in 1:nrow(castle_dataframe)){
    x = unlist(castle_dataframe[coordinate,1:3])
    castle[x[1],x[2],x[3]] = '\U1F6AA'
  }
  
  castle_and_dataframe = list('castle' = castle, 'dataframe' = castle_dataframe)
  return(castle_and_dataframe)
}


castle_descent_player =  setRefClass('castle_info', fields = list(player_health='numeric', player_coordinate='matrix',
                                                                  encountered_object='character',
                                                                  movement_coordinate = 'matrix',
                                                                  player_attack_range='numeric', 
                                                                  attack_power = 'numeric',
                                                                  floor = 'numeric',
                                                                  castle_dataframe_row = 'numeric'
))
#The dot hides variable from global environment

.iteration <<- 0

castle_descent = function(class){
  
  movement_dict = list('w' = c(-1,0,0), 'a' = c(0,-1,0), 's' = c(1,0,0),  'd' = c(0,1,0))
  
  castle_data = castle_create()
  
  player = class(player_coordinate = which(castle_data$castle=='\U1F93A', arr.ind = T),
                 player_health = 100,
                 player_attack_range=5:10,
                 floor = 1,
                 encountered_object = '\u2800'
                 
  )
  #######################################Setup Complete########################################
  if(.iteration == 0){
    print('Welcome to Castle Descent!', quote = F)
    print('You will be starting at the top of the castle!', quote = F)
    Sys.sleep(2)
  }
  else{
    print('New game.', quote = F)
    Sys.sleep(1)
  }
  
  
  while(!(player$encountered_object=='\U2395' | player$player_health <= 0)){    
    
    cat(rep("\n", 50))
    print(paste('Floor',which(castle_data$castle=='\U1F93A', arr.ind = T)[3], 'of',length(castle_data$castle)/(nrow(castle_data$castle)*ncol(castle_data$castle))), quote = F)
    print(castle_data$castle[,,player$floor], quote = F)
    

    player_action = tolower(noquote(readline('w (up), a (left), s (down), d (right): ')))
    while(!(player_action %in% c('w','a','s','d'))){
      player_action = tolower(noquote(readline('w (up), a (left), s (down), d (right): ')))
    }
    
    player$movement_coordinate = player$player_coordinate + movement_dict[[player_action]]
    if(castle_data$castle[player$movement_coordinate] == '\u2800'){
      # Add emoji to new coordinate is the object is empty
      castle_data$castle[player$player_coordinate] = '\u2800'
      #Make previous player coordinate empty
      player$player_coordinate = player$movement_coordinate
      #Movement coordinate now the new player coordinate
      castle_data$castle[player$player_coordinate] = '\U1F93A'
    }
    else if(castle_data$castle[player$movement_coordinate] %in% c('\U1F6AA','\U1F479')){
      #Look into dataframe to see what encountered object is supposed to be
      player$castle_dataframe_row = which(castle_data$dataframe$x == player$movement_coordinate[1] & castle_data$dataframe$y == player$movement_coordinate[2] & castle_data$dataframe$z == player$movement_coordinate[3])
      
      player$encountered_object = castle_data$dataframe[player$castle_dataframe_row,4]
      
       #Switch statement for events
      switch(player$encountered_object,
             '\U1F9DA'= {event_output = fairy_event(castle_data = castle_data, player = player)},
             '\U1F9DE'= {event_output = genie_event(castle_data = castle_data, player = player)},
             '\U1F479'={event_output = monster_event(castle_data = castle_data, player = player)},
             'S'={event_output = move_to_next_floor_event(castle_data = castle_data, player = player)}
      )
      castle_data = event_output[1:2]
      player = event_output[[3]]
    }
  }
  if(player$player_health <= 0){
    player_retry = tolower(noquote(readline('Want to play again? Yes (y) or No (n): ')))                     
    while(!(player_retry %in% c('yes','y','no','n'))){
      player_retry = tolower(noquote(readline('Want to play again? Yes (y) or No (n): '))) 
    }
    if(player_retry=='yes'| player_retry=='y'){
      .iteration <<- .iteration + 1
      castle_descent(class = castle_descent_player)
    }
    else{
      print('Thank you for playing Castle Descent!', quote = F)
    }
  }
  else{
    print('You found the exit!', quote = F)
    castle_data$castle[player$movement_coordinate] = player$encountered_object
    print(castle_data$castle[,,player$floor], quote = F)
    player_retry = tolower(noquote(readline('Want to play again? Yes (y) or No (n): ')))                     
    while(!(player_retry %in% c('yes','y','no','n'))){
      player_retry = tolower(noquote(readline('Want to play again? Yes (y) or No (n): '))) 
    }
    if(player_retry=='yes'| player_retry=='y'){
      .iteration <<- .iteration + 1
      castle_descent(class = castle_descent_player)
    }
    else{
      print('Thank you for playing Castle Descent!', quote = F)
    }
  }
}

castle_descent(class = castle_descent_player)







  

