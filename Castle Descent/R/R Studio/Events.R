#Function for fairy event
fairy_event = function(castle_data,player){
  #Add space between previously printed screen and new screen
  cat(rep("\n", 50))
  #cat with \n can be used here; however, unlike print,
  #using makes the printed screen look aesthetically unpleasing
  #numbers that are printed along with the board are moved aside to allow for the text
  #that does not happen with print
  print('You encountered a fairy!',quote = F)
  #Fairies increase health
  print('Your HP increased by 10 points.',quote = F)
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print(castle_data$castle[,,player$floor], quote = F)
  player$player_hp = player$player_hp + 10
  print(paste('New HP:',player$player_hp), quote = F)
  #Add pause to allow player to read information
  Sys.sleep(1.5)
  #Adding back door and a zero in dataframe
  castle_data$castle[player$movement_coordinate] = '\U1F6AA'
  castle_data$dataframe[player$castle_dataframe_row,5] = 0
  #Return information
  fairy_event_output = c(castle_data,player)
  return(fairy_event_output)
}
#Function for genie event
genie_event = function(castle_data,player){
  cat(rep("\n", 50))
  print('You encountered a genie!',quote = F)
  print('Your attack range increased by 2 points.',quote = F)
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print(castle_data$castle[,,player$floor], quote = F)
  player$attack_range = player$attack_range + 2
  #Genies inclease attack range
  print(paste('New attack range: ', paste0(min(player$attack_range),':',max(player$attack_range))), quote = F)
  Sys.sleep(2)
  #Adding back door and a zero in dataframe
  castle_data$castle[player$movement_coordinate] = '\U1F6AA'
  castle_data$dataframe[player$castle_dataframe_row,5] = 0
  #Return information
  genie_event_output = c(castle_data,player)
  return(genie_event_output)
}
#Function for monster event
monster_event = function(castle_data,player){
  cat(rep("\n", 50))
  #Go to dataframe and extract 
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print('You encountered a monster!', quote = F)
  print(castle_data$castle[,,player$floor], quote = F)
  #Display player information and monster hp that was extracted from the dataframe
  print(paste('Your HP: ',player$player_hp), quote = F)
  monster_hp = castle_data$dataframe[player$castle_dataframe_row,'hp']
  print(paste('Monster HP: ', monster_hp),quote = F)
  
  #An argument = length 0 issue occurs if this variable is left empty 
  player_action = 'a'
  #While loop so that player can engage with monster unless they decide to run, they faint, or they win
  while(!(player_action %in% c('r','run') | monster_hp == 0 | player$player_hp == 0)){
    read_console_player_monster_action()
    if(player_action == 'attack' | player_action == 'a'){
      cat(rep("\n", 50))
      print(castle_data$castle[,,player$floor], quote = F)
      print('You decided to attack',quote = F)
      player$attack_power = sample( player$attack_range,1)
      print(paste('You dealt',player$attack_power, 'points of damage'),quote = F)
      #Monster hp set to zero to exit loop if attack > than monster hp
      if(monster_hp - player$attack_power <= 0){
        castle_data$dataframe[player$castle_dataframe_row,'hp'] = monster_hp = 0
        print('The monster fainted. You won!',quote = F)
        Sys.sleep(2)
        #Adding back door and a zero in dataframe
        castle_data$castle[player$movement_coordinate] = '\U1F6AA'
        castle_data$dataframe[player$castle_dataframe_row,5] = 0
      }
      else{
        #If monster lives it gets to attack 
        #attack power is random
        castle_data$dataframe[player$castle_dataframe_row,'hp'] = monster_hp =  monster_hp - player$attack_power
        print(paste('Monster HP: ', monster_hp), quote = F)
        monster_attack = sample(1:5,1)
        player$player_hp = player$player_hp - monster_attack
        print(paste('Monster dealt', monster_attack, 'points of damage'),quote = F)
        print(paste('Your HP: ', player$player_hp), quote = F)
        
        if(player$player_hp <= 0){
          #player health set to zero if monster attack > than player hp
          player$player_hp = 0
          print('You died.', quote = F)
        }
        
      }
    }
  }
  #if player decides to run
  if(isTRUE(player_action %in% c('r','run')) == T){
    print('You decided to run.')
  }
  castle_data$castle[player$movement_coordinate] = '\U1F6AA'
  monster_event_output = c(castle_data,player)
  return(monster_event_output)
  
}
#Function to move to the next floor
move_to_next_floor_event = function(castle_data, player, zombie){
  cat(rep("\n", 50))
  print('You found the stars!', quote = F)
  print(paste('You can now advance to floor', player$floor + 1,'!'), quote = F)
  #Add 1 to player's current z-position
  player$floor = player$floor + 1
  player$coordinate[3] = player$floor
  castle_data$castle[player$coordinate] = '\U1F93A'
  #Spawn zombie onto new floor with player
  event_output = zombie$move_to_new_floor_event(castle_data = castle_data, player = player)
  castle_data = event_output[1:2]
  player = event_output[[3]]
  print(castle_data$castle[,,player$floor], quote = F)
  Sys.sleep(2)
  #Return information
  move_to_next_floor_event_output = c(castle_data,player)
  return(move_to_next_floor_event_output)
}

