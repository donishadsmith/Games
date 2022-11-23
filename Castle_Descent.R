#Objective is to get to the bottom of the castle and find the exit
#Python version in development

castle_descent = function(){
  moves = c("w", "s", "d","a",
            "r", "run", "attack", "y", "yes", "n", "no")
  castle_descent_player_information =  setRefClass("castle_info", fields = list(player_health="numeric", player_coordinate="matrix",
                                             encountered_object="character",
                                             movement_coordinate = "matrix",
                                             player_attack_range="numeric", 
                                             attack_power = "numeric",
                                             floor = "numeric"
                                              ))
  
  try_again = function(){
    player_retry = tolower(as.character(readline("Want to play again? Yes (y) or No (n): ")))
    while(!(player_retry %in% moves[8:11])){
      player_retry = tolower(as.character(readline(paste(paste0("'",player_retry,"'")," Is not a valid option. Please input yes (y) or no (n): "))))
    }
    if(player_retry=="yes"|| player_retry=="y"){
      castle_descent()
    }
   else{
      print("Thank you for playing Castle Descent!", quote = F)
    }
  }
  #######################################Setup Game########################################
  #Spawn castle
  #Castle will have a randomly selected row and column length from 8 to 10.
  # Area of the castle floor will equal castle_row * castle_col
  castle_row = 1:sample(8:10, 1)
  castle_col = 1:sample(8:10, 1)
  #Numerical board will contain numerical information regarding monster health and 
  #binary information indicating if a fairy/genie has been used or not
  numerical_board = castle = array( '\u2800',dim = c(length(castle_row),length(castle_col),3))
  #Spawn walls;These walls be used to ensure players don't go out of bounds in the array - basically boundary detection
  castle[1:nrow(castle),c(1,ncol(castle)),] ="\U2591"
  castle[c(1,nrow(castle)),1:ncol(castle),]="\U2591"
  #One is the top of the castle; this is where the player spawns
  castle[,,1][sample(which(castle[,,1]=='\u2800'),1)] = "\U1F93A"
  #Three is the bottom of the castle; this is where the exit spawns
  castle[,,3][sample(which(castle[,,3]=='\u2800'),1)] = "\U2395"
  #Spawn stairs
  castle[,,1][sample(which(castle[,,1]=='\u2800'),1)] = "S"
  castle[,,2][sample(which(castle[,,1]=='\u2800'),1)] = "S"
  noquote(castle)
  #Proportion of the empty spaces that will be monsters, genies, or fairies
  #Proportion of the empty spaces that will be monsters, genies, or fairies
  monster_proportion = round(length(which(castle == '\u2800'))*0.15,0)
  fairy_proportion = round(length(which(castle == '\u2800'))*0.025,0)
  genie_proportion = round(length(which(castle == '\u2800'))*0.025,0)
  #Monster spawn; randomly throughout the castle
  castle[,,1:3][sample(which(castle == '\u2800'),monster_proportion)] = "\U1F479"
  #Fairy spawn
  castle[,,1:3][sample(which(castle == '\u2800'),fairy_proportion)] = "\U1F9DA"
  #Genie spawn
  castle[,,1:3][sample(which(castle == '\u2800'),genie_proportion)] = "\U1F9DE"
  #Game Board that player will see, here monsters, fairy genies, and stairs are hidden with
  #the door unicode
  game_board = castle
  #Everything that is not an empty space, castle wall, 
  game_board[which(!(game_board %in% c('\u2800',"\U2591","\U1F93A")))] = "\U1F6AA"
  noquote(game_board)
  noquote(castle)
  #Adding information about monster health and fairies to the board 
  #Ones for fairies and genies indicate that the fairy/genie has not been used yet
  numerical_board[which(castle %in% c("\U1F9DE","\U1F9DA"))] = 1
  # Preventing error about length
  
  numerical_board[,,1][which(castle[,,1]=="\U1F479")] = sample(c(5:10),length(which(castle[,,1]=="\U1F479")), 
                                                               replace = T)
  numerical_board[,,2][which(castle[,,2]=="\U1F479")] = sample(c(10:15),length(which(castle[,,2]=="\U1F479")), 
                                                               replace = T)
  numerical_board[,,3][which(castle[,,3]=="\U1F479")] = sample(c(15:20),length(which(castle[,,3]=="\U1F479")),
                          replace = T)
  player_information = castle_descent_player_information(player_coordinate = which(castle=="\U1F93A", arr.ind = T),
                                               player_health = 100,
                                               player_attack_range=5:10,
                                               floor = 1,
                                               encountered_object = "S"
                                               
                                              )

  #######################################Setup Complete########################################
  print("Welcome to Castle Descent!")
  print(noquote(game_board))
  print("You will be starting at the top of the castle!")
  while(!(isTRUE(player_information$encountered_object=="\U2395")==T)){    
    if(player_information$encountered_object=="S"){
      temp_board = game_board[,,player_information$floor]
      temp_movement_coordinate = player_information$movement_coordinate[-c(3)]
      temp_board[temp_movement_coordinate[1],temp_movement_coordinate[2]] = "\U1F93A"
      print(temp_board, quote = F)
    }
    else{
    print(noquote(game_board[,,player_information$floor]))
    }
    player_input = tolower(as.character(readline("w (up), a (left), s (down), d (right): ")))
    while(!(player_input %in% moves[1:4])){
      player_input = tolower(as.character(readline(paste(paste0("'",player_input,"'"), "Is not a valid input. Please input: w (up), a (left), s (down), or, d (right): "))))
    }
    #Controller
    if(player_input == "w"){
      player_information$movement_coordinate = player_information$player_coordinate
      player_information$movement_coordinate[1] = player_information$movement_coordinate[1] - 1  
      }
    else if(player_input == "s"){
      player_information$movement_coordinate = player_information$player_coordinate
      player_information$movement_coordinate[1] = player_information$movement_coordinate[1] + 1
    }
    else if(player_input == "a"){
      player_information$movement_coordinate = player_information$player_coordinate
      player_information$movement_coordinate[2] = player_information$movement_coordinate[2] - 1
    } 
    else if(player_input == "d"){
      player_information$movement_coordinate = player_information$player_coordinate
      player_information$movement_coordinate[2] = player_information$movement_coordinate[2] + 1
    } 
    #See which object is in the coordinate
    player_information$encountered_object = castle[player_information$movement_coordinate]  
    #Object commands
    if(player_information$encountered_object=='\u2800' || player_information$encountered_object=="\U1F93A"){
      # Add emoji to new coordinate is the object is empty
      game_board[player_information$movement_coordinate] = "\U1F93A"
      #Make previous player coordinate empty
      game_board[player_information$player_coordinate] = '\u2800'
      #Movement coordinate now the new player coordinate
      player_information$player_coordinate = player_information$movement_coordinate
    }
    else if(player_information$encountered_object=="\U1F9DA"){
      game_board[player_information$movement_coordinate] = castle[player_information$movement_coordinate]
      print(noquote(game_board[,,player_information$floor]))
      print("You encountered a fairy! Your health increases by 10 HP.", quote = F)
      player_information$player_health = player_information$player_health + 10
      print(paste("New HP:",player_information$player_health), quote = F)
      numerical_board[player_information$movement_coordinate] = 0
      game_board[which(numerical_board==0)] = castle[which(numerical_board==0)] = '\u2800'
      #This code fixes an issue with the player temporarily being replaced with a string before inputting the next move
      game_board[player_information$player_coordinate]="\U1F93A"
    }
    else if(player_information$encountered_object=="\U1F9DE"){
      game_board[player_information$movement_coordinate] = castle[player_information$movement_coordinate]
      print(noquote(game_board[,,player_information$floor]))
      print("You encountered a genie!", qoute = F)
      print("Your attack range increased by 2 points.", qoute = F)
      player_information$player_attack_range = player_information$player_attack_range + 2
      print(paste("New attack range: ", paste0(min(player_information$player_attack_range),":",max(player_information$player_attack_range))), quote = F)
      numerical_board[player_information$movement_coordinate] = 0
      game_board[which(numerical_board==0)] = '\u2800'
      castle[which(numerical_board==0)] = '\u2800'
      game_board[player_information$player_coordinate]="\U1F93A"
      }
    else if(player_information$encountered_object=="\U1F479"){
      game_board[player_information$movement_coordinate] = castle[player_information$movement_coordinate]
      print(noquote(game_board[,,player_information$floor]))
      print("You encountered a monster!", qoute = F)
      print(paste("Your HP: ",player_information$player_health), quote = F)
      print(paste("Your attack range: ", paste0(min(player_information$player_attack_range),":",max(player_information$player_attack_range))), quote = F)
      print(paste("Monster HP: ", numerical_board[player_information$movement_coordinate]),quote = F)
      player_monster_choice = tolower(as.character(readline("Would you like to attack(a) or run(r)? ")))
      while(!(player_monster_choice %in% moves[c(4:7)])){
        player_monster_choice  = tolower(as.character(readline(paste(paste0("'",player_monster_choice ,"'"),"is not a valid choice. Would you like to attack(a) or run(r)?"))))
      }
      if(player_monster_choice == "attack" || player_monster_choice == "a"){
        print("You decided to attack",quote = F)
        while(!(player_monster_choice == "run" || player_monster_choice == "r" || player_information$player_health <=0 ||
                numerical_board[player_information$movement_coordinate] <= 0)){
          print(noquote(game_board[,,player_information$floor]))
          player_information$attack_power = sample( player_information$player_attack_range,1)
          print(paste("You dealt",player_information$attack_power, "points of damage"),quote = F)
          if(as.numeric(numerical_board[player_information$movement_coordinate]) - player_information$attack_power <= 0){
            numerical_board[player_information$movement_coordinate] = 0
            print("The monster fainted. You won!",quote = F)
            print("Your attack range increased by 2 points.", qoute = F)
            player_information$player_attack_range = player_information$player_attack_range + 2
            print(paste("New attack range: ", paste0(min(player_information$player_attack_range),":",max(player_information$player_attack_range))), quote = F)
            game_board[which(numerical_board==0)] = '\u2800'
            castle[which(numerical_board==0)] = '\u2800'
            game_board[player_information$player_coordinate]="\U1F93A"
          }
          else{
            numerical_board[player_information$movement_coordinate] =  as.numeric(numerical_board[player_information$movement_coordinate]) - player_information$attack_power
            print(paste("Monster HP: ", numerical_board[player_information$movement_coordinate]),quote = F)
            monster_attack = sample(1:5,1)
            if(player_information$player_health - monster_attack <= 0){
              print(paste("Monster dealt",monster_attack, "points of damage"),quote = F)
              print(paste("Your HP: ",player_information$player_health), quote = F)
              print("You died.", quote = F)
              try_again()
            }
            else{
              player_information$player_health = player_information$player_health - monster_attack
              print(paste("Monster dealt",monster_attack, "points of damage"),quote = F)
              print(paste("Your HP: ",player_information$player_health), quote = F)
              player_monster_choice = tolower(as.character(readline("Would you like to attack(a) or run(r)? ")))
              while(!(player_monster_choice %in% moves[c(4:7)])){
                player_monster_choice  = tolower(as.character(readline(paste(paste0("'",player_monster_choice ,"'"),"is not a valid choice. Would you like increased attack(a) or defense(d)? "))))
              }
            }
          }
        }
      }
      else{
        print("You decided to run.")
      }
    }
    else if(player_information$encountered_object=="S"){
      print(paste("You found the stars! You can now advance to floor", player_information$floor + 1,"!"), quote = F)
      player_information$floor = player_information$floor + 1
      player_information$movement_coordinate[3] = player_information$floor
      player_information$player_coordinate = player_information$movement_coordinate
    }
  }
  game_board[player_information$movement_coordinate] = castle[player_information$movement_coordinate]
  print(game_board[,,player_information$floor], quote = F)
  print("You found the exit!", quote = F)
  try_again()
}
castle_descent()




  

