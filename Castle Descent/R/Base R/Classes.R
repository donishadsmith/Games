#Script for player and zombie classes
#Zombie class contains the necessary attributes to keep track of zombie coordinates
#and methods to allow zombie to move
zombie_class = setRefClass('zombie_info', fields = list(movement_dict = 'list',
                                                        initial_coordinate = 'matrix',
                                                        current_coordinate = 'matrix',
                                                        distance_to_player= 'numeric',
                                                        floor = 'numeric'),
                           methods = list(
                             euclidean_distance = function(a,b){
                               c_squared = sqrt(sum((a - b)^2))
                               return(c_squared)
                             },
                             pathfinding = function(castle_data,player){
                               move_or_dont = sample(c(0,1),size = 1, prob = c(0.70,0.30))
                               if(move_or_dont == 1){
                                 #Using the current coordinate, get list of eight possible coordinates that allows movement in eight directions
                                 movement_vector = c()
                                 for(movement in movement_dict){
                                   possible_coordinate = current_coordinate + movement
                                   #If coordinate is empty or contains the player, it is a possible coordinate to move to
                                   if(castle_data$castle[possible_coordinate] == '\u2800'| castle_data$castle[possible_coordinate] == '\U1F93A'){
                                     movement_vector = c(movement_vector, list(possible_coordinate))
                                   }                            
                                 }
                                 #Calculate total cost = distance cost + manhattan distance from player
                                 #Diagonals will cost more farthest from the initial coordinate will cost more but ones closest to the initial 
                                 #position will cost less
                                 movement_cost = c()
                                 for(possible_coordinate in movement_vector){
                                   #Coordinates with lower manhattan distance from initial will have lower cost
                                   cost = sum(abs(initial_coordinate - current_coordinate))
                                   c_squared = euclidean_distance(possible_coordinate,player$coordinate)
                                   movement_cost = c(movement_cost, c_squared + cost)
                                 }
                                 
                                 #Erase zombie from old location and add to new location
                                 castle_data$castle[which(castle_data$castle=='\U1F9DF', arr.ind = T)] = '\u2800'
                                 current_coordinate <<-  movement_vector[[which(movement_cost == min(movement_cost))[1]]]
                                 castle_data$castle[current_coordinate] = '\U1F9DF'
                                 #Calculate new distance from zombie to player
                                 distance_to_player <<- euclidean_distance(current_coordinate,player$coordinate)
                                 if(distance_to_player == 0){
                                   cat(rep("\n", 50))
                                   print(castle_data$castle[,,player$floor], quote = F)
                                   print('You were eaten by the zombie' , quote = F)
                                 }
                                 pathfinder_output = c(castle_data,player)
                                 return(pathfinder_output)
                               }
                               else{
                                 pathfinder_output = c(castle_data,player)
                                 return(pathfinder_output)
                               }
                               
                             },
                             #Allow zombie to move ot new floor with player
                             move_to_new_floor_event = function(castle_data,player){
                               #Find the coordinate that allows the zombie to be at the greatest Euclidean distance from the
                               #player
                               movable_spaces = which(castle_data$castle[,,player$floor]=='\u2800', arr.ind = T)
                               c_squared = c()
                               for(row in 1:nrow(movable_spaces)){
                                 c_squared = euclidean_distance(movable_spaces[row,],player$coordinate[1:2])
                               }
                               coord = movable_spaces[which(c_squared == max(c_squared)),]
                               #Erase old zombie location
                               castle_data$castle[which(castle_data$castle=='\U1F9DF', arr.ind = T)] = '\u2800'
                               #Add new zombie location and update initial coordinate,current coordinate, and distance
                               castle_data$castle[,,player$floor][coord[1],coord[2]] = '\U1F9DF'
                               initial_coordinate <<- which(castle_data$castle=='\U1F9DF', arr.ind = T)
                               current_coordinate <<- which(castle_data$castle=='\U1F9DF', arr.ind = T)
                               distance_to_player <<- euclidean_distance(current_coordinate,player$coordinate)
                               move_to_new_floor_event_output = c(castle_data,player)
                               return(move_to_new_floor_event_output)
                             }))

#Player class keeps importint information about player such as health and current posisiton
player_class =  setRefClass('player_info', fields = list(player_hp='numeric', coordinate='matrix',
                                                         movement_dict = 'list',
                                                         encountered_object='character',
                                                         movement_coordinate = 'matrix',
                                                         attack_range='numeric', 
                                                         attack_power = 'numeric',
                                                         floor = 'numeric',
                                                         castle_dataframe_row = 'numeric'))
