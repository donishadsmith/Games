#Function to create castle and castle dataframe containing information about castle
castle_create = function(){
  castle_dataframe = data.frame('x' = NA, 'y' = NA, 'z' = NA, 'object' = NA,
                                'hp' = NA)
  # Area of the castle floor will equal castle_width* castle_length
  # x,y are odd numbers for aesthetic reasons; those reasons involve the distance of the objects from each other
  castle_x_length = sample(seq(9,13, 2),1)
  castle_y_length = sample(seq(9,13, 2),1)
  #Length of 3rd/z-dimension; 3D array
  castle_z_length = sample(3:6, 1)
  
  castle = array( '\u2800',dim = c(castle_x_length,castle_y_length,castle_z_length))
  #Spawn walls;These walls be used to ensure players don't go out of bounds in the array - basically boundary detection
  castle[1:nrow(castle),c(1,ncol(castle)),] ='\U2591'
  castle[c(1,nrow(castle)),1:ncol(castle),]='\U2591'
  
  #Isolating spaces for objects; objects will be evenly spaced
  object_spaces_x = seq(2,(castle_x_length-1), 2)
  object_spaces_y = seq(2,(castle_y_length-1), 2)
  #Area space in the array with one is where all objects will be allowed to spawn
  castle[object_spaces_x,object_spaces_y,] = 1
  
  
  #One is the top of the castle; this is where the player spawns
  castle[,,1][sample(which(castle[,,1]=='\u2800'),1)] = '\U1F93A'
  #Get player coordinate
  player_coord = which(castle=='\U1F93A', arr.ind = T)[1:2]
  #Create matrix of free spaces
  movable_spaces = which(castle[,,1]=='\u2800', arr.ind = T)
  
  #Spawn zombie at farthest location from player - Euclidean distance
  c_squared = c()
  for(row in 1:nrow(movable_spaces)){
    c_squared = c(c_squared,sqrt(sum((player_coord - movable_spaces[row,])^2)))
  }
  coord = movable_spaces[which(c_squared == max(c_squared))[1],]
  castle[,,1][coord[1],coord[2]] = '\U1F9DF'
  
  #Spawn stairs ; DS = Down staircase
  for(floor in 1:(castle_z_length-1)){
    castle[,,floor][sample(which(castle[,,floor]=='1'),1)] = 'DS'
    castle[,,floor + 1][which(castle[,,floor]=='DS')] = 'AS'
  }
  
  #Unlike the = sign <- assignment operator allows you to declare variables within functions
  castle_dataframe[nrow(castle_dataframe):nrow(which(castle == 'DS', arr.ind = T)),] = NA
  castle_dataframe[1:(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle=='DS', arr.ind = T), 
                                                                                         rep('DS',nrow(which(castle=='DS', arr.ind = T))),
                                                                                         rep(1,nrow(which(castle=='DS', arr.ind = T))))
  
  
  
  
  #AS stands for above staircase; variable is used as a filler space to keep the cell corresponding to the the above 'DS' object occupied
  castle_dataframe[(nrow(castle_dataframe) + 1):(nrow(castle_dataframe) + nrow(which(castle == 'AS', arr.ind = T))),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle=='AS', arr.ind = T), 
                                                                                                                   rep('AS',nrow(which(castle=='AS', arr.ind = T))),
                                                                                                                   rep(0,nrow(which(castle=='AS', arr.ind = T))))
  
  
  
  
  
  #Adding new rows and coordinates to dataframe
  #Bottom of the castle is where the exit spawns
  castle[,,castle_z_length][sample(which(castle[,,castle_z_length]=='1'),1)] = '\U2395'
  
  #Coordinates of the exit added to dataframe
  castle_dataframe[(nrow(castle_dataframe) + 1):(castle_dataframe_rows <- nrow(castle_dataframe) + 1),1:5] = data.frame(which(castle=='\U2395', arr.ind = T), '\U2395', rep(-1,1))
  
  
  #Spawning monster objects at any location that contains '1'
  castle[,,1:castle_z_length][which(castle == '1')] = '\U1F479'
  
  #Proportion of the empty spaces that will be monsters, genies, or fairies
  fairy_proportion = round(length(which(castle == '\U1F479'))*0.025,0)
  genie_proportion = round(length(which(castle == '\U1F479'))*0.025,0)
  
  #Adding same number of fairies and genies to each floor
  #This allows each floor to have the same proportion of fairies, monsters, and genies
  #simply adding fairies and genies without respect to each floor may result in an imbalance.
  #the majority of fairies and genies may be randomly placed at the top floors, bottom floors, or specfic floors.
  for(floor in 1:castle_z_length){
    castle[,,floor][sample(which(castle[,,floor] == '\U1F479'),fairy_proportion)] = '\U1F9DA'
    castle[,,floor][sample(which(castle[,,floor] == '\U1F479'),genie_proportion)] = '\U1F9DE'
  }
  
  #Adding information to dataframe
  castle_dataframe[(nrow(castle_dataframe) + 1):(nrow(castle_dataframe) + nrow(which(castle == '\U1F479', arr.ind = T))),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:4] = data.frame(which(castle == '\U1F479', arr.ind = T), rep('\U1F479', nrow(which(castle =='\U1F479', arr.ind = T))))
  
  #Adding hp for each each monster to dataframe
  #Intial hp ranges from 5:10
  base_hp_vector = 5:10
  for(floor in 1:castle_z_length){
    length = nrow(castle_dataframe[castle_dataframe$z == floor & castle_dataframe$object == '\U1F479',])
    castle_dataframe[castle_dataframe$z == floor & castle_dataframe$object == '\U1F479','hp'] = sample(base_hp_vector,length, replace = T)
    #Monsters on each floor will receive a plus 10 hp boost
    base_hp_vector = base_hp_vector + 10
  }
  
  #Adding fairy and genie coordinate information to the dataframe
  castle_dataframe[nrow(castle_dataframe) + nrow(which(castle == '\U1F9DA', arr.ind = T)),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle == '\U1F9DA', arr.ind = T), rep('\U1F9DA', nrow(which(castle =='\U1F9DA', arr.ind = T))),
                                                                                                                   rep(1,nrow(which(castle=='\U1F9DA', arr.ind = T))))
  
  castle_dataframe[nrow(castle_dataframe) + nrow(which(castle == '\U1F9DE', arr.ind = T)),] = NA
  castle_dataframe[(castle_dataframe_rows + 1):(castle_dataframe_rows <- nrow(castle_dataframe)),1:5] = data.frame(which(castle == '\U1F9DE', arr.ind = T), rep('\U1F9DE', nrow(which(castle =='\U1F9DE', arr.ind = T))),
                                                                                                                   rep(1,nrow(which(castle=='\U1F9DE', arr.ind = T))))
  
  #Spawning doors at coordinates that are objects
  for(coordinate in 1:nrow(castle_dataframe)){
    x = unlist(castle_dataframe[coordinate,1:3])
    castle[x[1],x[2],x[3]] = '\U1F6AA'
  }
  #Return information
  castle_and_dataframe = list('castle' = castle, 'dataframe' = castle_dataframe)
  return(castle_and_dataframe)
}

