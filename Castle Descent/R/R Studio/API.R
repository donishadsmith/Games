#Fucntions for the API
#Use RStudio's API to dynamically read R's Terminal
read_console_try_again_action = function(){
  print('Want to play again? Yes (y) or No (n)?', quote = F)
  while (rstudioapi::isAvailable()) {
    player_retry <<- tolower(rstudioapi::getConsoleEditorContext()$contents)
    if(player_retry %in% c('yes','y')){
      rstudioapi::sendToConsole("", execute = F)
      castle_descent()
    }
    else if (player_retry %in% c('no','n')){
      print("Thank you for playing Castle Descent!", quote = F)
      rstudioapi::sendToConsole("", execute = F)
      return(noquote(""))
    }
    #Needed so that player can escape game by pressing ctrl + c.
    #It suspends execution of R expressions every n seconds
    rstudioapi::sendToConsole("", execute = F)
    Sys.sleep(0.1)
  }
}


read_console_player_movement_action = function() {
  print('w (up), a (left), s (down), or, d (right). You can quit by pressing ctrl + c.', quote = F)
  while (rstudioapi::isAvailable()) {
    player_action <<- tolower(rstudioapi::getConsoleEditorContext()$contents)
    
    if (player_action %in% c('w','a','s','d')) {
      rstudioapi::sendToConsole("", execute = F)
      return(noquote(""))
    }
    rstudioapi::sendToConsole("", execute = F)
    Sys.sleep(0.1)
  }
}

read_console_player_monster_action= function() {
  print('attack(a) or run(r). You can quit by pressing ctrl + c', quote = F)
  while (rstudioapi::isAvailable()) {
    player_action <<- tolower(rstudioapi::getConsoleEditorContext()$contents)
    
    if (player_action %in% c('attack','a','r','run')) {
      rstudioapi::sendToConsole("", execute = F)
      return(noquote(""))
    }
    
    rstudioapi::sendToConsole("", execute = F)
    Sys.sleep(0.1)
  }
}
