##################################################################################
#Code for playing single-deck blackjack in R
#Code also allows you to play infinity since there is a function for trying again.
##################################################################################

two_player_blackjack = function(){
  #Characters in R are printed with quotations. Setting "quote" to false removes these quotations. It's a stylistic choice.
  #\n prints the statement it preceeds to a new line.
  print("Welcome to BlackJack in R!\nYou'll be playing a two-player game of 21 using a single-deck of cards.",quote = F)
  #readline is a function that prints a statement to the R console and allows to user to input/type in a response.
  #This is a two-player game of blackjack, so the users should decide who will be player 1 and 2 prior to beginning
  readline("Please select who will be Player 1 and Player 2 then press any key to continue.")
  # print("",quote = F) is used to add a single space/one empty line. It is equivalent to pressent enter/return twice.
  print("",quote = F)
  # valid_options is a variable that contains all the input options relevant to this game. This is what players will be able to input in the console.
  valid_options = c("hit", "stand", "h", "s", 'yes', 'no', 'y', 'n')
  #Keep track of number of games played
  game_iteration <<- 0
  #casino_functions is an R class that contains attributes (variables in the class) and methods (functions in the class). Classes allows one to assign methods and objects to a new variable.
  # This is significant because I can create two objects/instances "player_1" and "player_2" that can store its own information regarding the cards in the players hand, their value, etc.
  #fields are all the objects that this class contains and methods are the functions available in this class
  #initial_hand is the same as hand. The difference is that the first card is "hidden" by replacing it with "_", which is handled in a special way in the summing function.
  #"_" equals zero but a backup hand is also needed so that the original card is retained. "_" replaces the card for the initial hand but the actual hand is always kept in the "hand" variable.
  #position is a way to assign certain positions to player to activate certain codes. There is a randomizer in this game and the person going second
  #needs to have their first card hidden. 
  #unlisted_hand is used in the function that counts cards. Unicode symbols are used in this game, so there is a code to split the number from the unicode symbol
  #to allow for summation
  #Unfortuantely, the unicode for the actual cards are two small, only the symbols (heart, club, etc) are used.
  # hand is just the cards in hand, win and loss is for keeping track of each players wins and losses. There is a scoreboard function in this game.
  #hand_value is just the summation of the player's hand
  #player is different from position. A player's position may change (1 or 2 based on who goes first) but the player is always the same (1 for player 1 and 2 for player 2)
  #player is used for substitution in certain prompts.
  
  #Note <<- is global assignment, it equals "global" in python. Local assignment allows variable modification inside a function. 
  #The assignment operator for local assignment is <- or = . Global assignment allows a variable in R's global environment to be modified 
  # For instance, if I have a variable x = 1 outside a function and inside a function, I have x = x + 1 or x = 2, there will be an error
  #This is because x is not created within the function and x = x + 1 implies that there is a local variable named x
  #If I do, x <<- x + 1 instead or x <<- 2, the global x variable now equals 2
  casino_functions = setRefClass('casino', fields = list(hand = "character", hand_value = "numeric",
                                                         position = "numeric", initial_hand = "character",
                                                         unlisted_hand = "character", win = "numeric", loss = "numeric", 
                                                         player = "numeric"),
                                 methods = list(
                                   #random_card_generator() immediately uses other functions- the hidden_hand function and sum_cards function
                                   #so, only random_card_generator can be used.
                                   #Anytime this function is used, the initial card of the second player is hidden and the value of the cards are summed.
                                   #Essentially, while there are three functions, only one function needs to be called
                                   random_cards_generator = function(){
                                     #sample is a function in base R that allows for random selection with or without replacement. It defaults to without replacement.
                                     #The purpose for the next line of code is to shuffle the deck anytime the random_cards_generator function is called.
                                     #length is a function that informs you about the length of a vector. The cards in this game will be discarded
                                     #since it will replicate the probabilities of a real, single deck as the game progresses. So, the length must be updated for each iteration of 
                                     # random_cards_generator to avoid errors and ensure that all the cards are being selected and being put in a random new position.
                                     deck <<- sample(deck,length(deck))
                                     #Reset hand; player_1 and player_2 objects are created in the beginning of the game
                                     #Less variables to create during resetting game by just "emptying" the previous game's hand
                                     #The deck is recreated during each new game. So, this code activates once for each new game. 
                                     if(length(deck) == 52){
                                       hand <<- ""
                                       initial_hand <<- ""
                                     }
                                     #By stating that the length must be greater than 48, the function automatically selects two random cards
                                     #Basically used to draw two cards for both players when called by both player variables for the first time.
                                     if(length(deck) >48){
                                       number_of_cards_needed = 2
                                       random_cards <<- sample(deck, number_of_cards_needed, replace = F)
                                       #Ensuring no replicate cards by eliminating chosen cards. This will replicate the probability of a real single-deck game
                                       deck <<- deck[!(deck %in% random_cards)]
                                       hand <<- random_cards
                                     }
                                     #When the previous condition is not met, this function will give one random card whenever it is called
                                     else{
                                       number_of_cards_needed = 1
                                       random_cards <- sample(deck, number_of_cards_needed, replace = F)
                                       deck <<- deck[!(deck %in% random_cards)]
                                       hand <<- c(hand, random_cards)
                                     }
                                     #Generate hidden hand for the player going second. This game has a player randomizing fucntion. This code ensures that the 
                                     #player going second will always have their first card hidden. 
                                     if(position == 2){
                                       hidden_hand()
                                     }
                                     #Immediately sum hands.
                                     #Initial hand variable belongs to second player (not player_2, the player that goes second based on the randomizer)
                                     #Intitial hand is counted differently, only the value of the second card is shown
                                     #iteration is a global variable that is assigned to 0 at the beginning of the game
                                     if(iteration == 0 & position == 2){
                                       sum_cards(initial_hand)
                                     }
                                     else{
                                       sum_cards(hand)
                                     }
                                   },
                                   #Part that replaces the first hand to "_" for the player whose position is 2
                                   hidden_hand = function(){
                                     initial_hand <<- hand
                                     initial_hand[1]  <<- "_"
                                     
                                   },
                                   sum_cards = function(hand){
                                     #Initial total sum will be 0. Need to initialize variable with a number for summation
                                     total_sum = 0
                                     #for function to count each card in the hand
                                     for(card in hand){
                                       #split each character in a string "1♦" becomes "1" "♦", the first position will always be a number
                                       #strsplit allows each character (in this case, the number and sumbol) to become its own string. 
                                       #strsplit always ask for a mode output type. NULL is used so that strsplit assigns string as character/the appropriate mode
                                       #Even though, strsplit splits each character into its own string, the output is a list of length one
                                       #thus, unlist in order to return a vector with the approriate length
                                       #For instance, if x ="THE", strisplit wil split "THE" into "T" "H" "E"
                                       #length of x will = 1. This is because strisplit turns x into a recursive list, a list within a list.
                                       #So, is I need to select "T" in x, I need to do it by x[[1]][1]
                                       #[[1]][1] is saying "select position 1 in the second layer of the list, then in that specific sublist, select the first position"
                                       #To avoid that, unlist is used, I can use x[1] to call "T"
                                       unlisted_hand <<- unlist(strsplit(card, NULL))
                                       if(unlisted_hand[1] == "A"){
                                         if(total_sum + 11 > 21){
                                           number = 1
                                         }
                                         else{
                                           number = 11
                                         }
                                       }
                                       #Ensure the hidden card is assigned a value of zero
                                       else if(card == "_"){
                                         number = 0
                                       }
                                       # length(hand) == 3 is for 10 of diamonds, club, heart, and clover. 
                                       #Unlisting 10 + it's symbol with strsplit() results in "1" "0" "symbol", consequently, it is 
                                       else if(unlisted_hand[1] %in% list("J", "K", "Q") || length(unlisted_hand) == 3 ){
                                         number = 10
                                       }
                                       #Tested and works. If Ace = 11 exceeds 21, Ace will be equal to 1 instead
                                       else{
                                         number = as.numeric(unlisted_hand[1])
                                       }  
                                       total_sum = sum(cbind(total_sum,number))
                                     }
                                     hand_value <<- total_sum
                                   } ))
  #See if previous score files exists to continue previous streaks. The try_again function lets players save the scoreboard as a text file if they want to 
  if(file.exists("player_scores_for_two_player_blackjack_in_R.txt")){
    print("A save file of the previous session has been found", quote=F)
    choice = readline("Would you like to continue the previous session or begin a new game session? 'Yes[y]' or 'No[n].")
    choice = tolower(as.character(choice))
    while(!(choice %in% valid_options[5:8])){
      choice = readline(paste(paste0("'",choice, "'"), "is not a valid response. 'Yes[y]' or 'No[n]'."))
      choice = tolower(as.character(choice))
    }
    if(choice == "yes" | choice == "y"){
      scores <<- read.csv("player_scores_for_two_player_blackjack_in_R.txt", header = T, row.names =1 )
      player_1 <<- casino_functions(win = scores["player_1", "win"], loss = scores["player_1", "loss"])
      player_2 <<-  casino_functions(win = scores["player_2", "win"], loss = scores["player_2", "loss"])
     game_iteration <<- scores[1,"game_iteration"]
    }
    else{
      player_1 = casino_functions(win = 0, loss = 0)
      player_2 = casino_functions(win = 0, loss = 0)
  }
  }
  #If they don't want to continue the streak or the text file doesn't exist, then win and loss is assigned 0.
  #Will possibly add a part that ask players if they want to continue their previous streak instead of automatically using the previous scoreboard
  #Possibly, a player may want to save a previous streak but want to play a new game with another competitor 
  #Need to input code so that players can select specific files.
  player_1$player = 1
  player_2$player = 2
  #Function to randomize player, players choose which player they want to be, when resetting, a new player will be selected to go first.
  player_randomiser = function() {
    players = sample(c(1,2), 1)
    if(players == 1){
      print("______________________________",quote = F)
      print("Player 1 goes first.",quote = F)
      print("______________________________",quote = F)
      #Assigning positions so that random_card_generator knows which player needs to have their first card hidden
      player_1$position = 1
      player_2$position = 2
      #Calling the first two cards for each player
      player_1$random_cards_generator()
      player_2$random_cards_generator()
      #output is another fucntion that keeps track of the current status of the game
      output()
      player_function(player_1, player_2)
      
    }
    else{
      print("______________________________",quote = F)
      print("Player 2 goes first.",quote = F)
      print("______________________________",quote = F)
      player_1$position = 2
      player_2$position = 1
      player_2$random_cards_generator()
      player_1$random_cards_generator()
      output()
      player_function(player_2, player_1)
    }
  }
  #Function to try again. If player says yes, the game gets repeated
  try_again = function(){
    #readline allows for input in the R console. Player does not need to input their response into the function itself
    choice =  readline("Would you like to play again? Please choose 'Yes(y)' or 'No(n)'. ")
    #This part makes sure that the mode of the input is a character and makes everything lower case. So if player puts in "YES", "YeS", or any configuration
    #It will be in lowercase and recognized as long as it is spelled correctly
    choice = tolower(as.character(choice))
    while(!(choice %in% valid_options[5:8])){
      #paste is a function that literally paste strings together. So, this code repeats the players input.
      #If player inputs "ye" it will read "'ye' is not a valid option. Please choose 'Yes(y)' or 'No(n)'."
      #paste0 creates the 'ye' part in single quotes. paste0 creates zero spaces between characters while paste allows for spaces
      choice = readline(paste(paste0("'",choice,"'"), "is not a valid option. Please choose 'Yes(y)' or 'No(n)'. "))
      choice = tolower(as.character(choice))
    }
    if(choice %in% valid_options[5:8]){
      if(choice == "yes" || choice == "y"){
        game()
      }
      else if(choice == "no"|| choice == "n"){
        choice = tolower(readline("Would you like the current scoreboard to be saved as a text file to continue streaks for the next game? 'Yes[y]' or 'No[n]'"))
        while(!(choice %in% valid_options[5:8])){
          choice =  readline(paste(paste0("'",choice,"'"), "is not a valid option. Please choose 'Yes(y)' or 'No(n)'. "))
          choice = tolower(as.character(choice))
        }
        if(choice == "yes" || choice == "y"){
          #Let player choose to save current game states in text file
          scores <<- data.frame(win = c(player_1$win, player_2$win),loss = c(player_1$loss, player_2$loss), game_iteration = c(game_iteration, NA), row.names = c("player_1", "player_2"))
          write.csv(scores, file = "player_scores_for_two_player_blackjack_in_R.txt")
          print("The text file has been saved in the current working directory",quote = F)
        }
        print("Thanks for playing! Type 'two_player_blackjack()' with no quotations in the console if you want to play again.",quote = F)
        break
      }
    }
  }
  #Output information
  output = function () {
    print(paste("Player 2's hand:"),quote = F)
    #As mentioned before, there is a randomizer, the if statement prints the initial_hand of the player if the position for player_2 is 2
    # and the global iteration variable = 0
    if(player_2$position == 2 & iteration == 0){
      print(player_2$initial_hand,quote = F)
      print(paste("Player 2's sum:",player_2$hand_value),quote = F)
    }
    else{
      print(player_2$hand,quote = F)
      # Here player_2$sum_cards(player_2$hand) is used instead of player_2$hand_value
      #In the above code, player_2$hand reveals the hidden card; however, player_2$hand_value doesn't immediatially update.
      #In this game, when the first player stands, the game status updates to reveal the second players hand so that the second player can decide to
      #hit or stnad. During this period,player_2$hand_value still reflects the value of the hidden hand (0 + the value of the second card)
      #player_2$sum_cards(player_2$hand) is used to update the value of the hand to reflect the actual value 
      # I can't use player_2$random_cards_generator() because that code draws a card and sums the value. 
      #New cards shouldn't be selected unless the player wants to.
      print(paste("Player 2's sum:",player_2$sum_cards(player_2$hand)),quote = F)
    }
    print("",quote = F)
    print("",quote = F)
    print(paste("Player 1's hand:"),quote = F)
    if(player_1$position == 2 & iteration == 0){
      print(player_1$initial_hand,quote = F)
      print(paste("Player 1's sum:",player_1$hand_value),quote = F)
    }
    else{
      print(player_1$hand,quote = F)
      print(paste("Player 1's sum:",player_1$sum_cards(player_1$hand)),quote = F)
    }
  }
  #Output scoreboard at the end of each game
  score_tracker= function(information){
    if(information[2] == "win"){
      if(information[1] == 1){
        player_1$win <<- player_1$win + 1
        player_2$loss <<- player_2$loss + 1
      }
      else{
        player_2$win <<- player_2$win + 1
        player_1$loss <<- player_1$loss + 1
      }
    }
    else if(information[2] == "loss"){
      if(information[1] == 1){
        player_1$loss <<- player_1$loss + 1
        player_2$win <<- player_2$win + 1
      }
      else{
        player_2$loss <<- player_2$loss + 1
        player_1$win <<- player_1$win + 1
      }
    }
    game_iteration <<- game_iteration + 1
    print("______________________________",quote = F)
    print("",quote = F)
    print("Current scoreboard:",quote = F)
    print("______________________________",quote = F)
    print(paste("Total number of games played:",game_iteration),quote = F)
    print("",quote = F)
    print(paste("Player 1 wins:",  player_1$win),quote = F)
    print(paste("Player 1 losses:", player_1$loss),quote = F)
    print("",quote = F)
    print(paste("Player 2 wins:", player_2$win),quote = F)
    print(paste("Player 2 losses:", player_2$loss),quote = F)
  }
  #I tried to make the player function as compact as possible. Instead of creating one set of code for player 1 and another for player 2, they utilize the same code
  #You can do this by capitalizing on assigning variables to your function and using iterations.
  #For instance, if player 2 is the first player to go, then current_player = player_2 and other_player = player_1
  # When iteration = 0, only code that states that use iteration = 0 will be ran.
  #When player_2 (the current_player) is finished (by standing and not exceeding 21 as the game dictates), then 1 will be assigned to iteration.
  #After, you can do player_function(other_player,current_player), this means that other_player = the previous current player (player_2) and 
  #player_1 is now the new current_player. This is a way for player_1 and player_2 to use the same code without creating too many variables.
  #Improvements to this function and the entire script can still be made though.
  player_function = function(current_player, other_player){
    #There may be an instance where the first player automatically hits 21, I decided to immediately hand it over to the second player.
    if(current_player$hand_value == 21){
      if(iteration == 0){
        print("______________________________",quote = F)
        print("",quote = F)
        print(paste("Player", current_player$player, "hit 21. Now it is Player", paste0(other_player$player, "'s"), "turn."),quote = F)
        print(noquote("______________________________"))
        #Part where iteration is now "1". I typically code this way but tried to make this code as clear as possible iteration <<- 1 would have worked just as well.
        iteration <<- iteration + 1
        output()
        #Now, the players are switched
        player_function(other_player, current_player)
      }
    }
    
    player_choice = readline("Hit(h) or stand(s)? ")
    player_choice = tolower(as.character(player_choice))
    #As long has the player input isn't in valid_options, they will be asked to to provide a valid response
    while(!(player_choice %in% valid_options[1:4])){
      player_choice = readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). "))
      player_choice = tolower(as.character(player_choice))
    }
    #Next code loops as long as player uses hit
    while(player_choice == "hit" || player_choice == "h"){
      print("______________________________",quote = F)
      print("",quote = F)
      print(paste("Player",current_player$player , "decided to hit."),quote = F)
      print("______________________________",quote = F)
      current_player$random_cards_generator()
      output()
      #If player's hand exceeds 21. Player loses.
      if(current_player$hand_value > 21){
        print("",quote = F)
        print(paste("Bust! Player" , current_player$player, "loses!"), quote=F)
        #A way to provide the necessary information to the score_tracker function.
        information = c(current_player$player, "loss")
        score_tracker(information)
        try_again()
      }
      else if(iteration == 0){
        if(current_player$hand_value == 21){
          iteration <<- iteration + 1
          print("______________________________",quote = F)
          print(paste("Player", current_player$player, "hit 21. It is now Player" ,paste0(current_player$player, "'s"), "turn."),quote = F)
          print("______________________________",quote = F)
          output()
          player_function(other_player, current_player)
        }
      }
      #Automatically assess win, loss, and tie if iteration is 1 and both 
      #I could have used else if(iteration == 1 & current_player$hand_value == 21) instead. 
      #That would have allowed me to exclude the second else if and just use else. The code does the same thing and it doesn't cause major performance issues
      else if(iteration == 1){
        if(current_player$hand_value == 21 & !(other_player$hand_value == 21)){
          print("",quote = F)
          print(paste("Player", current_player$player, "got to 21. Player" ,current_player$player, "wins!"), quote=F)
          information = c(current_player$player, "win")
          score_tracker(information)
          try_again()
        }
        else if(current_player$hand_value == 21 & other_player$hand_value == 21){
          print("",quote = F)
          print("Tie!",quote = F)
          try_again()
        }
      }
      player_choice = readline("Hit(h) or stand(s)? ")
      player_choice = tolower(as.character(player_choice))
      while(!(player_choice %in% valid_options[1:4])){
        player_choice = readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). "))
        player_choice = tolower(as.character(player_choice))
      }
    }
    #If the first current_player hits stand and iteration is 0, then this code is activated to reveal the other players hidden card and allow the other player 
    # to hit or stand
    if(player_choice == "stand" & iteration == 0 || player_choice == "s" & iteration == 0){
      print("______________________________",quote = F)
      print("",quote = F)
      print(paste("Player", current_player$player, "decided to stand. Now it is Player", paste0(other_player$player, "'s"), "turn."))
      print("______________________________",quote = F)
      #Reveal dealer's hidden card. If dealer's hand exceeds 21, player wins.
      iteration <<- iteration + 1
      output()
      player_function(other_player, current_player)
    }
    #If the previous block isn't met, the proceeding else statement is activated. So, when the player the goes second hit stand, the game evaluates
    #both players hand and begins assigning wins and losses
    else{
      if(current_player$hand_value > other_player$hand_value){
        print("",quote = F)
        print(paste("Player",  current_player$player, "is closer to 21. Player" , current_player$player, "wins!"), quote=F)
        information = c(current_player$player, "win")
        score_tracker(information)
        try_again()
      }
      else if(current_player$hand_value < other_player$hand_value){
        print("",quote = F)
        print(paste("Player", other_player$player, "is closer to 21. Player" ,other_player$player, "wins!"), quote=F)
        information = c(other_player$player, "win")
        score_tracker(information)
        try_again()
      }
      else{
        print("",quote = F)
        print("Tie!",quote = F)
        try_again()
      }
    }
  }
  #Actual blackjack game
  game = function(){
    #If the player wants to try again, the deck needs to be created again because the cards are discarded as the game progresses.
    #The try_again function calls the game function
    deck <<-  c(paste0(c(2:10, "J", "Q", "K", "A"),"♦"),paste0(c(2:10, "J", "Q", "K", "A"),"♠"),
                paste0(c(2:10, "J", "Q", "K", "A"),"♥"), paste0(c(2:10, "J", "Q", "K", "A"),"✤")
    )
    #Same logic, iteration needs to be set back to zero.
    iteration <<-0
    #Randomization will always be done for each reset. The person that goes second typically has the advantage since they don't have to play with 
    # the uncertainty of the other players hand. This is a luck based game but randomising allows a bit of fairness so that one player isn't always put in the 
    #position of not knowing the other players cards for every new game
    
    player_randomiser()
  }
  game()
}
# This entire game is a function. So, the entire function and sub-functions are created first. The outermost fucntions needs to be called to start the game.
#The second outermost function is game(), which creates the deck and the global iteration variable and calls player_randomiser(), the third outermost function is player_randomiser() to randomize
#players and call output() and player_function(), player_function() is the fourth outermost function that actually contains the blackjack game and calls the output() function
#to show the game_status, score_tracker() to keep track of scores, and try_again to replay to game. 
two_player_blackjack()


