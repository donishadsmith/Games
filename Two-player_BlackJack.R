##################################################################################
#Code for playing single-deck blackjack in R
#Code also allows you to play infinity since there is a function for trying again.
##################################################################################

two_player_blackjack = function(){
  #Characters in R are printed with quotations, no quotes turns a character into a noquote object so that
  #characters are free from quotations. It's a stylistic choice
  print("Welcome to BlackJack in R!\nYou'll be playing a two-player game of 21 using a single-deck of cards.",quote = F)
  noquote(readline("Please select who will be Player 1 and Player 2 then press any key to continue."))
  print("",quote = F)
  #All the input options relevant to this game. Player will be able to input response in the R console
  valid_options = c("hit", "stand", "h", "s", 'yes', 'no', 'y', 'n')
  casino_functions = setRefClass('casino', fields = list(hand = "character", hand_value = "numeric",
                                                         position = "numeric", initial_hand = "character",
                                                         unlisted_hand = "character", win = "numeric", loss = "numeric", 
                                                         player = "numeric"),
                                 methods = list(#Generator allows for the deck to be changed globally (outside of the function)
                                   #random_card_generator() sends random cards to other the hidden_hand function and sum_cards function
                                   #Anytime this function is used, the initial card of the second player is hidden and the value of the cards are summed.
                                   #Essentially, while there are three functions, only one function needs to be called
                                   random_cards_generator = function(){
                                     #Always shuffle before selecting card
                                     deck <<- sample(deck,length(deck))
                                     #Reset hand; player_1 and player_2 objects are created in the outermost function the object and cannot be reset if player resets the game as opposed to the function.
                                     #The try_again() function just calls the game() function again.
                                     #Less variables to create during resetting game
                                     if(length(deck) == 52){
                                       hand <<- ""
                                       initial_hand <<- ""
                                     }
                                     #Initial two cards
                                     if(length(deck) >48){
                                       number_of_cards_needed = 2
                                       random_cards <<- sample(deck, number_of_cards_needed, replace = F)
                                       #Ensuring no replicate cards by eliminating chosen cards. This will replicate the probability of a real single-deck game
                                       deck <<- deck[!(deck %in% random_cards)]
                                       hand <<- random_cards
                                     }
                                     #Only 1 card will be taken
                                     else{
                                       number_of_cards_needed = 1
                                       random_cards <- sample(deck, number_of_cards_needed, replace = F)
                                       deck <<- deck[!(deck %in% random_cards)]
                                       hand <<- c(hand, random_cards)
                                     }
                                     #Generate hidden hand for the player going second
                                     if(position == 2){
                                       hidden_hand()
                                     }
                                     #Immediately sum hands.
                                     #Initial hand variable belongs to second player
                                     if(iteration == 0 & position == 2){
                                       sum_cards(initial_hand)
                                     }
                                     else{
                                       sum_cards(hand)
                                     }
                                   },
                                   hidden_hand = function(){
                                     initial_hand <<- hand
                                     initial_hand[1]  <<- "_"
                                     
                                   },
                                   sum_cards = function(hand){
                                     #Initial total sum will be 0. Need to initialize variable with a number for summation
                                     total_sum = 0
                                     for(card in hand){
                                       #split each character in a string "1♦" becomes "1" "♦", the first position will always be a number
                                       #except for the ace
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
  #See if previous score files exists to continue previous streaks
  if(file.exists("player_scores_for_two_player_blackjack_in_R.txt")){
    scores <<- read.csv("player_scores_for_two_player_blackjack_in_R.txt", header = T, row.names =1 )
    player_1 = casino_functions(win = scores["player_1", "win"], loss = scores["player_1", "loss"])
    player_2 = casino_functions(win = scores["player_2", "win"], loss = scores["player_2", "loss"])
    
  }
  else{
    player_1 = casino_functions(win = 0, loss = 0)
    player_2 = casino_functions(win = 0, loss = 0)
  }
  player_1$player = 1
  player_2$player = 2
  #Function to randomize player, players choose which player they want to be, when resetting, a new player will be selected to go first.
  player_randomiser = function() {
    players = sample(c(1,2), 1)
    if(players == 1){
      print("______________________________",quote = F)
      print("Player 1 goes first.",quote = F)
      print("______________________________",quote = F)
      player_1$position = 1
      player_2$position = 2
      player_1$random_cards_generator()
      player_2$random_cards_generator()
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
    #Typically, a funciton needs an input when activated single-deck
    choice =  noquote(readline("Would you like to play again? Please choose 'Yes(y)' or 'No(n)'. "))
    choice = tolower(as.character(choice))
    while(!(choice %in% valid_options[5:8])){
      choice =  noquote(readline(paste(choice, "is not a valid option. Please choose 'Yes(y)' or 'No(n)'. ")))
      choice = tolower(as.character(choice))
    }
    if(choice %in% valid_options[5:8]){
      if(choice == "yes" || choice == "y"){
        game()
      }
      else if(choice == "no"|| choice == "n"){
        choice = tolower(noquote(readline("Would you like the current scoreboard to be saved as a text file to continue streaks for the next game? 'Yes[y]' or 'No[n]'")))
        while(!(choice %in% valid_options[5:8])){
          choice =  noquote(readline(paste(choice, "is not a valid option. Please choose 'Yes(y)' or 'No(n)'. ")))
          choice = tolower(as.character(choice))
        }
        if(choice == "yes" || choice == "y"){
          scores <<- data.frame(win = c(player_1$win, player_2$win),loss = c(player_1$loss, player_2$loss), row.names = c("player_1", "player_2"))
          write.csv(scores, file = "player_scores_for_two_player_blackjack_in_R.txt")
          print("The text file has been saved in the current working directory",quote = F)
        }
        print("Thanks for playing! Type 'two_player_blackjack()' with no quotations in the console if you want to play again.",quote = F)
        invokeRestart("abort")
      }
    }
  }
  #Output information
  output = function () {
    print(paste("Player 2's hand:"),quote = F)
    if(player_2$position == 2 & iteration == 0){
      print(player_2$initial_hand,quote = F)
      print(paste("Player 2's sum:",player_2$hand_value),quote = F)
    }
    else{
      print(player_2$hand,quote = F)
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
  #Output scoreboard
  score_tracker= function(information){
    if(information[2] == "win"){
      if(information[1] == "1"){
        player_1$win <<- player_1$win + 1
        player_2$loss <<- player_2$loss + 1
      }
      else{
        player_2$win <<- player_2$win + 1
        player_1$loss <<- player_1$loss + 1
      }
    }
    else if(information[2] == "loss"){
      if(information[1] == "1"){
        player_1$loss <<- player_1$loss + 1
        player_2$win <<- player_2$win + 1
      }
      else{
        player_2$loss <<- player_2$loss + 1
        player_1$win <<- player_1$win + 1
      }
    }
    print("______________________________",quote = F)
    print("",quote = F)
    print("Current scoreboard:",quote = F)
    print("______________________________",quote = F)
    print(paste("Player 1 wins:",  player_1$win),quote = F)
    print(paste("Player 1 losses:", player_1$loss),quote = F)
    print("",quote = F)
    print(paste("Player 2 wins:", player_2$win),quote = F)
    print(paste("Player 2 losses:", player_2$loss),quote = F)
  }
  #
  player_function = function(current_player, other_player){
    if(current_player$hand_value == 21){
      if(iteration == 0){
        print("______________________________",quote = F)
        print("",quote = F)
        print(paste("Player", current_player$player, "hit 21. Now it is Player", paste0(other_player$player, "'s"), "turn."),quote = F)
        print(noquote("______________________________"))
        iteration <<- iteration + 1
        output()
        player_function(other_player, current_player)
      }
      }
    player_choice = noquote(readline("Hit(h) or stand(s)? "))
    player_choice = tolower(as.character(player_choice))
    while(!(player_choice %in% valid_options[1:4])){
      player_choice = noquote(readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). ")))
      player_choice = tolower(as.character(player_choice))
    }
    while(player_choice == "hit" || player_choice == "h"){
      print("______________________________",quote = F)
      print("",quote = F)
      print(paste("Player",current_player$player , "decided to hit."),quote = F)
      print("______________________________",quote = F)
      current_player$random_cards_generator()
      output()
      #If player's hand exceeds 21. Player lose.
      if(current_player$hand_value > 21){
        print("",quote = F)
        prompt = paste("Bust! Player" , current_player$player, "loses!")
        print(prompt,quote = F)
        prompt  = unlist(strsplit(prompt, NULL))
        information = prompt[which(prompt %in% c(1,2))]
        information = c(information, "loss")
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
        else if(iteration == 1){
          if(current_player$hand_value == 21 & !(other_player$hand_value == 21)){
            print("",quote = F)
            prompt = paste("Player", current_player$player, "got to 21. Player" ,current_player$player, "wins!")
            print(prompt,quote = F)
            prompt  = unlist(strsplit(prompt, ""))[1:8] 
            information = prompt[which(prompt %in% c(1,2))]
            information = c(information, "win")
            score_tracker(information)
            try_again()
            }
          else if(current_player$hand_value == 21 & other_player$hand_value == 21){
            print("",quote = F)
            print("Tie!",quote = F)
            try_again()
            }
        }
      player_choice = noquote(readline("Hit(h) or stand(s)? "))
      player_choice = tolower(as.character(player_choice))
      while(!(player_choice %in% valid_options[1:4])){
        player_choice = noquote(readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). ")))
        player_choice = tolower(as.character(player_choice))
      }
    }
    if(player_choice == "stand" & iteration == 0 || player_choice == "s" & iteration == 0){
      print("______________________________",quote = F)
      print("",quote = F)
      print(noquote(paste("Player", current_player$player, "decided to stand. Now it is Player", paste0(other_player$player, "'s"), "turn.")))
      print("______________________________",quote = F)
      #Reveal dealer's hidden card. If dealer's hand exceeds 21, player wins.
      iteration <<- iteration + 1
      output()
      player_function(other_player, current_player)
    }
   else{
      if(current_player$hand_value > other_player$hand_value){
        print("",quote = F)
        prompt = paste("Player",  current_player$player, "is closer to 21. Player" , current_player$player, "wins!")
        print(prompt,quote = F)
        prompt  = unlist(strsplit(prompt, ""))[1:8] 
        information = prompt[which(prompt %in% c(1,2))]
        information = c(information, "win")
        score_tracker(information)
        try_again()
      }
      else if(current_player$hand_value < other_player$hand_value){
        print("",quote = F)
        prompt = paste("Player", other_player$player, "is closer to 21. Player" ,other_player$player, "wins!")
        print(prompt,quote = F)
        prompt  = unlist(strsplit(prompt, ""))[1:8] 
        information = prompt[which(prompt %in% c(1,2))]
        information = c(information, "win")
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
    deck <<-  c(paste0(c(2:10, "J", "Q", "K", "A"),"♦"),paste0(c(2:10, "J", "Q", "K", "A"),"♠"),
                paste0(c(2:10, "J", "Q", "K", "A"),"♥"), paste0(c(2:10, "J", "Q", "K", "A"),"✤")
    )
    iteration <<-0
    player_randomiser()
  }
  game()
}
two_player_blackjack()


