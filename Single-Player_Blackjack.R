##################################################################################
#Code for playing single-deck blackjack in R
#Code also allows you to play infinity since there is a function for trying again.
##################################################################################

blackjack = function(){
  #Characters in R are printed with quotations, no quotes turns a character into a noquote object so that
  #characters are free from quotations. It's a stylistic choice
  print(noquote("Welcome to BlackJack in R!\nYou'll be playing a two-player game of 21 using a single-deck of cards."))
  print(noquote(""))
  #All the input options relevant to this game. Player will be able to input response in the R console
  valid_options = c("hit", "stand", "h", "s", 'yes', 'no', 'y', 'n')
  casino_functions = setRefClass('casino', fields = list(hand = "character", hand_value = "numeric",
                                                          initial_hand = "character",
                                                         unlisted_hand = "character", win = "numeric", loss = "numeric", player = "character" 
                                                        ),
                                 methods = list(#Generator allows for the deck to be changed globally (outside of the function)
                                   #random_card_generator() sends random cards to other the hidden_hand function and sum_cards function
                                   #Anytime this function is used, the initial card of the second player is hidden and the value of the cards are summed.
                                   #Essentially, while there are three functions, only one function needs to be called
                                   random_cards_generator = function(){
                                     #Always shuffle before selecting card
                                     deck <<- sample(deck,length(deck))
                                     #Reset hand; player and dealer objects are created in the outermost function the object and cannot be reset if player resets the game as opposed to the function.
                                     #The try_again() function just calls the game() function again.
                                     #Less variables to create during resetting game
                                     if(length(deck) == 52){
                                       hand <<- ""
                                       initial_hand <<- ""
                                     }
                                     #Initial two cards
                                     if(length(deck) > 48){
                                       number_of_cards_needed = 2
                                       random_cards <- sample(deck, number_of_cards_needed, replace = F)
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
                                     if(player == "Dealer"){
                                       hidden_hand()
                                     }
                                     #Immediately sum hands.
                                     #Initial hand variable belongs to second player
                                     else{
                                       sum_cards(hand)
                                     }
                                   },
                                   hidden_hand = function(){
                                     initial_hand <<- hand
                                     initial_hand[1]  <<- "_"
                                     sum_cards(initial_hand)
                                     
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
    player = casino_functions(win = scores["player", "win"], loss = scores["player", "loss"])
    dealer = casino_functions(win = scores["dealer", "win"], loss = scores["computer", "loss"])
    
  }
  else{
    player = casino_functions(win = 0, loss = 0)
    dealer = casino_functions(win = 0, loss = 0)
  }
  player$player = "Player"
  dealer$player = "Dealer"

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
        start_game()
      }
      else if(choice == "no"|| choice == "n"){
        choice = tolower(noquote(readline("Would you like the current scoreboard to be saved as a text file to continue streaks for the next game? 'Yes[y]' or 'No[n]'")))
        while(!(choice %in% valid_options[5:8])){
          choice =  noquote(readline(paste(choice, "is not a valid option. Please choose 'Yes(y)' or 'No(n)'. ")))
          choice = tolower(as.character(choice))
        }
        if(choice == "yes" || choice == "y"){
          scores <<- data.frame(win = c(player$win, dealer$win),loss = c(player$loss, dealer$loss), row.names = c("player", "dealer"))
          write.csv(scores, file = "player_scores_for_two_player_blackjack_in_R.txt")
          print("The text file has been saved in the current working directory", quote = F)
        }
        print("Thanks for playing! Type 'blackjack()' with no quotations in the console if you want to play again.", quote = F)
        invokeRestart("abort")
      }
    }
  }
  #Output information
  output = function () {
    print(paste("Dealer's hand:"), quote = F)
    if(iteration == 0 || iteration == 1){
      print(noquote(dealer$initial_hand))
      print(noquote(paste("Dealer's sum:",dealer$hand_value)))
    }
    else{
      print(noquote(dealer$hand))
      print(paste("Dealer's sum:",dealer$sum_cards(dealer$hand)),quote = F)
    }
    print(noquote(""))
    print(noquote(""))
    print(paste("Player's hand:"), quote = F)
    print(noquote(player$hand))
    print(paste("Player's sum:",player$sum_cards(player$hand)),quote = F)
  }
  #Output scoreboard
  score_tracker= function(information){
    if(information[2] == "win"){
      if(information[1] == "Player"){
        player$win <<- player$win + 1
        dealer$loss <<- dealer$loss + 1
      }
      else{
        dealer$win <<- dealer$win + 1
        player$loss <<- player$loss + 1
      }
    }
    else if(information[2] == "loss"){
      if(information[1] == "Player"){
        player$loss <<- player$loss + 1
        dealer$win <<- dealer$win + 1
      }
      else{
        dealer$loss <<- dealer$loss + 1
        player$win <<- player$win + 1
      }
    }
    print("______________________________",quote = F)
    print("",quote = F)
    print("Current scoreboard:",quote = F)
    print("______________________________",quote = F)
    print(paste("Player's wins:",  player$win),quote = F)
    print(paste("Player's losses:", player$loss),quote = F)
    print("",quote = F)
    print(paste("Dealer's wins:", dealer$win),quote = F)
    print(paste("Dealer's losses:", dealer$loss),quote = F)
  }
  #
  game_function = function(current_player, other_player){
    while(player_choice == "hit" || player_choice == "h" || iteration == 0 || iteration == 2 ){
      if(iteration == 0){
        iteration <<- iteration + 1
      }
      if(current_player$hand_value == 21){
        if(iteration == 1){
          print("______________________________",quote = F)
          print("",quote = F)
          print(paste(current_player$player, "hit 21. Now it is",  paste0(other_player$player, "'s"), "turn."),quote = F)
          print("______________________________",quote = F)
          iteration <<- iteration + 1
          output()
          game_function(other_player, current_player)
        }
      }
      if(iteration == 2)
        if(current_player$hand_value > 21){
          print(noquote(""))
          prompt = paste("Bust!", current_player$player, "loses!")
          print(prompt,quote = F)
          prompt  = unlist(strsplit(prompt, NULL))[1:12]
          information <<- paste0(prompt[which(prompt %in% c(c("P","l","a","y","e","r"),c("D","e","a","l","e","r")))], collapse="")
          information <<- c(information, "loss")
          score_tracker(information)
          try_again()
        }
        else if(current_player$hand_value == 21 & !(other_player$hand_value == 21)){
          print("",quote = F)
          prompt = paste(current_player$player, "got to 21." , current_player$player, "wins!")
          print(prompt,quote = F)
          information  = paste0(unlist(strsplit(prompt, NULL))[1:6], collapse = "")
          information = c(information, "win")
          score_tracker(information)
          try_again()
        }
        else if(current_player$hand_value == 21 & other_player$hand_value == 21){
          print("",quote = F)
          print("Tie!",quote = F)
          try_again()
        }
        else if(current_player$hand_value > other_player$hand_value){
          print("",quote = F)
          prompt = paste(current_player$player, "is closer to 21. " , current_player$player, "wins!")
          print(prompt,quote = F)
          information  = paste0(unlist(strsplit(prompt, NULL))[1:6], collapse = "")
          information = c(information, "win")
          score_tracker(information)
          try_again()
        }
        else if(current_player$hand_value == other_player$hand_value){
          decision = sample(c(1,2),1)
          if(decision == 1){
            print("",quote = F)
            print("Tie!",quote = F)
            try_again()
          }
        }
      #If player's hand exceeds 21. Player lose.
      if(current_player$hand_value > 21){
        print(noquote(""))
        prompt = paste("Bust!", current_player$player, "loses!")
        print(prompt,quote = F)
        prompt  = unlist(strsplit(prompt, NULL))[1:12]
        information <<- paste0(prompt[which(prompt %in% c(c("P","l","a","y","e","r"),c("D","e","a","l","e","r")))], collapse="")
        information <<- c(information, "loss")
        score_tracker(information)
        try_again()
      }
      else if(iteration == 1){
        player_choice <<- noquote(readline("Hit(h) or stand(s)? "))
        player_choice <<- tolower(as.character(player_choice))
        while(!(player_choice %in% valid_options[1:4])){
          player_choice <<- noquote(readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). ")))
          player_choice <<- tolower(as.character(player_choice))
          }
        if(player_choice == "stand" || player_choice == "s" ){
          print("______________________________",quote = F)
          print("",quote = F)
          print(paste(current_player$player, "decided to stand. Now it is", paste0(other_player$player, "'s"), "turn."),quote = F)
          print("______________________________",quote = F)
          #Reveal dealer's hidden card. If dealer's hand exceeds 21, player wins.
          iteration <<- iteration + 1
          output()
          game_function(other_player, current_player)
            }
      }
      print("______________________________",quote = F)
      print("",quote = F)
      if(iteration == 2){
        Sys.sleep(0.8)
      }  
      print(paste(current_player$player , "decided to hit."),quote = F)
      print("______________________________",quote = F)
      current_player$random_cards_generator()
      output()
      }
    }
  #Actual blackjack game
  start_game = function(){
    #If the player wants to try again, the deck needs to be created again because the cards are discarded as the game progresses.
    deck <<-  c(paste0(c(2:10, "J", "Q", "K", "A"),"♦"),paste0(c(2:10, "J", "Q", "K", "A"),"♠"),
              paste0(c(2:10, "J", "Q", "K", "A"),"♥"), paste0(c(2:10, "J", "Q", "K", "A"),"✤")
    )
    iteration <<-0
    #Error if player_choice isn't initialized due to conditions of the while loop
    player_choice <<- ""
    player$random_cards_generator()
    dealer$random_cards_generator()
    output()
    print("",quote = F)
    game_function(player, dealer)
  }
  start_game()
}
blackjack()

