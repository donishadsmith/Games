single_deck_blackjack = function(){
  print(noquote("Welcome to BlackJack in R! You'll be playing a game of 21 using a single-deck of cards."))
  print(noquote(""))
  #All the input options relevant to this game. Player will be able to input response in the R console
  valid_options = c("hit", "stand", "h", "s", 'yes', 'no', 'y', 'n')
  #Function to try again. If player says yes, the game gets repeated
  try_again = function(){
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
        print(noquote("Thanks for playing! Type 'single_deck_blackjack()' with no quotations in the console if you want to play again."))
        stop(call. = F)
      }
    }
  }
  #Actual blackjack game
  game = function(){
    #The deck is created. If the player wants to try again, the deck needs to be created again.
    #Cards are discarded as game progresses
    deck =  c(paste0(c(1:10, "J", "Q", "K"),"♦"),paste0(c(1:10, "J", "Q", "K"),"♠"),
              paste0(c(1:10, "J", "Q", "K"),"♥"), paste0(c(1:10, "J", "Q", "K"),"✤"), "A"
    )
    random_cards_generator = function(number_of_cards_needed){
      #Always shuffle before selecting card
      deck <<- sample(deck,length(deck))
      #Randomly select card without replacement
      random_cards <- noquote(sample(deck, number_of_cards_needed, replace = F))
      #Ensuring no replicate cards by eliminating chosen cards. This will replicate the probability of a real single-deck game
      deck <<- deck[!(deck %in% random_cards)]
      return(random_cards)

    }
    #Generating initial 2 cards for the dealer and player
    dealer_hand = random_cards_generator(2)
    #After cards are selected for player, the current deck has 49 cards
    #print(length(deck)) ###############
    player_hand = random_cards_generator(2)
    #Hide first card for the dealer
    initial_dealer_hand = dealer_hand
    initial_dealer_hand[1] = noquote("_")
    #Function to sum the value of the cards
    sum_cards = function(hand){
      #Initial total sum will be 0. Need to initialize variable with a number for summation
      total_sum = 0
      for(card in hand){
        #split each character in a string "1♦" becomes "1" "♦", the first position will always be a number
        #except for the ace
        hand = unlist(strsplit(card, NULL))
        if(hand[1] == "A"){
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
        #Unlisting 10 + it's symbol with strsplit results in "1" "0" "symbol"
        else if(hand[1] %in% list("J", "K", "Q") || length(hand) == 3 ){
          number = 10
        }
        #Tested and works. If Ace = 11 exceeds 21, Ace will be equal to 1 instead
        else{
          number = as.numeric(hand[1])
          }  
        total_sum = sum(cbind(total_sum,number))
        }
      return(total_sum)
    }
    #Function to inform player about the status of their game
    output = function (dealer, player) {
      print(noquote("Dealer's hand:"))
      print(dealer)
      print(noquote(paste("Dealer's sum:",sum_cards(dealer))))
      print(noquote(""))
      print(noquote("Your hand:"))
      print(player)
      print(noquote(paste("Your sum:",sum_cards(player))))
      }
    output(initial_dealer_hand, player_hand)
    #Function for the dealer
    dealer_function = function(){
      #print(deck)
      #While loop 
      while(isTRUE(sum_cards(dealer_hand) <= sum_cards(player_hand)) == T || isTRUE(sum_cards(dealer_hand) >= sum_cards(player_hand)) == T)
        {
        #print(length(deck)) ###############
        output(dealer_hand, player_hand)
        print(noquote("----------------------"))
        if(isTRUE(sum_cards(dealer_hand) > 21)){
          print(noquote(""))
          print(noquote("Dealer bust! You win!"))
          try_again()
        }
        #Half 4.5 of a second delay so player can read output text
        Sys.sleep(0.8)
        if(isTRUE(sum_cards(dealer_hand) > sum_cards(player_hand)) == T){
          if(sum_cards(dealer_hand) == 21){
            print(noquote(""))
            print(noquote("Dealer reached 21. You lose!"))
            try_again()}
          else{
            print(noquote(""))
            print(noquote("Dealer decided to stand."))
            print(noquote("----------------------"))
            print(noquote(""))
            print(noquote("Dealer closer to 21. You lose!"))
            try_again()
          }
        }
        #Code for how to handle a tie. 50% of the time, there will be a tie the other 50% results in a win or loss
        else if (sum_cards(dealer_hand) == sum_cards(player_hand)){
          
          choice = sample(c("tie", "hit"),1)
          if(sum_cards(dealer_hand) == 21){
            print(noquote(""))
            print(noquote("Tie!"))
            try_again()
          }
          else if(choice == "tie"){
            print(noquote(""))
            print(noquote("Tie!"))
            try_again()
          }
          else{
            print(noquote(""))
            print(noquote("Dealer decided to hit."))
            dealer_hand = c(dealer_hand, random_cards_generator(1))
          }
        }
        else{
          print(noquote(""))
          print(noquote("Dealer decided to hit."))
          dealer_hand = c(dealer_hand, random_cards_generator(1))
        }
        #Some seperation between the outputs of the while loop
        print(noquote("----------------------"))
      }
    }
    if(sum_cards(player_hand) == 21){
      dealer_function()
    }
    player_choice = noquote(readline("Hit(h) or stand(s)? "))
    player_choice = tolower(as.character(player_choice))
    while(!(player_choice %in% valid_options[1:4])){
      player_choice = noquote(readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). ")))
      player_choice = tolower(as.character(player_choice))
      }
    while(player_choice == "hit" || player_choice == "h") {
      player_hand = c(player_hand, random_cards_generator(1))
      output(initial_dealer_hand, player_hand)
      #print(length(deck)) ###############
      #If player's hand exceeds 21. Player lose.
      if(sum_cards(player_hand) > 21){
        output(dealer_hand, player_hand)
        print(noquote(""))
        print(noquote("Bust! You lose!"))
        try_again()
      }
      else if(sum_cards(player_hand) == 21){
        dealer_function()
      }
      player_choice = noquote(readline("Hit(h) or stand(s)? "))
      player_choice = tolower(as.character(player_choice))
      while(!(player_choice %in% valid_options[1:4])){
        player_choice = noquote(readline(paste(player_choice, "not a valid option. Please choose hit(h) or stand(s). ")))
        player_choice = tolower(as.character(player_choice))
      }
    }
    if(player_choice == "stand" || player_choice == "s"|| sum_cards(player_hand) == 21){
      #Reveal dealer's hidden card. If dealer's hand exceeds 21, player wins.
      dealer_function()
    }
    }
  game()
}
single_deck_blackjack()

