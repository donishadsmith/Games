
#print position
#for( letter in which(y %in% x)){print(letter)}
guess_the_word = function() {
  #All functions belong to base R
  dictionary = data.frame(easy = c("jam", "ace", "odd", "low", "the", "get", "lit", "fin", "guy", "bud"), intermediate = c("earth", "abode", "beach", "music","jumbo",
                                                                                                                           "field", "fried","quick", "drama", "slash"), hard = c("calvalry",
                                                                                                                               "iceberg",
                                                                                                                               "habitat",
                                                                                                                               "ukelele",
                                                                                                                               "fervent",
                                                                                                                               "gravity",
                                                                                                                               "ketchup",
                                                                                                                               "invoice",
                                                                                                                               "manhunt",
                                                                                                                               "progeny"))
  valid_options = list("e" = "easy", "i" = "intermediate", "h"= "hard","r" ="random level" , "y" = "yes",  "n" = "no", "full_options" = c("easy", "intermediate", "hard", "random level", "yes", "no"))
  input = noquote(readline("Select difficulty: 'easy(e) ', 'intermediate(i)', 'hard(h)', or 'random level(r)'? "))
  input = tolower(as.character(input))
  if(length(unlist(strsplit(input, NULL))) == 1){
    input = unlist(valid_options[input])
  }
  while(!(input %in% valid_options[["full_options"]][1:4])){
    input = noquote(readline(paste(input, "not a valid option. Please select: 'easy(e) ', 'intermediate(i)', 'hard(h)', or 'random level(r)'. ")))
    input = tolower(as.character(input))
    if(length(unlist(strsplit(input, NULL)) == 1)){
      input = unlist(valid_options[input])
    }  
  }
  attempts = noquote(readline("How many attempts to you want? Choose any number from 1-15. "))
  attempts = as.numeric(attempts)
  while(is.na(attempts) == TRUE){
    attempts = noquote(readline(paste("Number of attempts must be in numerical form. Please choose a number between 1-15. ")))
    attempts = as.numeric(attempts)
  }
  if(attempts > 15){
    attempts = 15
  }
  #Function
  try_again = function(){
    choice =  noquote(readline("Would you like to play again? Please choose 'Yes(y)' or 'No(n)'. "))
    choice = tolower(as.character(choice))
    if(length(unlist(strsplit(choice, NULL))) == 1){
      choice = unlist(valid_options[choice])
    } 
    while(!(choice %in% valid_options[["full_options"]][5:6])){
      choice =  noquote(readline(paste(choice, "is not a valid option. Would you like to play again? Please choose 'Yes(y)' or 'No(n)'. ")))
      choice = tolower(as.character(choice))
      if(length(unlist(strsplit(choice, NULL)) == 1)){
        choice = unlist(valid_options[choice])
      } 
    }
    if(choice %in% valid_options[["full_options"]][5:6]){
      if(choice == "yes"){
        guess_the_word()
        }
      else if(choice == "no"){
        print(noquote("Thanks for playing! Type 'guess_the_word()' with no quotations in the terminal if you want to play again."))
        stop(call. = F)
        }
      }
    }
  game = function(input)
  {
    if(input == "random level"){
      level = dictionary[,sample(1:3, 1)]
      target_word = level[sample(1:4,1)]
    }
    if(!(input == "random level")){
      level = dictionary[,input]
      target_word = level[sample(1:4,1)]
    }
    split_word = unlist(strsplit(target_word, NULL))
    print(noquote(paste("You chose", input, "mode the random word has", length(split_word), "characters." )))
    tracked_correct_letters = rep("_", length(split_word))
    print(noquote(tracked_correct_letters))
    used_letters = c()
    #Start guessing
    while(!(paste0(tracked_correct_letters, collapse = "") == paste0(split_word, collapse = ""))){
      guess_input = noquote(readline(paste("Input a single letter, multiple letters, or guess the word. You have", attempts,"attempts to guess the correct word. ")))
      guess_input = unique(unlist(strsplit(tolower(as.character(guess_input)), NULL)))
      turn_loss = 0
      if(length(which(guess_input %in% split_word) > 0)){
        for(letter in which(guess_input %in% split_word)){
          letter = guess_input[letter]
          tracked_correct_letters[which(split_word == letter)] = letter
          if(letter %in% used_letters){
            print(noquote(paste(letter, "already used.")))
            used_letters = used_letters
          }
          else if(!(letter %in% used_letters)){
            print(noquote(paste(letter, "is in the word.")))
            used_letters = cbind(used_letters, letter)
            turn_loss = turn_loss + 1
            }
        }
        for(letter in which(!(guess_input %in% split_word))){
          letter = guess_input[letter]
          if(letter %in% used_letters){
            print(noquote(paste(letter, "already used.")))
            used_letters = used_letters
          }
          else if(!(letter %in% used_letters)){
            print(noquote(paste(letter, "is not in the word.")))
            used_letters = cbind(used_letters, letter)
            turn_loss = turn_loss + 1
          }
        }
      }
      else if(!(guess_input %in% split_word)){
        for(letter in which(!(guess_input %in% split_word))){
          letter = guess_input[letter]
          if(letter %in% used_letters){
            print(noquote(paste(letter, "already used.")))
            used_letters = used_letters
          }
          else if(!(letter %in% used_letters)){
            print(noquote(paste(letter, "is not in the word.")))
            used_letters = cbind(used_letters, letter)
            turn_loss = turn_loss + 1
          }
        }
      }
      attempts = attempts - turn_loss
      if(attempts <= 0) { 
        print(noquote(paste("Sorry, you have 0 attempts left. You lose. The correct word was:", paste0(target_word, ".", collapse = ""))))
        try_again()
      }
      else if(attempts > 1){
        print(noquote(paste("You have", attempts, "attempts left.")))
      }
      else{
        print(noquote(paste("You have", attempts, "attempt left.")))
      }
      if(length(used_letters) == 1){
      print(noquote(paste("Letter used:",list(used_letters))))
      }
      else{
      print(noquote(paste("Letters used:",list(used_letters))))
      }
      print(noquote(paste("There are", sum(tracked_correct_letters == "_"), "letters left to guess.")))
      print(noquote(tracked_correct_letters))
      if(paste0(guess_input, collapse = "") == target_word || paste0(tracked_correct_letters, collapse = "") == paste0(split_word, collapse = "")){
        try_again()
      }
    }
  }
  game(input)
}

guess_the_word()





