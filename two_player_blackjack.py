#Two-player blackjack in Python. Still trying to refine the code but it is functional
from random import sample
import sys
import numpy as np
import pandas as pd
def two_player_blackjack():
    valid_options = ['yes', 'no', 'y','n', 'hit', 'h', 'stand', 's']
    iteration = 0
    deck = [str(i)+symbol for i in range(2,11) for symbol in ["♦","♥","♠","✤"]]
    add_cards = [face_card + symbol for face_card in ["J", "Q", "K", "A"] for symbol in ["♦","♥","♠","✤"]]
    class casino:
        def __init__(self, hand, hand_value, initial_hand, position, player, win, loss):
            self.hand = hand
            self.hand_value = hand_value
            self.initial_hand = initial_hand
            self.position = position
            self.player = player
            self.win = win
            self.loss = loss

        def sum_cards(self,current_hand):
            global iteration
            self.hand_value = 0
            if self.position == 2 and iteration == 0:
                hand = self.initial_hand
            else:
                hand = self.hand
            for card in hand:
                if card == "A":
                    if(self.hand_value + 11 > 21):
                        self.hand_value += 1
                    else:
                        self.hand_value += 11
                elif card[0] in ["J", "Q","K"] or len(card) == 3:
                    self.hand_value += 10
                elif card == "_":
                    self.hand_value += 0
                else:
                    self.hand_value += int(card[0])
            return self.hand_value

        def hidden_hand(self):
            self.initial_hand = list(self.hand)
            self.initial_hand[0] = '_'

        def card_selector(self):
            global deck
            deck = sample(deck,len(deck))
            if len(deck) > 49:
                number_of_cards_needed = 2
                random_cards = sample(deck,number_of_cards_needed)
                for card in random_cards:
                    deck.remove(card)
                    self.hand.append(card)
            else:
                number_of_cards_needed = 1
                random_card = sample(deck,number_of_cards_needed)
                #* removes brackets and quotations
                deck.remove(*random_card)
                self.hand.append(random_card[0])
            if self.position == 2:
                self.hidden_hand()
            self.sum_cards(self.hand)
    player_1 = casino([],'',[],'',1,0,0)
    player_2 = casino([],'',[],'',2,0,0)

    def player_randomiser():
        players = sample([1,2],1)[0]
        if players == 1:
            print("-----------------------------------------------------\nPlayer 1 goes first.\n-----------------------------------------------------")
            player_1.position = 1
            player_2.position = 2
            player_1.card_selector()
            player_2.card_selector()
            game_status()
            player_function(player_1, player_2)
        else:
            print("-----------------------------------------------------\nPlayer 2 goes first.\n-----------------------------------------------------")            
            player_1.position = 2
            player_2.position = 1
            player_1.card_selector()
            player_2.card_selector()
            game_status()
            player_function(player_2, player_1)
            
    def score_tracker(player, status):
        if status == "win":
            if player == "Player 1":
                player_1.win += 1
                player_2.loss += 1
            else:
                player_1.loss += 1
                player_2.win += 1
        if status == "loss":
            if player == "Player 1":
                player_1.loss += 1
                player_2.win += 1
            else:
                player_1.win += 1
                player_2.loss += 1
        print("")       
        print("-----------------------------------------------------")
        print("Scoreboard:")
        print("-----------------------------------------------------")
        print(f"Player 1's wins: {player_1.win}")
        print(f"Player 1's losses: {player_1.loss}")
        print("")
        print(f"Player 2's wins: {player_2.win}")
        print(f"Player 2's losses: {player_2.loss}")
        
    def try_again():
        print("")
        retry = input("Would you like to play again? Yes[y] or no[n]").lower()
        while retry not in valid_options:
            retry = input(f"{retry} is not a valid option. Please choose yes[y] or no[n].").lower()
        if retry == "yes" or retry == "y":
            player_1.hand = []
            player_2.hand = []
            player_1.initial_hand = []
            player_2.initial_hand = []
            start_game()
        else:
            print("")
            scores =pd.DataFrame( {"win": [player_1.win, player_2.win],
                       "loss": [player_1.loss, player_2.loss]},
                    index  = ["player_1", "player_2"])
            scores.to_csv("scoreboard_for_two_player_blackjack_python.txt")
            sys.exit("Thanks for playing!")
            
    def game_status():
        global iteration
        if player_2.position == 2 and iteration == 0:
            print("Player 2's hand:")
            print(*player_2.initial_hand)
        else:  
            print("Player 2's hand:")
            print(*player_2.hand)
        print(f"Player 2's sum: {player_2.sum_cards(player_2.hand)}")
        print("")
        if player_1.position == 2 and iteration == 0:
            print("Player 1's hand:")
            print(*player_1.initial_hand)
        else:  
            print("Player 1's hand:")
            print(*player_1.hand)
        print(f"Player 1's sum: {player_1.sum_cards(player_1.hand)}")

    def player_function(current_player, other_player):
        global iteration
        if(current_player.hand_value == 21):
            if iteration == 0:
                print(f"Player {current_player.player} hit 21. Now, it's player {other_player.player}'s turn.")
                print("")
                iteration += 1
                game_status()
                player_function(other_player, current_player)
            else:
                if current_player.hand_value > other_player.hand_value:
                    print(f"Player {current_player.player} reached 21. {current_player.player} wins!")
                    score_tracker({current_player.player}, "win")
                else:
                    print("Tie")
            try_again()
        print("")
        player_choice = input("Hit[h] or stand[s]?").lower()
        while player_choice not in valid_options:
            print("")
            print("")
            player_choice = input(f"{player_choice} is not a valid option. Please choose hit[h] or stand[s].").lower()
        while player_choice == "hit" or player_choice == "h":
            print("-----------------------------------------------------")
            print(f"Player {current_player.player} decided to hit.")
            print("-----------------------------------------------------")
            current_player.card_selector()
            game_status()
            if(current_player.hand_value > 21):
                print("")
                print(f"Bust! Player {current_player.player} loses!")
                score_tracker(current_player.player, "loss")
                try_again()
            elif iteration == 0 and current_player.hand_value == 21 :
                print(f" Player {current_player.player} hit 21. Now, it's player {other_player.player}'s turn.")
                print("")
                iteration += 1
                player_function(other_player, current_player)
            print("")
            player_choice = input("Hit[h] or stand[s]?").lower()
            while player_choice not in valid_options:
                player_choice = input(f"{player_choice} is not a valid option. Please choose hit[h] or stand[s].").lower()
        if iteration == 0:
            print("-----------------------------------------------------")
            print(f"Player {current_player.player} decided to stand. Now, it's player {other_player.player}'s turn.")
            print("-----------------------------------------------------")
            iteration += 1
            game_status()
            player_function(other_player, current_player)
        else:
            print("-----------------------------------------------------")
            print(f"Player {current_player.player} decided to stand.")
            print("-----------------------------------------------------")
            print("")
            if current_player.hand_value > other_player.hand_value:
                print(f"Player {current_player.player} is closer to 21. Player {current_player.player} wins!")   
                score_tracker(current_player.player, "win")
                try_again()
            elif current_player.hand_value < other_player.hand_value:
                print(f"Player {other_player.player} is closer to 21. Player {other_player.player} wins!")   
                score_tracker(other_player.player, "win")
                try_again()
            else:
                print("Tie!")
                try_again()
    
    def start_game(): 
        global deck
        global iteration
        deck = [str(i)+symbol for i in range(2,11) for symbol in ["♦","♥","♠","✤"]]
        add_cards = [face_card + symbol for face_card in ["J", "Q", "K", "A"] for symbol in ["♦","♥","♠","✤"]]
        global iteration
        iteration = 0
        player_randomiser()
        
    start_game()
    
two_player_blackjack()
    
