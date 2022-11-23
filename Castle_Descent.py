##Python version of Castle Descent. Need to clean up code and add comments for both oythin and R version
import random, copy
import numpy as np
def castle_descent():
    class castle_descent_player_information:
        
        def __init__(self, player, player_health, player_coordinate, encountered_object, movement_coordinate, attack_power,
                    player_attack_range, floor, free_space):
            self.player = player
            self.player_health = player_health
            self.player_coordinate = player_coordinate
            self.encountered_object = encountered_object
            self.movement_coordinate = movement_coordinate
            self.player_attack_range = player_attack_range
            self.floor = floor
            self.attack_power = attack_power
            self.free_space = free_space
        
        def try_again(self):
            player_retry = str(input('Want to play again? Yes (y) or No (n)? ').lower())
            while player_retry not in ['yes', 'no', 'y', 'n']:
                player_retry = str(input(f"'{player_retry}' is not a valid option. Want to play again? Yes (y) or No (n)? ").lower())
            if player_retry == 'yes' or player_retry == 'y':
                castle_descent()
            else:
                print('Thank you for playing Castle Descent!')
                           

        def controller(self,player_input):
            self.movement_coordinate = tuple(movement[player_input])
            self.encountered_object = castle[self.movement_coordinate]
            self.events()
            
        def events(self):
            if self.encountered_object == '\u2800':
                game_board[self.player_coordinate] = '\u2800'
                self.player_coordinate = copy.deepcopy(self.movement_coordinate)
                game_board[self.player_coordinate] = self.player
                print(np.array2string(game_board[self.floor], separator='   ', formatter={'str_kind': lambda x: x}))
  
            elif self.encountered_object=='\U0001f9da':
                game_board[self.movement_coordinate] = copy.deepcopy(castle[self.movement_coordinate])
                print(np.array2string(game_board[self.floor], separator='   ', formatter={'str_kind': lambda x: x}))
                print('You encountered a fairy! Your health increases by 10 HP.')
                self.player_health = self.player_health + 10
                print(f'New HP: {self.player_health}')
                numerical_board[self.movement_coordinate] = 0
                game_board[self.movement_coordinate] = '\u2800'
                castle[self.movement_coordinate]  = '\u2800'
  
            elif self.encountered_object==u'\U0001F9DE':
                game_board[self.movement_coordinate] = copy.deepcopy(castle[self.movement_coordinate])
                print(np.array2string(game_board[self.floor], separator='   ', formatter={'str_kind': lambda x: x}))
                print('You encountered a genie!')
                print('Your attack range increased by 2 points.')
                self.player_attack_range = [num + 2 for num in self.player_attack_range]
                print(f'New attack range: {min(self.player_attack_range)}:{max(self.player_attack_range)}')
                numerical_board[self.movement_coordinate] = 0
                game_board[self.movement_coordinate] = '\u2800'
                castle[self.movement_coordinate]  = '\u2800'
  
            elif self.encountered_object==u'\U0001f479':
                game_board[self.movement_coordinate] = copy.deepcopy(castle[player.movement_coordinate])
                print(np.array2string(game_board[self.floor], separator='   ', formatter={'str_kind': lambda x: x}))
                print('You encountered a monster!')
                print(f'Your HP: {self.player_health}')
                print(f'Your attack range: {min(self.player_attack_range)}:{max(self.player_attack_range)}')
                print(f'Monster HP: {numerical_board[self.movement_coordinate][0]}')
                player_monster_choice = str(input('Would you like to attack(a) or run(r)? ').lower())
                while player_monster_choice not in ['attack', 'run', 'a', 'r']:
                    player_monster_choice=str(input(f"'{player_monster_choice}' is not a valid option. Would you like to attack(a) or run(r)? ").lower())
                if player_monster_choice == 'attack' or player_monster_choice == 'a':
                    print('You decided to attack')
                    while player_monster_choice != 'run' and player_monster_choice != 'r' and numerical_board[player.movement_coordinate] > 0:
                        print(np.array2string(game_board[self.floor], separator='   ', formatter={'str_kind': lambda x: x}))
                        self.attack_power = random.sample(self.player_attack_range, 1)[0]
                        print(f'You dealt {self.attack_power} points of damage.')
                        numerical_board[self.movement_coordinate] = numerical_board[self.movement_coordinate] - player.attack_power
                        if numerical_board[self.movement_coordinate] <= 0:
                            print('The monster fainted. You won!')
                            numerical_board[self.movement_coordinate] = 0
                            game_board[self.movement_coordinate] = '\u2800'
                            castle[self.movement_coordinate]  = '\u2800'
                        else:
                            print(f'Monster HP: {numerical_board[self.movement_coordinate][0]}')
                            monster_attack = random.sample([num for num in range(1,6)], 1)[0]
                            print(f'Monster dealt {monster_attack} points of damage.')
                            self.player_health = self.player_health - monster_attack
                            print(f'Your HP: {self.player_health}')
                            if self.player_health <= 0:
                                print('You died')
                                self.try_again()
                            else:
                                player_monster_choice = str(input('Would you like to attack(a) or run(r)? ').lower())
                                while player_monster_choice not in ['attack', 'run', 'a', 'r']:
                                    player_monster_choice=str(input(f"'{player_monster_choice}' is not a valid option. Would you like to attack(a) or run(r)? ").lower())
                else:
                    print('You decided to run.')
        
            elif self.encountered_object=="S":
                self.floor = self.floor + 1
                #Will be creating better stairs logic
                print(f'You found the stars! You can now advance to floor {self.floor}!')
                game_board[self.movement_coordinate[0][0] + 1,self.movement_coordinate[1][0],
                self.movement_coordinate[2][0]] = self.player
                game_board[self.player_coordinate] = '\u2800'
                self.player_coordinate = np.where(game_board==u'\U0001F93A')
                print(np.array2string(game_board[player.floor], separator='   ', formatter={'str_kind': lambda x: x}))
  
    ###Create Castle   
    castle_area = random.sample([num for num in range(8,16, 2)],1)[0]
    castle = np.array(np.zeros([3, castle_area, castle_area]),  dtype='<U1')
    castle[np.where(castle=='0')] = '\u2800'
    castle[0:3,[0,castle_area-1],0:castle_area] = '\u2592'
    castle[0:3,:,[0,castle_area-1]] ='\u2592'
    inner_bounds = castle_area - 2

    for num in range(0,3):
        shape = castle[num].shape
        floor = castle[num].flatten()
    
    
        #np.where(floor=='\u2800')[0]),1) is an array within a tuple
        #np.where(floor=='\u2800')[0]),1)[0] is an array 
    
        monster_proportion = inner_bounds/4
        free_space = np.where(floor=='\u2800')[0][::(inner_bounds)]
    
        #Creating an equally spaced array with different unicode
        for row_begin in free_space:
            row_end = row_begin + inner_bounds
            floor[random.sample([x for x in range(row_begin,row_end)],int(monster_proportion))] = u'\U0001f479'
    
        genie_proportion = fairy_proportion = int(round(len(np.where((floor ==u'\U0001f479'))[0]) * .10,0))
        floor[random.sample(list(np.where((floor ==u'\U0001f479'))[0]), genie_proportion)] = u'\U0001F9DE'
        floor[random.sample(list(np.where((floor ==u'\U0001f479'))[0]), fairy_proportion)] = '\U0001f9da'
    
    
        if num == 2:
            floor[random.sample(list(np.where(floor==u'\U0001f479')[0]),1)[0]] = '\u2395'
     
        else:
            floor[random.sample(list(np.where(floor==u'\U0001f479')[0]),1)[0]] = 'S' 
        if num == 0:
            spawn_player = np.array(copy.deepcopy(floor))
            spawn_player[random.sample(list(np.where(spawn_player=='\u2800')[0]),1)[0]] = u"\U0001F93A" 
            spawn_player.shape = shape
        
        
        floor.shape = shape
        castle[num] = floor


    
    game_board = np.array(copy.deepcopy(castle))
    game_board[0] = spawn_player
    game_board[(game_board != '\u2800') & (game_board != '\u2592') & (game_board != u'\U0001F93A')] = u'\U0001f6aa'

    numerical_board = np.zeros_like(copy.deepcopy(castle), dtype = '>i4')

    for floor in range(0,3):
    
        castle_floor = castle[floor].flatten()
        numerical_floor = np.zeros_like(copy.deepcopy(castle_floor), dtype = '>i4')
    
        numerical_floor[np.where((castle_floor==u'\U0001F9DE') | (castle_floor==u'\U0001f9da'))[0]] = 1
        monster_coordinates = np.where(castle_floor == u'\U0001f479')[0]
        for monster in monster_coordinates:
            if floor == 0:
                hp = random.sample([x for x in range(5,11)],1)[0]
            elif floor == 1:
                hp = random.sample([x for x in range(11,21)],1)[0]
            else:
                hp = random.sample([x for x in range(21,31)],1)[0]
       
        
            numerical_floor[monster] = hp
        
        numerical_floor.shape = shape
        numerical_board[floor] = numerical_floor
        
    player = castle_descent_player_information(player = u'\U0001F93A', player_health = 100,
                                     player_attack_range = [num for num in range(5,11)],
                                     player_coordinate = np.where(game_board==u'\U0001F93A'),
                                     floor = 0, encountered_object = '', movement_coordinate = '',
                                              free_space = '\u2800', attack_power = '')

    player_input = ''
    
    print('Welcome to Castle Descent!')
    print(np.array2string(game_board, separator='   ', formatter={'str_kind': lambda x: x}))
    print('')
    print('')
    print('')
    print('')
    while player.encountered_object != '\u2395':
        #Dictionary gets updated with new coordinated after each iteration
        movement = {
            'w':list([player.player_coordinate[0],player.player_coordinate[1] - 1,player.player_coordinate[2]]),
            'a':list([player.player_coordinate[0],player.player_coordinate[1],player.player_coordinate[2] - 1]),
            's':list([player.player_coordinate[0],player.player_coordinate[1] + 1 ,player.player_coordinate[2]]),
            'd':list([player.player_coordinate[0],player.player_coordinate[1],player.player_coordinate[2] + 1])
        }
        
        if player_input == '':
            print('You will start at the top of the castle.')
            print(np.array2string(game_board[player.floor], separator='   ', formatter={'str_kind': lambda x: x}))
        
        player_input = str(input('w (up), a (left), s (down), d (right): ').lower())
        while player_input not in movement:
            player_input = str(input(f"'{player_input}' is not a valid option. w (up), a (left), s (down), d (right): ").lower())
        
        player.controller(player_input)
        
    game_board[player.movement_coordinate] = castle[player.movement_coordinate]
    print(np.array2string(game_board[player.floor], separator='   ', formatter={'str_kind': lambda x: x}))
    print("You found the exit!")
    player.try_again()

castle_descent()
