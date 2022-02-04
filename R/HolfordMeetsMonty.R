#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}

#' **TEST**
create_game()
create_game()
create_game()


#' @title
#'  Choose Your Lucky Door
#' @description
#'  `select_door()` randomly samples 1 number from the vector (1,2,3) to
#'  represent the door the contestant has chosen.
#' @details
#'  Once the game has been created, we have 3 potential prizes, goat, goat, car
#'  each positioned in a character vector. `select_door()` represents the selection
#'  of the contestant and will be associated with the position of the character vector
#'  with each prize.
#' @param
#'  No parameters are used in this function.
#' @return
#'  The function returns a single, random integer between 1 and 3 representing
#'  the door selected by the contestant -> a.pick
#' @examples
#'  select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

#' **TEST**
select_door()
select_door()
select_door()

#' @title
#' Open a Goat Door
#' @description
#' `open_goat_door()` gathers information from the two previous function and
#' identifies one "goat door" to reveal to the contestant, reducing the number
#' of doors the car could be behind to two.
#' @details
#' Now that the contestant has picked a door, the host reveals one of the doors
#' that does not hold the car. This reduces the number of doors that the
#' car could be behind to two: either the door the contestant already picked
#' or the last remaining door. This will set up the next choice presented to the
#' contestant in the game and represented in the next function.
#'
#' This function uses an if function allowing the game to to know which door
#' to open based on both the location of the car, and the location of the first pick.
#'
#'
#' @param
#' This function requires two arguments: "game" (a 3 length character vector)
#' and "a.pick" (a single integer 1:3). These arguments come from the two previous functions and define
#' 1. Which doors have goats behind them and can be opened
#' 2. Which door the contestant has picked and cannot be opened
#' @return
#' This function returns a single integer that indicates the position of a goat
#' not selected by the contestant.
#'
#' @examples
#' open_goat_door(game = c("goat", "goat", "car"), a.pick = 3)
#' open_goat_door(game = c("goat", "car", "goat"), a.pick= sample(c(1:3), size = 1))
#' open_goat_door(game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}

#'**TEST**
game <- c("goat","car","goat")
a.pick <- 2
open_goat_door(game, a.pick)
#' Since the car is in position 2, and the contestant picked door 2, `open_goat_door()`
#' should return either 1 or 3.

#' @title
#' Stay or Switch?
#' @description
#' `change_door()` ultimately delivers the final selection for the contestant as
#' a single integer. The final selection is dependent upon the argument stay = T, which
#' will default to staying with the door the contestant initially picked. If stay = F, then
#' the function will change the selection to the only remaining door (less the initial pick
#' and opened goat door.)
#' @details
#' Now that one possibility has been removed, the contestant is given the opportunity
#' to stay with their original choice or switch to the final remaining door. This is
#' where the crux of the problem lies. Do your increase your chances of winning by
#' switching or are you best to stay with your initial choice? By running a number
#' of simulations we can track the win percentage when the stay strategy is used and
#' when the switch strategy is used to answer that question.
#' @param
#' This function introduces the argument stay, and requires the arguments opened.door, and
#' a.pick. The stay argument tells the function whether or not to switch the selection.
#' The opened.door and a.pick arguments help the function determine the final pick.
#' @return
#' This function returns a single integer representing the final door pick. If stay = T,
#' the function will return the integer indicated by a.pick. If stay = F, the function will
#' return the only remaining positional integer after taking away the options of
#' a.pick and opened.door
#' @examples
#' change_door(stay = T, opened.door, a.pick)
#' change_door(stay = F, opened.door, a.pick)
#' change_door(stay = T, opened.door = 1, a.pick = 2)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}

#'**TEST**
game <- c("car", "goat", "goat")
a.pick <- 2
opened.door <- 3
#' Given the above scenario, when stay = T, change_door should return 2, and if
#' stay = F, change_door should return 1. Change these settings and run the functions below
#' to test.
change_door(stay = T, opened.door, a.pick)

change_door(stay = F, opened.door, a.pick)

#' @title
#' Win or Lose?
#' @description
#' `determine_winner()` runs the final pick determined by the previous function
#' and compares it to the position of the car from the initial game set up and returns
#' a character vector stating "Win" if the final pick and car position match, and "Lose"
#' if the final pick and car position do not match.
#' @details
#' The final step of the game is determining whether or not the contestant's final
#' pick reveals the car or goat. Now that the final.pick has been locked in, and the
#' location of the car door has been determined since `create_game()` we can
#' compare those two integers to see if the final pick door matches the car door.
#'
#' This function again uses an if function to allow for results specific to the
#' two potential options: IF final.pick matches the car door and IF final.pick
#' does NOT match the car door.
#' @param
#' `determine_winner()` takes two arguments. The first, final.pick, pulls in the
#' single integer indicating the contestants final choice and the location to look
#' for "car". The argument game recalls the positions of the 2 goats and 1 car.
#' The function will run final.pick against game, to determine whether or not
#' the final choice matches the position of the "car"
#' @return
#' This function returns a single character vector indicating whether or not
#' the final pick matched the location of the car "Win" or a goat "Lose".
#' @examples
#' determine_winner(final.pick = 2, game = c("goat", "car", "goat")) - would
#' return "Win"
#'
#' determine_winner(final.pick, game) - will pull results from previous functions
#' when all run together.
#'
#' determine_winner(final.pick = 1, game = c("goat", "car", "goat")) - would
#' return "Lose"
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}

#'**TEST OF FUNCTION ONLY**

game <- c("goat", "car", "goat")
final.pick <- sample(c(1:3), size = 1)
final.pick
determine_winner(final.pick, game)

#'**TEST OF ALL FUNCTIONS IN GAME**

this.game <- create_game()
my.initial.pick <- select_door()
opened.goat.door <- open_goat_door( this.game, my.initial.pick )

my.final.pick.stay <- change_door( stay=T,
                                   opened.door=opened.goat.door,
                                   a.pick=my.initial.pick )
my.final.pick.switch <- change_door( stay=F,
                                     opened.door=opened.goat.door,
                                     a.pick=my.initial.pick )
this.game
my.initial.pick
opened.goat.door
my.final.pick.stay
my.final.pick.switch

determine_winner(my.final.pick.stay, this.game)

determine_winner(my.final.pick.switch, this.game)

#' @title
#' Play A Game and Store Results by Stay and Switch
#'
#' @description
#' `play_game()` packages the entire game scenario with outcomes for both the
#' "stay" and "switch" options. By generating a data frame with the outcome
#' for both "stay" and "switch" we can see which strategy would result in a
#' "Win" and which strategy would result in a "Lose."
#'
#' @details
#' Since the main question behind this game is whether the better strategy is stay
#' with your initial pick or switch once a goat door has been revealed, packaging a single
#' game playing out both the "stay" and "switch" scenarios allows for deeper analysis
#' of which strategy is ultimately the better strategy. The `play_game()` function
#' contains all functions discussed so far and includes a section to identify
#' if staying would result in a win or a loss and if switching would result in a
#' win or loss. The final section packages these results in a data frame and returns
#' the outcomes for both strategies.
#'
#' play_game() runs as follows:
#'
#' create_game() builds the game scenario as a character vector and names it as an object
#'
#' select_door() randomly samples a numeric vector from 1:3 and returns a single integer as
#'              an object
#'
#' open_goat_door() compares the contestants pick against the positions of the car and two
#'                  goats, opens a goat door not selected by the contestant, and returns
#'                  a single integer.
#'
#' change_door() is then used to return a single integer representing the door of the contestant
#'               if they choose to stay with their original pick and if they change to the other remaining door.
#'               It returns to single integers one for a stay, one for a switch.
#'
#' determine_winner() runs both final pick options from stay and switch strategies against the position
#'                    of the car to determine if staying would lead to a win or if switching would lead
#'                    to a win. It names two objects, each a single character vector with "Win" or "Lose"
#'
#'
#'
#' @param
#' This function requires no arguments since each function within play_game
#' generates the arguments needed to run through the scenarios.
#'
#' @return
#' This returns a data frame with two character vectors both strategies and
#' the outcome for each strategy.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#' Scale to Analyze
#'
#' @description
#' `play_n_games()` builds out the final dimension of the package, allowing for the running
#' of n games to build a robust data frame to analyze the % of wins when staying
#' and the % of wins when switching.
#'
#' @details
#' `play_n_games()` loads the necessary library and creates a collector vector
#' for the results of each simulation of the game. From here, the function runs
#' the play game function to simulate the outcome of a game for both
#' staying and switching and store it in a data frame. Finally, the function
#' binds the data frames from each loops and returns a table with the percentage of
#' wins and loses for each strategy.
#'
#' `play_n_games()` runs as follows:
#'
#' Loads dplyr to allow for building a table later on
#' Creates a collector vector to store the data frame for each loop
#' And names a loop count
#'
#' Sets up a for() loop to run n simulations of the game using `play_game()` and store the game outcome
#' by loop count in a data frame.
#'
#' Ultimately, this data frame is converted into a table to show the percentage of wins
#' for stay and switch strategies.
#'
#' @param
#' `play_n_games()` requires one argument to name the number of games to be played
#'
#' @return
#' This function returns a data frame with two vectors one vector with the strategy
#' and one with the outcome for all games played.
#'
#' @examples
#'
#' play_n_games(n = 100)
#' play_n_games(n = 50)
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}

