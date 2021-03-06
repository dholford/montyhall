% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HolfordMeetsMonty.R
\name{determine_winner}
\alias{determine_winner}
\title{Win or Lose?}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{`determine_winner()`}{takes two arguments. The first, final.pick, pulls in the
single integer indicating the contestants final choice and the location to look
for "car". The argument game recalls the positions of the 2 goats and 1 car.
The function will run final.pick against game, to determine whether or not
the final choice matches the position of the "car"}
}
\value{
This function returns a single character vector indicating whether or not
the final pick matched the location of the car "Win" or a goat "Lose".
}
\description{
\code{determine_winner()} runs the final pick determined by the previous function
and compares it to the position of the car from the initial game set up and returns
a character vector stating "Win" if the final pick and car position match, and "Lose"
if the final pick and car position do not match.
}
\details{
The final step of the game is determining whether or not the contestant's final
pick reveals the car or goat. Now that the final.pick has been locked in, and the
location of the car door has been determined since \code{create_game()} we can
compare those two integers to see if the final pick door matches the car door.

This function again uses an if function to allow for results specific to the
two potential options: IF final.pick matches the car door and IF final.pick
does NOT match the car door.
}
\examples{
determine_winner(final.pick = 2, game = c("goat", "car", "goat")) - would
return "Win"

determine_winner(final.pick, game) - will pull results from previous functions
when all run together.

determine_winner(final.pick = 1, game = c("goat", "car", "goat")) - would
return "Lose"

}
