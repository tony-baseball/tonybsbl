\name{game_check}
\alias{game_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{game_check
%%  ~~function to do ... ~~
}
\description{
This will check the csv and see where mistaked were made in the PAofInning, PitchofPA, Balls, Strikes, columns. It will
also help infer where mistakes were made in the Batter, BatterSide, Pitcher, PitcherThrows, Top.Bottom, columns.

It creates a new dataframe and adds 3 columns, pa_check (checks PAofInning for incorrect values), pitch_check (checks PitchofPA for incorrect values ),
and count_check (checks for incorrect values in the Balls, Strikes columns).
For a game to be considered "correct", the values in each of those 3 columns must be TRUE. If a value is FALSE, then there
is a mistake somewhere.

It will also print out the 3 columns in the console, and tell you how many FALSE's are in each column. You need these to be 0.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
game_check(path_to_csv_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_to_csv_file}{
     Must be a local csv file path, "C:/User/filename.csv"
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
# this is required prior to running the function
path_to_csv_file <- "C:/User/filename.csv"

game_check(path_to_csv_file)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
