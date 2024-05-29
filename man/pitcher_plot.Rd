\name{pitcher_plot}
\alias{pitcher_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pitcher Plot
%%  ~~function to do ... ~~
}
\description{
This will print out the pitchers who pitched in the dataset, and a plotly interactive plot
will pop up in the viewer. If you do not know the pitchers in the game, you can initially
leave ther `pitcher` argument blank. it will throw you an error, but still print out the pitchers
who pitched in the game.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pitcher_plot(path_to_csv_file, pitcher_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_to_csv_file}{
     Must be a local csv file path, "C:/User/filename.csv"
}
  \item{pitcher_name}{
     Can use any part of their name.
     "Tony" or "Medina" or "Tony Medina" or "Ton" or "Med"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
---
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
