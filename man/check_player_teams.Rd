\name{check_player_teams}
\alias{check_player_teams}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{check_player_teams
%%  ~~function to do ... ~~
}
\description{
This will check the Yakkertech database for players who are on more than one team. This is usually due to tagging errors. If a player has in fact played for multiple teams, then it is ok, but the goal is with these 3 queries to return zero rows.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_player_teams(database_connection)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database_connection}{
     Must be a RSQLite database connection:
     db <- dbConnect(SQLite(),"C:/path_to/flashdb.sqlite")

}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
# this is required prior to running the function
db <- dbConnect(SQLite(),"C:/path_to/flashdb.sqlite")

check_player_teams(database_connection = db)

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
