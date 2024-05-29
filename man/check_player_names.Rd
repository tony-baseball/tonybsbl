\name{check_player_names}
\alias{check_player_names}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{check_player_names
%%  ~~function to do ... ~~
}
\description{
This will check the Yakkertech database and compare the names against the Frontier League database. Sometimes the name can be misspelled on either the Frontier League website or in the Yakkertech files, so they need to be fixed to match each other.
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
