\name{find.from}
\alias{find.from}
\title{Interactive debugging}
\description{\code{find.from( x, y)} returns the environment where \code{x} would be found if \R was trying to find object \code{x} within environment \code{y}.}
\usage{find.from( char.fname, from=mvb.sys.parent(), look.for.generics=TRUE)}
\arguments{
\item{ char.fname}{length 1 character vector giving the name of the object that's being looked for.}
\item{ from}{an environment, function, or number, showing where to start the search from. Defaults to the calling environment (or .GlobalEnv). If \code{from} is a function, its environment is used as the starting point for the search. If \code{from} is a number, sys.frames()[[ from]] is used.}
\item{ look.for.generics}{logical used internally in recursion; don't change this.}
}
\value{either FALSE (if not found) or an environment.}
\details{Used by \code{mtrace}. Doesn't attempt to deal with S4 methods. Doesn't have a \code{mode} argument (unlike \code{find}).}
\examples{find.from( "as.matrix.data.frame") # NULL; implies "package:base"}
\keyword{debugging}
