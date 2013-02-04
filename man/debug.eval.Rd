\name{debug.eval}
\alias{debug.eval}
\title{Evaluate expression in debugging window}
\description{Like \code{eval} but summons up a debug window, as when debugging a function call. Use it e.g. for debugging your own scriptlets stored as expressions (see also \code{fixr} in \code{mvbutils}). There are analogous functions \code{debug.evalq}, \code{debug.with}, \code{debug.within}, and \code{debug.try}, but currently (and for no particular reason) these are not exported, so you'd need e.g. \code{debug:::debug.with( dataset, commands)}.
}
\usage{debug.eval(expr, envir = parent.frame(), enclos = if (is.list(envir) ||  is.pairlist(envir)) parent.frame() else baseenv())
}
\arguments{\item{ expr, envir, enclos}{see \code{\link{eval}}}
}
\seealso{\code{fixr}, help for the \pkg{debug} package
}
\keyword{misc}
