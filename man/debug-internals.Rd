\name{debug-internals}
\alias{debug-internals}
\alias{add.numbers}
\alias{addnum.move.to.next.expr}
\alias{backtrack.to.loop}
\alias{check.legality}
\alias{debug.break}
\alias{debug.do.call}
\alias{debug.mvb.subst}
\alias{debug.next}
\alias{debug.on.exit}
\alias{debug.q}
\alias{debug.return}
\alias{debug.retval}
\alias{debug.sys.on.exit}
\alias{dismiss.debug.window}
\alias{enact.command.r}
\alias{.end.incarnation}
\alias{enter.on.exit}
\alias{eval.bp}
\alias{eval.catching.errors}
\alias{evaluator}
\alias{exit.on.exit}
\alias{find.active.control.frame}
\alias{find.debug.HQ}
\alias{interact}
\alias{launch.debug.windows}
\alias{move.to.next.expression}
\alias{next.incarnation}
\alias{print.if.small}
\alias{screen.line}
\alias{set.a.breakpoint}
\alias{set.global.debug.vars}
\alias{setup.debug.admin}
\alias{skipto.debug}
\alias{stop.here}
\alias{.update.debug.window}
\alias{.update.window.with.on.exit}
\title{Functions used internally by debug package}
\description{Not meant to be called directly.}
\usage{
add.numbers(expr, width = options()$width, numbering = TRUE,
 cat.on.exit = FALSE, expr.offset = 0, line.number.offset = 0)
addnum.move.to.next.expr(nlocal = sys.parent(), final)
backtrack.to.loop(expr, i)
check.legality(thing, call.type)
debug.break()
debug.do.call(what, args)
debug.mvb.subst(expr)
debug.next()
debug.on.exit(new.expr, add = FALSE)
debug.q()
debug.return(\dots)
debug.retval()
debug.sys.on.exit()
dismiss.debug.window(win)
enact.command.r(command, frame)
.end.incarnation()
enter.on.exit()
eval.bp(ex, envir)
eval.catching.errors(i, envir)
evaluator(fname)
exit.on.exit()
find.active.control.frame()
find.debug.HQ(create.if.not = TRUE)
interact(nlocal = sys.parent(), input)
launch.debug.windows()
move.to.next.expression(sorted.out = TRUE, nlocal = sys.parent())
next.incarnation(nlocal = sys.parent())
print.if.small(x, \dots)
screen.line(l, nlocal = sys.parent())
set.a.breakpoint(bp.expr, line.no, frame.number = sys.parent())
set.global.debug.vars(\dots)
setup.debug.admin(nlocal = sys.parent())
skipto.debug(nlocal = sys.parent())
stop.here(nlocal = sys.parent())
.update.debug.window(nlocal = sys.parent(), l)
.update.window.with.on.exit(nlocal = sys.parent(), i, l)
}
\author{Mark Bravington}
\keyword{debugging}
\keyword{internal}
