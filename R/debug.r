# This is package debug 

".end.incarnation" <-
function() do.in.envir( envir=find.debug.HQ( FALSE), {
#  cat( 'Ending', .frames.[nrow( .frames.), 'function.name'], '\n')
# In R 1.8.0, seem to need to de-link the listboxes before calling 'tkdestroy'
  if( nrow( .frames.) && .frames.$has.window.yet[ nrow( .frames.)])  
    try( evalq( {
        tkconfigure( line.list.win, yscroll=function(...) {}, xscroll=function(...) {})
        tkconfigure( bp.win, yscroll=function(...){})
        tkdestroy( tcl.win) },
      sys.frame( .frames.$debug[ nrow( .frames.)]) ) # evalq
    ) # try

  .frames. <<- .frames.[ -nrow( .frames.),]
  
  # Command recall: clear up temp history file if no more debug windows
  if( !nrow( .frames.) && debug.command.recall)
    unlink( debug.hist.file)
  .nothing.
})


".First.lib" <-
function( libname, pkgname)
  ( require( mvbutils, save=FALSE) && require( tcltk, save=FALSE) ) || 
      stop( "debug requires mvbutils and tcltk")


".onLoad" <-
function( libname, pkgname)
  ( require( mvbutils, save=FALSE) && require( tcltk, save=FALSE) ) || 
      stop( "debug requires mvbutils and tcltk")


".update.debug.window" <-
function( nlocal=sys.parent(), l) mlocal({
  l <- screen.line( lno)
  if( l != old.l) {
    if( old.l>0) # not first time
      tkitemconfigure( line.list.win, old.l, background='White', selectforeground='White')
    tkitemconfigure( line.list.win, l, background='Green', selectforeground='Green')
    old.l <- l }

  tksee( line.list.win, l)
})


".update.window.with.on.exit" <-
function( nlocal=sys.parent(), i, l) mlocal({
  l <- length( line.list)
  tkdelete( line.list.win, orig.line.list.length-1, prev.line.list.length-1)
#  tkinsert( line.list.win, 'end', line.list[ orig.line.list.length:l])
#  .Tcl( .Tcl.args( line.list.win, 'insert', 'end', line.list[ orig.line.list.length:l]))
  do.call( 'tkinsert', c( list( line.list.win, 'end'), 
      as.vector( line.list[ orig.line.list.length %upto% l])))

  if( prev.line.list.length > l)
    tkdelete( bp.win, l-1, prev.line.list.length-2)
  else if( prev.line.list.length < l)
    do.call( 'tkinsert', c( list( bp.win, 'end'), rep( '*', l-prev.line.list.length)))
#    .Tcl( .Tcl.args( bp.win, 'insert', 'end', rep( '*', l-prev.line.list.length)))
#    tkinsert( bp.win, 'end', rep( '*', l-prev.line.list.length))

# Blank any old breakpoints that would fall on non-breakpointable on-exit lines
  for( i in orig.line.list.length:l)
    tkitemconfigure( bp.win, i-1, foreground='white', selectforeground='white')
})


"add.numbers" <-
function( expr, width=options()$width, numbering=TRUE, cat.on.exit=FALSE,
    expr.offset=0, line.number.offset=0) {

  old.width <- options(width = 1000)$width
  if( cat.on.exit)
    cat.on.exit <- expression( cat( paste( format( names( line.list)), line.list, sep=':'), sep='\n'))[[1]]
  on.exit( { eval( cat.on.exit); options( width=old.width) })

  tab.width <- option.or.default( 'tab.width', 4)
  spaces <- function( n=1) paste( rep( ' ', n), collapse='')
  tab.sp <- spaces( tab.width)
  tabs <- function( more.or.less=0, exact=n.tabs + more.or.less) paste( rep( tab.sp, max( exact, 0)), collapse='')

# Utility functions
  assign( '[[', my.index)
  assign( '[[<-', my.index.assign)

  deparse1 <- function( x) {
      x <- deparse( x, width.cutoff=500)
      if( length( x)>3)
        x <- c( x[ 1], paste( x[ -c(1,length( x))], collapse='; '), x[ length( x)] )
      paste( x, collapse=' ')
    }
  ch <- function( ...) {
      stuff <- c( ...)
      stuff[ 1] <- stuff[ 1] + expr.offset
      paste( c( stuff, '.'), collapse=',') }
  add.to.last.line <- function( x)
      if( !is.na( x))
        line.list[ length( line.list)] <<- paste( line.list[ length( line.list)], x, sep='')
  make.indent <- function( n.in=1, loop=FALSE) {
        indents[ ch(i)] <<- n.in
        n.tabs <<- n.tabs + n.in
        if( loop)
          last.loop.tabs <<- c( last.loop.tabs, n.tabs)
      }
  debuggify.system.call <- function( nlocal=sys.parent()) mlocal( expr[[ i,1 ]] <- as.name( 'debug.' %&% as.character( expr[[ i, 1]])) )
  default.update.line.list <- function( nlocal=sys.parent()) mlocal( line.list <- c( line.list, tabs() %&% deparse1( expr[[ i]]) ) )

# Initialization
  if( is.a.function <- is.function( expr) ) {
    line.list <- deparse( do.call("args", list( expr)) )
    line.list <- line.list[ -length( line.list)] # the NULL at the end of "args" output
    line.list <- paste( line.list, collapse=' ') # you get 1 line of definition and that's it
    names(line.list) <- ""
    expr <- body( expr) }
  else
    line.list <- ''

# Next line wraps up expr to ensure that recursion will start OK.
# Might be better ways, but this is consistent with "evaluator"
  expr <- do.call( 'expression', list( expr))

  breakpoint <- vector("list", 0)
  n <- line.number.offset
  i <- 1
  n.tabs <- 1
  last.loop.tabs <- 0
  suffix <- structure( '', names=ch( 1))
  indents <- numeric( 0)

  while( length(i) ) {
    needs.a.number <- TRUE
    next.i <- numeric( 0)
    call.above <- as.character( expr[[ i[ -length( i)], 1]])

    if( i[ length( i)] != 1 && call.above=='switch') { # need to insert line with switch option
      add.to.last.line( ',')
      if( i[ length( i)] < length( expr[[ i[ -length( i)] ]]) )
        line.list <- c( line.list, tabs(-1) %&% "'" %&% names( expr[[ i[ -length( i)] ]])[ i[ length( i)] ] %&% "' = ")
    } # if inside "switch"

    if( mode( expr[[ i]])=='(' || !is.call( expr[[ i]]) ) { # first test because is.call( '(a)') is TRUE but mode='(' !!
  #   SYMBOL or PRIMITIVE element-- or '('ed thing
      anything.to.add <- deparse1( expr[[ i]])
      if( nchar( anything.to.add)) # won't be in e.g. empty { } or switch( ..., a=, ...)
        line.list <- c( line.list, tabs() %&% anything.to.add)
    } else {
         call.type <- expr[[ i, 1]]
         if( !is.name( call.type))
           call.type <- 'default' # to cope with e.g. x$fun( args)
         else
           call.type <- as.character( call.type)
         switch( call.type,
        '{' = { # }
            if( (length( i)==1) || call.above %in% c( 'if', 'switch', 'for', 'while', 'repeat') )
              add.to.last.line( ' {') # we are already indenting }
            else {
              line.list <- c( line.list, tabs() %&% '{') # a braced expression has been inserted for no good reason }
              make.indent() }
            needs.a.number <- FALSE
            automove <- FALSE
            suffix[ ch( i)] <- ' }' # {
            if( length( expr[[ i]]) == 1)  # empty brace would cause problems
              expr[[ i]] <- call( '{', NULL) # NULL brace has same effect }
            next.i <- c( i, 2)
          },
        '(' = { }, # unlikely here
        'if' = {
            if( call.above!='if' || i[ length( i)]!=4) {
              line.list <- c( line.list, tabs())
              make.indent()
            } # otherwise we've got "else-if" which has slightly different formatting
            add.to.last.line( 'if( ' %&% deparse1( expr[[ i, 2]]) %&% ')')
            next.i <- c( i, 3)
          },
        'switch' = {
            line.list <- c( line.list, tabs() %&% 'switch( ' %&% deparse1( expr[[ i, 2]]))
            make.indent(2)
            suffix[ ch( i)] <- ')'
            next.i <- c( i, 3)
          },
        'for' = {
            line.list <- c( line.list, paste( tabs(), 'for( ', expr[[ i, 2]], ' in ', deparse1( expr[[ i, 3]]), ' )', sep=''))
            make.indent( loop=TRUE)
            next.i <- c( i, 4)
          },
        'while' = {
            line.list <- c( line.list, paste( tabs(), 'while( ', deparse1( expr[[ i, 2]]), ' )', sep=''))
            make.indent( loop=TRUE)
            next.i <- c( i, 3)
          },
        'repeat' = {
              line.list <- c( line.list, tabs() %&% 'repeat')
              make.indent( loop=TRUE)
              needs.a.number <- FALSE
              next.i <- c( i, 2)
            },
        'break' = ,
        'next' = {
              line.list <- c( line.list, tabs( exact=last.loop.tabs[ length( last.loop.tabs)]-1 ) %&% call.type)
              debuggify.system.call()
            },
         'return' = {
              line.list <- c( line.list, deparse1( expr[[ i]])) # no indent
              debuggify.system.call()
            },
         'on.exit' = {
              default.update.line.list()
              debuggify.system.call()
            },
    #    DEFAULT: regular call
          default.update.line.list() 
      ) # switch
    } # call type

    if( needs.a.number) {
      n <- n + 1
      names(line.list)[ length( line.list)] <- format( c(n, 1000))[1]
      names( line.list)[ is.na( names( line.list))] <- '' # bug in R1.6.1
      if( numbering)
        breakpoint[[ ch(i) ]] <- is.a.function && (length(breakpoint) == 0)
    } # if needs.a.number

    if( length( next.i))
      i <- next.i
    else
      addnum.move.to.next.expr()
  } # big loop

# Placeholder for on.exit stuff:
  if( is.a.function) {
    line.list <- c( line.list, '###### ON EXIT ######') # un-numbered
    n <- n+1
    line.list <- c( line.list, structure( 'NULL', names=format( c( n, 1000))[1]))
    breakpoint[[ ch( 2)]] <- FALSE
  }

# Functions requiring a special debug version:
  expr[[1]] <- debug.mvb.subst( expr[[1]])
  sublist <- named( cq( nargs, sys.call, sys.parent, sys.on.exit, sys.function, match.call))
  sublist[] <- 'mvb.' %&% sublist
  sublist <- lapply( sublist, as.name) # list( nargs=as.name( 'mvb.nargs'), ...)
  expro <- substitute( substitute( e, sublist), list( e=expr[[1]], sublist=sublist))
  
#  expro <- substitute( substitute( e, sublist), list( e=expr[[1]], 
#    sublist=list( nargs=as.name( 'debug.nargs'), sys.call=as.name( 'mvb.sys.call'),   
#      sys.parent=as.name( 'mvb.sys.parent'))))
  expr[[ 1]] <- eval( expro)

# Tidy up
  ll <- names( line.list)
  ll[ is.na( ll)] <- '' # R1.6.1 bug
  ll <- ifelse( nchar( ll)>0, ll %&% ': ', spaces( 6))
  ll <- paste( ll, line.list, sep='')
  names( ll) <- names( line.list)
  class(ll) <- 'cat'

  invisible(list(expr = expr[[1]], breakpoint = breakpoint, line.list = ll, n = n))
}


"addnum.move.to.next.expr" <-
function( nlocal=sys.parent(), final) mlocal({
  final <- i[ length( i)]
  if( length( expr[[ i[-length(i)] ]]) <= i[ length( i)]) { # BACK UP 1 LEVEL
    i <- i[ -length( i)]
    if( !length( i))
return( local.return())

    add.to.last.line( suffix[ ch(i)])
    if( !is.na( this.indent <- indents[ ch( i)]))
      n.tabs <- n.tabs - this.indent
    if( is.call( expr[[ i]]) && as.character( expr[[ i, 1]]) %in% c( 'for', 'while', 'repeat') )
      last.loop.tabs <- last.loop.tabs[ -length( last.loop.tabs) ]

    addnum.move.to.next.expr()
  } else { # NORMAL
    if( final==3 & as.character( expr[[ i[ -length( i)], 1]])=='if' && length( expr[[ i[ -length( i)] ]])==4) # "ELSE"
      line.list <- c( line.list, tabs(-1) %&% 'else ')

    i[ length( i)] <- i[ length( i)] + 1
  }
})


"backtrack.to.loop" <-
function( expr, i) {
  assign( '[[', my.index)
  i.try <- clip( i)
  while( length( i.try) && (!is.call( expr[[ i.try]]) ||
      !(paste( as.character( expr[[ i.try, 1]]), collapse=' ') %in% c( 'for', 'while', 'repeat')) ) )
    i.try <- clip( i.try)

  i.try
}


"bp" <-
function(line.no, expr = TRUE, fname) do.in.envir( envir=find.debug.HQ( TRUE), { # NB can maybe call "bp" while not debugging
  .system. <<- TRUE
  expr <- do.call( 'substitute' , list( substitute( expr), list( F=FALSE))) # so displayed version will have no star

  repeat { # only to allow break
    # Get info on function being called
    if( missing( fname)) {
      this <- .frames.$actual[ nrow( .frames.)]
      if( !length( this)) {
        cat( 'bp: don\'t know which function to set breakpoint in\n')
  break }

      fname <- .frames.$function.name[ nrow( .frames.)]
      max.line.no <- length( get( 'breakpoints', envir=sys.frame( .frames.$debug[ nrow( .frames.)])))
      general <- line.no <= length( tracees[[ fname]]$breakpoint) # else it's in the 'on.exit' section-- specific to incarnation
    } else {
      general <- TRUE
      max.line.no <- length( tracees[[ fname]]$breakpoint) }

    if( is.null( tracees[[ fname]])) {
      cat( 'bp: no trace info for', fname, '\n')
  break }

    if( !(line.no %in.range% c( 1, max.line.no))) {
      cat( 'bp: out-of-range line number', line.no, 'for', fname, '\n')
  break }

    if( nrow( .frames.)) # ie live
      for( i in .frames.$debug[ .frames.$function.name==fname])
        set.a.breakpoint( expr, line.no, frame.number=i)

    if( general)
      tracees[[ fname]]$breakpoint[[line.no]] <<- expr # used to be substitute( expr)-- surely not??

  break
  }

  .nothing.
})


"check.for.tracees" <-
function( where=1) {
  o <- find.funs( where)
  if( !length( o))
return( character( 0))

  is.tracee <- function( x) {
    x <- get( x, pos=where)
    if( !my.index.exists( c( 2,2,1), body( x)))
  return( FALSE)
   
    bod <- my.index( body( x), 2, 2, 1)
    identical( as.name( 'evaluator'), bod) ||
    ( identical( as.name( 'mlocal'), bod) && identical( as.name( 'evaluator'), my.index( body( x), 2,2,2,1)))
  }  
  
  o[ sapply( o, is.tracee)]
}


"check.legality" <-
function( thing, call.type) do.in.envir( envir=find.debug.HQ( FALSE), {
# Trap non-logical first arguments to "if", "while", and non-subsettable arguments to "for" e.g. for( i in call( 'abc'))
#cat( 'Checking legality in call.type', call.type, 'of', thing, '\n')
  if( call.type %in% c( 'if', 'while') &&
      (typeof( thing) %in% dodgy.if.while.types || is.na( as.logical( thing)[ 1])) )
    message <- 'illegal if/while test'
  else if( call.type == 'for' && typeof( thing) %in% dodgy.for.counter.types)
    message <- 'illegal for-loop counter'
  else if( call.type == 'switch' && ( ! (typeof( thing) %in% valid.switch.types) || length( thing) != 1 ) )
    message <- 'illegal switch control argument'
  else
    return( TRUE)

  structure( .Data=FALSE, message=message)
})


"debug.break" <-
function() do.in.envir( envir=sys.frame( find.active.control.frame()), {
  .system. <<- TRUE
  .print.result. <<- FALSE

  i.try <- backtrack.to.loop( expr, i)
  if( !length( i.try))
stop( 'Not in a loop!')

  .evaluated.OK. <<- TRUE
  i <<- i.try
  move.to.next.expression( sorted.out=FALSE, nlocal=find.active.control.frame())

# Slightly weird construction: tell it to skip to itself!
  .skip. <<- TRUE
  .skipto. <<- i
  j # return previous value
})


"debug.do.call" <-
function( what, args) {
  funs.to.replace <- named( cq( break, next, return, on.exit, sys.on.exit)) # should this include "do.call"?!
  funs.to.replace[] <- 'debug.' %&% funs.to.replace
  new.what <- funs.to.replace[ what]
  if( !is.na( new.what))
    what <- new.what
  do.call( what, args)
}


"debug.mvb.subst" <-
function( expr) {
  sublist <- named( cq( nargs, sys.call, sys.parent, sys.function, sys.nframe, sys.on.exit, match.call))
  sublist[] <- 'mvb.' %&% sublist
  sublist <- lapply( sublist, as.name) # list( nargs=as.name( 'mvb.nargs'), ...)
  expro <- substitute( substitute( e, sublist), list( e=expr, sublist=sublist))
  eval( expro)
}


"debug.next" <-
function() do.in.envir( envir=sys.frame( find.active.control.frame()), {
  .system. <<- TRUE
  .print.result. <<- FALSE

  i.try <- backtrack.to.loop( expr, i)
  if( !length( i.try))
stop( 'Not in a loop!')

  .evaluated.OK. <<- TRUE
  i.try <- c( i.try, length( expr[[ i.try]]))
  i <<- i.try # from debug.break
  move.to.next.expression( sorted.out=FALSE, nlocal=find.active.control.frame()) # from debug.break

  .skip. <<- TRUE
  .skipto. <<- i
  j # return previous value
})


"debug.on.exit" <-
function( new.expr, add=FALSE) do.in.envir( envir=sys.frame( find.active.control.frame()), {
  if( i[1]>1)
stop( "Can't usefully call 'on.exit' while 'on.exit' is running!") # and return to D()> prompt

  if( !add)
    old <- list()
  else {
    old <- as.list( expr[[ 2]])
    if( !identical( old, list()))
      old <- old[ -c( 1, length( old))] # remove the starting brace and the final NULL
  }

  if( missing( new.expr))
    new.expr <- list()
  else
    new.expr <- substitute( new.expr)

  # Next line clunkily adds new.expr to existing on.exit code, all inside braces
  # Spare NULL at the end is to allow a stop after on.exit code has run
  new.expr <- as.call( c( list( as.name( '{')), old, new.expr, list( NULL)))
#  new.expr <- call( '{', old, new.expr, list( NULL))

  # Could redo the entire function here, but that can be slow
  adn <- add.numbers( new.expr, line.number.offset=orig.breakpoint.length-1, expr.offset=1)
  prev.line.list.length <- length( line.list)
  line.list <<- c( line.list[ 1:(orig.line.list.length-1)], adn$line.list)

  if( stopped.yet)
    .update.window.with.on.exit()

  # Check if any breakpoints were set in previous on.exit code. If so, set bp at start of new on.exit
  set.bp <- !all( sapply( FUN=identical, y=FALSE, breakpoints[ -(1:(orig.breakpoint.length-1))]))
  breakpoints <<- c( breakpoints[1:(orig.breakpoint.length-1)], adn$breakpoint)
  if( set.bp)
    set.a.breakpoint( TRUE, orig.breakpoint.length, find.active.control.frame())
  
  expr[[2]] <<- adn$expr # which has sorted out any breaks, null braces, etc.
  .nothing.
})


"debug.q" <-
function() do.in.envir( envir=find.debug.HQ( FALSE), {
#  .quit.debug. <<- TRUE
  cat( "To quit the debugger, type 'qqq()'\n")
  .nothing.
})


"debug.return" <-
function( ... ) do.in.envir( envir=sys.frame( find.active.control.frame() ), {
  if( i[1]>1)
stop( "Can't \"return\": no function to return from because \"on.exit\" code is executing")
  orig.mc <- mc <- as.list( match.call())[ -1]
  
  if( length( mc)) {
    if( length( mc)==1)
      mc <- eval( mc[[1]], envir=frame)
    else { # multiple arguments, so return as named list
      if( is.null( names( mc))) {
        which <- rep( TRUE, length( mc))
        names( mc) <- rep( '', length( mc))
      } else
        which <- names( mc)==''

      for( i in index( which))
        if( is.symbol( orig.mc[[ i]]))
          names( mc)[ i] <- as.character( orig.mc[[ i]] )
      mc <- lapply( mc, eval, envir=frame)
    }
  } else
    mc <- NULL

# Admin comes last, in case there's a crash in setting "mc"
  .skipto. <<- 2 # start the 'on.exit'
  .skip. <<- TRUE
#  .step. <<- FALSE # no hanging about at the return-value breakpoint
  .evaluated.OK. <<- TRUE

  mc
})


"debug.retval" <-
function() get( 'j', envir=sys.frame( find.active.control.frame()))


"debug.sys.on.exit" <-
function() do.in.envir( envir=sys.frame( find.active.control.frame()), {
  ex <- expr[[ 2]]
  if( length( ex)>1) { # it's been set via "debug.on.exit"
    ex <- as.list( ex)
    ex <- ex[ -c( 1, length( ex))] # brace & NULL
    if( length( ex)==1)
      ex <- ex[[ 1]]
    else
      ex <- as.call( c( list( as.name( '{')), ex))
  }
  ex
})


"dismiss.debug.window" <-
function( win) do.in.envir( envir=find.debug.HQ( FALSE), {
  if( m <- match( win, .frames.$tcl.title, 0))
    tkdestroy( get( 'tcl.win', envir=.frames.$debug[ m]))
})


"enact.command.r" <-
function( command, frame) do.in.envir( envir=find.debug.HQ( FALSE), {
  .evaluated.OK. <<- FALSE
  .system. <<- FALSE
  .print.result. <<- TRUE

  if( command=='') {
    .evaluated.OK. <<- TRUE
    .system. <<- TRUE
return() }

  .skip. <<- FALSE
  command <- try( list( parse( text=command)))
  if( command %is.not.a% 'try-error') {
    command <- command[[1]] # unwrap list in try
    if( length( command)) {
      command <- command[[1]] # parse returns expression(...)
      command <- do.call( 'substitute', list( command, list.of.command.subs))
      command <- debug.mvb.subst( command)
    #  print( command)
    #  cat( 'Mode=', mode( command), '\n')
      command <- try( list( eval( command, envir=frame)))

      if( command %is.not.a% 'try-error') 
        print.if.small( command[[1]]) # unwrap list from 'try'
       else
        .evaluated.OK. <<- FALSE # paranoid safety net; eOK _might_ have been set to TRUE before a crash!
    } else # length-0, presumably from line starting with a hash
      .system. <<- TRUE # so retval doesn't get set
  } # else try-error will be picked up on return
  
return( command) # to be unwrapped from 'try' list by 'evaluator'
})


"enter.on.exit" <-
function() get( 'orig.breakpoint.length', envir=sys.frame( find.active.control.frame()))


"eval.bp" <-
function(ex, envir) {
  try( {
    break.time <- eval( ex, envir=envir)
    if( length( break.time) != 1)
      break.time <- TRUE
    else
      break.time <- as.logical( break.time) })
  if( break.time %is.a% 'try.error') {
    cat( '\nInvalid breakpoint expression!\n')
    break.time <- TRUE }
  else if( is.na( break.time)) {
    cat( '\nBreakpoint evaluates to NA!\n')
    break.time <- TRUE }

  break.time
}


"eval.catching.errors" <-
function(i, envir) do.in.envir( envir=find.debug.HQ( FALSE), {
  # Thanks to Luke Tierney for this trick, which avoids "restart"
  j <- try( list( value=eval( i, envir=envir))) 
  .evaluated.OK. <<- j %is.not.a% 'try-error'
  if( .evaluated.OK.)
return( j$value) 
  else
return( NULL)
})


"evaluator" <-
function( fname) do.in.envir( envir=find.debug.HQ( TRUE), {
  on.exit( .end.incarnation())
  next.incarnation()

# Main loop through the expressions
  repeat {
    if( my.all.equal( i, 2) && in.body.code)
      retval <- j # we have finished the real body of the function, and will return this value
    else if( i[ 1]==3)
break # and then return

    if( i[ 1]!=1)
      in.body.code <- FALSE

    ch.i <- ch( i)
    .skip. <<- FALSE
    .skipto. <- 0

    repeat { # Figure out if user input is possible, and if so keep processing commands until told to continue debugger
      if( .quit.debug.) { # set by user's call to q(), mapped to 'q.debug'; this may be inside child
        cat( '\rNo ')
stop( 'merely quitting mvb\'s debugger') }
      
      .evaluated.OK. <<- TRUE
      .print.result. <<- FALSE
      find.line <- match( ch.i, names( breakpoints))
      if( !is.na( find.line))
        lno <- find.line
        
      if( !stop.here() || !.step.)
    break
    
      if( !stopped.yet) { # debug windows aren't launched until we actually stop for input (maybe never if no bp's)
        launch.debug.windows() # win=sdebug.window.name, fun=fname)
        stopped.yet <- TRUE }
      command <- interact()
      try.j <- enact.command.r( command, frame)
      if( try.j %is.not.a% 'try-error' && !.system.)
        j <- try.j[[1]]
      if( .evaluated.OK.) # mostly set to F even with valid command, to force repeat
    break

    } # user commands

    if( !.skip.) { # deal with next debuggee statement
#cat( ch( i), '  ') # these statements can be dehashed if you want to see which statements get looked at
      call.type <- get.call.type( expr[[ i]])
#      afei <- augment.for.eval( i, call.type)
#      if( !identical( afei, i))
#cat( ch( afei))
#cat( '\n')      
      
      if( call.type %in% c( 'normal', 'if', 'for', 'while', 'switch') ) { # alternatives are {} break next
        try.j <- eval.catching.errors( expr[[ augment.for.eval( i, call.type) ]], envir=frame)
        if( .evaluated.OK.) {
          j <- try.j
          if(.print.result.)
            print.if.small(j)
          .evaluated.OK. <<- check.legality( j, call.type) # illegal arg to if/while/for/switch
          if( !.evaluated.OK.)
            cat( 'Problem:', attr( .evaluated.OK., 'message'), '\n')
        }

        if( !.evaluated.OK.) { # back to outer loop and user's commands
          .step. <<- TRUE
  next } # jump straight back to getting user input
      } # not normal/if/for/while/switch
    } # not user-driven skip

    if( .skip.)
      skipto.debug()
    else
      move.to.next.expression()

  } # master loop

  if( in.body.code) { # I think this can only happen if the user intervenes heavily with "skip"
    cat( 'Function exited via "skip": return value may be strange\n')
    retval <- j }

  retval
})


"exit.on.exit" <-
function() length( get( 'breakpoints', envir=sys.frame( find.active.control.frame())))


"find.active.control.frame" <-
function() {
  dhq <- find.debug.HQ( FALSE)
  .frames. <- get( '.frames.', envir=dhq)
  .frames.[ nrow( .frames.), 'debug']
}


"find.debug.HQ" <-
function( create.if.not=TRUE ) {
  n.debug.HQ <- index( sapply( sys.frames(), 
      function( x) !is.null( attr( x, 'I.am.the.debug.HQ'))))
  if( length( n.debug.HQ))
    debug.HQ <- sys.frame( n.debug.HQ[ 1])
  else if( create.if.not) {
    debug.HQ <- parent.frame()
    attr( debug.HQ, 'I.am.the.debug.HQ') <- TRUE
    n.debug.HQ <- sys.parent()
    setup.debug.admin( nlocal=n.debug.HQ) }
  else # doesn't exist, not supposed to create it
return( FALSE)

  debug.HQ
}


"find.from" <-
function( char.fname, from=mvb.sys.parent(), look.for.generics=TRUE) {
  if( typeof( from)=='closure')
    from <- environment( from)
  else if( is.numeric( from)) 
    from <- if( from>0) sys.frames()[[ from]] else .GlobalEnv
   
  orig.from <- from
  
  repeat {
    found <- exists( char.fname, envir=from, inherits=FALSE)
    if (found || is.null( from)) 
  break
    from <- parent.env( from)
  }
  
  if( !found && look.for.generics) {
    # Perhaps it's a method; this from getAnywhere
    if( length( grep( '\\.', char.fname))) {
      parts <- strsplit( char.fname, '\\.')[[1]]
      for (i in 2 %upto% length(parts)) {
        gen <- paste( parts[1:(i - 1)], collapse = ".")
        ff <- find.from( gen, orig.from, look.for.generics=FALSE) # avoid nesting
        if( is.logical( ff) || !exists( '.__S3MethodsTable__.', ff, inherits=FALSE))
      next
      
        # else generic is defined in ff
        S3 <- get( '.__S3MethodsTable__.', ff)
        if( !exists( char.fname, envir=S3, inherits=FALSE))
      next # but no such method there
      
        found <- S3
      }
    }
        
    from <- found # FALSE if didn't find method
  }
  
  from
}


"get.retval" <-
function() do.in.envir( envir=sys.frame( find.active.control.frame()), 
 j
)


"go" <-
function(line.no) do.in.envir( envir=sys.frame( find.active.control.frame()), {
  set.global.debug.vars( .system.=TRUE)

  if( missing( line.no))
    temp.bp <<- ''
  else if( !is.numeric( line.no) || line.no[1] < 1) {
    cat("Go how far?\n")
return( .nothing.) }
  else {
    line.no <- trunc(line.no[1])
    l <- length( breakpoints)
    if( line.no>l ) {
      cat("Max. line number=", l, "\n")
return( .nothing.) }

    temp.bp <<- names(breakpoints)[line.no]
#    cat("Temp bp set at", temp.bp, "***\n")
  }

  set.global.debug.vars( .step.=FALSE, .evaluated.OK.=TRUE)
  .nothing.
})


"interact" <-
function( nlocal=sys.parent(), input) mlocal({
  .update.debug.window()

#  BELIEVED OBSOLETE:
#  if( exists( 'r.window.handle', 'mvb.session.info'))
#    push.to.foreground( r.window.handle)

  cat( '\nD(' %&% frame.number %&% ')> ')
  input <- readLines( n=1)
  if( nchar( input) && debug.command.recall) {
    cat( input, '\n', sep='', append=TRUE, file=debug.hist.file)
    loadhistory( debug.hist.file)
  }
  input
})


"launch.debug.windows" <-
function() do.in.envir( envir=find.debug.HQ( FALSE), {
  top <- tktoplevel( )
  for( i.win in index( !.frames.$has.window.yet)) { # i.e. not launched yet
#  cat( 'Launching', .frames.$window.name[ i.win], '\n')
    debug.env <- sys.frame( .frames.$debug[ i.win])
    line.list <- get( 'line.list', envir=debug.env)
    breakpoints <- get( 'breakpoints', envir=debug.env)
    nl <- length( line.list)
    tktitle( top) <- .frames.$window.name[ i.win]
    # TODO: define def.screen.pos to be a bit smarter about window placement
    # NB tkwinfo( 'screenwidth', top)
    screen.pos <- option.or.default( 'debug.screen.pos', '+5-5') # was +5-5
    font <- option.or.default( 'debug.font', 'Courier')
    height <- option.or.default( 'debug.height', 10)
    width <- option.or.default( 'debug.width', 120)
    
#    lapply( screen.pos, function( x) tkwm.geometry( top, x)) # to allow several calls

  #  buttons <- tkframe(top)
  #  tkpack(buttons, side="bottom", fill="x")#, pady="2m")
  #  go.button_ tkbutton( buttons, text='Run', command=function() { pushBack( 'hello', stdin(), newLine=TRUE); push.to.foreground( r.window.handle) })
  #  goto.button_ tkbutton( buttons, text='Run to selection', command=function() '27')
  #  tkpack( go.button, goto.button, side='left', expand=TRUE)

    # First create the objects, then link them with 'tkconfigure'
    listio <- tklistbox( top, font=font, bg='white', height=height, width=width, 
      setgrid=TRUE, borderwidth=0)
    show.bp <- tklistbox( top, font=font, fg='white', bg='white', selectforeground='blue', 
      height=height, setgrid=TRUE, width=1, borderwidth=0, takefocus=FALSE)
    yscrollio <- tkscrollbar( top)
    xscrollio <- tkscrollbar( top, orient='horizontal')

    tkconfigure( yscrollio,
        command=function(...) { 
          tkyview( listio,...); tkyview( show.bp, ...) } )
    tkconfigure( xscrollio, 
        command=function(...) tkxview( listio, ...))
    tkconfigure( listio, 
        yscroll=function(...) { 
          tkset( yscrollio,...); tkyview( show.bp, 'moveto', list(...)[[1]]) },
        xscroll=function(...) 
          tkset( xscrollio, ...)) 
    tkconfigure( show.bp, 
        yscroll=function(...) { 
          tkset( yscrollio,...); tkyview( listio, 'moveto', list(...)[[1]]) })

    # tkinsert doesn't allow length>1 vectors any more; also need to strip names. Hence:
    do.call( 'tkinsert', c( list( listio, 'end'), as.vector( line.list))) 
    do.call( 'tkinsert', c( list( show.bp, 'end'), rep( '*', nl)))

    # tkpack(buttons, side="bottom", fill="x")#, pady="2m")
    # tkpack( go.button, goto.button, side='left', expand=TRUE)

    tkpack( xscrollio, side='bottom', fill='x')
    tkpack( yscrollio, side='right', fill='y')
    tkpack( show.bp, listio, side='left', fill='both', expand=TRUE, ipadx=0)
    
lapply( screen.pos, function( x) tkwm.geometry( top, x)) # to allow several calls
    
    tkfocus( listio)
    tkselection.set( listio, 0)

    bp.list <- unlist( lapply( breakpoints, mark.bp), use.names=F)
    bps <- rep( FALSE, nl)
    bps[ names( line.list) != ''] <- bp.list
    for( i in index( bps))
      tkitemconfigure( show.bp, i-1, foreground='red', selectforeground='red')

    .Tcl( 'update') # not idletasks; worth a try

    # Wake up the window manager, avoiding redraw problems
    # Side-effect is to switch focus to the code window
    # May not be necessary on your system
    if( option.or.default( 'shakeup.debug.windows', FALSE)) {
      tkwm.withdraw( top)
      tkwm.deiconify( top)
    }

    if( FALSE && R.version$os=='mingw32' && exists( 'set.window.state') && is.loaded( 'enumerate_running')) { 
      # Circumvent TCL/TK bug that makes windows appear without text. Hopefully it's Windows-specific
      win.num <- windows.running( .frames.$window.name[ i.win])
      for( ij in c( 2, 1, 5))
        set.window.state( win.num, ij) }

  # Now put window pointers into the right debugging frame. This is clunky
    eval( substitute( {
        tcl.win <- top
        line.list.win <- listio
        bp.win <- show.bp }),
      envir=sys.frame( .frames.$debug[ i.win]))

  # and record successful launch
    .frames.$has.window.yet[ i.win] <<- TRUE
  }
#  cat( 'Launched OK\n')
})


"move.to.next.expression" <-
function( sorted.out=TRUE, nlocal=sys.parent()) mlocal({
# sorted.out=FALSE is used only after user calls break/next/skip
# Start with control decisions
  if( sorted.out) { # Normal behaviour: try to move into a loop etc.
    if(call.type == "for") {
      for.counters[[ ch.i]] <- j
      i <- c( i, 4)
      sorted.out <- FALSE } # so we set the counter
    else if( call.type == 'repeat')
      i <- c( i, 2)
    else if( call.type %in% c( 'if', 'while')) {
      if( as.logical( j)[ 1])
        i <- c(i, 3)
      else if( length( expr[[ i]])==4) # there's an ELSE
        i <- c( i, 4)
      else
        sorted.out <- FALSE }
    else if( call.type == 'switch') { # NB we have already guaranteed that length(j)==1 && j is char or numeric
      swlen <- length( expr[[ i]])
      if( is.numeric( j))
        jj <- floor( as.double( j)) # avoid complex floor
      else { # character match
        jj <- match( j, names( expr[[ i]]), NA)-2
        # If no match, move to "otherwise" if there is one; else leave it as NA 
        # Note that displayed value might be misleading
        if( is.na( jj) && names( expr[[ i]])[ swlen] == '') 
          jj <- swlen-2
      }
      
      if( is.na( jj) || jj<1 || jj>swlen-2) {
        j <- NULL
        sorted.out <- FALSE } # move to expr after switch
      else { # find first non-null expression at or after matched position
        jj <- jj+2
        while( jj<swlen && is.name( expr[[ i, jj]]) && 
            !nchar( as.character( expr[[ i, jj]])) )
          jj <- jj+1
        i <- c( i, jj)
      } }
    else if( call.type == '{') # }
      i <- c( i, 2)
    else
      sorted.out <- FALSE }

  if( sorted.out)
return( local.return())

# Now we try to move
  while( !my.all.equal( i, 1)) { # loop terminates when "sorted.out" and at viable statement
# cat( 'Main move loop: i=', ch( i), '\n' )
    i.parent <- clip( i)
    parent.call.type <- get.call.type( expr[[ i.parent]])

    if( sorted.out && i[ length( i)] <= length( expr[[ i.parent]]) )
return( local.return())

    if( parent.call.type %in% c( '{', 'expression') ) { # }
      if( sorted.out <- i[ length( i)] < length( expr[[ i.parent]]))
        i[ length( i)] <- i[ length( i)]+1
      else
        i <- i.parent } # now sorted.out= FALSE, so force a move
    else if( parent.call.type %in% c( 'while', 'repeat')) {
      i <- i.parent
      sorted.out <- TRUE } # and will definitely exit
    else if( parent.call.type == 'for') {
      ch.ip <- ch( i.parent)
      if( sorted.out <- length( for.counters[[ ch.ip]])) {
        val <- for.counters[[ ch.ip]][[ 1]]
        assign( as.character( expr[[ i.parent, 2]]), val, envir=frame)
        if( .step.) {
          cat( '\nFor-loop counter: ', expr[[ i.parent, 2 ]], 'Value:\n')
          print.if.small( val)
        }
        for.counters[[ ch.ip]] <- for.counters[[ ch.ip]][ -1 ] }
      else # if loop counter exhausted
        i <- i.parent } # sorted.out=FALSE so force a move
    else if( parent.call.type %in% c( 'if', 'switch')) { # can't see what else it might be
      i <- i.parent # but must move on
      sorted.out <- FALSE }
  }

# If we are here, then i==1 and we are meant to move to return-value breakpoint
  i <- 2
})


"mtrace" <-
function( fname, tracing=TRUE, char.fname=as.character( substitute( fname)), from=mvb.sys.parent()) {
# do.in.envir call REMOVED... gulp
# mtrace is "do.in.envir" (of its caller) so it can be called WHILE debugging another function
  assign( '[[', my.index)
  fname <- char.fname
  
  ff <- find.from( fname, from)

  if( !tracing && is.logical( ff)) # couldn't find
    f <- NULL # not completely useless; zap entry in 'tracees'
  else {
    if( is.logical( ff))
stop( "Can't find " %&% fname)
    f <- get( char.fname, ff)
    old.env <- environment( f)
    old.attr <- attributes( f)
  }

  if( tracing) {
    # Re-trace?
    if( length( body( f))>=2 && body( f)[[1]]=='{' && is.recursive( body(f)[[2]]) && body(f)[[2,1]]=='return' && length( body( f)[[2]])>1 &&
      ( (not.local <- body( f)[[2,2,1]]=='evaluator') || (body( f)[[2,2,1]]=='mlocal' && body(f)[[2,2,2,1]]=='evaluator')) ) {
      cat( 'Re-applying trace...\n') # next character is } to keep matcher happy!
      if( not.local)
        body( f) <- body( f)[[3]]
      else
        body( f) <- substitute( mlocal( x), list( x=body( f)[[3]]))
    }

    # mlocal or normal?
    if( is.recursive( body( f)) && body( f)[[1]]=='mlocal') {
      cc <- substitute( return( mlocal( evaluator( fname=this.fun.name))), list( this.fun.name=fname))
      body( f) <- body( f)[[2]] }
    else
      cc <- substitute( return( evaluator( fname=this.fun.name)), list( this.fun.name=fname) )

    this.tracee <- add.numbers( f)
    body( f) <- call( '{', cc, body( f) ) # } to keep matcher happy

    if( !exists.mvb( 'tracees', pos='mvb.session.info'))
      tracees <- list()
    else
      tracees <- get( 'tracees', pos='mvb.session.info')
    tracees <- tracees[ names( tracees) != fname]
    tracees <- c( tracees, structure( .Data=list(this.tracee), names=fname))
  } else {
    if( exists.mvb( 'tracees', pos='mvb.session.info')) 
      tracees <- get( 'tracees', pos='mvb.session.info') %without.name% fname
    else
      tracees <- list()
      
    if( is.recursive( body( f)) && body( f)[[1]]=='{' && length( body( f))>=2 && is.recursive( body( f)[[2]]) && 
        body(f)[[2,1]]=='return' && length( body( f)[[2]])>1) {
      if( body( f)[[2,2,1]]=='evaluator')
        body( f) <- list( body( f)[[3]]) # call to list seems harmless, and avoids problems with function() 9
      else if( body( f)[[2,2,1]]=='mlocal' && body(f)[[2,2,2,1]]=='evaluator')
        body( f) <- substitute( mlocal( x), list( x=body( f)[[3]])) }
    else 
      f <- NULL # don't bother saving f
  }

  if( !is.null( f)) {
    environment( f) <- old.env
    attributes( f) <- old.attr
#    if( !identical( old.env, .BaseNamespaceEnv))
#      do.call( '<<-', list( as.name( fname), f)) # should work regardless of where it was found
#    else
##    1.6.2: above doesn't work when environment= namespace:base
      locko <- bindingIsLocked( fname, ff)
      if( locko)
        unlockBinding( fname, ff)
      assign( fname, f, envir=ff)
      if( locko) {
        ow <- options( 'warn')
        on.exit( options( ow))
        options( warn=-1)
        lockBinding( fname, ff)
      }
  }
  
  put.in.session( tracees)
  invisible( f)
}


"mtrace.off" <-
function() {
  unmtrace <- function( m) mtrace( char.fname=m, tracing=F)
  if( exists( 'tracees'))
    sapply( names( tracees), unmtrace)
  sapply( check.for.tracees(), unmtrace) # any hangovers from save.image etc.
  invisible( NULL)
}


"next.incarnation" <-
function( nlocal=sys.parent()) mlocal({
# Set up debug admin. Already environmentalized so it'll find .frames. etc.
  trf <- try( get( 'tracees', 'mvb.session.info', inherits=FALSE))
  if( (trf %is.a% 'try-error') || !( fname %in% names( trf)))
stop( "No mtrace info for " %&% fname %&% 
    "; maybe saved before being un-mtraced?")
  trf <- trf[[ fname]] # match must be exact

  frame.number <- mvb.sys.parent( 1)
  frame <- sys.frame( frame.number)

  subframe <- sum( .frames.$actual==frame.number) # this is to do with "mlocal" functions
  if( subframe)
    subframe <- LETTERS[ subframe]
  else
    subframe <- ''
  .frames. <<- rbind( .frames., list( actual=frame.number, debug=mvb.sys.parent(0), function.name=fname, subframe=subframe,
      has.window.yet=FALSE, window.name= fname %&% '(' %&% frame.number %&% subframe %&% ')'))

# Doesn't handle .Last.debug in R version. In the S version, this attempts to shut down the Pascal window and then call the
# top .Last in the global environment, if it exists. This is of session-long duration (doesn't work in R as no frame 0)

  for.counters <- list()
  old.l <- -1 # shows which line is currently highlit
  lno <- 1
  breakpoints <- tracees[[ fname]]$breakpoint
  line.list <- tracees[[ fname]]$line.list
  orig.line.list.length <- length( line.list)
  orig.breakpoint.length <- length( breakpoints)
  temp.bp <- ''
#  expr_ do.call( 'substitute', list( tracees[[ fname]]$expr, list.of.command.subs)) # this should have been done by 'add.numbers'
  expr <- tracees[[ fname]]$expr
  expr <- do.call( 'expression', list( expr, quote( NULL), quote( NULL))) # wrap it up one level; matches 'add.numbers'
  stopped.yet <- FALSE
  in.body.code <- TRUE
  i <- 1
  j <- NULL

# Returns name of the window-- but I think this is obsolete
  .frames.$window.name[ nrow( .frames.)]
})


"print.if.small" <-
function(x, ...) {
  osx <- try( object.size( x), silent=TRUE)
  if( osx %is.a% 'try-error')
    osx <- NA
  if( !is.na( osx) && osx < option.or.default( 'threshold.debug.autoprint.size', 8192)) {
    try.to.print <- try( list( print(x, ...)))
    if( try.to.print %is.a% 'try-error')
      cat( "!! Couldn't successfully print result: ", c( try.to.print), "\n")
  } else {
    if(!is.null(class <- attr(x, "class")))
      cat("Class: ", class, " ")
    cat("Mode: ", mode(x), " ")
    cat("Length: ", length <- length(x), " ")
    if(!is.null(dim <- dim(x)))
      cat("dim: ", dim, " ")
    if(is.numeric(x))
      cat("Storage: ", storage.mode(x), " ")
    cat("Size: ", osx, "\n")
  }
}


"qqq" <-
function() do.in.envir( envir=find.debug.HQ( FALSE), {
  .quit.debug. <<- TRUE
  .nothing.
})


"README.debug" <-
function() 
  help( 'README.debug')


"screen.line" <-
function( l, nlocal=sys.parent()) mlocal(
  index( nchar( names( line.list))>0)[ l]-1 # tcl starts at 0, aaargh
)


"set.a.breakpoint" <-
function( bp.expr,line.no, frame.number=sys.parent()) do.in.envir( envir=sys.frame( frame.number), {
  breakpoints[[ line.no]] <<- bp.expr

  if( exists( 'bp.win', envir=sys.frame( frame.number), inherits=FALSE)) { # having cake + eating it
    colour <- if( mark.bp( bp.expr)) 'red' else 'white'
    tkitemconfigure( bp.win, screen.line( line.no), foreground=colour, selectforeground=colour)
  }
})


"set.global.debug.vars" <-
function( ...) {
  env <- find.debug.HQ( FALSE)
  l <- list( ...)
  for( i in names( l))
    assign( i, l[[ i]], envir=env)
}


"setup.debug.admin" <-
function( nlocal=sys.parent()) mlocal({
# "sys.parent()" is almost certainly NOT the appropriate value for 'nlocal'!
# Intended to be called only by 'find.debug.HQ'

  assign( '[[', my.index)
  assign( '[[<-', my.index.assign)
  .frames. <- empty.data.frame( actual=, debug=0, function.name=, window.name=, subframe='', has.window.yet=FALSE)
  .nothing. <- structure( 0, class='nullprint') # invisible object
  .step. <- .skip. <- .evaluated.OK. <- .system. <- .print.result. <- .in.users.commands. <- .quit.debug. <- FALSE
  .end.debug. <- NULL

  # TCL/TK stuff
  back.colour <- c( ' '='White', '*'='Red')
  select.colour <- c( ' '='Blue', '*'='Red')

  values.of.typeof <- cq( symbol, pairlist, closure, environment, promise, language, special,
      builtin, logical, integer, double, complex, character, '...', any, expression, list,
      externalptr)
  rogue.types <- c( 'for', 'while', 'repeat', 'if', 'switch', 'break', 'next', 'return', '{') # }
  dodgy.for.counter.types <-  cq( language, symbol, promise, environment, closure, '...', any, externalptr)
  dodgy.if.while.types <- cq( 'NULL', pairlist, closure, environment, promise, language, 
      special, builtin, '...', any, expression, list, externalptr)
  valid.switch.types <- cq( character, logical, integer, double, complex)
  
  # Command subs
  list.of.command.subs <- named( cq( break, next, return, q, on.exit, sys.on.exit, do.call))
  where.to.look <- 'package:debug'
  if( where.to.look %!in% search()) # 'debug' has been 'cd'ed up
    where.to.look <- match( 'debug', 
        sapply( 1:length( search()), 
            function( i) (names( attr( pos.to.env( i), 'path')) %&% '')[1]))
            
  if( exists( 'debug.next', where.to.look)) # normal
    subfun <- function( x) as.name( 'debug.' %&% x)
  else { # NAMESPACE 
    subfun <- function( x) call( ':::', quote( debug), as.name( 'debug.' %&% x))
    
    # Copy unexported functions into debug.HQ
    debug.namespace <- asNamespace( 'debug')
    funs <- ls( env=debug.namespace, all =TRUE) %except% find.funs( 'package:debug')
    funs <- funs[ sapply( funs, exists, env=debug.namespace, mode='function')]
    for( ifun in funs)
      assign( ifun, getFromNamespace( ifun, debug.namespace))
  }
    list.of.command.subs <- lapply( list.of.command.subs, subfun)
      
  # Command recall
  # Could implement several options-- recall debug commands only while in debugger, 
  # recall debug and non-debug but only while in debugger,
  # merge all debug commands with command-line commands.
  # For now, only the "merge all" option is implemented
  history.available <- function() { 
      df <- tempfile()
      sh <- try( savehistory( df), silent=TRUE)
      unlink( df)
      sh %is.not.a% 'try-error'
    }
    
  if( debug.command.recall <- option.or.default( 'debug.command.recall', 
      history.available())) {
    debug.hist.file <- tempfile()
    savehistory( debug.hist.file)
    # Next line would only be used by "recall debug commands only while debugging"
    # original.hist <- scan( debug.hist.file, what='', quiet=TRUE, sep='\n')
  }

  ch <- function( ...) paste( c( ..., '.'), collapse=',')
  star.or.space <- function( bpex) if( is.logical( bpex) && length( bpex)==1 && !is.na( bpex) && !bpex) ' ' else '*'
  mark.bp <- function( bpex) !( is.logical( bpex) && length( bpex)==1 && !is.na( bpex) && !bpex )
  augment.for.eval <- function( i, call.type) c( i, switch( call.type, 'for'=3, 'if'=, 'switch'=, 'while'=2, numeric( 0)) )
#        get.call.type_ function( ex) if( !is.call( ex) || mode( ex)=='(' || !( i_ match( as.character( ex[[ 1]]), rogue.types, 0))) 'normal' else rogue.types[ i]
  get.call.type <- function( ex) {
    if( !is.call( ex) || mode( ex)=='(' || # ')' 
        !( i <- match( paste( as.character( ex[[1]]), collapse=' '), rogue.types, 0)) ) { 
      if( !is.call( ex) && is.expression( ex))
        'expression'
      else
        'normal' }
    else
      rogue.types[ i]
  }

#       DON'T Make a copy of "tracees". This is really so that any on-the-fly traces aren't preserved
#        tracees_ get( 'tracees', pos='mvb.session.info')
  # push.to.foreground( r.window.handle)  # BELIEVED OBSOLETE
})


"skip" <-
function( line.no) do.in.envir( envir=sys.frame( find.active.control.frame()), {
# This is pretty easy & all we need to check, is that we don't try to move INTO a for-loop
  .system. <<- TRUE
  if( line.no<0)
    line.no <- lno-line.no
  if( line.no<0 || line.no > length( tracees[[ fname]]$breakpoint) ) {
    cat( "can't go there!")
return( .nothing.) }

  target <- names( tracees[[ fname]]$breakpoint)[ line.no]
  target <- as.numeric( strsplit( substring( target, 1, nchar( target)-1), ',')[[1]])

  mm <- min( length( i), length( target))
  id <- index( i[ 1:mm] != target[ 1:mm])[ 1]
  if( !is.na( id)) # mismatch at position id
    mm <- id-1
#  cat( 'MM=', mm, '\n')

  i.try <- i[ 1 %upto% mm]
  for( j in (mm+1) %upto% length( target))
    if( !is.call( expr[[ i.try]]) || expr[[ i.try, 1]] != 'for')
      i.try <- c( i.try, target[j])
    else {
      cat( "Can't skip into a new for-loop: stopping at the beginning of the for-loop instead\n")
return( .nothing.) }

# For some reason, I used to try to put .skipto. just before the expression to skip to, then (in skipto.debug)
# I would force a move. But this doesn't work. Don't know why-- I'm leaving the old code in case I had a good
# reason.
#  i.try[ length( i.try)]_ i.try[ length( i.try)] - 1 # we will force a move anyhow-- I think it works

  .skipto. <<- i.try
  .skip. <<- TRUE
  .evaluated.OK. <<- TRUE
return( .nothing.)
})


"skipto.debug" <-
function( nlocal=sys.parent()) mlocal({
  i <- .skipto. # See "skip". Old comment was: which had better be the statement BEFORE where you want to go
#  move.to.next.expression( FALSE) # FORCE a move; sets up loop
})


"stop.here" <-
function( nlocal=sys.parent()) mlocal({
# cat("i=", i, "\n")
# cat("stop at: ", names(tracees[[fname]]$breakpoint), "\n")
# cat("temp.bp", tracees[[fname]]$temp.bp, "*\n")
#  if( .skip.) {
#    .set.( .skip.=F, .step.=T)
#return( T) }

  which.bp <- names(breakpoints) == ch.i #
  if(any(which.bp)) {
#
#   Check for temporary & permanent breakpoints
#   cat( mode( temp.bp))
    if( temp.bp == ch.i)
      .step. <<- TRUE
    else
      .step. <<- eval.bp( breakpoints[ which.bp][[1]], envir=frame) || .step.
    # previous line changed 7/1/04 so that breakpoints are eval'd in step mode too

    if(.step.) # discard the temporary breakpoint
      temp.bp <- ''

#   cat("new .step.=", .step., "\n")
  }

  any(which.bp)
})
