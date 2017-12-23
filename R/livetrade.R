

symbol = "IBM:342343"

# for each symbol generate strategy for symbol that installs

# pass symbols
# pass parameters 

parameters <- data.frame(
	Symbol = "IBM:342343",
	border_value.z_score = 1.96, 
	border_value.look_back = 254, 
	border_value.fixed = FALSE, 
	fear_factor.factor = 0.90, 
	days_factor.factor = 10
)

port = 7496
library(IBrokers)
tws.connection  <- IBrokers::twsConnect(port = port)
connection =tws.connection
envir = ".GlobalEnv"
twsDisconnect(tws.connection)



initTWS <- function(connection, symbol, barsize, duration, envir){

    barsize  <- match.arg(barsize, c("1 week", "1 day", "1 hour", "30 mins", "15 mins", "5 mins"))
    # duration <- match.arg(duration, paste(c("S", "D", "W", "M", "Y"), rep(1:1000)))

    combined <- unlist(strsplit(symbol, ":"))
    ticker   <- combined[1]
    # permno   <- combined[2]
    tws.ticker  <- IBrokers::twsSTK(ticker)
    XTS         <- IBrokers::reqHistoricalData(
                        conn        = connection, 
                        Contract    = tws.ticker,
                        barSize     = barsize,
                        duration    = duration)

    ## PUT CHECK HERE IF TICKER STOPPED TRADING

    XTS <- xts(
        x        = cbind(rep(1, nrow(XTS)), XTS), 
        dimnames = list(NULL, c(paste0(ticker, ".Trading"), dimnames(XTS)[[2]])))
    tzone(XTS) <- "Etc/GMT+5"  

    assign(symbol, TOHLCV(XTS), envir = eval(parse(text = envir)))
    return(TRUE)        
}

parameters = list(
    `IBM:423423` = list(look.back = 200, days.factor = 10, fear.factor = 0.80, z.score = 2.2), 
    `AMZN:34234` = list(look.back = 250, days.factor = 20, fear.factor = 0.20, z.score = 4.2), 
    `AAPL:4234` = list(look.back = 130, days.factor = 60, fear.factor = 0.80, z.score = 2.2))

symbol = "IBM:342343"
symbols = c("IBM:423423", "AMZN:34234", "AAPL:4234")


liveTWS <- function(connection, symbol, barsize, envir){

    barsize <- match.arg(barsize, 
      c("1 week", "1 day", "1 hour", "30 mins", "15 mins", "5 mins", "1 mins"))
    to.FUN <- switch(barsize, 
      "1 week"  = xts::to.weekly, 
      "1 day"   = xts::to.daily, 
      "1 hour"  = xts::to.hourly, 
      "30 mins" = xts::to.minutes30, 
      "15 mins" = xts::to.minutes15, 
      "5 mins"  = xts::to.minutes5, 
      "3 mins"  = xts::to.minutes3, 
      "1 mins"  = xts::to.minutes 
    )

    ticker      <- unlist(strsplit(symbol, ":"))[1]
    tws.ticker  <- lapply(ticker, IBrokers::twsSTK)
    DATA.old    <- get(symbol, envir = eval(parse(text = envir))) # need for signal calcs

    snapshot    <- reqMktData(connection, tws.ticker, snapshot = TRUE) # not supposed to do this, but works?

    ## PUT CHECK HERE IF TICKER STOPED TRADING

    DATA.update <- as.xts(OHLCV(snapshot), order.by = snapshot$lastTimeStamp)
    DATA.new    <- to.FUN(rbind(OHLCV(DATA.old[-1, ]), DATA.update), drop.time = FALSE) # maintain same size of mktdata object
    DATA.new    <- cbind(Trading = rep(1, nrow(DATA.new)), DATA.new)

    attributes(DATA.new)$updated <- snapshot$lastTimeStamp
    colnames(DATA.new) <- colnames(DATA.old)
    assign(symbol, DATA.new, envir = eval(parse(text = envir)))
    return(TRUE)
}  

init.FUN    = initTWS
live.FUN    = liveTWS

tradeStrategy <- function(
	strategy, 
	parameters	= NULL, 
	symbols 	= NULL,

    init        = initTWS,
    update      = liveIB,
    error       = c("stop", "pass"),

    period      = "30min",
	debug 		= FALSE, 
	gc 			= FALSE,
	rule.subset = NULL,
	mdenv 		= NULL,
	...
) 
{

    # turn into strategy object
    strategy         <- quantstrat:::must.be.strategy(strategy)
    symbol.arguments <- new.env(hash = TRUE)

    .check_FUN <- function(FUN){
        if(!is.function(FUN))
            FUN <- try(get(FUN, mode = 'function'))
            if(inherits(FUN, "try-error"))
                stop("Please specify a mktdata init or live function")
        else
            return(TRUE)
    }

    stopifnot(all(sapply(c(init.FUN, live.FUN), .check_FUN)))
    stopifnot(is.list(parameters))
    if(length(unique(sapply(parameters, length))) != 1)
        stop("Please specify the same number of parameters per symbol")

    if(!is.null(parameters) && !all(symbols %in% names(parameters)))
        stop("The symbols included in your portfolio do not all have accompanying parameters")
    strategy$paramsets <- NULL # no longer applicable

    if(is.null(symbols))
       symbols <- ls(.getPortfolio(portfolio)$symbols)

    streamMktdata <- function(FUN, ...){

        if(!any(names(as.list(match.call(expand.dots = TRUE))) %in% c("symbols", "symbol"))) 
            stop("You must specify a symbol or symbols argument")

        if(is.function(FUN)) 
            initFUN <- FUN
        else if(exists(FUN, mode = 'function'))
            initFUN <- get(FUN, mode = "function")
        else
            stop(paste0("Could not stream data because ", FUN, " could not be found"))
        
        # override defaults if specified and add dots
        args <- quantstrat:::modify.args(formals(initFUN), NULL, ..., dots = TRUE)
        temp <- do.call(initFUN, args, envir = parent.frame(1)) # run function
    }

    # init data stream
    # IF ON PERIOD 
    streamMktdata(FUN = init.FUN, ... = ...) 
    # streamMktdata(FUN = init.FUN, connection = tws.connection, symbol = symbols[3], barsize = '1 day', duration = '1 Y', envir = ".GlobalEnv")
    # (get(symbol))

    while(TRUE){

        for(symbol in symbols){

            sret <- new.env(hash = TRUE)
            streamMktdata(FUN = live.FUN, ...) # update the datafeed for symbol
            # streamMktdata(FUN = live.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', envir = ".GlobalEnv")

            if(!is.null(mdenv))
                envir <- if(is.function(mdenv)) mdenv else eval(parse(text = mdenv))
            else
                envir <- .GlobalEnv

            mktdata <- get(symbol, envir = envir)

            # loop over indicators
            sret$indicators <- applyIndicators(
            	strategy 	= strategy, 
            	mktdata 	= mktdata, 
            	parameters 	= parameters[[symbol]]
            	# ... 
            )

            if(inherits(sret$indicators,"xts") && 
               nrow(mktdata) == nrow(sret$indicators)){
                mktdata         <- sret$indicators
                sret$indicators <- NULL
            }
               
            # loop over signals
            sret$signals <- applySignals(
                strategy    = strategy, 
                mktdata     = mktdata, 
                parameters  = parameters[[symbol]]
                # ... 
            )
            signals.num <- length(strategy$signals)
            signals.idx <- ncol(sret$signals) 

            if(inherits(sret$signals,"xts") && 
               nrow(mktdata) == nrow(sret$signals)){
                mktdata         <- sret$signals
                sret$signals    <- NULL
            }

            print(tail(mktdata))
        }

    }
}

tradeStrategy(strategy =.strategy_name, parameters = parameters, )

        curIndex <- 1 
    ## push into env that stores mktdata for each symbol

        signal <- last(mktdata[ ,(signals.idx - signals.num + 1L):signals.idx])       
        if(!signal) # if no signal ==> move onto next symbol 
            next

        # signal so fire rules
        if(!any(unlist(sapply(strategy$rules, 
           function(x) sapply(x, function(x) x$path.dep)))))
            stop("Path indepedent rules not yet supported")
        sret$rules <- list()


         
tradeRules <- function(
    portfolio,
    symbol,
    mktdata,
    indicators,
    signals,
    parameters){

    # cut mktdata object to last signal // similar to dindex 
    last(mktdata)

    # only evaluate on current time t

    # get prices for the current signal
    if(is.BBO(mktdata)) {

        mktPrice <- list(
            stoplimit = list(
                posQty = mktdata[ ,has.Ask(mktdata, which = TRUE)[1]],
                negQty = mktdata[ ,has.Bid(mktdata, which = TRUE)[1]]
        ),
            limit = list(
                posQty = mktdata[ ,has.Ask(mktdata, which = TRUE)[1]],
                negQty = mktdata[ ,has.Bid(mktdata, which = TRUE)[1]]
        ),
            stoptrailing = list(
                posQty = getPrice(mktdata, prefer = 'offer')[ ,1],
                negQty = getPrice(mktdata, prefer = 'bid')[ ,1])
        )
    } else if (is.OHLC(mktdata)) {

        mktPrice <- list(
            stoplimit = list(
                posQty = mktdata[ ,has.Hi(mktdata, which = TRUE)[1]],
                negQty = mktdata[ ,has.Lo(mktdata, which = TRUE)[1]]
            ),
            limit = list(
                posQty = mktdata[ ,has.Lo(mktdata, which = TRUE)[1]],
                negQty = mktdata[ ,has.Hi(mktdata, which = TRUE)[1]]
            ),
            stoptrailing = list(
                posQty = getPrice(mktdata, prefer = 'close')[ ,1],
                negQty = getPrice(mktdata, prefer = 'close')[ ,1]
            )
        )
    } else { # univariate or something built with fn_SpreadBuilder

        prefer <- if(hasArg("prefer")) match.call(expand.dots = TRUE)$prefer else NULL
        mktPrice <- list(
            stoplimit    = list(price = getPrice(mktdata, prefer = prefer)[ ,1]),
            limit        = list(price = getPrice(mktdata, prefer = prefer)[ ,1]),
            stoptrailing = list(price = getPrice(mktdata, prefer = prefer)[ ,1])
        )
    }

    mktPrice$isOHLC <- is.OHLC(mktdata)
    mktPrice$isBBO  <- is.BBO(mktdata)

    ## loop through the rules and send the orders to the broker

    ## evaluate if the orders where closed?
    ## and get there results

    ## save mktdata object
    

}

         
         if(isTRUE(initBySymbol)) {
             if(hasArg(Interval)){
                 Interval <- match.call(expand.dots=TRUE)$Interval
                 if(!is.null(Interval)){
                     temp.symbol <- get(symbol) 
                     ep_args     <- blotter:::.parse_interval(Interval)
                     temp.symbol <- temp.symbol[endpoints(temp.symbol, on = ep_args$on, k = ep_args$k)]
                     if(hasArg(prefer)){
                         prefer      <- match.call(expand.dots=TRUE)$prefer
                         temp.symbol <- getPrice(temp.symbol, prefer=prefer)[,1]
                     }
                     assign(symbol, temp.symbol, envir = .GlobalEnv)
                 }
             } else {
                 rm(list = symbol)
                 gc()
             }
         }
             
         if(isTRUE(debug)) ret[[portfolio]][[symbol]]<-sret
         
         if(isTRUE(delorders)) .strategy[[paste("order_book",portfolio,sep='.')]][[symbol]]<-NULL #WARNING: This is VERY DESTRUCTIVE  

         if(isTRUE(gc)) gc()
       }
       
       # call updateStrategy
       if(isTRUE(updateStrat)) updateStrategy(strategy, portfolio, Symbols=symbols, ...=...)
       
     }
     
     if(isTRUE(debug)) return(ret)
}

