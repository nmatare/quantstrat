

symbol = "IBM:342343"

# for each symbol generate strategy for symbol that installs

# pass symbols
# pass parameters 
load.strategy(.strategy_name) # load the desired strategy

port = 7496
library(IBrokers)
tws.connection  <- IBrokers::twsConnect(port = port)
connection =tws.connection
envir = ".GlobalEnv"
twsDisconnect(tws.connection)



initTWS <- function(connection, symbol, period, duration, envir){

    barsize  <- match.arg(period, 
        c("1 week", "1 day", "1 hour", "30 mins", "15 mins", "5 mins", "1 min"))
    duration <- match.arg(duration, 
        c(paste0(1:356, " D"), paste0(1:52L, " W"), paste0(1:12, " M"), c("1 Y")))

    ticker     <- unlist(strsplit(symbol, ":"))[1]
    tws.ticker <- IBrokers::twsSTK(ticker)

    XTS         <- IBrokers::reqHistoricalData(
                        conn        = connection, 
                        Contract    = tws.ticker,
                        barSize     = barsize,
                        duration    = duration)

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


liveTWS <- function(connection, symbol, period, envir){
    browser()
    barsize <- match.arg(period, 
      c("1 week", "1 day", "1 hour", "30 mins", "15 mins", "5 mins", "1 min"))
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
    if(is.na(snapshot$lastTimeStamp)) 
        stop(paste0("Error pulling data for: ", symbol))

    DATA.update <- as.xts(OHLCV(snapshot), order.by = snapshot$lastTimeStamp)
    DATA.new    <- to.FUN(rbind(OHLCV(DATA.old[-1, ]), DATA.update), drop.time = FALSE) # maintain same size of mktdata object
    DATA.new    <- cbind(Trading = rep(1, nrow(DATA.new)), DATA.new)

    attributes(DATA.new)$updated <- snapshot$lastTimeStamp
    colnames(DATA.new) <- colnames(DATA.old)
    assign(symbol, DATA.new, envir = eval(parse(text = envir)))
    return(TRUE)
}  

strategy    = .strategy_name
period = "1 min"

tradeStrategy <- function(
	strategy, 
	parameters	= NULL, 
	symbols 	= NULL,

    init        = NULL,
    update      = NULL,


    period      = "30min",
	debug 		= FALSE, 
	gc 			= FALSE,
	rule.subset = NULL,
	mdenv 		= NULL,
    trace       = TRUE,
	...
) 
{

    # turn into strategy object
    strategy         <- quantstrat:::must.be.strategy(strategy)

    stopifnot(is.list(parameters))
    if(length(unique(sapply(parameters, length))) != 1)
        stop("Please specify the same number of parameters per symbol")

    if(!is.null(parameters) && !all(symbols %in% names(parameters)))
        stop("The symbols included in your portfolio do not all have accompanying parameters")
    strategy$paramsets <- NULL # no longer applicable

    if(is.null(symbols))
       symbols <- ls(.getPortfolio(portfolio)$symbols)

    suppressMessages( # load calendars for mktdata updates
        bizdays::load_rmetrics_calendars(lubridate::year(Sys.time())))

    fetchMktdata <- function(FUN, ...){

        if(!any(names(as.list(match.call(expand.dots = TRUE))) %in% c("symbol"))) 
            stop("You must specify a symbol argument to your function")

        if(!any(names(as.list(match.call(expand.dots = TRUE))) %in% c("period"))) 
            stop("You must specify a period(frequency) over which to trade your strategy")

        if(is.function(FUN)) 
            initFUN <- FUN
        else if(exists(FUN, mode = 'function'))
            initFUN <- get(FUN, mode = "function")
        else
            stop(paste0("Could not stream data because ", FUN, " could not be found"))

        # override defaults if specified and add dots
        args <- quantstrat:::modify.args(formals(initFUN), NULL, ..., dots = TRUE)
        do.call(initFUN, args, envir = parent.frame(1)) # run function
    }

    updateOn <- function(period){

        period <- match.arg(period, 
          c("1 day", "1 hour", "30 mins", "15 mins", "5 mins", "1 min"))

        if(exists("POLL")) # reduce CPU usuage by only looking every 'POLL' time periods
            Sys.sleep(POLL) 
  
        if(period == "1 day"){
            
            RUN <- lubridate::hour(Sys.time())   == 15L && # query 05 min before
                   lubridate::minute(Sys.time()) == 55L || # mktclose and 05 
                   lubridate::hour(Sys.time())   == 09L && # after mktopen
                   lubridate::minute(Sys.time()) == 35L
            POLL    <<- 60L
            SLEEP   <<- 60L * 60L * 6L + 60L * 20L # sleep for 6 hours 20 mins

        } else if(period == "1 hour"){
            
            RUN     <- lubridate::minute(Sys.time()) == 0  # queries  every hour
            POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds
            SLEEP   <<- 60L

        } else if(period == "30 mins"){
            
            RUN     <- lubridate::minute(Sys.time()) == 30 # queries  every 30min
            POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds
            SLEEP   <<- 60L * 30L

        } else if(period == "15 mins"){
            
            RUN     <- lubridate::minute(Sys.time()) == 30 # queries  every 15min
            POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds
            SLEEP   <<- 60L * 15L 

        } else if(period == "5 mins"){
            
            RUN     <- lubridate::minute(Sys.time()) == 5 # queries  every 5min
            POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds
            SLEEP   <<- 60L * 5L # sleep for 5 min

        } else if(period == "1 min"){
            
            RUN     <- round(lubridate::second(Sys.time())) == 0  # queries  every minute
            POLL    <<- (60 - floor(lubridate::second(Sys.time()))) / 60  # 'wakeup' the next 'POLL' seconds
            SLEEP   <<- 60L # sleep for 60 seconds

        } else 
            return(NA)

        return(RUN)
        # SLEEP is return as number of seconds to the parent.env()
        # POLL is return as number of seconds to the parent.env()
    }

    i = 1; fatal = FALSE # init parameters
    while(!fatal){ # run until chron job exits or fatal error is thrown

        if(trace && i == 1) 
            message(paste0(" ... starting live strategy every ", period))

        if(i == 1){ # init the data stream

            while(!updateOn(period)) NULL # will poll until TRUE and then release 

            if(trace) message(paste0(" ... initializing live data on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))   

            for(symbol in symbols) 
                fetchMktdata(
                    FUN     = init, 
                    symbol  = symbol, 
                    period  = period, 
                    ...     = ...
                )
                # fetchMktdata(FUN = init.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', duration = '1 Y', envir = ".GlobalEnv")
            i       = 2L    # index is now 2
            updateOn(period) # set the POLL to next period
        }

        if(trace && i != 1)
            message(paste0(" ... waiting for next ", period))

        while(!updateOn(period) && i != 1) NULL # will poll until TRUE and then release 

        if(trace) 
            message(paste0(" ... streaming live data on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))     

        # trade the strategy
        for(symbol in symbols){

            sret <- new.env(hash = TRUE)
            fetchMktdata( # update the datafeed for symbol
                FUN     = update, 
                symbol  = symbol, 
                period  = period, 
                ...     = ...
            ) 
            # streamMktdata(FUN = live.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', envir = ".GlobalEnv")

            if(!is.null(mdenv))
                envir <- if(is.function(mdenv)) mdenv else eval(parse(text = mdenv))
            else
                envir <- .GlobalEnv

            mktdata <- get(symbol, envir = envir)

            # loop over indicators
            sret$indicators <- applyIndicators(
              strategy    = strategy, 
              mktdata     = mktdata, 
              parameters  = parameters[[symbol]]
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
            # signals.num <- length(strategy$signals)
            # signals.idx <- ncol(sret$signals) 

            if(inherits(sret$signals,"xts") && 
               nrow(mktdata) == nrow(sret$signals)){
                mktdata         <- sret$signals
                sret$signals    <- NULL
            }

            if(trace) 
                message(paste0(" ... signal generated for: ", 
                    symbol, ", executing rules"))

            print(tail(mktdata))
        }

        i = i + 1 # corresponds to the number of updates    
    }
}

tradeStrategy(
    strategy =.strategy_name, parameters = parameters, symbols = symbols, 
    connection = tws.connection, period = "1 min", init = initTWS, update = liveTWS, 
    duration = '1 W', envir = ".GlobalEnv")


fetchMktdata(FUN = init.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', duration = '1 Y', envir = ".GlobalEnv")

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

