

symbol = "ISIS:43861"

# for each symbol generate strategy for symbol that installs

# pass symbols
# pass parameters 

parameters <- data.frame(
	Symbol = "ISIS:43861",
	border_value.z_score = 1.96, 
	border_value.look_back = 254, 
	border_value.fixed = FALSE, 
	fear_factor.factor = 0.90, 
	days_factor.factor = 10
)

port = 7496
library(IBrokers)
tws.connection  <- IBrokers::twsConnect(port = port)
envir = ".GlobalEnv"

snapShot <- function(twsCon, eWrapper, timestamp, file, playback = 1, ...){
    if(missing(eWrapper))
        eWrapper <- eWrapper()
    names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
    
    con <- twsCon[[1]]
    while(TRUE){

        socketSelect(list(con), FALSE, 2L) # 2 sec timeout
        curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        if(!length(curMsg)) 
            stop("Timeout")           
        if(!is.null(timestamp))
            processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, ...)
        else 
            processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        
        if(!any(sapply(eWrapper$.Data$data, is.na)))
            return(do.call(rbind, lapply(eWrapper$.Data$data,as.data.frame)))
    }
}

initTWS <- function(connection, symbol, barsize, duration, envir){

    combined <- unlist(strsplit(symbol, ":"))
    ticker   <- combined[1]
    # permno   <- combined[2]
    tws.ticker  <- IBrokers::twsSTK(ticker)
    XTS         <- IBrokers::reqHistoricalData(
                        conn        = connection, 
                        Contract    = tws.ticker,
                        barSize     = barsize,
                        duration    = duration
    )
    assign(symbol, OHLCV(XTS), envir = eval(parse(text = envir)))
    return(TRUE)        
}

connection =tws.connection
liveTWS <- function(connection, symbol, envr){

    # fetch symbol data
    combined    <- unlist(strsplit(symbol, ":"))
    ticker      <- combined[1]
    ticker = "SPY"
    # permno      <- combined[2]
    tws.ticker  <- IBrokers::twsSTK(ticker)

    DATA.old <- get(symbol, envir = eval(parse(text = envir))) # need for signal calcs

    reqRealTimeBars(connection, tws.ticker, eventWrapper = eWrapper.data(1), CALLBACK = snapShot)

    # add to old data
    # periodicty checks
    # remove last observation from old data
    # reassign
}

tradeStrategy <- function(
    query,
	strategy, 
	parameters	= NULL, 
	symbols 	= NULL,

    init        = initTWS,
    update      = liveIB,
    report      = ,
    error       = ,

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

    init.FUN    = initTWS
    live.FUN    = liveTWS

    .check_FUN <- function(FUN){
        if(!is.function(FUN))
            FUN <- try(get(FUN, mode = 'function'))
            if(inherits(FUN, "try-error"))
                stop("Please specify a mktdata init and live function")
        else
            return(TRUE)
    }

    stopifnot(all(sapply(c(init.FUN, live.FUN), .check_FUN))) 

    # must pass parameters as a data.frame that corresponds to 
    stopifnot(is.data.frame(parameters))
    # need to check data.frame

    .initialize_mktdata <- function(FUN, ...){

        if(!is.function(FUN))
            stop("You must pass a user specified function to init data")

        # override defaults if specified and add dots
        args <- quantstrat:::modify.args(formals(FUN), NULL, ..., dots = TRUE)
        temp <- do.call(FUN, args, envir = parent.frame(1)) # run function
    }

    .get_symbol_args <- function(symbol, param.combo){

        # look up parameters specific to symbol
        stopifnot(is.data.frame(param.combo))
        param.labels        <- colnames(param.combo)
        symbol.index        <- match("Symbol", param.labels)
        symbol.parameters   <- param.combo[
            param.combo$Symbol == symbol, -symbol.index]
        
        if(nrow(symbol.parameters) != 1) 
            stop(paste("Could not find parameters for", symbol))

        return(symbol.parameters)
    }

   
    symbol = "IBM:342343"
    # initalize 
    for(symbol in symbols){
        
        # user defined function to init mktdata for each symbol
        .initialize_mktdata(FUN = init.FUN, ... = ...)      # first init data
        symbol.arguments[[symbol]]  <- 
            quantstrat:::install.param.combo(
                strategy        = strategy,                             # global strategy 
                param.combo     = .get_symbol_args(symbol, parameters), # symbol strategy
                paramset.label  = names(strategy$paramsets))
    }
 

    .initialize_mktdata(FUN = init.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', duration = '1 Y', envir = ".GlobalEnv")
    get(symbol)

    while(TRUE){

        # what until Sys.time reaches period for update - proc time

        # update across all contracts

        # if perido

       for(symbol in symbols){
	       sret <- new.env(hash = TRUE)
           .update_mktdata(FUN = live.FUN, ...) # update the datafeed for symbol

        # apply indicators
        # apply signals
        # apply trade logic

       }

       # record errors
       # do book keeping in new account and portfolio
       # send bookkeep
       # sleep for period

    }



	for(symbol in symbols){

   



        # get current mktdata for the symbol
        mktdataFUN <- ### DO HERE TO PULL RECENT DATA ###
        mktdata <- simfeed(DATA)

## get mktdata on period, given backfill
## run on this


        symbol.strategy

        combined <- unlist(strsplit(symbol, ":"))
        ticker   <- combined[1]
        ticker      <- "IBM"
        permno   <- combined[2]

        
        tws.ticker      <- IBrokers::twsSTK(ticker)


        

        symbol = 
        data_AAPL = reqHistoricalData(tws, symbol)
        print (data_AAPL)


        mktdta <- requestLive(, ...) # how to query data

        # loop over indicators
        sret$indicators <- applyIndicators(
        	strategy 	= strategy, 
        	mktdata 	= mktdata, 
        	parameters 	= parameters, 
        	... 
        )
         
        if(inherits(sret$indicators,"xts") && 
        	nrow(mktdata) == nrow(sret$indicators)){
        mktdata 			<- sret$indicators
        sret$indicators 	<- NULL
        }
         
        # loop over signal generators
        sret$signals <- applySignals(strategy=strategy, mktdata=mktdata, parameters=parameters, ... )

        if(inherits(sret$signals,"xts") & nrow(mktdata)==nrow(sret$signals)){
        mktdata<-sret$signals
        sret$signals<-NULL
        }

         # fire rules if signal else move onto next symbol
         
         #loop over rules  
         sret$rules<-list()
             
         # only fire nonpath/pathdep when true
         # TODO make this more elegant
         pd <- FALSE
         for(i in 1:length(strategy$rules)){  
           if(length(strategy$rules[[i]])!=0){
            z <- strategy$rules[[i]] 
                if(z[[1]]$path.dep==TRUE)
                    pd <- TRUE
            }
         }
         
         sret$rules$nonpath<-applyRules(portfolio=portfolio, 
                                        symbol=symbol, 
                                        strategy=strategy, 
                                        mktdata=mktdata, 
                                        Dates=NULL, 
                                        indicators=sret$indicators, 
                                        signals=sret$signals, 
                                        parameters=parameters,  
                                        ..., 
                                        path.dep=FALSE,
                                        rule.subset=rule.subset,
                                        debug=debug)
         
         # Check for open orders
         rem.orders <- suppressWarnings(getOrders(portfolio=portfolio, symbol=symbol, status="open")) 
         if(NROW(rem.orders)>0){pd <- TRUE}
         if(pd==TRUE){sret$rules$pathdep<-applyRules(portfolio=portfolio, 
                                                     symbol=symbol, 
                                                     strategy=strategy, 
                                                     mktdata=mktdata, 
                                                     Dates=NULL, 
                                                     indicators=sret$indicators, 
                                                     signals=sret$signals, 
                                                     parameters=parameters,  
                                                     ..., 
                                                     path.dep=TRUE,
                                                     rule.subset=rule.subset,
                                                     debug=debug)}
         
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

