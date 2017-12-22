

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
envir = ".GlobalEnv"
twsDisconnect(tws.connection)


snapShot <- function(twsCon, eWrapper, timestamp, file, ...){
    if(missing(eWrapper))
        eWrapper <- eWrapper()
    names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
    
    con <- twsCon[[1]]
    while(TRUE){

        if(!socketSelect(list(con), FALSE, NULL)) # infinte 'tries'
            break
        else {

            # browser()
            curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
      
            if(!is.null(timestamp))
                processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, ...)
            else 
                processMsg(curMsg, con, eWrapper, timestamp, file, ...)
            
            if(!any(sapply(eWrapper$.Data$data, is.na)))
                return(do.call(rbind, lapply(eWrapper$.Data$data, as.xts)))

        }  
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
                        duration    = duration)
    tzone(XTS) <- "Etc/GMT+5"   
    assign(symbol, OHLCV(XTS), envir = eval(parse(text = envir)))
    return(TRUE)        
}

connection =tws.connection

symbols = c("IBM:423423", "AMZN:34234", "AAPL:4234")


liveTWS <- function(connection, symbols, envr){

    # fetch symbol data
    tickers      <- sapply(strsplit(symbols, ":"), function(x) x[1])
    tws.tickers  <- lapply(tickers, IBrokers::twsSTK)

    reqRealTimeBars(connection, tws.tickers, eventWrapper = eWrapper.RealTimeBars.CSV(3), CALLBACK = snapShot)

    DATA.update <- reqRealTimeBars(connection, tws.tickers, eventWrapper = eWrapper.RealTimeBars(), CALLBACK = snapShot)
    

    DATA.old    <- get(symbol, envir = eval(parse(text = envir))) # need for signal calcs
    DATA.update <- OHLCV(DATA.update)
    DATA.new    <- to.weekly(rbind(DATA.old[-1, ], DATA.update)) # maintain same size of mktdata object

    colnames(DATA.new) <- colnames(DATA.old)
    return(DATA.new)

}  

for(ticker in tickers){

     print(ticker)
     tws.ticker  <- IBrokers::twsSTK(ticker)
     print(reqRealTimeBars(connection, tws.ticker, eventWrapper = eWrapper.RealTimeBars(), CALLBACK = snapShot))

}

eWrapper.snapshot <- function() {
  eW <- eWrapper(NULL)
  eW$assign.Data("EOD", FALSE)
  sapply(c("bidSize","bidPrice",
           "askPrice","askSize",
           "lastPrice",
           "Open","High","Low","Close",
           "lastSize","Volume","lastTimeStamp"), function(X) eW$assign.Data(X, NA))
  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...)
  {
    tickType = msg[3]
    if(tickType == .twsTickType$BID) {
      eW$assign.Data("bidPrice", as.numeric(msg[4]))
      eW$assign.Data("bidSize" , as.numeric(msg[5])) 
    } else
    if(tickType == .twsTickType$ASK) {
      eW$assign.Data("askPrice", as.numeric(msg[4]))
      eW$assign.Data("askSize" , as.numeric(msg[5])) 
    } else
    if(tickType == .twsTickType$LAST) {
      eW$assign.Data("lastPrice", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$OPEN) {
      eW$assign.Data("Open", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$HIGH) {
      eW$assign.Data("High", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$LOW) {
      eW$assign.Data("Low", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$CLOSE) {
      eW$assign.Data("Close", as.numeric(msg[4]))
    }
  }
  eW$tickSize <- function(curMsg, msg, timestamp, file, ...)
  {
    tickType <- msg[3]
    if(tickType == .twsTickType$LAST_SIZE) {
      eW$assign.Data("lastSize", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$VOLUME) {
      eW$assign.Data("Volume", as.numeric(msg[4]))
    }
  }
  eW$tickString <- function(curMsg, msg, timestamp, file, ...)
  {
    tickType <- msg[3]
    eW$assign.Data("lastTimeStamp", structure(as.numeric(msg[4]),
                                        class=c("POSIXt","POSIXct")))
  }
  eW$tickSnapshotEnd <- function(curMsg, msg, timestamp, file, ...)
  {
    eW$assign.Data("EOD", TRUE)
  }
  return(eW)
}

connection <- tws.connection <- IBrokers::twsConnect(port = port)
reqRealTimeBars(connection, lapply(tickers[1], twsSTK), eventWrapper = eWrapper(1))
twsDisconnect(connection)

reqMktData(connection, lapply(tickers, twsSTK), snapshot = TRUE)

reqMktData(connection, lapply(tickers, twsSTK), eventWrapper = eWrapper.snapshot())


if(snapshot == "1") {
  eventWrapper <- eWrapper.snapshot()
  while(1) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1)
    processMsg(curMsg, con, eventWrapper, NULL, file, ...)
    if(curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
    fullSnapshot <- rbind(fullSnapshot, data.frame(
                            lastTimeStamp=eventWrapper$get.Data("lastTimeStamp"),
                            symbol=symbol.or.local(Contract[[n]]),
                            #symbol=Contract[[n]]$symbol,
                            bidSize=eventWrapper$get.Data("bidSize"),
                            bidPrice=eventWrapper$get.Data("bidPrice"),
                            askPrice=eventWrapper$get.Data("askPrice"),
                            askSize=eventWrapper$get.Data("askSize"),
                            lastPrice=eventWrapper$get.Data("lastPrice"),
                            Volume=eventWrapper$get.Data("Volume"),
                            Open=eventWrapper$get.Data("Open"),
                            High=eventWrapper$get.Data("High"),
                            Low=eventWrapper$get.Data("Low"),
                            Close=eventWrapper$get.Data("Close")
                           ))
    break
    }
  }
if(n == length(Contract)) return(fullSnapshot)
}
ticker_id <- as.character(as.numeric(tickerId)+n)
symbols. <- c(symbols., symbol.or.local(Contract[[n]]))
#symbols. <- c(symbols., Contract[[n]]$symbol)
}
}


tickers = c('IBM', "AMZN")


    # add to old data
    # periodicty checks
    # remove last observation from old data
    # reassign}

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
    # init strategy(s) per symbol 
    for(symbol in symbols){
        
        symbol.arguments[[symbol]]  <- 
            quantstrat:::install.param.combo(
                strategy        = strategy,                             # global strategy 
                param.combo     = .get_symbol_args(symbol, parameters), # symbol strategy
                paramset.label  = names(strategy$paramsets))
        symbol.arguments[[symbol]]$paramsets <- NULL                    # avoid confusion
    }

    # init mktdata objects
    .initialize_mktdata(FUN = init.FUN, ... = ...) 
 

    .initialize_mktdata(FUN = init.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', duration = '1 Y', envir = ".GlobalEnv")
    mktdata = (get(symbol))

    while(TRUE){

        #IF OLHC wait until next bar period then run update
        # ?per symbol or as a group

        #IF BBO then continously querry for the bid ask spread and feed into apply rules

        # what until Sys.time reaches period for update - proc time

        # update across all contracts

        # if perido

       # if BBO trade on ticks vs if OHLC trade on bars

       for(symbol in symbols){
	       sret <- new.env(hash = TRUE)
           .update_mktdata(FUN = live.FUN, ...) # update the datafeed for symbol

        Cl(mktdata)

        mktdata  = cbind(IBM.Trading = rep(1, nrow(mktdata)), mktdata)

        # loop over indicators
        sret$indicators <- applyIndicators(
        	strategy 	= strategy, 
        	mktdata 	= mktdata, 
        	parameters 	= parameters
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
            parameters  = parameters
            # ... 
        )
        signals.num <- length(strategy$signals)
        signals.idx <- ncol(sret$signals) 

        if(inherits(sret$signals,"xts") && 
           nrow(mktdata) == nrow(sret$signals)){
            mktdata         <- sret$signals
            sret$signals    <- NULL
        }

        mktdata.current <- last(mktdata) # 'push' 

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

