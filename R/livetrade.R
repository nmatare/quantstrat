

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
twsDisconnect(connection)


tws.ticker <-IBrokers::twsSTK("IBM")

reqIds(connection)

placeOrder(twsconn = connection, Contract = tws.ticker, 
    Order = twsOrder(orderId = reqIds(connection), 
        action = "BUY", totalQuantity = 100, orderType = "MKT"))



reqExecutions(connection, ExecutionFilter = twsExecutionFilter(clientId = '1'))
reqOpenOrders(connection) ## need to write eWrapper that gets the order prices
ac=reqAccountUpdates(connection)
as.character(ac[[1]]$AccountCode['value'])
twsPortfolioValue(reqAccountUpdates(connection))


e_open_order <- function(msg, contents, ...) {

  # same as IBrokers:::e_open_order expect removes EOF cat 
  eoo <- list(
         # need to add contractId to twsContract...
              contract   = IBrokers:::twsContract(
                             conId   = contents[3],
                             symbol  = contents[4],
                             sectype = contents[5],
                             expiry  = contents[6],
                             strike  = contents[7],
                             right   = contents[8],
                             exch    = contents[9],
                             currency= contents[10],
                             local   = contents[11],
                             combo_legs_desc = contents[66],
                             # the following are required to correctly specify a contract
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL,
                             multiplier = NULL
                           ),

              order      = IBrokers:::twsOrder(
                             orderId = contents[2],
                             action  = contents[12],                           
                             totalQuantity = contents[13],
                             orderType     = contents[14],
                             lmtPrice      = contents[15],
                             auxPrice      = contents[16],
                             tif           = contents[17],
                             ocaGroup      = contents[18],
                             account       = contents[19],
                             openClose     = contents[20],
                             origin        = contents[21],
                             orderRef      = contents[22],
                             clientId      = contents[23],
                             permId        = contents[24],
                             outsideRTH    = contents[25],
                             hidden        = contents[26],
                             discretionaryAmt = contents[27],
                             goodAfterTime = contents[28],
                             # skip deprecated amount contents[29]
                             faGroup       = contents[30],
                             faMethod      = contents[31],
                             faPercentage  = contents[32],
                             faProfile     = contents[33],
                             goodTillDate  = contents[34],
                             rule80A       = contents[35],
                             percentOffset = contents[36],
                             settlingFirm  = contents[37],
                             shortSaleSlot = contents[38],
                             designatedLocation = contents[39],
                             auctionStrategy = contents[40],
                             startingPrice = contents[41],
                             stockRefPrice = contents[42],
                             delta         = contents[43],
                             stockRangeLower = contents[44],
                             stockRangeUpper = contents[45],
                             displaySize   = contents[46],
                             blockOrder    = contents[47],
                             sweepToFill   = contents[48],
                             allOrNone     = contents[49],
                             minQty        = contents[50],
                             ocaType       = contents[51],
                             eTradeOnly    = contents[52],
                             firmQuoteOnly = contents[53],
                             nbboPriceCap  = contents[54],
                             parentId      = contents[55],
                             triggerMethod = contents[56],
                             volatility    = contents[57],
                             volatilityType = contents[58],
                             deltaNeutralOrderType = contents[59],
                             deltaNeutralAuxPrice  = contents[60],
                             continuousUpdate = contents[61],
                             referencePriceType = contents[62],
                             trailStopPrice     = contents[63],
                             basisPoints        = contents[64],
                             basisPointsType    = contents[65],
                             # part of contract #66
                             scaleInitLevelSize = contents[67],
                             scaleSubsLevelSize = contents[68],
                             scalePriceIncrement = contents[69],
                             clearingAccount = contents[70],
                             clearingIntent  = contents[71],
                             notHeld         = contents[72],
                             # this contingent on UnderComp Not Yet Available in IBrokers [74+]
                             # algoStrategy [75+]
                             whatIf          = contents[75]
                           ),

              orderstate = IBrokers:::twsOrderState(
                             status      = contents[76],
                             initMargin  = contents[77],
                             maintMargin = contents[78],
                             equityWithLoan = contents[79],
                             commission  = contents[80],
                             minCommission = contents[81],
                             maxCommission = contents[82],
                             commissionCurrency = contents[83],
                             warningText = contents[84]
                           )
         )
  eoo <- structure(eoo, class='eventOpenOrder')
  return(eoo)
}

e_order_status <- function(msg, contents, ...) {
   # same as IBrokers:::e_open_order expect removes EOF cat 
   eos <- list(orderId = contents[2],
               status  = contents[3],
               filled  = contents[4],
               remaining = contents[5],
               averageFillPrice = contents[6],
               permId = contents[7],
               parentId = contents[8],
               lastFillPrice = contents[9],
               clientId = contents[10],
               whyHeld  = contents[11]
  )
  eos <- structure(eos, class="eventOrderStatus")
  return(eos)
}

eWrapper.orders <- function(){

    eW <- eWrapper() # inherit all class components

    sapply(c( # init the field names  
        "symbol",
        "status",
        "orderId",
        "action",
        "conId",
        "clientId",
        "parentId",
        "ocaGroup",
        "ocaType",
        "permId",
        "account",
        "sectype",
        "exch",
        "orderType",
        "filled",
        "remaining",
        "totalQuantity",
        "minQty",
        "goodAfterTime",
        "goodTillDate",
        "tif",
        "expiry",
        "goodTillDate",
        "averageFillPrice",
        "lastFillPrice",
        "whyHeld",
        "lastTimeStamp"), function(x) eW$assign.Data(x, NULL)) 

    eW$orderStatus <- eW$openOrder <- # override default functions
        function(curMsg, msg, timestamp, file,  ...) {

        eW$assign.Data("lastTimeStamp", msg[4])

        if(curMsg == .twsIncomingMSG$OPEN_ORDER){

            eoo <- e_open_order(curMsg, msg, ...)

            eW$assign.Data("symbol", eoo$contract$symbol)
            eW$assign.Data("status", eoo$orderstate$status)
            eW$assign.Data("orderId", eoo$order$orderId)
            eW$assign.Data("action", eoo$order$action)
            eW$assign.Data("conId", eoo$contract$conId)
            eW$assign.Data("clientId", eoo$order$clientId)
            eW$assign.Data("parentId", eoo$order$parentId)
            eW$assign.Data("ocaGroup", eoo$order$ocaGroup)
            eW$assign.Data("ocaType", eoo$order$ocaType)
            eW$assign.Data("permId", eoo$order$permId)            
            eW$assign.Data("account", eoo$order$account)
            eW$assign.Data("sectype", eoo$contract$status)
            eW$assign.Data("exch", eoo$contract$exch)
            eW$assign.Data("orderType", eoo$order$orderType)
            eW$assign.Data("totalQuantity", eoo$order$totalQuantity)
            eW$assign.Data("minQty", eoo$order$minQty)           
            eW$assign.Data("goodAfterTime", eoo$order$goodAfterTime)
            eW$assign.Data("goodTillDate", eoo$order$goodTillDate)
            eW$assign.Data("tif", eoo$order$tif)
            eW$assign.Data("expiry", eoo$contract$expiry)
            eW$assign.Data("goodTillDate", eoo$order$goodTillDate)
        }

        if(curMsg == .twsIncomingMSG$ORDER_STATUS){

            eos <- e_order_status(curMsg, msg, ...) 
            eW$assign.Data("filled", eos$filled)
            eW$assign.Data("remaining", eos$remaining)
            eW$assign.Data("averageFillPrice", eos$averageFillPrice)
            eW$assign.Data("lastFillPrice", eos$lastFillPrice)
            eW$assign.Data("whyHeld", eos$whyHeld)
        }
    }

    return(eW)
}

twsconn=connection

reqOpenOrders.snapshot <- function(twsconn, ...){
    .reqAllOpenOrders(twsconn)
    con <- twsconn[[1]]

    eventWrapper    <- eWrapper.orders()
    orderStatus     <- list()

    while(TRUE){

        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eventWrapper)

        orderStatus[[eventWrapper$get.Data("permId")]] <- list(
            symbol        = eventWrapper$get.Data("symbol"),
            orderId       = eventWrapper$get.Data("orderId"),
            clientId      = eventWrapper$get.Data("clientId"),
            status        = eventWrapper$get.Data("status"),
            totalQuantity = eventWrapper$get.Data("totalQuantity"),
            filled        = eventWrapper$get.Data("filled"),
            remaining     = eventWrapper$get.Data("remaining"),
            parentId      = eventWrapper$get.Data("parentId"),
            whyHeld       = eventWrapper$get.Data("whyHeld")
        )
      
        if(curMsg == .twsIncomingMSG$OPEN_ORDER_END) break
    }

    orderStatus <- do.call(rbind.data.frame, orderStatus)
    return(orderStatus)
}




posTWS <- function(connection, symbol){

    account         <- reqAccountUpdates(connection)[[2]]
    ticker.position <- which(sapply(account, function(x) 
        x$contract$symbol) == symbol)

    if(!length(ticker.position)) return(0L) # no positions for the ticker

    pos <- as.numeric(account[[ticker.position]]$portfolioValue$position)
    return(pos)
}


## Class of instrument 

## needs to inherit attributes of FinancialInstrument
## mktdata
## position
## position limit
## open.orders
## 

class_symbolData    <- function() # new init function that does this for each symbol

class_symbolDefintions <- function() # add attributes

class_symbolTrading <- function() 


class_symbolTrading <- function(connection, symbol, envir, period, ...){

    as.instrument <- try(FinancialInstrument::getInstrument(
        symbol, silent = TRUE), silent = TRUE)

    PERIOD  <- match.arg(period, 
        c("1 week", "1 day", "1 hour", "30 mins", 
         "15 mins", "5 mins", "3 mins", "1 min", 
         "30 sec", "5 sec"))

    if(inherits(as.instrument, "try-error"))
        stop(paste0("instrument", symbol," not found, please create it first."))

    FI              <- new.env()
    FI$.mktdata     <- xts()
    FI$.account     <- data.table()
    FI$.instrument  <- as.instrument 

    # FI$get.account.coinbase   = # TO DO
    FI$get.account.tws  = function(connection, ...){

        account         <- IBrokers::reqAccountUpdates(connection)[[2]]
        ticker.position <- which(sapply(account, function(x) 
            x$contract$symbol) == FI$.instrument$identifers$ticker)

        if(!length(ticker.position)) 
            FI$.account <<- rbind(NULL, FI$.account)
        else 
            FI$.account <<- rbind(FI$.account, data.frame(timestamp = timestamp, 
                account[[ticker.position]]$portfolioValue))
    }

    FI$getPos.tws       = function(...){ 
        FI$get.account.tws(connection, ...) # get latest
        pos <- as.numeric(data.table::last(FI$.account)$position)
        return(pos)
    }

    FI$getData.tws      = function(connection, duration, ...){

        duration <- match.arg(duration, 
            c(paste0(1:356, " D"), 
              paste0(1:52L, " W"), 
              paste0(1:12,  " M"), c("1 Y")))

        hist.data   <- IBrokers::reqHistoricalData(
            conn        = connection, 
            Contract    = IBrokers::twsSTK(FI$.instrument$identifers$ticker),
            barSize     = barsize,
            duration    = PERIOD)

        hist.data <- xts(
            x        = cbind(rep(1, nrow(hist.data)), hist.data), 
            dimnames = list(NULL, c(
                paste0(symbol, ".Trading"), 
                paste0(symbol, gsub(FI$.instrument$identifers$ticker, "", 
                    dimnames(hist.data)[[2]]))
        )))

        tzone(hist.data) <- "Etc/GMT+5"  
        FI$.mktdata <<- TOHLCV(hist.data)
    }

    FI$getLive.tws      = function(connection, ...){
   
        to.FUN <- switch(PERIOD, 
                    "1 week"  = xts::to.weekly, 
                    "1 day"   = xts::to.daily, 
                    "1 hour"  = xts::to.hourly, 
                    "30 mins" = xts::to.minutes30, 
                    "15 mins" = xts::to.minutes15, 
                    "5 mins"  = xts::to.minutes5, 
                    "3 mins"  = xts::to.minutes3, 
                    "1 min"   = xts::to.minutes,
                    "30 sec"  = xts::to.period(k = 30L,   period = "seconds"),
                    "5 sec"   = stop("IBrokers has an 11 second delay on 
                        snapshot requests; a five second period 
                        would be meaningless"))

        # not supposed to do this, but works.
        snapshot <- IBrokers::reqMktData(connection, 
            IBrokers::twsSTK(FI$.instrument$identifers$ticker), snapshot = TRUE) 
    
        if(is.na(snapshot$lastTimeStamp)){ 
            message(paste0("Error pulling data for: ", FI$.instrument$identifers$ticker))
            assign("FATAL", TRUE, envir = .GlobalEnv)
        }

        update <- as.xts(OHLCV(snapshot), order.by = snapshot$lastTimeStamp)
        update <- to.FUN( # maintain same size of mktdata object
            rbind.xts(OHLCV(FI$.mktdata[-1, ]), update), drop.time = FALSE)
        # ?proper? instead of appending, grab everything and align to time?
        update              <- cbind(Trading = 1L, update)
        colnames(update)    <- colnames(FI$.mktdata)
        FI$.mktdata         <<- update
    }

    FI$getOrders.tws    = function(...){}

    FI$setPosLimit      = function(...){} 
    FI$getPosLimit      = function(...){}

    # Update the Class Methods
    FI$updateMktdata <- FI$getLive.tws 
    FI$updateMktdata <- FI$getData.tws  
    FI$getPos        <- FI$getPos.tws 
    FI$getOrders     <- FI$getOrders.tws

    ## define S3 methods so get knows how to get objects

}


parameters = list(
    `IBM:423423` = list(look.back = 200, days.factor = 10, fear.factor = 0.80, z.score = 2.2), 
    `AMZN:34234` = list(look.back = 250, days.factor = 20, fear.factor = 0.20, z.score = 4.2), 
    `AAPL:4234` = list(look.back = 130, days.factor = 60, fear.factor = 0.80, z.score = 2.2))

symbols = c("IBM:423423", "AMZN:34234", "AAPL:4234")


convertStrategy <- function(strategy, sizing, )  

strategy    = .strategy_name
period = "1 min"
trace=TRUE
init        = initTWS
update      = liveTWS

tradeStrategy <- function(
	strategy, 
	parameters	= NULL, 
	symbols 	= NULL,

    init.data   = NULL,
    update.data = NULL,

    get.position =  NULL,
    get.orders   =  NULL,
    place.orders =  NULL,



    period      = "30min",
	debug 		= FALSE, 
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

    runQuery <- function(period){

        update          <- new.env()
        update$period   <- match.arg(period, 
            c("1 day", "1 hour", "30 mins", "15 mins", "5 mins", "1 min"))
        curIndex <<- update$curIndex <- curIndex <- 1L # set first every curIndex

        update$.RUN   <- vector()
        update$.POLL  <- vector()
        update$QUERY  <- function(...){

            if(length(update$.POLL)) # wait until next polling period
                Sys.sleep(update$.POLL)
            # else first period
            if(update$period == "1 day"){
                
                .RUN     <<- 
                       lubridate::hour(Sys.time())   == 15L && # query 15 min before
                       lubridate::minute(Sys.time()) == 45L || # mktclose and 05 
                       lubridate::hour(Sys.time())   == 09L && # after mktopen
                       lubridate::minute(Sys.time()) == 35L
                .POLL    <<- 60L

            } else if(update$period == "1 hour"){
                
                .RUN     <<- lubridate::minute(Sys.time()) == 0  # queries  every hour
                .POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds
                       } else if(update$period == "30 mins"){
                
                .RUN     <<- lubridate::minute(Sys.time()) == 30 # queries  every 30min
                .POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds
                
            } else if(update$period == "15 mins"){
                
                .RUN     <<- lubridate::minute(Sys.time()) == 30 # queries  every 15min
                .POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds

            } else if(update$period == "5 mins"){
                
                .RUN     <<- lubridate::minute(Sys.time()) == 5 # queries  every 5min
                .POLL    <<- 60L - floor(lubridate::second(Sys.time())) # wake'up in the next 'POLL' seconds

            } else if(update$period == "1 min"){
                
                .RUN     <<- round(lubridate::second(Sys.time())) == 0  # queries  every minute
                .POLL    <<- (60 - floor(lubridate::second(Sys.time())))  # 'wakeup' the next 'POLL' seconds

            } else 
                assign("FATAL", TRUE, envir = .GlobalEnv)

            update$curIndex <<- update$curIndex + 1L    # update the index              
            assign("curIndex", update$curIndex, envir = .GlobalEnv)
        }

        # Set class and environment
        # curIndex <<- curIndex # assign the first ever index   
        # assign("curIndex", curIndex, envir = .GlobalEnv) # assign the first ever index        
        environment(update$QUERY) <- as.environment(update)
        class(update)             <- "update"
        invisible(update) 
    }


    FATAL   = FALSE
    LIVE    = runQuery(period) # init the live stream 
    while(!FATAL){ # run until chron job exits or fatal error is thrown

        if(trace && curIndex == 1) 
            message(paste0(" ... starting live strategy every ", period))

        if(curIndex == 1){ # init the data stream

            LIVE$QUERY() # wait until the top of the next period 

            if(trace) message(paste0(" ... initializing live data on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))   

            ## each symbol is of class('symbol') and it has several methods
            # get position
            # get limit
            # get price
            # get stats

            for(symbol in symbols) 
                fetchMktdata(
                    FUN     = init, 
                    symbol  = symbol, 
                    period  = period, 
                    ...     = ...
                )
                # for(symbol in symbols) fetchMktdata(FUN = initTWS, connection = tws.connection, symbol = symbol, period = period, duration = '1 W', envir = ".GlobalEnv")
        }

        if(trace && curIndex != 1)
            message(paste0(" ... waiting for next ", period))

        if(!period %in% c("1 day")) LIVE$QUERY() # don't wait the next day to run     

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
            # fetchMktdata(FUN = update, connection = tws.connection, symbol = symbol, barsize = '1 day', period = period, envir = ".GlobalEnv")

            if(!is.null(mdenv))
                envir <- if(is.function(mdenv)) mdenv else eval(parse(text = mdenv))
            else
                envir <- .GlobalEnv

            # browser()
            mktdata <- get(symbol, envir = envir)  ## NOW change to symbol$get('mktdata')

            # loop over indicators
            sret$indicators <- applyIndicators( ## NOW apply indicators to mktdata
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
            signals.num <- length(strategy$signals)
            signals.idx <- ncol(sret$signals) 

            if(inherits(sret$signals,"xts") && 
               nrow(mktdata) == nrow(sret$signals)){
                mktdata         <- sret$signals
                sret$signals    <- NULL
            }

            # if(last(mktdata)[ ,signals.idx] == 0) next # no signal generated

            ## AND CHECK THAT NO ORDERS ARE CLOSED  ## now query symbol to see if it has open orders

            if(trace) 
                message(paste0(" ... signal generated for: ", 
                    symbol, ", executing rules"))

           # Check to make sure path independent is not attempted
            if(!any(unlist(sapply(strategy$rules, 
               function(x) sapply(x, function(x) x$path.dep)))))
                stop("Path independent rules not yet supported")

            sret$rules       <- list()
            mktdataTimeStamp <- last(mktdata) # following original convention

            ## need to get positions from tracking portfolio so orders won't be triggered by osFUN


            print(tail(mktdata))
        }

    }
}

## init_setup, no price data. .strateg_name + live implementation; this is the portfolio object now

tradeStrategy(
    strategy =.strategy_name, parameters = parameters, symbols = c("IBM:423423", "AMZN:34234", "AAPL:4234"), 
    connection = tws.connection, period = "1 min", init = initTWS, update = liveTWS, 
    duration = '1 W', envir = ".GlobalEnv")

fetchMktdata(FUN = init.FUN, connection = tws.connection, symbol = symbol, barsize = '1 day', duration = '1 Y', envir = ".GlobalEnv")

## transform the backtest strategy object with live functions
# function for market orders
# fnnctino for XYZ orders in each argument 

         
applyRules.live <- function(
    portfolio,
    symbol,
    mktdata,
    indicators,
    signals,
    parameters,
    ...)
{

    # get prices for the current signal
    # if(is.BBO(mktdata)) {

    #     mktPrice <- list(
    #         stoplimit = list(
    #             posQty = mktdata[ ,has.Ask(mktdata, which = TRUE)[1]],
    #             negQty = mktdata[ ,has.Bid(mktdata, which = TRUE)[1]]
    #     ),
    #         limit = list(
    #             posQty = mktdata[ ,has.Ask(mktdata, which = TRUE)[1]],
    #             negQty = mktdata[ ,has.Bid(mktdata, which = TRUE)[1]]
    #     ),
    #         stoptrailing = list(
    #             posQty = getPrice(mktdata, prefer = 'offer')[ ,1],
    #             negQty = getPrice(mktdata, prefer = 'bid')[ ,1])
    #     )
    # } else if (is.OHLC(mktdata)) {

    #     mktPrice <- list(
    #         stoplimit = list(
    #             posQty = mktdata[ ,has.Hi(mktdata, which = TRUE)[1]],
    #             negQty = mktdata[ ,has.Lo(mktdata, which = TRUE)[1]]
    #         ),
    #         limit = list(
    #             posQty = mktdata[ ,has.Lo(mktdata, which = TRUE)[1]],
    #             negQty = mktdata[ ,has.Hi(mktdata, which = TRUE)[1]]
    #         ),
    #         stoptrailing = list(
    #             posQty = getPrice(mktdata, prefer = 'close')[ ,1],
    #             negQty = getPrice(mktdata, prefer = 'close')[ ,1]
    #         )
    #     )
    # } else { # univariate or something built with fn_SpreadBuilder

    #     prefer <- if(hasArg("prefer")) match.call(expand.dots = TRUE)$prefer else NULL
    #     mktPrice <- list(
    #         stoplimit    = list(price = getPrice(mktdata, prefer = prefer)[ ,1]),
    #         limit        = list(price = getPrice(mktdata, prefer = prefer)[ ,1]),
    #         stoptrailing = list(price = getPrice(mktdata, prefer = prefer)[ ,1])
    #     )
    # }

    mktinstr        <- getInstrument(symbol)
    mktPrice$isOHLC <- is.OHLC(mktdata)
    mktPrice$isBBO  <- is.BBO(mktdata)

    timestamp = index(mktdata) # only one timestamp should be evaluated
    types <- sort(factor(names(strategy$rules), # order is important, obviously
        levels = c("pre", "risk", "order", "rebalance", 
                   "exit", "enter", "chain", "post")))

    # fire the rules on the observation
    for(type in types){

        # rules are still processes in order but logic is passed onto the
        # trading platform; that is, the platform, given that the correct
        # parameters, should determine which orders are opened/closed/held

        if(length(strategy$rules[[type]] >=  1)){

            processRules(
                ruletypelist    = strategy$rules[[type]], 
                timestamp       = timestamp, 
                path.dep        = TRUE, # FALSE not yet supported 
                mktdata         = mktdata,
                portfolio       = portfolio, 
                symbol          = symbol, 
                ruletype        = type, 
                mktinstr        = mktinstr, 
                parameters      = parameters, 
                curIndex        = curIndex, # curIndex is 'update' in live implementation
                ...)
        } else 

         next()
    }

    if(debug)
        return(mktdata)
    else
        return()
}

switchOp <- function(backtestFUN, liveFUN){

    if(is.function(liveFUN)) 
        liveFUN <- liveFUN
    else if(exists(liveFUN, mode = 'function'))
        liveFUN <- get(liveFUN, mode = "function")
    else
        stop(paste0("Could not stream data because ", FUN, " could not be found"))    
}

processRules <- function(
    ruletypelist, 
    path.dep, 
    ruletype, 
    timestamp   = NULL, 
    parameters  = NULL, 
    ...)
{
    
    for(rule in ruletypelist){
        if(!rule$path.dep == path.dep) next()
        if(is.function(rule$name))
            ruleFun <- rule$name
        else {
            if(exists(rule$name, mode = "function"))

                ruleFun <- get(rule$name, mode = "function")
            
            else {
                rule.name <- paste("rule", rule$name, sep=".")

                if(exists(rule.name, mode = "function")) {

                    ruleFun     <- get(rule.name, mode = "function")
                    rule$name   <- rule.name

                } else {

                    message("Skipping rule ", rule$name,
                            " because there is no function by that name to call")
                    next()
                }
            }
        }

        if(!isTRUE(rule$enabled)) next()

        # modify a few things
        rule$arguments$timestamp    = timestamp
        rule$arguments$ruletype     = ruletype
        rule$arguments$label        = rule$label

        # replace default function arguments with rule$arguments
        .formals <- formals(rule$name)
        .formals <- modify.args(.formals, rule$arguments,   dots = TRUE)
        .formals <- modify.args(.formals, parameters,       dots = TRUE)
        .formals <- modify.args(.formals, NULL, ...,        dots = TRUE)
        .formals$`...` <- NULL
        
        if(!is.null(rule$arguments$prefer)) 
            .formals$prefer = rule$arguments$prefer
        tmp_val <- do.call(ruleFun, .formals, envir = parent.frame(1))

    } #end rules loop
} 

ruleSignal <- function(
    mktdata=mktdata, 
    timestamp, 
    sigcol, 
    sigval, 
    orderqty=0, 
    ordertype, 
    orderside=NULL, 
    orderset=NULL, 
    threshold=NULL, 
    tmult=FALSE, 
    replace=TRUE, 
    delay=0.0001, 
    osFUN='osNoOp',
    pricemethod=c('market','opside','active'), 
    portfolio, 
    symbol, 
    ..., 
    ruletype, 
    TxnFees=0, 
    prefer=NULL, 
    sethold=FALSE, 
    label='', 
    order.price=NULL, 
    chain.price=NULL, 
    time.in.force=''
)
{
    if(!is.function(osFUN)) # original osFUN should now be replaced w/ live vr.
        osFUN <- match.fun(osFUN)

    # curIndex is the 'update' number now
    curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())

    if(curIndex > 0 && curIndex <= nrow(mktdata) && (ruletype=='chain' || 
        (!is.na(mktdata[curIndex,sigcol]) && mktdata[curIndex,sigcol]==sigval)))
    {
        #calculate order price using pricemethod
        pricemethod<-pricemethod[1] #only use the first if not set by calling function

        if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
        else prefer = NULL

        #if(hasArg(TxnFees)) TxnFees=match.call(expand.dots=TRUE)$TxnFees
        #else TxnFees=0

        # compute threshold
        if(!is.null(threshold))
        {
            if(!is.numeric(threshold))
            {
                # threshold should be the name of an indicator column in mktdata

                col.idx <- grep(threshold, colnames(mktdata))

                if(length(col.idx) < 1)
                    stop(paste('no indicator column in mktdata matches threshold name "', threshold, '"', sep=''))
                if(length(col.idx) > 1)
                    stop(paste('more than one indicator column in mktdata matches threshold name "', threshold, '"', sep=''))

                threshold <- as.numeric(mktdata[curIndex,col.idx])
            }
        }

        if(is.null(orderside) & !isTRUE(orderqty == 0))
        {
            curqty<-getPosQty(Portfolio=portfolio, Symbol=symbol, Date=timestamp)
            if (curqty>0 ){
                #we have a long position
                orderside<-'long'
            } else if (curqty<0){
                #we have a short position
                orderside<-'short'
            } else {
                # no current position, which way are we going?
                if (orderqty>0) 
                    orderside<-'long'
                else
                    orderside<-'short'
            }
        }
        
        if(orderqty=='all'){
            if (orderside=='long'){
                #we're flattenting a long position
                tmpqty <-  1
            } else {
                tmpqty <- -1
            }
        } else {
            tmpqty <- orderqty
        }

    if(!is.null(order.price))
    {
        orderprice <- order.price
    }
    else if(!is.null(chain.price))
    {
        orderprice <- chain.price
    }
    else
    {
        # switch(pricemethod,
        #     market = ,
        #     opside = ,
        #     active = {
        #         if(is.BBO(mktdata)){
        #         if (tmpqty>0) 
        #             prefer='ask'  # we're buying, so pay what they're asking
        #         else
        #             prefer='bid'  # we're selling, so give it to them for what they're bidding  
        #         } 
        #         orderprice <- try(getPrice(x=mktdata[curIndex,], prefer=prefer)[,1]) 
        #     },
        #     passive =,
        #     work =,
        #     join = {
        #         if(is.BBO(mktdata)){
        #         if (tmpqty>0) 
        #             prefer='bid'  # we're buying, so work the bid price
        #         else
        #             prefer='ask'  # we're selling, so work the ask price
        #         }
        #         orderprice <- try(getPrice(x=mktdata[curIndex,], prefer=prefer)[,1]) 
        #     },
        #     maker = {
        #         if(hasArg(price) & length(match.call(expand.dots=TRUE)$price)>1) {
        #         # we have prices, just use them
        #         orderprice <- try(match.call(expand.dots=TRUE)$price)
        #         } else {
        #         if(!is.null(threshold)) {
        #             baseprice <- last(getPrice(x=mktdata[curIndex,])[,1]) # this should get either the last trade price or the Close
        #             if(hasArg(tmult) & isTRUE(match.call(expand.dots=TRUE)$tmult)) {
        #             baseprice <- last(getPrice(x=mktdata[curIndex,])[,1]) # this should get either the last trade price or the Close
        #             # threshold is a multiplier of current price
        #             if (length(threshold)>1){
        #                 orderprice <- baseprice * threshold # assume the user has set proper threshold multipliers for each side
        #             } else {
        #                 orderprice <- c(baseprice*threshold,baseprice*(1+1-threshold)) #just bracket on both sides
        #             }
        #             } else {
        #             # tmult is FALSE or NULL, threshold is numeric
        #             if (length(threshold)>1){
        #                 orderprice <- baseprice + threshold # assume the user has set proper threshold numerical offsets for each order
        #             } else {
        #                 orderprice <- c(baseprice+threshold,baseprice+(-threshold)) #just bracket on both sides
        #             }
        #             }
        #         } else{
        #             # no threshold, put it on the averages?
        #             stop('maker orders without specified prices and without threholds not (yet?) supported')
        #             if(is.BBO(mktdata)){

        #             } else {

        #             }
        #         }
        #         }
        #         if(length(orderqty)==1) orderqty <- c(orderqty,-orderqty) #create paired market maker orders at the same size
        #     }
        # ) # end switch

        if(inherits(orderprice,'try-error')) orderprice<-NULL
        if(length(orderprice)>1 && pricemethod!='maker') orderprice <- last(orderprice[timestamp])
        if(!is.null(orderprice) && !is.null(ncol(orderprice))) orderprice <- orderprice[,1]
    }

        if(is.null(orderset)) orderset=NA
        
        ## now size the order
        #TODO add fancy formals matching for osFUN
        if(orderqty!='all')
        {
            orderqty <- osFUN(strategy=strategy, 
                              data=mktdata, 
                              timestamp=timestamp, 
                              orderqty=orderqty, 
                              ordertype=ordertype, 
                              orderside=orderside, 
                              portfolio=portfolio, 
                              symbol=symbol,
                              ...=...,
                              ruletype=ruletype, 
                              orderprice=as.numeric(orderprice))
        }

        if(!is.null(orderqty) && orderqty!=0 && length(orderprice))
        {
                addOrder(portfolio=portfolio, 
                         symbol=symbol, 
                         timestamp=timestamp, 
                         qty=orderqty, 
                         price=as.numeric(orderprice), 
                         ordertype=ordertype, 
                         side=orderside, 
                         orderset=orderset, 
                         threshold=threshold, 
                         status="open", 
                         replace=replace , 
                         delay=delay, 
                         tmult=tmult, 
                         ...=..., 
                         prefer=prefer, 
                         TxnFees=TxnFees,
                         label=label,
                               time.in.force=time.in.force)
        }
    }
    if(sethold) hold <<- TRUE
}




    

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


    # fire the rules on the observation
    for(type in types){
        switch(type,
                pre = 
                {
                    if(length(strategy$rules[[type]]) >= 1){
                        ruleProc.live(
                            ruletypelist    = strategy$rules$pre, 
                            timestamp       = timestamp, 
                            path.dep        = TRUE, # FALSE not yet supported 
                            mktdata         = mktdata,
                            portfolio       = portfolio, 
                            symbol          = symbol, 
                            ruletype        = type, 
                            mktinstr        = mktinstr, 
                            parameters      = parameters, 
                            # curIndex        = curIndex, # remove arg
                            ...
                        )
                    }
                },
                risk = 
                {
                    if(length(strategy$rules$risk) >= 1){
                        ruleProc.live(
                            ruletypelist    = strategy$rules$risk,
                            timestamp       = timestamp, 
                            path.dep        = TRUE, # FALSE not yet supported 
                            mktdata         = mktdata,
                            portfolio       = portfolio, 
                            symbol          = symbol, 
                            ruletype        = type, 
                            mktinstr        = mktinstr,
                            parameters      = parameters, 
                            # curIndex        = curIndex, 
                            ...
                        )
                    }
                },
                order = 
                {

                    stop("Not yet supported")
                    if(length(strategy$rules[[type]]) >= 1){

                        # will open a new market order
                        # ruleProc.live( 
                        #     ruletypelist    = strategy$rules[[type]],
                        #     timestamp       = timestamp, 
                        #     path.dep        = FALSE, 
                        #     mktdata         = mktdata,
                        #     portfolio       = portfolio, 
                        #     symbol          = symbol, 
                        #     ruletype        = type, 
                        #     mktinstr        = mktinstr, 
                        #     parameters      = parameters, 
                        #     # curIndex        = curIndex, 
                        #     ...
                        # )

                    } else {

                        # else will close any orders that have not yet been filled
                        # if(isTRUE(TRUE)) # only path depedent currently supported
                        #     timespan <- format(timestamp, "::%Y-%m-%d %H:%M:%OS6") #may be unecessary
                        # else
                        #     timestamp=NULL
                        # live function would query orderinfascture and search for closed orders
                        # closed.orders <- ruleOrderProc(
                        #     portfolio   = portfolio, 
                        #     symbol      = symbol, 
                        #     mktdata     = mktdata, 
                        #     timestamp   = timestamp, 
                        #     periodicity = freq, 
                        #     # curIndex    = curIndex, 
                        #     ...
                        # )
                    }
                },
                exit = ,enter = 
                {

                    if(length(strategy$rules[[type]]) >= 1){
                        ruleProc.live(
                            ruletypelist    = strategy$rules[[type]],
                            timestamp       = timestamp, 
                            path.dep        = TRUE, 
                            mktdata         = mktdata,
                            portfolio       = portfolio, 
                            symbol          = symbol, 
                            ruletype        = type,
                            mktinstr        = mktinstr, 
                            parameters      = parameters, 
                            # curIndex        = curIndex, 
                            ...
                        )
                    }
                },
                # if its an exit or enter it's not connected to anything
                # but need

                chain = 
                {
                    # if it's chained it's connected to another order
                    if(!is.null(closed.orders))
                    {
                        # determine which closed orders are chained to an entry
                        chain.rules      <- strategy$rules[[type]]
                        chain.rule.names <- sapply(chain.rules, '[[', 'parent')
                        closed.chain     <- closed.orders[closed.orders$Rule %in% chain.rule.names]
                        # loop over each closed order and call ruleProc() on each rule
                        for(i in seq_len(nrow(closed.chain))) {
                            rules <- chain.rules[chain.rule.names %in% closed.chain$Rule[i]]
                            for(j in seq_along(rules)){
                                                               
                                txns        <- getTxns(Portfolio=portfolio, Symbol=symbol, Dates=timestamp)
                                txn.price   <- last(txns$Txn.Price)   # last() because there may be more than one txn at this timestamp

                                ruleProc.live(
                                    ruletypelist    = rules[j], 
                                    timestamp       = timestamp, 
                                    path.dep        = TRUE, 
                                    mktdata         = mktdata, 
                                    portfolio       = portfolio, 
                                    symbol          = symbol, 
                                    ruletype        = type, 
                                    mktinstr        = mktinstr, 
                                    parameters      = list(chain.price = txn.price)#, 
                                    # curIndex        = curIndex
                                )
                            }
                        }
                    }
                },
                post = 
                {
                   
                    if(length(strategy$rules$post) >= 1){
                        ruleProc.live(
                            ruletypelist    = strategy$rules$post,
                            timestamp       = timestamp, 
                            path.dep        = TRUE, 
                            mktdata         = mktdata,
                            portfolio       = portfolio, 
                            symbol          = symbol, 
                            ruletype        = type, 
                            mktinstr        = mktinstr, 
                            parameters      = parameters, 
                            # curIndex        = curIndex, 
                            ...)
                    }
                }
        )
    }