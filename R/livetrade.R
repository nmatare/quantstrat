



# for each symbol generate strategy for symbol that installs

# pass symbols
# pass parameters 
.strategy_name='fearVest'
load.strategy(.strategy_name) # load the desired strategy

port = 7496
library(IBrokers)
tws.connection  <- IBrokers::twsConnect(port = port)
connection =tws.connection
envir = ".GlobalEnv"
twsDisconnect(tws.connection)
twsDisconnect(connection)


tws.ticker <-IBrokers::twsSTK("AAPL")

reqIds(connection)

placeOrder(twsconn = connection, Contract = tws.ticker, 
    Order = twsOrder(orderId = reqIds(connection), 
        action = "BUY", totalQuantity = 100, orderType = "MKT"))



reqExecutions(connection, ExecutionFilter = twsExecutionFilter(clientId = '1'))
reqOpenOrders(connection) ## need to write eWrapper that gets the order prices
ac=reqAccountUpdates(connection)
as.character(ac[[1]]$AccountCode['value'])
twsPortfolioValue(reqAccountUpdates(connection))




reqOpenOrders.snapshot(connection)




## Class of instrument 

## needs to inherit attributes of FinancialInstrument
## mktdata
## position
## position limit
## open.orders
## 

FinancialInstrument::currency('USD')
symbol = "IBM:342343"



SYMBOL <- class_SymbolTrading(period = "1 min", connection)
SYMBOL$make_as_stock(ticker = "IBM", permno = "342343")
SYMBOL$methods()

SYMBOL$get_account_tws()
SYMBOL$.account
SYMBOL$get_position_tws()

SYMBOL$set_pos_limit(max.pos = 20, min.pos = -20)
SYMBOL$get_pos_limit()
SYMBOL$.pos_limit

SYMBOL$get_hist_data_tws(duration = "1 Y")

parameters = list(
    `IBM:423423` = list(look.back = 200, days.factor = 10, fear.factor = 0.80, z.score = 2.2), 
    `AMZN:34234` = list(look.back = 250, days.factor = 20, fear.factor = 0.20, z.score = 4.2), 
    `AAPL:4234` = list(look.back = 130, days.factor = 60, fear.factor = 0.80, z.score = 2.2))

symbols = c("IBM:423423", "AMZN:34234", "AAPL:4234")






convert_strategy(.strategy_name, "ruleSignal", osFUN = NULL)


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

            for(symbol in symbols) do(symbol)$get_hist_data() 
        }

        if(trace && curIndex != 1)
            message(paste0(" ... waiting for next ", period))

        if(!period %in% c("1 day")) LIVE$QUERY() # don't wait the next day to run     

        if(trace) 
            message(paste0(" ... streaming live data on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))     

        # trade the strategy
        for(symbol in symbols){

            sret <- new.env(hash = TRUE)
            do(symbol)$get_live_data() # updates internalu\y into $.DATA

            if(!is.null(mdenv))
                envir <- if(is.function(mdenv)) mdenv else eval(parse(text = mdenv))
            else
                envir <- .GlobalEnv

            mktdata <- get(symbol)

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

            # if there is no signal and no outstanding open orders, move onto next symbol 
            # if there are open orders process them at this timestamp
            signal = last(mktdata)[ ,signals.idx] == 0
            orders = do(symbol)$get_orders() == 0
            
            if(signal && orders) next 
            if(trace  && signal) 
                message(paste0(" ... signal generated for: ", 
                    symbol, ", executing rules"))
            if(trace  && orders)
                message(paste0(" ... open orders exist for: ", 
                    symbol, ", executing rules"))                

           # Check to make sure path independent is not attempted
            if(!any(unlist(sapply(strategy$rules, 
               function(x) sapply(x, function(x) x$path.dep)))))
                stop("Path independent rules not yet supported")

            sret$rules       <- list()

            tradeRules(
                symbol,
                mktdata,
                indicators,
                parameters,
            )

        }
    }
    
}

## init_setup, no price data. .strateg_name + live implementation; this is the portfolio object now

tradeStrategy(
    strategy =.strategy_name, parameters = parameters, symbols = c("IBM:423423", "AMZN:34234", "AAPL:4234"), 
    connection = tws.connection, period = "1 min", init = initTWS, update = liveTWS, 
    duration = '1 W', envir = ".GlobalEnv")

## transform the backtest strategy object with live functions
# function for market orders
# fnnctino for XYZ orders in each argument 

         
tradeRules <- function(
    symbol,
    mktdata,
    indicators,
    parameters,
    ...)
{

    timestamp = index(mktdata) # only one timestamp should be evaluated
    types <- sort(factor(names(strategy$rules), # order is important, obviously
        levels = c("pre", "risk", "order", "rebalance", 
                   "exit", "enter", "chain", "post")))

    for(type in types){ # fire the rules on the observation

        # rules are still processes in order but logic is passed onto the
        # trading platform; that is, the platform, given that the correct
        # parameters, should determine which orders are opened/closed/held

        if(length(strategy$rules[[type]] >= 1)){

            processRules(
                ruletypelist    = strategy$rules[[type]], 
                timestamp       = timestamp, 
                path.dep        = TRUE, # FALSE not yet supported 
                mktdata         = mktdata,
                # portfolio       = portfolio, 
                symbol          = symbol, 
                ruletype        = type, 
                # mktinstr        = mktinstr, 
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
        if(is.function(rule$name)){
            ruleFun <- rule$name

        } else {

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



signals_to_trades <- function(
    mktdata         = mktdata, 
    timestamp, 
    sigcol, 
    sigval, 
    ruletype, 
    symbol, 
    ordertype, 
    orderqty        = 0L, 
    tmult           = FALSE, 
    replace         = TRUE, 
    orderside       = NULL, 
    threshold       = NULL, 
    orderset        = NULL, 
    prefer          = NULL, 
    order.price     = NULL, 
    chain.price     = NULL, 
    osFUN           = NULL,
    label           = '', 
    time.in.force   = '',
    ...
)
{
    if(!is.function(osFUN)) # original osFUN should now be replaced w/ live vr.
        osFUN <- match.fun(osFUN)

    # curIndex is the 'update' number now
    curIndex <- eval(match.call(expand.dots = TRUE)$curIndex, parent.frame())

    if(curIndex > 0 && curIndex <= nrow(mktdata) && (ruletype =='chain' || # aka fire the rule
        (!is.na(mktdata[curIndex, sigcol]) && mktdata[curIndex, sigcol] == sigval)))
    {

        if(hasArg(prefer)) 
            prefer = match.call(expand.dots = TRUE)$prefer
        else 
            prefer = NULL

        if(is.null(orderside) & !isTRUE(orderqty == 0)) # aka figure out orderside
        {   
            curqty <- symbol$get_position_tws()

            if(curqty > 0)          # we have a long position
                orderside = 'long'
            else if(curqty < 0)     # we have a short position
                orderside = 'short'

            else                    # no current position, which way are we going?
                if(orderqty > 0) 
                    orderside = 'long'
                else
                    orderside = 'short'
        }
        
        if(orderqty == 'all')
            if(orderside == 'long') # we're flattenting a long position  
                tmpqty =  1
            else
                tmpqty = -1
        else
            tmpqty <- orderqty

        if(!is.null(order.price)){
          
            orderprice <- order.price
        
        } else if(!is.null(chain.price)){
            
            orderprice <- chain.price

        } else {

            orderprice <- symbol$get_quote(prefer) # get the last updated price of symbol
        }

        if(orderqty != 'all'){ # size the order correctly

            orderqty <- osFUN(
                            symbol      = symbol,
                            orderqty    = orderqty, 
                            ordertype   = ordertype, 
                            orderside   = orderside, 
                            ruletype    = ruletype, 
                            orderprice  = as.numeric(orderprice),
                            ...         = ...
            )
        }

        if(!is.null(orderqty) && orderqty!= 0 && length(orderprice))
        {
            ## ADD ORDER VIA TWS
        }
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


  