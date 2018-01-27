# quantstrat

### Development branch :
* Adds patches and functionality that are not currently implemented in quantstrat/braverock;and
* Needs testing; and
* To view complete history, see closed pull requests.

#### [21 Decemember 2017]
* rule.subset modifies mktdata object directly
* goodaftertime orders supported
* 'applyStrategy' parameter's arugment supports lookup per symbol
* 'applyStrategy' now accepts symbols in its 'Symbols' argument 
* 'clone.orderbook' now removes orderbook history; that is, strip.history = TRUE does what it's supposed to

#### [22 Decemember 2017]
* 'clone.portfolio' no longer modifies the original portfolio object; aka .getPortfolio() method changed to getPortfolio()

#### [05 January 2018]
* moved sret variable in strategy.R inside the symbols for loop so that sret does not get overridden with last symbol

#### [06 January 2018]
* patch to goodaftertime orders so that indices not belonging to goodaftertime ordertypes are skipped and not evaluated

#### [27 January 2018]
* removed combine.results from apply.paramset and set verbosity to FALSE; these additive functions should be done outside of apply.paramset
