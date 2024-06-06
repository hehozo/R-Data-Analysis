library(tidyverse)

# current FX rates against the Euro (using current values instead of historical)
fxRates <- read_delim("fxRatesEuro.txt", delim = "\t", col_names = c("currency","fromEuro","toEuro"))
# Euro needs to be added to this file to make future calcs make sense
fxRates <- add_row(fxRates, currency = "Euro", fromEuro = 1, toEuro = 1)

# total values invoiced per currency across the two full years of my employment 2022 and 2023
ismTotals <- read_csv("ism_currency_totals.csv")

# we want to be able to calculate the total value of invoices sent in EUR to have 1 easy number

##### Method 1 - step by step iterative
# add the exchange rates to ismTotals
ismTotals <- left_join(ismTotals, fxRates, join_by(currencyLongForm == currency))

# calculate the eur equivalent
ismTotals <- mutate(ismTotals, euroEquivalent = netAmt*toEuro)

# return the total in EUR per year
summarize(ismTotals, .by = year, euroTotal = sum(euroEquivalent))


###### Method 2 - one operation
ismTotals %>% 
  left_join(fxRates, join_by(currencyLongForm == currency)) %>% 
  mutate(euroEquivalent = netAmt*toEuro) %>% 
  group_by(year) %>% 
  summarize(euroTotal = sum(euroEquivalent))
