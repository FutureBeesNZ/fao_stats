library(ggplot2)
library(dplyr)
library(janitor)
library(countrycode) 

## Honey Data From FAO Stats
honey_value <- read_csv('data/FAOSTAT_data_9-16-2021_honey_value.csv') %>% 
  janitor::clean_names()

honey_prod <- read_csv('data/FAOSTAT_data_9-16-2021_honey_prod.csv') %>% 
  janitor::clean_names() %>% 
  filter(year > 2005)

# Look at the value of Honey prod
honey_value_country <- function(countries) {
  names <- paste(unlist(countries), collapse = ", ")
  honey %>% 
    filter(area %in% countries, 
           element_code == 152) %>% # Honey production in 2014-2016 dollars
    ggplot(aes(x=year, y=value, color=area)) +
    geom_line() + 
    ylab("Gross Production Value in $ thousands (adjusted as 2014-2016 dollars)") + ggtitle(glue::glue("Gross Production Value of Honey in {names}"))
}

honey_value_country(c("New Zealand", "Australia", "Canada", "Mexico"))

# Make a not-in operator used to exclude countries
`%nin%` = Negate(`%in%`)

production_efficiency <- function(year) { 
  honey_prod %>% filter(element == "Prod Popultn" | element == "Yield", year == {{year}} ) %>%
    filter(area %nin% c("Ukraine", "Latvia", "Fiji", "Belarus", "Rwanda")) %>% 
    select(year, area, element, value) %>% 
    pivot_wider(names_from = element, values_from = value) %>% 
    janitor::clean_names() %>% 
    mutate(country = countrycode(area, origin = 'country.name', destination = 'iso2c')) %>% 
    ggplot(aes(x=log10(prod_popultn), y=log10(yield), label=country)) + 
    geom_text() + ggtitle(glue::glue("Log10 Production vs. Log10 Honey Yield for {year}")) + 
    xlab("Log10 Producing Population (I think this is hives)") + 
    ylab("Log10 Honey Yield in Tonnes") + geom_smooth()
}

production_efficiency(2019)