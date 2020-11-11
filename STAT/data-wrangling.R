library(tidyverse)
library(skimr) #install.packages("skimr")
coronavirus <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv', col_types = cols(province = col_character()))

coronavirus

table(coronavirus$type)

View(coronavirus)

skim(coronavirus)

ggplot(coronavirus) +
  geom_line(mapping = aes(x = date, y = cases, linetype = type))

filter(coronavirus, cases > 0) 

#figure out the conuntry name
table(coronavirus$country)

filter(coronavirus, country == "US" | country == "Canada" )

filter(coronavirus, country %in% c("US","Canada") )

US_death <- filter(coronavirus, country == "US", type == "death")  
sum(US_death$cases)

European <-  filter(coronavirus, country %in% c("Norway","France","Germany"), type == "death", date == "2020-10-04")

select(coronavirus, date, country, type, cases)

select(coronavirus, -lat, -long) # you can use - to deselect columns

select(coronavirus, country:long) # list country,lat, long == contry:long

select(coronavirus, contains("o"))
select(coronavirus, ends_with('e'))


select(coronavirus, casetype = type)
select(coronavirus, casetype = type, everything())
rename(coronavirus, casetype = type)

coronavirus_us  <- filter(coronavirus, country == "US")
coronavirus_us2 <- select(coronavirus_us, -lat, -long, -province) 

# Meet the new pipe %>% operator

coronavirus %>% head(6)

coronavirus_us  <- coronavirus %>% filter(country == "US")
coronavirus_us2 <- coronavirus_us %>% select(-lat, -long, -province) 

coronavirus %>% 
  filter(country == "US") %>% 
  select(-lat, -long, - province)
# same as 
#There are many ways we could subset columns, here's one way:
coronavirus[coronavirus$country == "US", colnames(coronavirus) %in% c("lat", "long", "province")==FALSE] ## repeat `coronavirus`, [i, j] indexing is distracting.
# or 
coronavirus[coronavirus$country == "US", c(2,3, 6:7)] 
coronavirus %>% 
  filter(country %in% c("US","Canada","Mexico"), type == "death") %>% 
  select(country,date,cases)


## lesson8##
library(tidyverse)
library(skimr)
coronavirus <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv', col_types = cols(province = col_character()))
skim(coronavirus)
view(coronavirus)

#summarize the data into country, type and sum(cases)
coronavirus_ttd <- coronavirus %>% 
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarize(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)

coronavirus_ttd

coronavirus_ttd %>% 
  mutate(deathrate = round((death/confirmed),2)) %>% #get round 2 decimal number
  select(deathrate,everything())#arrange the deathrate to first

coronavirus %>%
  filter(type == "confirmed") %>% 
  group_by(country) %>% 
  summarize(sum = sum(cases), n = n())#can see the how many observations the cases be summed up

coronavirus %>% 
  group_by(date, type) %>% 
  summarize(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)
