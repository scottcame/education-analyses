library(readr)
library(openxlsx)
library(tidyverse)
library(ggthemes)
library(ggrepel)

reformatOspiDistrictAsCensus <- function(df) {
  mutate(df, District=case_when(
    District=='CASHMERE SCHOOL DISTRICT' ~ 'Cashmere School District',
    District=='Columbia (Stevens) School District' ~ 'Columbia School District',
    District=='Columbia (Walla Walla) School District' ~ 'Columbia School District (Walla Walla)',
    District=='Fife School District' ~ 'Fife Public Schools',
    District=='Kiona-Benton City School District' ~ 'Kiona-Benton School District',
    District=='Longview School District' ~ 'Longview Public Schools',
    District=='Mary M Knight School District' ~ 'Mary M. Knight School District',
    District=='Seattle Public Schools' ~ 'Seattle School District',
    District=='Spokane School District' ~ 'Spokane Public Schools',
    District=='Star School District No. 054' ~ 'Star School District',
    District=='Steilacoom Hist. School District' ~ 'Steilacoom Historical School District',
    District=='Tacoma School District' ~ 'Tacoma Public Schools',
    District=='Vancouver School District' ~ 'Vancouver Public Schools',
    District=='Walla Walla Public Schools' ~ 'Walla Walla School District',
    District=='West Valley School District (Spokane)' ~ 'West Valley School District (Spokane County)',
    District=='West Valley School District (Yakima)' ~ 'West Valley School District (Yakima County)',
    District=='Yelm School District' ~ 'Yelm Community Schools',
    TRUE ~ District
  ))
}

# https://factfinder.census.gov/bkmk/table/1.0/en/ACS/15_5YR/B25077/0400000US53.97000
houseValue <- read_csv('data/ACS_15_5YR_B25077_with_ann.csv') %>%
  select(CensusGeoID=GEO.id2, DistrictName=`GEO.display-label`, MedianHouseValue=HD01_VD01) %>%
  mutate(DistrictName=gsub(x=DistrictName, pattern='(.+), Washington', replacement='\\1'))

# https://factfinder.census.gov/bkmk/table/1.0/en/ACS/15_5YR/S1902/0400000US53.97000
income <- read_csv('data/ACS_15_5YR_S1902_with_ann.csv') %>%
  select(CensusGeoID=GEO.id2, DistrictName=`GEO.display-label`, MeanHouseholdIncome=HC02_EST_VC02) %>%
  mutate(DistrictName=gsub(x=DistrictName, pattern='(.+), Washington', replacement='\\1'))

# from http://reportcard.ospi.k12.wa.us/DataDownload.aspx
ospiDemographics <- read.xlsx('data/1_1_Demographic Information by District.xlsx') %>%
  reformatOspiDistrictAsCensus() %>%
  mutate(PercentFreeorReducedPricedMeals=as.numeric(PercentFreeorReducedPricedMeals))

ospiSGP <- read.xlsx('data/2015_16.xlsx', sheet='District') %>%
  as_tibble() %>%
  rename(District=DistrictName) %>%
  reformatOspiDistrictAsCensus() %>%
  filter(Subject=='English Language Arts' & Group=='No Group' & Grade=='Grade 4')

writeLines(setdiff(ospiDemographics$District, houseValue$DistrictName))
writeLines(setdiff(ospiDemographics$District, income$DistrictName))
writeLines(setdiff(ospiSGP$District, income$DistrictName))
writeLines(setdiff(ospiDemographics$District, ospiSGP$District))
writeLines(setdiff(houseValue$DistrictName, ospiDemographics$District))

df <- inner_join(houseValue, ospiDemographics, by=c('DistrictName'='District')) %>%
  inner_join(income, by='DistrictName')

ggplot(data=df, mapping=aes(x=MedianHouseValue, y=PercentFreeorReducedPricedMeals)) +
  geom_point(aes(size=TotalEnrollment)) +
  geom_text_repel(data=df %>%
                    filter(MedianHouseValue > 450000 | PercentFreeorReducedPricedMeals > 90 |
                             (MedianHouseValue > 300000 & PercentFreeorReducedPricedMeals > 60)),
                  mapping=aes(label=DistrictName), size=2.5,
                  point.padding=unit(1, 'lines')) +
  theme_economist() +
  labs(y='% Free/Reduced Price Meals', size='Total Enrollment', x='Median Home Value')

ggplot(data=df %>% filter(TotalEnrollment > 100), mapping=aes(x=MeanHouseholdIncome, y=PercentFreeorReducedPricedMeals)) +
  geom_point(aes(size=TotalEnrollment)) +
  geom_text_repel(data=df %>%
                    filter(MeanHouseholdIncome > 130000 | PercentFreeorReducedPricedMeals > 90 |
                          (MeanHouseholdIncome < 60000 & PercentFreeorReducedPricedMeals > 25 & PercentFreeorReducedPricedMeals < 40) |
                          (MeanHouseholdIncome > 90000 & PercentFreeorReducedPricedMeals > 25) |
                            grepl(x=DistrictName, pattern='Highline.+|Belling.+')),
                  mapping=aes(label=DistrictName), size=2.5,
                  point.padding=unit(1, 'lines')) +
  theme_economist() + theme(plot.caption=element_text(size=8, face='italic'), legend.position='right', legend.text=element_text(size=9),
                            legend.title=element_text(size=10)) +
  labs(y='% Free/Reduced Price Meals', size='Student Count', x='Mean Annual Household Income',
       title='Relationship of FRPM and Household Income',
       caption='Note: Mean Annual Household Income is from American Community Survey, 5-year estimates, 2015')

df <- inner_join(income, ospiSGP, by=c('DistrictName'='District')) %>%
  inner_join(ospiDemographics, by=c('DistrictName'='District'))

ggplot(data=df, mapping=aes(x=MeanHouseholdIncome, y=PercentMetStandard)) +
  geom_point(aes(size=StudentCount)) +
  geom_text_repel(data=df %>%
                    filter(PercentMetStandard > .8 | PercentMetStandard < .20 | MeanHouseholdIncome > 115000),
                  mapping=aes(label=DistrictName), size=2.5,
                  point.padding=unit(1, 'lines')) +
  theme_economist() + theme(plot.caption=element_text(size=8, face='italic'), legend.position='right', legend.text=element_text(size=9),
                            legend.title=element_text(size=10)) +
  labs(y='Proficiency (% meeting standard)', size='Student Count', x='Mean Annual Household Income',
       title='Student Proficiency and Household Income',
       subtitle='Grade 4, Engligh Language Arts, 2016',
       caption='Note: Mean Annual Household Income is from American Community Survey, 5-year estimates, 2015')

ggplot(data=df, mapping=aes(x=MeanHouseholdIncome, y=MedianSGP)) +
  geom_point(aes(size=StudentCount)) +
  geom_text_repel(data=df %>%
                    filter(MedianSGP > 72 | MedianSGP < 20 | MeanHouseholdIncome > 130000),
                  mapping=aes(label=DistrictName), size=2.5,
                  point.padding=unit(1, 'lines')) +
  theme_economist() + theme(plot.caption=element_text(size=8, face='italic'), legend.position='right', legend.text=element_text(size=9),
                            legend.title=element_text(size=10)) +
  labs(y='Median SGP', size='Student Count', x='Mean Annual Household Income',
       title='Median Student Growth Percentile and Household Income',
       subtitle='Grade 4, Engligh Language Arts, 2016',
       caption='Note: Mean Annual Household Income is from American Community Survey, 5-year estimates, 2015')

ggplot(data=df, mapping=aes(x=PercentFreeorReducedPricedMeals, y=PercentMetStandard)) +
  geom_point(aes(size=StudentCount)) +
  #geom_text_repel(data=df %>%
  #                  filter(PercentMetStandard > .8 | PercentMetStandard < .20 | MeanHouseholdIncome > 115000),
  #                mapping=aes(label=DistrictName), size=2.5,
  #                point.padding=unit(1, 'lines')) +
  theme_economist() + theme(plot.caption=element_text(size=8, face='italic'), legend.position='right', legend.text=element_text(size=9),
                            legend.title=element_text(size=10)) +
  labs(y='Proficiency (% meeting standard)', size='Student Count', x='% Free/Reduced Price Meals',
       title='Student Proficiency and Poverty',
       subtitle='Grade 4, Engligh Language Arts, 2016')

