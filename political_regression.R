# initialization
library("tidyverse")
library("ggthemes")
library("RPMG")

# set working directory
setwd("C:/Users/tetrg/R/COVID vaccine hesitancy data")

# import data
vhdata_dirty <- read.csv("Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates_20240624.csv") # data from the Centers for Disease Control & Prevention, Apr. 2021, provided by HHS ASPE, found at https://data.cdc.gov/d/q9mh-h2tw. Public Domain work of the U.S. Government
edata_dirty <- read.csv("countypres_2000-2020.csv") # data from the MIT Election Data and Science Lab, 2018, "County Presidential Election Returns 2000-2020", https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V12, UNF:6:KNR0/XNVzJC+RnAqIx5Z1Q== [fileUNF]

# vhdata cleanup
vhdata <- vhdata_dirty[,2:4]
# county cleanup because Virginia, Louisiana and Alaska hate us. uses regex
vhdata$County.Name <- gsub(paste("County"), "", vhdata$County.Name) # cut out everything that says "County"
vhdata$County.Name <- gsub(paste("Parish"), "", vhdata$County.Name) # cut out everything that says "Parish" - Louisiana
vhdata$County.Name <- gsub(paste("Municipality"), "", vhdata$County.Name) # cut out everything that says "Municipality" - Alaska
vhdata$County.Name <- gsub(paste("Census Area"), "", vhdata$County.Name) # cut out everything that says "Census Area" - Alaska
vhdata$County.Name <- gsub(paste("Borough"), "", vhdata$County.Name) # cut out everything that says "Borough - Alaska"
vhdata$County.Name <- gsub(paste("City and Borough"), "", vhdata$County.Name) # cut out everything that says "City and Borough" - Alaska
# TODO: deal with Alaska's weird election districts
vhdata$County.Name <- gsub(paste(" ,.*"), "", vhdata$County.Name) # cut out ", [state name]"
vhdata$County.Name <- str_to_upper(vhdata$County.Name)

# edata cleanup
edata <- edata_dirty %>% filter(year == "2020") # filters for just the 2020 presidential election
edata <- edata %>% select(year, state, county_name, party, candidatevotes, totalvotes) # filters for the columns stated - others are useless to me
candidatepercentage <- c() # new column to collapse candidatevotes and totalvotes
for (i in 1:nrow(edata)){
    cv <- as.numeric(edata[i, 5]) # 5th column is candidatevotes
    tv <- as.numeric(edata[i, 6]) # 6th column is totalvotes
    ratio <- 100 * (cv / tv) # calculates the percentage of the vote the party received
    candidatepercentage <- c(candidatepercentage, ratio) # adds that calculated percentage to the new column
}
edata[,5:6] <- NULL # removes candidatevotes and totalvotes column
edata[,1] <- NULL # removes years column
edata <- cbind(edata, candidatepercentage) # adds candidatepercentage to dataset
edata <- edata %>% filter(str_equal(party, "DEMOCRAT") | str_equal(party, "REPUBLICAN")) # removes 3rd party candidates
edata <- setNames(aggregate(edata$candidatepercentage, list(edata$party, edata$county_name, edata$state), sum), c("Party", "County", "State", "pvote")) # combines pesky states where they did not report votes by "TOTAL", instead providing multiple different categories

# instead of horrendous political analysis, we're just using raw Republican polling percentages (bad political analysis).
edata <- edata %>% filter(str_equal(Party, "REPUBLICAN")) # remove Democratic percentages
edata[,1] <- NULL

# combine into one dataset
data <- merge(edata, vhdata, by.x = c("County", "State"), by.y = c("County.Name", "State"), all.y = TRUE)

# graph
ggplot(data, aes(x = pvote, y = Estimated.hesitant * 100, color = pvote)) +
    geom_point() +
        scale_color_gradient2(low = "blue", mid="purple", high = "red", midpoint = 50) + # TODO: fix colors; needs 50% as white
    geom_smooth(method="lm") + # linear regression prediction
    scale_y_log10() + # log scale yay
    labs( # graph labels
        fill = "Republican-ness", # bad title lol
        title = paste("Vaccine Hesitancy vs. Voting Affiliation in the United States of America", sep = ""),
        subtitle = "From the CDC dataset \"Vaccine Hesitancy for COVID-19: County and local estimates\", and the MEDSL dataset \"County Presidential Election Returns 2000-2020\". Each dot represents a county.",
        y = "Estimated % of population vaccine hesitant, April 2021", # % because the formula as.numeric(Estimated.strongly.hesitant) * 100 first converts strongly hesitant to numeric, then multiplies by 100%
        x = "Republican percentage of vote in 2020 presidential election" # bad title lol
    )
    # TODO: next steps: multiple linear regression. consider other variables, such as age, gender, etc.?
