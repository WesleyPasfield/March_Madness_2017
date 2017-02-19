#!/usr/local/bin/Rscript

#### KENPOM DATA PULL #####

## Insert years of data you'd like to pull & location to drop.
## This will pull all years between year start & year end
## Min Year is 2002, max is 2017
## ex - kenpom_scrape(2002, 2017, '~/Documents/Kaggle/MarchMadness/2017')
print('hello world')
kenpom_scrape <- function( year_start, year_end, location ) {
  
  if( year_start < 2002 ) stop('First Available year is 2002')
  if( year_end > 2017) stop("We can't see into the future, last available year is 2017")
    
  ## Load required packages
  
  library(rvest)
  library(tidyverse)
  
  ## Create sequence of all years required & instatiate empty list & variable for counting

  years <- seq(year_start, year_end, 1)
  urlList <- list()
  a <- 0

  ## Get URL using base KenPom site parameter & year for all requested years

    for (y in years) {
    a <- a + 1
    siteURL <- read_html(paste0('http://kenpom.com/index.php?y=',year_start[a]))
    urlList[[a]] <- siteURL
  }

  ## Create empty list for storing each year
  
  yearList <- list()
  
  ## Loop through each year & pull all relevant data for each team in each year
  
  for (c in 1:length(years))  { 
    
    ## Pull relevant information from KenPom.com. Used selectorgadget to obtain information
    
    kenpom <- urlList[[c]] %>%
      html_nodes('thead:nth-child(1) tr:nth-child(2) th:nth-child(3) , thead:nth-child(1) a, .td-left, #ratings-table #ratings-table , td:nth-child(5), td:nth-child(4), td:nth-child(1), td a') %>%
      html_text(trim =T)
    
    ## Strip + signs out of numeric sections, change column names to match data
    
    kenpom <- gsub('\\+', '', kenpom)
    kenpom[4] <- 'W-L'
    kenpom <- kenpom[-5]
    kenpom[10] <- 'AdjEM_SOS'
    kenpom[13] <- 'AdjEM_NCSOS'
    
    ## Convert to data frame & identify column names, then convert all numeric variable to numeric & add year variable
    
    kenpom2 <- as.data.frame(matrix(kenpom, ncol = 13, byrow = T), header = T, stringsAsFactors = F)
    colnames(kenpom2) <- kenpom2[1,]
    kenpom2 <- kenpom2[-1,] %>%
      mutate(AdjEM = as.numeric(AdjEM),
             AdjO = as.numeric(AdjO),
             AdjD = as.numeric(AdjD),
             AdjT = as.numeric(AdjT),
             Luck = as.numeric(Luck),
             AdjEM_SOS = as.numeric(AdjEM_SOS),
             OppO = as.numeric(OppO),
             OppD = as.numeric(OppD),
             AdjEM_NCSOS = as.numeric(AdjEM_NCSOS),
             Year = years[c])
    
    ## Save each year to List
    
    yearList[[c]] <- kenpom2
  }
  
  ## Stack all years into one dataframe
  
  kenpomAll <- do.call(rbind.data.frame, yearList) 
  
  ## Save to CSV & return the DF

  setwd(location)
  write.csv(kenpomAll, paste0('KenPom_',year_start,'_',year_end,'.csv'), row.names = F)
  return(kenpomAll)
}

## Run function to save file at desired location

kenpom_All <- kenpom_scrape(2002, 2017, '~/Documents/Kaggle/MarchMadness/2017')
head(kenpom_All)
