#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tablerDash)
library(shinyEffects)
library(echarts4r)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(bs4Dash)
library(shinyBS)
library(tableHTML)
library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(tidyRSS)
#library(rtweet)
library(lubridate)
library(anytime)
library(plotly)
library(quantmod)
library(aRpsDCA)
library(scales)
library(purrr)
library(httr)
library(jsonlite)
library(sp)
library(maptools)
library(maps)
library(tools)
library(geosphere)
library(tigris)
library(openintro)
library(RColorBrewer)
library(akima)
library(rgdal)
library(kernlab)
library(caret)
library(leaflet)
library(leaflet.extras)
library(shinyalert)
library(DT)
#library(excelR)

source("body.R")


options(stringsAsFactors = FALSE)
options(scipen = 999)

# opList1 <- c('APA',  'AR', 'AREX', 'BCEI', 'BRY', 'CDEV', 'CHAP', 'CHK', 'CLR',
#              'CNX', 'COG', 'COP', 'CPE', 'CRC', 'CRK', 'CRZO', 'CXO', 'DNR', 'DVN', 'ECA','EOG',
#              'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP', 'HES', 'HPR', 'JAG', 'LLEX', 'LPI', 'MGY', 'MR',
#              'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP',
#              'REI', 'RRC', 'SPSIOP', 'SBOW', 'SD', 'SM', 'SWN', 'UPLC', 'WLL', 'WPX', 'WTI', 'XEC', 'XOG', 'XOM')  
is2List <- c('AVERAGE SHARES OUTSTANDING', 'Average Shares Outstanding', 'Average shares outstanding', 'average shares outstanding',
             'COMMON SHARES OUTSTANDING', 'Common Shares Outstanding', 'Common shares outstanding', 'common shares outstanding',
             'AVERAGE NUMBER OF', 'Average Number of', 'Average number of', 'average number of', 
             'AVERAGE COMMON SHARES', 'Average Common Shares', 'Average common shares', 'average common shares',
             'A Common Stock', 'of Common Stock', 'TO COMMON STOCK', 'To Common Stock', 'to Common Stock', 'to common stock',
             'WEIGHTED AVERAGE SHARES', 'Weighted Average Shares', 'Weighted average shares', 'weighted average shares',
             'per share att', 'Earnings per Share', 'EARNINGS PER SHARE', 'Earnings per share', 'attributable to common stockh')
is1List <- c('COSTS AND EXPENSES', 'Oil and gas sales', 'Costs and Expenses', 'Costs and expenses',
             'COSTS AND OTHER', 'Costs and Other', 'Costs and other', 
             'OPERATING EXPENSES', 'Operating expenses', 'Operating Expenses',
             'OPERATING COSTS AND EXPENSES', 'Operating Costs and Expenses', 'Operating costs and expenses',
             'TOTAL REVENUES', 'Total Revenues', 'Total revenues', 'REVENUES', 'Revenues')
bs2List <- c('TOTAL LIABILITIES AND', 'Total Liabilities and', 'Total liabilities and', 'Total liabilities', 'TOTAL EQUITY', 'Total stockholders', 'LIABILITIES AND EQUITY')
bs1List <- c('Total Other Assets', 'TOTAL ASSETS',  'CURRENT ASSETS', 'Current Assets', 'Current assets',
             'ASSETS', 'Total Assets','Total assets', 'Assets')
cf1List <- c('FROM OPERATING ACTIVITIES', 'From Operating Activities', 'from operating activities',
             'OPERATING ACTIVITIES','Operating activities', 'Operating Activities', 'operating activities')

cf2List <- c('IN CASH AND CASH', 'In Cash and Cash', 'in Cash and Cash', 'in cash and cash',
             'NET CHANGE IN CASH', 'Net Change in Cash', 'Net change in cash', 'CASH EQUIVALENTS AND RESTRICTED',
             'Cash Equivalents and Restricted', 'Cash equivalents and restricted', 'cash equivalents and restricted',
             'CASH AND CASH', 'Cash and Cash', 'Cash and cash', 'cash and cash', 'SUPPLEMENTAL CASH', 'Supplemental Cash',
             'Supplemental cash', 'PROVIDED BY FINANCING ACTIV', 'Provided by Financing Activ', 'Provided by financing activ', 'provided by financing activ')

prodList <- c('Natural gas liquids production', 'Cash settled deriv', 'Sales volume detail', 'Summary Operating', 'Volume Variances', 'Total STACK Areas', 'Production data', 
              'Total MMBoe','Net Production Data', 'Average Net Production', 'Barrels of Oil Equivalent', 'Crude Oil & Condensate','Total Production ',
              'Total company', 'Net production', 'Production Data',
              'Volumes reconciliation', 'mboed', 'mboepd',
              'Production volumes', 'All Fields', 'Production Volumes','Total production volumes','Production information', 'BOEs', 'Average daily production', 'Natural gas production volumes', 
              'per day', 'Sales volumes','Net sales volume', 'Net Sales Volume','Production Sales Volume', 'Total production',
              'Oil equivalent', 'NGLs and gas sales including', 'LIQUIDS', 'Eagle Ford', 'NGLs', 'Crude oil equivalent', 'per Boe', 'Bbls', 'Average Daily Production',
              'Average net sales', 'Sales Volume')

reserveList <- c('Proved Developed', 'PROVED DEVELOPED', 'Proved developed', 'Standardized measure of discount', 'Discounted future net')

opList1 <- c('APA',  'AR', 'AREX', 'BCEI', 'BP', 'CDEV', 'CHAP', 'CHK', 'CLR',
             'CNX', 'COG', 'COP', 'CPE',  'CRK',  'CXO',  'DVN', 'ECA','EOG',
             'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP', 'GUI', 'HES', 'HPR',  'LLEX', 'LPI', 'MGY', 'MR',
             'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP', 'RDS-A',
              'RRC', 'SBOW', 'SM', 'SWN',  'WLL', 'WPX', 'XEC', 'XOG', 'XOM')  

opLink <- data.frame(ticker = opList1, operator = c('Apache', 'Antero Resources',
                                                   'Approach Resources',  'Bonanza Creek Energy', 'BP','Centennial Resource Development',
                                                   'Chaparral Energy', 'Chesapeake Energy', 'Continental Resources',
                                                   'CNX Resources', 'Cabot Oil & Gas', 'ConocoPhillips', 'Callon Petroleum',
                                                   'Comstock Resources',  'Concho Resources',
                                                   'Devon Energy', 'Encana Corporation', 'EOG Resources', 'EQT Corporation', 'Earthstone Energy',
                                                   'Diamondback Energy', 'Gulfport Energy', 'Goodrich Petroleum', 'Guidon Energy Management Services',
                                                   'Hess Corporation', 'HighPoint Resources', 
                                                   'Lilis Energy', 'Laredo Petroleum',
                                                   'Magnolia Oil & Gas Operating', 'Montage Resources', 'Marathon Oil',
                                                   'Matador Resources', 'Murphy Oil', 'Noble Energy', 'Oasis Petroleum',
                                                   'Occidental Petroleum', 'PDC Energy', 'Parsley Energy', 'Penn Virginia Corporation',
                                                   'Pioneer Natural Resources', 'QEP Resources', 'Shell', 'Range Resources',
                                                   'SilverBow Resources', 'SM Energy', 'Southwestern Energy', 'Whiting Petroleum Corporation',
                                                   'WPX Energy', 'Cimarex Energy', 'Extraction Oil & Gas', 'ExxonMobil'))


derivList <- c('SWAP', 'Swap', 'swap', 'COLLAR','Collar', 'collar', 'Natural gas MMbtu',
               'Weighted average index', 'Weightedaverage in', 
               'Mont Belvieu', 'NYMEX to TCO',  'Ceiling price', 'Settlement Index', 'Volumes Hedged', 
               'Natural gas and power', 'Daily Volume', 'Location', 'NYMEX H', 'NYMEX W', 'Gulfport Receives', 'Threshold', 'Weighted Average Price',
               'Puts', 'PUTS')

debtList <- c('Notes and deb', 'Senior Notes due', 'Commercial paper', 'Credit Facility', 'Credit facility', 'Senior secured credit', 'Senior Unsecured', 
              'Revolving credit', 'Revolving Credit A', 'Percent of total debt', 'Debt issuance', 'Contractual Obligation', 'Senior Second Lien', 'Notes due 2', 'notes due 2')

firmList <- c('Firm Tr', 'Firm tr', 'FIRM TR', 'firm trans')

ebitdaList <- c('EBITDA', 'ebitda')

bsScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            if(TRUE %in% grepl('Subsidi', this_table) & !check$comp.ticker %in% c('CNX', 'SBOW', 'PXD', 'GPOR', 'FANG')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Third Party', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Operating Expense', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Earnings Before', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Basis Only', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Total Revenue', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Total Reportable', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Variable Lease Cost', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Obtained in Exchange', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Customer Relationships', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Utica Shale', this_table) & check$comp.ticker %in% c('CNX')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Fresh-Start', this_table) & check$comp.ticker %in% c('BCEI')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Consideration Given', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Total liabilities of discontinued operations', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Guarantor', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Fair Value', this_table)& !check$comp.ticker %in% c('SBOW')){
                this_table <- NULL
            }
            if(TRUE %in% grepl('swaps', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Operating A', this_table)){
                this_table <- NULL
            }
            
            
            # if(TRUE %in% grepl('Income', this_table)&  !check$comp.ticker %in% c('GPOR', 'FANG')){
            #   this_table <- NULL
            # }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

isScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            if(TRUE %in% grepl('Guarantor', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('LIABILI', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Changes in future development', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Column C', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Predecess', this_table) & substr(check$date,2,2) == '4' & check$comp.ticker == 'CRK'){
                this_table <- NULL
            }
            
            if((TRUE %in% grepl('Subsid', this_table)) & (check$comp.ticker == 'COP')){
                this_table <- NULL
            }
            
            if((TRUE %in% grepl('Nine Months', this_table)) & (check$comp.ticker == 'AR')){
                this_table <- NULL
            }
            
            if((TRUE %in% grepl('Under Successful', this_table)) & (check$comp.ticker == 'CHK')){
                this_table <- NULL
            }
            
            if((TRUE %in% grepl('Retained Earnings', this_table)) & (check$comp.ticker == 'BRY')){
                this_table <- NULL
            }
            
            if((TRUE %in% grepl('Certificate', this_table)) & (check$comp.ticker == 'BRY')){
                this_table <- NULL
            }
            
            if((TRUE %in% grepl('per Bbl', this_table))){
                this_table <- NULL
            }
            if((TRUE %in% grepl('Boe', this_table))){
                this_table <- NULL
            }
            if((TRUE %in% grepl('Per Unit', this_table))){
                this_table <- NULL
            }
            if((TRUE %in% grepl('Current assets', this_table))){
                this_table <- NULL
            }
            if((TRUE %in% grepl('per BOE', this_table))){
                this_table <- NULL
            }
            if((TRUE %in% grepl('EPS calculation', this_table))){
                this_table <- NULL
            }
            if((TRUE %in% grepl('Oil and gas sales', this_table) & check$comp.ticker == 'JAG')){
                this_table <- NULL
            }
            if((TRUE %in% grepl('excluding corporate overhead', this_table) & check$comp.ticker == 'MUR')){
                this_table <- NULL
            }
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

cfScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            if(TRUE %in% grepl('Guarantor', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Activity Type', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('retrospective', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Subsid', this_table) & check$comp.ticker == 'COP'){
                this_table <- NULL
            }
            
            #if((TRUE %in% grepl('Nine Months', this_table)) & (check$comp.ticker == 'AR')){
            #  this_table <- NULL
            #}
            
            if((TRUE %in% grepl('Under Successful', this_table)) & (check$comp.ticker == 'CHK')){
                this_table <- NULL
            }
            
            
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}


prodScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            if(TRUE %in% grepl('Guarantor', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('compared to the same period', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('QRE', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('means one million', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('means one thousand', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('barrel of oil equivalent per day', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Proved Developed Reserves', this_table)){
                this_table <- NULL
            }
            # if(TRUE %in% grepl('Eagle Ford', this_table) & !check$comp.ticker %in% c('CHK', 'DVN')){
            #   this_table <- NULL
            # }
            if(TRUE %in% grepl('Production information', this_table) & check$comp.ticker != 'PXD'){
                this_table <- NULL
            }
            if(TRUE %in% grepl('All Fields', this_table) & check$comp.ticker != 'SBOW'){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Access Pipeline', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('refers to production', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Lucid Energy', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Company record average', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Completion', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('EBITDA', this_table) & check$comp.ticker == 'MTDR'){
                this_table <- NULL
            }
            
            if(check$comp.ticker == 'XEC'){
                if(TRUE %in% grepl('Variance', this_table) & check$comp.ticker != 'COG'){
                    this_table <- NULL
                }
            }
            if(TRUE %in% grepl('Changes due', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('net dollar effect of change', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Guidance', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Swap', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Other Metrics', this_table)){
                this_table <- NULL
            }
            
            
            if(TRUE %in% grepl('noncontrolling', this_table) & check$comp.ticker != 'MUR'){
                this_table <- NULL
            }
            if(TRUE %in% grepl('WTI index', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Total average production of', this_table) & check$comp.ticker == 'CNX'){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Net sales volumes produced ', this_table) & check$comp.ticker == 'CNX'){
                this_table <- NULL
            }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

derivScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            if(TRUE %in% grepl('Cash settle', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('PURCHASES OF EQUITY S', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Paraxylene', this_table)){
                this_table <- NULL
            }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

debtScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            
            if(TRUE %in% grepl('Bylaws', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Wellbore', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('investing activities', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('initial public offering', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Certificat', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Investing Activities', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('Exhibit', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('CURRENT LIABILITIES', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Current liabilities', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Balance Sheet Data', this_table)){
                this_table <- NULL
            }
            
            if(TRUE %in% grepl('Other assets', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('OPERATING EXPENSES', this_table)){
                this_table <- NULL
            }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

firmScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            
            if(TRUE %in% grepl('Bylaws', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('principles generally', this_table)){
                this_table <- NULL
            }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

reserveScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            
            if(TRUE %in% grepl('Bylaws', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('principles generally', this_table)){
                this_table <- NULL
            }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

ebitdaScrape <- function(nodes, string1, check) {
    nodes <- nodes
    check <- check
    #html_nodes('a') %>% 
    #html_attr("href")
    #tbls <- tbls[!duplicated(tbls)]
    strMatch <- string1
    
    signal <- FALSE
    my_tables <- list()
    #my_length <- list()
    j <- 0
    for (i in 1:length(nodes)) {
        signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        # if title signal previously set and this is a table tag
        if (signal & html_name(nodes[i]) == "table") {
            cat("Match..\n")
            print(i)
            # get the table (data frame)
            list1 <- nodes[i]# %>% html_nodes('tr')
            this_table <- list1 %>% paste(collapse='\n')
            this_table <- data.frame(list1 = this_table)
            list1 <- nodes[i] %>% html_nodes('tr')
            this_table$rows <- length(list1)
            
            
            if(TRUE %in% grepl('Bylaws', this_table)){
                this_table <- NULL
            }
            if(TRUE %in% grepl('principles generally', this_table)){
                this_table <- NULL
            }
            
            j = j + 1
            my_tables[[j]] <- this_table
            
            
            
            # and reset the signal so we search for the next one
            signal <- FALSE
        }
        
        # if the signal is clear look for matching title
        if (!signal) {
            signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
        }
    }
    return(my_tables)
}

IRRcalc <- function(cf, months){
    if(sum(cf) > 0){
        IRR1 <- 3
        loop <- 1
        while(sum(cf/((1+IRR1)^(months/12)))
              < 0){
            IRR1 <- IRR1 - 0.01
            loop = loop + 1
        }
        
    }else {
        IRR1 <- 0
    }
    return(IRR1)
}
rsq <- function(x, y) summary(lm(y~x))$r.squared
# wellData <- readRDS("./data/wellData2.rds") 
# subBasinList <- unique(wellData$subBasin)
# 
# leaseSummary <- readRDS("./data/leases2.rds")
# leaseSummary$operator[leaseSummary$operator == 'Bp'] <- 'BP'
# leaseSummary$operator[leaseSummary$operator == 'Swn Production'] <- 'Southwestern Energy'
# propUplift <- readRDS("./data/propUplift.rds")
# perfRisk <- data.frame(perf = c(0, 2500, 5000, 7500, 10000, 12500, 15000), risk = c(0, 0.55, 1, 1.45, 1.85, 2.2, 2.5))
# 
# perfUplift <- lm(perfRisk$risk ~ perfRisk$perf + I(perfRisk$perf**2))
# costData <- readRDS("./data/costData.rds")
# countyData <- readRDS("./data/countyData.rds")
# wellInfoX <- readRDS('./data/wellInfo.rds')
# prodData <- readRDS("./data/prod.rds")

# cards




# app
shiny::shinyApp(
    ui = tablerDashPage(
        
        
        
        title = "EnFinExplorer",
        enable_preloader = TRUE,
        loading_duration = 2,
        navbar = tablerDashNav(
            
                
            id = "mymenu",
            src = "rig.png",
            h3('Energy Financial Explorer'),
            navMenu = tablerNavMenu(
                tablerNavMenuItem(
                    tabName = "home",
                    icon = "home",
                    "Home"
                ),
                tablerNavMenuItem(
                  tabName = "Options",
                  icon = "box",
                  "Derivatives"
                ),
                tablerNavMenuItem(
                    tabName = "comp",
                    icon = "box",
                    "Company Analysis"
                ),

                
                tablerNavMenuItem(
                  tabName = "pdp",
                  icon = 'box',
                  "Proved Developed Forecast"
                ),
                tablerNavMenuItem(
                    tabName = "typeCurves",
                    icon = 'box',
                    "Type Curves"
                ),
                
                # tablerNavMenuItem(
                #   tabName = "wmac",
                #   icon = 'box',
                #   "Woodmac TC Generator"
                # ),
                tablerNavMenuItem(
                    tabName = "devPlan",
                    icon = 'box',
                    "Development Plan"
                ),
                tablerNavMenuItem(
                    tabName = "finCalc",
                    icon = 'box',
                    "RS-TUV Engineering"
                ),
                tablerNavMenuItem(
                    tabName = "About",
                    icon = "info",
                    "About"
                )
            ),
            tablerDropdown(
                
                    icon = 'info',
                    p("Built with:"),
                    a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30"), href='https://shiny.rstudio.com/',target='_blank'),
                    
                    a(img(src = 'tablerDash.svg', height = '30'),href='https://github.com/RinteRface/tablerDash',target='_blank'),
                    
                    a(img(src = 'dplyr.png', height = '30'),href='https://dplyr.tidyverse.org/',target='_blank'),
                    a(img(src = 'rvest.png', height = '30'),href='https://github.com/tidyverse/rvest',target='_blank'),
                    a(img(src = 'tidyverse.png', height = '30'),href='https://www.tidyverse.org/',target='_blank'),
                   
                    a(img(src = 'lubridate.png', height = '30'),href='https://lubridate.tidyverse.org/',target='_blank'),
                    
                    a(img(src = 'rtweet.png', height = '30'), href='https://rtweet.info/',target='_blank'),
                    a(img(src = 'tableHTML.png', height = '30'), href='https://github.com/LyzandeR/tableHTML',target='_blank'),
                    a(img(src = 'echarts.png', height = '30'), href='https://echarts4r.john-coene.com/index.html',target='_blank'),
                    a(img(src = 'shinyEffects.png', height = '30'), href='https://github.com/RinteRface/shinyEffects',target='_blank'),
                    br(),

                    a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30"), href='',target='_blank'),
                    a(img(src = "love.png", height = "30"), href='https://deanattali.com/shinyjs/',target='_blank'),
                    br(),
                    
                    (a("    shinyBS    ", href="https://ebailey78.github.io/shinyBS/index.html",target='_blank')),
                    br(),
                    (a("    shinyWidgets    ", href="https://github.com/dreamRs/shinyWidgets",target='_blank')),
                    br(),
                    (a('    quantmod    ', href = 'https://github.com/joshuaulrich/quantmod',target='_blank')),
                    br(),
                  
                    (a('    DT    ', href = 'https://github.com/rstudio/DT',target='_blank')),
                    br(),
                   
                    (a('    tidyRSS    ', href = 'https://github.com/RobertMyles/tidyRSS',target='_blank')),
                    br(),
                    (a('    aRpsDCA    ', href = 'https://github.com/derrickturk/aRpsDCA',target='_blank'))
                    
                    
                    
                
                
                
            )
        ),
        
        body = body,        
        footer = tablerDashFooter(
            # tablerIcon(name = "maestro", lib = "payment"),
            # tablerIcon(name = "mastercard", lib = "payment"),
            
            copyrights = "@enFinExplorer, 2020"
        )
    ),
    server = function(input, output,session) {
        options(shiny.maxRequestSize=30*1024^2)
        values <- reactiveValues()
        
        output$news <- DT::renderDataTable({
            comp.ticker <- input$operatorSelect
            comp.name <- finreportr::CompanyInfo(comp.ticker)
            comp.name <- comp.name$company[1]
            comp.name <- gsub(' ', '%20', comp.name)
            url1 <- paste0("https://news.google.com/rss/search?q=", comp.name)
            #print(url1)
            tidy1 <- tidyRSS::tidyfeed(url1)
            url2 <- paste0("https://news.google.com/rss/search?q=", comp.ticker)
            
            tidy2 <- tidyRSS::tidyfeed(url2)
            tidy2 <- tidy2 %>% filter(!feed_link %in% tidy1$feed_link)
            tidy1 <- rbind(tidy1, tidy2)
            tidy1$item_link <- paste0("<a target='_blank' href='",tidy1$item_link,"'>",tidy1$item_title,"</a>")
            tidy1 <- tidy1[,c('item_link', 'item_date_published')]
            tidy1$item_date_published <- as.Date(tidy1$item_date_published)
            names(tidy1) <- c('Article', 'Published Date')
            DT::datatable(tidy1, rownames = FALSE, escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
                          options = list(pageLength = 10,
                                         lengthMenu = c(5, 10, 15, 20)))
        })
        
        output$hcplot <- renderEcharts4r({
            ticker <- input$operatorSelect
            stock <- getSymbols(ticker, src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
            df <- data.frame(Date = index(stock), coredata(stock))
            df <- df %>% filter(Date >= input$start_date)
            rm(stock)
            removeSymbols()
            names(df)[1:5] <- c('date', 'opening', 'high', 'low', 'closing')
            #print(names(df))
            df$date <- as.character(df$date)
            df %>%
                e_charts(date) %>%
                e_candle(opening, closing, low, high, name = input$operatorSelect) %>% 
                e_datazoom(type = "slider") %>% 
                e_title("Candlestick chart", "Quantmod data")%>%
                e_theme('auritus')%>%
                e_tooltip(trigger = "axis")
            
        })
        
        output$hcplot1 <- renderEcharts4r({
            tickers <- input$operatorSelect1
            tickers <- append('^GSPC', tickers)
            #print(tickers)
            i<-1
            df1 <- data.frame()
            while(i <= length(tickers)){
                stock <- getSymbols(tickers[i], src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
                
                df <- data.frame(Date = index(stock), coredata(stock))
                df <- df %>% filter(Date >= input$start_date1)
                #print(head(df))
                df$percentChange <- df[,5]/df[1,5]-1
                df$ticker <- tickers[i]
                rm(stock)
                removeSymbols()
                names(df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted', 'percentChange', 'ticker')
                df <- as.data.frame(df)
                df1 <- rbind(df1, df)
                i <- i +1
            }
            df1$ticker[df1$ticker == '^GSPC'] <- 'SP500'
            df1 <- as.data.frame(df1)
            df1$Date <- as.character(df1$Date)
            #print(head(df1))
            df1$percentChange <- df1$percentChange*100
            
            df1 %>% 
                group_by(ticker) %>% 
                e_charts(Date) %>% 
                e_line(percentChange) %>% 
                e_title("", "Percent Change") %>%
                #e_datazoom(type = "slider") %>% 
                e_theme('auritus')%>%  # theme
                e_legend(right = 0)%>%  # move legend to the bottom
                e_tooltip(trigger = "axis")
        })
        derivValues <- reactive({
            data.frame(
                Component = c('volumeHedged', 'swap1', 'collar21', 'collar22', 'collar33', 'oilPrice', 'gasPrice'),
                
                Value = c(input$volumeHedged, input$swap1, input$collar21, input$collar22, input$collar33, input$oilPrice, input$gasPrice),
                stringsAsFactors = FALSE) %>% spread(Component, Value)
            
        })

        
        twoWay <- function(x) {
            if(x <= derivValues()$collar22){
                MTM <- derivValues()$volumeHedged*(derivValues()$collar22-x)
            } else {
                if(x > derivValues()$collar21){
                    MTM <- derivValues()$volumeHedged*(derivValues()$collar21-x)
                } else {
                    MTM <- 0
                }
            }
            
            return(MTM)
        }
        
        threeWay <- function(x) {
            if(x <= derivValues()$collar22){
                MTM1 <- derivValues()$volumeHedged*(derivValues()$collar22-x)
            } else {
                if(x > derivValues()$collar21){
                    MTM1 <- derivValues()$volumeHedged*(derivValues()$collar21-x)
                } else {
                    MTM1 <- 0
                }
            }
            
            if(x >= derivValues()$collar33){
                MTM2 <- 0
            } else {
                MTM2 <- derivValues()$volumeHedged*(x-derivValues()$collar33)
            }
            MTM <- MTM1 + MTM2
            return(MTM)
        }
        
        url1 <- function(x) {
            paste0('https://www.sec.gov', ((xml2::read_html(x) %>%
                                                rvest::html_nodes('table') %>% .[1] %>%
                                                rvest::html_nodes('a') %>% 
                                                rvest::html_attr('href')))[1])
        }
        
        observeEvent(input$loadFilings, {
            updateButton(session, 'loadFilings', label = 'Gathering...', style = 'danger')
            shinyjs::disable('loadFilings')
            filingList <- data.frame(edgarWebR::company_details(input$operatorSelect, type = '10-K', count = 8)) %>% filter(!grepl('A', filings.type))
            filingList1 <- data.frame(edgarWebR::company_details(input$operatorSelect, type = '10-Q', count = 24)) %>% filter(!grepl('A', filings.type))
            
            filingList <- rbind(filingList, filingList1) %>% arrange(desc(filings.filing_date))
            compInfo <- finreportr::CompanyInfo(input$operatorSelect)
            filingList$Company <- compInfo$company
            rm(filingList1)

            filingList$url1 <- lapply(filingList$filings.href,  url1)
            filingList$url1 <- gsub('/ix?doc=', '', filingList$url1, fixed=TRUE)
            values$check <- filingList
            filingList <- filingList[,c('Company', 'filings.filing_date', 'filings.type', 'url1')] %>% arrange(desc(filings.filing_date))
            filingList$quarter <- lubridate::quarter(filingList$filings.filing_date) - 1
            filingList$year <- lubridate::year(filingList$filings.filing_date)
            filingList$quarter[filingList$quarter == 0] <- 4
            filingList$year[filingList$quarter == 4] <- filingList$year[filingList$quarter == 4]-1
            filingList$period <- paste0('Q', filingList$quarter, filingList$year)
            updateSelectizeInput(session, 'Filing', choices = filingList$period)
            names(filingList)[1:4] <- c('Company', 'filingDate', 'type', 'url1')
            filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
            filingList <- data.frame(lapply(filingList, function(x){
                gsub("iXBRL", "", x)
            }))
            filingList <- data.frame(lapply(filingList, function(x){
                gsub("\\s+", "", x)
            }))
            
            filingList$type <- paste0('<a href="', filingList$url1, '" target="_blank">', filingList$type,'</a>')
            values$filingList <- filingList
            filingList <- filingList[,c('Company', 'period', 'filingDate', 'type')]
            names(filingList) <- c('Company', 'Filing Period', 'Filing Date', 'Report')
            filingList <- as.data.frame(filingList)
            values$filingList1 <- filingList
            updateButton(session, 'loadFilings', label = 'SUCCESS', style = 'success')
            updateButton(session, 'loadFilings', label = 'LOAD', style = 'primary')
            shinyjs::enable('loadFilings')
        })
        
        
        observeEvent(input$operatorSelect, {
            values$bs <- NULL
            values$is <- NULL
            values$cf <- NULL
            values$prod <- NULL
            values$deriv <- NULL
            values$debt <- NULL
            values$reserves <- NULL
            values$firm <- NULL
            values$ebitda <- NULL
            updateMultiInput(session, 'operatorSelect1', selected=input$operatorSelect)
            #shinyjs::hide('hideTables')
        })
        
        observeEvent(input$Filing, {
            values$bs <- NULL
            values$is <- NULL
            values$cf <- NULL
            values$prod <- NULL
            values$deriv <- NULL
            values$debt <- NULL
            values$reserves <- NULL
            values$firm <- NULL
            values$ebitda <- NULL
            #shinyjs::hide('hideTables')
        })
        
        
        
        output$filingList <- DT::renderDataTable({
            if(is.null(values$filingList1)){
                NULL
            } else {
                filingList <- values$filingList1
                filingList <- subset(filingList, select = -c(Company))
                DT::datatable(filingList, escape = FALSE, rownames = FALSE, options = list(paging = FALSE, searching = FALSE))
            }
        })
        
        
        output$debtLink <- DT::renderDataTable({
            if(is.null(values$debt)){
                NULL
            } else {
                my_test <- values$filingList
                names(my_test)[2] <- 'date'
                my_test <- my_test %>% filter(date %in% input$Filing)
                check <- my_test
                check$comp.ticker <- input$operatorSelect
                nodes1 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_attr('href'))
                names(nodes1) <- 'url1'
                nodes2 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_text())
                names(nodes2) <- 'label'
                
                nodes1 <- cbind(nodes1, nodes2)
                
                nodes1 <- nodes1 %>% filter(grepl('Indenture', label)|
                                                grepl('Notes due', label)|
                                                grepl('Credit Agreement', label)) %>% filter(!duplicated(url1))
                if(nrow(nodes1) == 0|is.null(nodes1)){
                    NULL
                } else {
                    nodes1$url1 <- paste0("<a target='_blank' href='",nodes1$url1,"'>",'Link',"</a>")
                }
                
                if(nrow(nodes1) == 0|is.null(nodes1)){
                    NULL
                } else {
                    names(nodes1) <- c('', '')
                    DT::datatable(nodes1,  rownames = FALSE, escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
                                  options = list(pageLength = 5,
                                                 lengthMenu = c(5, 10, 15)))
                }
            }
            
        })
        
        
        output$reportLink <- DT::renderDataTable({
            if(is.null(values$reserves)){
                NULL
            } else {
                my_test <- values$check
                my_test$quarter <- lubridate::quarter(my_test$filings.filing_date) - 1
                my_test$year <- lubridate::year(my_test$filings.filing_date)
                my_test$quarter[my_test$quarter == 0] <- 4
                my_test$year[my_test$quarter == 4] <- my_test$year[my_test$quarter == 4]-1
                my_test$period <- paste0('Q', my_test$quarter, my_test$year)
                my_test <- my_test %>% filter(period %in% input$Filing)
                names(my_test)[2] <- 'cik'
                
                names(my_test)[20] <- 'accession_number'
                check <- my_test
                check$comp.ticker <- input$operatorSelect
                nodes1 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_attr('href'))
                names(nodes1) <- 'url1'
                nodes2 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_text())
                names(nodes2) <- 'label'
                
                nodes1 <- cbind(nodes1, nodes2)
                nodes1 <- nodes1 %>% filter(grepl('99', url1)|grepl('99', label)|grepl('Ryder', label)|
                                                grepl('DeGol', label)|
                                                grepl('NSAI', label))
                
                nodes1 <- nodes1 %>% filter(grepl('99', label)|
                                                grepl('Ryder', label)|
                                                grepl('DeGol', label)|
                                                grepl('NSAI', label)|
                                                grepl('Sewell', label)|
                                                grepl('Cawley', label)|
                                                grepl('CGA', label)|
                                                grepl('LaRoche', label)|
                                                grepl('Schlum', label)|
                                                grepl('Reserves', label)) %>%
                    filter(!grepl('Consent', label)) %>% filter(!grepl('http', url1))
                if(nrow(nodes1) == 0|is.null(nodes1)){
                    NULL
                } else {
                    accessionNo <- gsub("-", "", check$accession_number)
                    nodes1 <- nodes1 %>% mutate(url1 = paste('https://www.sec.gov/Archives/edgar/data', as.numeric(check$cik), accessionNo, url1, sep='/'))
                    nodes1$url1 <- paste0("<a target='_blank' href='",nodes1$url1,"'>",'Go to Report',"</a>")
                }
                
                
                if(nrow(nodes1) == 0){
                    NULL
                } else {
                    names(nodes1) <- c('', '')
                    DT::datatable(nodes1,  rownames = FALSE, escape = FALSE, options=list(paging = TRUE, info = FALSE, ordering=FALSE))
                }
            }
            
        })
        observeEvent(input$loadTables, {
            updateButton(session, 'loadTables', label = 'Calculating...', style = 'danger')
            shinyjs::disable('loadTables')
            
            my_test <- values$filingList
           # print(head(my_test))
            names(my_test)[2] <- 'date'
            my_test <- my_test %>% filter(date %in% input$Filing)
            check <- my_test
            check$comp.ticker <- input$operatorSelect
            nodes <- read_html(my_test$url1) %>% html_nodes('table') #%>% #.[7] %>% html_table(fill=TRUE)
            id <- 1
            i <- 1
            my_tables <- NULL
            while(i <= length(bs1List)){
                print(bs1List[i])
                if(length(my_tables) == 0){
                    my_tables <- bsScrape(nodes, bs1List[i], check)
                }
                i <- i+1
            }
            
            #print(my_tables[[1]]$list1)
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            test1 <- data.frame(my_tables[[tableSize]])
            test1 <- test1$list1
            #test1 <- gsub('', NA, test1)
            #test1 <- data.frame(my_tables[[1]])
            bs <- test1
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(bs2List)){
                print(bs2List[i])
                if(length(my_tables) == 0){
                    my_tables <- bsScrape(nodes, bs2List[i], check)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            test1 <- data.frame(my_tables[[tableSize]])
            test1 <- test1$list1
            #test1 <- gsub('', NA, test1)
            #test1 <- data.frame(my_tables[[1]])
            
            if(is.null(test1)){
                NULL
            } else {
                if((test1) == (bs)){
                    NULL
                } else {
                    
                    bs <- paste0(bs, test1)
                }
            }
            
            
            values$bs <- bs
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(is1List)){
                print(is1List[i])
                if(length(my_tables) == 0){
                    my_tables <- isScrape(nodes, is1List[i], check)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            
            if((check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 3)|(check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 2)){
                test1 <- data.frame(my_tables[[1]])
                test1 <- test1$list1
            } else {
                
                test1 <- data.frame(my_tables[[tableSize]])
                test1 <- test1$list1
            }
            
            is <- test1
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(is2List)){
                print(is2List[i])
                if(length(my_tables) == 0){
                    my_tables <- isScrape(nodes, is2List[i], check)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            
            if((check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 3)|(check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 2)){
                test1 <- data.frame(my_tables[[1]])
                test1 <- test1$list1
            } else {
                
                test1 <- data.frame(my_tables[[tableSize]])
                test1 <- test1$list1
            }
            
            is2 <- test1
            
            i <- 1
            my_tables <- NULL
            while(i <= length(cf1List)){
                print(cf1List[i])
                if(length(my_tables) == 0){
                    my_tables <- cfScrape(nodes, cf1List[i], check)
                }
                i <- i+1
            }
            
            #print(my_tables[[1]]$list1)
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            test1 <- data.frame(my_tables[[tableSize]])
            test1 <- test1$list1
            
            cf <- test1
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(cf2List)){
                print(cf2List[i])
                if(length(my_tables) == 0){
                    my_tables <- cfScrape(nodes, cf2List[i], check)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            test1 <- data.frame(my_tables[[tableSize]])
            test1 <- test1$list1
            
            if(is.null(test1)){
                NULL
            } else {
                if((test1) == (cf)){
                    NULL
                } else {
                    
                    cf <- paste0(cf, test1)
                }
            }
            if(is.null(is2)){
                NULL
            }else{
                if((is2) == (is)){
                    NULL
                } else {
                    if((cf) == (is2)){
                        NULL
                    } else{
                        is <- paste0(is, is2)
                    }
                    
                }
            }
            values$is <- is 
            
            
            #my_test <- values$check %>% arrange(desc(filing_date)) %>% filter(date %in% input$Filing)
            #check <- values$check %>% arrange(desc(filing_date)) %>% filter(date %in% input$Filing)
            #print(head(my_test))
            #nodes <- read_html(my_test$url1) %>% html_nodes('table') #%>% #.[7] %>% html_table(fill=TRUE)
            i <- 1
            my_tables <- NULL
            while(i <= length(cf1List)){
                print(cf1List[i])
                if(length(my_tables) == 0){
                    my_tables <- cfScrape(nodes, cf1List[i], check)
                }
                i <- i+1
            }
            
            #print(my_tables[[1]]$list1)
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            test1 <- data.frame(my_tables[[tableSize]])
            test1 <- test1$list1
            
            cf <- test1
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(cf2List)){
                print(cf2List[i])
                if(length(my_tables) == 0){
                    my_tables <- cfScrape(nodes, cf2List[i], check)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            tableSize <- NA
            i <- 1
            while(i <= length(my_tables)){
                counts <- my_tables[[i]]$rows
                tableSize <- rbind(tableSize, counts)
                i <- i+1
            }
            tableSize <- tableSize[!is.na(tableSize)]
            tableSize <- which(tableSize == max(tableSize))[1]
            test1 <- data.frame(my_tables[[tableSize]])
            test1 <- test1$list1
            
            if(is.null(test1)){
                NULL
            } else {
                if((test1) == (cf)){
                    NULL
                } else {
                    
                    cf <- paste0(cf, test1)
                }
            }
            values$cf <- cf
            
            
            
            #my_test <- values$check %>% arrange(desc(filing_date)) %>% filter(date %in% input$Filing)
            #check <- values$check %>% arrange(desc(filing_date)) %>% filter(date %in% input$Filing)
            #print(head(my_test))
            #nodes <- read_html(my_test$url1) %>% html_nodes('table') #%>% #.[7] %>% html_table(fill=TRUE)
            i <- 1
            my_tables <- NULL
            while(i <= length(prodList)){
                print(prodList[i])
                if(length(my_tables) == 0){
                    my_tables <- prodScrape(nodes, prodList[i], check)
                }
                i <- i+1
            }
            
            if(length(my_tables) == 0){
                values$prod <- NULL
            } else {
                #print(my_tables[[1]]$list1)
                my_tables <- Filter(Negate(is.null), my_tables)
                tableSize <- NA
                i <- 1
                while(i <= length(my_tables)){
                    counts <- my_tables[[i]]$rows
                    tableSize <- rbind(tableSize, counts)
                    i <- i+1
                }
                
                un <- unlist(my_tables)
                res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
                rm(un)
                
                my_tables <- res.list
                
                i <- 1
                test1 <- NULL
                
                while(i <= length(my_tables)){
                    if(ncol(my_tables[[i]]) > 0){
                        my_tables[[i]] <- my_tables[[i]]$list1
                        test1 <- rbind(test1, my_tables[[i]])
                    } else {
                        my_tables[[i]] <- NULL
                    }
                    i <- i + 1
                }
                
                prod <- test1
                values$prod <- prod
            }
            
            
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(derivList)){
                print(derivList[i])
                if(length(my_tables) == 0){
                    my_tables <- derivScrape(nodes, derivList[i], check)
                } else {
                    my_tables1 <- derivScrape(nodes, derivList[i], check)
                    my_tables <- rbind(my_tables, my_tables1)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            
            un <- unlist(my_tables)
            res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
            rm(un)
            
            my_tables <- res.list
            
            i <- 1
            test1 <- NULL
            
            while(i <= length(my_tables)){
                if(ncol(my_tables[[i]]) > 0){
                    my_tables[[i]] <- my_tables[[i]]$list1
                    test1 <- rbind(test1, my_tables[[i]])
                } else {
                    my_tables[[i]] <- NULL
                }
                i <- i + 1
            }
            values$deriv <- test1
            
            
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(debtList)){
                print(debtList[i])
                if(length(my_tables) == 0){
                    my_tables <- debtScrape(nodes, debtList[i], check)
                } else {
                    my_tables1 <- debtScrape(nodes, debtList[i], check)
                    my_tables <- rbind(my_tables, my_tables1)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            
            un <- unlist(my_tables)
            res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
            rm(un)

            my_tables <- res.list
            
            i <- 1
            test1 <- NULL
            
            while(i <= length(my_tables)){
                if(ncol(my_tables[[i]]) > 0){
                    my_tables[[i]] <- my_tables[[i]]$list1
                    test1 <- rbind(test1, my_tables[[i]])
                } else {
                    my_tables[[i]] <- NULL
                }
                i <- i + 1
            }
         
            values$debt <- test1
            
            i <- 1
            my_tables <- NULL
            while(i <= length(reserveList)){
                print(reserveList[i])
                if(length(my_tables) == 0){
                    my_tables <- reserveScrape(nodes, reserveList[i], check)
                } else {
                    my_tables1 <- reserveScrape(nodes, reserveList[i], check)
                    my_tables <- rbind(my_tables, my_tables1)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            
            un <- unlist(my_tables)
            res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
            rm(un)
            
            my_tables <- res.list
            
            i <- 1
            test1 <- NULL
            
            while(i <= length(my_tables)){
                if(ncol(my_tables[[i]]) > 0){
                    my_tables[[i]] <- my_tables[[i]]$list1
                    test1 <- rbind(test1, my_tables[[i]])
                } else {
                    my_tables[[i]] <- NULL
                }
                i <- i + 1
            }
            
            
            
            values$reserves <- test1
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(firmList)){
                print(firmList[i])
                if(length(my_tables) == 0){
                    my_tables <- firmScrape(nodes, firmList[i], check)
                } else {
                    my_tables1 <- firmScrape(nodes, firmList[i], check)
                    my_tables <- rbind(my_tables, my_tables1)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            
            un <- unlist(my_tables)
            res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
            rm(un)
            
            
            my_tables <- res.list
        
            
            i <- 1
            test1 <- NULL
            
            while(i <= length(my_tables)){
                if(ncol(my_tables[[i]]) > 0){
                    my_tables[[i]] <- my_tables[[i]]$list1
                    test1 <- rbind(test1, my_tables[[i]])
                } else {
                    my_tables[[i]] <- NULL
                }
                i <- i + 1
            }
            #test1 <- dplyr::bind_rows(my_tables)
            values$firm <- test1
            
            
            
            i <- 1
            my_tables <- NULL
            while(i <= length(ebitdaList)){
                print(ebitdaList[i])
                if(length(my_tables) == 0){
                    my_tables <- ebitdaScrape(nodes, ebitdaList[i], check)
                } else {
                    my_tables1 <- ebitdaScrape(nodes, ebitdaList[i], check)
                    my_tables <- rbind(my_tables, my_tables1)
                }
                i <- i+1
            }
            
            my_tables <- Filter(Negate(is.null), my_tables)
            
            un <- unlist(my_tables)
            res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
            rm(un)
            
            my_tables <- res.list
            
            i <- 1
            test1 <- NULL
            
            while(i <= length(my_tables)){
                if(ncol(my_tables[[i]]) > 0){
                    my_tables[[i]] <- my_tables[[i]]$list1
                    test1 <- rbind(test1, my_tables[[i]])
                } else {
                    my_tables[[i]] <- NULL
                }
                i <- i + 1
            }
            
            values$ebitda <- test1
            
            shinyjs::enable('loadTables')
            shinyjs::show('hideTables')
            #updateButton(session, 'loadTables', label = 'SUCCESS!', style = 'success')
            updateButton(session, 'loadTables', label = 'LOAD', style = 'primary')
        })
        output$bs <- render_tableHTML({
            if(is.null(values$bs)){
                h3('Not Available')
            } else {
                
                bs <- values$bs
                # labels1 <- bs[1,]
                # labels1[1,] <- ''
                # names(bs) <- labels1
                # bs[is.na(bs)] <- ''
                HTML(bs)
                #DT::datatable(bs,  rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        
        output$is <- render_tableHTML({
            if(is.null(values$is)){
                h3('Not Available')
            } else {
                is <- values$is
                # labels1 <- is[1,]
                # labels1[1,] <- ''
                # names(is) <- labels1
                # is[is.na(is)] <- ''
                HTML(is)
                #DT::datatable(is, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$cf <- render_tableHTML({
            if(is.null(values$cf)){
                h3('Not Available')
            } else {
                cf <- values$cf
                # labels1 <- cf[1,]
                # labels1[1,] <- ''
                # names(cf) <- labels1
                # cf[is.na(cf)] <- ''
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$prod <- render_tableHTML({
            if(is.null(values$prod)){
                h3('Not Available')
            } else {
                cf <- values$prod
                #labels1 <- cf[1,]
                #labels1[1,] <- ''
                #names(cf) <- labels1
                #cf[is.na(cf)] <- ''
                #tableHTML(cf)
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$deriv <- render_tableHTML({
            if(is.null(values$deriv)){
                h3('Not Available')
            } else {
                cf <- values$deriv
                #labels1 <- cf[1,]
                #labels1[1,] <- ''
                #names(cf) <- labels1
                #cf[is.na(cf)] <- ''
                #tableHTML(cf)
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$debt <- render_tableHTML({
            if(is.null(values$debt)){
                h3('Not Available')
            } else {
                cf <- values$debt
                #labels1 <- cf[1,]
                #labels1[1,] <- ''
                #names(cf) <- labels1
                #cf[is.na(cf)] <- ''
                #tableHTML(cf)
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$firm <- render_tableHTML({
            if(is.null(values$firm)){
                h3('Not Available')
            } else {
                cf <- values$firm
                #labels1 <- cf[1,]
                #labels1[1,] <- ''
                #names(cf) <- labels1
                #cf[is.na(cf)] <- ''
                #tableHTML(cf)
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$reserves <- render_tableHTML({
            if(is.null(values$reserves)){
                h3('Not Available')
            } else {
                cf <- values$reserves
                #labels1 <- cf[1,]
                #labels1[1,] <- ''
                #names(cf) <- labels1
                #cf[is.na(cf)] <- ''
                #tableHTML(cf)
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })
        output$ebitda <- render_tableHTML({
            if(is.null(values$ebitda)){
                h3('Not Available')
            } else {
                cf <- values$ebitda
                #labels1 <- cf[1,]
                #labels1[1,] <- ''
                #names(cf) <- labels1
                #cf[is.na(cf)] <- ''
                #tableHTML(cf)
                HTML(cf)
                #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
            }
        })

        observe({
            if(input$product == 'Gas'){
                shinyjs::hide('oilPrice')
                shinyjs::show('gasPrice')
                updateNumericInput(session, 'volumeHedged', label = 'Total Volume Hedged, MCF/MMBTU')
                updateNumericInput(session, 'swap1', value = 2.5, min = 0, max = 10)
                updateNumericInput(session, 'collar21', value = 2.5, min = 0, max = 10)
                updateNumericInput(session, 'collar22', value = 2.5, min = 0, max = 10)
                updateNumericInput(session, 'collar33', value = 2.5, min = 0, max = 10)
                #updateNumericInput(session, 'collar34', value = 1.8, min = 0, max = 10)
            } else {
                shinyjs::show('oilPrice')
                shinyjs::hide('gasPrice')
                updateNumericInput(session,'volumeHedged', label = 'Total Oil Hedged, BBL')
                updateNumericInput(session, 'swap1', value = 54, min = 0, max = 150)
                updateNumericInput(session, 'collar21', value = 54, min = 0, max = 150)
                updateNumericInput(session, 'collar22', value = 54, min = 0, max = 150)
                updateNumericInput(session, 'collar33', value = 54, min = 0, max = 150)
                #updateNumericInput(session, 'collar34', value = 1.8, min = 0, max = 150)
            }
            
            if(input$options == 'Swap'){
                shinyjs::hide('collar21')
                shinyjs::hide('collar22')
                shinyjs::hide('collar33')
                #shinyjs::hide('collar34')
                shinyjs::show('swap1')
        
            } else {
            
                if(input$options == 'Two-way Collar'){
                    shinyjs::show('collar21')
                    shinyjs::show('collar22')
                    shinyjs::hide('collar33')
                    #shinyjs::hide('collar34')
                    shinyjs::hide('swap1')
                    
                } else {
                    if(input$options == 'Three-way Collar'){
                        shinyjs::show('collar21')
                        shinyjs::show('collar22')
                        shinyjs::show('collar33')
                        #shinyjs::show('collar34')
                        shinyjs::hide('swap1')
                        
                    }
                }
            }
            
        })
        
        


        output$flowGl <- renderEcharts4r({
            
            
            if(input$options == 'Swap' & input$product == 'Gas'){
                df <- data.frame(Price = seq(0, 10, 1))
                df1 <- data.frame(Price = c(derivValues()$swap1, derivValues()$gasPrice))
                df <- rbind(df, df1)
                
                df$MTM <- derivValues()$volumeHedged*(derivValues()$swap1-df$Price)
            }
            if(input$options == 'Swap' & input$product == 'Oil'){
                df <- data.frame(Price = seq(0, 150, 10))
                df1 <- data.frame(Price = c(derivValues()$swap1, derivValues()$oilPrice))
                df <- rbind(df, df1)
                
                df$MTM <- derivValues()$volumeHedged*(derivValues()$swap1-df$Price)
            }
            
            if(input$options == 'Two-way Collar' & input$product == 'Gas'){
                df <- data.frame(Price = seq(0, 10, 1))
                df1 <- data.frame(Price = c(derivValues()$collar21, derivValues()$collar22, derivValues()$gasPrice))
                df <- rbind(df, df1)
                
                df$MTM <- apply(df, 1, twoWay)
                
                
            }
            if(input$options == 'Two-way Collar' & input$product == 'Oil'){
                df <- data.frame(Price = seq(0, 150, 10))
                df1 <- data.frame(Price = c(derivValues()$collar21, derivValues()$collar22, derivValues()$oilPrice))
                df <- rbind(df, df1)
                
                df$MTM <- apply(df, 1, twoWay)
            }
            
            if(input$options == 'Three-way Collar' & input$product == 'Gas'){
                df <- data.frame(Price = seq(0, 10, 1))
                df1 <- data.frame(Price = c(derivValues()$collar21, derivValues()$collar22, derivValues()$collar33, derivValues()$gasPrice))
                df <- rbind(df, df1)
                
                df$MTM <- apply(df, 1, threeWay)
                
                
            }
            if(input$options == 'Three-way Collar' & input$product == 'Oil'){
                df <- data.frame(Price = seq(0, 150, 10))
                df1 <- data.frame(Price = c(derivValues()$collar21, derivValues()$collar22, derivValues()$collar33, derivValues()$oilPrice))
                df <- rbind(df, df1)
                
                df$MTM <- apply(df, 1, threeWay)
            }
            
            values$mtm <-df$MTM[nrow(df)] 
             
            #print(df)
            if(max(abs(df$MTM)) >= 1000000){
                df$MTM <- df$MTM/1000000
                df %>% 
                    e_charts(x = Price) %>% # initialise and set x
                    e_area(serie = MTM) %>% # add a line 
                    e_y_axis(
                        formatter = e_axis_formatter("currency")
                    ) %>% 
                    e_x_axis(name = 'Index Price', axisLabel = list(interval = 0, rotate = 45),
                             formatter = e_axis_formatter("currency", currency = 'USD'))%>%  # add x axis name
                    e_title("Derivative Mark to Market", "Million US$") %>%  # Add title & subtitle
                    e_theme("infographic") %>%
                    e_legend(right = 0)%>%  # move legend to the bottom
                    e_tooltip(trigger = "axis"
                    )#%>% e_toolbox_feature(feature = "saveAsImage")
            } else {
                if(max(abs(df$MTM)) >= 1000){
                    df$MTM <- df$MTM/1000
                    df %>% 
                        e_charts(x = Price) %>% # initialise and set x
                        e_area(serie = MTM) %>% # add a line 
                        e_y_axis(
                            formatter = e_axis_formatter("currency")
                        ) %>% 
                        e_x_axis(name = 'Index Price', axisLabel = list(interval = 0, rotate = 45),
                                 formatter = e_axis_formatter("currency", currency = 'USD'))%>%  # add x axis name
                        e_title("Derivative Mark to Market", "Thousand US$") %>%  # Add title & subtitle
                        e_theme("infographic") %>%
                        e_legend(right = 0)%>%  # move legend to the bottom
                        e_tooltip(trigger = "axis"
                        )#%>% e_toolbox_feature(feature = "saveAsImage")
                } else {
                    df %>% 
                        e_charts(x = Price) %>% # initialise and set x
                        e_area(serie = MTM) %>% # add a line 
                        e_y_axis(
                            formatter = e_axis_formatter("currency")
                        ) %>% 
                        e_x_axis(name = 'Index Price', axisLabel = list(interval = 0, rotate = 45),
                                 formatter = e_axis_formatter("currency", currency = 'USD'))%>%  # add x axis name
                        e_title("Derivative Mark to Market", "US$") %>%  # Add title & subtitle
                        e_theme("infographic") %>%
                        e_legend(right = 0)%>%  # move legend to the bottom
                        e_tooltip(trigger = "axis"
                        )#%>% e_toolbox_feature(feature = "saveAsImage")
                }
            }
            
        })
        
        
        output$mtm <- renderText({
            paste0('Mark to Market: ', scales::dollar(values$mtm))
        })
        
        output$tcPlot <- renderEcharts4r({
            
            oil <- curtailed.q(arps.decline(
                declineValues()$qiOil*365, as.nominal(declineValues()$DiOil), declineValues()$bOil, as.nominal(declineValues()$DfOil)),
                declineValues()$curtailOil/12.0, seq(0, 50-1/12, by = (1/12)))/12
            gas <- curtailed.q(arps.decline(
                declineValues()$qiGas*365, as.nominal(declineValues()$DiGas), declineValues()$bGas, as.nominal(declineValues()$DfGas)),
                declineValues()$curtailGas/12.0, seq(0, 50-1/12, by = (1/12)))/12
            
            water <- curtailed.q(arps.decline(
                declineValues()$qiWater*365, as.nominal(declineValues()$DiWater), declineValues()$bWater, as.nominal(declineValues()$DfWater)),
                declineValues()$curtailWater/12.0, seq(0, 50-1/12, by = (1/12)))/12
            
            df <- data.frame(Months = seq(1, 50*12, by = 1), Gas = gas, Oil = oil, Water = water)
            rm(oil, gas, water)
            if(input$abandonmentMethod == 'Time'){
                #print(declineValues()$wellLife)
                df <- df %>% filter(Months <= (declineValues()$wellLife)*12) %>%filter(!duplicated(Months))
                df <- as.data.frame(df)
                #print(df %>% filter(Oil == 0))
            }
            
            if(input$abandonmentMethod == 'Oil Rate'){
                df <- df %>% filter(Oil >= declineValues()$qfOil*30.45)
            }
            if(input$abandonmentMethod == 'Gas Rate'){
                df <- df %>% filter(Gas >= declineValues()$qfGas*30.45)
            }
            
           # print(head(df))
            values$oilEUR <- as.integer(sum(df$Oil)/1000)
            values$gasEUR <- as.integer(sum(df$Gas)/1000)
            values$waterEUR <- as.integer(sum(df$Water)/1000)
            values$fcst <- df
            df <- as.data.frame(df)
            df$Oil <- df$Oil/30.45
            df$Gas <- df$Gas/30.45
            df$Water <- df$Water/30.45
            #print(head(df))
            #df <- df %>% gather(Component, Value, -c(Months))
            #df$Value <- df$Value/30.45
            df <- df %>% filter((Months >= 1 & Months < 12)|Months %% 12 == 0)
            if(input$logTC == 'Log'){
            
                df %>%
                    #group_by(Component) %>%
                    e_charts(Months) %>%
                    e_x_axis(name = 'Months', index = 0) %>%
                    e_y_axis(type = 'log') %>%
                    e_datazoom(type = "slider") %>% 
                    e_line(Gas, smooth = TRUE) %>%
                    e_line(Oil, smooth = TRUE) %>%
                    e_line(Water, smooth = TRUE) %>%
                    e_title('Type Curve', 'Daily Rate (bpd/mcfd)') %>%
                    #e_legend(right = 0) %>%
                    e_theme('infographic')
            } else {
                df %>%
                    #group_by(Component) %>%
                    e_charts(Months) %>%
                    e_x_axis(name = 'Months', index = 0) %>%
                    #e_y_axis(type = 'log') %>%
                    e_datazoom(type = "slider") %>% 
                    e_line(Gas, smooth = TRUE) %>%
                    e_line(Oil, smooth = TRUE) %>%
                    e_line(Water, smooth = TRUE) %>%
                    e_title('Type Curve', 'Daily Rate (bpd/mcfd)') %>%
                    #e_legend(right = 0) %>%
                    e_theme('infographic')
            }
            
        })

        output$eurData <- renderText({
            paste0('Oil EUR: ', values$oilEUR, ' MBO, Gas EUR: ', values$gasEUR, ' MMCF, Water EUR: ', values$waterEUR, ' MBW')
        })
        
        observe({
            if(input$priceType == 'Current Strip'){
                shinyjs::show('hidePrice')
                shinyjs::hide('wti')
                shinyjs::hide('hh')
                
                crude <-'https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M'
                webpage <- read_html(crude)
                tbls_ls <- webpage %>%
                    html_nodes('table') %>%
                    .[5] %>%
                    html_table(fill = TRUE)
                wti1 <- tbls_ls[[1]]
                wti1 <- wti1 %>% filter(!is.na(Jan))
                
                wti1 <- wti1 %>% gather(DATE, WTI, -c(Year))
                wti1 <- wti1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
                wti1$DATE <- as.POSIXct(wti1$DATE, format = '%b/%d/%Y')
                wti1 <- wti1 %>% arrange(DATE) %>% select(DATE, WTI)
                
                crude <-'https://www.eia.gov/dnav/ng/hist/rngwhhdm.htm'
                webpage <- read_html(crude)
                tbls_ls <- webpage %>%
                    html_nodes('table') %>%
                    .[5] %>%
                    html_table(fill = TRUE)
                hh1 <- tbls_ls[[1]]
                hh1 <- hh1 %>% filter(!is.na(Jan))
                
                hh1 <- hh1 %>% gather(DATE, HH, -c(Year))
                hh1 <- hh1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
                hh1$DATE <- as.POSIXct(hh1$DATE, format = '%b/%d/%Y')
                hh1 <- hh1 %>% arrange(DATE) %>% select(DATE, HH)
                wti1 <- wti1 %>% filter(DATE >= min(hh1$DATE))
                wti1 <- wti1 %>% filter(!is.na(WTI))
                hh1 <- hh1 %>% filter(!is.na(HH))
                crude = 'https://www.wsj.com/market-data/quotes/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
                webpage <- read_html(crude)
                #tbls <- html_nodes(webpage, 'table')
                
                tbls_ls <- webpage %>%
                    html_nodes('table') %>%
                    .[1] %>%
                    html_table(fill = TRUE)
                
                wti <- tbls_ls[[1]]
                
                crude = 'https://www.wsj.com/market-data/quotes/futures/NATURAL%20GAS/contracts'
                webpage <- read_html(crude)
                #tbls <- html_nodes(webpage, 'table')
                
                tbls_ls <- webpage %>%
                    html_nodes('table') %>%
                    .[1] %>%
                    html_table(fill = TRUE)
                
                    
                
                    hh <- tbls_ls[[1]]
                
                
                
                rm(crude, webpage, tbls_ls)
                wti <- wti[,c('MONTH', 'SETTLEMENT')]
                hh <- hh[,c('MONTH', 'SETTLEMENT')]
                
                wti <- wti %>% filter(MONTH != 'Front Month')
                hh <- hh %>% filter(MONTH != 'Front Month')
                
                
                
                wti$YEAR <- substr(wti$MONTH, nchar(wti$MONTH)-3, nchar(wti$MONTH))
                wti$MONTH <- substr(wti$MONTH, nchar(wti$MONTH)-7, nchar(wti$MONTH)-5)
                
                
                hh$YEAR <- substr(hh$MONTH, nchar(hh$MONTH)-3, nchar(hh$MONTH))
                hh$MONTH <- substr(hh$MONTH, nchar(hh$MONTH)-7, nchar(hh$MONTH)-5)
                
                
                wti$DATE <- paste(wti$MONTH, '/01/', wti$YEAR, sep='')
                hh$DATE <- paste(hh$MONTH, '/01/', hh$YEAR, sep='')
                
                wti$DATE <- as.POSIXct(wti$DATE, format = '%b/%d/%Y')
                hh$DATE <- as.POSIXct(hh$DATE, format = '%b/%d/%Y')
                
                
                wti <- wti[,c('DATE', 'SETTLEMENT')]
                hh <- hh[,c('DATE', 'SETTLEMENT')]
                
                
                names(wti) <- c('DATE', 'WTI')
                names(hh) <- c('DATE', 'HH')
                
                date1 <- min(wti1$DATE)
                date2 <- max(hh$DATE)
                date3 <- data.frame(DATE = seq(0, 1000, 1))
                date3$DATE <- date1 %m+% months(date3$DATE) 
                date3 <- date3 %>% filter(DATE <= date2)
                wti <- rbind(wti1, wti)
                wti <- merge(date3, wti, by='DATE', all.x=TRUE)
                
                hh <- rbind(hh1, hh)
                
                price <- merge(wti, hh, by='DATE', all.x=TRUE, all.y=TRUE)
                
                rm(wti, hh)
                
                
                if (is.na(price$WTI[1])) {
                    price$WTI[1] <- price$WTI[2]
                }
                
                
                
                price$WTI <- na.locf(price$WTI)
                price$HH <- na.locf(price$HH)
                values$price <- price
                
                
            } else {
                shinyjs::hide('hidePrice')
                shinyjs::show('wti')
                shinyjs::show('hh')
                
            }
        })
        
        output$stripPrice <- renderEcharts4r({
            df <- values$price
            df$DATE <- as.character(df$DATE)
            
            df %>%
                #group_by(Component) %>%
                e_charts(DATE) %>%
                e_datazoom(type = "slider") %>% 
                e_tooltip(trigger = "axis") %>% 
                e_line(WTI, smooth = TRUE) %>%
                e_line(HH, y_index = 1, smooth = TRUE) %>%
                e_title('Forward Strip', 'Source: WSJ') %>%
                #e_legend(right = 0) %>%
                e_theme('infographic')
            
            # df %>% 
            #     e_charts(DATE) %>% 
            #     e_line(WTI) %>% 
            #     e_line(HH, x_index = 1, y_index = 1) %>% 
            #     e_grid(height = "35%") %>% 
            #     e_grid(height = "35%", top = "50%") %>% 
            #     e_y_axis(gridIndex = 1) %>% 
            #     e_x_axis(gridIndex = 1) %>% 
            #     e_tooltip(trigger = "axis") %>% 
            #     e_datazoom(x_index = c(0, 1))
            
        })
        
        
        observe({
            if(input$abandonmentMethod == 'Gas Rate'){
                shinyjs::show('qfGas')
                shinyjs::hide('qfOil')
                shinyjs::hide('wellLife')
            } else {
                if(input$abandonmentMethod == 'Oil Rate'){
                    shinyjs::show('qfOil')
                    shinyjs::hide('qfGas')
                    shinyjs::hide('wellLife')
                } else {
                    shinyjs::hide('qfOil')
                    shinyjs::hide('qfGas')
                    shinyjs::show('wellLife')
                }
            }
            
            
            if(input$tcProduct == 'Gas'){
                shinyjs::hide('qiOil')
                shinyjs::hide('bOil')
                shinyjs::hide('DiOil')
                shinyjs::hide('DfOil')
                shinyjs::hide('curtailOil')
                shinyjs::hide('qiWater')
                shinyjs::hide('bWater')
                shinyjs::hide('DiWater')
                shinyjs::hide('DfWater')
                shinyjs::hide('curtailWater')
                shinyjs::show('qiGas')
                shinyjs::show('bGas')
                shinyjs::show('DiGas')
                shinyjs::show('DfGas')
                shinyjs::show('curtailGas')
            } else {
                if(input$tcProduct == 'Oil') {
                    shinyjs::show('qiOil')
                    shinyjs::show('bOil')
                    shinyjs::show('DiOil')
                    shinyjs::show('DfOil')
                    shinyjs::show('curtailOil')
                    shinyjs::hide('qiGas')
                    shinyjs::hide('bGas')
                    shinyjs::hide('DiGas')
                    shinyjs::hide('DfGas')
                    shinyjs::hide('curtailGas')
                    shinyjs::hide('qiWater')
                    shinyjs::hide('bWater')
                    shinyjs::hide('DiWater')
                    shinyjs::hide('DfWater')
                    shinyjs::hide('curtailWater')
                } else {
                    shinyjs::hide('qiOil')
                    shinyjs::hide('bOil')
                    shinyjs::hide('DiOil')
                    shinyjs::hide('DfOil')
                    shinyjs::hide('curtailOil')
                    shinyjs::hide('qiGas')
                    shinyjs::hide('bGas')
                    shinyjs::hide('DiGas')
                    shinyjs::hide('DfGas')
                    shinyjs::hide('curtailGas')
                    shinyjs::show('qiWater')
                    shinyjs::show('bWater')
                    shinyjs::show('DiWater')
                    shinyjs::show('DfWater')
                    shinyjs::show('curtailWater')
                }
            }
            
            
            
        })
        
        declineValues <- reactive({
            data.frame(
                Component = c('qiOil', 'bOil', 'DiOil', 'DfOil', 'curtailOil', 'qfOil', 
                              'qiGas', 'bGas', 'DiGas', 'DfGas', 'curtailGas', 'qfGas',
                              'qiWater', 'bWater', 'DiWater', 'DfWater', 'curtailWater',
                              'wellLife', 'wti', 'hh', 'wtiDev', 'hhDev', 'wtiPDP', 'hhPDP',
                              'DfTC', 'bTC', 'wellLifeTC',  'lowB', 'highB', 
                              'lowDf', 'highDf'),
                
                Value = c(input$qiOil, input$bOil, input$DiOil, input$DfOil, input$curtailOil, input$qfOil,
                          input$qiGas, input$bGas, input$DiGas, input$DfGas, input$curtailGas, input$qfGas,
                          input$qiWater, input$bWater, input$DiWater, input$DfWater, input$curtailWater,
                          input$wellLife, input$wti, input$hh, input$wtiDev, input$hhDev, input$wtiPDP, input$hhPDP, input$DfTC,
                          input$bTC, input$wellLifeTC,  input$lowB, input$highB, 
                          input$lowDf, input$highDf),
                stringsAsFactors = FALSE) %>% spread(Component, Value)
            
        })
        
        expenseValues <- reactive({
            data.frame(
                Component = c('nri', 'spudToProd', 'drillCost', 'completeCost', 'pna', 'discOil', 
                              'discGas', 'discNGL', 'btu', 'shrink', 'nglYield', 'stxOil',
                              'stxGas', 'oilSTX', 'gasSTX', 'atx', 'yr1Fixed',
                              'yr2Fixed', 'finalFixed', 'varOilExp', 'varGasExp', 'varWaterExp', 
                              'varBOEExp', 'wrkExp',
                              'penalty', 'reversionIRR', 'nri2', 'wi', 'wi2',
                              'wiPDP', 'nriPDP', 'oilDiffPDP', 'hhDiffPDP', 'nglDiffPDP',
                              'shrinkPDP', 'nglYieldPDP', 'btuPDP', 'wellsPDP',
                              'fixedPDP', 'varGasPDP', 'varOilPDP', 'varWaterPDP', 'varBOEPDP',
                              'pnaPDP', 'stxOilPDP', 'stxGasPDP', 'oilSTXPDP', 'gasSTXPDP', 'atxPDP',
                              'effDate', 'pdpDisc'),
                
                Value = c(input$nri, input$spudToProd, input$drillCost, input$completeCost, input$pna, input$discOil,
                          input$discGas, input$discNGL, input$btu, input$shrink, input$nglYield, input$stxOil,
                          input$stxGas, input$oilSTX, input$gasSTX, input$atx, input$yr1Fixed,
                          input$yr2Fixed, input$finalFixed, input$varOilExp, input$varGasExp, input$varWaterExp, 
                          input$varBOEExp, input$wrkExp,
                          input$penalty, input$reversionIRR, input$nri2, input$wi, input$wi2,
                          input$wiPDP, input$nriPDP, input$oilDiffPDP, input$hhDiffPDP, input$nglDiffPDP,
                          input$shrinkPDP, input$nglYieldPDP, input$btuPDP, input$wellsPDP,
                          input$fixedPDP, input$varGasPDP, input$varOilPDP, input$varWaterPDP,
                          input$varBOEPDP, input$pnaPDP, input$stxOilPDP, input$stxGasPDP, input$oilSTXPDP,
                          input$gasSTXPDP, input$atxPDP, input$effDate, input$pdpDisc),
                stringsAsFactors = FALSE) %>% spread(Component, Value)
            
        })
        
        observe({
            if(input$reversion == 'None'){
                shinyjs::hide('reversionIRR')
                shinyjs::hide('penalty')
                shinyjs::hide('wi2')
                
                shinyjs::hide('nri2')
            } else if(input$reversion == 'IRR') {
                shinyjs::show('reversionIRR')
                shinyjs::hide('penalty')
                shinyjs::show('wi2')
                
                shinyjs::show('nri2')
            } else {
                shinyjs::hide('reversionIRR')
                shinyjs::show('penalty')
                shinyjs::show('wi2')
                shinyjs::show('nri2')
            }
        })
        
        output$economics <- renderText({
            df <- values$fcst
            #print(head(expenseValues()))
            df$Oil <- df$Oil*expenseValues()$wi/100
            #print(head(df))
            df$Gas <- df$Gas*expenseValues()$wi/100
            df$Water <- df$Water*expenseValues()$wi/100
            df$Sales_Gas <- df$Gas*expenseValues()$shrink
            
            df$NGL <- df$Gas*expenseValues()$nglYield/1000
            
            df$wi <- expenseValues()$wi
            
            
            df1 <- data.frame(Months = seq(((expenseValues()$spudToProd-1)*-1), 0, 1), Gas = 0, Oil = 0, Water = 0)
            df$capex <- 0
            df1$capex <- 0
            df1$capex[1] <- expenseValues()$drillCost*expenseValues()$wi/100
            df1$capex[nrow(df1)] <- expenseValues()$completeCost*expenseValues()$wi/100
            if(input$priceType == 'Flat'){
                df$oilPrice <- declineValues()$wti
                df$gasPrice <- declineValues()$hh
                df$nglPrice <- df$oilPrice*expenseValues()$discNGL/100
            } else {
                prices <- values$price
                prices <- prices %>% filter(DATE >= today())
                prices <- prices[(expenseValues()$spudToProd):nrow(prices),]
                if(nrow(prices) > nrow(df)){
                    prices <- prices[1:nrow(df),]
                }
                df$oilPrice <- NA
                df$oilPrice[1:nrow(prices)] <- prices$WTI
                df$gasPrice <- NA
                df$gasPrice[1:nrow(prices)] <- prices$HH
                rm(prices)
                df$oilPrice <- na.locf(df$oilPrice)
                df$gasPrice <- na.locf(df$gasPrice)
                df$nglPrice <- df$oilPrice*expenseValues()$discNGL/100
            }
            
            
            df$nri <- expenseValues()$nri#*expenseValues()$wi/100
            df$oilRev <- (df$oilPrice-expenseValues()$discOil)*df$nri/100*df$Oil
            df$gasRev <- (df$gasPrice-expenseValues()$discGas)*df$nri/100*df$Sales_Gas*expenseValues()$btu
            df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
            df$rev <- df$oilRev+df$gasRev+df$nglRev
            df$tax <- df$oilRev*expenseValues()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues()$stxGas/100 +
                df$Oil*df$nri/100*expenseValues()$oilSTX + df$Gas*df$nri/100*expenseValues()$gasSTX +
                df$rev*df$nri/100*expenseValues()$atx/100
            
            df$expense <- df$Oil*expenseValues()$varOilExp + df$Gas*expenseValues()$varGasExp + 
                (df$Oil + df$NGL)*expenseValues()$varBOEExp +  df$Water*expenseValues()$varWaterExp +
                expenseValues()$wrkExp*expenseValues()$wi/100 + expenseValues()$yr1Fixed*expenseValues()$wi/100
            
            df$expense[13:24] <- df$expense[13:24] - expenseValues()$yr1Fixed*expenseValues()$wi/100 + expenseValues()$yr2Fixed*expenseValues()$wi/100
            df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - expenseValues()$yr1Fixed*expenseValues()$wi/100 + expenseValues()$finalFixed*expenseValues()$wi/100
            
            
            df$nocf <- df$rev-df$tax-df$expense
            
            
            if(input$reversion == 'Penalty'){
                penalty <- expenseValues()$penalty/100*(expenseValues()$drillCost*expenseValues()$wi/100 + expenseValues()$completeCost*expenseValues()$wi/100)
                
                df$cumNOCF <- cumsum(df$nocf)
                df <- df %>% mutate(wi2 = replace(wi, cumNOCF > penalty, expenseValues()$wi2))
                df <- df %>% mutate(nri = replace(nri, cumNOCF > penalty, expenseValues()$nri2))
                df$Oil <- df$Oil/df$wi*df$wi2
                df$Gas <- df$Gas/df$wi*df$wi2
                df$Water <- df$Water/df$wi*df$wi2
                df$Sales_Gas <- df$Sales_Gas/df$wi*df$wi2
                df$NGL <- df$NGL/df$wi*df$wi2
                df$wi <- df$wi2
                
                df$oilRev <- (df$oilPrice-expenseValues()$discOil)*df$nri/100*df$Oil
                df$gasRev <- (df$gasPrice-expenseValues()$discGas)*df$nri/100*df$Sales_Gas*expenseValues()$btu
                df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
                df$rev <- df$oilRev+df$gasRev+df$nglRev
                df$tax <- df$oilRev*expenseValues()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues()$stxGas/100 +
                    df$Oil*df$nri/100*expenseValues()$oilSTX + df$Gas*df$nri/100*expenseValues()$gasSTX +
                    df$rev*df$nri/100*expenseValues()$atx/100
                
                df$expense <- df$Oil*expenseValues()$varOilExp + df$Gas*expenseValues()$varGasExp + 
                    (df$Oil + df$NGL)*expenseValues()$varBOEExp +  df$Water*expenseValues()$varWaterExp +
                    expenseValues()$wrkExp*expenseValues()$wi/100 + expenseValues()$yr1Fixed*expenseValues()$wi/100
                
                
                df$expense[13:24] <- df$expense[13:24] - expenseValues()$yr1Fixed*df$wi[13:24]/100 + expenseValues()$yr2Fixed*df$wi[13:24]/100
                df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - expenseValues()$yr1Fixed*df$wi[25:nrow(df)]/100 + expenseValues()$finalFixed*df$wi[25:nrow(df)]/100
                df$nocf <- df$rev-df$tax-df$expense
                df <- subset(df, select = -c(cumNOCF))
                df <- subset(df, select = -c(wi2))
                df <- as.data.frame(df)
            }
            
            if(input$reversion == 'IRR'){
                Missing <- setdiff(dput(names(df)), names(df1))  # Find names of missing columns
                df2 <- df1
                df2[Missing] <- 0                    # Add them, filled with '0's
                df2 <- df2[,names(df)]
                
                df3 <- rbind(df2, df)
                df3$fcf <- df3$nocf - df3$capex
                i <- 2
                IRR2 <- expenseValues()$reversionIRR/100
                IRRx <- 0
                while(IRRx <= IRR2 & i<=(nrow(df3))){
                    IRRx <- IRRcalc(df3$fcf[1:i], df3$Months[1:i])
                    i <- i + 1
                }
                
                i <- i - nrow(df2)
                df <- as.data.frame(df)
                
                if(i >= nrow(df)){
                    NULL
                } else {
                    #i <- i -1
                    df$nri[i:nrow(df)] <- expenseValues()$nri2
                    df$wi2 <- df$wi
                    df$wi2[i:nrow(df)] <- expenseValues()$wi2
                    df$Oil <- df$Oil/df$wi*df$wi2
                    df$Gas <- df$Gas/df$wi*df$wi2
                    df$Water <- df$Water/df$wi*df$wi2
                    df$Sales_Gas <- df$Sales_Gas/df$wi*df$wi2
                    df$NGL <- df$NGL/df$wi*df$wi2
                    df$wi <- df$wi2
                    
                    df$oilRev <- (df$oilPrice-expenseValues()$discOil)*df$nri/100*df$Oil
                    df$gasRev <- (df$gasPrice-expenseValues()$discGas)*df$nri/100*df$Sales_Gas*expenseValues()$btu
                    df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
                    df$rev <- df$oilRev+df$gasRev+df$nglRev
                    df$tax <- df$oilRev*expenseValues()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues()$stxGas/100 +
                        df$Oil*df$nri/100*expenseValues()$oilSTX + df$Gas*df$nri/100*expenseValues()$gasSTX +
                        df$rev*df$nri/100*expenseValues()$atx/100
                    
                    df$expense <- df$Oil*expenseValues()$varOilExp + df$Gas*expenseValues()$varGasExp + 
                        (df$Oil + df$NGL)*expenseValues()$varBOEExp +  df$Water*expenseValues()$varWaterExp +
                        expenseValues()$wrkExp*expenseValues()$wi/100 + expenseValues()$yr1Fixed*expenseValues()$wi/100
                    
                    df$expense[13:24] <- df$expense[13:24] - expenseValues()$yr1Fixed*df$wi[13:24]/100 + expenseValues()$yr2Fixed*df$wi[13:24]/100
                    df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - expenseValues()$yr1Fixed*df$wi[25:nrow(df)]/100 + expenseValues()$finalFixed*df$wi[25:nrow(df)]/100
                    df$nocf <- df$rev-df$tax-df$expense
                    df <- subset(df, select = -c(wi2))
                    df <- as.data.frame(df)
                }
                
                
                
                
                
                
                
            }
            
            if(input$econAban == 'Yes'){
                df <- df[nrow(df):1,]
                df$cumNOCF <- cumsum(df$nocf)
                df$prev <- df$cumNOCF- dplyr::lag(df$cumNOCF, n = 1L)
                prev <- which(df$prev > 0)[1]
                df <- df[prev:nrow(df),]
                df <- df[nrow(df):1,]
                df <- subset(df, select = -c(cumNOCF, prev))
            }
            df <- as.data.frame(df)
            df$pna <- 0
            df$pna[nrow(df)] <- expenseValues()$pna*df$wi[nrow(df)]/100
            
            Missing <- setdiff(dput(names(df)), names(df1))  # Find names of missing columns
            df1[Missing] <- 0                    # Add them, filled with '0's
            df1 <- df1[,names(df)]
            
            df <- rbind(df1, df)
            df$fcf <- df$nocf - df$capex - df$pna
            df$Months <- seq(0,nrow(df)-1,1)
            
            
           
            
            
            NPV10 <- sum(df$fcf/(1.1^(df$Months/12)))
            values$npv10 <- NPV10
            
            # if(sum(df$fcf) > 0){
            #     IRR1 <- 3
            #     loop <- 1
            #     while(sum(df$fcf/((1+IRR1)^(df$Months/12)))
            #           < 0){
            #         IRR1 <- IRR1 - 0.01
            #         loop = loop + 1
            #     }
            #     
            # }else {
            #     IRR1 <- 0
            # }
            IRR1 <- IRRcalc(df$fcf, df$Months)
            values$irr <- IRR1
            
            #values$oilEUR <- as.integer(sum(df$Oil)/1000)
            #values$gasEUR <- as.integer(sum(df$Gas)/1000)
            #values$waterEUR <- as.integer(sum(df$Water)/1000)
            
            paste0('WI Oil EUR: ', as.integer(sum(df$Oil)/1000), ' MBO, WI Sales Gas EUR: ', as.integer(sum(df$Sales_Gas)/1000), ' MMCF, WI NGL EUR: ',as.integer(sum(df$NGL)/1000), ' MBBL')
            
        })
        
        output$economicIRR <- renderText({
            NPV10 <- values$npv10
            IRR1 <- values$irr
            
            paste0('NPV10, M$: ', as.integer(NPV10/1000), ', IRR:', scales::percent(IRR1))
        })
        
        observeEvent(input$saveTC, {
            updateButton(session, 'saveTC', label = 'Prices and Revenue Not Saved', style = 'danger')
            df <- expenseValues()
            df1 <- declineValues()
            df1 <- subset(df1, select = -c(wti, hh))
            
            df <- cbind(df, df1)
            df$econAban <- input$econAban
            df$reversion <- input$reversion
            df$abandonmentMethod <- input$abandonmentMethod
            df$oilEUR <- values$oilEUR
            df$gasEUR <- values$gasEUR
            df$waterEUR <- values$waterEUR
            df$yr1Dev <- 0
            df$yr2Dev <- 0
            df$yr3Dev <- 0
            df$yr4Dev <- 0
            df$yr5Dev <- 0
            df$yr6Dev <- 0
            df$yr7Dev <- 0
            df$yr8Dev <- 0
            df$yr9Dev <- 0
            df$yr10Dev <- 0
            df$startDate <- today()
            df$wellCount <- 100
            df$id <- input$tcName
            
            if(is.null(values$devPlan)||nrow(values$devPlan)==0){
                values$devPlan <- df
            } else {
                df1 <- values$devPlan %>% filter(id != input$tcName)
                values$devPlan <- rbind(df1, df)
            }
 
            updateAwesomeRadio(session, 'tcLoads', choices = unique(values$devPlan$id), status = 'primary')
            Sys.sleep(1)
            updateButton(session, 'saveTC', label = 'Save TC to Development Plan', style = 'primary')
            #print(unique(values$devPlan$id))
            
        })
       
        observeEvent(input$removeTC, {
            id1 <- input$tcLoads
            df1 <- values$devPlan %>% filter(id != id1)
            values$devPlan <- df1
            if(nrow(values$devPlan) == 0){
                updateAwesomeRadio(session, 'tcLoads', choices = '',  status = 'primary') 
            } else {
                updateAwesomeRadio(session, 'tcLoads', choices = unique(values$devPlan$id),  status = 'primary')
            }
            
            df1 <- values$pudFcst %>% filter(id != id1)
            values$pudFcst <- df1
            
        })
       
        
        
      
        
        output$pdpInstruct <- renderText({
            if(input$pdpType == 'Production Only'){
                'Please Load Date (monthly in MM/DD/YYYY format), Gross Oil/Month, Gross Gas/Month, Gross Water/Month'
            } else {
                'Please Load Date (monthly in MM/DD/YYYY format), Gross WI Oil/Month, Gross WI Wet Gas/Month,
                Gross WI Sales Gas/Month, Gross WI NGL/Month, Gross WI Water/Month,
                Net Oil/Month, Net Sales Gas/Month, Net NGL/Month,Operating Expense/Month,
                Capital Expenditures/Month, P&A/Month'
            }
        })
        
        output$wellsInfo <- renderText({
            inFile <- input$file2
            if (is.null(inFile)){
                shinyjs::disable('calcDecline')
                return(NULL)
            }
            df <- read.csv(inFile$datapath, header=TRUE)
            values$tcWells <- df
            #print(head(df))
            
            df1 <- paste0('Wells Uploaded: ', length(unique(df[,1])))
            
            names(df)[1] <- 'well'
            df <- df %>% group_by(well) %>% mutate(count=n()) %>% ungroup() %>% group_by(well) %>% filter(max(count) >= 60)
            #print(head(df))
            df2 <- paste0(', Wells Greather than 5 years : ', length(unique(df$well)))
            shinyjs::enable('calcDecline')
            append(df1, df2)
            
        }
        )
        
        getmode <- function(v) {
            uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        
        observeEvent(input$calcDecline, {
            if(is.null(values$tcWells)){
                NULL
            } else {
                updateButton(session, 'calcDecline', 'Calculating', style = 'danger')
                shinyjs::disable('calcDecline')
                df <- values$tcWells
                names(df) <- c('well', 'date', 'volume')
                df$volume <- as.numeric(df$volume)
                df$id <- paste0(df$well, df$date)
                df <- df %>% filter(!duplicated(id))
                
                
                df <- subset(df, select = -c(id))
                #print(head(df))
                df$date1 <- df$date
                df$date <- as.POSIXct(df$date, format = '%m/%d/%Y')
                df$date1 <- as.POSIXct(df$date1, format = '%m-%d-%Y')
                #df$date <- anytime(df$date)
                df$date[is.na(df$date)] <- df$date1[is.na(df$date)]
                df <- subset(df, select = -c(date1))
                df <- df %>% arrange(well, date) %>% mutate(months = 1) %>%
                    group_by(well) %>% 
                    mutate(months = cumsum(months), fp.year = year(min(date)), lp.year=year(max(date))) %>% ungroup()
                zeroMonths <- df %>% filter(volume == 0) %>% group_by(well) %>% summarise(zeroMonths =n()) %>%ungroup()
                df <- merge(df, zeroMonths, by='well', all.x=TRUE) 
                df$hour <- hour(df$date)
                df$date[df$hour == 23] <- df$date[df$hour == 23] %m+% hours(1)
                #print(head(df))
                
                prod.data1 <- df %>% filter(lp.year == max(df$lp.year)) %>% group_by(well) %>% 
                    filter(max(months) >= 60) %>% ungroup()  %>%
                    arrange(well, date)
                prod.data1 <- as.data.frame(prod.data1)
                #print(head(prod.data1))
                if(nrow(prod.data1)>0){
                    
                    econSummary <- lapply(split(prod.data1, prod.data1[,'well']), function (well) tryCatch({
                        well <- as.data.frame(well)
                        well <- well %>% mutate(prodMonths = cumsum(months/months))
                        prod.oil <- well[,c('date', 'volume')]
                        
                        
                        prod.oil <- prod.oil %>% mutate(peak = if_else(volume == max(prod.oil$volume), date, as.POSIXct(today()))) %>% mutate(peak = min(peak)) %>% mutate(volume = if_else(date < peak, 0, volume))
                        
                        prod.oil <- prod.oil %>% mutate(cumOil = sum(volume), zeroOil = n()) %>% filter(volume > 0) %>% mutate(zeroOil = zeroOil - n(), prodMonths = cumsum((volume+1)/(volume+1)))
                        
                        if(nrow(prod.oil) < 3) {
                            arps <- data.frame(qi = 0, Di = 16, Df = 0.08, b = 1)
                            t.curtail <- 0
                            bFitOil <- list(arps, t.curtail)
                            names(bFitOil)[1] <- 'arps'
                            names(bFitOil)[2] <- 't.curtail'
                        } else {
                            bFitOil <- best.hyp2exp.curtailed.from.interval(
                                prod.oil$volume, prod.oil$prodMonths/12, t.begin = 0.0,
                                lower = c(0, 0.4, 0.01, 0.02, 0),
                                upper = c(max(prod.oil$volume)*12*2, 30, 2.5, 0.3, 24))$decline
                        }
                        
                       
                        df <- data.frame(b = round(bFitOil$arps$b,2),  Df = round(as.effective(bFitOil$arps$Df),3))
                        
                        df
                    },
                    error = function(e) {
                        e
                        NULL
                    }))
                    
                    
                    
                    terminalDeclines <- dplyr::bind_rows(econSummary) %>% filter(b < 2.5 & b >0.01) %>%
                        filter(as.nominal(Df) > 0.02 & as.nominal(Df) < 0.3)
                    values$terminalDeclines <- as.data.frame(terminalDeclines)
                    updateNumericInput(session, 'DfTC', 'Default Terminal Decline', value = getmode(terminalDeclines$Df))
                    #updateNumericInput(session, 'DiTC', 'Default Initial Decline', value = getmode(terminalDeclines$Di))
                    updateNumericInput(session, 'bTC', 'Default B-Factor', value = getmode(terminalDeclines$b))
                    #print(quantile(terminalDeclines$b))
                    #print(quantile(terminalDeclines$Df))
                } else {
                    values$terminalDeclines <- data.frame(b = declineValues()$bTC, Df = declineValues()$DfTC)
                }
                shinyjs::enable('calcDecline')
                updateButton(session, 'calcDecline', 'Calculate', style = 'primary')
            }
        })
        
        
        

        observeEvent(input$DfTC, {
            if(is.null(values$terminalDeclines)){
                NULL
            } else {
                updateSliderInput(session, 'lowDf', 'Minimum Terminal Decline', max = input$DfTC-0.0001)
                updateSliderInput(session, 'highDf', 'Maximum Terminal Decline', min = input$DfTC)
            }
        })
        observeEvent(input$bTC, {
            if(is.null(values$terminalDeclines)){
                NULL
            } else {
                updateSliderInput(session, 'lowB', 'Minimum B-Factor', max = input$bTC-0.0001)
                updateSliderInput(session, 'highB', 'Maximum B-Factor', min = input$bTC)
            }
        })

        
        observe({
            if(is.null(values$terminalDeclines)||nrow(values$terminalDeclines)==1){
                NULL
            } else {
                values$plotB <- values$terminalDeclines %>% filter(Df <= declineValues()$highDf) %>% filter(Df >= declineValues()$lowDf) %>%
                     filter(b >= declineValues()$lowB) %>%
                    filter(b <= declineValues()$highB) %>%
                    e_charts() %>%
                    #e_histogram(b, name = "histogram") %>%
                    e_density(b, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) %>%
                    e_tooltip(trigger = "axis")
                
                
                
                values$plotDf <- values$terminalDeclines %>% filter(Df <= declineValues()$highDf) %>% filter(Df >= declineValues()$lowDf) %>%
                    filter(b >= declineValues()$lowB) %>%
                    filter(b <= declineValues()$highB) %>%
                    e_charts() %>%
                    #e_histogram(b, name = "histogram") %>%
                    e_density(Df, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) %>%
                    e_tooltip(trigger = "axis")
            }
        })
        
        
        output$DfPlot <- renderEcharts4r({
            if(is.null(values$terminalDeclines)||nrow(values$terminalDeclines)==1){
                NULL
            } else {
                values$plotDf%>%
                    e_mark_line(data = list(xAxis = declineValues()$lowDf), title = 'Minimum Terminal Decline') %>%
                    e_mark_line(data = list(xAxis = declineValues()$highDf), title = 'Maximum Terminal Decline')
                
            }
        })
        
       
        
        output$bPlot <- renderEcharts4r({
            if(is.null(values$terminalDeclines)||nrow(values$terminalDeclines)==1){
                NULL
            } else {
               values$plotB %>%
                    e_mark_line(data = list(xAxis = declineValues()$lowB), title = 'Minimum B-Factor') %>%
                    e_mark_line(data = list(xAxis = declineValues()$highB), title = 'Maximum B-Factor')
                
            }
        })
        
        
        observeEvent(input$buildPDP, {
            if(is.null(values$tcWells)){
                NULL
            } else {
                updateButton(session, 'buildPDP', 'Calculating', style = 'danger')
                shinyjs::disable('buildPDP')
                df <- values$tcWells
                names(df) <- c('well', 'date', 'volume')
                df$volume <- as.numeric(df$volume)
                df$id <- paste0(df$well, df$date)
                df <- df %>% filter(!duplicated(id))
                
                
                df <- subset(df, select = -c(id))
                #print(head(df))
                df$date1 <- df$date
                df$date <- as.POSIXct(df$date, format = '%m/%d/%Y')
                df$date1 <- as.POSIXct(df$date1, format = '%m-%d-%Y')
                #df$date <- anytime(df$date)
                df$date[is.na(df$date)] <- df$date1[is.na(df$date)]
                df <- subset(df, select = -c(date1))
                df <- df %>% arrange(well, date) %>% mutate(months = 1) %>%
                    group_by(well) %>% 
                    mutate(months = cumsum(months), fp.year = year(min(date)), lp.year=year(max(date))) %>% ungroup()
                zeroMonths <- df %>% filter(volume == 0) %>% group_by(well) %>% summarise(zeroMonths =n()) %>%ungroup()
                df <- merge(df, zeroMonths, by='well', all.x=TRUE) 
                df$zeroMonths[is.na(df$zeroMonths)] <- 0
                df$hour <- hour(df$date)
                df$date[df$hour == 23] <- df$date[df$hour == 23] %m+% hours(1)
                names(df)[1:3] <- c('API', 'date', 'oil')
                #print(head(df))
                wellData <- df %>% group_by(API) %>% summarise(cumOil = sum(oil), lp.year = mean(lp.year), fp.year=mean(fp.year)) %>% ungroup()
                wellData$oilEUR <- NA
                wellData$oilEUR[wellData$lp.year < 2019] <- wellData$cumOil[wellData$lp.year < 2019]
                
                
                prod.data1 <- df %>% filter(lp.year == 2019) %>% group_by(API) %>% filter(max(months) >= 12) %>% 
                    ungroup()  %>% arrange(API, date)

                
                econSummary <- lapply(split(prod.data1, prod.data1[,'API']), function (well) tryCatch({
                    well <- as.data.frame(well)
                    well <- well %>% mutate(prodMonths = seq(1, nrow(well), 1))
                    prod.oil <- well[,c('date', 'oil')]
                    
                    prod.oil <- prod.oil %>% mutate(peak = if_else(oil == max(prod.oil$oil), date, as.POSIXct(today()))) %>% mutate(peak = min(peak)) %>% mutate(oil = if_else(date < peak, 0, oil))
                    prod.oil <- prod.oil %>% mutate(cumOil = sum(oil), zeroOil = n()) %>% filter(oil > 0) %>% mutate(zeroOil = zeroOil - n(), prodMonths = cumsum((oil+1)/(oil+1)))
                    #print(head(prod.oil))
                    if(nrow(prod.oil) < 3) {
                        arps <- data.frame(qi = 0, Di = 16.8, Df = declineValues()$DfTC, b = declineValues()$bTC)
                        t.curtail <- 0
                        bFitOil <- list(arps, t.curtail)
                        names(bFitOil)[1] <- 'arps'
                        names(bFitOil)[2] <- 't.curtail'
                    } else {
                        bFitOil <- best.hyp2exp.curtailed.from.interval(
                            prod.oil$oil, prod.oil$prodMonths/12, t.begin = 0.0,
                            lower = c(0, 0.4, declineValues()$lowB, as.nominal(declineValues()$lowDf), 0),
                            upper = c(max(prod.oil$oil)*12*2, 16.8, declineValues()$highB, as.nominal(declineValues()$highDf), 24))$decline
                    }
                    
                    if(nrow(prod.oil) == 0) {
                        bFitOil$t.curtail <- 0
                    } else {
                        bFitOil$t.curtail <- prod.oil$zeroOil[1]/12 + bFitOil$t.curtail
                    }
                    
                    well$fcstOil <- curtailed.q(arps.decline(
                        bFitOil$arps$qi, bFitOil$arps$Di, bFitOil$arps$b, bFitOil$arps$Df),
                        (bFitOil$t.curtail), well$prodMonths/12 - 1/12)/12.0
                    
                   #print(head(well))
                    
                    qiOilRisk <- sum(well$oil[(nrow(well)-2):nrow(well)])/sum(well$fcstOil[(nrow(well)-2):nrow(well)])
                    
                    
                    qiOilRisk[is.nan(qiOilRisk)] <- 1
                    
                    
                    bFitOil$arps$qi <- bFitOil$arps$qi*qiOilRisk
                   
                    
                    qiOilMax <- max(well$oil)*24
                   
                    if (bFitOil$arps$qi > qiOilMax) {
                        bFitOil$arps$qi <- qiOilMax
                    }
                    
                    #print(head(well))
                    well$fcstOil <- curtailed.q(arps.decline(
                        bFitOil$arps$qi, bFitOil$arps$Di, bFitOil$arps$b, bFitOil$arps$Df),
                        (bFitOil$t.curtail), well$prodMonths/12 - 1/12)/12.0
                    
                  
                    fcstOil <- curtailed.q(arps.decline(bFitOil$arps$qi, bFitOil$arps$Di, bFitOil$arps$b, bFitOil$arps$Df), bFitOil$t.curtail, seq(nrow(well)/12, 80, by=(1/12)))/12.0
                    
                    #print(head(fcstOil))
                    fcst <- as.data.frame(fcstOil)
                    rm(fcstOil)
                    
                    fcst <- fcst[1:(declineValues()$wellLifeTC*12-nrow(well)),]
                    #print(head(fcst))
                    oilEUR <- sum(well$oil) + sum(fcst)
                    
                    
                    df <- data.frame(API = well$API[1], oilEUR = oilEUR,
                                     qiOil = bFitOil$arps$qi, DiOil = bFitOil$arps$Di, 
                                     bOil = bFitOil$arps$b, DfOil = bFitOil$arps$Df, curtailOil = bFitOil$t.curtail)
                    
                    df
                },
                error = function(e) {
                    e
                    NULL
                }))
               
                
                eurData <- dplyr::bind_rows(econSummary)
                #print(head(eurData))
                data1 <- wellData %>% filter(fp.year >= 2014)
               
               
                eurData1 <- eurData %>% filter(API %in% data1$API)
                DiOilMin <- quantile(eurData1$DiOil, 0.25)
                DiOilMax <- quantile(eurData1$DiOil, 0.75)
               
                
                
                
                prod.data1 <- df %>% filter(lp.year == 2019) %>% 
                    group_by(API) %>% filter(max(months) < 12) %>% ungroup()  %>% arrange(API, date)
                
                econSummary <- lapply(split(prod.data1, prod.data1[,'API']), function (well) tryCatch({
                    well <- as.data.frame(well)
                    well <- well %>% mutate(prodMonths = seq(1, nrow(well), 1))
                    prod.oil <- well[,c('date', 'oil')]
                    
                    prod.oil <- prod.oil %>% mutate(peak = if_else(oil == max(prod.oil$oil), date, as.POSIXct(today()))) %>% mutate(peak = min(peak)) %>% mutate(oil = if_else(date < peak, 0, oil))
                    prod.oil <- prod.oil %>% mutate(cumOil = sum(oil), zeroOil = n()) %>% filter(oil > 0) %>% mutate(zeroOil = zeroOil - n(), prodMonths = cumsum((oil+1)/(oil+1)))
                    
                    if(nrow(prod.oil) < 3) {
                        arps <- data.frame(qi = 0, Di = 16.8, Df = declineValues()$DfTC, b = declineValues()$bTC)
                        t.curtail <- 0
                        bFitOil <- list(arps, t.curtail)
                        names(bFitOil)[1] <- 'arps'
                        names(bFitOil)[2] <- 't.curtail'
                    } else {
                        bFitOil <- best.hyp2exp.curtailed.from.interval(
                            prod.oil$oil, prod.oil$prodMonths/12, t.begin = 0.0,
                            lower = c(0, DiOilMin, declineValues()$lowB, as.nominal(declineValues()$lowDf), 0),
                            upper = c(max(prod.oil$oil)*12*2, DiOilMax, declineValues()$highB, as.nominal(declineValues()$highDf), 24))$decline
                    }
                    
                    if(nrow(prod.oil) == 0) {
                        bFitOil$t.curtail <- 0
                    } else {
                        bFitOil$t.curtail <- prod.oil$zeroOil[1]/12 + bFitOil$t.curtail
                    }
                    
                    well$fcstOil <- curtailed.q(arps.decline(
                        bFitOil$arps$qi, bFitOil$arps$Di, bFitOil$arps$b, bFitOil$arps$Df),
                        (bFitOil$t.curtail), well$prodMonths/12 - 1/12)/12.0
                    
                    
                    
                    qiOilRisk <- sum(well$oil[(nrow(well)-2):nrow(well)])/sum(well$fcstOil[(nrow(well)-2):nrow(well)])
                    
                    
                    qiOilRisk[is.nan(qiOilRisk)] <- 1
                    
                    
                    bFitOil$arps$qi <- bFitOil$arps$qi*qiOilRisk
                    
                    
                    qiOilMax <- max(well$oil)*24
                    
                    if (bFitOil$arps$qi > qiOilMax) {
                        bFitOil$arps$qi <- qiOilMax
                    }
                    
                    
                    well$fcstOil <- curtailed.q(arps.decline(
                        bFitOil$arps$qi, bFitOil$arps$Di, bFitOil$arps$b, bFitOil$arps$Df),
                        (bFitOil$t.curtail), well$prodMonths/12 - 1/12)/12.0
                    
                    
                    fcstOil <- curtailed.q(arps.decline(bFitOil$arps$qi, bFitOil$arps$Di, bFitOil$arps$b, bFitOil$arps$Df), bFitOil$t.curtail, seq(nrow(well)/12, 80, by=(1/12)))/12.0
                    
                    
                    fcst <- as.data.frame(fcstOil)
                    rm(fcstOil)
                    
                    fcst <- fcst[1:(declineValues()$wellLifeTC*12-nrow(well)),]
                    
                    oilEUR <- sum(well$oil) + sum(fcst)
                    
                    
                    df <- data.frame(API = well$API[1], oilEUR = oilEUR,
                                     qiOil = bFitOil$arps$qi, DiOil = bFitOil$arps$Di, 
                                     bOil = bFitOil$arps$b, DfOil = bFitOil$arps$Df, curtailOil = bFitOil$t.curtail)
                    
                    df
                },
                error = function(e) {
                    e
                    NULL
                }))
              
                eurData1 <- dplyr::bind_rows(econSummary)
                eurData <- rbind(eurData, eurData1)
                #print(head(eurData))
                
                names(eurData)[2] <- c('oilEUR1')
                wellData <- merge(wellData, eurData, by='API', all.x=TRUE)
                
                wellData$oilEUR[is.na(wellData$oilEUR)] <- wellData$oilEUR1[is.na(wellData$oilEUR)]
                
                wellData <- subset(wellData, select = -c(oilEUR1))
                wellData$DiOil[is.na(wellData$DiOil)] <- mean(wellData$DiOil, na.rm=TRUE)
                wellData$bOil[is.na(wellData$bOil)] <- mean(wellData$bOil, na.rm=TRUE)
                wellData$DfOil[is.na(wellData$DfOil)] <- mean(wellData$DfOil, na.rm=TRUE)
                wellData$curtailOil[is.na(wellData$curtailOil)] <- 0
                
                
                
                ipRates <- df %>% group_by(API) %>% summarise(qiOil = max(oil)*12) %>% ungroup()
                missing <- wellData %>% filter(is.na(qiOil))
                names(ipRates)[2] <- c('qiOil1')
                
                wellData <- merge(wellData, ipRates, by='API', all.x=TRUE)
                wellData$qiOil[is.na(wellData$qiOil)] <- wellData$qiOil1[is.na(wellData$qiOil)]
                
                
                wellData <- subset(wellData, select = -c(qiOil1))
                
                wellData1 <- wellData %>% filter(is.na(oilEUR))
                
                econSummary <- lapply(split(wellData1, wellData1[,'API']), function (well) tryCatch({
                    fcstOil <- curtailed.q(arps.decline(well$qiOil, well$DiOil, well$bOil, well$DfOil), well$curtailOil, seq(0, 80, by=(1/12)))/12.0
                    
                    fcst <- as.data.frame(fcstOil)
                    
                    fcst <- as.data.frame(fcst[1:(declineValues()$wellLifeTC*12-nrow(well)),])
                    names(fcst) <- c('fcstOil')
                    
                    fcst$API <- well$API
                    fcst <- fcst %>% group_by(API) %>% summarise(oilEUR1 = sum(fcstOil)) %>% ungroup()
                    fcst <- as.data.frame(fcst)
                    fcst
                    
                    
                },
                error = function(e) {
                    e
                    NULL
                }))
             
                
                wellData1 <- dplyr::bind_rows(econSummary)
                
                wellData <- merge(wellData, wellData1, by='API', all.x=TRUE)
                
                wellData$oilEUR[is.na(wellData$oilEUR)] <- wellData$oilEUR1[is.na(wellData$oilEUR)]
                #print(head(wellData))
                
                
                wellData <- subset(wellData, select = -c(oilEUR1))
                
                firstProd <- df %>% group_by(API) %>% summarise(firstProd = min(date), lastProd = max(date))
                wellData <- merge(wellData, firstProd, by='API', all.x=TRUE)
                wellData <- wellData %>% filter(!duplicated(API))
                
               econSummary <- lapply(split(wellData, wellData[,'API']), function (well) tryCatch({
                    fcstOil <- curtailed.q(arps.decline(well$qiOil, well$DiOil, well$bOil, well$DfOil), well$curtailOil, seq(0, 80, by=(1/12)))/12.0
                    
                    fcst <- as.data.frame(fcstOil)
                    fcst$date <- well$firstProd
                    fcst$API <- well$API
                    fcst$months <- seq(0,(nrow(fcst)-1),1)
                    fcst$date <- fcst$date %m+% months(fcst$months)
                    
                    if(well$lp.year < 2019){
                        fcst$fcstOil <- 0
                    }
                    
                    prodData <- df %>% filter(API == well$API) %>% arrange(API, date)
                    fcst$fcstOil[1:nrow(prodData)] <- prodData$oil
                    
                    fcst <- fcst[1:(declineValues()$wellLifeTC*12),]
                    
                    fcst <- fcst[,c('API', 'date', 'fcstOil')]
                    names(fcst) <- c('API', 'date', 'oil')
                    fcst$fp.year <- well$fp.year
                    fcst
                    
                    
                },
                error = function(e) {
                    e
                    NULL
                }))

                values$prodSummary <- dplyr::bind_rows(econSummary)
                values$pdpDeclineFactors <- wellData
                #print(head(wellData))
                #print(head(values$prodSummary))
                updateButton(session, 'buildPDP', 'AUTOCAST', style = 'primary')
                shinyjs::enable('buildPDP')
            }
        })
        
        
        output$autoGraph <- renderPlotly({
            if(is.null(values$prodSummary) || nrow(values$prodSummary) == 0){
                NULL
            } else {
                storeWarn<- getOption("warn")
                options(warn = -1)
                
                df <- values$prodSummary %>% group_by(fp.year, date) %>% summarise(Production = sum(oil)) %>% ungroup() %>% arrange(fp.year, date) %>%
                    mutate(fp.year = as.factor(fp.year))
                
                df$Production[year(df$date) <= 2019] <- df$Production[year(df$date) <= 2019]/days_in_month(df$date[year(df$date) <= 2019])
                df$Production[year(df$date) > 2019] <- df$Production[year(df$date) > 2019]/30.45
                
                plott <- plotly::plot_ly(df,
                                         x=~date, y=~Production,type='area',  mode = 'stack', stackgroup='one',fillcolor = ~fp.year, fill = ~fp.year)  %>%
                    layout(title = 'Daily Production By Year', yaxis = list(title='Daily Rate (bbl/mcf)'), xaxis=  list(title = ''))
                
                shinyjs::delay(expr =({
                    options(warn = storeWarn)
                }) ,ms = 100)
                
                plott
            }
            
       
            
            
        })
        
        output$autoTable <- DT::renderDataTable({
            if(is.null(values$pdpDeclineFactors)){
                NULL
            }  else {
                #print(head(values$pdpTable))
                df <- values$pdpDeclineFactors
                df$oilEUR <- as.integer(df$oilEUR)
                df$qiOil <- as.integer(df$qiOil/365)
                df$DiOil <- round(as.effective(df$DiOil), 3)
                df$bOil <- round(df$bOil, 2)
                df$DfOil <- round(as.effective(df$DfOil), 3)
                df$curtailOil <- round(df$curtailOil*12,0)
                df$firstProd <- as.Date(df$firstProd)
                #df <- subset(df, select = -c(lastProd))
                
                df <- df[,c('API', 'firstProd', 'fp.year', 'lp.year', 'cumOil', 'qiOil', 'bOil', 'DiOil', 'DfOil', 'curtailOil', 'oilEUR')]
                
                names(df) <- c('Well', 'First Production Date',  'First Production Year', 'Last Production Year','Cumulative Volume',
                               'Daily IP', 'B-Factor (H for ARIES)', 'Effective Initial Decline',  'Effective Terminal Decline',
                               'Flat Period, Months', 'EUR')
                
                DT::datatable(df, rownames = FALSE,
                              extensions = c('Buttons', 'Scroller'), 
                              options = list(
                                  dom = 'Bfrtip',
                                  scrollX = TRUE,
                                  scrollY = FALSE,
                                  deferRender = TRUE,
                                  paging = FALSE,
                                  searching = FALSE,
                                  buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                              ))  
            }
        })
      
        output$contents <- renderPlotly({
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, it will be a data frame with 'name',
            # 'size', 'type', and 'datapath' columns. The 'datapath'
            # column will contain the local filenames where the data can
            # be found.
            inFile <- input$file1

            if (is.null(inFile))
                return(NULL)

            df <- read.csv(inFile$datapath, header = TRUE)
           
            if(input$pdpType == 'Production Only'){
                names(df) <- c('Date', 'Oil', 'Gas', 'Water')
            } else {
                names(df) <- c('Date', 'Oil', 'Gas', 'Sales_Gas', 'NGL', 'Water',
                               'netOil', 'netGas', 'netNGL', 'expense',  'capex', 'pna')
            }
            #print(head(df))
            df$Date <- as.POSIXct(df$Date, format = '%m/%d/%Y')
            df$Date <- paste0(month(df$Date),'/01/', year(df$Date))
            df$Date <- as.POSIXct(df$Date, format = '%m/%d/%Y')
            df$hour <- hour(df$Date)
            df$Date[df$hour == 23] <- df$Date[df$hour == 23] %m+% hours(1)
            df <- subset(df, select = -c(hour))
            df <- as.data.frame(df)
            values$pdpCF <- df
            if(input$pdpType == 'Production Only'){
                names(df) <- c('DATE', 'OIL', 'GAS', 'WATER')
            } else{

                names(df) <- c('DATE', 'WI OIL', 'WI GAS','WI SALES GAS', 'WI NGL','WI WATER',
                               'NET OIL', 'NET GAS', 'NET NGL', 'EXPENSE', 'CAPEX', 'P&A')
            }
            plotly::plot_ly(df %>% gather(Component, Value, -DATE),
                            x=~DATE, y=~Value, color=~Component, type='scatter', mode='line')


        })
        
        observe({
            if(input$pdpType == 'Production Only'){
                shinyjs::show('wiPDP')
                shinyjs::show('nriPDP')
                shinyjs::show('shrinkPDP')
                shinyjs::show('nglYieldPDP')
                shinyjs::show('btuPDP')
                shinyjs::show('wellsPDP')
                shinyjs::show('fixedPDP')
                shinyjs::show('varOilPDP')
                shinyjs::show('varGasPDP')
                shinyjs::show('varWaterPDP')
                shinyjs::show('varBOEPDP')
                shinyjs::show('pnaPDP')
            } else {
                shinyjs::hide('wiPDP')
                shinyjs::hide('nriPDP')
                shinyjs::hide('shrinkPDP')
                shinyjs::hide('nglYieldPDP')
                shinyjs::hide('btuPDP')
                shinyjs::hide('wellsPDP')
                shinyjs::hide('fixedPDP')
                shinyjs::hide('varOilPDP')
                shinyjs::hide('varGasPDP')
                shinyjs::hide('varWaterPDP')
                shinyjs::hide('varBOEPDP')
                shinyjs::hide('pnaPDP')
            
            }
            
            if(input$pdpPrice == 'Flat'){
                shinyjs::show('wtiPDP')
                shinyjs::show('hhPDP')
                
            } else {
                shinyjs::hide('wtiPDP')
                shinyjs::hide('hhPDP')
            }
        })
        
        output$pdpPlot <- renderPlotly({
            if(is.null(values$pdpCF)||is.null(input$file1)){
                NULL
            } else {
                df <- values$pdpCF# %>% filter(Date >= input$effDate)
                if(input$pdpType == 'Production Only'){
                   df$Oil <- df$Oil * expenseValues()$wiPDP/100 
                   df$Gas <- df$Gas * expenseValues()$wiPDP/100
                   df$Water <- df$Water * expenseValues()$wiPDP/100
                   df$Sales_Gas <- df$Gas*expenseValues()$shrinkPDP*expenseValues()$btuPDP
                   df$NGL <- df$Gas*expenseValues()$nglYieldPDP/1000  
                   df$netOil <- df$Oil*expenseValues()$nriPDP/100
                   df$netGas <- df$Sales_Gas*expenseValues()$nriPDP/100
                   df$netNGL <- df$NGL*expenseValues()$nriPDP/100
                   df$wells <- expenseValues()$wellsPDP*expenseValues()$wiPDP/100
                   
                   
                   df1 <- df %>% filter(Date < input$effDate)
                   df <- df %>% filter(Date >= input$effDate)
                   wellDrop <- expenseValues()$wellsPDP*expenseValues()$wiPDP/100/(nrow(df)-1)
                   df$wellDrop <- 0
                   df$wellDrop[2:nrow(df)] <- wellDrop
                   df$wells <- df$wells - cumsum(df$wellDrop)
                   
                   df$expense <- df$Oil * expenseValues()$varOilPDP +
                       df$Gas * expenseValues()$varGasPDP + 
                       df$Water * expenseValues()$varWaterPDP +
                       (df$Oil + df$NGL)*expenseValues()$varBOEPDP +
                       df$wells * expenseValues()$fixedPDP
                   
                   df$capex <- 0
                   df$pna <- df$wellDrop * expenseValues()$pnaPDP
                   df1$pna <- 0
                   df1$expense <- df1$Oil * expenseValues()$varOilPDP +
                       df1$Gas * expenseValues()$varGasPDP + 
                       df1$Water * expenseValues()$varWaterPDP +
                       (df1$Oil + df1$NGL)*expenseValues()$varBOEPDP +
                       df1$wells * expenseValues()$fixedPDP
                   df1$capex <- 0
                   
                   df <- df[,c('Date', 'Oil', 'Gas', 'Water', 'Sales_Gas', 'NGL',
                               'netOil', 'netGas', 'netNGL', 'expense', 'capex', 'pna')]
                   df1 <- df1[,names(df)]
                   df <- rbind(df1, df)
                   
                } else {
                    df <- df[,c('Date', 'Oil', 'Gas', 'Water', 'Sales_Gas', 'NGL',
                                'netOil', 'netGas', 'netNGL', 'expense', 'capex', 'pna')]
                }
                values$pdpData <- df
                df <- df %>% filter(Date >= input$effDate)
                names(df) <- c('DATE', 'WI OIL', 'WI GAS', 'WI WATER', 'WI SALES GAS', 'WI NGL',
                               'NET OIL', 'NET GAS', 'NET NGL', 'EXPENSE', 'CAPEX', 'P&A')
                
                plotly::plot_ly(df %>% gather(Component, Value, -DATE),
                                x=~DATE, y=~Value, color=~Component, type='scatter', mode='line')
            }
        })
        
        output$pdpPV <- renderText({
            df <- values$pdpData %>% filter(Date >= input$effDate)
            prices <- values$price
            names(prices) <- c('Date', 'oilPrice', 'gasPrice')
            prices <- as.data.frame(prices)
            # if(nrow(prices) > nrow(df)){
            #     prices <- prices[1:nrow(df),]
            # }
            df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
            df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
            
            if(input$pdpPrice == 'Flat'){
                df$oilPrice <- declineValues()$wtiPDP
                df$gasPrice <- declineValues()$hhPDP
                df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
            } else {
                df <- as.data.frame(df)
                #str(df$Date)
                #str(prices$Date)
                df <- merge(df, prices, by='Date', all.x=TRUE)
                df <- as.data.frame(df)
                #print(head(df))
                
                #rm(prices)
                df$oilPrice[1:24][is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                df$gasPrice[1:24][is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                df$oilPrice <- na.locf(df$oilPrice)
                df$gasPrice <- na.locf(df$gasPrice)
                
                print(head(df))
                df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
                #print(head(df))
            }
            
            df$oilRev <- df$netOil * (df$oilPrice - expenseValues()$oilDiffPDP)
            df$gasRev <- df$netGas * (df$gasPrice - expenseValues()$hhDiffPDP)
            df$nglRev <- df$netNGL * (df$oilPrice * expenseValues()$nglDiffPDP/100)
            df$revenue <- df$oilRev+df$gasRev+df$nglRev
            #print(head(df))
            df$tax <- df$oilRev*expenseValues()$stxOilPDP/100 +
                (df$gasRev+df$nglRev)*expenseValues()$stxGasPDP/100 +
                df$netOil*expenseValues()$oilSTXPDP + df$netGas/expenseValues()$shrinkPDP*expenseValues()$gasSTXPDP +
                df$revenue*expenseValues()$atxPDP/100
            #print(head(df))
            df$nocf <- df$revenue - df$tax - df$expense
            #print(head(df))
            if(input$econLimitPDP == 'Yes'){
                row1 <- max(which(df$nocf >= 0))+1
                if(row1 >= nrow(df)){
                    NULL
                } else {
                    pnaSum <- sum(df$pna[row1:nrow(df)])
                    df$pna[(row1-1)] <- df$pna[(row1-1)]+pnaSum
                    df <- df[1:(row1-1),]
                }
                
            }
            #print(head(df))
            df$fcf <- df$nocf - df$capex - df$pna
            
            df$Months <- seq(0, nrow(df)-1, 1)
            df$pv <- df$fcf/(1+expenseValues()$pdpDisc/100)^(df$Months/12)
            values$pdpTable <- df
            paste0('PDP NPV, M$: ', dollar(as.integer(sum(df$pv)/1000)))
        })
        
        output$pdpTable <- DT::renderDataTable({
          if(is.null(values$pdpTable)){
              NULL
          }  else {
              #print(head(values$pdpTable))
              df <- values$pdpTable
              df <- subset(df, select = -c(Months, pv))
              df$Date <- as.Date(df$Date)
              df$expense <- dollar(df$expense)
              df$capex <- dollar(df$capex)
              df$pna <- dollar(df$pna)
              df$oilPrice <- dollar(df$oilPrice)
              df$gasPrice <- dollar(df$gasPrice)
              df$nglPrice <- dollar(df$nglPrice)
              df$oilRev <- dollar(df$oilRev)
              df$gasRev <- dollar(df$gasRev)
              df$nglRev <- dollar(df$nglRev)
              df$revenue <- dollar(df$revenue)
              df$tax <- dollar(df$tax)
              df$nocf <- dollar(df$nocf)
              df$fcf <- dollar(df$fcf)
              names(df) <- c('DATE', 'WI Oil, bbls',
                             'WI Gas, mcf', 'WI Water, bbls',
                             'WI Sales Gas, mcf', 'WI NGL, bbls',
                             'NRI Oil, bbls', 'NRI Gas, mcf',
                             'NRI NGL, bbls', 'Operating Expenses, $',
                             'Capital Expenditures, $', 'P&A, $',
                             'Oil Price, $/bbl', 'Gas Price, $/mcf',
                             'NGL Price, $/bbl', 'Oil Revenue, $',
                             'Gas Revenue, $', 'NGL Revenue, $', 'Revenue, $',
                             'Production Taxes', 'NOCF', 'FCF')
              DT::datatable(df, rownames = FALSE,
                            extensions = c('Buttons', 'Scroller'), 
                            options = list(
                              dom = 'Bfrtip',
                              scrollX = TRUE,
                              scrollY = FALSE,
                              deferRender = TRUE,
                              paging = FALSE,
                              searching = FALSE,
                              buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                            ))  
          }
        })
        
        output$pdpQtrs <- DT::renderDataTable({
          if(is.null(values$pdpTable)){
            NULL
          }  else {
            #print(head(values$pdpTable))
            
            
            df <- values$pdpTable
            df <- subset(df, select = -c(Months, pv))
            df$Date <- as.Date(df$Date)
            df$Period <- paste0('Q', quarter(df$Date), year(df$Date))
            

            df <- df %>% group_by(Period) %>% 
              summarise(Date = mean(Date),
                        Oil = as.integer(sum(Oil)/1000),
                        Gas = as.integer(sum(Gas)/1000),
                        Water = as.integer(sum(Water)/1000),
                        Sales_Gas = as.integer(sum(Sales_Gas)/1000),
                        NGL = as.integer(sum(NGL)/1000),
                        netOil = as.integer(sum(netOil)/1000),
                        netGas = as.integer(sum(netGas)/1000),
                        netNGL = as.integer(sum(netNGL)/1000),
                        expense = as.integer(sum(expense)/1000),
                        capex = as.integer(sum(capex)/1000),
                        pna = as.integer(sum(pna)/1000),
                        oilPrice = mean(oilPrice),
                        gasPrice = mean(gasPrice),
                        nglPrice = mean(nglPrice),
                        oilRev = as.integer(sum(oilRev)/1000),
                        gasRev = as.integer(sum(gasRev)/1000),
                        nglRev = as.integer(sum(nglRev)/1000),
                        revenue = as.integer(sum(revenue)/1000),
                        tax = as.integer(sum(tax)/1000),
                        nocf = as.integer(sum(nocf)/1000),
                        fcf = as.integer(sum(fcf)/1000)) %>% ungroup() %>%
              arrange(Date)
            
            df <- subset(df, select = -c(Date))
                        
            df$expense <- dollar(df$expense)
            df$capex <- dollar(df$capex)
            df$pna <- dollar(df$pna)
            df$oilPrice <- dollar(df$oilPrice)
            df$gasPrice <- dollar(df$gasPrice)
            df$nglPrice <- dollar(df$nglPrice)
            df$oilRev <- dollar(df$oilRev)
            df$gasRev <- dollar(df$gasRev)
            df$nglRev <- dollar(df$nglRev)
            df$revenue <- dollar(df$revenue)
            df$tax <- dollar(df$tax)
            df$nocf <- dollar(df$nocf)
            df$fcf <- dollar(df$fcf)
            names(df) <- c('PERIOD', 'WI Oil, mbbls',
                           'WI Gas, mmcf', 'WI Water, mbbls',
                           'WI Sales Gas, mmcf', 'WI NGL, mbbls',
                           'NRI Oil, mbbls', 'NRI Gas, mmcf',
                           'NRI NGL, mbbls', 'Operating Expenses, m$',
                           'Capital Expenditures, m$', 'P&A, m$',
                           'Oil Price, $/bbl', 'Gas Price, $/mcf',
                           'NGL Price, $/bbl', 'Oil Revenue, m$',
                           'Gas Revenue, m$', 'NGL Revenue, m$', 'Revenue, m$',
                           'Production Taxes, m$', 'NOCF, m$', 'FCF, m$')
            DT::datatable(df, rownames = FALSE,
                          extensions = c('Buttons', 'Scroller'), 
                          options = list(
                            dom = 'Bfrtip',
                            scrollX = TRUE,
                            scrollY = FALSE,
                            deferRender = TRUE,
                            paging = FALSE,
                            searching = FALSE,
                            buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                          ))  
          }
        })
        
        
        output$tcInfo <- renderText({
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                df <- values$devPlan %>% filter(id == input$tcLoads)
                paste0('Oil EUR: ', df$oilEUR, ' MBO, Gas EUR: ', df$gasEUR, ' MMCF, Water EUR: ',
                       df$waterEUR, ' MBW, Capex: ',(df$completeCost + df$drillCost)/1000, ' M$, Reversion?: ', df$reversion,
                       ', Economic Limit?: ', df$econAban, ', Well Count: ', df$wellCount)
            }
        })
        
        observe({
            if(input$priceSelection == 'Current Strip'){
                shinyjs::hide('wtiDev')
                shinyjs::hide('hhDev')
            } else {
                shinyjs::show('wtiDev')
                shinyjs::show('hhDev')
            }
          
          
        })
        
        observe({
            # if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
            #     NULL
            # } else {
                id1 <- input$tcLoads
                
                updateDateInput(session, 'startDate', value = values$devPlan$startDate[values$devPlan$id == id1])
                updateNumericInput(session, 'wellCount', value = values$devPlan$wellCount[values$devPlan$id == id1])
                updateNumericInput(session, 'yr1Dev', value = values$devPlan$yr1Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr2Dev', value = values$devPlan$yr2Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr3Dev', value = values$devPlan$yr3Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr4Dev', value = values$devPlan$yr4Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr5Dev', value = values$devPlan$yr5Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr6Dev', value = values$devPlan$yr6Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr7Dev', value = values$devPlan$yr7Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr8Dev', value = values$devPlan$yr8Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr9Dev', value = values$devPlan$yr9Dev[values$devPlan$id == id1])
                updateNumericInput(session, 'yr10Dev', value = values$devPlan$yr10Dev[values$devPlan$id == id1])
                
                
            # }
        })
        
        observeEvent(input$startDate, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$startDate <- input$startDate
                values$devPlan <- rbind(df, df1)
            }
        })
        
        observeEvent(input$wellCount, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$wellCount)){
                  df$wellCount <- df$wellCount
                } else {
                  df$wellCount <- yearValues()$wellCount
                  values$devPlan <- rbind(df, df1)
                }
            }
        })
        
        yearValues <- reactive({
            data.frame(
                Component = c('yr1Dev', 'yr2Dev', 'yr3Dev', 'yr4Dev', 'yr5Dev', 'yr6Dev', 
                              'yr7Dev', 'yr8Dev', 'yr9Dev', 'yr10Dev', 'wellCount'),
                
                Value = c(input$yr1Dev, input$yr2Dev, input$yr3Dev, input$yr4Dev, input$yr5Dev, input$yr6Dev,
                          input$yr7Dev, input$yr8Dev, input$yr9Dev, input$yr10Dev, input$wellCount),
                stringsAsFactors = FALSE) %>% spread(Component, Value)
            
        })
        
        
        observeEvent(input$yr1Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                
                if(is.na(yearValues()$yr1Dev)){
                  df$yr1Dev <- df$yr1Dev
                } else {
                  df$yr1Dev <- yearValues()$yr1Dev
                  values$devPlan <- rbind(df, df1)
                }
                
            }
        })
        
        observeEvent(input$yr2Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr2Dev)){
                  df$yr2Dev <- df$yr2Dev
                } else {
                  df$yr2Dev <- yearValues()$yr2Dev
                  values$devPlan <- rbind(df, df1)
                }
  
            }
        })
        
        observeEvent(input$yr3Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr3Dev)){
                  df$yr3Dev <- df$yr3Dev
                } else {
                  df$yr3Dev <- yearValues()$yr3Dev
                  values$devPlan <- rbind(df, df1)
                }

            }
        })
        
        observeEvent(input$yr4Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr4Dev)){
                  df$yr4Dev <- df$yr4Dev
                } else {
                  df$yr4Dev <- yearValues()$yr4Dev
                  values$devPlan <- rbind(df, df1)
                }

            }
        })
        
        observeEvent(input$yr5Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr5Dev)){
                  df$yr5Dev <- df$yr5Dev
                } else {
                  df$yr5Dev <- yearValues()$yr5Dev
                  values$devPlan <- rbind(df, df1)
                }

            }
        })
        
        observeEvent(input$yr6Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr6Dev)){
                  df$yr6Dev <- df$yr6Dev
                } else {
                  df$yr6Dev <- yearValues()$yr6Dev
                  values$devPlan <- rbind(df, df1)
                }

            }
        })
        
        observeEvent(input$yr7Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr7Dev)){
                  df$yr7Dev <- df$yr7Dev
                } else {
                  df$yr7Dev <- yearValues()$yr7Dev
                  values$devPlan <- rbind(df, df1)
                }
      
            }
        })
        
        observeEvent(input$yr8Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr8Dev)){
                  df$yr8Dev <- df$yr8Dev
                } else {
                  df$yr8Dev <- yearValues()$yr8Dev
                  values$devPlan <- rbind(df, df1)
                }
            }
        })
        
        observeEvent(input$yr9Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr9Dev)){
                  df$yr9Dev <- df$yr9Dev
                } else {
                  df$yr9Dev <- yearValues()$yr9Dev
                  values$devPlan <- rbind(df, df1)
                }
                
            }
        })
        
        observeEvent(input$yr10Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                if(is.na(yearValues()$yr10Dev)){
                  df$yr10Dev <- df$yr10Dev
                } else {
                  df$yr10Dev <- yearValues()$yr10Dev
                  values$devPlan <- rbind(df, df1)
                }
            }
        })
        
        output$rem1 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev)
        })
        
        output$rem2 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev - yearValues()$yr2Dev)
        })
        
        output$rem3 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev -
                       yearValues()$yr2Dev - yearValues()$yr3Dev)
        })
        
        output$rem4 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev -
                       yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev)
        })
        
        output$rem5 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev-
                       yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev -
                       yearValues()$yr5Dev)
        })
        
        output$rem6 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev-
                       yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev -
                       yearValues()$yr5Dev - yearValues()$yr6Dev)
        })
        
        output$rem7 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev-
                       yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev -
                       yearValues()$yr5Dev - yearValues()$yr6Dev - yearValues()$yr7Dev)
        })
        
        output$rem8 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev-
                       yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev -
                       yearValues()$yr5Dev - yearValues()$yr6Dev - yearValues()$yr7Dev -
                       yearValues()$yr8Dev)
        })
        
        output$rem9 <- renderText({
            paste0('Remaining Wells: ', yearValues()$wellCount - yearValues()$yr1Dev-
                       yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev -
                       yearValues()$yr5Dev - yearValues()$yr6Dev - yearValues()$yr7Dev -
                       yearValues()$yr8Dev - yearValues()$yr9Dev)
        })
        
        output$rem10 <- renderText({
            if(input$yr10Dev == 0 || is.null(input$yr10Dev) || is.na(input$yr10Dev)){
                'N/A'
            } else {
                wells <- yearValues()$wellCount - yearValues()$yr1Dev-
                           yearValues()$yr2Dev - yearValues()$yr3Dev - yearValues()$yr4Dev -
                           yearValues()$yr5Dev - yearValues()$yr6Dev - yearValues()$yr7Dev -
                           yearValues()$yr8Dev - yearValues()$yr9Dev
                
                wells <- as.integer(wells/yearValues()$yr10Dev*12)
                paste0('Remaining Months: ', wells)
            }
        })
        
        observeEvent(input$addFcst, {
            if(is.null(values$devPlan) || nrow(values$devPlan) == 0  || is.null(input$wellCount)){
                values$pudFcst <- NULL
            } else {
                updateButton(session, 'addFcst', label = 'Calculating...', style = 'danger')
                #print(head(values$price))
                dfx <- values$devPlan
                #print(head(dfx))
                dfx <- dfx[!duplicated(dfx),]
                dfx$effDate <- input$effDate
                dfx$startDate <- anytime(dfx$startDate)
                
                dfx$startDate <- paste0(month(dfx$startDate), '/01/',year(dfx$startDate))
                dfx$effDate <- paste0(month(dfx$effDate), '/01/',year(dfx$effDate))
                dfx$startDate <- anytime(dfx$startDate)
                dfx$effDate <- anytime(dfx$effDate)
                econSummary <- lapply(split(dfx, dfx[,'id']), function (well) tryCatch({
                    startDate1 <- data.frame(Month = seq(0, 30*12-1, 1))
                    startDate1$date <- min(well$startDate)
                    startDate1$date <- startDate1$date %m+% months(startDate1$Month)
                    startDate1$Month <- seq(1, 30*12, 1)
                    startDate1$wells <- 0
                    startDate1$wells[1:12] <-well$yr1Dev/12
                    startDate1$wells[13:24] <-well$yr2Dev/12
                    startDate1$wells[25:36] <-well$yr3Dev/12
                    startDate1$wells[37:48] <-well$yr4Dev/12
                    startDate1$wells[49:60] <-well$yr5Dev/12
                    startDate1$wells[61:72] <-well$yr6Dev/12
                    startDate1$wells[73:84] <-well$yr7Dev/12
                    startDate1$wells[85:96] <-well$yr8Dev/12
                    startDate1$wells[97:108] <-well$yr9Dev/12
                    startDate1$wells[109:nrow(startDate1)] <- well$yr10Dev/12
                    if(well$yr10Dev == 0){
                        startDate1 <- startDate1 %>% filter(wells != 0)
                    } else {
                        startDate1 <- startDate1 %>% filter(cumsum(wells) <= well$wellCount)
                        
                        startDate1 <- startDate1 %>% filter(wells != 0) %>% filter(!is.na(wells))
                        missingWells <- well$wellCount - sum(startDate1$wells)
                        #print(missingWells)
                        #startDate1$wells[nrow(startDate1)] <- startDate1$wells[nrow(startDate1)] + missingWells
                        
                    }
                    startDate1$id <- well$id
                    startDate1$id2 <- paste0(startDate1$id,startDate1$date)
                    startDate1 <- merge(startDate1, well, by='id', all.x=TRUE)
                    startDate1
                },
                error = function(e) {
                    e
                    NULL
                }
                )
                )
                
                dfx <- dplyr::bind_rows(econSummary)
                dfx <- dfx[!duplicated(dfx),]
                prices <- values$price
                names(prices) <- c('Date', 'oilPrice', 'gasPrice')
                prices <- as.data.frame(prices)
                
                
                if(nrow(dfx) == 0) {
                    values$pudFcst <- NULL
                } else {
                    dfx$wi <- dfx$wi *dfx$wells
                    dfx$wi2 <- dfx$wi2*dfx$wells
                    #print(head(dfx))
                    
                    
                    
                    econSummary <- lapply(split(dfx, dfx[,'id2']), function (df1) tryCatch({
                    
                        oil <- curtailed.q(arps.decline(
                            df1$qiOil*365, as.nominal(df1$DiOil), df1$bOil, as.nominal(df1$DfOil)),
                            df1$curtailOil/12.0, seq(0, 50-1/12, by = (1/12)))/12
                        gas <- curtailed.q(arps.decline(
                            df1$qiGas*365, as.nominal(df1$DiGas), df1$bGas, as.nominal(df1$DfGas)),
                            df1$curtailGas/12.0, seq(0, 50-1/12, by = (1/12)))/12
                        
                        water <- curtailed.q(arps.decline(
                            df1$qiWater*365, as.nominal(df1$DiWater), df1$bWater, as.nominal(df1$DfWater)),
                            df1$curtailWater/12.0, seq(0, 50-1/12, by = (1/12)))/12
                        
                        df <- data.frame(Months = seq(1, 50*12, by = 1), Gas = gas, Oil = oil, Water = water)
                        rm(oil, gas, water)
                        if(df1$abandonmentMethod == 'Time'){
                            #print(declineValues()$wellLife)
                            df <- df %>% filter(Months <= (df1$wellLife)*12) %>%filter(!duplicated(Months))
                            df <- as.data.frame(df)
                            #print(df %>% filter(Oil == 0))
                        }
                        
                        if(df1$abandonmentMethod == 'Oil Rate'){
                            df <- df %>% filter(Oil >= df1$qfOil*30.45)
                        }
                        if(df1$abandonmentMethod == 'Gas Rate'){
                            df <- df %>% filter(Gas >= df1$qfGas*30.45)
                        }
                        
                        df$Date <- df1$date %m+% months(df$Months -1 + df1$spudToProd)
                        df$Date <- df$Date %m+% days(1)
                        df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
                        df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
                        df$Oil <- df$Oil*df1$wi/100
                        df$Gas <- df$Gas*df1$wi/100
                        df$Water <- df$Water*df1$wi/100
                        df$Sales_Gas <- df$Gas*df1$shrink
                        df$NGL <- df$Gas*df1$nglYield/1000
                        df$wi <- df1$wi
                        df2 <- data.frame(Months = seq(((df1$spudToProd-1)*-1), 0, 1), Gas = 0, Oil = 0, Water = 0)
                        df$capex <- 0
                        df2$capex <- 0
                        df2$capex[1] <- df1$drillCost*df1$wi/100
                        df2$capex[nrow(df2)] <- df1$completeCost*df1$wi/100
                        df2$Date <- df$Date[1] %m+% months(df2$Months -1)
                        
                        
                        
                        if(input$priceSelection == 'Flat'){
                            df$oilPrice <- declineValues()$wtiDev
                            df$gasPrice <- declineValues()$hhDev
                            df$nglPrice <- df$oilPrice*df1$discNGL/100
                        } else if (input$priceSelection == 'Current Strip'){
                            df <- as.data.frame(df)
                            #str(df$Date)
                            #str(prices$Date)
                            df <- merge(df, prices, by='Date', all.x=TRUE)
                            df <- as.data.frame(df)
                            #print(head(df))
    
                            #rm(prices)
                            df$oilPrice[1:24][is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                            df$gasPrice[1:24][is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                            df$oilPrice <- na.locf(df$oilPrice)
                            df$gasPrice <- na.locf(df$gasPrice)
                            #df$oilPrice[is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                            #df$gasPrice[is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
    
                            df$nglPrice <- df$oilPrice*df1$discNGL/100
                            #print(head(df))
                        }
                        # 
                        # df$oilPrice <- 60
                        # df$gasPrice <- 2
                        # df$nglPrice <- 15
                        
                        df$nri <- df1$nri#*expenseValues()$wi/100
                        df$oilRev <- (df$oilPrice-df1$discOil)*df$nri/100*df$Oil
                        df$gasRev <- (df$gasPrice-df1$discGas)*df$nri/100*df$Sales_Gas*df1$btu
                        df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
                        df$rev <- df$oilRev+df$gasRev+df$nglRev
                        df$tax <- df$oilRev*df1$stxOil/100 + (df$gasRev+df$nglRev)*df1$stxGas/100 +
                            df$Oil*df$nri/100*df1$oilSTX + df$Gas*df1$nri/100*df1$gasSTX +
                            df$rev*df$nri/100*df1$atx/100
                        
                        df$expense <- df$Oil*df1$varOilExp + df$Gas*df1$varGasExp + df$Water*df1$varWaterExp +
                            (df$Oil + df$NGL)*df1$varBOEExp +
                            df1$wrkExp*df1$wi/100 + df1$yr1Fixed*df1$wi/100
                        
                        df$expense[13:24] <- df$expense[13:24] - df1$yr1Fixed*df1$wi/100 + df1$yr2Fixed*df1$wi/100
                        df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - df1$yr1Fixed*df1$wi/100 + df1$finalFixed*df1$wi/100
                        df$nocf <- df$rev-df$tax-df$expense
                        
                        
                        if(df1$reversion == 'Penalty'){
                            penalty <- df1$penalty/100*(df1$drillCost*df1$wi/100 + df1$completeCost*df1$wi/100)
                            
                            df$cumNOCF <- cumsum(df$nocf)
                            df <- df %>% mutate(wi2 = replace(wi, cumNOCF > penalty, df1$wi2))
                            df <- df %>% mutate(nri = replace(nri, cumNOCF > penalty, df1$nri2))
                            df$Oil <- df$Oil/df$wi*df$wi2
                            df$Gas <- df$Gas/df$wi*df$wi2
                            df$Water <- df$Water/df$wi*df$wi2
                            df$Sales_Gas <- df$Sales_Gas/df$wi*df$wi2
                            df$NGL <- df$NGL/df$wi*df$wi2
                            df$wi <- df$wi2
                            
                            df$oilRev <- (df$oilPrice-df1$discOil)*df$nri/100*df$Oil
                            df$gasRev <- (df$gasPrice-df1$discGas)*df$nri/100*df$Sales_Gas*df1$btu
                            df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
                            df$rev <- df$oilRev+df$gasRev+df$nglRev
                            df$tax <- df$oilRev*df1$stxOil/100 + (df$gasRev+df$nglRev)*df1$stxGas/100 +
                                df$Oil*df$nri/100*df1$oilSTX + df$Gas*df$nri/100*df1$gasSTX +
                                df$rev*df$nri/100*df1$atx/100
                            
                            df$expense <- df$Oil*df1$varOilExp + df$Gas*df1$varGasExp + df$Water*df1$varWaterExp +
                                (df$Oil + df$NGL)*df1$varBOEExp +
                                df1$wrkExp*df1$wi/100 + df1$yr1Fixed*df1$wi/100
                            
                            df$expense[13:24] <- df$expense[13:24] - df1$yr1Fixed*df$wi[13:24]/100 + df1$yr2Fixed*df$wi[13:24]/100
                            df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - df1$yr1Fixed*df$wi[25:nrow(df)]/100 + df1$finalFixed*df$wi[25:nrow(df)]/100
                            df$nocf <- df$rev-df$tax-df$expense
                            df <- subset(df, select = -c(cumNOCF))
                            df <- subset(df, select = -c(wi2))
                            df <- as.data.frame(df)
                        }
                        
                        if(df1$reversion == 'IRR'){
                            Missing <- setdiff(dput(names(df)), names(df2))  # Find names of missing columns
                            df3 <- df2
                            df3[Missing] <- 0                    # Add them, filled with '0's
                            df3 <- df3[,names(df)]
                            
                            df4 <- rbind(df3, df)
                            df4$fcf <- df4$nocf - df4$capex
                            i <- 2
                            IRR2 <- df1$reversionIRR/100
                            IRRx <- 0
                            while(IRRx <= IRR2 & i<=(nrow(df4))){
                                IRRx <- IRRcalc(df4$fcf[1:i], df4$Months[1:i])
                                i <- i + 1
                            }
                            
                            i <- i - nrow(df2)
                            df <- as.data.frame(df)
                            
                            if(i >= nrow(df)){
                                NULL
                            } else {
                                #i <- i -1
                                df$nri[i:nrow(df)] <- df1$nri2
                                df$wi2 <- df$wi
                                df$wi2[i:nrow(df)] <- df1$wi2
                                df$Oil <- df$Oil/df$wi*df$wi2
                                df$Gas <- df$Gas/df$wi*df$wi2
                                df$Water <- df$Water/df$wi*df$wi2
                                df$Sales_Gas <- df$Sales_Gas/df$wi*df$wi2
                                df$NGL <- df$NGL/df$wi*df$wi2
                                df$wi <- df$wi2
                                
                                df$oilRev <- (df$oilPrice-df1$discOil)*df$nri/100*df$Oil
                                df$gasRev <- (df$gasPrice-df1$discGas)*df$nri/100*df$Sales_Gas*df1$btu
                                df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
                                df$rev <- df$oilRev+df$gasRev+df$nglRev
                                df$tax <- df$oilRev*df1$stxOil/100 + (df$gasRev+df$nglRev)*df1$stxGas/100 +
                                    df$Oil*df$nri/100*df1$oilSTX + df$Gas*df$nri/100*df1$gasSTX +
                                    df$rev*df$nri/100*df1$atx/100
                                
                                df$expense <- df$Oil*df1$varOilExp + df$Gas*df1$varGasExp + df$Water*df1$varWaterExp +
                                    (df$Oil + df$NGL)*df1$varBOEExp +
                                    df1$wrkExp*df1$wi/100 + df1$yr1Fixed*df1$wi/100
                                
                                df$expense[13:24] <- df$expense[13:24] - df1$yr1Fixed*df$wi[13:24]/100 + df1$yr2Fixed*df$wi[13:24]/100
                                df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - df1$yr1Fixed*df$wi[25:nrow(df)]/100 + df1$finalFixed*df$wi[25:nrow(df)]/100
                                df$nocf <- df$rev-df$tax-df$expense
                                df <- subset(df, select = -c(wi2))
                                df <- as.data.frame(df)
                            }
                            
                            
                            
                            
                            
                            
                            
                        }
                        
                        if(df1$econAban == 'Yes'){
                            df <- df[nrow(df):1,]
                            df$cumNOCF <- cumsum(df$nocf)
                            df$prev <- df$cumNOCF- dplyr::lag(df$cumNOCF, n = 1L)
                            prev <- which(df$prev > 0)[1]
                            df <- df[prev:nrow(df),]
                            df <- df[nrow(df):1,]
                            df <- subset(df, select = -c(cumNOCF, prev))
                        }
                        df <- as.data.frame(df)
                        df$pna <- 0
                        df$pna[nrow(df)] <- df1$pna*df$wi[nrow(df)]/100
                        
                        Missing <- setdiff(dput(names(df)), names(df2))  # Find names of missing columns
                        df2[Missing] <- 0                    # Add them, filled with '0's
                        df2 <- df2[,names(df)]
                        
                        df <- rbind(df2, df)
                        df$fcf <- df$nocf - df$capex - df$pna
                        df <- df %>% filter(Date >= input$effDate)
                        df$Months <- seq(0,nrow(df)-1,1)
                        df$id <- df1$id
                        #df$shrink <- df1$shrink
                    
                        df
                    },
                    error = function(e) {
                        e
                        NULL
                    }
                    )
                    )
                    
                    df <- dplyr::bind_rows(econSummary)
                    df <- df[!duplicated(df),]
                    if(nrow(df) == 0){
                        values$pudFcst <- NULL
                    } else {
                        df$wells <- df$wi/100
                        df$netOil <- df$Oil*df$nri/100
                        df$netGas <- df$Sales_Gas*df$nri/100
                        df$netNGL <- df$NGL*df$nri/100
                        df$oilPrice[df$oilPrice == 0] <- NA
                        df$gasPrice[df$gasPrice == 0] <- NA
                        df$nglPrice[df$nglPrice == 0] <- NA
                        #print(head(df))
                        df1 <- df %>% group_by(Date, id) %>% 
                            summarise(Oil = sum(Oil), Gas = sum(Gas), Sales_Gas = sum(Sales_Gas), NGL = sum(NGL), Water = sum(Water), 
                                      netOil = sum(netOil), netGas = sum(netGas), netNGL = sum(netNGL), oilPrice = mean(oilPrice, na.rm=TRUE), 
                                      gasPrice = mean(gasPrice, na.rm=TRUE), nglPrice = mean(nglPrice, na.rm=TRUE), oilRev = sum(oilRev), gasRev = sum(gasRev),
                                      nglRev = sum(nglRev), revenue = sum(rev), tax = sum(tax), expense = sum(expense), nocf=sum(nocf), capex = sum(capex), pna = sum(pna), fcf = sum(fcf), wells = sum(wells))
                        #print(head(df))
                        df1$Date <- as.Date(df1$Date)
                        values$pudFcst <- df1
                        
                        
                    }
                }
                
                }
            updateButton(session, 'addFcst', label = 'Add to Forecast', style = 'primary')
            
            
        })
        
        output$pudYrFcst <- DT::renderDataTable({
          if(is.null(values$pudFcst) || nrow(values$pudFcst) == 0){
            NULL
          } else {
            if(is.null(values$pdpData)){
              
              
              df <- values$pudFcst %>% filter(Date >= input$effDate) %>% mutate(Year = year(Date)) %>%
                group_by(Year) %>%  summarise(Oil = as.integer(sum(Oil)), Gas = as.integer(sum(Gas)), Sales_Gas = as.integer(sum(Sales_Gas)), NGL = as.integer(sum(NGL)),
                                                                     Water = as.integer(sum(Water)),  netOil = as.integer(sum(netOil)), netGas = as.integer(sum(netGas)), netNGL = as.integer(sum(netNGL)),
                                                                     oilPrice = dollar(mean(oilPrice, na.rm=TRUE)),gasPrice = dollar(mean(gasPrice, na.rm=TRUE)), nglPrice = dollar(mean(nglPrice, na.rm=TRUE)),
                                                                     oilRev = dollar(as.integer(sum(oilRev))), gasRev = dollar(as.integer(sum(gasRev))),nglRev = dollar(as.integer(sum(nglRev))), 
                                                                     revenue = dollar(as.integer(sum(revenue))), tax = dollar(as.integer(sum(tax))), expense = dollar(as.integer(sum(expense))), 
                                                                     nocf= dollar(as.integer(sum(nocf))), capex = dollar(as.integer(sum(capex))), pna = dollar(as.integer(sum(pna))), fcf = dollar(as.integer(sum(fcf))))
            } else {
              
              df <- values$pdpData %>% filter(Date >= input$effDate)
              prices <- values$price
              names(prices) <- c('Date', 'oilPrice', 'gasPrice')
              prices <- as.data.frame(prices)
              df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
              df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
              #print(head(df))
              if(input$priceSelection == 'Flat'){
                df$oilPrice <- declineValues()$wtiDev
                df$gasPrice <- declineValues()$hhDev
                df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
              } else {
                df <- as.data.frame(df)
                #str(df$Date)
                #str(prices$Date)
                df <- merge(df, prices, by='Date', all.x=TRUE)
                df <- as.data.frame(df)
                #print(head(df))
                
                #rm(prices)
                df$oilPrice[1:24][is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                df$gasPrice[1:24][is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                df$oilPrice <- na.locf(df$oilPrice)
                df$gasPrice <- na.locf(df$gasPrice)
                #df$oilPrice[is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                #df$gasPrice[is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                
                df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
                #print(head(df))
              }
              df$oilRev <- df$netOil * (df$oilPrice - expenseValues()$oilDiffPDP)
              df$gasRev <- df$netGas * (df$gasPrice - expenseValues()$hhDiffPDP)
              df$nglRev <- df$netNGL * (df$oilPrice * expenseValues()$nglDiffPDP/100)
              df$revenue <- df$oilRev+df$gasRev+df$nglRev
              #print(head(df))
              df$tax <- df$oilRev*expenseValues()$stxOilPDP/100 +
                (df$gasRev+df$nglRev)*expenseValues()$stxGasPDP/100 +
                df$netOil*expenseValues()$oilSTXPDP + df$netGas/expenseValues()$shrinkPDP*expenseValues()$gasSTXPDP +
                df$revenue*expenseValues()$atxPDP/100
              #print(head(df))
              df$nocf <- df$revenue - df$tax - df$expense
              #print(head(df))
              if(input$econLimitPDP == 'Yes'){
                row1 <- max(which(df$nocf >= 0))+1
                if(row1 >= nrow(df)){
                  NULL
                } else {
                  pnaSum <- sum(df$pna[row1:nrow(df)])
                  df$pna[(row1-1)] <- df$pna[(row1-1)]+pnaSum
                  df <- df[1:(row1-1),]
                }
                
              }
              #print(head(df))
              df$fcf <- df$nocf - df$capex - df$pna
              
              df <- df[,c('Date', 'Oil', 'Gas', 'Sales_Gas', 'NGL', 'Water',
                          'netOil', 'netGas', 'netNGL', 'oilPrice', 'gasPrice', 'nglPrice',
                          'oilRev', 'gasRev', 'nglRev', 'revenue', 'tax', 'expense', 'nocf', 'capex', 'pna', 'fcf')]
              
              df$wells <- expenseValues()$wellsPDP
              df$id <- '1PDP'
              df1 <- df
              df <- values$pudFcst
              df <- as.data.frame(df)
              df1 <- as.data.frame(df1)
              df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
              df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
              df1$Date <- paste0(year(df1$Date),'-',month(df1$Date), '-01')
              df1$Date <- as.POSIXct(df1$Date, format = '%Y-%m-%d')
              #print(head(df))
              #print(head(df1))
              df1 <- df1[,names(df)]
              df <- rbind(df1, df)
              df$Date <- as.Date(df$Date)
              df <- df %>% filter(Date >= input$effDate) %>% mutate(Year = year(Date)) %>%
                group_by(Year) %>%  summarise(Oil = as.integer(sum(Oil)), Gas = as.integer(sum(Gas)), Sales_Gas = as.integer(sum(Sales_Gas)), NGL = as.integer(sum(NGL)),
                                              Water = as.integer(sum(Water)),  netOil = as.integer(sum(netOil)), netGas = as.integer(sum(netGas)), netNGL = as.integer(sum(netNGL)),
                                              oilPrice = dollar(mean(oilPrice, na.rm=TRUE)),gasPrice = dollar(mean(gasPrice, na.rm=TRUE)), nglPrice = dollar(mean(nglPrice, na.rm=TRUE)),
                                              oilRev = dollar(as.integer(sum(oilRev))), gasRev = dollar(as.integer(sum(gasRev))),nglRev = dollar(as.integer(sum(nglRev))), 
                                              revenue = dollar(as.integer(sum(revenue))), tax = dollar(as.integer(sum(tax))), expense = dollar(as.integer(sum(expense))), 
                                              nocf= dollar(as.integer(sum(nocf))), capex = dollar(as.integer(sum(capex))), pna = dollar(as.integer(sum(pna))), fcf = dollar(as.integer(sum(fcf))))
              
              
              
            }
            
            names(df) <- c('YEAR', 'OIL, BBL', 'GAS, MCF', 'SALES GAS, MCF', 'NGL, BBL', 'WATER, BBL', 'NET OIL, BBL', 'NET GAS, MCF', 'NET NGL, BBL', 'OIL PRICE, $', 'GAS PRICE, $', 'NGL PRICE, $', 'OIL REVENUE, $',
                           'GAS REVENUE, $', 'NGL REVENUE, $', 'TOTAL REVENUE, $', 'TAXES, $', 'OPERATING EXPENSE, $', 'NET OPERATING CASH FLOW, $', 'CAPITAL EXPENDITURES, $', 'P&A, $', 'FREE CASH FLOW, $')
            
            values$finCalc <- df
            DT::datatable(df, rownames = FALSE,
                          extensions = c('Buttons', 'Scroller'), 
                          options = list(
                            dom = 'Bfrtip',
                            scrollX = TRUE,
                            scrollY = FALSE,
                            deferRender = TRUE,
                            paging = FALSE,
                            searching = FALSE,
                            buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                          ))  
            
          }
        })
        
        
        output$pudFcst <- DT::renderDataTable({
            if(is.null(values$pudFcst) || nrow(values$pudFcst) == 0){
                NULL
            } else {
                if(is.null(values$pdpData)){
                
                    
                    df <- values$pudFcst %>% group_by(Date) %>%  summarise(Oil = as.integer(sum(Oil)), Gas = as.integer(sum(Gas)), Sales_Gas = as.integer(sum(Sales_Gas)), NGL = as.integer(sum(NGL)),
                                                                           Water = as.integer(sum(Water)),  netOil = as.integer(sum(netOil)), netGas = as.integer(sum(netGas)), netNGL = as.integer(sum(netNGL)),
                                                                           oilPrice = dollar(mean(oilPrice, na.rm=TRUE)),gasPrice = dollar(mean(gasPrice, na.rm=TRUE)), nglPrice = dollar(mean(nglPrice, na.rm=TRUE)),
                                                                           oilRev = dollar(as.integer(sum(oilRev))), gasRev = dollar(as.integer(sum(gasRev))),nglRev = dollar(as.integer(sum(nglRev))), 
                                                                           revenue = dollar(as.integer(sum(revenue))), tax = dollar(as.integer(sum(tax))), expense = dollar(as.integer(sum(expense))), 
                                                                           nocf= dollar(as.integer(sum(nocf))), capex = dollar(as.integer(sum(capex))), pna = dollar(as.integer(sum(pna))), fcf = dollar(as.integer(sum(fcf))),
                                                                           wells = as.integer(sum(wells)))
                } else {
                    
                        df <- values$pdpData %>% filter(Date >= input$effDate)
                        prices <- values$price
                        names(prices) <- c('Date', 'oilPrice', 'gasPrice')
                        prices <- as.data.frame(prices)
                        df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
                        df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
                        #print(head(df))
                        if(input$priceSelection == 'Flat'){
                            df$oilPrice <- declineValues()$wtiDev
                            df$gasPrice <- declineValues()$hhDev
                            df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
                        } else {
                            df <- as.data.frame(df)
                            #str(df$Date)
                            #str(prices$Date)
                            df <- merge(df, prices, by='Date', all.x=TRUE)
                            df <- as.data.frame(df)
                            #print(head(df))
                            
                            #rm(prices)
                            df$oilPrice[1:24][is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                            df$gasPrice[1:24][is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                            df$oilPrice <- na.locf(df$oilPrice)
                            df$gasPrice <- na.locf(df$gasPrice)
                            #df$oilPrice[is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                            #df$gasPrice[is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                            
                            df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
                            #print(head(df))
                        }
                        df$oilRev <- df$netOil * (df$oilPrice - expenseValues()$oilDiffPDP)
                        df$gasRev <- df$netGas * (df$gasPrice - expenseValues()$hhDiffPDP)
                        df$nglRev <- df$netNGL * (df$oilPrice * expenseValues()$nglDiffPDP/100)
                        df$revenue <- df$oilRev+df$gasRev+df$nglRev
                        #print(head(df))
                        df$tax <- df$oilRev*expenseValues()$stxOilPDP/100 +
                            (df$gasRev+df$nglRev)*expenseValues()$stxGasPDP/100 +
                            df$netOil*expenseValues()$oilSTXPDP + df$netGas/expenseValues()$shrinkPDP*expenseValues()$gasSTXPDP +
                            df$revenue*expenseValues()$atxPDP/100
                        #print(head(df))
                        df$nocf <- df$revenue - df$tax - df$expense
                        #print(head(df))
                        if(input$econLimitPDP == 'Yes'){
                            row1 <- max(which(df$nocf >= 0))+1
                            if(row1 >= nrow(df)){
                                NULL
                            } else {
                                pnaSum <- sum(df$pna[row1:nrow(df)])
                                df$pna[(row1-1)] <- df$pna[(row1-1)]+pnaSum
                                df <- df[1:(row1-1),]
                            }
                            
                        }
                        #print(head(df))
                        df$fcf <- df$nocf - df$capex - df$pna
                        
                        df <- df[,c('Date', 'Oil', 'Gas', 'Sales_Gas', 'NGL', 'Water',
                                    'netOil', 'netGas', 'netNGL', 'oilPrice', 'gasPrice', 'nglPrice',
                                    'oilRev', 'gasRev', 'nglRev', 'revenue', 'tax', 'expense', 'nocf', 'capex', 'pna', 'fcf')]
                        
                        df$wells <- expenseValues()$wellsPDP
                        df$id <- '1PDP'
                        df1 <- df
                        df <- values$pudFcst
                        df <- as.data.frame(df)
                        df1 <- as.data.frame(df1)
                        df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
                        df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
                        df1$Date <- paste0(year(df1$Date),'-',month(df1$Date), '-01')
                        df1$Date <- as.POSIXct(df1$Date, format = '%Y-%m-%d')
                        #print(head(df))
                        #print(head(df1))
                        df1 <- df1[,names(df)]
                        df <- rbind(df1, df)
                        df$Date <- as.Date(df$Date)
                        df <- df%>% group_by(Date) %>%  summarise(Oil = as.integer(sum(Oil)), Gas = as.integer(sum(Gas)), Sales_Gas = as.integer(sum(Sales_Gas)), NGL = as.integer(sum(NGL)),
                                                                               Water = as.integer(sum(Water)),  netOil = as.integer(sum(netOil)), netGas = as.integer(sum(netGas)), netNGL = as.integer(sum(netNGL)),
                                                                               oilPrice = dollar(mean(oilPrice, na.rm=TRUE)),gasPrice = dollar(mean(gasPrice, na.rm=TRUE)), nglPrice = dollar(mean(nglPrice, na.rm=TRUE)),
                                                                               oilRev = dollar(as.integer(sum(oilRev))), gasRev = dollar(as.integer(sum(gasRev))),nglRev = dollar(as.integer(sum(nglRev))), 
                                                                               revenue = dollar(as.integer(sum(revenue))), tax = dollar(as.integer(sum(tax))), expense = dollar(as.integer(sum(expense))), 
                                                                               nocf= dollar(as.integer(sum(nocf))), capex = dollar(as.integer(sum(capex))), pna = dollar(as.integer(sum(pna))), fcf = dollar(as.integer(sum(fcf))),
                                                                               wells = as.integer(sum(wells)))
                        
                        
                    
                }
                
                names(df) <- c('DATE', 'OIL, BBL', 'GAS, MCF', 'SALES GAS, MCF', 'NGL, BBL', 'WATER, BBL', 'NET OIL, BBL', 'NET GAS, MCF', 'NET NGL, BBL', 'OIL PRICE, $', 'GAS PRICE, $', 'NGL PRICE, $', 'OIL REVENUE, $',
                               'GAS REVENUE, $', 'NGL REVENUE, $', 'TOTAL REVENUE, $', 'TAXES, $', 'OPERATING EXPENSE, $', 'NET OPERATING CASH FLOW, $', 'CAPITAL EXPENDITURES, $', 'P&A, $', 'FREE CASH FLOW, $', 'NET WELLS')
                DT::datatable(df, rownames = FALSE,
                              extensions = c('Buttons', 'Scroller'), 
                              options = list(
                                  dom = 'Bfrtip',
                                  scrollX = TRUE,
                                  scrollY = FALSE,
                                  deferRender = TRUE,
                                  paging = FALSE,
                                  searching = FALSE,
                                  buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                              ))  

            }
        })
        
   
        output$cfGraph <- renderPlotly({
            if(is.null(values$pudFcst) || nrow(values$pudFcst) == 0){
                NULL
            } else {
                
                if(is.null(values$pdpData)){
                    
                
                df <- values$pudFcst %>% mutate(netBOE = netOil + netGas/6 + netNGL, netMCFE = netOil/6+netGas + netNGL/6)
                } else {
                    df <- values$pdpData %>% filter(Date >= input$effDate)
                    prices <- values$price
                    names(prices) <- c('Date', 'oilPrice', 'gasPrice')
                    prices <- as.data.frame(prices)
                    df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
                    df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
                    #print(head(df))
                    if(input$priceSelection == 'Flat'){
                        df$oilPrice <- declineValues()$wtiDev
                        df$gasPrice <- declineValues()$hhDev
                        df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
                    } else {
                        df <- as.data.frame(df)
                        #str(df$Date)
                        #str(prices$Date)
                        df <- merge(df, prices, by='Date', all.x=TRUE)
                        df <- as.data.frame(df)
                        #print(head(df))
                        
                        #rm(prices)
                        df$oilPrice[1:24][is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                        df$gasPrice[1:24][is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                        df$oilPrice <- na.locf(df$oilPrice)
                        df$gasPrice <- na.locf(df$gasPrice)
                        #df$oilPrice[is.na(df$oilPrice)] <- mean(df$oilPrice, na.rm=TRUE)
                        #df$gasPrice[is.na(df$gasPrice)] <- mean(df$gasPrice, na.rm=TRUE)
                        
                        df$nglPrice <- df$oilPrice*expenseValues()$nglDiffPDP/100
                        #print(head(df))
                    }
                    df$oilRev <- df$netOil * (df$oilPrice - expenseValues()$oilDiffPDP)
                    df$gasRev <- df$netGas * (df$gasPrice - expenseValues()$hhDiffPDP)
                    df$nglRev <- df$netNGL * (df$oilPrice * expenseValues()$nglDiffPDP/100)
                    df$revenue <- df$oilRev+df$gasRev+df$nglRev
                    #print(head(df))
                    df$tax <- df$oilRev*expenseValues()$stxOilPDP/100 +
                        (df$gasRev+df$nglRev)*expenseValues()$stxGasPDP/100 +
                        df$netOil*expenseValues()$oilSTXPDP + df$netGas/expenseValues()$shrinkPDP*expenseValues()$gasSTXPDP +
                        df$revenue*expenseValues()$atxPDP/100
                    #print(head(df))
                    df$nocf <- df$revenue - df$tax - df$expense
                    #print(head(df))
                    if(input$econLimitPDP == 'Yes'){
                        row1 <- max(which(df$nocf >= 0))+1
                        if(row1 >= nrow(df)){
                            NULL
                        } else {
                            pnaSum <- sum(df$pna[row1:nrow(df)])
                            df$pna[(row1-1)] <- df$pna[(row1-1)]+pnaSum
                            df <- df[1:(row1-1),]
                        }
                        
                    }
                    #print(head(df))
                    df$fcf <- df$nocf - df$capex - df$pna
                    
                    df <- df[,c('Date', 'Oil', 'Gas', 'Sales_Gas', 'NGL', 'Water',
                                'netOil', 'netGas', 'netNGL', 'oilPrice', 'gasPrice', 'nglPrice',
                                'oilRev', 'gasRev', 'nglRev', 'revenue', 'tax', 'expense', 'nocf', 'capex', 'pna', 'fcf')]
                    
                    df$wells <- expenseValues()$wellsPDP
                    df$id <- '1PDP'
                    df1 <- df
                    df <- values$pudFcst
                    df <- as.data.frame(df)
                    df1 <- as.data.frame(df1)
                    df$Date <- paste0(year(df$Date),'-',month(df$Date), '-01')
                    df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
                    df1$Date <- paste0(year(df1$Date),'-',month(df1$Date), '-01')
                    df1$Date <- as.POSIXct(df1$Date, format = '%Y-%m-%d')
                    #print(head(df))
                    #print(head(df1))
                    df1 <- df1[,names(df)]
                    df <- rbind(df1, df)
                    df$Date <- as.Date(df$Date)
                    df <- df %>% mutate(netBOE = netOil + netGas/6 + netNGL, netMCFE = netOil/6+netGas + netNGL/6)
                    
                }
                
                
                df <- df %>% gather(Component, Value, -c(Date, id)) %>% filter(Component == input$graphSelect)
                
                plot_ly(df, x = ~Date, y = ~Value, name = ~id, type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = ~id) %>%
                    layout(title = 'Metric By Type-Curve',
                           xaxis = list(title = "",
                                        showgrid = FALSE),
                           yaxis = list(title = "Volume in Monthly BOE/MCFE, Values in $'s",
                                        showgrid = FALSE))
            }
            
            # df %>% group_by(id) %>% arrange(Date) %>% mutate(Value = cumsum(Value)) %>% 
            #     plot_ly(type = 'scatter', x = ~Date, y = ~Value, color = ~id, 
            #             mode = 'lines', fill = 'tonexty')%>%
            #     layout(title = 'Metric By Type-Curve',
            #            xaxis = list(title = "",
            #                         showgrid = FALSE),
            #            yaxis = list(title = "Volume in Monthly BBLS/MCF, Values in $'s",
            #                         showgrid = FALSE))
            
            
            
        })
        
    }
)