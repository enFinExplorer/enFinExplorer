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
#library(bs4Dash)
library(shinyBS)
library(tableHTML)
library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(tidyRSS)
library(rtweet)
library(lubridate)
#library(plotly)
library(quantmod)
library(aRpsDCA)



api_key <- "YD0LELAB40MThlu12RlH3WVZl"
api_secret_key <- "DgFSQcNJQlVPA1kFHSusITTRej9Fa4TIFs4Ea98FYRxIcGeOZ7"
access_token <- "26022505-ZaOSEs58RDXz8EuuOcaLxunNwjKv9JHZhmGt8Ubg3"
access_token_secret <- "Zys1kmkfYDcTTYPJ8G2vXu6kNp4iLdMlvB46vqIwwOfWT"
## authenticate via web browser
token <- create_token(
    app = "enFinExplorer",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret)

options(stringsAsFactors = FALSE)
options(scipen = 999)

opList1 <- c('APA',  'AR', 'AREX', 'BCEI', 'BRY', 'CDEV', 'CHAP', 'CHK', 'CLR',
             'CNX', 'COG', 'COP', 'CPE', 'CRC', 'CRK', 'CRZO', 'CXO', 'DNR', 'DVN', 'ECA','EOG',
             'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP', 'HES', 'HPR', 'JAG', 'LLEX', 'LPI', 'MGY', 'MR',
             'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP',
             'REI', 'RRC', 'SBOW', 'SD', 'SM', 'SWN', 'UPLC', 'WLL', 'WPX', 'WTI', 'XEC', 'XOG', 'XOM')  
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

opList1 <- c('APA',  'AR', 'AREX', 'BCEI', 'BRY', 'CDEV', 'CHAP', 'CHK', 'CLR',
             'CNX', 'COG', 'COP', 'CPE', 'CRC', 'CRK', 'CRZO', 'CXO', 'DNR', 'DVN', 'ECA','EOG',
             'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP', 'HES', 'HPR', 'JAG', 'LLEX', 'LPI', 'MGY', 'MR',
             'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP',
             'REI', 'RRC', 'SBOW', 'SD', 'SM', 'SWN', 'UPLC', 'WLL', 'WPX', 'WTI', 'XEC', 'XOG', 'XOM')  

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
# cards
flowCard <- tablerCard(
    title = "Options Visual",
    closable = FALSE,
    zoomable = TRUE,
    options = tagList(
        tablerIcon(name = 'usd', lib=c('font-awesome'))
    ),
    width = 12,
    echarts4rOutput("flowGl")
)

profileCard <- tablerProfileCard(
    width = 12,
    title = "energyfinexplorer@gmail.com",
    subtitle = "Oil and Gas Equity Analysis",
    background = "",
    src = "bd.jpg",
    tablerSocialLinks(
        
        tablerSocialLink(
            name = "twitter",
            href = "https://www.twitter.com/Brandon17832728",
            icon = "twitter"
        )
    )
)




# app
shiny::shinyApp(
    ui = tablerDashPage(
        
        
        
        title = "EnFinExplorer",
        enable_preloader = TRUE,
        loading_duration = 1,
        navbar = tablerDashNav(
            id = "mymenu",
            src = "rig.png",
            h3('Energy Financial Explorer'),
            navMenu = tablerNavMenu(
                tablerNavMenuItem(
                    tabName = "home",
                    icon = "home",
                    "Company Analysis"
                ),
                tablerNavMenuItem(
                    tabName = "Options",
                    icon = "box",
                    "Derivatives"
                ),
                tablerNavMenuItem(
                    tabName = "typeCurves",
                    icon = 'box',
                    "Type Curves"
                ),
                tablerNavMenuItem(
                    tabName = "devPlan",
                    icon = 'box',
                    "Development Plan"
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
                    
                    #br(),
                    
                    #br(),
                    #a(img(src = 'argonDash.svg', height = '30'),href='https://rinterface.github.io/argonDash/', target='_blank'),
                    #a(img(src = 'argonR.svg', height = '30'),href='https://rinterface.com/shiny/argonR/',target='_blank'),
                    a(img(src = 'tablerDash.svg', height = '30'),href='https://github.com/RinteRface/tablerDash',target='_blank'),
                    #br(),
                    a(img(src = 'dplyr.png', height = '30'),href='https://dplyr.tidyverse.org/',target='_blank'),
                    a(img(src = 'rvest.png', height = '30'),href='https://github.com/tidyverse/rvest',target='_blank'),
                    a(img(src = 'tidyverse.png', height = '30'),href='https://www.tidyverse.org/',target='_blank'),
                    #br(),
                    a(img(src = 'lubridate.png', height = '30'),href='https://lubridate.tidyverse.org/',target='_blank'),
                    #a(img(src = 'naniar.png', height = '30'), href='https://github.com/njtierney/naniar',target='_blank'),
                    a(img(src = 'rtweet.png', height = '30'), href='https://rtweet.info/',target='_blank'),
                    a(img(src = 'tableHTML.png', height = '30'), href='https://github.com/LyzandeR/tableHTML',target='_blank'),
                    a(img(src = 'echarts.png', height = '30'), href='https://echarts4r.john-coene.com/index.html',target='_blank'),
                    a(img(src = 'shinyEffects.png', height = '30'), href='https://github.com/RinteRface/shinyEffects',target='_blank'),
                    br(),

                    a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30"), href='',target='_blank'),
                    a(img(src = "love.png", height = "30"), href='https://deanattali.com/shinyjs/',target='_blank'),
                    br(),
                    
                    #(a("    shinyauthr    ", href="https://github.com/PaulC91/shinyauthr",target='_blank')),
                    #br(),
                    (a("    shinyBS    ", href="https://ebailey78.github.io/shinyBS/index.html",target='_blank')),
                    br(),
                    (a("    shinyWidgets    ", href="https://github.com/dreamRs/shinyWidgets",target='_blank')),
                    br(),
                    (a('    quantmod    ', href = 'https://github.com/joshuaulrich/quantmod',target='_blank')),
                    br(),
                    #(a('    sodium    ', href = 'https://github.com/jeroen/sodium',target='_blank')),
                    #br(),
                    (a('    DT    ', href = 'https://github.com/rstudio/DT',target='_blank')),
                    br(),
                    #(a('    formattable    ', href = 'https://renkun-ken.github.io/formattable/',target='_blank')),
                    #br(),
                    (a('    tidyRSS    ', href = 'https://github.com/RobertMyles/tidyRSS',target='_blank')),
                    br(),
                    (a('    aRpsDCA    ', href = 'https://github.com/derrickturk/aRpsDCA',target='_blank'))
                    
                    
                    
                
                
                
            )
        ),
        
        body = tablerDashBody(
            
            
            setZoom(class = "card"),
            chooseSliderSkin("Nice"),
            useShinyjs(),
            tablerTabItems(
                tablerTabItem('home',
                              selectizeInput('operatorSelect', 'Select Operator', choices = opList1),
                              fluidRow(
                                  column(6,
                                         tablerCard(
                                             title = 'Stock Price (Candlestick)',
                                             closable = FALSE,
                                             zoomable = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             dateInput(inputId = 'start_date', label = 'Start Date', value = '2019-01-01'),
                                             echarts4rOutput('hcplot')
                                             
                                         )
                                         
                                  ),
                                  column(6,
                                         tablerCard(
                                             title = 'S&P 500 Benchmarking',
                                             closable = FALSE,
                                             zoomable = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             dateInput(inputId = 'start_date1', label = 'Start Date', value = '2019-01-01'),
                                             multiInput('operatorSelect1', 'Select Operators', choices = opList1, selected = 'APA'),
                                            
                                             echarts4rOutput('hcplot1')
                                             
                                         )
                                         
                                  )
                              ),
                              fluidRow(
                                  column(6,
                                         tablerCard(
                                             title =icon('newspaper'),
                                             closable = FALSE,
                                             zoomable = FALSE,
                                             collapsed = FALSE,
                                             width = 12,

                                             DT::dataTableOutput('news')
                                             
                                         )
                                         
                                  ),
                                  column(6,
                                         tablerCard(
                                             title = icon('twitter'),
                                             closable = FALSE,
                                             zoomable = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             DT::dataTableOutput('tweets')
                                             
                                         )
                                         
                                  )
                              ),
                              
                              fluidRow(
                                  column(6,
                                         tablerCard(
                                             title ='SEC Quarterly/Annual Filings' ,
                                             closable = FALSE,
                                             zoomable = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             
                                                
                                                
                                                bsButton('loadFilings', 'LOAD', style='primary', size='extra-small'),
                                                DT::dataTableOutput('filingList')
                                                 
                                             )
                                         
                                    ),
                                  column(6,
                                         tablerCard(
                                             title ='Load Tables' ,
                                             closable = FALSE,
                                             zoomable = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             selectizeInput('Filing', 'Available Periods', choices = '', selected = NULL),
                                             
                                             
                                             
                                             bsButton('loadTables', 'LOAD', style='primary', size='extra-small')
                                             
                                         )
                                         
                                  )
                                  ),
                              
                                  fluidRow(
                                      
                                          column(6,
                                                 tablerCard(
                                                     title = 'Balance Sheet',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     tableHTML_output('bs')
                                                 )),
                                      
                                          column(6,
                                                 tablerCard(
                                                     title = 'Income Statement',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     
                                                     width = 12,
                                                     tableHTML_output('is')
                                                 ))
                                  ),
                                  fluidRow(
                                     
                                          column(6,
                                                 tablerCard(
                                                     title = 'Cash Flow Statement',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     tableHTML_output('cf')
                                                 )),
                                          column(6,
                                      
                                                 tablerCard(
                                                     title = 'Production Tables',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     tableHTML_output('prod')
                                                 ))
                                  ),
                                  fluidRow(
                                     
                                          column(6,
                                                 tablerCard(
                                                     title = 'Derivatives',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     tableHTML_output('deriv')
                                                 )),
                                      
                                          column(6,
                                                 tablerCard(
                                                     title = 'Debt Tables',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     DT::dataTableOutput('debtLink'),
                                                     tableHTML_output('debt')
                                                 ))
                                      ),
                                  fluidRow(
                                      
                                          column(6,
                                                 tablerCard(
                                                     title = 'Firm Transportation',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     tableHTML_output('firm')
                                                 )),
                                      
                                          column(6,
                                                 tablerCard(
                                                     title = 'Reserves Information',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     DT::dataTableOutput('reportLink'),
                                                     tableHTML_output('reserves')
                                                 ))
                                  ),
                                  fluidRow(
                                      
                                          column(6,
                                                 tablerCard(
                                                     title = 'EBITDA tables',
                                                     closable = FALSE,
                                                     zoomable = FALSE,
                                                     collapsed = FALSE,
                                                     width = 12,
                                                     tableHTML_output('ebitda')
                                                 ))
                                  )
                                
                                  
                              
                              ),
                
                tablerTabItem(
                    tabName = "Options",
                    fluidRow(
                        column(
                            width = 6,
                            
                            tablerCard(
                                title = 'Option Type',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                awesomeRadio('options',
                                             '',
                                             choices = c('Swap', 'Two-way Collar', 'Three-way Collar'),
                                             selected = 'Swap',
                                             status = 'primary')
                            ),
                            tablerCard(
                                title = 'Product Type',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                awesomeRadio('product',
                                             '',
                                             choices = c('Gas', 'Oil'),
                                             selected = 'Gas',
                                             status = 'primary')
                            )
                        ),
                        
                        column(
                            width = 6,
                            tablerCard(
                                title = 'Derivative Parameters',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                numericInput('volumeHedged', 'Total Volume Hedged, MCF/MMBTU', 1500000, min = 0),
                                numericInput('oilPrice', 'Index Oil Price, $/BBL', value = 54, min = 0, max = 150),
                                numericInput('gasPrice', 'Index Gas Price, $/M', value = 2.5, min = 0, max = 10),
                                numericInput('swap1', 'Swap Price', value = 2.5, min = 0, max = 10),
                                numericInput('collar21', 'Short Call Price', value = 2.5, min = 0, max = 10),
                                numericInput('collar22', 'Long Put Price', value = 2.5, min = 0, max = 10),
                                numericInput('collar33', 'Short Put Price', value = 2.5, min = 0, max = 10),
                                textOutput('mtm')#,
                                #numericInput('collar34', 'Premium Price', value = 1.8, min = 0, max = 10)
                                
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            flowCard
                        )
                    )
                ),
                tablerTabItem(
                    tabName = "typeCurves",
                    fluidRow(
                        column(
                            width = 3,
                            
                            
                            tablerCard(
                                title = 'Selection Parameters',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                awesomeRadio('tcProduct',
                                             '',
                                             choices = c('Gas', 'Oil', 'Water'),
                                             selected = 'Oil',
                                             status = 'primary'),
                                awesomeRadio('abandonmentMethod',
                                             'Abandonment Calculation',
                                             choices = c('Oil Rate', 'Gas Rate', 'Time'),
                                             selected = 'Time',
                                             status = 'primary')
                            ),
                            tablerCard(
                                title = 'Decline Parameters',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                numericInput('qiOil', 'Oil IP Rate, bbl/d', 500, min = 0),
                                numericInput('bOil', 'Oil B Factor', value = 1, min = 0, max = 2),
                                numericInput('DiOil', 'Oil Initial Decline (Tangent Effective)', value = 0.9, min = 0.2, max = 0.99999999999),
                                numericInput('DfOil', 'Oil Terminal Decline (Tangent Effective)', value = 0.1, min = 0, max = 0.1999999999),
                                numericInput('curtailOil', 'Choke Period (Months)', value = 0, min = 0),
                                numericInput('qiGas', 'Gas IP Rate, mcf/d', 1000, min = 0),
                                numericInput('bGas', 'Gas B Factor', value = 1, min = 0, max = 2),
                                numericInput('DiGas', 'Gas Initial Decline (Tangent Effective)', value = 0.9, min = 0.2, max = 0.99999999999),
                                numericInput('DfGas', 'Gas Terminal Decline (Tangent Effective)', value = 0.1, min = 0, max = 0.1999999999),
                                numericInput('curtailGas', 'Choke Period (Months)', value = 0, min = 0),
                                numericInput('qiWater', 'Water IP Rate, mcf/d', 1500, min = 0),
                                numericInput('bWater', 'Water B Factor', value = 1, min = 0, max = 2),
                                numericInput('DiWater', 'Water Initial Decline (Tangent Effective)', value = 0.9, min = 0.2, max = 0.99999999999),
                                numericInput('DfWater', 'Water Terminal Decline (Tangent Effective)', value = 0.1, min = 0, max = 0.1999999999),
                                numericInput('curtailWater', 'Choke Period (Months)', value = 0, min = 0),
                                numericInput('qfOil', 'Abandonment Rate, bbl/d', value = 1, min = 0.1),
                                numericInput('qfGas', 'Abandonment Rate, mcf/d', value = 1, min = 0.1),
                                numericInput('wellLife', 'Total Well Life (Years)', value = 30, min = 1, max = 50),
                            )
                        ),
                        column(
                            width = 6,
                            hidden(div(
                                id = 'hidePrice',
                                tablerCard(
                                    title = 'Strip Pricing',
                                    closable = FALSE,
                                    zoomable = TRUE,
                                    width = 12,
                                    echarts4rOutput('stripPrice')
                                )
                            )),
                            tablerCard(
                                title = 'Type Curve Plot',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                awesomeRadio('logTC', '', choices = c('Log', 'Cartesian'), selected = 'Cartesian'),
                                echarts4rOutput('tcPlot'),
                                textOutput('eurData'),
                                textOutput('economics'),
                                textOutput('economicIRR')
                            ),
                            bsButton('saveTC', 'Save TC to Development Plan', style = 'primary')
                        ),
                        
                        column(
                            width = 3,
                            tablerCard(
                                title = 'Pricing',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                awesomeRadio('priceType',
                                             'Price Methodology',
                                             choices = c('Current Strip', 'Flat'),
                                             selected = 'Current Strip',
                                             status = 'primary'),
                                numericInput('wti', 'Oil Price in WTI ($/BBL)', 60, min = 0),
                                numericInput('hh', 'Gas Price in Henry Hub ($/MCF)', 3, min = 0)
                                
                                
                            ),
                            tablerCard(
                                title = 'Economic Factors',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                awesomeRadio('econAban',
                                             'Abandon at Economic Limit?',
                                             choices = c('Yes', 'No'),
                                             selected = 'Yes',
                                             status = 'primary'),
                                numericInput('wi', 'WI, %', 100, min = 0),
                                numericInput('nri', 'NRI, % (to the 100%)', 75, min = 0),
                                awesomeRadio('reversion',
                                             'Reversion Type',
                                             choices = c('None', 'IRR', 'Penalty'),
                                             selected = 'None',
                                             status = 'primary'),
                                numericInput('reversionIRR', 'Reversion IRR %', value = 10, min = 0, max = 50),
                                numericInput('penalty', 'Non-consent Penalty %', value = 100, min = 0, max = 300),
                                numericInput('wi2', 'Post Reversion WI %', value = 90, min = 0, max = 100),
                                numericInput('nri2', 'Post Reversion NRI % (to the 100%)', value = 75, min = 0, max = 100)
                                ),
                            tablerCard(
                                title = 'Capex',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                numericInput('spudToProd', 'Spud to First Production (months)', 3, min = 1),
                                numericInput('drillCost', 'Drill Cost, $', 3000000, min = 1),
                                numericInput('completeCost', 'Complete & Equip Cost, $', 4500000, min = 1),
                                numericInput('pna', 'Plug & Abandon, $', 40000, min = 1)
                                ),
                            tablerCard(
                                title = 'Revenue Adjustments',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                numericInput('discOil', 'Oil Basis Diff, $/BBL', 2, min = 0),
                                numericInput('discGas', 'Gas Basis Diff, $/MCF', 2, min = 0),
                                numericInput('discNGL', 'NGL %WTI', 20, min = 0),
                                numericInput('btu', 'BTU Uplift', 1, min = 0, max = 3),
                                numericInput('shrink', 'Gas Shrink (fraction retained)', 0.75, min = 0, max = 1),
                                numericInput('nglYield', 'NGL Yield, bbl/MMCF', 90, min = 0, max = 200)
                                ),
                            tablerCard(
                                title = 'Taxes',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                numericInput('stxOil', 'Oil Severance % Revenue', 4.6, min = 0, max = 20),
                                numericInput('stxGas', 'Gas/NGL Severance % Revenue', 7.5, min = 0, max = 20),
                                numericInput('oilSTX', 'Oil Severance/BBL', 1, min = 0, max = 20),
                                numericInput('gasSTX', 'Gas Severance/MCF', .1, min = 0, max = 20),
                                numericInput('atx', 'Ad Val % Revenue', 2.5, min = 0, max = 20)
                                ),
                            tablerCard(
                                title = 'Operating Expenses',
                                closable = FALSE,
                                zoomable = TRUE,
                                width = 12,
                                numericInput('yr1Fixed', 'Fixed Opex/Month, Year 1', 10000, min = 0),
                                numericInput('yr2Fixed', 'Fixed Opex/Month, Year 2', 10000, min = 0),
                                numericInput('finalFixed', 'Fixed Opex/Month, Remaining', 10000, min = 0),
                                numericInput('varOilExp', 'Variable Expense/BBL Oil', 2, min = 0),
                                numericInput('varGasExp', 'Variable Expense/MCF Gas', 0.25, min = 0),
                                numericInput('varWaterExp', 'Variable Expense/BBL Water', 2, min = 0),
                                numericInput('wrkExp', 'Workover Expense/Month', 750, min = 0)
                                
                                
                            )
                        )
                    )
                ),
                tablerTabItem(
                    tabName = 'devPlan',
                    fluidRow(
                        column(3,
                               awesomeRadio('tcLoads', 'Available Type Curves', choices = '', inline = TRUE, checkbox = TRUE, status = "primary"),
                               textOutput('tcInfo'),
                               bsButton('removeTC', 'Remove Selected TC', style = 'danger')
                               ),
                        column(3,
                               tablerCard(
                                   title = 'Price Assumption',
                                   closable = FALSE,
                                   zoomable = TRUE,
                                   width = 12,
                                   awesomeRadio('priceSelection', 'Price Scenario', 
                                                choices = c('Current Strip', 'Flat'), 
                                                selected = 'Current Strip', inline = TRUE, checkbox =TRUE, 
                                                status = 'primary'),
                                   numericInput('wtiDev', 'WTI, $/BBL', value = 60, min = 1),
                                   numericInput('hhDev', 'HH, $/MCF', value = 2, min = 0.1)
                                   )
                        ),
                        column(6,
                               tablerCard(
                                   title = 'Development Plan',
                                   closable = FALSE,
                                   zoomable = TRUE,
                                   width = 12,
                                   fluidRow(
                                       column(6,
                                        dateInput('startDate', label = 'Forecast Start', value = today())),
                                       column(6,
                                              numericInput('wellCount', 'Total Wells', value = 0, min = 0))
                                   ),
                                   fluidRow(
                                       column(6,
                                        numericInput('yr1Dev', 'Year 1 Wells', value = 0, min = 0),
                                        textOutput('rem1')),
                                       
                                       column(6,
                                        numericInput('yr2Dev', 'Year 2 Wells', value = 0, min = 0),
                                        textOutput('rem2'))
                                       ),
                                   fluidRow(
                                       column(6,
                                        numericInput('yr3Dev', 'Year 3 Wells', value = 0, min = 0),
                                        textOutput('rem3')),
                                       column(6,
                                        numericInput('yr4Dev', 'Year 4 Wells', value = 0, min = 0),
                                        textOutput('rem4'))
                                       ),
                                   fluidRow(
                                       column(6,
                                        numericInput('yr5Dev', 'Year 5 Wells', value = 0, min = 0),
                                        textOutput('rem5')),
                                       column(6,
                                        numericInput('yr6Dev', 'Year 6 Wells', value = 0, min = 0),
                                        textOutput('rem6'))
                                       ),
                                   fluidRow(
                                       column(6,
                                        numericInput('yr7Dev', 'Year 7 Wells', value = 0, min = 0),
                                        textOutput('rem7')),
                                       column(6,
                                        numericInput('yr8Dev', 'Year 8 Wells', value = 0, min = 0),
                                        textOutput('rem8'))
                                       ),
                                   fluidRow(
                                       column(6,
                                        numericInput('yr9Dev', 'Year 9 Wells', value = 0, min = 0),
                                        textOutput('rem9')),
                                       column(6,
                                        numericInput('yr10Dev', 'Year 10+ (Wells/Year)', value = 0, min = 0),
                                        textOutput('rem10')))
                               )
                        )
                    )
                ),
                tablerTabItem(
                    tabName = "About",
                    

                    profileCard
                )
            )
        ),
        footer = tablerDashFooter(
            # tablerIcon(name = "maestro", lib = "payment"),
            # tablerIcon(name = "mastercard", lib = "payment"),
            
            copyrights = "@enFinExplorer, 2020"
        )
    ),
    server = function(input, output,session) {
        
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
        
        output$tweets <- DT::renderDataTable({
            q <- paste0('$',input$operatorSelect)
            #print(q)
            rt <- search_tweets(q, geocode = lookup_coords('usa'), n = 18000, token = token)
            rt <- as.data.frame(rt)
            
            rt <- rt %>% arrange(desc(retweet_count))
            rt <- rt %>% filter(grepl('Twitter', source))
            rt <- rt %>% filter(is_retweet == FALSE)
            rt$hashCount <- str_count(rt$text, '\\$')
            rt <- rt %>% filter(hashCount <= 3)
            #rt <- rt %>% filter(favorite_count > 0)
            #rt <- rt %>% filter(retweet_count > 0)
            rt <- rt %>% filter(screen_name != 'shortvolumes') %>% 
                filter(screen_name != 'Tickeron') %>%
                filter(screen_name != 'OptionsPastor') %>%
                filter(screen_name != 'InfinitusCap')%>%
                filter(screen_name != 'VegasTours')%>%
                filter(screen_name != 'stockspastor')%>%
                filter(screen_name != 'CarmichaelLeval')%>%
                filter(screen_name != 'GingerRoelofs')%>%
                filter(screen_name != 'AlertTrade')%>%
                filter(screen_name != 'Gambiste1') %>% 
                filter(screen_name != 'UPBOptionMil')%>%
                filter(screen_name != 'r_wallstreet_')%>%
                filter(screen_name != 'mscullion')%>%
                filter(screen_name != 'oilbot123')%>%
                filter(screen_name != '15minofPham')%>%
                filter(screen_name != 'TeresaTrades')%>%
                filter(screen_name != 'tastytraderMike')%>%
                filter(screen_name != 'winthrop317')%>%
                filter(screen_name != 'BearBullTraders') %>%
                filter(screen_name != 'TheBurdetteLawF') %>%
                filter(screen_name != 'apbeaton') %>%
                filter(screen_name != 'MDLive4') %>%
                filter(screen_name != 'msectors') %>%
                filter(screen_name != 'CarlosmBBT') %>%
                filter(screen_name != 'stickycactusink')
            
            if(nrow(rt) == 0){
                NULL
            } else {
                rt <- rt %>% filter(!duplicated(text))
                
                rt <- rt[,c('profile_image_url', 'screen_name', 'text', 'created_at')]
                
                #rt$profile_image_url <- img(src=rt$profile_image_url)
                rt <- rt %>% mutate(image = paste0('<img src ="', profile_image_url, '" height = "52"></img>'))
                rt <- rt[,c('image', 'screen_name', 'text', 'created_at')]
                rt$screen_name <- paste0("<a target='_blank' href='",paste0('https://twitter.com/', rt$screen_name),"'>",rt$screen_name,"</a>")
                rt$created_at <- as.Date(rt$created_at)
                #rt$screen_name <- a(rt$screen_name, href=paste0('https://twitter.com/', rt$screen_name))
                #print(head(rt))
                names(rt) <- c('', '', '', '')
                
                DT::datatable(rt, rownames = FALSE, escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
                              options = list(pageLength = 10,
                                             lengthMenu = c(5, 10, 15)))
            }
            
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
                declineValues()$curtailOil/12.0, seq(0, 50*12-1/12, by = (1/12)))/12
            gas <- curtailed.q(arps.decline(
                declineValues()$qiGas*365, as.nominal(declineValues()$DiGas), declineValues()$bGas, as.nominal(declineValues()$DfGas)),
                declineValues()$curtailGas/12.0, seq(0, 50*12-1/12, by = (1/12)))/12
            
            water <- curtailed.q(arps.decline(
                declineValues()$qiWater*365, as.nominal(declineValues()$DiWater), declineValues()$bWater, as.nominal(declineValues()$DfWater)),
                declineValues()$curtailWater/12.0, seq(0, 50*12-1/12, by = (1/12)))/12
            
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
                crude = 'https://quotes.wsj.com/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
                webpage <- read_html(crude)
                #tbls <- html_nodes(webpage, 'table')
                
                tbls_ls <- webpage %>%
                    html_nodes('table') %>%
                    .[2] %>%
                    html_table(fill = TRUE)
                
                wti <- tbls_ls[[1]]
                
                crude = 'https://quotes.wsj.com/futures/NATURAL%20GAS/contracts'
                webpage <- read_html(crude)
                #tbls <- html_nodes(webpage, 'table')
                
                tbls_ls <- webpage %>%
                    html_nodes('table') %>%
                    .[2] %>%
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
                              'wellLife', 'wti', 'hh'),
                
                Value = c(input$qiOil, input$bOil, input$DiOil, input$DfOil, input$curtailOil, input$qfOil,
                          input$qiGas, input$bGas, input$DiGas, input$DfGas, input$curtailGas, input$qfGas,
                          input$qiWater, input$bWater, input$DiWater, input$DfWater, input$curtailWater,
                          input$wellLife, input$wti, input$hh),
                stringsAsFactors = FALSE) %>% spread(Component, Value)
            
        })
        
        expenseValues <- reactive({
            data.frame(
                Component = c('nri', 'spudToProd', 'drillCost', 'completeCost', 'pna', 'discOil', 
                              'discGas', 'discNGL', 'btu', 'shrink', 'nglYield', 'stxOil',
                              'stxGas', 'oilSTX', 'gasSTX', 'atx', 'yr1Fixed',
                              'yr2Fixed', 'finalFixed', 'varOilExp', 'varGasExp', 'varWaterExp', 'wrkExp',
                              'penalty', 'reversionIRR', 'nri2', 'wi', 'wi2'),
                
                Value = c(input$nri, input$spudToProd, input$drillCost, input$completeCost, input$pna, input$discOil,
                          input$discGas, input$discNGL, input$btu, input$shrink, input$nglYield, input$stxOil,
                          input$stxGas, input$oilSTX, input$gasSTX, input$atx, input$yr1Fixed,
                          input$yr2Fixed, input$finalFixed, input$varOilExp, input$varGasExp, input$varWaterExp, input$wrkExp,
                          input$penalty, input$reversionIRR, input$nri2, input$wi, input$wi2),
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
            df$Oil <- df$Oil*expenseValues()$wi/100
            df$Gas <- df$Gas*expenseValues()$wi/100
            df$Water <- df$Water*expenseValues()$wi/100
            df$Sales_Gas <- df$Gas*expenseValues()$shrink
            df$NGL <- df$Gas*expenseValues()$nglYield/100
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
                df$Oil*df$nri/100*expenseValues()$oilSTX + df$Gas*expenseValues()$nri/100*expenseValues()$gasSTX +
                df$rev*df$nri/100*expenseValues()$atx/100
            
            df$expense <- df$Oil*expenseValues()$varOilExp + df$Gas*expenseValues()$varGasExp + df$Water*expenseValues()$varWaterExp +
                expenseValues()$wrkExp*expenseValues()$wi/100 + expenseValues()$yr1Fixed*expenseValues()$wi/100
            
            df$expense[13:24] <- df$expense[13:24] - expenseValues()$yr1Fixed*expenseValues()$wi/100 + expenseValues()$yr2Fixed*expenseValues()$wi/100
            df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - expenseValues()$yr1Fixed*expenseValues()$wi/100 + expenseValues()$yr2Fixed*expenseValues()$wi/100
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
                
                df$expense <- df$Oil*expenseValues()$varOilExp + df$Gas*expenseValues()$varGasExp + df$Water*expenseValues()$varWaterExp +
                    expenseValues()$wrkExp*df$wi/100 + expenseValues()$yr1Fixed*df$wi/100
                
                df$expense[13:24] <- df$expense[13:24] - expenseValues()$yr1Fixed*df$wi[13:24]/100 + expenseValues()$yr2Fixed*df$wi[13:24]/100
                df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - expenseValues()$yr1Fixed*df$wi[25:nrow(df)]/100 + expenseValues()$yr2Fixed*df$wi[25:nrow(df)]/100
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
                    
                    df$expense <- df$Oil*expenseValues()$varOilExp + df$Gas*expenseValues()$varGasExp + df$Water*expenseValues()$varWaterExp +
                        expenseValues()$wrkExp*df$wi/100 + expenseValues()$yr1Fixed*df$wi/100
                    
                    df$expense[13:24] <- df$expense[13:24] - expenseValues()$yr1Fixed*df$wi[13:24]/100 + expenseValues()$yr2Fixed*df$wi[13:24]/100
                    df$expense[25:nrow(df)] <- df$expense[25:nrow(df)] - expenseValues()$yr1Fixed*df$wi[25:nrow(df)]/100 + expenseValues()$yr2Fixed*df$wi[25:nrow(df)]/100
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
            df$startDate <- min(values$price$DATE)
            df$wellCount <- 0

            if(is.null(values$devPlan)){
                df$id <- 'tc1'
                values$devPlan <- df
            } else {
                id <- paste0('tc', (length(unique(values$devPlan$id))+1))
                df$id <- id
                values$devPlan <- rbind(values$devPlan, df)
            }
           
            updateAwesomeRadio(session, 'tcLoads', choices = unique(values$devPlan$id), selected = 'tc1', status = 'primary')
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
        })
        
        output$tcInfo <- renderText({
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                df <- values$devPlan %>% filter(id == input$tcLoads)
                paste0('Oil EUR: ', df$oilEUR, ' MBO, Gas EUR: ', df$gasEUR, ' MMCF, Water EUR: ', df$waterEUR, ' MBW, Capex: ',(df$completeCost + df$drillCost)/1000, ' M$')
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
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
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
                
                
            }
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
                df$wellCount <- input$wellCount
                values$devPlan <- rbind(df, df1)
            }
        })
        
        
        
        
        observeEvent(input$yr1Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr1Dev <- input$yr1Dev
                values$devPlan <- rbind(df, df1)
            
                
            }
        })
        
        observeEvent(input$yr2Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr2Dev <- input$yr2Dev
                values$devPlan <- rbind(df, df1)
  
            }
        })
        
        observeEvent(input$yr3Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr3Dev <- input$yr3Dev
                values$devPlan <- rbind(df, df1)

            }
        })
        
        observeEvent(input$yr4Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr4Dev <- input$yr4Dev
                values$devPlan <- rbind(df, df1)

            }
        })
        
        observeEvent(input$yr5Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr5Dev <- input$yr5Dev
                values$devPlan <- rbind(df, df1)

            }
        })
        
        observeEvent(input$yr6Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr6Dev <- input$yr6Dev
                values$devPlan <- rbind(df, df1)

            }
        })
        
        observeEvent(input$yr7Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr7Dev <- input$yr7Dev
                values$devPlan <- rbind(df, df1)
      
            }
        })
        
        observeEvent(input$yr8Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr8Dev <- input$yr8Dev
                values$devPlan <- rbind(df, df1)
                
            }
        })
        
        observeEvent(input$yr9Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr9Dev <- input$yr9Dev
                values$devPlan <- rbind(df, df1)
                
            }
        })
        
        observeEvent(input$yr10Dev, {
            if(nrow(values$devPlan) == 0 || is.null(values$devPlan)){
                NULL
            } else {
                id1 <- input$tcLoads
                df <- values$devPlan %>% filter(id == id1)
                df1 <- values$devPlan %>% filter(id != id1)
                df$yr10Dev <- input$yr10Dev
                values$devPlan <- rbind(df, df1)
            }
        })
        
        output$rem1 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev)
        })
        
        output$rem2 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev - input$yr2Dev)
        })
        
        output$rem3 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev -
                       input$yr2Dev - input$yr3Dev)
        })
        
        output$rem4 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev -
                       input$yr2Dev - input$yr3Dev - input$yr4Dev)
        })
        
        output$rem5 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev-
                       input$yr2Dev - input$yr3Dev - input$yr4Dev -
                       input$yr5Dev)
        })
        
        output$rem6 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev-
                       input$yr2Dev - input$yr3Dev - input$yr4Dev -
                       input$yr5Dev - input$yr6Dev)
        })
        
        output$rem7 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev-
                       input$yr2Dev - input$yr3Dev - input$yr4Dev -
                       input$yr5Dev - input$yr6Dev - input$yr7Dev)
        })
        
        output$rem8 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev-
                       input$yr2Dev - input$yr3Dev - input$yr4Dev -
                       input$yr5Dev - input$yr6Dev - input$yr7Dev -
                       input$yr8Dev)
        })
        
        output$rem9 <- renderText({
            paste0('Remaining Wells: ', input$wellCount - input$yr1Dev-
                       input$yr2Dev - input$yr3Dev - input$yr4Dev -
                       input$yr5Dev - input$yr6Dev - input$yr7Dev -
                       input$yr8Dev - input$yr9Dev)
        })
        
        output$rem10 <- renderText({
            if(input$yr10Dev == 0){
                'N/A'
            } else {
                wells <- input$wellCount - input$yr1Dev-
                           input$yr2Dev - input$yr3Dev - input$yr4Dev -
                           input$yr5Dev - input$yr6Dev - input$yr7Dev -
                           input$yr8Dev - input$yr9Dev
                
                wells <- as.integer(wells/input$yr10Dev*12)
                paste0('Remaining Months: ', wells)
            }
        })
        
 
        
    }
)