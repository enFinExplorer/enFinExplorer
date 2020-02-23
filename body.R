library(shiny)
library(tablerDash)
library(shinyEffects)
library(echarts4r)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
#library(bs4Dash)
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
#library(sp)
#library(maptools)
#library(maps)
library(tools)
#library(geosphere)
#library(tigris)
#library(openintro)
library(RColorBrewer)
#library(akima)
library(rgdal)
#library(kernlab)
#library(caret)
#library(leaflet)
#library(leaflet.extras)
library(shinyalert)
library(DT)
#library(excelR)
#library(echarts4r.assets)

options(stringsAsFactors = FALSE)
options(scipen = 999)
opList1 <- c('APA',  'AR', 'AREX', 'ARTEX', 'BCEI', 'BP', 'CDEV', 'CHAP', 'CHK', 'CLR',
             'CNX', 'COG', 'COP', 'CPE',  'CRK',  'CXO',  'DVN', 'ECA','EOG',
             'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP', 'GUI', 'HES', 'HPR',  'LLEX', 'LPI', 'MGY', 'MR',
             'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP', 'RDS-A',
             'RRC', 'SBOW', 'SM', 'SWN',  'WLL', 'WPX', 'XEC', 'XOG', 'XOM')  

opLink <- data.frame(ticker = opList1, operator = c('Apache', 'Antero Resources',
                                                    'Approach Resources',  'Artex Oil', 'Bonanza Creek Energy', 'BP','Centennial Resource Development',
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

css <- HTML(
  "#pdpTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #pdpTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }
    #pdpQtrs > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #pdpQtrs > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }
   #autoTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #autoTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }"
)

css1 <- HTML(
  "#pudFcst > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #pudFcst > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }
    #pudYrFcst > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #pudYrFcst > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }"
)

# wellData <- readRDS("./data/wellData2.rds") 
# subBasinList <- unique(wellData$subBasin)



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

body <- tablerDashBody(
  
  
  
  chooseSliderSkin("Nice"),
  useShinyjs(),
  tablerTabItems(
    
    
    tablerTabItem('home',
                  fluidRow(
                    column(12,
                           
                           tablerCard(
                             title = 'Strip Pricing',
                             closable = FALSE,
                             zoomable = TRUE,
                             width = 12,
                             echarts4rOutput('stripPrice')
                           )
                    )
                  )
    ),
    tablerTabItem('comp',
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
                    column(12,
                           tablerCard(
                             title =icon('newspaper'),
                             closable = FALSE,
                             zoomable = FALSE,
                             collapsed = FALSE,
                             width = 12,
                             
                             DT::dataTableOutput('news')
                             
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
    # tablerTabItem(
    #     tabName = 'twitter',
    #     fluidRow(
    #         column(width = 12,
    #          uiOutput('embedded_user_tweets')
    #         )
    #     )
    # ),
    
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
          textInput('tcName', 'Type Curve ID', placeholder = 'tc1'),
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
            numericInput('varBOEExp', 'Variable Expense/BBL Oil + NGL', 2, min = 0),
            numericInput('wrkExp', 'Workover Expense/Month', 750, min = 0)
            
            
          )
        )
      )
    ),
    
    tablerTabItem(
      tabName = "pdp",
      tags$head(tags$style(css)),
      fluidRow(
        column(3,
               tablerCard(
                 title = 'PDP Profile Type',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 awesomeRadio('pdpType',
                              'PDP Input Type',
                              choices = c('Full Economics', 'Production Only'),
                              selected = 'Production Only',
                              status = 'primary')
                 
                 
               ),
               tablerCard(
                 title = 'Basic Assumptions',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 dateInput('effDate', 'Effective Date'),
                 numericInput('wiPDP', 'WI %', value =100, min = 1, max = 100),
                 numericInput('nriPDP', 'NRI % (to the 100%)', value =75, min = 1, max = 100),
                 numericInput('pnaPDP', 'P&A Per Well, $', value = 20000, min = 1),
                 numericInput('pdpDisc', 'PDP Discount Rate, %', value = 10, min =0, max = 30),
                 awesomeRadio('econLimitPDP',
                              'Economic Limit Cutoff?',
                              choices = c('Yes', 'No'),
                              selected = 'Yes',
                              status = 'primary')
               )
        ),
        column(6,
               tablerCard(
                 title = 'Data Load',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 
                 textOutput('pdpInstruct'),
                 #tableHTML_output('rowInstruct'),
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 plotlyOutput('contents')
               ),
               tablerCard(
                 title = 'PDP Plot',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 plotlyOutput('pdpPlot'),
                 textOutput('pdpPV')
               )
        ),
        column(3,
               
               tablerCard(
                 title = 'Revenue Assumptions',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 awesomeRadio('pdpPrice',
                              'PDP Price Case',
                              choices = c('Current Strip', 'Flat'),
                              selected = 'Current Strip',
                              status = 'primary'),
                 
                 numericInput('wtiPDP', 'WTI, $/BBL', value =60, min = 1),
                 numericInput('hhPDP', 'HH, $/BBL', value = 2, min = 0.1),
                 numericInput('oilDiffPDP', 'Oil Basis Diff, $/BBL', value = 2, min = 0),
                 numericInput('hhDiffPDP', 'Gas Basis Diff, $/MCF', value = 0.5, min = 0),
                 numericInput('nglDiffPDP', 'NGL Price % WTI', value = 20, min = 0, max = 100),
                 numericInput('shrinkPDP', 'Shrink (fraction retained)', value = 0.75, min = 0, max = 1),
                 numericInput('nglYieldPDP', 'NGL Yield, BBL/MMCF', value = 75, min = 0, max = 200),
                 numericInput('btuPDP', 'Gas BTU', value = 1, min = 0, max = 3)
               ),
               tablerCard(
                 title = 'Expense Assumptions',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 numericInput('wellsPDP', 'Wells', value = 100, min = 1),
                 numericInput('fixedPDP', 'Fixed Opex/Month/Well', value =1000, min = 1),
                 numericInput('varGasPDP', 'Variable Gas Opex, $/MCF', value = 0.25, min = 0),
                 numericInput('varOilPDP', 'Variable Oil Opex, $/BBL', value = 2, min = 0),
                 numericInput('varWaterPDP', 'Variable Water Opex, $/BBL', value = 0.5, min = 0),
                 numericInput('varBOEPDP', 'Variable Liquids (Oil+NGL), $/BBL', value = 1, min = 0)
               ),
               tablerCard(
                 title = 'Tax Assumptions',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 numericInput('stxOilPDP', 'Oil Severance % Revenue', 4.6, min = 0, max = 20),
                 numericInput('stxGasPDP', 'Gas/NGL Severance % Revenue', 7.5, min = 0, max = 20),
                 numericInput('oilSTXPDP', 'Oil Severance/BBL', 1, min = 0, max = 20),
                 numericInput('gasSTXPDP', 'Gas Severance/MCF', .1, min = 0, max = 20),
                 numericInput('atxPDP', 'Ad Val % Revenue', 2.5, min = 0, max = 20)
               )
        )
      ),
      fluidRow(
        column(12,
               
               tablerCard(
                 title = 'Quarterly Cash Flow Summary',
                 closable = FALSE,
                 zoomable = FALSE,
                 width = 12,
                 
                 DT::dataTableOutput('pdpQtrs')
                 
               ))),
      fluidRow(
        column(12,
               
               tablerCard(
                 title = 'PDP Cash Flow Summary',
                 closable = FALSE,
                 zoomable = FALSE,
                 width = 12,
                 
                 DT::dataTableOutput('pdpTable')
                 
               )))
      
      
    ),
    
    tablerTabItem(
      tabName = 'devPlan',
      tags$head(tags$style(css1)),
      fluidRow(
        column(3,
               tablerCard(
                 title = 'Type Curve Information',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 awesomeRadio('tcLoads', 'Available Type Curves', choices = '', inline = TRUE, checkbox = TRUE, status = "primary"),
                 textOutput('tcInfo'),
                 bsButton('removeTC', 'Remove Selected TC', style = 'danger')
               )
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
                          numericInput('wellCount', 'Gross Wells', value = 10, min = 0))
                 ),
                 fluidRow(
                   column(6,
                          numericInput('yr1Dev', 'Year 1 Wells', value = 1, min = 0),
                          textOutput('rem1')),
                   
                   column(6,
                          numericInput('yr2Dev', 'Year 2 Wells', value = 1, min = 0),
                          textOutput('rem2'))
                 ),
                 fluidRow(
                   column(6,
                          numericInput('yr3Dev', 'Year 3 Wells', value = 1, min = 0),
                          textOutput('rem3')),
                   column(6,
                          numericInput('yr4Dev', 'Year 4 Wells', value = 1, min = 0),
                          textOutput('rem4'))
                 ),
                 fluidRow(
                   column(6,
                          numericInput('yr5Dev', 'Year 5 Wells', value = 1, min = 0),
                          textOutput('rem5')),
                   column(6,
                          numericInput('yr6Dev', 'Year 6 Wells', value = 1, min = 0),
                          textOutput('rem6'))
                 ),
                 fluidRow(
                   column(6,
                          numericInput('yr7Dev', 'Year 7 Wells', value = 1, min = 0),
                          textOutput('rem7')),
                   column(6,
                          numericInput('yr8Dev', 'Year 8 Wells', value = 1, min = 0),
                          textOutput('rem8'))
                 ),
                 fluidRow(
                   column(6,
                          numericInput('yr9Dev', 'Year 9 Wells', value = 1, min = 0),
                          textOutput('rem9')),
                   column(6,
                          numericInput('yr10Dev', 'Year 10+ (Wells/Year)', value = 1, min = 0),
                          textOutput('rem10'))),
                 bsButton('addFcst', 'Add to Forecast', style = 'primary', size='small')
               )
        )
      ),
      fluidRow(
        column(6,
               tablerCard(
                 title = 'Annual Cash Flow Summary',
                 closable = FALSE,
                 zoomable = FALSE,
                 width = 12,
                 DT::dataTableOutput('pudYrFcst')),
               tablerCard(
                 title = 'Cash Flow Summary',
                 closable = FALSE,
                 zoomable = FALSE,
                 width = 12,
                 DT::dataTableOutput('pudFcst'))
        ),
        column(6,
               tablerCard(
                 title = 'Cash Flow Graph',
                 closable = FALSE,
                 zoomable = TRUE,
                 width = 12,
                 selectizeInput('graphSelect', 'Select Metric', choices = c('Oil', 'Gas', 'Sales_Gas',
                                                                            'NGL', 'Water', 'netOil', 'netGas', 'netBOE', 'netMCFE',
                                                                            'netNGL', 'revenue', 'tax', 'expense', 'nocf',
                                                                            'capex'), selected = 'netBOE'),
                 
                 plotlyOutput('cfGraph')
               ))
      )
    ),
    tablerTabItem('finCalc',
                  tags$head(tags$style(css)),
                   fluidRow(
                    
                            
                            tablerCard(
                              title = 'Upload Well Data',
                              closable = FALSE,
                              zoomable = TRUE,
                              width = 9,
                              h4('Please Upload Monthly Well Data as:'),
                              h6('Column 1:  API Number or Well Identifier'),
                              h6('Column 2:  Date in either MM/DD/YYYY or MM-DD-YYYY'),
                              h6('Column 3:  Volume in BBLS or MCF'),
                              br(),
                              h5('To really work properly, this should be several wells (50+ or so),
                                 and several of them should be 5+ years old.  If not please set a narrow range
                                 for terminal decline, though it will not be as predictive'),
                              #tableHTML_output('rowInstruct'),
                              fileInput("file2", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              textOutput('wellsInfo')
                            ),
                     
                   
                    
                           tablerCard(
                             title = 'Decline Calculations',
                             closable = FALSE,
                             zoomable = TRUE,
                             width = 3,
                             #numericInput('DiTC', 'Initial Decline', value = 0.9, min = 0.40, max = 0.99999),
                             numericInput('DfTC', 'Default Terminal Decline', value = 0.1, min = 0.02, max = 0.3),
                             
                             numericInput('bTC', 'Default B-Factor', value = 1, min = 0.01, max = 2.5),
                             numericInput('wellLifeTC', 'Well Life (years)', value = 30, min = 10, max = 50),
                             bsButton('calcDecline', 'Calculate', style = 'primary', size = 'small')
                             
                           )
                           ),
                  
                  fluidRow(
                    tablerCard(
                      title = 'B-Factor Values',
                      closable = FALSE,
                      zoomable = TRUE,
                      width = 6,
                      echarts4rOutput('bPlot')
                      
                    ),
                    tablerCard(
                      title = 'Terminal Decline Values',
                      closable = FALSE,
                      zoomable = TRUE,
                      width = 6,
                      echarts4rOutput('DfPlot')
                    )
                            
                   ),
                  fluidRow(
                    tablerCard(
                      title = 'Minimum B-Factor',
                      closable = FALSE,
                      zoomable = FALSE,
                      width = 3,
                           
                           sliderInput('lowB', '', value = 0.01, min = 0.01, max = 1)
                      ),
                    tablerCard(
                      title = 'Maximum B-Factor',
                      closable = FALSE,
                      zoomable = FALSE,
                      width = 3,
                           sliderInput('highB', '', value = 2.5, min = 1.001, max = 2.5)),
                    tablerCard(
                      title = 'Minimum Terminal Decline',
                      closable = FALSE,
                      zoomable = FALSE,
                      width = 3,
                           
                           sliderInput('lowDf', '', value = (0.02), min = (0.02), max = (0.1))),
                    tablerCard(
                      title = 'Maximum Terminal Decline',
                      closable = FALSE,
                      zoomable = FALSE,
                      width = 3,
                           sliderInput('highDf', '', value = (0.25), min = (0.101), max = (0.25)))
                  ),
                  bsButton('buildPDP', 'AUTOCAST', style = 'primary'),
                  fluidRow(
                    tablerCard(
                      title = 'Stacked Forecast',
                      closable = FALSE,
                      zoomable = FALSE,
                      width = 12,
                      plotlyOutput('autoGraph')
                    ),
                    tablerCard(
                      title = 'Well Data',
                      closable = FALSE,
                      zoomable = FALSE,
                      width = 12,
                      DT::dataTableOutput('autoTable')
                    )
                  )
     
    ),
    
    tablerTabItem(
      tabName = "About",
      
      
      profileCard
    )
  )
)