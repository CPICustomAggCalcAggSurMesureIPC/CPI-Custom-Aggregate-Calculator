
## The CPI Custom Aggregate Calculator 
- Is an interactive app which allows users of Statistics Canada data to select published CPI geographies and products and calculate Custom CPIs as aggregates of the selected series or as All-items excluding the selections. 
- Results are displayed in graphs and tables as percentage changes, index levels, or contributions to All-items percentage change.

## Download and run the app in R
You can download the R code from GitHub and run it on your device using the following R code:
- shiny::runGitHub("CPI-Custom-Aggregate-Calculator", "CPICustomAggCalcAggSurMesureIPC")
- If this method fails, try following the instructions at https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github. 
    - In your browser, enter the URL to the GitHub repository: https://github.com/CPICustomAggCalcAggSurMesureIPC/CPI-Custom-Aggregate-Calculator
	- In the Code button dropdown, select Download ZIP
	- Navigate to the downloaded .zip file, then copy the directory elsewhere on your device
	- Open the directory you just copied containing the English version
	- Run the file app.r
- Prerequisites:
    - R installed on your device
	- A display at least 1140 pixels wide
	- Approximately 500MB of RAM


## Using the CPI Custom Aggregate Calculator
- English instructions: https://github.com/CPICustomAggCalcAggSurMesureIPC/CPI-Custom-Aggregate-Calculator/blob/main/man/How_to_Use_the_CPI_Custom_Aggregate_Calculator.docx

## Development:
- Gerry O'Donnell, Principal Consumer Prices Analyst, Consumer Prices Division, Statistics Canada, gerry.odonnell@statcan.gc.ca
- Thanks also to 
    - Taylor Mitchell and team for help with dissemination
    - Zack Lansfield, Vishal Sood for help with packaging and accessibility
	- Clément Yélou for help with formulae and translation
	- Chris Bazos for help with testing
	- Zack Glazier, Lance Taylor for code reviews
	- many others for input on design 

## How it works:
- Downloading the code in R runs the file \\app.R, which ...
    - If needed, installs packages and loads them in the session
	- Sets the language to English
	- Contains several internal-only functions
        - fPeriodSeq190001 converts a date as string ("yyyy-mm-dd") to a month in sequence starting 1900-01
        - fRefDate converts a month in sequence starting 1900-01 to date as string ("yyyy-mm-dd")
        - fRoundHAFZ uses fuzzy half-away-from-zero rounding at specified number of digits
        - fGetEnFrText retrieves English or French text for UI object
        - fGetVarNameFromEnFrText gets UI object name from English or French text
        - fIndexWeightChgCont accepts the selected series, base start and end periods, and CODR indexes and weights, then calculates and returns custom aggregate values and a status message  
        - fGetDisplaySeries accepts component series, returns remaining available series
        - fPlotTimeSeries accepts data for available series, returns plotly graphic
        - fMessage writes message to console by code block
    - Reads metadata in data-raw\Data_for_R_Shiny.xlsx needed to initialize app with data specifying ...
        - Effective dates for CPI baskets
        - CODR table 18100004 and 18100007 series identifiers
        - Popular aggregate definitions and component series 
        - English and French text for UI components
	- Creates global variables
    - Defines reusable UI-related functions
    - Defines the ui function, which creates and positions UI objects and creates JS functions
    - Defines the server function, which receives user input, retrieves CODR data and displays results
    - Calls shinyApp(ui, server)
