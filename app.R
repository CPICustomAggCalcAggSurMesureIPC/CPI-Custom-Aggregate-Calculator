

#---------------------------------
# 0 Load libraries and set language

# 1 Define Functions
# 1.0 Define global functions
# 1.1 fIndexWeightChgCont() Main function to prepare estimates
# 1.2 fPlotTimeSeries() function to prepare graphs
# 1.3 fGetDisplaySeries() function to prepare available series
# 1.4 fMessage() function to display messages

# 2 Read input data and instantiate global objects

# 3 Shiny UI variables and functions

# 4 Server function
#---------------------------------








#---------------------------------
# 0 Load libraries and set language
#---------------------------------


if (!require("pacman")){
  install.packages("pacman")
}

# Load packages
pacman::p_load(
  dplyr,
  tidyr,
  readxl,
  sqldf,
  shiny,
  shinydashboard,
  shinyjs,
  reactable,
  waiter,
  plotly,
  httr,
  jsonlite,
  purrr,
  shinyBS,
  shinyWidgets,
  lubridate,
  scales
)


library(dplyr)
library(tidyr)
library(readxl)
library(httr)
library(jsonlite)
library(sqldf)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS) # info buttons
library(reactable)
library(waiter)
library(plotly)
library(scales)
library(shinyWidgets)
library(purrr)
library(DBI)
library(odbc)
  



cAppLanguage    <- "English"
#cAppLanguage    <- "French"








#---------------------------------
# 1.0 Define global functions
#---------------------------------


# global function to get number of months since 1900-01 from date
fPeriodSeq190001 <- function(fvcRefDate){
	 return((as.integer(substr(fvcRefDate, 1, 4)) - 1900) * 12 + as.integer(substr(fvcRefDate, 6, 7)))
}


# global function to get ref date from number of months since 1900-01
fRefDate <-function(fviPeriodSeq190001){
	 iYear  <- as.integer((fviPeriodSeq190001 - 1 ) / 12) + 1900
	 iMonth <- as.integer((fviPeriodSeq190001 - 1) %% 12) + 101
	 return(paste(as.character(iYear), substr(as.character(iMonth), 2, 3), "01", sep =  "-"))
}


# global rounding function
fRoundHAFZ <- function(fvnNumber, fviDigits) {
	 iSign                 <- sign(fvnNumber)
  nAbs                  <- abs(fvnNumber)
  nAbsRaised            <- nAbs * 10 ^ fviDigits
  nFuzz                 <- sqrt(.Machine$double.eps)
  nAbsRaisedFuzzed      <- nAbsRaised + 0.5 + nFuzz
  nAbsRaisedFuzzedTrunc <- trunc(nAbsRaisedFuzzed)
  nRoundedHAFZ          <- nAbsRaisedFuzzedTrunc / 10 ^ fviDigits * iSign
	 return(nRoundedHAFZ)
}


# global function to get English or French labels
fGetEnFrText <- function(fvcVarName){
  if      (cAppLanguage == "English") {
  	 nLanguageColumn <- 2
  	 Sys.setlocale("LC_ALL", "English.utf8")
  }
	 else if (cAppLanguage == "French")  {
		  nLanguageColumn <- 3
  	 Sys.setlocale("LC_ALL", "French.utf8")
	 }
	
	 return(dfTextEnFr[which(dfTextEnFr$variable_name == fvcVarName), nLanguageColumn])
}


# global function to get variable name from English or French label
fGetVarNameFromEnFrText <- function(fvcEnFrText){
  if      (cAppLanguage == "English") {
  	 nLanguageColumn <- 2
  }
	 else if (cAppLanguage == "French")  {
		  nLanguageColumn <- 3
	 }
	
	return( dfTextEnFr[which(dfTextEnFr[ , nLanguageColumn] == fvcEnFrText), 1] )
}




#---------------------------------
# 1.1 fIndexWeightChgCont() Main function to prepare estimates
  # A: get saved metadata (codes, descriptors and vector ids) for relevant series
  # B: retrieve CODR indexes and weights
  # C: get original 2001 weights and join with all other weights samegeo_all_to_Canada_all_weight_link_new_segment
  # D: Arrange saved and retrieved data
  # E: calculate indexes link:t for published series and all-items
  # F: calculate spagg, all-items excluding spagg link weights, index at link month, index link:t, relative tm1:t, index earliest:t and rebased index
  # G: calculate contributions to all-items same geo 12m and 1m change
  # H: calculate index 12m & 1m change, and change versus previous month in 12m & 1m change and 12m & 1m contribution
  # I: prepare list containing status message and results dataframe

  # descriptions                                                          series2   denominator for contrib and weight 
  # series_type                            geography_code  product_code            Cda, all           same geo, all
  # sel pub geo,         sel pub prod      sel             sel                     0, c0              s, c0
  # Canada,              all prod          0               c0             s4       0, c0
  # sel pub geo,         all prod          sel             c0
  # sel spagg geo,       all prod          sel             c0
  # sel spagg geo,       sel spagg prod    sel             sel
  # spagg geo,           all ex spagg prod u1              u2             s3        0, c0              u1, c0
  # spagg geo,           all prod          u1              c0             s5        0, c0
  # spagg geo,           spagg prod        u1              u1             s1        0, c0              u1, c0
  # Canada ex spagg geo, all ex spagg prod u2              u2             s2        0, c0              n/a
#---------------------------------


# function to retrieve data and calculate
fIndexWeightChgCont <- function(fvdfBasket, fvdfSeriesReg, fvdfSeriesSpagg, fvdfCODRIndexAll, fvdfCODRWeightAll, fvdfRefPeriods, fvvSpaggRow, fvcStartBasePer, fvcEndBasePer){
	
	
# A: get saved metadata (codes, descriptors and vector ids) for relevant series
# get codes, descriptors and vectors for selected spagg components
dfSpagg <- fvdfSeriesSpagg[fvvSpaggRow, ] |>
		mutate(series_type = "sel spagg geo, sel spagg prod") |>
		select(series_type, where_to_code, i_dim2_position, table_18100004_vector, table_18100007_samegeo_vector, i_base_period, geography_en, geography_fr,
         product_or_product_group_en, product_or_product_group_fr) |>
		left_join(fvdfSeriesSpagg  |>
				filter(i_dim2_position == "c0") |>
				select(where_to_code, table_18100007_Canada_vector),
		by = "where_to_code") |>
		rename(geography_code = where_to_code,
			      product_code = i_dim2_position)
	

# get codes, descriptors and vectors for selected spagg series as if they're regular series from pre-202312 version of code
dfReg <- dfSpagg |>
		select(geography_code, product_code) |>
		left_join(fvdfSeriesSpagg |>
	   mutate(series_type                  = "sel pub geo, sel pub prod",
	  			     table_18100007_Canada_vector = NA) |>
  	 select(series_type, where_to_code, i_dim2_position, table_18100004_vector, table_18100007_samegeo_vector, table_18100007_Canada_vector, i_base_period,
  				     geography_en, geography_fr, product_or_product_group_en, product_or_product_group_fr) |>
    rename(geography_code = where_to_code,
  				     product_code   = i_dim2_position),
		by = c("geography_code", "product_code"))


# combine codes, descriptors and vectors for selected regular series and spagg components
dfRegSpagg <- rbind(dfReg, dfSpagg) |>
 	mutate(table_18100007_Canada_vector = NA)

  
# also get codes, descriptors and vectors for All-items in selected geos
dfCanadaAll <- fvdfSeriesReg |> 
  filter(where_to_code == "0" & i_dim2_position == "c0") |>
  select(where_to_code, i_dim2_position) |>
 	mutate(series_type = "Canada, all prod")
  				 

dfRegGeoAll <- fvdfSeriesReg |> 
   filter(i_dim2_position == "c0" & where_to_code %in% unique(dfReg$geography_code)) |>
   select(where_to_code, i_dim2_position) |>
  	mutate(series_type = "sel pub geo, all prod")

dfSpaggGeoAll <- fvdfSeriesReg |> 
  filter(i_dim2_position == "c0" & where_to_code %in% unique(dfSpagg$geography_code)) |>
  select(where_to_code, i_dim2_position) |>
 	mutate(series_type = "sel spagg geo, all prod")

dfAll <- rbind(dfCanadaAll, dfRegGeoAll, dfSpaggGeoAll) |>
 	select(series_type, where_to_code, i_dim2_position) |>
 	left_join(fvdfSeriesReg |>
				select(where_to_code, i_dim2_position, table_18100004_vector, table_18100007_samegeo_vector, table_18100007_Canada_vector, i_base_period,
           geography_en, geography_fr, product_or_product_group_en, product_or_product_group_fr), 
  by = c("where_to_code", "i_dim2_position")) |>
  rename(geography_code = where_to_code,
  				   product_code   = i_dim2_position)


# combine codes, descriptors and vectors for selected regular series, spagg components and All-items in selected geos
dfRegSpaggAll <- rbind(dfRegSpagg, dfAll)


# get weight same geo and Canada vectors
viWeightSameGeoVector <- dfRegSpaggAll[!is.na(dfRegSpaggAll$table_18100007_samegeo_vector), ]$table_18100007_samegeo_vector
viWeightCanadaVector  <- dfRegSpaggAll[!is.na(dfRegSpaggAll$table_18100007_Canada_vector), ]$table_18100007_Canada_vector
viWeightVector        <- c(viWeightSameGeoVector, viWeightCanadaVector)
dfWeightVector        <- as.data.frame(viWeightVector)
  
  
  

  
  
# B: retrieve CODR indexes and weights
  # get CODR indexes
  dfCODRIndex <- dfRegSpaggAll |>
    distinct(table_18100004_vector) |>
    left_join(
      fvdfCODRIndexAll,
      by = "table_18100004_vector")


  # get weight same geo and Canada vectors
  viWeightSameGeoVector <- dfRegSpaggAll[!is.na(dfRegSpaggAll$table_18100007_samegeo_vector), ]$table_18100007_samegeo_vector
  viWeightCanadaVector  <- dfRegSpaggAll[!is.na(dfRegSpaggAll$table_18100007_Canada_vector), ]$table_18100007_Canada_vector
  viWeightVector        <- unique(c(viWeightSameGeoVector, viWeightCanadaVector))
  dfWeightVector        <- as.data.frame(viWeightVector) |> rename(table_18100007_vector = viWeightVector)


  # get CODR weights
  dfRegSpaggAllWeight <- dfWeightVector |>
    left_join(
      fvdfCODRWeightAll,
      by = "table_18100007_vector")


  # C: get original 2001 weights and join with all other weights samegeo_all_to_Canada_all_weight_link_new_segment
  # No longer needed


  # D: Arrange saved and retrieved data
  # start with selected series metadata, create row for all possible ref periods, join with basket history, CODR indexes and weights, get link index
  dfRegSpaggAllIndexWeight <- sqldf::sqldf("
    select v.*,
           rp.reference_period,
           b.*,
           cws.geo_prod_to_samegeo_all_weight_link,
           cwc.geo_all_to_Canada_all_weight_link,
           ci.index_t,
           cilink.index_link,
           cws.geo_prod_to_samegeo_all_weight_link     as geo_prod_to_samegeo_all_weight_link2

    from   
           dfRegSpaggAll v

    join
           fvdfRefPeriods rp
        
    left join
          (select weight_reference_period,
                  weight_version,
                  link_period,
                  first_period                as basket_first_period,
                  last_period                 as basket_last_period

           from   fvdfBasket) b
    on  rp.reference_period >= b.basket_first_period
    and rp.reference_period <= b.basket_last_period
  
    left join
          (select table_18100007_vector       as table_18100007_samegeo_vector, 
                  weight_reference_period,
                  weight_version,
                  weight_r                    as geo_prod_to_samegeo_all_weight_link

           from   dfRegSpaggAllWeight) cws
    on  v.table_18100007_samegeo_vector = cws.table_18100007_samegeo_vector
    and b.weight_reference_period       = cws.weight_reference_period
    and b.weight_version                = cws.weight_version
 
    left join
          (select table_18100007_vector       as table_18100007_Canada_vector, 
                  weight_reference_period,
                  weight_version,
                  weight_r                    as geo_all_to_Canada_all_weight_link

           from   dfRegSpaggAllWeight) cwc
    on  v.table_18100007_Canada_vector = cwc.table_18100007_Canada_vector
    and b.weight_reference_period      = cwc.weight_reference_period
    and b.weight_version               = cwc.weight_version
 
    left join
          (select table_18100004_vector, 
                  reference_period,
                  index_r                     as index_t

           from   dfCODRIndex) ci
    on  v.table_18100004_vector = ci.table_18100004_vector
    and rp.reference_period     = ci.reference_period
 
    left join
          (select table_18100004_vector,
                  reference_period            as link_period, 
                  index_r                     as index_link

           from   dfCODRIndex) cilink
    on  v.table_18100004_vector = cilink.table_18100004_vector
    and b.link_period           = cilink.link_period
 
    order by series_type,
           geography_code,
           product_code,
           rp.reference_period") |>
	  mutate(ref_period_seq_190001 = fPeriodSeq190001(reference_period) ) 
# df299 <- dfRegSpaggAllIndexWeight[dfRegSpaggAllIndexWeight$reference_period >= "2024-07-01", ]
  
  
  # calc weight geo_prod_to_Canada_all and keep weight geo_all_to_Canada_all
  dfRegSpaggAllIndexWeight <- 
    dfRegSpaggAllIndexWeight |>
  	 left_join(
  	    dfRegSpaggAllIndexWeight |>
  							filter(product_code == "c0" & !is.na(geo_all_to_Canada_all_weight_link)) |>
  							group_by(geography_code, ref_period_seq_190001, geo_all_to_Canada_all_weight_link) |>
  						 summarize(c = n(), .groups = "keep") |>
  							rename(geo_all_to_Canada_all_weight_link2 = geo_all_to_Canada_all_weight_link),
  					by = c("geography_code", "ref_period_seq_190001")) |>
  	 mutate(geo_prod_to_Canada_all_weight_link = geo_prod_to_samegeo_all_weight_link2 * geo_all_to_Canada_all_weight_link2 / 100) |>
  	 select(-c(geo_prod_to_samegeo_all_weight_link, geo_prod_to_samegeo_all_weight_link2, geo_all_to_Canada_all_weight_link, c, geo_all_to_Canada_all_weight_link2))

  
  # for spagg calcs, only keep records post-19xx
  dfRegSpaggAllIndexWeight2 <- dfRegSpaggAllIndexWeight |>
  	 filter(ref_period_seq_190001 >= fPeriodSeq190001(cFirstWeightEffectivePeriod))
  
  
  # nullify a series during life of basket if index in any month in basket is null
  dfRegSpaggAllIndexWeightAdj <- 
    dfRegSpaggAllIndexWeight2 |>
  	 left_join(
  	   dfRegSpaggAllIndexWeight2 |>
  						filter(is.na(index_t)) |>
  						group_by(series_type, geography_code, product_code, link_period) |>
  						summarize(num_null_indexes = n(), .groups = "keep"),
      by = c("series_type", "geography_code", "product_code", "link_period")) |>
  	 mutate(index_link_t                           = ifelse(is.na(num_null_indexes) | num_null_indexes == 0, index_t / index_link, NA),
  				     geo_prod_to_Canada_all_weight_link_adj = ifelse(is.na(num_null_indexes) | num_null_indexes == 0, geo_prod_to_Canada_all_weight_link, NA))
#  df254 <- dfRegSpaggAllIndexWeightAdj[dfRegSpaggAllIndexWeightAdj$reference_period == "2022-06-01", ]


  
  
  
  
  
  
  
  # E: calculate indexes link:t for all series
  dfRegAllIndexWeightLinkt <- dfRegSpaggAllIndexWeightAdj |>
  	 select(-geo_prod_to_Canada_all_weight_link) |>
  	 mutate(geo_prod_to_Canada_all_weighted_index_link_t = geo_prod_to_Canada_all_weight_link_adj * index_t / index_link) |>
  	 rename(geo_prod_to_Canada_all_weight_link = geo_prod_to_Canada_all_weight_link_adj) |>
  	 select(series_type, geography_code, geography_en, geography_fr, product_code, product_or_product_group_en, product_or_product_group_fr,
  				     i_base_period, reference_period, ref_period_seq_190001, weight_reference_period, weight_version, index_t, link_period,
           basket_first_period, basket_last_period, geo_prod_to_Canada_all_weight_link, geo_prod_to_Canada_all_weighted_index_link_t, index_link_t)
#  df404 <- dfRegAllIndexWeightLinkt[dfRegAllIndexWeightLinkt$reference_period == "2022-06-01", ]


      

  
  
  
    
  # F: calculate spagg, Canada & spagg geo all-items excluding spagg link weights, index at link month, index link:t, relative tm1:t, index earliest:t and rebased index
  # get spagg inputs: (s, s) & (s, c0) (sel spagg geo, sel spagg prod) & (sel spagg geo, all prod)
  
  # calculate new aggregates (u1, u1) & (u1, c0) (spagg geo, spagg prod) & (spagg geo, all prod)
  dfSpaggGeoIndexWeight <- dfRegAllIndexWeightLinkt |>
  	 filter(series_type == 'sel spagg geo, sel spagg prod' | series_type == 'sel spagg geo, all prod') |>
  	 group_by(series_type, reference_period, weight_reference_period, weight_version, link_period, basket_first_period, basket_last_period, ref_period_seq_190001) |>
    summarize(geo_prod_to_Canada_all_weight_link           = sum(geo_prod_to_Canada_all_weight_link, na.rm = TRUE),
    					     geo_prod_to_Canada_all_weighted_index_link_t = sum(geo_prod_to_Canada_all_weighted_index_link_t, na.rm = TRUE),
    					     index_link_t                                 = sum(geo_prod_to_Canada_all_weighted_index_link_t, na.rm = TRUE) / sum(geo_prod_to_Canada_all_weight_link, na.rm = TRUE), 
    					     .groups= "keep") |>
    mutate(series_type                 = ifelse(series_type == 'sel spagg geo, sel spagg prod', 'spagg geo, spagg prod', 'spagg geo, all prod'),
    			    geography_code              = "u1",
    			    product_code                = ifelse(series_type == 'spagg geo, spagg prod', 'u1', 'c0'),
           geography_en                = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomGeographyUserLabel"), 2],
  				     geography_fr                = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomGeographyUserLabel"), 3],
  				     product_or_product_group_en = ifelse(product_code == "u1", dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductUserLabel"), 2], dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductAllLabel"),  2]),
  				     product_or_product_group_fr = ifelse(product_code == "u1", dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductUserLabel"), 3], dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductAllLabel"),  3]) )  
# df299 <- dfSpaggGeoIndexWeight[dfSpaggGeoIndexWeight$reference_period == "2007-04-01", ]  
 


  # combine regular, Canada all-items and spagg
  dfRegSpaggAllIndexWeightLinkt <- rbind(dfRegAllIndexWeightLinkt |> select(-c(i_base_period, index_t)), dfSpaggGeoIndexWeight) 
#  df471 <- dfRegSpaggAllIndexWeightLinkt[dfRegSpaggAllIndexWeightLinkt$reference_period == "2022-06-01", ]

  
  
  # calculate (u2, u2)  (Canada ex spagg geo, all ex spagg prod)
  dfCanadaAllExSpaggIndexLinkt <- 
    dfRegSpaggAllIndexWeightLinkt |>
  	   filter(series_type == 'spagg geo, spagg prod') |>
  	   rename(selected_geo_prod_to_Canada_all_weight_link           = geo_prod_to_Canada_all_weight_link,
  				       selected_geo_prod_to_Canada_all_weighted_index_link_t = geo_prod_to_Canada_all_weighted_index_link_t) |>
  	   select(geography_code, geography_en, geography_fr, reference_period, ref_period_seq_190001, weight_reference_period, weight_version,
             link_period, basket_first_period, basket_last_period, selected_geo_prod_to_Canada_all_weight_link, selected_geo_prod_to_Canada_all_weighted_index_link_t) |>
  	 left_join(
  	   dfRegSpaggAllIndexWeightLinkt |>
  	     filter(series_type == 'Canada, all prod') |>
  						rename(Canada_all_geo_prod_to_Canada_all_weight_link           = geo_prod_to_Canada_all_weight_link,
  						  			  Canada_all_geo_prod_to_Canada_all_weighted_index_link_t = geo_prod_to_Canada_all_weighted_index_link_t) |>
  						select(reference_period, Canada_all_geo_prod_to_Canada_all_weight_link, Canada_all_geo_prod_to_Canada_all_weighted_index_link_t),
  				by = "reference_period") |>
  	  mutate(series_type                                  = "Canada ex spagg geo, all ex spagg prod",
    			     geography_code                               = "u2",
    			     product_code                                 = "u2",
  				      geo_prod_to_Canada_all_weight_link           = Canada_all_geo_prod_to_Canada_all_weight_link - selected_geo_prod_to_Canada_all_weight_link,
            geo_prod_to_Canada_all_weighted_index_link_t = Canada_all_geo_prod_to_Canada_all_weighted_index_link_t - selected_geo_prod_to_Canada_all_weighted_index_link_t,
            index_link_t                                 =   (Canada_all_geo_prod_to_Canada_all_weighted_index_link_t - selected_geo_prod_to_Canada_all_weighted_index_link_t) 
  				                                                     / (Canada_all_geo_prod_to_Canada_all_weight_link - selected_geo_prod_to_Canada_all_weight_link),
  		        geography_en                                 = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomGeographyAllExUserLabel"), 2],
  				      geography_fr                                 = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomGeographyAllExUserLabel"), 3],
  				      product_or_product_group_en                  = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductAllExUserLabel"),   2],
  				      product_or_product_group_fr                  = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductAllExUserLabel"),   3]) |>
  	  select(series_type, geography_code, product_code, reference_period, ref_period_seq_190001, weight_reference_period, weight_version,
            link_period, basket_first_period, basket_last_period, geo_prod_to_Canada_all_weight_link, geo_prod_to_Canada_all_weighted_index_link_t, index_link_t,
  				      geography_en, geography_fr, product_or_product_group_en, product_or_product_group_fr)
#  df526 <- dfCanadaAllExSpaggIndexLinkt[dfCanadaAllExSpaggIndexLinkt$reference_period == "2022-06-01", ]
  

  # calculate (u1, u2) (spagg geo, all ex spagg prod)
  dfGeoAllExSpaggIndexLinkt <- 
    dfRegSpaggAllIndexWeightLinkt |>
  	   filter(series_type == 'spagg geo, spagg prod') |>
  	   rename(selected_geo_prod_to_Canada_all_weight_link           = geo_prod_to_Canada_all_weight_link,
  				       selected_geo_prod_to_Canada_all_weighted_index_link_t = geo_prod_to_Canada_all_weighted_index_link_t) |>
  	   select(geography_code, geography_en, geography_fr, reference_period, ref_period_seq_190001, weight_reference_period, weight_version,
             link_period, basket_first_period, basket_last_period, selected_geo_prod_to_Canada_all_weight_link, selected_geo_prod_to_Canada_all_weighted_index_link_t) |>
  	 left_join(
  	    dfRegSpaggAllIndexWeightLinkt |>
  	      filter(series_type == 'spagg geo, all prod') |>
  						 rename(Canada_all_geo_prod_to_Canada_all_weight_link           = geo_prod_to_Canada_all_weight_link,
  						  			   Canada_all_geo_prod_to_Canada_all_weighted_index_link_t = geo_prod_to_Canada_all_weighted_index_link_t) |>
  						 select(reference_period, Canada_all_geo_prod_to_Canada_all_weight_link, Canada_all_geo_prod_to_Canada_all_weighted_index_link_t),
  				by = "reference_period") |>
  	 mutate(series_type                                  = "spagg geo, all ex spagg prod",
    			    geography_code                               = "u1",
    			    product_code                                 = "u2",
  				     geo_prod_to_Canada_all_weight_link           = Canada_all_geo_prod_to_Canada_all_weight_link - selected_geo_prod_to_Canada_all_weight_link,
           geo_prod_to_Canada_all_weighted_index_link_t = Canada_all_geo_prod_to_Canada_all_weighted_index_link_t - selected_geo_prod_to_Canada_all_weighted_index_link_t,
           index_link_t                                 =   (Canada_all_geo_prod_to_Canada_all_weighted_index_link_t - selected_geo_prod_to_Canada_all_weighted_index_link_t) 
  				                                                    / (Canada_all_geo_prod_to_Canada_all_weight_link - selected_geo_prod_to_Canada_all_weight_link),
  		       geography_en                                 = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomGeographyAllExUserLabel"), 2],
  				     geography_fr                                 = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomGeographyAllExUserLabel"), 3],
  				     product_or_product_group_en                  = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductAllExUserLabel"),   2],
  				     product_or_product_group_fr                  = dfTextEnFr[which(dfTextEnFr$variable_name == "CustomProductAllExUserLabel"),   3]) |>
  	 select(series_type, geography_code, geography_en, geography_fr, product_code, reference_period, ref_period_seq_190001, weight_reference_period, weight_version,
           link_period, basket_first_period, basket_last_period, geo_prod_to_Canada_all_weight_link, geo_prod_to_Canada_all_weighted_index_link_t, index_link_t,
  		     	 product_or_product_group_en, product_or_product_group_fr)
 #  df583 <- dfGeoAllExSpaggIndexLinkt[dfGeoAllExSpaggIndexLinkt$reference_period == "2022-06-01", ]
 
  
  # union (u1, u1) & (u1, c0), (u2, u2), (u1, u2)  (spagg geo, spagg prod) & (spagg geo, all prod), (Canada ex spagg geo, all ex spagg prod), (spagg geo, all ex spagg prod)
  dfSpaggAllExSpaggIndexWeightLinkt <- 
    data.frame(rbind(
      dfSpaggGeoIndexWeight, 
  		  dfCanadaAllExSpaggIndexLinkt, 
  		  dfGeoAllExSpaggIndexLinkt))
   
  
  # calculate all new spagg monthly relative
  dfSpaggAllExSpaggIndex <- dfSpaggAllExSpaggIndexWeightLinkt |>
  	 select(series_type, geography_code, geography_en,  geography_fr, product_code, product_or_product_group_en,  product_or_product_group_fr,
           reference_period, ref_period_seq_190001, weight_reference_period, weight_version, link_period, basket_first_period, basket_last_period,
           geo_prod_to_Canada_all_weight_link, geo_prod_to_Canada_all_weighted_index_link_t, index_link_t) |>
  	 arrange(series_type, geography_code, product_code, ref_period_seq_190001) |>
  	 mutate(index_link_tm1 = ifelse(series_type == lag(series_type) & geography_code == lag(geography_code) & product_code == lag(product_code) & ref_period_seq_190001 == lag(ref_period_seq_190001) + 1, lag(index_link_t), NA),
  				     relative_tm1_t = ifelse(reference_period == basket_first_period, index_link_t, index_link_t / index_link_tm1))
#  df384 <- dfSpaggAllExSpaggIndex[dfSpaggAllExSpaggIndex$reference_period == "2022-06-01", ]
  

  # set spagg and all ex spagg period 0 as min period where link weight > 0
  # removes spagg geo All-ex selected when selected components entirely all items
  dfSpaggAllExSpaggIndex <- 
    dfSpaggAllExSpaggIndex |>
  	 left_join(
  	   dfSpaggAllExSpaggIndex |>
  						filter(relative_tm1_t > 0) |>
  						group_by(series_type, geography_code) |>
  						summarize(period_0_seq_190001 = min(ref_period_seq_190001) - 1, .groups = "keep"),
  				by = c("series_type", "geography_code")) |>
  	 mutate(count_periods_0_t = ref_period_seq_190001 - period_0_seq_190001) |>
  	 filter(count_periods_0_t >= 0) |> 
	# calc spagg and all ex spagg index 0 and round 
	   mutate(relative_tm1_t_temp = ifelse(count_periods_0_t == 0, 1, relative_tm1_t),
           index_0_t           = ave(relative_tm1_t_temp, series_type, FUN = cumprod) * 100) |>
    select(-relative_tm1_t_temp)
# df401 <- dfSpaggAllExSpaggIndex[dfSpaggAllExSpaggIndex$reference_period == "2022-06-01", ]
 
  
  

  dStartBasePeriod <- as.Date(paste0(fvcStartBasePer, "-01"), format="%Y-%m-%d")
  dEndBasePeriod   <- as.Date(paste0(fvcEndBasePer,   "-01"), format="%Y-%m-%d")
  
  # create base period text
  if (fvcStartBasePer == fvcEndBasePer) {
  	 cBasePer <- paste0(substr(fvcStartBasePer, 1, 4), substr(fvcStartBasePer, 6, 7), "=100")
  } else if (substr(fvcStartBasePer, 1, 4) == substr(fvcEndBasePer, 1, 4) & substr(fvcStartBasePer, 6, 7) == "01" & substr(fvcEndBasePer, 6, 7) == "12") {
  	 cBasePer <- paste0(substr(fvcStartBasePer, 1, 4), "=100")
  } else {
  	 cBasePer <- paste0(substr(fvcStartBasePer, 1, 4), substr(fvcStartBasePer, 6, 7), "-", substr(fvcEndBasePer, 1, 4), substr(fvcEndBasePer, 6, 7), "=100")
  }
  
  # build series text
  # change 2024-07-12 to sort by code
  dfSpaggComponents <- dfSpagg |>
  	 mutate(language   = cAppLanguage,
           geo        = ifelse(language == "English", geography_en, geography_fr),
           prod       = ifelse(language == "English", product_or_product_group_en, product_or_product_group_fr),
  		  	    prod_geo   = paste0(prod, " / ", geo) ) |>
  	 arrange(geography_code, product_code)
  
  cSeriesGeoOption1  <- fGetEnFrText("SeriesGeoNameOption1Text")
 	cSeriesGeoOption2  <- fGetEnFrText("SeriesGeoNameOption2Text")
 	cSeriesProdOption1 <- fGetEnFrText("SeriesProdNameOption1Text")
 	cSeriesProdOption2 <- fGetEnFrText("SeriesProdNameOption2Text")
 	cSeriesPart1       <- fGetEnFrText("SeriesNamePart1Text")  
 	cSeriesPart2       <- fGetEnFrText("SeriesNamePart2Text")  
 	cSeriesPart3       <- fGetEnFrText("SeriesNamePart3Text")  

	# create compound geography descriptor
  iNumGeo <- length(unique(dfSpaggComponents$geo))
  if        (iNumGeo == 1) {cSpaggGeo <- unique(dfSpaggComponents$geo)
  } else if (iNumGeo <= 3) {cSpaggGeo <- paste0("(", paste(unique(dfSpaggComponents$geo), collapse = " + "), ")")
  } else if (iNumGeo == 4) {cSpaggGeo <- paste0("(", paste(unique(dfSpaggComponents$geo)[1:3], collapse = " + "), " + ", cSeriesGeoOption1, ")")
  } else if (iNumGeo  > 4) {cSpaggGeo <- paste0("(", paste(unique(dfSpaggComponents$geo)[1:3], collapse = " + "), " + ", iNumGeo - 3, " ", cSeriesGeoOption2, ")")}

	# create compound product & geography descriptor
  iNumProdGeo   <- length(unique(dfSpaggComponents$prod_geo))
  if        (iNumProdGeo == 1) {cSpaggProdGeo <- unique(dfSpaggComponents$prod_geo)
  } else if (iNumProdGeo <= 3) {cSpaggProdGeo <- paste0("(", paste(unique(dfSpaggComponents$prod_geo), collapse = " + "), ")")
  } else if (iNumProdGeo == 4) {cSpaggProdGeo <- paste0("(", paste(unique(dfSpaggComponents$prod_geo)[1:3], collapse = " + "), " + ", cSeriesProdOption1, ")")
  } else if (iNumProdGeo  > 4) {cSpaggProdGeo <- paste0("(", paste(unique(dfSpaggComponents$prod_geo)[1:3], collapse = " + "), " + ", iNumProdGeo - 3, " ", cSeriesProdOption2, ")")}
  
# prod	geo	display option 2
# u1	u1	(1) CPI for (Rent / Quebec + Rent / Ontario)
# u2	u2	(2) CPI for All-items / Canada excluding (Rent / Quebec + Rent / Ontario)
# u2	u1	(3) CPI for All-items / (Quebec + Ontario) excluding (Rent / Quebec + Rent / Ontario)
# c0	0	  (4) CPI for All-items / Canada
# c0	u1	(5) CPI for All-items / (Quebec + Ontario)
  dfSpaggAllExSpaggAllIndex <- rbind(
   	dfRegAllIndexWeightLinkt |>
   		 filter(product_code == "c0" & geography_code == "0") |>
	     select(series_type, geography_code, geography_en, geography_fr, product_code, product_or_product_group_en, product_or_product_group_fr, 
	    	 		 reference_period, ref_period_seq_190001, weight_reference_period, weight_version, i_base_period, 
	    		 	 index_t, link_period, basket_first_period, basket_last_period, geo_prod_to_Canada_all_weight_link, geo_prod_to_Canada_all_weighted_index_link_t),
  	 dfSpaggAllExSpaggIndex |>
      mutate(i_base_period = cBasePer) |>
  		  rename(index_t = index_0_t) |>
      select(series_type, geography_code, geography_en, geography_fr, product_code, product_or_product_group_en, product_or_product_group_fr, 
	    			     reference_period, ref_period_seq_190001, weight_reference_period, weight_version, i_base_period, 
	    			     index_t, link_period, basket_first_period, basket_last_period, geo_prod_to_Canada_all_weight_link, geo_prod_to_Canada_all_weighted_index_link_t)) |>
    mutate(ref_period = as.Date(reference_period, format = "%Y-%m-%d"),
	  	       series2 = case_when(product_code == "u1" & geography_code == "u1" ~ "s1",
	  			 				                    product_code == "u2" & geography_code == "u2" ~ "s2",
	  			 				                    product_code == "u2" & geography_code == "u1" ~ "s3",
	  			 				                    series_type  == "Canada, all prod"            ~ "s4",
	  			 				                    product_code == "c0" & geography_code == "u1" ~ "s5"),
	  	       series  = case_when(series2 == "s1" ~ paste0(cSeriesPart1, " ", cSpaggProdGeo),
	  			 								                series2 == "s2" ~ paste0(cSeriesPart1, " ", cSeriesPart2, " / Canada ", cSeriesPart3, " ", cSpaggProdGeo),
	  			 								                series2 == "s3" ~ paste0(cSeriesPart1, " ", cSeriesPart2, " / ", cSpaggGeo, " ", cSeriesPart3, " ", cSpaggProdGeo),
	  			 								                series2 == "s4" ~ paste0(cSeriesPart1, " ", cSeriesPart2, " / Canada"),
	  			 								                series2 == "s5" ~ paste0(cSeriesPart1, " ", cSeriesPart2, " / ", cSpaggGeo)) ) |>
    filter(!is.na(series2))
# df478 <- dfSpaggAllExSpaggAllIndex  |> filter(reference_period == "2023-04-01")  |> mutate(index_t = format(index_t, nsmall = 12)) |> select(geography_code, product_code, index_t)

  

  # create dataset of rebased s1:s5
  dfSpaggAllExSpaggAllIndexRebased <- 
    dfSpaggAllExSpaggAllIndex |>
  	 left_join(
  	   dfSpaggAllExSpaggAllIndex |>
  						filter(ref_period >= dStartBasePeriod & ref_period <= dEndBasePeriod) |>
  	     group_by(series2) |>
  						summarize(index_b = mean(index_t)),
  				by = "series2") |>
  	 mutate(index_r_r     = fRoundHAFZ(index_t / index_b * 100, 1),
  				     i_base_period = cBasePer,
  				     rebase_type   = "rebased") |>
  	 select(-c(index_t, index_b)) 
# df539 <- dfSpaggAllExSpaggAllIndexRebased  |> filter(reference_period == "2007-04-01")


  # create dataset of published s1, s4, s5
  dfSpaggAllPub <- dfRegSpaggAllIndexWeightAdj |>
    mutate(series2 = case_when(series_type == "sel spagg geo, sel spagg prod" ~ "s1",
    							                    series_type == "Canada, all prod"              ~ "s4",
	  			 								                series_type == "sel spagg geo, all prod"       ~ "s5")) |>
  	 rename(index_pub         = index_t,
  			      i_base_period_pub = i_base_period) |>
    select(series2, reference_period, index_pub, i_base_period_pub) |>
    filter(!(is.na(series2) | is.na(index_pub)))

  
  # changed from left_ to inner_join 2024-03-25 for cases like Cell serv Canada where 1st weight period and base period is after first published
  if (iNumProdGeo == 1) {
    dfSpaggAllExSpaggAllIndexRebasedPub <- 
      dfSpaggAllPub |>
      inner_join(
        dfSpaggAllExSpaggAllIndexRebased, 
    		  by = c("series2", "reference_period"))
  } else if (iNumGeo == 1) {
    dfSpaggAllExSpaggAllIndexRebasedPub <- 
      dfSpaggAllPub |>
  	     filter(series2 == "s4" | series2 == "s5") |>
      inner_join(
        dfSpaggAllExSpaggAllIndexRebased, 
    				by = c("series2", "reference_period"))
  } else {
    dfSpaggAllExSpaggAllIndexRebasedPub <- 
      dfSpaggAllPub |>
  	     filter(series2 == "s4") |>
      inner_join(
        dfSpaggAllExSpaggAllIndexRebased, 
    				by = c("series2", "reference_period"))
  }


  # use values from published series  
  dfSpaggAllExSpaggAllIndexRebasedPub <- dfSpaggAllExSpaggAllIndexRebasedPub |>
  	 mutate(index_t       = ifelse(!is.na(i_base_period_pub), index_pub, index_r_r),
  				     i_base_period = ifelse(!is.na(i_base_period_pub), i_base_period_pub, i_base_period),
  				     rebase_type   = "published") |> 
    select(-c(i_base_period_pub, index_pub, index_r_r))

  
  dfSpaggAllExSpaggAllIndexRebased2 <- rbind(
  	dfSpaggAllExSpaggAllIndexRebased |> rename(index_t = index_r_r), 
  	dfSpaggAllExSpaggAllIndexRebasedPub)
# df537 <- dfSpaggAllExSpaggAllIndexRebased2 |> filter(series2 == "s2") |> arrange(reference_period)
# df537 <- dfSpaggAllExSpaggAllIndexRebased2 |> filter(reference_period == "2023-12-01") |> arrange(series2)
# unique(dfSpaggAllExSpaggAllIndexRebasedPub[ , c("series2", "series", "rebase_type", "i_base_period")]) 

  
  # derive necesary columns for Reg series
  dfRegAllIndexWeightLinkt2 <- dfRegAllIndexWeightLinkt |> 
  	 filter(series_type == "sel pub geo, sel pub prod") |>
  	 group_by(geography_code, product_code) |>
  	 mutate(group_id = cur_group_id()) |>
  	 ungroup() |>
  	 mutate(language    = cAppLanguage,
  				     ref_period  = as.Date(reference_period, format = "%Y-%m-%d"),
  				     rebase_type = "published",
  				     series      = ifelse(language == "English",  paste0(cSeriesPart1, " ", product_or_product_group_en, " / ", geography_en), paste0(cSeriesPart1, " ", product_or_product_group_fr, " / ", geography_fr)),
  				     series2     = paste0("r", group_id)) |> 
  	 select(-c(index_link_t, language, group_id))
  
  
  # get rebased
  dfRegAllIndexWeightLinktRebased <- 
    dfRegAllIndexWeightLinkt2 |>
  	 left_join(
  	   dfRegAllIndexWeightLinkt2 |>
  						filter(ref_period >= dStartBasePeriod & ref_period <= dEndBasePeriod) |>
  						group_by(series2) |>
  						summarize(index_b = mean(index_t)),
  				by = "series2") |>
  	 mutate(index_r_r     = fRoundHAFZ(index_t / index_b * 100, 1),
  				     i_base_period = cBasePer,
  				     rebase_type   = "rebased") |>
  	 select(-c(index_t, index_b)) |> 
  	 rename(index_t = index_r_r) |> 
  	 filter(!is.na(index_t))
  
  dfRegAllIndexWeightLinktRebased2 <- rbind(dfRegAllIndexWeightLinktRebased, dfRegAllIndexWeightLinkt2)

  





   
  # G: calculate contributions to Canada and same geo all-items 12m- and 1m-change
  # get input data, combine regular and spagg, all ex spagg indexes, weights by ref period
  dfRegSpaggAllExSpaggIndexWeight <- rbind(dfSpaggAllExSpaggAllIndexRebased2, dfRegAllIndexWeightLinktRebased2) |>
  	 mutate(series = paste0(series, ", ", i_base_period)) |>
  	 arrange(series_type, geography_code, product_code, reference_period)
#  df582 <- dfRegSpaggAllExSpaggIndexWeight[dfRegSpaggAllExSpaggIndexWeight$reference_period == "2022-06-01", ]


  # select and create variables necessary for contribution
  dfContComp <-dfRegSpaggAllExSpaggIndexWeight |>
  	 arrange(series2, rebase_type, series_type, geography_code, product_code, reference_period) |>
   	mutate(reference_period                               = as.Date(reference_period, format="%Y-%m-%d"),
				       ref_period_seq_190001                          = fPeriodSeq190001(reference_period),
				       link_period_seq_190001                         = fPeriodSeq190001(link_period),
				       basket_first_period_seq_190001                 = fPeriodSeq190001(basket_first_period),
				       basket_last_period_seq_190001                  = fPeriodSeq190001(basket_last_period),
				       geo_prod_to_Canada_all_weight_link_new_segment = geo_prod_to_Canada_all_weight_link,
				       geo_prod_to_Canada_all_weight_link_eff_segment = ifelse(geography_code == lag(geography_code) & product_code == lag(product_code) & ref_period_seq_190001 == lag(ref_period_seq_190001) + 1, geo_prod_to_Canada_all_weight_link, 0)) |>
    select(series2, rebase_type, series_type, geography_code, product_code, reference_period, ref_period_seq_190001, link_period_seq_190001, basket_first_period_seq_190001, 
           basket_last_period_seq_190001, geo_prod_to_Canada_all_weight_link_new_segment, geo_prod_to_Canada_all_weight_link_eff_segment, index_t)
# df613 <- dfContComp[dfContComp$reference_period == "2023-12-01", ]
# df839 <- dfContComp |> filter(series2 == "s2") |> arrange(reference_period)
# unique(dfRegSpaggAllExSpaggIndexWeight[ , c("series2", "series", "rebase_type", "i_base_period")]) 

  
   # get Canada All-items values
   # updated 2024-02-26
   # updated 2024-09-09 to include s4 and s5 in contrib
   # updated 2024-10-01 to include r1:rn in contrib
   dfContCompAll <- sqldf::sqldf("
    select l.*,
           r.Canada_all_to_Canada_all_weight_link_new_segment,
           r.Canada_all_to_Canada_all_weight_link_eff_segment,
           r.Canada_all_index_t,
           
           case when substr(series2, 1, 1) = 's' and r2.samegeo_all_to_Canada_all_weight_link_new_segment_published    is not null then r2.samegeo_all_to_Canada_all_weight_link_new_segment_published
                when substr(series2, 1, 1) = 's' and r2.samegeo_all_to_Canada_all_weight_link_new_segment_published    is null     then r3.samegeo_all_to_Canada_all_weight_link_new_segment_rebased
                when substr(series2, 1, 1) = 'r' and r4.samegeo_all_to_Canada_all_weight_link_new_segment_published_u1 is not null then r4.samegeo_all_to_Canada_all_weight_link_new_segment_published_u1
                when substr(series2, 1, 1) = 'r' and r4.samegeo_all_to_Canada_all_weight_link_new_segment_published_u1 is null     then r5.samegeo_all_to_Canada_all_weight_link_new_segment_rebased_u1
                end as samegeo_all_to_Canada_all_weight_link_new_segment,
                
           case when substr(series2, 1, 1) = 's' and r2.samegeo_all_to_Canada_all_weight_link_eff_segment_published    is not null then r2.samegeo_all_to_Canada_all_weight_link_eff_segment_published
                when substr(series2, 1, 1) = 's' and r2.samegeo_all_to_Canada_all_weight_link_eff_segment_published    is null     then r3.samegeo_all_to_Canada_all_weight_link_eff_segment_rebased
                when substr(series2, 1, 1) = 'r' and r4.samegeo_all_to_Canada_all_weight_link_eff_segment_published_u1 is not null then r4.samegeo_all_to_Canada_all_weight_link_eff_segment_published_u1
                when substr(series2, 1, 1) = 'r' and r4.samegeo_all_to_Canada_all_weight_link_eff_segment_published_u1 is null     then r5.samegeo_all_to_Canada_all_weight_link_eff_segment_rebased_u1
                end as samegeo_all_to_Canada_all_weight_link_eff_segment,
                                                                                            
           case when substr(series2, 1, 1) = 's' and r2.samegeo_all_index_pub_t    is not null then r2.samegeo_all_index_pub_t 
                when substr(series2, 1, 1) = 's' and r2.samegeo_all_index_pub_t    is null     then r3.samegeo_all_index_rebased_t
                when substr(series2, 1, 1) = 'r' and r4.samegeo_all_index_pub_t_u1 is not null then r4.samegeo_all_index_pub_t_u1
                when substr(series2, 1, 1) = 'r' and r4.samegeo_all_index_pub_t_u1 is null     then r5.samegeo_all_index_rebased_t_u1
                end as samegeo_all_index_t

    from
          (select series2,
  					           rebase_type,
                  series_type,
                  geography_code,
    			           product_code,
                  reference_period,
                  ref_period_seq_190001,
                  link_period_seq_190001,
                  basket_first_period_seq_190001,
                  basket_last_period_seq_190001,
                  geo_prod_to_Canada_all_weight_link_new_segment,
    			           geo_prod_to_Canada_all_weight_link_eff_segment,
    			           index_t                                            as geo_prod_index_t
          
           from dfContComp) l
        
    left join
          (select reference_period,
                  geo_prod_to_Canada_all_weight_link_new_segment     as Canada_all_to_Canada_all_weight_link_new_segment,
    			           geo_prod_to_Canada_all_weight_link_eff_segment     as Canada_all_to_Canada_all_weight_link_eff_segment,
    			           index_t                                            as Canada_all_index_t

           from   dfContComp
        
           where series_type = 'Canada, all prod'
             and rebase_type = 'published') r
    on  l.reference_period = r.reference_period
    
    left join
          (select geography_code,
                  reference_period,
                  geo_prod_to_Canada_all_weight_link_new_segment     as samegeo_all_to_Canada_all_weight_link_new_segment_published,
                  geo_prod_to_Canada_all_weight_link_eff_segment     as samegeo_all_to_Canada_all_weight_link_eff_segment_published,
                  index_t                                            as samegeo_all_index_pub_t
               
           from   dfContComp
        
           where series_type in ('sel pub geo, all prod', 'spagg geo, all prod', 'Canada ex spagg geo, all ex spagg prod', 'Canada, all prod')
             and rebase_type = 'published') r2
    on  l.reference_period = r2.reference_period
    and l.geography_code   = r2.geography_code

    left join
          (select geography_code,
                  reference_period,
                  geo_prod_to_Canada_all_weight_link_new_segment     as samegeo_all_to_Canada_all_weight_link_new_segment_rebased,
                  geo_prod_to_Canada_all_weight_link_eff_segment     as samegeo_all_to_Canada_all_weight_link_eff_segment_rebased,
                  index_t                                            as samegeo_all_index_rebased_t
               
           from   dfContComp
        
           where series_type in ('sel pub geo, all prod', 'spagg geo, all prod', 'Canada ex spagg geo, all ex spagg prod', 'Canada, all prod')
             and rebase_type = 'rebased') r3
    on  l.reference_period = r3.reference_period
    and l.geography_code   = r3.geography_code

    left join
          (select reference_period,
                  geo_prod_to_Canada_all_weight_link_new_segment     as samegeo_all_to_Canada_all_weight_link_new_segment_published_u1,
    			           geo_prod_to_Canada_all_weight_link_eff_segment     as samegeo_all_to_Canada_all_weight_link_eff_segment_published_u1,
    			           index_t                                            as samegeo_all_index_pub_t_u1

           from   dfContComp
        
           where series_type = 'spagg geo, all prod'
             and rebase_type = 'published') r4
    on  l.reference_period = r4.reference_period

    left join
          (select reference_period,
                  geo_prod_to_Canada_all_weight_link_new_segment     as samegeo_all_to_Canada_all_weight_link_new_segment_rebased_u1,
    			           geo_prod_to_Canada_all_weight_link_eff_segment     as samegeo_all_to_Canada_all_weight_link_eff_segment_rebased_u1,
    			           index_t                                            as samegeo_all_index_rebased_t_u1

           from   dfContComp
        
           where series_type = 'spagg geo, all prod'
             and rebase_type = 'rebased') r5
    on  l.reference_period = r5.reference_period

    order by l.geography_code,
           l.product_code,
           l.reference_period")
# df741 <- dfContCompAll[dfContCompAll$reference_period == "2023-12-01", ]
# df741 <- dfContCompAll |> filter(series2 %in% c("s4", "s5")) |> arrange(reference_period)
# df741 <- dfContCompAll |> arrange(reference_period)
   

  # get link period indexes
  dfContCompAll2 <- sqldf::sqldf("
    select l.series2,
  				     l.rebase_type,
           l.series_type,
           l.geography_code,
           l.product_code,
           l.reference_period, 
           l.ref_period_seq_190001, 
           l.link_period_seq_190001, 
           l.basket_first_period_seq_190001, 
           l.basket_last_period_seq_190001, 
           l.geo_prod_to_Canada_all_weight_link_new_segment, 
           l.geo_prod_to_Canada_all_weight_link_eff_segment,
           l.geo_prod_index_t,
           l.Canada_all_to_Canada_all_weight_link_new_segment, 
           l.Canada_all_to_Canada_all_weight_link_eff_segment,
           l.Canada_all_index_t,
           l.samegeo_all_to_Canada_all_weight_link_new_segment, 
           l.samegeo_all_to_Canada_all_weight_link_eff_segment,
           l.samegeo_all_index_t,
           r.geo_prod_link_period_index,
           r.Canada_all_link_period_index,
           r.samegeo_all_link_period_index
  
    from dfContCompAll l
  
    left join
        (select rebase_type,
                series_type,
                geography_code,
                product_code,
                ref_period_seq_190001          as link_period_seq_190001,
                geo_prod_index_t               as geo_prod_link_period_index,
                Canada_all_index_t             as Canada_all_link_period_index,
                samegeo_all_index_t            as samegeo_all_link_period_index
              
         from   dfContCompAll) r
    on  l.rebase_type            = r.rebase_type
    and l.series_type            = r.series_type
    and l.geography_code         = r.geography_code
    and l.product_code           = r.product_code
	  and l.link_period_seq_190001 = r.link_period_seq_190001")
# df757 <- dfContCompAll[dfContCompAll$reference_period == "2022-06-01", ]  
   
  
  # for each period, get previous 12 months and tm12 index 
  dfContYearHist <- sqldf::sqldf("
    select l.*,
           r.year_hist_reference_period,
           r.year_hist_ref_period_seq_190001,
           r.basket_first_period_seq_190001,
           r.basket_last_period_seq_190001,
           r.geo_prod_to_Canada_all_weight_link_new_segment,
           r.geo_prod_to_Canada_all_weight_link_eff_segment,
           r.geo_prod_index_t,
           r.geo_prod_link_period_index,
           r.Canada_all_to_Canada_all_weight_link_new_segment,
           r.Canada_all_to_Canada_all_weight_link_eff_segment,
           r.Canada_all_index_t,
           r.Canada_all_link_period_index,
           r.samegeo_all_to_Canada_all_weight_link_new_segment,
           r.samegeo_all_to_Canada_all_weight_link_eff_segment,
           r.samegeo_all_index_t,
           r.samegeo_all_link_period_index,
           tm12.tm12_period_seq_190001,
           tm12.geo_prod_tm12_index,
           tm12.Canada_all_tm12_index,
           tm12.samegeo_all_tm12_index

    from
         (select series2,
                 rebase_type,
                 series_type,
                 geography_code,
                 product_code,
                 reference_period,
                 ref_period_seq_190001
                 
          from   dfContCompAll2) l
  
    left join
         (select rebase_type,
                 series_type,
                 geography_code,
                 product_code,
                 reference_period                                    as year_hist_reference_period,
                 ref_period_seq_190001                               as year_hist_ref_period_seq_190001,
                 basket_first_period_seq_190001,
                 basket_last_period_seq_190001,
                 geo_prod_to_Canada_all_weight_link_new_segment,
                 geo_prod_to_Canada_all_weight_link_eff_segment,
                 geo_prod_index_t,
                 geo_prod_link_period_index,
                 Canada_all_to_Canada_all_weight_link_new_segment,
                 Canada_all_to_Canada_all_weight_link_eff_segment,
                 Canada_all_index_t,
                 Canada_all_link_period_index,
                 samegeo_all_to_Canada_all_weight_link_new_segment,
                 samegeo_all_to_Canada_all_weight_link_eff_segment,
                 samegeo_all_index_t,
                 samegeo_all_link_period_index

         from   dfContCompAll2) r
    on  l.rebase_type    = r.rebase_type
    and l.series_type    = r.series_type
    and l.geography_code = r.geography_code
    and l.product_code   = r.product_code
    and l.ref_period_seq_190001 - r.year_hist_ref_period_seq_190001 between 0 and 12
    
    left join
         (select rebase_type,
                 series_type,
                 geography_code,
                 product_code,
                 ref_period_seq_190001                as tm12_period_seq_190001,
                 geo_prod_index_t                     as geo_prod_tm12_index,
                 Canada_all_index_t                   as Canada_all_tm12_index,
                 samegeo_all_index_t                  as samegeo_all_tm12_index
              
          from   dfContCompAll2) tm12
    on  l.rebase_type                = tm12.rebase_type
    and l.series_type                = tm12.series_type
    and l.geography_code             = tm12.geography_code
    and l.product_code               = tm12.product_code
	   and l.ref_period_seq_190001 - 12 = tm12.tm12_period_seq_190001
						 
  	order by series2,
  	       rebase_type,
          series_type,
  	       geography_code,
          product_code,
          reference_period,
          year_hist_reference_period")  
# df1037 <- dfContYearHist[dfContYearHist$reference_period == "2023-07-01" & dfContYearHist$series_type == "spagg geo, spagg prod", ]  
# df1037 <- dfContYearHist[dfContYearHist$reference_period == "2023-12-01", ]  
# df1057  <- dfContYearHist |> filter(series2 == "s2") |> arrange(reference_period)

  
  # only operate on months with 12 historical records, and output tm0 contributions to 12-m and 1-m All-items change
  # only keep 12-m cont 12 months after usable period
  dfContYear <- dfContYearHist |>
    group_by(series2, rebase_type, series_type, geography_code, product_code, ref_period_seq_190001) |>
    summarise(count_periods_year_hist = n(), .groups = "drop") |>
  	 as.data.frame() |>
	   left_join(
	     dfContYearHist, 
	     by = c("series2", "rebase_type", "series_type", "geography_code", "product_code", "ref_period_seq_190001")) |>
 	  mutate(t_minus                                        = ref_period_seq_190001 - year_hist_ref_period_seq_190001,
           is_link_type_period                            = ifelse(year_hist_ref_period_seq_190001 == tm12_period_seq_190001 | year_hist_ref_period_seq_190001 == basket_last_period_seq_190001, TRUE, FALSE),
           geo_prod_tm12orlink_index                      = ifelse(basket_first_period_seq_190001 <= tm12_period_seq_190001, geo_prod_tm12_index,    geo_prod_link_period_index),
           geo_prod_to_Canada_all_ptqb                    = geo_prod_to_Canada_all_weight_link_eff_segment * geo_prod_index_t / geo_prod_link_period_index,
           geo_prod_to_Canada_all_ptm12orlinkqb           = geo_prod_to_Canada_all_weight_link_eff_segment * geo_prod_tm12orlink_index / geo_prod_link_period_index,
           Canada_all_tm12orlink_index                    = ifelse(basket_first_period_seq_190001 <= tm12_period_seq_190001, Canada_all_tm12_index,  Canada_all_link_period_index),
	      			 samegeo_all_tm12orlink_index                   = ifelse(basket_first_period_seq_190001 <= tm12_period_seq_190001, samegeo_all_tm12_index, samegeo_all_link_period_index),
           geo_prod_to_Canada_all_contrib_within_segment  = (geo_prod_to_Canada_all_ptqb - geo_prod_to_Canada_all_ptm12orlinkqb) / (Canada_all_tm12orlink_index / Canada_all_link_period_index), 
           geo_prod_to_samegeo_all_contrib_within_segment = (geo_prod_to_Canada_all_ptqb - geo_prod_to_Canada_all_ptm12orlinkqb) / (samegeo_all_to_Canada_all_weight_link_eff_segment / 100) / (samegeo_all_tm12orlink_index / samegeo_all_link_period_index), 
           Canada_all_to_Canada_all_ptqb                  = Canada_all_to_Canada_all_weight_link_eff_segment  * Canada_all_index_t  / Canada_all_tm12orlink_index,
           Canada_all_new_growth                          = ifelse(t_minus == 12, 1, ifelse(is_link_type_period == TRUE, Canada_all_to_Canada_all_ptqb  / Canada_all_to_Canada_all_weight_link_new_segment,  1)),
           samegeo_all_to_Canada_all_ptqb                 = samegeo_all_to_Canada_all_weight_link_eff_segment * samegeo_all_index_t / samegeo_all_tm12orlink_index,
           samegeo_all_new_growth                         = ifelse(t_minus == 12, 1, ifelse(is_link_type_period == TRUE, samegeo_all_to_Canada_all_ptqb / samegeo_all_to_Canada_all_weight_link_new_segment, 1)) ) |>
  	 group_by(series2, rebase_type, series_type, geography_code, product_code, ref_period_seq_190001) |>
  	 mutate(Canada_all_cum_growth                          = cumprod(Canada_all_new_growth),
  	        samegeo_all_cum_growth                         = cumprod(samegeo_all_new_growth)) |>
  	 ungroup() |>
  	 mutate(geo_prod_new_geo_prod_to_Canada_all_contrib    = ifelse(t_minus == 12, 0, ifelse(is_link_type_period == TRUE | t_minus == 0, geo_prod_to_Canada_all_contrib_within_segment  * lag(Canada_all_cum_growth), 0)),
  	        geo_prod_new_geo_prod_to_samegeo_all_contrib   = ifelse(t_minus == 12, 0, ifelse(is_link_type_period == TRUE | t_minus == 0, geo_prod_to_samegeo_all_contrib_within_segment * lag(samegeo_all_cum_growth), 0))) |>
  	 group_by(series2, rebase_type, series_type, geography_code, product_code, ref_period_seq_190001) |>
  	 mutate(geo_prod_cum_geo_prod_to_Canada_all_contrib    = cumsum(geo_prod_new_geo_prod_to_Canada_all_contrib),
  	        geo_prod_cum_geo_prod_to_samegeo_all_contrib   = cumsum(geo_prod_new_geo_prod_to_samegeo_all_contrib)) |>
  	 ungroup() |>
  	 filter(t_minus == 0) |>
  	 arrange(series2, rebase_type, series_type, geography_code, product_code, ref_period_seq_190001) |>
   	mutate(geo_prod_to_Canada_all_cont_12mchg    = geo_prod_cum_geo_prod_to_Canada_all_contrib,
   	       geo_prod_to_samegeo_all_cont_12mchg   = geo_prod_cum_geo_prod_to_samegeo_all_contrib,
           geo_prod_to_Canada_all_cont_12mchg_r2 = fRoundHAFZ(geo_prod_to_Canada_all_cont_12mchg,  2),
           geo_prod_to_samegeo_all_cont_12mchg_r2= fRoundHAFZ(geo_prod_to_samegeo_all_cont_12mchg, 2),
   	       geo_prod_to_Canada_all_cont_12mchg_r  = ifelse(ref_period_seq_190001 < fPeriodSeq190001(cFirstWeightUsablePeriod) + 11 | count_periods_year_hist <=12, NA, geo_prod_to_Canada_all_cont_12mchg_r2),
  	    			 geo_prod_to_samegeo_all_cont_12mchg_r = ifelse(ref_period_seq_190001 < fPeriodSeq190001(cFirstWeightUsablePeriod) + 11 | count_periods_year_hist <=12, NA, geo_prod_to_samegeo_all_cont_12mchg_r2),
           geo_prod_to_Canada_all_ptqbm1qb       = ifelse(rebase_type == lag(rebase_type) & geography_code == lag(geography_code) & product_code == lag(product_code) & ref_period_seq_190001 == lag(ref_period_seq_190001) + 1, geo_prod_to_Canada_all_ptqb * lag(geo_prod_index_t) / geo_prod_index_t, NA),
           Canada_all_index_tm1                  = ifelse(rebase_type == lag(rebase_type) & geography_code == lag(geography_code) & product_code == lag(product_code) & ref_period_seq_190001 == lag(ref_period_seq_190001) + 1, lag(Canada_all_index_t) , NA),
           samegeo_all_index_tm1                 = ifelse(rebase_type == lag(rebase_type) & geography_code == lag(geography_code) & product_code == lag(product_code) & ref_period_seq_190001 == lag(ref_period_seq_190001) + 1, lag(samegeo_all_index_t), NA), 
           geo_prod_to_Canada_all_cont_1mchg     = (geo_prod_to_Canada_all_ptqb - geo_prod_to_Canada_all_ptqbm1qb) / (Canada_all_to_Canada_all_weight_link_eff_segment  * Canada_all_index_tm1  / Canada_all_link_period_index),
           geo_prod_to_samegeo_all_cont_1mchg    = (geo_prod_to_Canada_all_ptqb - geo_prod_to_Canada_all_ptqbm1qb) / (samegeo_all_to_Canada_all_weight_link_eff_segment * samegeo_all_index_tm1 / samegeo_all_link_period_index),
           geo_prod_to_Canada_all_cont_1mchg_r   = fRoundHAFZ(geo_prod_to_Canada_all_cont_1mchg  * 100, 2),
           geo_prod_to_samegeo_all_cont_1mchg_r  = fRoundHAFZ(geo_prod_to_samegeo_all_cont_1mchg * 100, 2) )    
# df934 <- dfContYear[dfContYear$reference_period == "2023-07-01", ]
# df934  <- dfContYear |> filter(series2 == "s4") |> arrange(reference_period)


  
    
  
  
  
  # H: calculate index 12m & 1m change
  # regular and spagg indexes post-2007-04
  dfRegSpaggAllExSpaggIndexWeightAllPer <- dfRegSpaggAllExSpaggIndexWeight |>
	   filter(reference_period >= cFirstIndexDisplayPeriod) |>
	   select(series2, series, rebase_type, series_type, geography_code, geography_en, geography_fr, product_code, product_or_product_group_en,  product_or_product_group_fr, i_base_period, 
		      			reference_period, ref_period_seq_190001, index_t)


  bSpaggCanada <- all(dfSpagg$geography_code == "0")
  
  # get index hist, calculate 12-m and 1-m change, join with cont starting 1995-01
  dfRegSpaggAllExSpaggChgCont <- sqldf::sqldf("
    select t.*,
           tm12.index_tm12,
           tm1.index_tm1,
           c.geo_prod_to_Canada_all_cont_12mchg_r,
           c.geo_prod_to_samegeo_all_cont_12mchg_r,
           c.geo_prod_to_Canada_all_cont_1mchg_r,
           c.geo_prod_to_samegeo_all_cont_1mchg_r
           
    from  
          (select *

           from   dfRegSpaggAllExSpaggIndexWeightAllPer
           
           where reference_period >= (select min(reference_period)
                                      from (select reference_period
                                            from   dfRegSpaggAllExSpaggIndexWeightAllPer
                                            where  index_t > 0
                                            union all
                                            select reference_period
                                            from   dfRegSpaggAllExSpaggIndexWeight
                                            where  geo_prod_to_Canada_all_weight_link > 0) ) ) t

    left join
         (select rebase_type,
                 series_type,
                 geography_code,
                 product_code,
                 ref_period_seq_190001    as ref_period_seq_190001_m12,
                 index_t                  as index_tm12

         from   dfRegSpaggAllExSpaggIndexWeightAllPer) tm12
    on  t.rebase_type                = tm12.rebase_type
    and t.series_type                = tm12.series_type
    and t.geography_code             = tm12.geography_code
    and t.product_code               = tm12.product_code
    and t.ref_period_seq_190001 - 12 = tm12.ref_period_seq_190001_m12

    left join
         (select rebase_type,
                 series_type,
                 geography_code,
                 product_code,
                 ref_period_seq_190001    as ref_period_seq_190001_m1,
                 index_t                  as index_tm1

         from   dfRegSpaggAllExSpaggIndexWeightAllPer) tm1
    on  t.rebase_type               = tm1.rebase_type
    and t.series_type               = tm1.series_type
    and t.geography_code            = tm1.geography_code
    and t.product_code              = tm1.product_code
    and t.ref_period_seq_190001 - 1 = tm1.ref_period_seq_190001_m1

    left join
         (select rebase_type,
                 series_type,
                 geography_code,
                 product_code,
                 ref_period_seq_190001,
                 geo_prod_to_Canada_all_cont_12mchg_r,
                 geo_prod_to_samegeo_all_cont_12mchg_r,
                 geo_prod_to_Canada_all_cont_1mchg_r,
                 geo_prod_to_samegeo_all_cont_1mchg_r
                 
          from   dfContYear) c
    on  t.rebase_type               = c.rebase_type
    and t.series_type               = c.series_type
    and t.geography_code            = c.geography_code
    and t.product_code              = c.product_code
    and t.ref_period_seq_190001     = c.ref_period_seq_190001

  	order by rebase_type,
           series_type,
           geography_code,
           product_code,
           reference_period") |>
  	mutate(index_12mchg_r                        = fRoundHAFZ((index_t / index_tm12 - 1) * 100, 1),
          index_1mchg_r                         = fRoundHAFZ((index_t / index_tm1   - 1) * 100, 1),
  	       geo_prod_to_Canada_all_cont_12mchg_r  = ifelse(series2 == "s4" | (series2 == "s5" & bSpaggCanada == TRUE), fRoundHAFZ((index_t / index_tm12 - 1) * 100, 2), geo_prod_to_Canada_all_cont_12mchg_r),
     				 geo_prod_to_Canada_all_cont_1mchg_r   = ifelse(series2 == "s4" | (series2 == "s5" & bSpaggCanada == TRUE), fRoundHAFZ((index_t / index_tm1 - 1)  * 100, 2), geo_prod_to_Canada_all_cont_1mchg_r),
          geo_prod_to_samegeo_all_cont_12mchg_r = ifelse(series2 == "s5" | (series2 == "s4" & bSpaggCanada == TRUE), fRoundHAFZ((index_t / index_tm12 - 1) * 100, 2), geo_prod_to_samegeo_all_cont_12mchg_r),
          geo_prod_to_samegeo_all_cont_1mchg_r  = ifelse(series2 == "s5" | (series2 == "s4" & bSpaggCanada == TRUE), fRoundHAFZ((index_t / index_tm1 - 1)  * 100, 2), geo_prod_to_samegeo_all_cont_1mchg_r)) |>
  	rename(Canada_cont_12mchg_r         = geo_prod_to_Canada_all_cont_12mchg_r,
          samegeo_cont_12mchg_r        = geo_prod_to_samegeo_all_cont_12mchg_r,
          Canada_cont_1mchg_r          = geo_prod_to_Canada_all_cont_1mchg_r,
          samegeo_cont_1mchg_r         = geo_prod_to_samegeo_all_cont_1mchg_r) |>
  	arrange(series_type, series, geography_code, product_code, rebase_type, ref_period_seq_190001) |>
  	select(series2, series, rebase_type, series_type, geography_code, geography_en, geography_fr, product_code, product_or_product_group_en, product_or_product_group_fr, i_base_period,
  				    reference_period, ref_period_seq_190001, index_t, index_12mchg_r, index_1mchg_r, Canada_cont_12mchg_r, samegeo_cont_12mchg_r, Canada_cont_1mchg_r, samegeo_cont_1mchg_r)
# df1083 <- dfRegSpaggAllExSpaggChgCont |> filter(reference_period == "2023-07-01")
  
 
  

  
  
  

  # I: prepare list containing status message and results dataframe
  # prepare list containing query status and data
  lQueryResult <- list(status_code = NULL, status_text_en = "", status_text_fr = "", dfSpaggComponents = NULL, dfQueryResult = NULL)

  if (!(exists("dfRegSpaggAllExSpaggChgCont") && is.data.frame(get("dfRegSpaggAllExSpaggChgCont")))	| (exists("dfRegSpaggAllExSpaggChgCont") && nrow(dfRegSpaggAllExSpaggChgCont) == 0 )) {
  	 lQueryResult$status_code = 9
  	 lQueryResult$status_text_en = dfTextEnFr[which(dfTextEnFr$variable_name == "DiagnosticNoDataText"), 2]
  	 lQueryResult$status_text_fr = dfTextEnFr[which(dfTextEnFr$variable_name == "DiagnosticNoDataText"), 3]
  } else {
  	
  	# diagnostic 1 for spagg check if in any basket the total weight of custom aggregate selection = 100; if so, return null spagg values for all periods
  	# diagnostic 2 for spagg check if in any basket after 1st weight period there are weights but no indexes or indexes but no weights; if so, warn user
  	dfRegSpaggAllExSpaggChgCont <- dfRegSpaggAllExSpaggChgCont |>
   		mutate(is_single_base_period = ifelse( (  series_type == "spagg geo, spagg prod" | series_type == "spagg geo, all ex spagg prod" | series_type == "Canada ex spagg geo, all ex spagg prod")
  																					  & substr(reference_period, 1, 7) == ifelse(is.null(fvcStartBasePer), "", fvcStartBasePer)
  																					  & substr(reference_period, 1, 7) == ifelse(is.null(fvcEndBasePer), "", fvcEndBasePer), TRUE, FALSE))
  	
   dfRegSpaggAllExSpaggChgContDiagnostic <- sqldf::sqldf(paste0("
  	    select l.*,
  	           d2.periods_missing_index_weight
  	           
        from   dfRegSpaggAllExSpaggChgCont l
  	   
        left join
  	          (select series_type,
                      ref_period_seq_190001,
                      count(*)                            as periods_missing_index_weight
           
              from   dfRegSpaggAllIndexWeight
          
              where  series_type = 'sel spagg geo, sel spagg prod'
                and  reference_period >= (select min(ref_period_seq_190001)
                                          from   dfRegSpaggAllIndexWeight
                                          where  series_type = 'sel spagg geo, sel spagg prod' 
                                            and  reference_period >= ", cFirstWeightEffectivePeriod, "
                                            and  geo_prod_to_Canada_all_weight_link > 0)
                and  (   ( (geo_prod_to_Canada_all_weight_link = 0 or geo_prod_to_Canada_all_weight_link is null) and index_link > 0 and index_t > 0)
                      or (  geo_prod_to_Canada_all_weight_link > 0                                                and (index_link = 0 or index_link is null or index_t = 0 or index_t is null) ) )
              
              group by series_type,
                     ref_period_seq_190001) d2
  	    on  l.series_type    = d2.series_type
  	    and l.ref_period_seq_190001    = d2.ref_period_seq_190001"))  

  	  dfRegSpaggAllExSpaggChgCont <- dfRegSpaggAllExSpaggChgCont |>
  	    select(-is_single_base_period)

     if (nrow(subset(dfRegSpaggAllExSpaggChgContDiagnostic, !is.na(periods_missing_index_weight) )) > 0) {
  	    lQueryResult$status_code = 1
       lQueryResult$status_text_en    = dfTextEnFr[which(dfTextEnFr$variable_name == "DiagnosticSomeDataText"), 2]
  	    lQueryResult$status_text_fr    = dfTextEnFr[which(dfTextEnFr$variable_name == "DiagnosticSomeDataText"), 3]
     } else {
  	    lQueryResult$status_code = 0
       lQueryResult$status_text_en    = dfTextEnFr[which(dfTextEnFr$variable_name == "DiagnosticAllDataText"), 2]
  	    lQueryResult$status_text_fr    = dfTextEnFr[which(dfTextEnFr$variable_name == "DiagnosticAllDataText"), 3]
  	  }
  	  lQueryResult$dfSpaggComponents = dfSpaggComponents
     lQueryResult$dfQueryResult     = dfRegSpaggAllExSpaggChgCont
   }

  return(lQueryResult)
}










#---------------------------------
# 1.2 fPlotTimeSeries() function to prepare graphs
#---------------------------------


fPlotTimeSeries <- function(fvdfData, fvdfSeries, fvviSeries, fvcSelectedStatVarName, fvdfSeriesFormats)	{
# 	message(paste0("fPlotTimeSeries: ", fvcSelectedStatVarName))

  lFont       <- list(family = "'Noto Sans'", size = 9,  weight = "bold")
  lFont2      <- list(family = "'Noto Sans'", size = 12, weight = "bold")
  viSeriesReg <- fvviSeries[which(fvviSeries > 5)]
  fvdfSeries  <- fvdfSeries |> mutate(series = as.character(lapply(strwrap(series, width = 100, simplify= FALSE), paste, collapse = "<br>") ) )
  
  plTimeSeries <- plotly::plot_ly()
  for (i in 1:length(fvviSeries)) {
	 # message(paste0("fPlotTimeSeries: ", i))

  	iSeries                         <- fvviSeries[i]
  	cSeries2                        <- ifelse(iSeries <= 5, paste0("s", iSeries), paste0("r", iSeries - 5))
  	iFormat                         <- ifelse(iSeries <= 5, iSeries, 5 + which(viSeriesReg == iSeries))
  	dfDataFormatted                 <- fvdfData[ , c(1, i + 1)]
  	if (cAppLanguage == "English") {
  		 if (  fvcSelectedStatVarName == "Statistic12mCanadaCont" | fvcSelectedStatVarName == "Statistic12mSameGeoCont"
  			 	  | fvcSelectedStatVarName == "Statistic1mCanadaCont"  | fvcSelectedStatVarName == "Statistic1mSameGeoCont") {
  			  dfDataFormatted$value_formatted <- format(dfDataFormatted[ , 2], decimal.mark = '.', big.mark = ',', scientific = F, nsmall = 2)
  		 } else {
  			 dfDataFormatted$value_formatted <- format(dfDataFormatted[ , 2], decimal.mark = '.', big.mark = ',', scientific = F, nsmall = 1)
  		 }
  	} else {
  	 	if (  fvcSelectedStatVarName == "Statistic12mCanadaCont" | fvcSelectedStatVarName == "Statistic12mSameGeoCont"
  				   | fvcSelectedStatVarName == "Statistic1mCanadaCont"  | fvcSelectedStatVarName == "Statistic1mSameGeoCont") {
  			  dfDataFormatted$value_formatted <- format(dfDataFormatted[ , 2], decimal.mark = ',', big.mark = ' ', scientific = F, nsmall = 2)
  		 } else {
  			  dfDataFormatted$value_formatted <- format(dfDataFormatted[ , 2], decimal.mark = ',', big.mark = ' ', scientific = F, nsmall = 1)
  		 }
  	}
  	
   plTimeSeries <- plotly::add_trace(
     plTimeSeries,
     type      = "scatter", 
     mode      = "lines", 
     x         = fvdfData[["reference_period"]], 
     y         = fvdfData[[cSeries2]], 
#    													hovertext = paste0(fvdfSeries[iSeries, 3], "\n", fvdfData[["reference_period"]], ": ", fvdfData[[paste0("s", as.character(iSeries))]]),
    	hovertext = paste0(fvdfSeries[iSeries, 3], "\n", dfDataFormatted$reference_period, ": ", dfDataFormatted$value_formatted),
     hoverinfo = paste0("text"),
     name      = fvdfSeries[iSeries, 3], 
     color     = I(fvdfSeriesFormats[iFormat, 2]), 
     linetype  = I(fvdfSeriesFormats[iFormat, 3]))
  }
  plTimeSeries <- plTimeSeries |>
   	plotly::layout(
   	  xaxis      = list(title = "", font = lFont, type = "date", tickformat = "%Y-%m", fixedrange = TRUE, rangeslider = list(type = "date")),
 				 yaxis      = list(title = "", font = lFont, fixedrange = TRUE), 
 				 showlegend = TRUE, 
 				 legend     = list(orientation = "h", xanchor = "left", x = 0, y = -0.4, font = lFont2))
  if (cAppLanguage == "English") {
    plTimeSeries <- plTimeSeries |>
      plotly::config(locale = 'ca', displayModeBar = FALSE)
  } else {
   	plTimeSeries <- plTimeSeries |>
   	  plotly::config(locale = 'fr', displayModeBar = FALSE)
  }

  return(plTimeSeries)
}



#fvdfData <- dfQueryResultRenamedUnformattedSelectedT; fvdfSeries <- dfAllSeries; fvviSeries <- viSeries; fvcSelectedStatVarName <- cSelectedStatVarName
#fPlotTimeSeries(fvdfData, fvdfSeries, fvviSeries, fvcSelectedStatVarName)










#---------------------------------
# 1.3 fGetDisplaySeries() function to prepare available series
#---------------------------------


fGetDisplaySeries <- function(fviNumComp, fviN, fvrvSavedGeoProd, fvdfSeriesSpagg) {
  dfSavedGeoProd <- data.frame(cbind(fvrvSavedGeoProd$sel_geo, fvrvSavedGeoProd$sel_prod)) |>
	 	 mutate(comp_num = row_number())
  
  iSavedRows <- nrow(dfSavedGeoProd)
# message(paste0("fGetCandidateSeries nrow(dfSelSeries)="), iSavedRows)

  if (iSavedRows == 0) {
#  	message(paste0("fGetCandidateSeries nrow(dfSelSeries) == 0"))
  	# if saved series = 0, all spagg series are candidates
    dfDisplaySeries <- fvdfSeriesSpagg |>
  		  select(indented_geography, indented_product) |>
  		  rename(geo  = indented_geography,
  					      prod = indented_product) |>
		    mutate(comp_num = fviNumComp)
    
  } else if (iSavedRows > 0){
#  	message(paste0("fGetCandidateSeries nrow(dfSelSeries) > 0"), typeof(dfSavedGeoProd), dim(dfSavedGeoProd))
  	 names(dfSavedGeoProd)[1:2] <- c('geo', 'prod')

  	# if saved series > 0, first get geo and prod codes
 	  dfSelSeries <- 
 	    dfSavedGeoProd |>
  	   left_join(
  	     fvdfSeriesSpagg |>
  							 mutate(prod_code = ifelse(i_dim2_position == 'c0', 'c', i_dim2_position))  |>
  							 select(indented_geography, indented_product, where_to_code, prod_code) |>
  						  rename(geo      = indented_geography,
  						  			    prod     = indented_product,
  						  			    geo_code = where_to_code),
  						by = c("geo", "prod"))

  	# then find series which are not candidates, then select all not in this set
	  dfCandidateSeries <- sqldf::sqldf("
	  select indented_geography     as geo,
		 			    indented_product       as prod

  	from   fvdfSeriesSpagg
  	
  	where  i_coordinate not in 
  	      (select i_coordinate
  	      
  	       from   dfSelSeries s,
  	       
  	              (select i_coordinate,
  	                      where_to_code                                                        as avail_geo_code,
  	                      indented_geography                                                   as avail_geo,
  	                      case when i_dim2_position = 'c0' then 'c' else i_dim2_position end   as avail_prod_code,
		 			                   indented_product                                                     as avail_prod

         	         from  fvdfSeriesSpagg) a
  	
           where (        s.geo        = 'Canada' 
 	                and (   a.avail_geo != 'Canada' 
 	                     or s.prod_code  = substr(a.avail_prod_code, 1, length(s.prod_code))
 	                     or substr(s.prod_code, 1, length(a.avail_prod_code)) = a.avail_prod_code))
 	            or (     s.geo                != 'Canada' 
 	                and (a.avail_geo           = 'Canada' 
 	                       or (    s.geo_code  = a.avail_geo_code
 	                           and (   s.prod_code = substr(a.avail_prod_code, 1, length(s.prod_code)) 
 																	or substr(s.prod_code, 1, length(a.avail_prod_code)) = a.avail_prod_code))))) ") |>
		mutate(comp_num = fviNumComp)
  	

    if (fviNumComp == iSavedRows) {
      dfDisplaySeries <- dfSelSeries
# message(paste0("fGetCandidateSeries nrow(dfSavedGeoProd) == fviNumComp & nrow(dfSavedGeoProd) > 0"))
    } else if (fviNumComp > iSavedRows) {
      dfDisplaySeries <- rbind(dfSavedGeoProd, dfCandidateSeries)
# message(paste0("fGetCandidateSeries nrow(dfSavedGeoProd) < fviNumComp & nrow(dfSavedGeoProd) > 0"))
    }
  }
	return(dfDisplaySeries)
}








#---------------------------------
# 1.4 fMessage() function to display messages
#---------------------------------


fMessage <- function(fvcBlock, fvcMessage) {
  if      (length(dfMessages[dfMessages$block == fvcBlock, "show_message"]) == 0) {return(message(paste0(fvcBlock, " has no message")))}
  else if (dfMessages[dfMessages$block == fvcBlock, "show_message"] == 1)         {return(message(paste0(fvcBlock, ": ", fvcMessage)))}
	 else                                                                            {return("")}       
}
#fMessage('Restart before', "abcd")









#---------------------------------
# 2 Read input data and instantiate global objects
#---------------------------------


# get English and French text
dfTextEnFr <- as.data.frame(readxl::read_xlsx("data-raw/Data_for_R_Shiny.xlsx", sheet="En & Fr text"))


# get basket periods
dfBasket   <- as.data.frame(readxl::read_xlsx("data-raw/Data_for_R_Shiny.xlsx", sheet="basket")) |>
	 mutate(basket_ref_date         = weight_reference_period,
	        weight_reference_period = paste0(as.character(weight_reference_period),"-01-01"),
         link_period             = as.character(link_period),
         first_period            = as.character(first_period),
         last_period             = as.character(last_period) )


# get series
dfSeriesReg <- as.data.frame(readxl::read_xlsx("data-raw/Data_for_R_Shiny.xlsx", sheet="map vectors across tables")) |>
  mutate(table_18100004_vector         = as.integer(substr(i_vector,              2, nchar(i_vector))),
         table_18100007_samegeo_vector = as.integer(substr(w_link_samegeo_vector, 2, nchar(w_link_samegeo_vector))),
         table_18100007_Canada_vector  = as.integer(substr(w_link_Canada_vector,  2, nchar(w_link_Canada_vector))),
         i_first_ref_date              =            substr(i_first_ref_date,      1, 7),
         weight_version                = ifelse(substr(i_dim2_position, 1, 1) == "s" & w_first_ref_date == 2001, "revised", "original"),
  			    language                      = cAppLanguage) |>
	 left_join(
	   dfBasket |>
			   mutate(w_first_period = substr(first_period, 1, 7)) |>
			   select(basket_ref_date, weight_version, w_first_period),
			 by = join_by("weight_version", "w_first_ref_date" == "basket_ref_date")) |>
	 arrange(where_to_code, i_dim2_position)


dfSeriesSpagg <- dfSeriesReg |>
  filter(substr(i_dim2_position, 1, 1) == "c" & (w_link_samegeo_vector != "" | w_link_Canada_vector != "") & !(where_to_code == "0" & i_dim2_position == "c0")) |>
  mutate(geography          = ifelse(language == "English", geography_en,                geography_fr),
         product            = ifelse(language == "English", product_or_product_group_en, product_or_product_group_fr),
         indent_geo         = ifelse(where_to_code == "0", 0, 1) * 2,
  			    indent_prod        = ifelse(i_dim2_position == "c0", 0, ifelse(nchar(i_dim2_position) == 2, 1, (nchar(i_dim2_position) - 1) / 2 + 1)),
  			    indented_geography = paste0(strrep(intToUtf8(160), indent_geo  * 2), geography),
  			    indented_product   = paste0(strrep(intToUtf8(160), indent_prod * 2), product),
  			    w_first_period     = substr(fRefDate(fPeriodSeq190001(paste0(w_first_period, "-01")) - 1), 1, 7),
    	    base_period        = substr(i_base_period, 1, nchar(i_base_period) - 4),
    		   nchar_base_period  = nchar(base_period),
    		   start_base_period  = ifelse(nchar_base_period == 4, paste0(base_period, "-01"),
    				   													                          ifelse(nchar_base_period == 6, paste0(substr(base_period, 1, 4), "-", substr(base_period, 5, 6)),
    				 	  												                                                         paste0(substr(base_period, 1, 4), "-", substr(base_period, 5, 6)))),
    		   end_base_period    = ifelse(nchar_base_period == 4, paste0(base_period, "-12"),
    				 													                            ifelse(nchar_base_period == 6, start_base_period,
    				 													                                                           paste0(substr(base_period, 8, 11), "-", substr(base_period, 12, 13)))),
   	     as_component_start_base_period = pmax(start_base_period, w_first_period),
    	    as_component_end_base_period   = pmax(end_base_period,   w_first_period)) |>
	 select(-c(base_period, nchar_base_period, start_base_period, end_base_period)) |>
	 arrange(where_to_code, i_dim2_position)


# get popular aggregates
dfPopularAggDefn <- as.data.frame(readxl::read_xlsx("data-raw/Data_for_R_Shiny.xlsx", sheet="popular aggregate defn")) |>
	 filter(display == "y") |>
  mutate(language            = cAppLanguage,
  			    aggregate_geography = ifelse(language == "English", aggregate_geography_en, aggregate_geography_fr),
  			    aggregate_product   = ifelse(language == "English", aggregate_product_en,   aggregate_product_fr),
         indented_geography  = paste0(strrep(intToUtf8(160), indent_geo  * 2), aggregate_geography),
         indented_product    = paste0(strrep(intToUtf8(160), indent_prod * 2), aggregate_product)) |>
  arrange(aggregate_sort_position)


# get popular aggregate components
dfPopularAggComp <- as.data.frame(readxl::read_xlsx("data-raw/Data_for_R_Shiny.xlsx", sheet="popular aggregate components")) |>
	 select(aggregate_id, where_to_code, i_dim2_position)


# set graph colours and linetypes
dfSeriesFormats <- data.frame(num       = 1:12,
															colour    = c("#0000FF", "#000000", "#000099", "#CC0000", "#9900FF", "#990000", "#0066CC", "#990099", "#003366", "#660033", "#006600", "#CC00CC"),
															linetype  = c("solid",   "dotted",  "dashdot", "solid",  "dotted",    "dotted",  "dotted",  "dashed", "dashed",  "solid",   "dashdot",  "dotted"),
															symbol    = c("circle",  "diamond", "square",  "circle",  "diamond",  "square", "circle",  "diamond", "square",  "circle",   "diamond", "square"))


# create dataframe of messages to help code development
dfMessages <- data.frame(block = "renderUI", show_message = 0)
dfMessages <- rbind(dfMessages, 
										c('renderUI function(i)', 0),
										c('renderUI function(i)2', 0),
										c("Initialize popular aggregate", 0), 
										c("Initialize popular aggregate2", 0), 
										c('Initialize Prod', 0),
										c('Initialize Prod2', 0),
										c('Enable / Disable buttons', 0),
										c('Enable / Disable buttons2', 0),
										c('Enable / Disable buttons before', 0),
										c('Enable / Disable buttons after', 0),
										c('Apply popular aggregate before', 0),
										c('Apply popular aggregate after', 0),
										c('Remove', 0),
										c('Remove > for > iCompID > scenarios 2, 3, 5', 0),
										c('Remove > for > iCompID > scenario 4', 0),
										c('Remove > for > iCompID > scenario 1', 0),
						    c('Remove > for > local', 0),
					     c('Remove > for > local > observeEvent', 0),
					     c('Remove > for > local > observeEvent > iLocal == iCompID > before', 0),
					     c('Remove > for > local > observeEvent > iLocal == iCompID > to remove', 0),
					     c('Remove > for > local > observeEvent > if scenario 1', 0),
					     c('Remove > for > local > observeEvent > if scenario 2', 0),
				  	   c('Remove > for > local > observeEvent > if scenario 3', 0),
				  	   c('Remove > for > local > observeEvent > if scenario 4', 0),
				  	   c('Remove > for > local > observeEvent > if scenario 5', 0),
				  	   c('Remove > for > local > observeEvent > iLocal == iCompID > after', 0),
				  	   c('Remove > for > local > observeEvent > iLocal == iCompID > after2', 0),
				  	   c('Add before', 0),
				  	   c('Add at start or after restart', 0),
				  	   c('Add when 2 or more displayed & count disp > count saved', 0),
				  	   c('Add after go or after removing last component', 0),
				  	   c('Add iCandidatesCount', 0),
				  	   c('Add after', 0),
				  	   c('Run before', 0),
				  	   c('Run after', 0),
				  	   c('Restart before', 0),
				  	   c('Restart after', 0) )
#dfMessages <- dfMessages |> mutate(show_message = "n")


# Global constants
iFirstIndexYear             <- 2004
iFirstIndexPeriod           <- 07
cFirstIndexYearMonth        <- paste0(as.character(iFirstIndexYear), substr(as.character(100 + iFirstIndexPeriod), 2, 3))
cFirstIndexDisplayPeriod    <- "2007-04-01"
cDefaultStartBasePeriod     <- "2007-04-01"
cDefaultEndBasePeriod       <- "2007-04-01"
cDefaultBasePeriod          <- "200704=100"
cFirstWeightRefPeriod       <- "2001-01-01"
cFirstWeightEffectivePeriod <- "2004-07-01"
cFirstWeightUsablePeriod    <- "2007-05-01"
cMaxWeightRefPeriod         <- max(dfBasket$weight_reference_period)
iDefaultMaxSeriesCount      <- 8 # default number of series to plot










#---------------------------------
# 3 Shiny UI variables and functions
#---------------------------------


# UI constants
cGraphHeight <- "500px"
cGraphWidth  <- "100%"
cTableHeight <- "700px"
cTableWidth  <- "100%"
cTabHeight   <- ifelse(cAppLanguage == "French",  "160px", "140px")
cTabFontSize <- ifelse(cAppLanguage == "French",  "70%", "75%")
cButtonStyle <- ifelse(cAppLanguage == "French",  "btnActionFr", "btnAction")


# UI functions
# Statistic panel
fUIStatPanel <- function(fvcStatName, fviStatNum) {
  shiny::tabPanel(
           br(),
           title = fGetEnFrText(paste0("Statistic", fvcStatName)),
           fluid = TRUE,
           withTags({div(a(href = paste0("#c", fviStatNum), class = "skip-link", paste0(fGetEnFrText("AccessibilitySkipPart1Text"), " ", fGetEnFrText(paste0("Statistic", fvcStatName)), " ", fGetEnFrText("AccessibilitySkipPart2Text"))))}),
           shiny::fluidRow(shiny::column(12, align = "left",
             h3(HTML(fGetEnFrText("GraphHeaderText"), paste0(" ", fviStatNum, " <br>"))),
             div(style="display: inline-block; font-weight: bold !important;", fGetEnFrText(paste0("Statistic", fvcStatName))),
             div(style="display: inline-block;", withTags({div(a(id=paste0("inToolTip", fvcStatName), href = "#centred-popup", style="text-decoration: none", `aria-controls`="centred-popup", class="wb-lbx wb-init wb-lbx-inited", role="button", `aria-label`="Information about statistic",
		                                span(style="margin-top: 0.5em; color: #26374a;", class="glyphicon glyphicon-info-sign", `aria-hidden`="true") )) }) ))),
        	  br(),
           shiny::fluidRow(shiny::column(12, tags$div(id = "plot-container", `aria-hidden` = "true", plotly::plotlyOutput(paste0("outPlotly", fvcStatName), height = cGraphHeight, width = cGraphWidth)))),
           br(),
           shiny::fluidRow(shiny::column(6, shiny::downloadButton(paste0("outPlotlyDownload", fvcStatName),    label = fGetEnFrText("SaveGraphButtonLabel"),     icon = NULL), align = "center"),
                           shiny::column(6, shiny::downloadButton(paste0("outReactableDownload", fvcStatName), label = fGetEnFrText("DownloadTableButtonLabel"), icon = NULL), align = "center") ),
           withTags({div(id=paste0("c", fviStatNum), class = "mrgn-tp-md mrgn-bttm-md", h3(HTML(fGetEnFrText("TableHeaderText"), paste0(" ", fviStatNum, " <br>"), fGetEnFrText(paste0("Statistic", fvcStatName)))))}),
           shiny::fluidRow(shiny::column(12, reactable::reactableOutput(paste0("outReactable", fvcStatName), height = cTableHeight, width =  cTableWidth)))
  )
}







ui <- function(request) {
shinydashboard::dashboardPage(
shinydashboard::dashboardHeader(disable = TRUE),
shinydashboard::dashboardSidebar(disable = TRUE),
shinydashboard::dashboardBody(
  fluid = TRUE,
  shinyjs::useShinyjs(),
  waiter::use_waiter(),

  # needed for accessibility, see "Power BI and R for dissemination to the public-eng.docx", p. 13
  withTags({link(rel="stylesheet", type="text/css", href="https://www150.statcan.gc.ca/wet-boew4b/css/theme.min.css")}),


  shiny::tags$head(
		  shiny::tags$style(HTML(paste0("
      body                               {max-width: 1140px; font-size: 140%;}
      hr                                 {margin-left: 3px !important; margin-right: 3px !important; margin-top: 3px !important; margin-bottom: 3px !important; border: 1px solid #dcdee1 !important;}
      span                               {font-size: 90%;}
      h1                                 {font-family: 'Noto Sans' !important; font-size: 150% !important; margin: 0px 0px !important; text-align: center !important; font-weight: bold !important; border-bottom: 0px !important}
      h2                                 {font-family: 'Noto Sans' !important; font-size: 120% !important; margin: 0px 0px !important; font-weight: bold !important;}
      h3                                 {font-family: 'Noto Sans' !important; font-size: 100% !important; margin: 0px 0px !important; font-weight: bold !important;}
 		   .content-wrapper                   {background-color: white;}
      .well                              {margin: 3px 3px !important; padding: 0px 10px !important; background-color: #f5f5f5;}
      .btn                               {min-width: 25px !important; width: auto !important; min-height: 25px !important; height: auto !important; margin: 0px 0px; padding: 1px 1px !important; white-space: normal; border: 1px solid #dcdee1; background-color: #eaebed !important;}
      .btnAction                         {position: absolute !important; right: 10px !important; min-width: 60px !important; width: 60px !important; min-height: 25px !important; height: auto !important; font-size: 70% !important; margin: 0px 0px; padding: 1px 1px !important; white-space: normal; border: 1px solid #dcdee1; }
      .btnActionFr                       {position: absolute !important; right: 10px !important; min-width: 60px !important; width: 60px !important; min-height: 25px !important; height: auto !important; font-size: 55% !important; margin: 0px 0px; padding: 1px 1px !important; white-space: normal; border: 1px solid #dcdee1; }
      .btnToggle                         {position: absolute !important; right: 10px !important; min-width: 60px !important; width: 60px !important; min-height: 25px !important; height: auto !important; font-size: 75% !important; margin: 0px 0px; padding: 1px 1px !important; white-space: normal; border: 1px solid #dcdee1; }
      .modal                             {text-align: left; width: 90%; font-size: 90%;}
      .modal-header                      {background-color: #26374a}
      .modal-title                       {color: #FFFFFF !important;}
      .form-group, shiny-input-container {margin: 0px 0px !important;}
      .form-control, shiny-bound-input   {margin: 0px 0px !important; font-size: 90% !important;}
      .box                               {margin: 3px 0px !important; border-top: 0px !important;}
      .box-header                        {padding-top: 3px !important; padding-bottom: 10px !important; padding-left: 15px !important; padding-right: 15px !important; background-color: #f5f5f5;}
      .box-body                          {padding: 0px 15px !important; background-color: #f5f5f5;}
      .box-title                         {width: 100% !important;}
      .box-tools.pull-right              {display: none !important; }
      .control-label                     {font-size: 100% !important; font-weight: normal;}
      .radio                             {font-size: 90% !important; margin-top: 5px !important; margin-bottom: 0px !important; }
      .checkbox                          {font-size: 90% !important; }
      .shiny-input-select                {padding: 3px 3px !important; font-size: 80% !important; min-height: 20px !important; height: auto !important;  white-space: wrap}
      .shiny-input-select-dropdown       {width: 100% !important;}
      .nav>li                            {text-align: center; padding: 0px 0px !important;}
      .nav-tabs > li > a                 {border: 1px solid #dcdee1 !important; display: flex !important; align-items: center !important; justify-content: center !important; white-space: normal; text-align: center;}
      .nav>li>a                          {padding: 0px 0px !important; height: ", cTabHeight, "; width: 83px; font-size: ", cTabFontSize, ";}
      #buttons                           {display: flex; align-items: center; justify-content: center;}
      #toggleBox                         {position: absolute; right: 5px !important; top: 8px; z-index: 2; padding-left: 20px; border: 1px solid #dcdee1; background-color: #eaebed;}
      #outTextQueryStatus                {background-color: white; font-family: 'Noto Sans'; white-space: pre-wrap; word-break: keep-all; max-height: 200px; font-size: 80%;}
      .skip-link                         {position: absolute; left: -999px; top: auto; width: 1px; height: 1px; overflow: hidden;}
		  "))),


    # Hide arrow in Chrome, Safari, Edge; Optional: remove extra padding caused by arrow space; Hide arrow in Firefox;
    shiny::tags$style(HTML("
      select              {-webkit-appearance: none; -moz-appearance: none; appearance: none; background-image: none !important;}
      select.form-control {padding-right: 0.75rem; }
      select::-ms-expand  {display: none;}
    "))
   ),


		  # set focus on a specific element inside the modal when it's shown, and delay to allow modal to fully open
		  shiny::tags$script(HTML("$(document).on('shown.bs.modal', '.modal', function () {
        setTimeout(function() {
          const focusTarget = document.getElementById('modal-focus-start');
          if (focusTarget) {
            focusTarget.focus();
          }
        }, 100);
      });")),


		# create expand-collapse button for box()
		shiny::tags$script(paste0(HTML("$(document).on('shiny:connected', function() {
      $(document).on('click', '#toggleBox', function(e) {
        var $box = $(this).closest('.box');
        if ($box.hasClass('collapsed-box')) {
          $box.removeClass('collapsed-box');
          $box.find('#toggleLabel').text('", fGetEnFrText("BoxCollapseLabel"), "');
          $(this).attr('aria-expanded', 'true');
        } else {
          $box.addClass('collapsed-box');
          $box.find('#toggleLabel').text('", fGetEnFrText("BoxExpandLabel"), "');
          $(this).attr('aria-expanded', 'false');
        }
        e.preventDefault();
        e.stopPropagation();
      });
      // Update label of button when Collapsed/Expanded
      $('.box').on('expanded.lte.boxwidget collapsed.lte.boxwidget', function() {
        var $box = $(this);
        var isCollapsed = $box.hasClass('collapsed-box');
        $box.find('#toggleLabel').text(isCollapsed ? '", fGetEnFrText("BoxExpandLabel"), "' : '", fGetEnFrText("BoxCollapseLabel"), "');
        $box.find('#toggleBox').attr('aria-expanded', (!isCollapsed).toString());
      });
    });"))),


		# set focus on outTextQueryStatus when inBtnRun clicked
		shiny::tags$script(HTML("$(document).on('shiny:inputchanged', function(event) {
    if      (event.name === 'inBtnRestartComp')                      {$('#inSelPopularAggGeo').focus();}
    else if (event.name === 'inBtnRun')                              {$('#outTextQueryStatus').focus();}
		  else if (event.name === 'inBtnCloseToolTipCustAggSeries')        {$('#inToolTipCustAggSeries').focus();}
		  else if (event.name === 'inBtnCloseToolTipCustCompSeries')       {$('#inToolTipCustCompSeries').focus();}
		  else if (event.name === 'inBtnCloseToolTipSetBasePeriod')        {$('#inToolTipSetBasePeriod').focus();}
		  else if (event.name === 'inBtnCloseToolTipRebase')               {$('#inToolTipRebase').focus();}
  	 else if (event.name === 'inBtnCloseToolTipDispCompSeries')       {$('#inToolTipDispCompSeries').focus();}
		  else if (event.name === 'inBtnCloseToolTip12mChg')               {$('#inToolTip12mChg').focus();}
	   else if (event.name === 'inBtnCloseToolTip1mChg')                {$('#inToolTip1mChg').focus();}
		  else if (event.name === 'inBtnCloseToolTipIndex')                {$('#inToolTipIndex').focus();}
	   else if (event.name === 'inBtnCloseToolTip12mCanadaCont')        {$('#inToolTip12mCanadaCont').focus();}
		  else if (event.name === 'inBtnCloseToolTip12mSameGeoCont')       {$('#inToolTip12mSameGeoCont').focus();}
	   else if (event.name === 'inBtnCloseToolTip1mCanadaCont')         {$('#inToolTip1mCanadaCont').focus();}
		  else if (event.name === 'inBtnCloseToolTip1mSameGeoCont')        {$('#inToolTip1mSameGeoCont').focus();}
	   else if (event.name === 'inBtnCloseModalCustCompNoneAvailable')  {$('#inToolTipCustCompSeries').focus();}
		});")),

  # Listener function 'focusNewComp' to focus tab order on new component
  shiny::tags$script(HTML("
  Shiny.addCustomMessageHandler('focusNewComp', function(id) {
    console.log('focusNewComp triggered for id:', id);

    setTimeout(function() {
      var el = document.getElementById(id);
      if (!el) {
        console.warn('Element with ID ' + id + ' not found.');
        return;
      }
      var $select = $('#' + id);
      if ($select.length > 0 && $select[0].select) {
        console.log('Attempting to focus select wrapper');
        var wrapper = $select[0].select.$wrapper;
        if (wrapper && wrapper[0]) {
          // Ensure wrapper is focusable
          wrapper.attr('tabindex', '-1');
          wrapper[0].focus();
          console.log('select wrapper focused');
        } else {
          console.warn('select wrapper not found');
        }
      } else {
        console.log('Focusing plain element:', id);
        el.focus();
      }
    }, 400);
    });
  ")),

 
  shiny::mainPanel(width = '100%',
    shiny::fluidRow(h1(fGetEnFrText("TitleText"))),

    shiny::fluidRow(
    	# Left col
    	shiny::column(5, style = "padding-right: 2px;", shiny::fluidRow(shiny::wellPanel(id = "panelTopLeft",

       # Step 1 Aggregate series calc type
  	    shiny::fluidRow(shiny::column(12,
		      shiny::fluidRow(shiny::column(12, align = "left",
		        div(style="display: inline-block", h2(fGetEnFrText("CustAggStepText"))),
		        div(style="display: inline-block", withTags({div(a(id="inToolTipCustAggSeries", href = "#centred-popup", style="text-decoration: none", `aria-controls`="centred-popup", class="wb-lbx wb-init wb-lbx-inited", role="button", `aria-label`="Information for step 1",
		                                 span(style="margin-top: 0.5em; color: #26374a;", class="glyphicon glyphicon-info-sign", `aria-hidden`="true") )) }) ))),
		      shiny::fluidRow(shiny::column(12, div(style = "height: 10px;"))),
		      shiny::fluidRow(
   		      shiny::column(12, shiny::radioButtons(inputId = "inRadioCustAggSeries", label = h3(fGetEnFrText("CustAggRadioButtonLabel")),
   					    										  choiceNames = list(fGetEnFrText("CustAggSeriesSum"), fGetEnFrText("CustAggSeriesCdaAllExSel"), fGetEnFrText("CustAggSeriesBothSel")),
   					    										  choiceValues = list(1, 2, 3), selected = 1, inline = FALSE, width = "100%"))))), #left r1 aggregate series selections
        shiny::fluidRow(shiny::column(12, div(style = "height: 20px;"))),

        # Step 2 Series selections
		      shiny::fluidRow(shiny::column(12, align = "left",
		        div(style="display: inline-block", h2(fGetEnFrText("CustCompStepText"))),
		        div(style="display: inline-block", withTags({div(a(id="inToolTipCustCompSeries", href = "#centred-popup", style="text-decoration: none", `aria-controls`="centred-popup", class="wb-lbx wb-init wb-lbx-inited", role="button", `aria-label`="Information for step 2",
                                         span(style="margin-top: 0.5em; color: #26374a;", class="glyphicon glyphicon-info-sign", `aria-hidden`="true") )) }) ))),
        shiny::fluidRow(shiny::column(12, div(style = "height: 10px;"))),

        # Popular aggregates
		      shiny::fluidRow(
   		      shiny::column(12, h3(fGetEnFrText("CustAggGroupStepText"), align = "left"))),
        shiny::fluidRow(shiny::column(12, div(style = "height: 10px;"))),

        shiny::fluidRow(
          shiny::column(4, style = "                    padding-right: 2px;", 
            div(shiny::selectInput("inSelPopularAggGeo", width = "100%",  
                  label = div(HTML(fGetEnFrText("CustAggGroupGeoText")),  style = "font-size: 80%; font-weight: normal;"),
                  choices = c("", unique(dfPopularAggDefn$indented_geography)), selected = "", multiple = FALSE, selectize = FALSE), 
                  style = "margin-bottom: 0px; height: 100px !important;")),
          shiny::column(6, style = "padding-left: 2px; padding-right: 0px;", 
            div(shiny::selectInput("inSelPopularAggProd", width = "100%", 
                  label = div(HTML(fGetEnFrText("CustAggGroupProdText")), style = "font-size: 80%; font-weight: normal;"),
                  choices = c("", dfPopularAggDefn$indented_product),   selected = "", multiple = FALSE, selectize = FALSE), 
                  style = "margin-bottom: 0px; height: 100px !important;")),
          shiny::column(2, shiny::actionButton("inBtnApplyPopularAgg", label = fGetEnFrText("ApplyPopularAggButtonLabel"), class = "btnAction"))),
        br(),
    	 
        # Component series
        shiny::fluidRow(shiny::column(12,
		        shiny::fluidRow(
   		        shiny::column(12, h3(fGetEnFrText("CustCompSeriesPickerText"), align = "left")) ),
            # Dynamic UI
       	    uiOutput("uiOutPanelComp"))),

        # Add
        br(),
        shiny::fluidRow(shiny::column(12,
          shiny::fluidRow(
      	    shiny::column(10),
            shiny::column(2, shiny::actionButton("inBtnAddComp",     label = fGetEnFrText("AddCustCompButtonLabel"), class = "btnAction"),  style = "padding-left: 0px; padding-right: 0px;")))),
        br(),
        br(),

        # Restart
        shiny::fluidRow(shiny::column(12,
          shiny::fluidRow(
    	       shiny::column(10),
      	     shiny::column(2, shiny::actionButton("inBtnRestartComp", label = fGetEnFrText("RestartCustCompButtonLabel"), class = cButtonStyle), style = "padding-left: 0px; padding-right: 0px;")))),
   	    br(),
        br(),

        # Step 3 Set base period
		      shiny::fluidRow(shiny::column(12, align = "left",
		        div(style="display: inline-block", h2(fGetEnFrText("SetBasePeriodStepText"))),
		        div(style="display: inline-block", withTags({div(a(id="inToolTipSetBasePeriod", href = "#centred-popup", style="text-decoration: none", `aria-controls`="centred-popup", class="wb-lbx wb-init wb-lbx-inited", role="button", `aria-label`= "Information for step 3",
		                                     span(style="margin-top: 0.5em; color: #26374a;", class="glyphicon glyphicon-info-sign", `aria-hidden`="true") )) }) ))),
		      shiny::fluidRow(shiny::column(12, div(style = "height: 10px;"))),
		      shiny::fluidRow(
 		        shiny::column(11, h3(fGetEnFrText("SetBasePeriodText"), align = "left"))),
		      shiny::fluidRow(
		        shiny::column(5, div(shiny::selectInput("inBaseStartPeriod", label = div(fGetEnFrText("SetBasePeriodStartText"), style = "font-size: 80%; font-weight: normal;"), width = "100%", choices = NULL, multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;")),
		        shiny::column(5, div(shiny::selectInput("inBaseEndPeriod",   label = div(fGetEnFrText("SetBasePeriodEndText"),   style = "font-size: 80%; font-weight: normal;"), width = "100%", choices = NULL, multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;"))),
		      shiny::fluidRow(shiny::column(12, div(style = "height: 20px;"))),

        # Run
 	      shiny::fluidRow(shiny::column(12,
		        shiny::fluidRow(shiny::column(12, h2(fGetEnFrText("RunStepText")), align = "left")),
  	    	  shiny::fluidRow(div(shiny::actionButton("inBtnRun", label = strong(fGetEnFrText("RunButtonLabel"))), align = "center")),
		      shiny::fluidRow(shiny::column(12, div(style = "height: 5px;")))
  	      )) #left Run
  	    )) #r3 & panelTopLeft
    	), #left col




     	# Right col
    	shiny::column(7,

        # Results of last run
        shiny::fluidRow(shiny::column(12,
          shinydashboard::box(width = NULL, collapsible = TRUE, solidHeader = FALSE, title = tagList(uiOutput("uiOutQueryStatusTitle")),
            shiny::fluidRow(shiny::column(12, shiny::verbatimTextOutput("outTextQueryStatus")), style = "padding-top: 2px;")
        ))),


        # Rebase options
        shiny::fluidRow(shiny::column(12,
          shinydashboard::box(width = NULL, collapsible = TRUE, solidHeader = FALSE, title = tagList(uiOutput("uiOutRebaseBoxTitle")),
              shinyjs::hidden(shiny::radioButtons(inputId = "inRadioRebase", label = div(fGetEnFrText("RebaseRadioButtonLabel"), style = "font-weight: bold;"),
   		                              choiceNames = list(fGetEnFrText("RebaseFalse"), fGetEnFrText("RebaseTrue")),
   					    										choiceValues = list(1, 2), selected = 1, inline = FALSE, width = "100%"))
        ))),


        # Series display options
        shiny::fluidRow(shiny::column(12, shinydashboard::box(width = NULL, collapsible = TRUE, solidHeader = FALSE, title = tagList(uiOutput("uiOutCompSeriesBoxTitle")),
          shinyjs::hidden(shiny::checkboxGroupInput(inputId = "inCheckBoxDisplaySeries", label = div(fGetEnFrText("DisplayComponentSeriesLabel"), style = "font-weight: bold;"), choices = NULL, width = "100%"))
        ))),


        # Visualize statistics in graph and table
  	    shiny::wellPanel(id = "panelRight", style = "padding: 8px 10px !important; margin: 3px 0px !important;",
  	      h2(fGetEnFrText("StatisticHeaderText")),
  	      br(),
          shiny::fluidRow(shiny::column(12, shiny::tabsetPanel( #tabset graph
        	  id = "inTabsetPanelGraph",
            fUIStatPanel("12mChg",         1),
            fUIStatPanel("1mChg",          2),
            fUIStatPanel("Index",          3),
            fUIStatPanel("12mCanadaCont",  4),
            fUIStatPanel("1mCanadaCont",   5),
            fUIStatPanel("12mSameGeoCont", 6),
            fUIStatPanel("1mSameGeoCont",  7)
        ))))
    ) #right col
  ))
))}












#---------------------------------
# 4 Server function
#---------------------------------


server <- function(input, output, session) {
	 # save reusable reactive values
  rvCompCount           <- shiny::reactiveValues(n = 1)
  rvCompLastID          <- shiny::reactiveValues(n = 1)
  rvCompDispIDs         <- shiny::reactiveValues(n = NULL)
  rvSavedSel            <- shiny::reactiveValues(comp_id = NULL, sel_geo = NULL, sel_prod = NULL)
  rvBasePeriod          <- shiny::reactiveValues(base_start = NULL, base_end = NULL)
  rvLastRunSel          <- shiny::reactiveValues(comp_id = NULL, sel_geo = NULL, sel_prod = NULL)
  rvLastRunBasePeriod   <- shiny::reactiveValues(base_start = NULL, base_end = NULL)
  rvLastRunQueryResult  <- shiny::reactiveValues(query_result = NULL)
  rvCandidatePlotSeries <- shiny::reactiveValues(series = NULL)
  rvCODRData            <- shiny::reactiveValues(dfCODRIndexAll = NULL, dfCODRWeightAll = NULL, vRefPeriod = NULL, dfRefPeriods = NULL)

 	# tooltips
  # Setting tab ID used in JS script "modal-focus-start"
	 shinyjs::onclick('inToolTipCustAggSeries',  shiny::showModal(modalDialog(title = HTML(fGetEnFrText("CustAggSeriesTooltipTitleText")),           tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("CustAggSeriesTooltipText")),          easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTipCustAggSeries",  fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTipCustCompSeries', shiny::showModal(modalDialog(title = HTML(fGetEnFrText("CustCompSeriesTooltipTitleText")),          tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("CustCompSeriesTooltipText")),         easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTipCustCompSeries", fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTipSetBasePeriod',  shiny::showModal(modalDialog(title = HTML(fGetEnFrText("SetBasePeriodTooltipTitleText")),           tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("SetBasePeriodTooltipText")),          easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTipSetBasePeriod",  fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTipRebase',         shiny::showModal(modalDialog(title = HTML(fGetEnFrText("RebaseTooltipTitleText")),                  tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("RebaseTooltipText")),                 easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTipRebase",         fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTipDispCompSeries', shiny::showModal(modalDialog(title = HTML(fGetEnFrText("DisplayComponentSeriesTooltipTitleText")),  tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("DisplayComponentSeriesTooltipText")), easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTipDispCompSeries", fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTip12mChg',         shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic12mChgToolTipTitleText")),         tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("Statistic12mChgToolTipText")),        easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip12mChg",         fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTip1mChg',          shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic1mChgToolTipTitleText")),          tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("Statistic1mChgToolTipText")),         easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip1mChg",          fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTipIndex',          shiny::showModal(modalDialog(title = HTML(fGetEnFrText("StatisticIndexToolTipTitleText")),          tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(paste0(fGetEnFrText("StatisticIndexPart1ToolTipText"), fGetEnFrText("StatisticIndexPart2ToolTipText"), fGetEnFrText("StatisticIndexPart3ToolTipText"))),
	                                                                                                                                                                                                                                                      easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTipIndex",          fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
  shinyjs::onclick('inToolTip12mCanadaCont',  shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic12mCanadaContToolTipTitleText")),  tags$div(id = "modal-focus-start", tabindex = "-1"),  HTML(fGetEnFrText("Statistic12mCanadaContToolTipText")), easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip12mCanadaCont",  fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTip12mSameGeoCont', shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic12mSameGeoContToolTipTitleText")), tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("Statistic12mSameGeoContToolTipText")), easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip12mSameGeoCont", fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTip1mCanadaCont',   shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic1mCanadaContToolTipTitleText")),   tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("Statistic1mCanadaContToolTipText")),   easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip1mCanadaCont",   fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
	 shinyjs::onclick('inToolTip1mSameGeoCont',  shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic1mSameGeoContToolTipTitleText")),  tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("Statistic1mSameGeoContToolTipText")),  easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip1mSameGeoCont",  fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )

 	observeEvent(input$inBtnCloseToolTipCustAggSeries,  {removeModal()})
	 observeEvent(input$inBtnCloseToolTipCustCompSeries, {removeModal()})
	 observeEvent(input$inBtnCloseToolTipSetBasePeriod,  {removeModal()})
	 observeEvent(input$inBtnCloseToolTipRebase,         {removeModal()})
	 observeEvent(input$inBtnCloseToolTipDispCompSeries, {removeModal()})
	 observeEvent(input$inBtnCloseToolTip12mChg,         {removeModal()})
	 observeEvent(input$inBtnCloseToolTip1mChg,          {removeModal()})
	 observeEvent(input$inBtnCloseToolTipIndex,          {removeModal()})
	 observeEvent(input$inBtnCloseToolTip12mCanadaCont,  {removeModal()})
	 observeEvent(input$inBtnCloseToolTip12mSameGeoCont, {removeModal()})
	 observeEvent(input$inBtnCloseToolTip1mCanadaCont,   {removeModal()})
	 observeEvent(input$inBtnCloseToolTip1mSameGeoCont,  {removeModal()})


  # At first launch, ask user to accept Terms of Use before load
  shiny::showModal(modalDialog(title = HTML(fGetEnFrText("TermsOfUseHeaderText")),  tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("TermsOfUseText")),  easyClose = FALSE, 
    footer = tagList(actionButton("modalBtnAcceptTermsForGetData", fGetEnFrText("TermsOfUseButtonAcceptLabel")), 
                     actionButton("modalBtnRefuseTermsForGetData", fGetEnFrText("TermsOfUseButtonRefuseLabel")) ) ) )

  
  # Once terms of use accepted, get data
  shiny::observeEvent(input$modalBtnAcceptTermsForGetData, {
    shiny::removeModal()
    waiter::waiter_show(html = waiter::spin_fading_circles())

    # get index data from ZIP
#    dfCODRIndexAll <- data.frame(table_18100004_vector = 1234, reference_period = "2025-01-01", index_r = 123.4)
    dfCODRIndexAll <- as.data.frame(readr::read_csv(archive::archive_read("https://www150.statcan.gc.ca/n1/tbl/csv/18100004-eng.zip", file = 1), show_col_types = FALSE)) |>
      filter(REF_DATE >= cFirstIndexYearMonth) |>
      select(table_18100004_vector = VECTOR,
             reference_period      = REF_DATE,
             index_r               = VALUE) |>
      mutate(table_18100004_vector = as.integer(substr(table_18100004_vector, 2, nchar(table_18100004_vector))),
             reference_period = paste0(reference_period, "-01"))

    # get CODR weights
#    dfCODRWeightAll <- data.frame(table_18100007_vector = 1234, weight_reference_period = "2026-01-01", weight_r = 23.4, weight_version = "original")
    dfCODRWeightAll <- as.data.frame(readr::read_csv(archive::archive_read("https://www150.statcan.gc.ca/n1/tbl/csv/18100007-eng.zip", file = 1), show_col_types = FALSE)) |>
     	select(table_18100007_vector   = VECTOR,
             weight_reference_period = REF_DATE,
             weight_r                = VALUE) |>
      mutate(table_18100007_vector   = as.integer(substr(table_18100007_vector, 2, nchar(table_18100007_vector))),
             weight_reference_period = paste0(weight_reference_period, "-01-01"),
      			    weight_version          = ifelse(weight_reference_period == "2001-01-01", "revised", "original"))

    # Create vector and dataframe of all ref periods
    iLastIndexYear           <- as.integer(substr(as.character(max(dfCODRIndexAll$reference_period)), 1, 4))
    iLastIndexPeriod         <- as.integer(substr(as.character(max(dfCODRIndexAll$reference_period)), 6, 7))
    vRefPeriod <- vector('character')
    i <- 0
    for (y in iFirstIndexYear:iLastIndexYear){
      for (m in 1:12){
  	     if ( ((y * 100 + m) >= (iFirstIndexYear * 100 + iFirstIndexPeriod)) & ((y * 100 + m) <= (iLastIndexYear * 100 + iLastIndexPeriod)) ) {
  	       i  <- i + 1
          vRefPeriod[i] <- paste0(as.character(y), "-", substr(as.character(m + 100), 2, 3), "-01")
  	     }
      }
    }
    dfRefPeriods <- data.frame(vRefPeriod) |>
      rename("reference_period" = vRefPeriod)

    # prepare list of all objects for future use    
  		rvCODRData$dfCODRIndexAll  <- dfCODRIndexAll
  		rvCODRData$dfCODRWeightAll <- dfCODRWeightAll
  		rvCODRData$vRefPeriod      <- vRefPeriod
  		rvCODRData$dfRefPeriods    <- dfRefPeriods
  			
    waiter::waiter_hide()
    
	   # Warn user if weight metadata is out-of-date
    if (max(rvCODRData$dfCODRWeightAll$weight_reference_period) > cMaxWeightRefPeriod) {
 	    shiny::showModal(modalDialog(title = HTML(fGetEnFrText("UpdateNeededTitleText")),  tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("UpdateNeededText")),  easyClose = FALSE, footer = modalButton(fGetEnFrText("ToolTipMessageButtonCloseLabel"))) )
    }
  })
 
   
  # Stop the Shiny app
  observeEvent(input$modalBtnRefuseTermsForGetData, {
    stopApp()
  })
  


	 # create dynamic output whenever rvCompCount$n changes
	 output$uiOutPanelComp <- renderUI({
    fMessage("renderUI", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
    waiter::waiter_show(html = waiter::spin_fading_circles())

  	lDynOutput <- lapply(1:rvCompCount$n, function(i) {
      dfDisplaySeries <- fGetDisplaySeries(rvCompCount$n, i, rvSavedSel, dfSeriesSpagg) |>
       	filter(comp_num == i)
      vcDisplayGeo <- unique(dfDisplaySeries$geo)

      if (length(rvSavedSel$sel_geo) > 0) {
   	    if (!is.na(rvSavedSel$comp_id[i])) {
   	 	    iCompID <- rvSavedSel$comp_id[i]
   	    } else {iCompID <- rvCompLastID$n}
      } else {iCompID <- rvCompLastID$n}

      fMessage("renderUI function(i)", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
   	  fMessage("renderUI function(i)2", paste0("renderUI lapply i: ", i, ", iCompID: ", iCompID, ", vcDisplayGeo: ", paste0(vcDisplayGeo, collapse = ", ")))

   	  if (i > length(rvSavedSel$sel_geo) & length(vcDisplayGeo) > 1) {
   	    vcChoicesGeo = c("", vcDisplayGeo)
   	    cSelectedGeo = ""
   	  } else {
   	    vcChoicesGeo = vcDisplayGeo
   	    cSelectedGeo = vcDisplayGeo
   	  }
   	  if (i > length(rvSavedSel$sel_geo)) {
   	    vcChoicesProd = c("", dfDisplaySeries$prod)
   	    cSelectedProd = ""
   	  } else {
   	    vcChoicesProd = dfDisplaySeries$prod
   	    cSelectedProd = dfDisplaySeries$prod
   	  }
   	  if (i == 1) {
   	    tags$div(
   	      shiny::fluidRow(
   	        shiny::column(4, style = "padding-right: 2px;",                     div(shiny::selectInput(paste0("dynSelGeo",  i),  label = div(fGetEnFrText("CustCompSeriesGeoText"),  style = "font-size: 80%; font-weight: normal;"), width = "100%", choices = vcChoicesGeo,  selected = cSelectedGeo,  multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;")),
   	        shiny::column(6, style = "padding-left: 2px; padding-right: 0px;",  div(shiny::selectInput(paste0("dynSelProd",  i), label = div(fGetEnFrText("CustCompSeriesProdText"), style = "font-size: 80%; font-weight: normal;"), width = "100%", choices = vcChoicesProd, selected = cSelectedProd, multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;")),
   	        shiny::column(2,
   	                      br(),
   	                      shiny::fluidRow(column(12, shiny::actionButton(paste0("dynBtnRemoveCompID", iCompID), label = fGetEnFrText("RemoveCustCompButtonLabel"), class = "btnAction"), style = "padding-left: 0px; padding-right: 0px; vertical-align: bottom !important;"))))
   	    )
   	  } else {
   	    tags$div(
   	      shiny::fluidRow(
   	        shiny::column(4, style = "padding-right: 2px;",                    div(shiny::selectInput(paste0("dynSelGeo",  i),  label = NULL, width = "100%", choices = vcChoicesGeo,  selected = cSelectedGeo,  multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;")),
   	        shiny::column(6, style = "padding-left: 2px; padding-right: 0px;", div(shiny::selectInput(paste0("dynSelProd",  i), label = NULL, width = "100%", choices = vcChoicesProd, selected = cSelectedProd, multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;")),
   	        shiny::column(2, shiny::fluidRow(column(12, shiny::actionButton(paste0("dynBtnRemoveCompID", iCompID), label = fGetEnFrText("RemoveCustCompButtonLabel"), class = "btnAction"), style = "padding-left: 0px; padding-right: 0px; vertical-align: bottom !important;"))))
   	    )
   	  }

    })
    waiter::waiter_hide()
    do.call(tagList, lDynOutput)
  })


  # Initialize Popular aggregate product selector
  shiny::observeEvent(input$inSelPopularAggGeo, {
    cSelPopularAggGeo    <- input$inSelPopularAggGeo

    if (length(cSelPopularAggGeo) > 0) {
      vcPopularAggProd <- dfPopularAggDefn[dfPopularAggDefn$indented_geography == cSelPopularAggGeo, "indented_product"]
 	 	  shiny::updateSelectInput(session, "inSelPopularAggProd", choices = vcPopularAggProd)
    }

    fMessage("Initialize popular aggregate",  paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
    fMessage("Initialize popular aggregate2", paste0("input$inSelPopularAggGeo: ", input$inSelPopularAggGeo, ", input$inSelPopularAggProd: ", input$inSelPopularAggProd))
  })


  # Initialize Prod selector
  shiny::observeEvent(input[[ paste0("dynSelGeo",  rvCompCount$n) ]], {
    cSelGeo    <- input[[ paste0("dynSelGeo",  rvCompCount$n) ]]

 	 	shiny::updateSelectInput(session, paste0("dynSelProd", rvCompCount$n), choices = "", selected = NULL)

    if (length(cSelGeo) > 0) {
      dfDisplaySeries <- fGetDisplaySeries(rvCompCount$n, 1, rvSavedSel, dfSeriesSpagg)
      vcCustCompProd  <- dfDisplaySeries[dfDisplaySeries$comp_num == rvCompCount$n & dfDisplaySeries$geo == cSelGeo, "prod"]

 	 	  if (cSelGeo != "") {
 	 	  	if (length(vcCustCompProd) == 1) {shiny::updateSelectInput(session, paste0("dynSelProd", rvCompCount$n), choices = vcCustCompProd)}
 	 	  	else                             {shiny::updateSelectInput(session, paste0("dynSelProd", rvCompCount$n), choices = vcCustCompProd)}
 	 	  }
    }

    fMessage("Initialize Prod", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
    fMessage("Initialize Prod2", paste0("Initialize Prod cSelGeo: ", cSelGeo, ", length(vcCustCompProd): ", length(vcCustCompProd)))
  })


  # Enable / Disable buttons and selectors
  shiny::observe({

    cSelPopularAggGeo  <- input$inSelPopularAggGeo
    cSelPopularAggProd <- input$inSelPopularAggProd
    cSelGeo            <- input[[ paste0("dynSelGeo",  rvCompCount$n) ]]
    cSelProd           <- input[[ paste0("dynSelProd", rvCompCount$n) ]]

    fMessage("Enable / Disable buttons2", paste0("cSelPopularAggGeo: ", cSelPopularAggGeo, ", cSelPopularAggProd: ", cSelPopularAggProd, ", cSelGeo: ", cSelGeo, ", cSelProd: ", cSelProd))

    shinyjs::enable("inSelPopularAggGeo")
    shinyjs::enable("inSelPopularAggProd")
    shinyjs::enable("inBtnApplyPopularAgg")
    shinyjs::enable(paste0("dynSelGeo",  rvCompCount$n))
    shinyjs::enable(paste0("dynSelProd", rvCompCount$n))
    shinyjs::enable("inBtnAddComp")
  	 shinyjs::enable("inBtnRestartComp")
  	 shinyjs::enable("inBtnRun")
  	 shinyjs::enable("inBaseStartPeriod")
  	 shinyjs::enable("inBaseEndPeriod")

  	if (length(cSelPopularAggGeo) > 0) {
  	  if (nchar(cSelPopularAggGeo) == 0) {
  	    shinyjs::disable("inSelPopularAggProd")
  	  }
  	}
  	if (length(cSelPopularAggProd) > 0) {
  		if (nchar(cSelPopularAggProd) == 0) {
 		    shinyjs::disable("inBtnApplyPopularAgg")
      }
  	}
  	if (length(cSelGeo) > 0) {
  	  if (nchar(cSelGeo) == 0) {
  	    shinyjs::disable(paste0("dynSelProd", rvCompCount$n))
  	  }
  	}
  	if (length(cSelProd) > 0) {
  		if (nchar(cSelProd) == 0) {
 	 	    shinyjs::disable("inBtnAddComp")
 		    shinyjs::disable("inBtnRun")
  	    shinyjs::disable("inBaseStartPeriod")
  	    shinyjs::disable("inBaseEndPeriod")
  		}
  	}
  	if (length(cSelPopularAggGeo) > 0 & length(cSelPopularAggProd) > 0 & length(cSelGeo) > 0 & length(cSelProd) > 0) {
  		if (nchar(cSelPopularAggGeo) == 0 & nchar(cSelPopularAggProd) == 0 & nchar(cSelGeo) == 0 & nchar(cSelProd) == 0) {
 	 	    shinyjs::disable("inBtnRestartComp")
  		}
  	}

    fMessage("Enable / Disable buttons2", paste0("cSelPopularAggGeo: ", cSelPopularAggGeo, ", cSelPopularAggProd: ", cSelPopularAggProd, ", cSelGeo: ", cSelGeo, ", cSelProd: ", cSelProd))
    fMessage("Enable / Disable buttons", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
  })


  # Apply popular aggregate
  shiny::observeEvent(input$inBtnApplyPopularAgg, {
    fMessage("Apply popular aggregate before", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
  	waiter::waiter_show(html = waiter::spin_fading_circles())

  	cSelPopularAggGeo   <- input$inSelPopularAggGeo
  	cSelPopularAggProd  <- input$inSelPopularAggProd

    dfSelPopularAggComp <- dfPopularAggDefn |>
    	filter(indented_geography == cSelPopularAggGeo & indented_product == cSelPopularAggProd) |>
    	select(aggregate_id) |>
    	left_join(dfPopularAggComp,
    						by = "aggregate_id") |>
    	left_join(dfSeriesSpagg,
    						by = c("where_to_code", "i_dim2_position")) |>
    	arrange(where_to_code, i_dim2_position)

    iPopularAggCompCount <- nrow(dfSelPopularAggComp)
    rvCompCount$n        <- iPopularAggCompCount
    iLastID              <- rvCompLastID$n
    rvCompLastID$n       <- iLastID + iPopularAggCompCount # +1 needed to avoid remove scenario 1
    rvCompDispIDs$n      <- 1:iPopularAggCompCount + iLastID # +1 needed to avoid remove scenario 1
    rvSavedSel$comp_id   <- 1:iPopularAggCompCount + iLastID # +1 needed to avoid remove scenario 1
    rvSavedSel$sel_geo   <- dfSelPopularAggComp$indented_geography
    rvSavedSel$sel_prod  <- dfSelPopularAggComp$indented_product

    waiter::waiter_hide()
    fMessage("Apply popular aggregate after", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
  })


  # Create vector of unique component ids to use in Remove Component n's for loop
  shiny::observe({
  	if (length(rvSavedSel$sel_geo) == 0) {
  		rvCompDispIDs$n       <- rvCompLastID$n
  	}	else if (rvCompCount$n > length(rvSavedSel$sel_geo)) {
  		rvCompDispIDs$n       <- c(rvSavedSel$comp_id, rvCompLastID$n)
  	}	else if (rvCompCount$n == length(rvSavedSel$sel_geo)) {
  		rvCompDispIDs$n       <- rvSavedSel$comp_id
  	}
  })


  # Remove Component n
  shiny::observe({
    fMessage("Remove", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

    for (i in rvCompDispIDs$n) {
    	if (length(rvSavedSel$sel_geo) > 0) { #scenarios 2, 3, 4, 5
   	    if (length(which(rvSavedSel$comp_id == i)) > 0) { #scenarios 2, 3, 5
   	 	    iCompID <- i
          fMessage("Remove > for > iCompID > scenarios 2, 3, 5", paste0("i: ", i, ", iCompID: ", iCompID))
   	    } else { #scenario 4
   	    	iCompID <- rvCompLastID$n
          fMessage("Remove > for > iCompID > scenario 4", paste0("i: ", i, ", iCompID: ", iCompID))
   	    }
    	} else { #scenario 1
      	iCompID <- rvCompLastID$n
        fMessage("Remove > for > iCompID > scenario 1", paste0("i: ", i, ", iCompID: ", iCompID))
      }

      base::local({
     	  iLocal       <- i
     	  iLocalCompID <- iCompID # for testing whether this was selected
     		fMessage("Remove > for > local", paste0("i: ", i, ", iLocal: ", iLocal, ", iCompID: ", iCompID, ", iLocalCompID: ", iLocalCompID, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", ")))

        observeEvent(input[[paste0("dynBtnRemoveCompID", iLocalCompID)]],{
         	iRemoveCompPosition <- which(rvSavedSel$comp_id == iLocalCompID)
    		  fMessage("Remove > for > local > observeEvent", paste0("i: ", i, ", iLocal: ", iLocal, ", iCompID: ", iCompID, ", iLocalCompID: ", iLocalCompID, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", ")))

          if (iLocal == iLocalCompID) {
             fMessage("Remove > for > local > observeEvent > iLocal == iCompID > before", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste0(as.numeric(rvCompDispIDs$n), collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
     		     fMessage("Remove > for > local > observeEvent > iLocal == iCompID > to remove", paste0("iLocalCompID: ", iLocalCompID, ", length(iRemoveCompPosition): ", length(iRemoveCompPosition), ", iRemoveCompPosition: ", iRemoveCompPosition, ", rvSavedSel$comp_id[iRemoveCompPosition]: ", rvSavedSel$comp_id[iRemoveCompPosition], ", sel_geo: ", rvSavedSel$sel_geo[iRemoveCompPosition], ", sel_prod: ", rvSavedSel$sel_prod[iRemoveCompPosition]))

             waiter::waiter_show(html = waiter::spin_fading_circles())
            # scenario 1: components displayed: 1; components saved: 0; remove component: 1;      resulting rvCompCount: 1; resulting rvCompLastID: +1?; resulting saved: 0; path: 1b;
            # scenario 2: components displayed: 1; components saved: 1; remove component: 1;      resulting rvCompCount: 1; resulting rvCompLastID: +1?; resulting saved: 0; path: 1a2;
            # scenario 3: components displayed: 2; components saved: 1; remove component: 1;      resulting rvCompCount: 1; resulting rvCompLastID: 1?;  resulting saved: 0; path: 1a1;
            # scenario 4: components displayed: 2; components saved: 1; remove component: 2;      resulting rvCompCount: 1; resulting rvCompLastID: 1;   resulting saved: 1; path: 1b;
            # scenario 5: components displayed: 2; components saved: 2; remove component: 1 or 2; resulting rvCompCount: 1; resulting rvCompLastID: 1;   resulting saved: 1; path: 1a1;

            if (rvCompCount$n == 1 & length(rvSavedSel$sel_geo) == 0) {
              # scenario 1: components displayed: 1; components saved: 0; remove component: 1;      resulting rvCompCount: 1; resulting rvCompLastID: +1?; resulting saved: 0; path: 1b;
     		      fMessage("Remove > for > local > observeEvent > if scenario 1", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste0(as.numeric(rvCompDispIDs$n), collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

              rvCompCount$n       <- 0 #needed to ensure Prod selector is refreshed
              rvCompCount$n       <- 1
              rvCompLastID$n      <- rvCompLastID$n + 1
              rvCompDispIDs$n     <- rvCompLastID$n
              rvSavedSel$comp_id  <- NULL
              rvSavedSel$sel_geo  <- NULL
              rvSavedSel$sel_prod <- NULL

            } else if (rvCompCount$n == 1 & length(rvSavedSel$sel_geo) == 1) {
            	if (length(iRemoveCompPosition) > 0) {
              # scenario 2: components displayed: 1; components saved: 1; remove component: 1;      resulting rvCompCount: 1; resulting rvCompLastID: +1?; resulting saved: 0; path: 1a2;
     		        fMessage("Remove > for > local > observeEvent > if scenario 2", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste0(as.numeric(rvCompDispIDs$n), collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

                rvCompCount$n       <- 0 #needed to ensure Prod selector is refreshed
                rvCompCount$n       <- 1
                rvCompLastID$n      <- rvCompLastID$n + 1
                rvCompDispIDs$n     <- rvCompLastID$n
                rvSavedSel$comp_id  <- NULL
                rvSavedSel$sel_geo  <- NULL
                rvSavedSel$sel_prod <- NULL
            	}
            } else if (rvCompCount$n > 1 & length(rvSavedSel$sel_geo) == rvCompCount$n - 1 & length(iRemoveCompPosition) > 0) {
              # scenario 3: components displayed: 2; components saved: 1; remove component: 1;      resulting rvCompCount: 1; resulting rvCompLastID: 1?;  resulting saved: 0; path: 1a1;
     		      fMessage("Remove > for > local > observeEvent > if scenario 3", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

     		    	rvSavedSel$comp_id  <- rvSavedSel$comp_id[-iRemoveCompPosition]
     		      rvSavedSel$sel_geo  <- rvSavedSel$sel_geo[-iRemoveCompPosition]
     		      rvSavedSel$sel_prod <- rvSavedSel$sel_prod[-iRemoveCompPosition]
     	        rvCompCount$n       <- rvCompCount$n - 1
              rvCompLastID$n      <- rvCompLastID$n + 1
     		      rvCompDispIDs$n     <- rvCompDispIDs$n[-which(rvCompDispIDs$n == iLocalCompID)]
            } else if (rvCompCount$n > 1 & length(rvSavedSel$sel_geo) == rvCompCount$n - 1) {
            	if (length(iRemoveCompPosition) == 0) {
              # scenario 4: components displayed: 2; components saved: 1; remove component: 2;      resulting rvCompCount: 1; resulting rvCompLastID: 1;   resulting saved: 1; path: 1b;
     		        fMessage("Remove > for > local > observeEvent > if scenario 4", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

      		  	  # don't update rvSavedSel
     	          rvCompCount$n     <- rvCompCount$n - 1
                rvCompLastID$n    <- rvCompLastID$n + 1
     		        rvCompDispIDs$n   <- rvCompDispIDs$n[-length(rvCompDispIDs$n)]
        	      }
            } else if (rvCompCount$n > 1 & length(rvSavedSel$sel_geo) == rvCompCount$n) {
            	if (length(iRemoveCompPosition) > 0) {
              # scenario 5: components displayed: 2; components saved: 2; remove component: 1 or 2; resulting rvCompCount: 1; resulting rvCompLastID: 1;   resulting saved: 1; path: 1a1;
     		        fMessage("Remove > for > local > observeEvent > if scenario 5", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

     		    	  rvSavedSel$comp_id  <- rvSavedSel$comp_id[-iRemoveCompPosition]
     		        rvSavedSel$sel_geo  <- rvSavedSel$sel_geo[-iRemoveCompPosition]
     		        rvSavedSel$sel_prod <- rvSavedSel$sel_prod[-iRemoveCompPosition]
     	          rvCompCount$n       <- rvCompCount$n - 1
                rvCompLastID$n      <- rvCompLastID$n + 1
     		        rvCompDispIDs$n     <- rvCompDispIDs$n[-which(rvCompDispIDs$n == iLocalCompID)]
              }
            }
            waiter::waiter_hide()

     		    fMessage("Remove > for > local > observeEvent > iLocal == iCompID > after", paste0("i: ", i, ", iLocal: ", iLocal, ", iCompID: ", iCompID, ", iLocalCompID: ", iLocalCompID, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvCompDispIDs$n): ", length(rvCompDispIDs$n)))
            fMessage("Remove > for > local > observeEvent > iLocal == iCompID > after2", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
          } #if (iLocal == iLocalCompID
        }) #observeEvent
      }) #base::local
    } #for(i in 1:iN)
  })

 # Add Component
  shiny::observeEvent(input$inBtnAddComp, {
    fMessage("Add before", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
  	waiter::waiter_show(html = waiter::spin_fading_circles())

   	if (rvCompLastID$n == 1 | (rvCompLastID$n > 1 & rvCompCount$n == 1 & length(rvSavedSel$sel_geo) == 0)) {
      fMessage("Add at start or after restart", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

   		rvSavedSel$comp_id[1]  <- rvCompLastID$n
      rvSavedSel$sel_geo[1]  <- input[[ paste0("dynSelGeo", 1) ]]
      rvSavedSel$sel_prod[1] <- input[[ paste0("dynSelProd", 1) ]]
   	} else if (rvCompCount$n > length(rvSavedSel$sel_geo)) {
      fMessage("Add when 2 or more displayed & count disp > count saved", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

   		rvSavedSel$comp_id[rvCompCount$n]  <- rvCompLastID$n
      rvSavedSel$sel_geo[rvCompCount$n]  <- input[[ paste0("dynSelGeo", rvCompCount$n ) ]]
      rvSavedSel$sel_prod[rvCompCount$n] <- input[[ paste0("dynSelProd", rvCompCount$n ) ]]
   	} else if (rvCompCount$n == length(rvSavedSel$sel_geo)) { # after Go or after removing last component
      fMessage("Add after go or after removing last component", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
   	}

    shinyjs::disable(paste0("dynSelProd", rvCompCount$n ))

    dfDisplaySeries  <- fGetDisplaySeries(rvCompCount$n + 1, 1, rvSavedSel, dfSeriesSpagg)
    iCandidatesCount <- nrow(dfDisplaySeries[dfDisplaySeries$comp_num == rvCompCount$n + 1, ])
    fMessage("Add iCandidatesCount", as.character(iCandidatesCount))
    if (iCandidatesCount > 0) {
      rvCompCount$n  <- rvCompCount$n + 1
      rvCompLastID$n <- rvCompLastID$n + 1
    } else {
    	shiny::showModal(shiny::modalDialog(tags$div(id = "modal-focus-start", tabindex = "-1"), fGetEnFrText("CustCompNoneAvailableModalText"), easyClose = FALSE, footer = tagList(actionButton("inBtnCloseModalCustCompNoneAvailable",  fGetEnFrText("ToolTipMessageButtonCloseLabel"))  ) ) )
    }

    waiter::waiter_hide()
    fMessage("Add after", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
  })


  # Set focus after add (when rvCompCount increases)
  observeEvent(rvCompCount$n, {
    req(input[[paste0("dynSelGeo", rvCompCount$n-1)]])
    session$sendCustomMessage("focusNewComp", paste0("dynSelGeo", rvCompCount$n))
  }, ignoreInit = TRUE)


  observeEvent(input$inBtnCloseModalCustCompNoneAvailable,  {removeModal()})


 # Restart
 shiny::observeEvent(input$inBtnRestartComp, {
   fMessage("Restart before", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
   waiter::waiter_show(html = waiter::spin_fading_circles())

   rvCompCount$n       <- 0 #needed to ensure Prod selector is refreshed
   rvCompCount$n       <- 1
   rvCompLastID$n      <- 1
   rvCompDispIDs$n     <- 1
   rvSavedSel$comp_id  <- NULL
   rvSavedSel$sel_geo  <- NULL
   rvSavedSel$sel_prod <- NULL
   shiny::updateSelectInput(session, "inSelPopularAggGeo",  choices = c("", unique(dfPopularAggDefn$indented_geography)))
   shiny::updateSelectInput(session, "inSelPopularAggProd", choices = "", selected = "")

   		shiny::tags$script(HTML("$(document).on('shiny:inputchanged', function(event) {
         if      (event.name === 'inBtnRestartComp')                {$('#inSelPopularAggGeo')[0].select.focus();}
   		});"))

   waiter::waiter_hide()
   fMessage("Restart after", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
 })


	# Set possible base period start & end dates
  shiny::observeEvent(input[[ paste0("dynSelProd",  rvCompCount$n) ]], {
    iN       <- rvCompCount$n
    cSelGeo  <- input[[ paste0("dynSelGeo",  iN) ]]
    cSelProd <- input[[ paste0("dynSelProd", iN) ]]

    dfSavedGeoProd <- data.frame(cbind(rvSavedSel$sel_geo, rvSavedSel$sel_prod)) |>
    	mutate(comp_num = row_number())

    if (length(cSelProd) > 0 && nchar(cSelProd) > 0) {
    	if (nrow(dfSavedGeoProd) > 0) {
    		names(dfSavedGeoProd)[1:2] <- c("sel_geo", "sel_prod")
    		dfSavedGeoProd   <- dfSavedGeoProd[dfSavedGeoProd$comp_num < iN, ]
    		dfSelSeriesSpagg <- rbind(dfSavedGeoProd, data.frame(sel_geo = cSelGeo, sel_prod = cSelProd, comp_num = iN))
    	} else if (length(cSelGeo) > 0 & length(cSelProd) > 0) {
    		dfSelSeriesSpagg <- data.frame(sel_geo = cSelGeo, sel_prod = cSelProd, comp_num = iN)
    	}

      dfSelSeriesSpagg <- dfSelSeriesSpagg |>
        left_join(dfSeriesSpagg |>
    		  				  mutate(row_number = row_number()) |>
    			  			  rename(sel_geo  = indented_geography,
    				  		 			   sel_prod = indented_product),
    					    by = c("sel_geo", "sel_prod")) |>
    	  arrange(sel_geo, sel_prod)
   	  vSelectedCustAggRows <- vector('numeric')
   	  vSelectedCustAggRows <- dfSelSeriesSpagg$row_number

  	  cStartBasePeriod        <- substr(max(min(dfSeriesSpagg[vSelectedCustAggRows, ]$i_first_ref_date), min(dfSeriesSpagg[vSelectedCustAggRows, ]$w_first_period), cDefaultStartBasePeriod), 1, 7)
 	    rvBasePeriod$base_start <- cStartBasePeriod
 	    rvBasePeriod$base_end   <- cStartBasePeriod
  	  vRef                    <- substr(rvCODRData$vRefPeriod, 1, 7)
  	  vRef                    <- sort(vRef[vRef >= cStartBasePeriod], decreasing = TRUE)
      shiny::updateSelectInput(session, "inBaseStartPeriod", choices = vRef, selected = cStartBasePeriod)
      shiny::updateSelectInput(session, "inBaseEndPeriod",   choices = vRef, selected = cStartBasePeriod)
    } else {
 	    rvBasePeriod$base_start     <- NULL
 	    rvBasePeriod$base_end       <- NULL
      shiny::updateSelectInput(session, "inBaseStartPeriod", choices = NULL, selected = NULL)
      shiny::updateSelectInput(session, "inBaseEndPeriod",   choices = NULL, selected = NULL)
	  }
  })


 	# Adjust base period start & end dates based on other's value
  shiny::observeEvent(input$inBaseStartPeriod, {
  	if (length(input$inBaseStartPeriod) > 0 & length(rvBasePeriod$base_start) > 0 && (!is.na(input$inBaseStartPeriod) & !is.na(rvBasePeriod$base_start))) {
  		if (input$inBaseStartPeriod != rvBasePeriod$base_start) {
  	    rvBasePeriod$base_start <- input$inBaseStartPeriod
  	    vRef                    <- substr(rvCODRData$vRefPeriod, 1, 7)
   	    vRef                    <- sort(vRef[vRef >= input$inBaseStartPeriod], decreasing = TRUE)
 	      if (input$inBaseEndPeriod < input$inBaseStartPeriod) {
 	    	  rvBasePeriod$base_end <- rvBasePeriod$base_start
 	      } else {
 	    	  rvBasePeriod$base_end <- input$inBaseEndPeriod
 	      }
        shiny::updateSelectInput(session, "inBaseEndPeriod", choices = vRef, selected = rvBasePeriod$base_end)
  	  }
  	}
  })


 	# Adjust base period start & end dates based on other's value
  shiny::observeEvent(input$inBaseEndPeriod, {
  	if (length(input$inBaseEndPeriod) > 0 & length(rvBasePeriod$base_end) > 0
  			&& (!is.na(input$inBaseEndPeriod) & !is.na(rvBasePeriod$base_end))) {
  	  if (input$inBaseEndPeriod != rvBasePeriod$base_end) {
 	      rvBasePeriod$base_end <- input$inBaseEndPeriod
  	  }
  	}
  })
  

  # on Run button click, create Query Result list that can be reused throughout server function
  lQueryResult <- shiny::eventReactive(input$inBtnRun, {
    fMessage("Run before", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

    	waiter::waiter_show(html = waiter::spin_fading_circles())

    	iN       <- rvCompCount$n
    	cSelGeo  <- input[[ paste0("dynSelGeo",  iN) ]]
    	cSelProd <- input[[ paste0("dynSelProd", iN) ]]

    	dfSavedGeoProd <- data.frame(cbind(rvSavedSel$sel_geo, rvSavedSel$sel_prod)) |>
    		mutate(comp_num = row_number())

    	if (nrow(dfSavedGeoProd) > 0) {
    		names(dfSavedGeoProd)[1:2] <- c("sel_geo", "sel_prod")
    		dfSavedGeoProd   <- dfSavedGeoProd[dfSavedGeoProd$comp_num < iN, ]
    		dfSelSeriesSpagg <- rbind(dfSavedGeoProd, data.frame(sel_geo = cSelGeo, sel_prod = cSelProd, comp_num = iN))
    	} else if (length(cSelGeo) > 0 & length(cSelProd) > 0) {
    		dfSelSeriesSpagg <- data.frame(sel_geo = cSelGeo, sel_prod = cSelProd, comp_num = iN)
    	} else { # use first row, for initial load
    		dfSelSeriesSpagg <- dfSeriesSpagg[1, ] |>
    			select(indented_geography, indented_product) |>
    			rename(sel_geo  = indented_geography,
    						 sel_prod = indented_product) |>
    			mutate(comp_num = 1)
    	}

    	dfSelSeriesSpagg <- dfSelSeriesSpagg |>
    		left_join(dfSeriesSpagg |>
    								mutate(row_number = row_number()) |>
    								rename(sel_geo  = indented_geography,
    											 sel_prod = indented_product),
    							by = c("sel_geo", "sel_prod")) |>
    		arrange(sel_geo, sel_prod)
    	vSelectedCustAggRows <- vector('numeric')
    	vSelectedCustAggRows <- dfSelSeriesSpagg$row_number

    	# retrieve last run geo and prod components, if no change versus this run, don't execute query results
    	dfLastRunGeoProd <- data.frame(cbind(rvLastRunSel$sel_geo,
    																			 rvLastRunSel$sel_prod))
    	if (nrow(dfLastRunGeoProd) > 0) {
    		names(dfLastRunGeoProd)[1:2] <- c("sel_geo", "sel_prod")
    		dfLastRunGeoProd <- dfLastRunGeoProd |>
    			arrange(sel_geo, sel_prod)
    	}

    	if (identical(dfSelSeriesSpagg[, 1:2], dfLastRunGeoProd) & identical(rvBasePeriod$base_start, rvLastRunBasePeriod$base_start) & identical(rvBasePeriod$base_end, rvLastRunBasePeriod$base_end)) {
    		lQueryResult <- rvLastRunQueryResult$query_result
    	} else { # new components
    		###### CALL MAIN FUNCTION #####
#    		lQueryResult <- lZQueryResult
    		lQueryResult <- fIndexWeightChgCont(dfBasket, dfSeriesReg, dfSeriesSpagg, rvCODRData$dfCODRIndexAll, rvCODRData$dfCODRWeightAll, rvCODRData$dfRefPeriods, vSelectedCustAggRows, rvBasePeriod$base_start, rvBasePeriod$base_end)

    		# update rvLastRun...
    		rvLastRunSel$sel_geo              <- dfSelSeriesSpagg$sel_geo
    		rvLastRunSel$sel_prod             <- dfSelSeriesSpagg$sel_prod
    		rvLastRunQueryResult$query_result <- lQueryResult
    		rvLastRunBasePeriod$base_start    <- rvBasePeriod$base_start
    		rvLastRunBasePeriod$base_end      <- rvBasePeriod$base_end

    		# update rvSavedSel...
    		rvSavedSel$comp_id[iN]  <- rvCompLastID$n
        rvSavedSel$sel_geo[iN]  <- cSelGeo
        rvSavedSel$sel_prod[iN] <- cSelProd
    	} # new components

    	iQueryRowCount <- nrow(lQueryResult$dfQueryResult)

    	# on Run button click, if query rows > 0, enable objects, otherwise hide or disable
    	if (iQueryRowCount == 0 | is.null(iQueryRowCount) ) {
    		shinyjs::disable("outPlotlyDownload12mChg")
    		shinyjs::disable("outPlotlyDownload1mChg")
    		shinyjs::disable("outPlotlyDownloadIndex")
    		shinyjs::disable("outPlotlyDownload12mCanadaCont")
    		shinyjs::disable("outPlotlyDownload12mSameGeoCont")
    		shinyjs::disable("outPlotlyDownload1mCanadaCont")
    		shinyjs::disable("outPlotlyDownload1mSameGeoCont")
    		shinyjs::disable("outReactableDownload12mChg")
    		shinyjs::disable("outReactableDownload1mChg")
    		shinyjs::disable("outReactableDownloadIndex")
    		shinyjs::disable("outReactableDownload12mCanadaCont")
    		shinyjs::disable("outReactableDownload12mSameGeoCont")
    		shinyjs::disable("outReactableDownload1mCanadaCont")
    		shinyjs::disable("outReactableDownload1mSameGeoCont")
    	} else {
    		shinyjs::enable("outPlotlyDownload12mChg")
    		shinyjs::enable("outPlotlyDownload1mChg")
    		shinyjs::enable("outPlotlyDownloadIndex")
    		shinyjs::enable("outPlotlyDownload12mCanadaCont")
    		shinyjs::enable("outPlotlyDownload12mSameGeoCont")
    		shinyjs::enable("outPlotlyDownload1mCanadaCont")
    		shinyjs::enable("outPlotlyDownload1mSameGeoCont")
    		shinyjs::enable("outReactableDownload12mChg")
    		shinyjs::enable("outReactableDownload1mChg")
    		shinyjs::enable("outReactableDownloadIndex")
    		shinyjs::enable("outReactableDownload12mCanadaCont")
    		shinyjs::enable("outReactableDownload12mSameGeoCont")
    		shinyjs::enable("outReactableDownload1mCanadaCont")
    		shinyjs::enable("outReactableDownload1mSameGeoCont")

    		output$outTextTableSelectedStatTitle <- renderText({ input$inTabsetPanelGraph })
    	}

     fMessage("Run after", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))
     
     shinyjs::show(id = "inRadioRebase")
     shinyjs::show(id = "inCheckBoxDisplaySeries")
     
    	waiter::waiter_hide()

    	return(lQueryResult)
  })


  iRebase         <- shiny::reactive(input$inRadioRebase)
  vcSelPlotSeries <- shiny::reactive(input$inCheckBoxDisplaySeries)


  shiny::observeEvent(input$inCheckBoxDisplaySeries, {
    if(length(input$inCheckBoxDisplaySeries) > iDefaultMaxSeriesCount){
      updateCheckboxGroupInput(session, "inCheckBoxDisplaySeries", selected = tail(input$inCheckBoxDisplaySeries, iDefaultMaxSeriesCount))
    }
  })


  # done this way to better align title within box
  output$uiOutQueryStatusTitle <- renderUI({
    shiny::fluidRow(style = "padding-bottom: 0px; padding-left: 0px; padding-right: 0px;",
      shiny::column(10, align = "left", h2(fGetEnFrText("QueryStatusTextboxLabel"))),
      shiny::column(2, style = "padding-left: 0px; padding-right: 0px;", id = "buttons",
        actionButton("toggleBox", span(id = "toggleLabel", fGetEnFrText("BoxCollapseLabel")), class = "btnToggle", `aria-expanded` = "true", `aria-controls` = "contentBox", style = "padding-left: 20px;")),
    br())
  })


  # done this way to better align title within box
 	output$uiOutRebaseBoxTitle <- renderUI({
 	  shiny::fluidRow(style = "padding-left: 0px; padding-right: 0px;",
 	    shiny::column(10, align = "left",
 	      div(style="display: inline-block;", h2(fGetEnFrText("RebaseOptionLabel"))),
 	      div(style="display: inline-block;", withTags({div(a(id="inToolTipRebase", href = "#centred-popup", style="text-decoration: none", `aria-controls`="centred-popup", class="wb-lbx wb-init wb-lbx-inited", role="button", `aria-label`="Information for rebase options",
 	                                                       span(style="margin-top: 0.5em; color: #26374a;", class="glyphicon glyphicon-info-sign", `aria-hidden`="true") )) }))),
 	    shiny::column(2, style = "padding-left: 0px; padding-right: 0px;", id = "buttons",
 	      actionButton("toggleBox", span(id = "toggleLabel", fGetEnFrText("BoxCollapseLabel")), class = "btnToggle", `aria-expanded` = "true", `aria-controls` = "contentBox", style = "padding-left: 20px;")),
 	  br())
  })


  # done this way to better align title within box
 	output$uiOutCompSeriesBoxTitle <- renderUI({
 	  shiny::fluidRow(style = "padding-left: 0px; padding-right: 0px;",
 	    shiny::column(10, align = "left",
 	                  div(style="display: inline-block;", h2(fGetEnFrText("DisplaySeriesOptionLabel"))),
 	                  div(style="display: inline-block;", withTags({div(a(id="inToolTipDispCompSeries", href = "#centred-popup", style="text-decoration: none", `aria-controls`="centred-popup", class="wb-lbx wb-init wb-lbx-inited", role="button", `aria-label`="Information for series display options",
 	                                                                     span(style="margin-top: 0.5em; color: #26374a;", class="glyphicon glyphicon-info-sign", `aria-hidden`="true") )) }))),
 	    shiny::column(2, id = "buttons", actionButton("toggleBox", span(id = "toggleLabel", fGetEnFrText("BoxCollapseLabel")), class = "btnToggle", `aria-expanded` = "true", `aria-controls` = "contentBox", style = "padding-left: 20px; align: right !important;")),
 	  br())
 	})


  # Create lQueryResultArranged to use in graphs, tables and downloads
  lQueryResultUnformatted <- shiny::reactive({
  	if (!is.null(lQueryResult())) {
  		iQueryRowCount <- nrow(lQueryResult()$dfQueryResult)
  		if (iQueryRowCount > 0 & !is.null(iQueryRowCount) ) {
  			nSel                 <- input$inRadioCustAggSeries
  			vcSpaggGeo           <- unique(lQueryResult()$dfSpaggComponents$geo)
  			iNumGeo              <- length(vcSpaggGeo)
  			vcSpaggProdCode      <- unique(lQueryResult()$dfSpaggComponents$product_code)
  			iNumProd             <- length(vcSpaggProdCode)
  			vcSpaggProdGeo       <- unique(lQueryResult()$dfSpaggComponents$prod_geo)
  			iNumProdGeo          <- length(vcSpaggProdGeo)
  			cSelectedStatistic   <- input$inTabsetPanelGraph
  			cSelectedStatVarName <- fGetVarNameFromEnFrText(cSelectedStatistic)
  			bUseRebased          <- ifelse(iRebase() == 1, FALSE, TRUE)

  			dfQueryResultRenamedUnformatted <- lQueryResult()$dfQueryResult
  			names(dfQueryResultRenamedUnformatted)[14:20] <- c(
  				fGetEnFrText('StatisticIndex'),
  				fGetEnFrText('Statistic12mChg'),
  				fGetEnFrText('Statistic1mChg'),
  				fGetEnFrText('Statistic12mCanadaCont'),
  				fGetEnFrText('Statistic12mSameGeoCont'),
  				fGetEnFrText('Statistic1mCanadaCont'),
  				fGetEnFrText('Statistic1mSameGeoCont'))

  			if        (nSel == 1) {
  				if (all(vcSpaggGeo == "Canada") | all(vcSpaggProdCode == "c0")) {
  					viCustSeries <- c(1, 4)
  				} else {
  					viCustSeries <- c(1, 4, 5)
  				}
  			} else if (nSel == 2) {
  				if (all(vcSpaggGeo == "Canada")) {
  					viCustSeries <- c(2, 4)
  				} else if (all(vcSpaggProdCode == "c0")) {
  					viCustSeries <- c(2, 4, 5)
  				}	else {
  					viCustSeries <- c(2, 3, 4, 5)
  				}
  			} else if (nSel == 3) {
  				if (all(vcSpaggGeo == "Canada") | all(vcSpaggProdCode == "c0")) {
  					viCustSeries <- c(1, 2, 4)
  				} else {
  					viCustSeries <- c(1, 2, 3, 4, 5)
  				}
  			}
  			if (cSelectedStatVarName %in% c("Statistic12mSameGeoCont", "Statistic1mSameGeoCont") & all(vcSpaggGeo != "Canada")) {
 			 	  viCustSeries <- viCustSeries[!viCustSeries %in% c(2, 4)]
  			}

  			# decide which rebased series to use
  			# changed 2024-07-03 for series like Cell services with base period earlier than first weight period, and since all s1s should be rebased to 200704 at earliest
  			if (bUseRebased == FALSE) {
  				if      (iNumGeo == 1 & iNumProdGeo == 1) {dfCustSeriesBase <- data.frame(rebase_type = c("rebased", "rebased", "rebased", "published", "published"))}
  				else if (iNumGeo == 1 & iNumProdGeo > 1)  {dfCustSeriesBase <- data.frame(rebase_type = c("rebased", "rebased", "rebased", "published", "published"))}
  				else if (iNumGeo >  1 & iNumProdGeo > 1)  {dfCustSeriesBase <- data.frame(rebase_type = c("rebased", "rebased", "rebased", "published", "rebased"))}
  			} else {
  				dfCustSeriesBase <- data.frame(rebase_type = c("rebased", "rebased", "rebased", "rebased",   "rebased"))
  			}


     # make dataframe, 1 row for each of s1:s5, with cols for series name, rebase type and base period
  			dfCustSeries <- data.frame(cbind(data.frame("series2" = c("s1", "s2", "s3", "s4", "s5")),
  																	 dfCustSeriesBase)) |>
  				left_join(unique(dfQueryResultRenamedUnformatted[ , c("series2", "series", "rebase_type", "i_base_period")]),
  									by = c("series2", "rebase_type")) |>
  				arrange(series2)

     # make dataframe, 1 row for each selected cust agg component r1:rN, with cols for series name, rebase type and base period
  			dfRegSeries  <- dfQueryResultRenamedUnformatted |>
  				filter(substr(series2, 1, 1) == "r" & rebase_type == if_else(bUseRebased == FALSE, "published", "rebased")) |>
  				group_by(series2, rebase_type, series, i_base_period) |>
  				summarize(n = n(), .groups = "keep") |>
  				select(-n)

  			if (nrow(dfRegSeries) > 1) {
  			  dfAllSeries       <- rbind(dfCustSeries, dfRegSeries)
  			  dfCandidateSeries <- rbind(dfCustSeries[viCustSeries, ], dfRegSeries)
  			} else {
  			  dfAllSeries       <- dfCustSeries
  			  dfCandidateSeries <- dfCustSeries[viCustSeries, ]
  			}

  			if (length(rvCandidatePlotSeries$series) == 0) {
          updateCheckboxGroupInput(session, "inCheckBoxDisplaySeries", choices  = dfCandidateSeries$series, selected = dfAllSeries[viCustSeries, "series"])
  			  rvCandidatePlotSeries$series <- dfCandidateSeries$series
  			} else if (!identical(sort(unique(dfCandidateSeries$series)), sort(unique(rvCandidatePlotSeries$series)) )) {
          updateCheckboxGroupInput(session, "inCheckBoxDisplaySeries", choices  = dfCandidateSeries$series, selected = dfAllSeries[viCustSeries, "series"])
  			  rvCandidatePlotSeries$series <- dfCandidateSeries$series
  			}

     lQueryResultUnformatted <- list(
       dfQueryResultRenamedUnformatted = NULL,
       dfAllSeries                     = NULL,
       dfCandidateSeries               = NULL,
  				 viCustSeries                    = NULL,
       dfRegSeries                     = NULL,
  				 cSelectedStatistic              = NULL,
  				 cSelectedStatVarName            = NULL)
  			lQueryResultUnformatted$dfQueryResultRenamedUnformatted <- dfQueryResultRenamedUnformatted
  			lQueryResultUnformatted$dfAllSeries                     <- dfAllSeries
  			lQueryResultUnformatted$dfCandidateSeries               <- dfCandidateSeries
  			lQueryResultUnformatted$viCustSeries                    <- viCustSeries
  			lQueryResultUnformatted$dfRegSeries                     <- dfRegSeries
  		 lQueryResultUnformatted$cSelectedStatistic              <- cSelectedStatistic
  			lQueryResultUnformatted$cSelectedStatVarName            <- cSelectedStatVarName

  			# text for query results
  			output$outTextQueryStatus <- renderPrint({
  				if (cAppLanguage == "English") {writeLines(lQueryResult()$status_text_en)}
  				else                           {writeLines(lQueryResult()$status_text_fr)}

  				if (nSel %in% c(1, 3)) {
  					if (length(unique(dfAllSeries[dfAllSeries$series2 == "s1" & dfAllSeries$rebase_type == "published", "series"])) > 0){
  						cCustAggTextS1 <- paste0(fGetEnFrText('QueryStatusPart1Text'),
  																		 unique(dfAllSeries[dfAllSeries$series2 == "s1" & dfAllSeries$rebase_type == "published", "series"]),
  																		 fGetEnFrText('QueryStatusPart2Text'),
  																		 " ", unique(dfAllSeries[dfAllSeries$series2 == "s1" & dfAllSeries$rebase_type == "published", "i_base_period"]),
  																		 " ", fGetEnFrText('QueryStatusPart3S1Text'), "\n ",
  																		 paste(lQueryResult()$dfSpaggComponents$prod_geo, collapse = "\n "))
  					} else {
  						cCustAggTextS1 <- paste0(fGetEnFrText('QueryStatusPart1Text'),
  																		 unique(dfAllSeries[dfAllSeries$series2 == "s1" & dfAllSeries$rebase_type == "rebased", "series"]),
  																		 fGetEnFrText('QueryStatusPart2Text'),
  																		 " ", unique(dfAllSeries[dfAllSeries$series2 == "s1" & dfAllSeries$rebase_type == "rebased", "i_base_period"]),
  																		 " ", fGetEnFrText('QueryStatusPart3S1Text'), "\n ",
  																		 paste(lQueryResult()$dfSpaggComponents$prod_geo, collapse = "\n "))
  					}
  					writeLines(paste0("\n", cCustAggTextS1))
  				}
  				if (nSel %in% c(2, 3)) {
  					cCustAggTextS2 <- paste0(fGetEnFrText('QueryStatusPart1Text'),
  																	 unique(dfAllSeries[dfAllSeries$series2 == "s2" & dfAllSeries$rebase_type == "rebased", "series"]),
  																	 fGetEnFrText('QueryStatusPart2Text'),
  																	 " ", unique(dfAllSeries[dfAllSeries$series2 == "s2" & dfAllSeries$rebase_type == "rebased", "i_base_period"]),
  																	 " ", fGetEnFrText('QueryStatusPart3S2Text'), "\n ",
  																	 paste(lQueryResult()$dfSpaggComponents$prod_geo, collapse = "\n "))
  					writeLines(paste0("\n", cCustAggTextS2))

  					if (!is.na(dfAllSeries[dfAllSeries$series2 == "s3" & dfAllSeries$rebase_type == "rebased", "series"])) {
  						if (length(unique(dfAllSeries[dfAllSeries$series2 == "s3" & dfAllSeries$rebase_type == "rebased", "series"])) > 0){
  							if (unique(dfAllSeries[dfAllSeries$series2 == "s2" & dfAllSeries$rebase_type == "rebased", "series"])        != unique(dfAllSeries[dfAllSeries$series2 == "s3" & dfAllSeries$rebase_type == "rebased", "series"]) |
  									unique(dfAllSeries[dfAllSeries$series2 == "s2" & dfAllSeries$rebase_type == "rebased", "i_base_period"]) != unique(dfAllSeries[dfAllSeries$series2 == "s3" & dfAllSeries$rebase_type == "rebased", "i_base_period"])) {
  								cCustAggTextS3 <- paste0("\n", fGetEnFrText('QueryStatusPart1Text'),
  																				 unique(dfAllSeries[dfAllSeries$series2 == "s3" & dfAllSeries$rebase_type == "rebased", "series"]),
  																				 fGetEnFrText('QueryStatusPart2Text'),
  																				 " ", unique(dfAllSeries[dfAllSeries$series2 == "s3" & dfAllSeries$rebase_type == "rebased", "i_base_period"]),
  																				 " ", fGetEnFrText('QueryStatusPart3S3AText'), "\n ",
  																				 paste0(vcSpaggGeo, collapse = "\n "),
  																				 "\n", fGetEnFrText('QueryStatusPart3S3BText'), "\n ",
  																				 paste(lQueryResult()$dfSpaggComponents$prod_geo, collapse = "\n "))
  								writeLines(paste0("\n", cCustAggTextS3))
  							}
  						}
  					}
  				}
  			})
  			return(lQueryResultUnformatted)
  		}
  	} # !is.null( lQueryResult())
  })


  # Create lQueryResultArranged to use in graphs, tables and downloads
  lQueryResultArranged <- shiny::reactive({
  	if (!is.null(lQueryResultUnformatted())) {
  		dfQueryResultRenamedUnformatted <- lQueryResultUnformatted()$dfQueryResultRenamedUnformatted
  		dfAllSeries                     <- lQueryResultUnformatted()$dfAllSeries
  		dfCandidateSeries               <- lQueryResultUnformatted()$dfCandidateSeries
  		viCustSeries                    <- lQueryResultUnformatted()$viCustSeries
  		dfRegSeries                     <- lQueryResultUnformatted()$dfRegSeries
  		cSelectedStatistic              <- lQueryResultUnformatted()$cSelectedStatistic
  		cSelectedStatVarName            <- lQueryResultUnformatted()$cSelectedStatVarName

  		iQueryRowCount <- nrow(dfQueryResultRenamedUnformatted)
  		if (iQueryRowCount > 0 & !is.null(iQueryRowCount) ) {

  			 vcSelPlotSeriesSeries2 <- dfCandidateSeries[which(dfCandidateSeries$series %in% vcSelPlotSeries()), "series2"]
  			 viSelPlotSeriesRow     <- which(dfAllSeries$series2 %in% vcSelPlotSeriesSeries2)
        if (length(viSelPlotSeriesRow) == 0) {viSeries <- viCustSeries
        } else                               {viSeries <- viSelPlotSeriesRow}
  			 dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformatted |>
  				  select(reference_period, series2, rebase_type, all_of(cSelectedStatistic)) |>
  				  inner_join(dfAllSeries |> select(-"series"),
  									   by = c("series2", "rebase_type")) |>
  				  arrange(reference_period, series2) |>
  				  select(-c(rebase_type, i_base_period)) |>
  				  spread(series2, all_of(cSelectedStatistic))

   			vcColNames <- colnames(dfQueryResultRenamedUnformattedSelectedT)
  		  dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT |>
  				  mutate(s3               = ifelse("s3" %in% vcColNames, s3, as.numeric(NA)),
  							     reference_period = substr(reference_period, 1, 7) )

  			 if (nrow(dfRegSeries) > 1) {
  				  dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT |>
  				    select(reference_period, s1, s2, s3, s4, s5, all_of(dfRegSeries$series2))
  			  } else {
  				  dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT |>
  				    select(reference_period, s1, s2, s3, s4, s5)
  			  }

 				  dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT[ , c(1, viSeries + 1)]
  			  dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT[complete.cases(dfQueryResultRenamedUnformattedSelectedT), ]

  			  # format table for language
  			  dfQueryResultRenamedFormattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT |>
  				   rename(ref_per = reference_period) |>
  				   select(ref_per, everything()) |>
  				   arrange(desc(ref_per))

  			  iCol <- ncol(dfQueryResultRenamedFormattedSelectedT)
  			  if (cAppLanguage == "English") {
  				   if (  cSelectedStatVarName == "Statistic12mCanadaCont" | cSelectedStatVarName == "Statistic12mSameGeoCont"
  						     | cSelectedStatVarName == "Statistic1mCanadaCont"  | cSelectedStatVarName == "Statistic1mSameGeoCont") {
  					    dfQueryResultRenamedFormattedSelectedT[, 2:iCol] <- format(dfQueryResultRenamedFormattedSelectedT[, 2:iCol], decimal.mark = '.', big.mark = ',', scientific = F, nsmall = 2)
  				   } else {
  					    dfQueryResultRenamedFormattedSelectedT[, 2:iCol] <- format(dfQueryResultRenamedFormattedSelectedT[, 2:iCol], decimal.mark = '.', big.mark = ',', scientific = F, nsmall = 1)
  				   }
  			  } else {
  				   if (  cSelectedStatVarName == "Statistic12mCanadaCont" | cSelectedStatVarName == "Statistic12mSameGeoCont"
  						     | cSelectedStatVarName == "Statistic1mCanadaCont"  | cSelectedStatVarName == "Statistic1mSameGeoCont") {
  					    dfQueryResultRenamedFormattedSelectedT[, 2:iCol] <- format(dfQueryResultRenamedFormattedSelectedT[, 2:iCol], decimal.mark = ',', big.mark = ' ', scientific = F, nsmall = 2)
  				   } else {
  					    dfQueryResultRenamedFormattedSelectedT[, 2:iCol] <- format(dfQueryResultRenamedFormattedSelectedT[, 2:iCol], decimal.mark = ',', big.mark = ' ', scientific = F, nsmall = 1)
  				   }
  			  }
  			  names(dfQueryResultRenamedFormattedSelectedT)[2:dim(dfQueryResultRenamedFormattedSelectedT)[2]] <- dfAllSeries[viSeries, 3]
  			  for (i in 2:(length(viSeries) + 1)) {
  				   dfQueryResultRenamedFormattedSelectedT[ , i] <- ifelse(trimws(dfQueryResultRenamedFormattedSelectedT[ , i]) == 'NA', '..', dfQueryResultRenamedFormattedSelectedT[ , i])
  			  }
  			  dfQueryResultRenamedFormattedSelectedT2 <- dfQueryResultRenamedFormattedSelectedT
  			  names(dfQueryResultRenamedFormattedSelectedT2)[1] <- fGetEnFrText("RefPeriodText")

       lQueryResultArranged <- list(
         dfQueryResultRenamedUnformattedSelectedT = NULL,
  					  dfQueryResultRenamedFormattedSelectedT = NULL,
  					  dfQueryResultRenamedFormattedSelectedT2 = NULL,
  					  dfAllSeries = NULL,
  					  viSeries = NULL,
  					  cSelectedStatVarName = NULL)
  			  lQueryResultArranged$dfQueryResultRenamedUnformattedSelectedT <- dfQueryResultRenamedUnformattedSelectedT
  			  lQueryResultArranged$dfQueryResultRenamedFormattedSelectedT   <- dfQueryResultRenamedFormattedSelectedT
  			  lQueryResultArranged$dfQueryResultRenamedFormattedSelectedT2  <- dfQueryResultRenamedFormattedSelectedT2
  			  lQueryResultArranged$dfAllSeries                              <- dfAllSeries
  			  lQueryResultArranged$viSeries                                 <- viSeries
  			  lQueryResultArranged$cSelectedStatVarName                     <- cSelectedStatVarName

  			  return(lQueryResultArranged)
  		  }
  	 } # !is.null( lQueryResult())
  })


  # call graph functions
  plotlyGraph <- shiny::reactive({if (!is.null(lQueryResultArranged())) {fPlotTimeSeries(lQueryResultArranged()$dfQueryResultRenamedUnformattedSelectedT, lQueryResultArranged()$dfAllSeries, lQueryResultArranged()$viSeries, lQueryResultArranged()$cSelectedStatVarName, dfSeriesFormats) } })


  # render graph objects
  output$outPlotly12mChg         <- plotly::renderPlotly({plotlyGraph()})
  output$outPlotly1mChg          <- plotly::renderPlotly({plotlyGraph()})
  output$outPlotlyIndex          <- plotly::renderPlotly({plotlyGraph()})
  output$outPlotly12mCanadaCont  <- plotly::renderPlotly({plotlyGraph()})
  output$outPlotly12mSameGeoCont <- plotly::renderPlotly({plotlyGraph()})
  output$outPlotly1mCanadaCont   <- plotly::renderPlotly({plotlyGraph()})
  output$outPlotly1mSameGeoCont  <- plotly::renderPlotly({plotlyGraph()})


  # enable download of plot objects
  output$outPlotlyDownload12mChg         <- shiny::downloadHandler(filename = paste0(fGetEnFrText('Download12mChg'),         ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )
  output$outPlotlyDownload1mChg          <- shiny::downloadHandler(filename = paste0(fGetEnFrText('Download1mChg'),          ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )
  output$outPlotlyDownloadIndex          <- shiny::downloadHandler(filename = paste0(fGetEnFrText('StatisticIndex'),         ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )
  output$outPlotlyDownload12mCanadaCont  <- shiny::downloadHandler(filename = paste0(fGetEnFrText('Download12mCanadaCont'),  ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )
  output$outPlotlyDownload12mSameGeoCont <- shiny::downloadHandler(filename = paste0(fGetEnFrText('Download12mSameGeoCont'), ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )
  output$outPlotlyDownload1mCanadaCont   <- shiny::downloadHandler(filename = paste0(fGetEnFrText('Download1mCanadaCont'),   ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )
  output$outPlotlyDownload1mSameGeoCont  <- shiny::downloadHandler(filename = paste0(fGetEnFrText('Download1mSameGeoCont'),  ".html"), content = function(file) {htmlwidgets::saveWidget(plotlyGraph(), file)} )


  # output dataframe of selected statistic to table
  output$outReactable12mChg         <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})
  output$outReactable1mChg          <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})
  output$outReactableIndex          <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})
  output$outReactable12mCanadaCont  <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})
  output$outReactable12mSameGeoCont <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})
  output$outReactable1mCanadaCont   <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})
  output$outReactable1mSameGeoCont  <- reactable::renderReactable(if (!is.null(lQueryResultArranged())) {reactable::reactable(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT, defaultColDef = reactable::colDef(headerVAlign = "center", filterable = FALSE, align = "right", vAlign = "center"), columns = list(ref_per = reactable::colDef(name = fGetEnFrText("RefPeriodText"), align = "left")),resizable = TRUE, compact = TRUE, outlined = TRUE, bordered = TRUE, pagination = FALSE, theme = reactable::reactableTheme(headerStyle = list(style = list(fontSize = "1.2rem") )) )})


  # download table of selected statistic
  output$outReactableDownload12mChg         <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('Download12mChg'),         ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})
  output$outReactableDownload1mChg          <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('Download1mChg'),          ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})
  output$outReactableDownloadIndex          <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('StatisticIndex'),         ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})
  output$outReactableDownload12mCanadaCont  <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('Download12mCanadaCont'),  ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})
  output$outReactableDownload12mSameGeoCont <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('Download12mSameGeoCont'), ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})
  output$outReactableDownload1mCanadaCont   <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('Download1mCanadaCont'),   ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})
  output$outReactableDownload1mSameGeoCont  <- shiny::downloadHandler(filename = function() {paste0(fGetEnFrText('Download1mSameGeoCont'),  ".csv")}, content = function(file) {write.csv(lQueryResultArranged()$dfQueryResultRenamedFormattedSelectedT2, file, row.names = FALSE)})

}




shinyApp(ui, server)

