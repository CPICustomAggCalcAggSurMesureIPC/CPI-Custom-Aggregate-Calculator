#---------------------------------
# 0 Define global functions
# 1 fIndexWeightChgCont() Main function to prepare estimates
# 2 fPlotTimeSeries() function to prepare graphs
# 3 fGetDisplaySeries() function to prepare available series
# 4 fMessage() function to display messages
#---------------------------------






#---------------------------------
# 0 Define global functions
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
# 1 fIndexWeightChgCont() Main function to prepare estimates
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
fIndexWeightChgCont <- function(fvdfSeriesReg, fvdfSeriesSpagg, fvdfCODRIndexAll, fvdfCODRWeightAll, fvdfRefPeriods, fvvSpaggRow, fvcStartBasePer, fvcEndBasePer){
	
	
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

           from   dfBasket) b
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
# 2 fPlotTimeSeries() function to prepare graphs
#---------------------------------


fPlotTimeSeries <- function(fvdfData, fvdfSeries, fvviSeries, fvcSelectedStatVarName)	{
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
     color     = I(dfSeriesFormats[iFormat, 2]), 
     linetype  = I(dfSeriesFormats[iFormat, 3]))
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
# 3 fGetDisplaySeries() function to prepare available series
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
# 4 fMessage() function to display messages
#---------------------------------


fMessage <- function(fvcBlock, fvcMessage) {
  if      (length(dfMessages[dfMessages$block == fvcBlock, "show_message"]) == 0) {return(message(paste0(fvcBlock, " has no message")))}
  else if (dfMessages[dfMessages$block == fvcBlock, "show_message"] == 1)         {return(message(paste0(fvcBlock, ": ", fvcMessage)))}
	 else                                                                            {return("")}       
}


#fMessage('Restart before', "abcd")
