library("shiny")
library("dplyr")
library("tidyr")

rm(list=ls())


cAppLanguage    <<- "English"

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


# get CODR weights
#dfCODRWeightAll <- data.frame(table_18100007_vector = 1234, weight_reference_period = "2026-01-01", weight_r = 23.4, weight_version = "original")
dfCODRWeightAll <<- as.data.frame(readr::read_csv(archive::archive_read("https://www150.statcan.gc.ca/n1/tbl/csv/18100007-eng.zip", file = 1), show_col_types = FALSE)) |>
 	select(table_18100007_vector   = VECTOR,
         weight_reference_period = REF_DATE,
         weight_r                = VALUE) |>
  mutate(table_18100007_vector   = as.integer(substr(table_18100007_vector, 2, nchar(table_18100007_vector))),
         weight_reference_period = paste0(weight_reference_period, "-01-01"),
  			    weight_version          = ifelse(weight_reference_period == "2001-01-01", "revised", "original"))


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


# Set index ref period ranges and get index data
# set index reference period start
iFirstIndexYear      <- 2004
iFirstIndexPeriod    <- 07
cFirstIndexYearMonth <- paste0(as.character(iFirstIndexYear), substr(as.character(100 + iFirstIndexPeriod), 2, 3))


# get index data from ZIP
#dfCODRIndexAll <- data.frame(table_18100004_vector = 1234, reference_period = "2025-01-01", index_r = 123.4)
dfCODRIndexAll <- as.data.frame(readr::read_csv(archive::archive_read("https://www150.statcan.gc.ca/n1/tbl/csv/18100004-eng.zip", file = 1), show_col_types = FALSE)) |>
  filter(REF_DATE >= cFirstIndexYearMonth) |>
  select(table_18100004_vector = VECTOR,
         reference_period      = REF_DATE,
         index_r               = VALUE) |>
  mutate(table_18100004_vector = as.integer(substr(table_18100004_vector, 2, nchar(table_18100004_vector))),
         reference_period = paste0(reference_period, "-01"))


# derive index reference period end
iLastIndexYear           <- as.integer(substr(as.character(max(dfCODRIndexAll$reference_period)), 1, 4))
iLastIndexPeriod         <- as.integer(substr(as.character(max(dfCODRIndexAll$reference_period)), 6, 7))
cLastIndexRefPeriod      <<- paste0(as.character(iLastIndexYear),  "-", substr(as.character(iLastIndexPeriod  + 100), 2, 3), "-01")
cFirstIndexDisplayPeriod <<- "2007-04-01"


# Set default base periods
cDefaultStartBasePeriod     <<- "2007-04-01"
cDefaultEndBasePeriod       <<- "2007-04-01"
cDefaultBasePeriod          <<- "200704=100"


# Set weight ref period ranges and get weight data
cFirstWeightRefPeriod       <<- "2001-01-01"
cFirstWeightEffectivePeriod <<- "2004-07-01"
cFirstWeightUsablePeriod    <<- "2007-05-01"
cMaxWeightRefPeriod         <<- max(dfBasket$weight_reference_period)



# Create vector and dataframe of all ref periods
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


# Set default number of series to plot
iDefaultMaxSeriesCount <<- 8

rm(i, y, m, iFirstIndexYear, iFirstIndexPeriod, iLastIndexYear, iLastIndexPeriod)


source("Functions.R")



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
      h1                                 {font-family: 'Noto Sans' !important; font-size: 150% !important; margin: 0px 0px !important; text-align: center !important; ; border-bottom: 0px !important}
      h2                                 {font-family: 'Noto Sans' !important; font-size: 120% !important; margin: 0px 0px !important;}
      h3                                 {font-family: 'Noto Sans' !important; font-size: 100% !important; margin: 0px 0px !important;}
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
   		      shiny::column(12, shiny::radioButtons(inputId = "inRadioCustAggSeries", label = div(fGetEnFrText("CustAggRadioButtonLabel"),  style = "font-weight: bold;"),
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
          shiny::column(4, style = "height: 100px;                    padding-right: 2px;", div(shiny::selectInput("inSelPopularAggGeo",  label = div(HTML(fGetEnFrText("CustAggGroupGeoText")),  style = "font-size: 80%; font-weight: normal;"), width = "100%",
            choices = c("", unique(dfPopularAggDefn$indented_geography)), selected = "", multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px; height: 100px !important;")),
          shiny::column(6, style = "height: 100px; padding-left: 2px; padding-right: 0px;", div(shiny::selectInput("inSelPopularAggProd", label = div(HTML(fGetEnFrText("CustAggGroupProdText")), style = "font-size: 80%; font-weight: normal;"), width = "100%",
            choices = c("", dfPopularAggDefn$indented_product),   selected = "", multiple = FALSE, selectize = FALSE), style = "margin-bottom: 0px;")),
          shiny::column(2, style = "height: 100px;", id = "buttons", shiny::actionButton("inBtnApplyPopularAgg", label = fGetEnFrText("ApplyPopularAggButtonLabel"), class = "btnAction"))),
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
  rvTermsAccepted       <- shiny::reactiveValues(terms_accepted = FALSE)
  rvCandidatePlotSeries <- shiny::reactiveValues(series = NULL)


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


 	# Warn user if weight metadata is out-of-date
  if (max(dfCODRWeightAll$weight_reference_period) > cMaxWeightRefPeriod) {
  	 shiny::showModal(modalDialog(title = HTML(fGetEnFrText("UpdateNeededTitleText")),  tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("UpdateNeededText")),  easyClose = FALSE, footer = modalButton(fGetEnFrText("ToolTipMessageButtonCloseLabel"))) )
  }

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
  	  vRef                    <- substr(vRefPeriod, 1, 7)
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
  	    vRef                    <- substr(vRefPeriod, 1, 7)
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


  # Ask user to accept Terms of Use before Run. If Refuse -> no action, if Accept -> Click Run
  shiny::observeEvent(input$modalBtnAcceptTermsForRun, {
    shiny::removeModal()
   	rvTermsAccepted$terms_accepted <- TRUE
    shinyjs::click("inBtnRun")
    shinyjs::show(id = "inRadioRebase")
    shinyjs::show(id = "inCheckBoxDisplaySeries")
  })


  # on Run button click, create Query Result list that can be reused throughout server function
  lQueryResult <- shiny::eventReactive(input$inBtnRun, {
    fMessage("Run before", paste0("rvCompCount$n: ", rvCompCount$n, ", rvCompLastID$n: ", rvCompLastID$n, ", rvCompDispIDs$n: ", paste(rvCompDispIDs$n, collapse = ", "), ", length(rvSavedSel$sel_geo): ", length(rvSavedSel$sel_geo), ", ", paste0(paste(as.character(rvSavedSel$comp_id), rvSavedSel$sel_geo, rvSavedSel$sel_prod, sep = "/"), collapse = ", ")))

    if (rvTermsAccepted$terms_accepted == FALSE) {
      #     shiny::showModal(modalDialog(title = HTML(fGetEnFrText("Statistic1mSameGeoContToolTipTitleText")),  tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("Statistic1mSameGeoContToolTipText")),  easyClose = FALSE, footer = tagList(actionButton("inBtnCloseToolTip1mSameGeoCont",  fGetEnFrText("ToolTipMessageButtonCloseLabel"))) ) ) )
      #    	shiny::showModal(modalDialog(h3(fGetEnFrText("TermsOfUseHeaderText")), br(), HTML(fGetEnFrText("TermsOfUseText")), footer = tagList(actionButton("modalBtnAcceptTermsForRun", fGetEnFrText("TermsOfUseButtonAcceptLabel")), modalButton(fGetEnFrText("TermsOfUseButtonRefuseLabel")) ) ) )
        shiny::showModal(modalDialog(title = HTML(fGetEnFrText("TermsOfUseHeaderText")),  tags$div(id = "modal-focus-start", tabindex = "-1"), HTML(fGetEnFrText("TermsOfUseText")),  easyClose = FALSE, footer = tagList(actionButton("modalBtnAcceptTermsForRun", fGetEnFrText("TermsOfUseButtonAcceptLabel")), modalButton(fGetEnFrText("TermsOfUseButtonRefuseLabel")) ) ) )
    } else { # rvTermsAccepted$terms_accepted == TRUE
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
    		lQueryResult <- fIndexWeightChgCont(dfBasket, dfSeriesReg, dfSeriesSpagg, dfCODRIndexAll, dfCODRWeightAll, dfRefPeriods, vSelectedCustAggRows, rvBasePeriod$base_start, rvBasePeriod$base_end)

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
    	waiter::waiter_hide()

    	return(lQueryResult)
    } #rvTermsAccepted$terms_accepted == TRUE

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

