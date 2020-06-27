# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 Class DataFileSAMICO22019 ####

#
#' @export
#'
#' @title 
#'   A DataFileSAMICO22019 file class (R6 Class)
#'
#' @description 
#'   Provides utilities for managing data in a sAMI-CO2 data file,
#'   with the file structure typical in 2019.
#'
DataFileSAMICO22019 <- R6Class(
   classname = "DataFileSAMICO22019",
   public = list(
      
      #' @field filePath
      #'   character string representing the path to the file
      filePath = NULL,
      
      #' @field timeZone
      #'   character string representing the time zone (system dependent)
      timeZone = NULL,

      #' @field timeFormat
      #'   character string representing the time format (strptime)
      timeFormat = NULL,
      
      #' @field signal
      #'   signal containing data from the file
      signal = NULL,
      
      #' @field numMetaRows
      #'   integer representing the number of rows of metadata at the top
      #'   of the file
      numMetaRows = NULL,

      #' @field metaColumns
      #'   Data frame with context for each column
      metaColumns = NULL,

      # Method DataFileSAMICO22019$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class DataFileSAMICO22019
      #'   
      #' @param filePath
      #'   Character string representing the path to the file
      #' @param timeZone
      #'   Optional character string representing the time zone.
      #'   Defaults to "UTC".
      #' @param timeFormat
      #'   Character string representing the time format (strptime) of the time column.
      #'   Defaults to "month/day/Year(4-digit) Hour(24-hour):minute"
      #' @param numMetaRows
      #'   Optional integer representing the number of rows of metadata at the top
      #'   of the file
      #'   Defaults to 1.
      #' @param metaColumns
      #'   Optional data frame with metadata about the columns.
      #'   Defaults to columns for a SAMI CO2 export with the columns
      #'   Year Day, Round Year, DateStr, TimeStr, Type Name, CO2, Temperature C,
      #'   Battery Voltage, DarkRef (Raw), DarkSig (Raw), 434Ratio (Raw), 434Ref (Raw),
      #'   434Sig (Raw), 620Ratio (Raw), 620Ref (Raw), 620Sig (Raw).
      #'   
      initialize = function
      (
         filePath,
         timeZone = "UTC",
         timeFormat = "%m/%d/%Y %H:%M",
         numMetaRows = 1,
         metaColumns = data.frame(
            # Columns
            dayOfYear = c("days UTC", "Time"),
            year = c("years CE UTC", "Time"),
            dateString = c("Gregorian UTC", "Time"),
            timeString = c("Hour(24):minute UTC", "Time"),
            recordType = c("Text", "Category"),
            pCO2 = c("microatmospheres", "Force per Area"),
            temp = c("degrees C", "Temperature"),
            batteryVoltage = c("volts", "Energy per Charge"),
            refDarkRaw = c("unknown", "unknown"),
            sigDarkRaw = c("unknown", "unknown"),	
            ratio434Raw = c("unknown", "unknown"),	
            ref434Raw = c("unknown", "unknown"),
            sig434Raw = c("unknown", "unknown"),
            ratio620Raw = c("unknown", "unknown"),
            ref620Raw = c("unknown", "unknown"),
            sig620Raw = c("unknown", "unknown"),
            
            # Parameters
            row.names = c("units", "dimensions"),
            stringsAsFactors = FALSE
         )
      ) 
      {
         self$filePath <- filePath;
         self$timeZone <- timeZone;
         self$timeFormat <- timeFormat;
         self$numMetaRows <- numMetaRows;
         self$metaColumns <- metaColumns;
      },
      
      # Method DataFileSAMICO22019$read ####
      #
      #' @description 
      #'   Read the data file to populate the signal attribute
      #'   
      #' @param metadata
      #'   Optional reference to metadata object for adding variables.
      #'   Defaults to NULL.
      #' @param instrument
      #'   Optional character string naming the instrument used.
      #'   Defaults to "Sunburst SAMI CO2".
      #' @param dataLayer
      #'   Optional character string naming the associated datalayer in the metadata.
      #'   Defaults to "root"
      #' @param stringsAsFactors
      #'   Optional logical value to turn strings as factors on or off.
      #'   Default value is FALSE (do not interpret strings as categorical factors).
      #' @param recordType
      #'   Optional parameter for designating the record type to include in the signal.
      #'   Defaults to "CO2Aver+".
      #'   
      #' @return 
      #'   Reference to the signal object created from data in the file.
      #'   
      read = function
      (
         metadata = NULL,
         instrument = "Sunburst SAMI CO2",
         dataLayer = "root",
         stringsAsFactors = FALSE,
         recordType = "CO2Aver+"
      ) 
      {
         data <- read.table(
            file = self$filePath,
            sep = "\t",
            skip = self$numMetaRows,
            header = FALSE,
            col.names = names(self$metaColumns),
            stringsAsFactors = stringsAsFactors
         );
         
         if (!is.null(metadata)) {
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "dayOfYear",
               name = "Decimal day of the year",
               units = "days UTC",
               dimensions = "Time",
               stat = "instataneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "year",
               name = "Yers current era",
               units = "years CE UTC",
               dimensions = "Time",
               stat = "instataneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "date",
               name = "Date",
               units = "Gregorian UTC",
               dimensions = "Time",
               stat = "instataneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "timeOfDay",
               name = "Time of day",
               units = "Hour(24):minute UTC",
               dimensions = "Time",
               stat = "instataneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "pCO2",
               name = "Partial pressure of carbon dioxide",
               units = "microatmospheres",
               dimensions = "Force per Area",
               stat = "circa 5-min equilibration",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "temp",
               name = "Water temperature",
               units = "degrees C",
               dimensions = "Temperature",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "batteryVoltage",
               name = "Power supply battery voltage",
               units = "volts",
               dimensions = "Energy per Charge",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "refDarkRaw",
               name = "Dark reference",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "sigDarkRaw",
               name = "Dark signal",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "ratio434Raw",
               name = "Ratio for 434 nanometers",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "ref434Raw",
               name = "Reference for 434 nanometers",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "sig434Raw",
               name = "Signal for 434 nanometers",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "ratio620Raw",
               name = "Ratio for 620 nanometers",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "ref620Raw",
               name = "Reference for 620 nanometers",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "sig620Raw",
               name = "Signal for 620 nanometers",
               units = "unknown",
               dimensions = "unknown",
               stat = "instantaneous",
               instrument = instrument
            );
         }

         data <- data[(data$recordType == recordType),];
         
         data$time <- as.character(as.POSIXct(
            x = paste(data$dateString, data$timeString),
            tz = self$timeZone,
            format = self$timeFormat
         ));
         
         self$metaColumns$time <- c("Gregorian UTC", "Time");
         
         self$metaColumns["dataLayer",] <- dataLayer;
         
         self$signal <- SignalTable$new(
            data = data,
            meta = metadata,
            metaColumns = self$metaColumns,
            tz = self$timeZone
         );
         
         return(self$signal);
      },
      
      # Method DataFileSAMICO22019$plotCO2 ####
      #
      #' @description 
      #'   Plot the pCO2 data
      #'   
      #' @param signal
      #'   Optional signal to plot.
      #'   Defaults to the signal attribute of this object.
      #' @param mar
      #'   Optional vector of values to use as margins passed to par.
      #'   Defaults to c(4.5, 4.5, 1, 1).
      #' @param time
      #'   Optional vector for time axis.
      #'   Default value is the time associated with this object.
      #' @param timeZone
      #'   Optional time zone for time axis.
      #'   Default value is the time zone associated with this object.
      #' @param timeLab
      #'   Optional label for time axis.
      #'   Default value is Time with the time zone provided above.
      #' @param timeAxisFormat
      #'   Optional value for the strptime format for labels on the time axis
      #'   Default value is <Month abbrev.> <Day of month> <Line break> Hour(24-hour):minute
      #' @param timepadj
      #'   Optional adjustment (axis argument) to the location of the labels on the time axis.
      #'   Defaults to 0.5.
      #' @param ypCO2
      #'   Optional vector for the pCO2 to plot on y axis.
      #'   Default value is the pCO2 in the signal argument above.
      #' @param ylim
      #'   Optional limits for the pCO2 scale on the y axis.
      #'   Default value is the min and max of the ypCO2 argument
      #' @param ylab
      #'   Optional label for the y axis.
      #'   Default value is formatted CO2 partial pressure with units of microatmospheres
      #' @param ...
      #'   Optional additional parameters passed on to the call to plot.default
      #'   
      #' @return 
      #'   No defined return value. 
      #'   
      plotpCO2 = function
      (
         signal = self$signal,
         mar = c(4.5, 4.5, 1, 1),
         time = signal$time,
         timeZone = self$timeZone,
         timeLab = sprintf("Time (%s Time Zone)", timeZone),
         timeAxisFormat = "%e %b \n %H:%M",
         timepadj = 0.5,
         ypCO2 = signal$getVariable("pCO2"),
         ylim = c(
            min(ypCO2),
            max(ypCO2)
         ),
         ylab = bquote(CO[2] ~ partial ~ pressure ~ .("(") * mu * atm * .(")")),
         ...
      )
      {
         attributes(time)$tzone <- timeZone;
         par(mar = mar);
         plot.default( 
            x = time,
            xaxt = "n",
            xlab = timeLab,
            y = ypCO2,
            ylim = ylim,
            ylab = ylab,
            ...
         );
         axis.POSIXct(
            side = 1,
            x = time,
            padj = timepadj,
            format = timeAxisFormat
         );
      },
      
      # Method DataFileSAMICO22019$plotTemp ####
      #
      #' @description 
      #'   Plot the nitrate data
      #'   
      #' @param signal
      #'   Optional signal to plot.
      #'   Defaults to the signal attribute of this object.
      #' @param mar
      #'   Optional vector of values to use as margins passed to par.
      #'   Defaults to c(4.5, 4.5, 1, 1).
      #' @param time
      #'   Optional vector for time axis.
      #'   Default value is the time associated with this object.
      #' @param timeZone
      #'   Optional time zone for time axis.
      #'   Default value is the time zone associated with this object.
      #' @param timeLab
      #'   Optional label for time axis.
      #'   Default value is Time with the time zone provided above.
      #' @param timeAxisFormat
      #'   Optional value for the strptime format for labels on the time axis
      #'   Default value is <Month abbrev.> <Day of month> <Line break> Hour(24-hour):minute
      #' @param timepadj
      #'   Optional adjustment (axis argument) to the location of the labels on the time axis.
      #'   Defaults to 0.5.
      #' @param ytemp
      #'   Optional vector for the temperature to plot on the y axis.
      #'   Default value is the temperature from the signal argument above.
      #' @param ylim
      #'   Optional limits for the temperature axis.
      #'   Default value is the min and max of the ytemp argument
      #' @param ylab
      #'   Optional label for the y axis.
      #'   Default value is formatted Temperature in degrees Celsius
      #' @param ...
      #'   Optional additional parameters passed on to the call to plot.default
      #'   
      #' @return 
      #'   No defined return value. 
      #'   
      plotTemp = function
      (
         signal = self$signal,
         mar = c(4.5, 4.5, 1, 1),
         time = signal$time,
         timeZone = self$timeZone,
         timeLab = sprintf("Time (%s Time Zone)", timeZone),
         timeAxisFormat = "%e %b \n %H:%M",
         timepadj = 0.5,
         ytemp = signal$getVariable("temp"),
         ylim = c(
            min(ytemp),
            max(ytemp)
         ),
         ylab = bquote(Temperature ~ .("(") * degree * C * .(")")),
         ...
      )
      {
         attributes(time)$tzone <- timeZone;
         par(mar = mar);
         plot.default( 
            x = time,
            xaxt = "n",
            xlab = timeLab,
            y = ytemp,
            ylim = ylim,
            ylab = ylab,
            ...
         );
         axis.POSIXct(
            side = 1,
            x = time,
            padj = timepadj,
            format = timeAxisFormat
         );
      }
      
   )
)
