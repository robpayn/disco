# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 Class DataFileSUNA2019 ####

#
#' @export
#'
#' @title 
#'   A DataFileSUNA2019 file class (R6 Class)
#'
#' @description 
#'   Provides utilities for managing data in a SUNA data file,
#'   with the file structure typical in 2019.
#'
DataFileSUNA2019 <- R6Class(
   classname = "DataFileSUNA2019",
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
      
      #' @field numMetaRows
      #'   integer representing the number of rows of metadata at the top
      #'   of the file
      numMetaRows = NULL,
      
      #' @field signal
      #'   Signal object with data read from the file
      signal = NULL,
      
      #' @field metaColumns
      #'   Data frame with context for each column
      metaColumns = NULL,
      
      # Method DataFileSUNA2019$initialize ####
      #
      #' @description 
      #'   Called when a new instance of the class DataFileSUNA2019 is created
      #'   
      #' @param filePath
      #'   Character string representing the path to the file
      #' @param timeZone
      #'   Optional character string representing the time zone.
      #'   Defaults to "UTC".
      #' @param timeFormat
      #'   character string representing the time format (strptime)
      #'   Defaults to "month/day/Year(4-digit) Hour(24-hour):minute:second"
      #' @param numMetaRows
      #'   Optional integer representing the number of rows of metadata at the top
      #'   of the file.
      #'   Defaults to 4.
      #' @param metaColumns
      #'   Optional data frame providing context for the data columns.
      #'   Defaults to a definition of a standard SUNA exported table.
      #'   
      initialize = function
      (
         filePath,
         timeZone = "UTC",
         timeFormat = "%m/%d/%Y %H:%M:%S",
         numMetaRows = 1,
         metaColumns = data.frame(
            # Columns in the data frame
            frameSync = c("Text", "Category"),
            timeString = c("Gregorian UTC m/d/Y H:m:s", "Time"),
            nitrateMolar = c("micromoles nitrate per liter", "Count per Volume"),
            nitrateMass = c("milligrams nitrogen per liter", "Mass per Volume"),
            abs254 = c("a.u.", "Unknown"),
            abs350 = c("a.u.", "Unknown"),
            brTrace = c("milligrams per liter", "Mass per Volume"),
            specAvg = c("Unknown", "Unknown"),
            darkAvg = c("Unknown", "Unknown"),
            intFactor = c("Unknown", "Unknown"),
            uv190_64 = c("Unknown", "Unknown"),
            uv191_44 = c("Unknown", "Unknown"),
            uv192_24 = c("Unknown", "Unknown"),
            uv193_03 = c("Unknown", "Unknown"),
            uv193_83 = c("Unknown", "Unknown"),
            uv194_63 = c("Unknown", "Unknown"),
            uv195_43 = c("Unknown", "Unknown"),
            uv196_23 = c("Unknown", "Unknown"),
            uv197_03 = c("Unknown", "Unknown"),
            uv197_83 = c("Unknown", "Unknown"),
            uv198_63 = c("Unknown", "Unknown"),
            uv199_43 = c("Unknown", "Unknown"),
            uv200_23 = c("Unknown", "Unknown"),
            uv201_03 = c("Unknown", "Unknown"),
            uv201_83 = c("Unknown", "Unknown"),
            uv202_64 = c("Unknown", "Unknown"),
            uv203_44 = c("Unknown", "Unknown"),
            uv204_24 = c("Unknown", "Unknown"),
            uv205_04 = c("Unknown", "Unknown"),
            uv205_84 = c("Unknown", "Unknown"),
            uv206_65 = c("Unknown", "Unknown"),
            uv207_45 = c("Unknown", "Unknown"),
            uv208_25 = c("Unknown", "Unknown"),
            uv209_05 = c("Unknown", "Unknown"),
            uv209_86 = c("Unknown", "Unknown"),
            uv210_66 = c("Unknown", "Unknown"),
            uv211_46 = c("Unknown", "Unknown"),
            uv212_27 = c("Unknown", "Unknown"),
            uv213_07 = c("Unknown", "Unknown"),
            uv213_87 = c("Unknown", "Unknown"),
            uv214_68 = c("Unknown", "Unknown"),
            uv215_48 = c("Unknown", "Unknown"),
            uv216_29 = c("Unknown", "Unknown"),
            uv217_09 = c("Unknown", "Unknown"),
            uv217_90 = c("Unknown", "Unknown"),
            uv218_70 = c("Unknown", "Unknown"),
            uv219_51 = c("Unknown", "Unknown"),
            uv220_31 = c("Unknown", "Unknown"),
            uv221_12 = c("Unknown", "Unknown"),
            uv221_92 = c("Unknown", "Unknown"),
            uv222_73 = c("Unknown", "Unknown"),
            uv223_53 = c("Unknown", "Unknown"),
            uv224_34 = c("Unknown", "Unknown"),
            uv225_15 = c("Unknown", "Unknown"),
            uv225_95 = c("Unknown", "Unknown"),
            uv226_76 = c("Unknown", "Unknown"),
            uv227_57 = c("Unknown", "Unknown"),
            uv228_37 = c("Unknown", "Unknown"),
            uv229_18 = c("Unknown", "Unknown"),
            uv229_99 = c("Unknown", "Unknown"),
            uv230_79 = c("Unknown", "Unknown"),
            uv231_60 = c("Unknown", "Unknown"),
            uv232_41 = c("Unknown", "Unknown"),
            uv233_21 = c("Unknown", "Unknown"),
            uv234_02 = c("Unknown", "Unknown"),
            uv234_83 = c("Unknown", "Unknown"),
            uv235_64 = c("Unknown", "Unknown"),
            uv236_45 = c("Unknown", "Unknown"),
            uv237_25 = c("Unknown", "Unknown"),
            uv238_06 = c("Unknown", "Unknown"),
            uv238_87 = c("Unknown", "Unknown"),
            uv239_68 = c("Unknown", "Unknown"),
            uv240_49 = c("Unknown", "Unknown"),
            uv241_30 = c("Unknown", "Unknown"),
            uv242_10 = c("Unknown", "Unknown"),
            uv242_91 = c("Unknown", "Unknown"),
            uv243_72 = c("Unknown", "Unknown"),
            uv244_53 = c("Unknown", "Unknown"),
            uv245_34 = c("Unknown", "Unknown"),
            uv246_15 = c("Unknown", "Unknown"),
            uv246_96 = c("Unknown", "Unknown"),
            uv247_77 = c("Unknown", "Unknown"),
            uv248_58 = c("Unknown", "Unknown"),
            uv249_39 = c("Unknown", "Unknown"),
            uv250_20 = c("Unknown", "Unknown"),
            uv251_01 = c("Unknown", "Unknown"),
            uv251_82 = c("Unknown", "Unknown"),
            uv252_63 = c("Unknown", "Unknown"),
            uv253_44 = c("Unknown", "Unknown"),
            uv254_25 = c("Unknown", "Unknown"),
            uv255_06 = c("Unknown", "Unknown"),
            uv255_87 = c("Unknown", "Unknown"),
            uv256_68 = c("Unknown", "Unknown"),
            uv257_49 = c("Unknown", "Unknown"),
            uv258_30 = c("Unknown", "Unknown"),
            uv259_11 = c("Unknown", "Unknown"),
            uv259_92 = c("Unknown", "Unknown"),
            uv260_73 = c("Unknown", "Unknown"),
            uv261_54 = c("Unknown", "Unknown"),
            uv262_35 = c("Unknown", "Unknown"),
            uv263_16 = c("Unknown", "Unknown"),
            uv263_97 = c("Unknown", "Unknown"),
            uv264_78 = c("Unknown", "Unknown"),
            uv265_60 = c("Unknown", "Unknown"),
            uv266_41 = c("Unknown", "Unknown"),
            uv267_22 = c("Unknown", "Unknown"),
            uv268_03 = c("Unknown", "Unknown"),
            uv268_84 = c("Unknown", "Unknown"),
            uv269_65 = c("Unknown", "Unknown"),
            uv270_46 = c("Unknown", "Unknown"),
            uv271_27 = c("Unknown", "Unknown"),
            uv272_09 = c("Unknown", "Unknown"),
            uv272_90 = c("Unknown", "Unknown"),
            uv273_71 = c("Unknown", "Unknown"),
            uv274_52 = c("Unknown", "Unknown"), 
            uv275_33 = c("Unknown", "Unknown"),
            uv276_14 = c("Unknown", "Unknown"),
            uv276_95 = c("Unknown", "Unknown"),
            uv277_77 = c("Unknown", "Unknown"),
            uv278_58 = c("Unknown", "Unknown"),
            uv279_39 = c("Unknown", "Unknown"),
            uv280_20 = c("Unknown", "Unknown"),
            uv281_01 = c("Unknown", "Unknown"),
            uv281_82 = c("Unknown", "Unknown"),
            uv282_64 = c("Unknown", "Unknown"),
            uv283_45 = c("Unknown", "Unknown"),
            uv284_26 = c("Unknown", "Unknown"),
            uv285_07 = c("Unknown", "Unknown"),
            uv285_88 = c("Unknown", "Unknown"),
            uv286_69 = c("Unknown", "Unknown"),
            uv287_51 = c("Unknown", "Unknown"),
            uv288_32 = c("Unknown", "Unknown"),
            uv289_13 = c("Unknown", "Unknown"),
            uv289_94 = c("Unknown", "Unknown"),
            uv290_75 = c("Unknown", "Unknown"),
            uv291_56 = c("Unknown", "Unknown"),
            uv292_38 = c("Unknown", "Unknown"),
            uv293_19 = c("Unknown", "Unknown"),
            uv294_00 = c("Unknown", "Unknown"),
            uv294_81 = c("Unknown", "Unknown"),
            uv295_62 = c("Unknown", "Unknown"),
            uv296_43 = c("Unknown", "Unknown"),
            uv297_25 = c("Unknown", "Unknown"),
            uv298_06 = c("Unknown", "Unknown"),
            uv298_87 = c("Unknown", "Unknown"),
            uv299_68 = c("Unknown", "Unknown"),
            uv300_49 = c("Unknown", "Unknown"),
            uv301_30 = c("Unknown", "Unknown"),
            uv302_11 = c("Unknown", "Unknown"),
            uv302_93 = c("Unknown", "Unknown"),
            uv303_74 = c("Unknown", "Unknown"),
            uv304_55 = c("Unknown", "Unknown"),
            uv305_36 = c("Unknown", "Unknown"),
            uv306_17 = c("Unknown", "Unknown"),
            uv306_98 = c("Unknown", "Unknown"),
            uv307_79 = c("Unknown", "Unknown"),
            uv308_60 = c("Unknown", "Unknown"),
            uv309_42 = c("Unknown", "Unknown"),
            uv310_23 = c("Unknown", "Unknown"),
            uv311_04 = c("Unknown", "Unknown"),
            uv311_85 = c("Unknown", "Unknown"),
            uv312_66 = c("Unknown", "Unknown"),
            uv313_47 = c("Unknown", "Unknown"),
            uv314_28 = c("Unknown", "Unknown"),
            uv315_09 = c("Unknown", "Unknown"),
            uv315_90 = c("Unknown", "Unknown"),
            uv316_71 = c("Unknown", "Unknown"),
            uv317_52 = c("Unknown", "Unknown"),
            uv318_33 = c("Unknown", "Unknown"),
            uv319_14 = c("Unknown", "Unknown"),
            uv319_95 = c("Unknown", "Unknown"),
            uv320_76 = c("Unknown", "Unknown"),
            uv321_57 = c("Unknown", "Unknown"),
            uv322_38 = c("Unknown", "Unknown"),
            uv323_19 = c("Unknown", "Unknown"),
            uv324_00 = c("Unknown", "Unknown"),
            uv324_81 = c("Unknown", "Unknown"),
            uv325_62 = c("Unknown", "Unknown"),
            uv326_43 = c("Unknown", "Unknown"),
            uv327_24 = c("Unknown", "Unknown"),
            uv328_05 = c("Unknown", "Unknown"),
            uv328_86 = c("Unknown", "Unknown"),
            uv329_67 = c("Unknown", "Unknown"),
            uv330_48 = c("Unknown", "Unknown"),
            uv331_29 = c("Unknown", "Unknown"),
            uv332_10 = c("Unknown", "Unknown"),
            uv332_91 = c("Unknown", "Unknown"),
            uv333_72 = c("Unknown", "Unknown"),
            uv334_52 = c("Unknown", "Unknown"),
            uv335_33 = c("Unknown", "Unknown"),
            uv336_14 = c("Unknown", "Unknown"),
            uv336_95 = c("Unknown", "Unknown"),
            uv337_76 = c("Unknown", "Unknown"),
            uv338_57 = c("Unknown", "Unknown"),
            uv339_37 = c("Unknown", "Unknown"),
            uv340_18 = c("Unknown", "Unknown"),
            uv340_99 = c("Unknown", "Unknown"),
            uv341_80 = c("Unknown", "Unknown"),
            uv342_60 = c("Unknown", "Unknown"),
            uv343_41 = c("Unknown", "Unknown"),
            uv344_22 = c("Unknown", "Unknown"),
            uv345_03 = c("Unknown", "Unknown"),
            uv345_83 = c("Unknown", "Unknown"),
            uv346_64 = c("Unknown", "Unknown"),
            uv347_45 = c("Unknown", "Unknown"),
            uv348_25 = c("Unknown", "Unknown"),
            uv349_06 = c("Unknown", "Unknown"),
            uv349_87 = c("Unknown", "Unknown"),
            uv350_67 = c("Unknown", "Unknown"),
            uv351_48 = c("Unknown", "Unknown"),
            uv352_28 = c("Unknown", "Unknown"),
            uv353_09 = c("Unknown", "Unknown"),
            uv353_90 = c("Unknown", "Unknown"),
            uv354_70 = c("Unknown", "Unknown"),
            uv355_51 = c("Unknown", "Unknown"),
            uv356_31 = c("Unknown", "Unknown"),
            uv357_12 = c("Unknown", "Unknown"),
            uv357_92 = c("Unknown", "Unknown"),
            uv358_73 = c("Unknown", "Unknown"),
            uv359_53 = c("Unknown", "Unknown"),
            uv360_33 = c("Unknown", "Unknown"),
            uv361_14 = c("Unknown", "Unknown"),
            uv361_94 = c("Unknown", "Unknown"),
            uv362_75 = c("Unknown", "Unknown"),
            uv363_55 = c("Unknown", "Unknown"),
            uv364_35 = c("Unknown", "Unknown"),
            uv365_16 = c("Unknown", "Unknown"),
            uv365_96 = c("Unknown", "Unknown"),
            uv366_76 = c("Unknown", "Unknown"),
            uv367_56 = c("Unknown", "Unknown"),
            uv368_37 = c("Unknown", "Unknown"),
            uv369_17 = c("Unknown", "Unknown"),
            uv369_97 = c("Unknown", "Unknown"),
            uv370_77 = c("Unknown", "Unknown"),
            uv371_57 = c("Unknown", "Unknown"),
            uv372_38 = c("Unknown", "Unknown"),
            uv373_18 = c("Unknown", "Unknown"),
            uv373_98 = c("Unknown", "Unknown"),
            uv374_78 = c("Unknown", "Unknown"),
            uv375_58 = c("Unknown", "Unknown"),
            uv376_38 = c("Unknown", "Unknown"),
            uv377_18 = c("Unknown", "Unknown"),
            uv377_98 = c("Unknown", "Unknown"),
            uv378_78 = c("Unknown", "Unknown"),
            uv379_58 = c("Unknown", "Unknown"),
            uv380_38 = c("Unknown", "Unknown"),
            uv381_18 = c("Unknown", "Unknown"),
            uv381_98 = c("Unknown", "Unknown"),
            uv382_78 = c("Unknown", "Unknown"),
            uv383_58 = c("Unknown", "Unknown"),
            uv384_37 = c("Unknown", "Unknown"),
            uv385_17 = c("Unknown", "Unknown"),
            uv385_97 = c("Unknown", "Unknown"),
            uv386_77 = c("Unknown", "Unknown"),
            uv387_57 = c("Unknown", "Unknown"),
            uv388_36 = c("Unknown", "Unknown"),
            uv389_16 = c("Unknown", "Unknown"),
            uv389_96 = c("Unknown", "Unknown"),
            uv390_75 = c("Unknown", "Unknown"),
            uv391_55 = c("Unknown", "Unknown"),
            uv392_35 = c("Unknown", "Unknown"),
            uv393_14 = c("Unknown", "Unknown"),
            uv393_94 = c("Unknown", "Unknown"),
            uv394_73 = c("Unknown", "Unknown"),
            uv395_53 = c("Unknown", "Unknown"),
            uv396_32 = c("Unknown", "Unknown"),
            tempInternal = c("degrees Celsius", "Temperature"),
            tempSpec = c("degrees Celsius", "Temperature"),
            tempLamp = c("degrees Celsius", "Temperature"),
            lampTime = c("seconds", "Time"),
            humidityInternal = c("percent saturation", "-"),
            voltageMain = c("volts", "Energy per Charge"),
            volt12 = c("volts", "Energy per Charge"),
            volt5 = c("volts", "Energy per Charge"),
            current = c("milliamperes?", "Charge per Time"),
            fitS2 = c("Unknown", "Unknown"),
            fitS3 = c("Unknown", "Unknown"),
            fitB0 = c("Unknown", "Unknown"),
            fitB1 = c("Unknown", "Unknown"),
            rmsError = c("Unknown", "Unknown"),
            ctdTime = c("seconds", "Time"),
            ctdSal = c("Unknown", "Unknown"),
            ctdTemp = c("degrees Celsius", "Unknown"),
            ctdDepth = c("meters", "Length"),
            check = c("Unknown", "Unknown"),
            # Data frame arguments
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
      
      # Method DataFileSUNA2019$read ####
      #
      #' @description 
      #'   Read the data file to populate the table and time attributes
      #' 
      #' @param metadata
      #'   Optional argument to specify the metadata object to which
      #'   metadata about variables measureded will be added.
      #'   Defaults to NULL.
      #' @param instrument
      #'   Optional argument to specify the instrument used.
      #'   Defaults to Seabird SUNA.
      #' @param dataLayer
      #'   Optional argument to specify the associated dataLayer in the metadata
      #'   Defaults to "root".
      #' @param stringsAsFactors
      #'   Optional logical value to turn strings as factors on or off.
      #'   Default value is FALSE (do not interpret strings as categorical factors).
      #'   
      #' @return 
      #'   Reference to the signal object created from data in the file.
      #'   
      read = function
      (
         metadata = NULL,
         instrument = "Seabird SUNA",
         dataLayer = "root",
         stringsAsFactors = FALSE
      )
      {
         self$signal <- self$createSignal(
            table = read.table(
               file = self$filePath,
               sep = ",",
               header = FALSE,
               skip = self$numMetaRows,
               col.names = names(self$metaColumns),
               stringsAsFactors = stringsAsFactors
            ),
            metadata = metadata,
            instrument = instrument,
            dataLayer = dataLayer
         );
         return(self$signal);
      },
      
      # Method DataFileSUNA2019$plotNitrate ####
      #
      #' @description 
      #'   Plot the nitrate data
      #' 
      #' @param signal
      #'   Optional reference to a signal to plot.
      #'   Defaults to self$signal.
      #' @param mar
      #'   Optional vector of values to use as margins passed to par.
      #'   Defaults to c(4.5, 4.5, 1, 4.5).
      #' @param time
      #'   Optional vector for time axis.
      #'   Default value is the time associated with this object.
      #' @param timeZone
      #'   Optional time zone for time axis.
      #'   Default value is the time zone associated with this object.
      #' @param yMolar
      #'   Optional vector for the molar concentration scaled by the left y axis.
      #'   Default value is the molar nitrate concentration from the signal argument.
      #' @param ylimMolar
      #'   Optional limits for the molar concentration scaled by the left y axis.
      #'   Default value is the min and max of the yMolar argument
      #' @param yMass
      #'   Optional vector for the mass concentration scaled by the left y axis.
      #'   Default value is the molar nitrate concentration from the signal argument.
      #' @param ylimMass
      #'   Optional limits for the mass concentration scaled by the left y axis.
      #'   Default value is the min and max of the yMass argument
      #' @param timeAxisFormat
      #'   Optional character string giving the time format for the labels on the time axis (strptime)
      #'   Defaults to "<Month abbrev.> <Day of month> Hour(24-hour):minute".
      #' @param molarArgs
      #'   Optional additional parameters passed on to the call to plot.default for the molar 
      #'   concentrations.
      #'   Defaults to an empty list
      #' @param massArgs
      #'   Optional additional parameters passed on to the call to plot.default for the mass 
      #'   concentrations.
      #'   Defaults to an empty list
      #'   
      #' @return 
      #'   No defined return value. 
      #'   
      plotNitrate = function
      (
         signal = self$signal,
         mar = c(4.5, 4.5, 1, 4.5),
         time = signal$time,
         timeZone = self$timeZone,
         yMolar = signal$getVariable("nitrateMolar"),
         ylimMolar = c(
            min(yMolar),
            max(yMolar)
         ),
         yMass = signal$getVariable("nitrateMass"),
         ylimMass = ylimMolar * 0.014,
         timeAxisFormat = "%e %b \n %H:%M",
         padj = 0.5,
         molarArgs = list(),
         massArgs = list()
      )
      {
         attributes(time)$tzone <- timeZone;
         par(mar = mar);
         do.call(
            what = plot.default, 
            args = c(
               list(
                  x = time,
                  xaxt = "n",
                  xlab = sprintf("Time (%s Time Zone)", timeZone),
                  y = yMolar,
                  ylab = bquote(Nitrate ~ conc. ~ .("(") * mu * mol ~ NO[3]^-phantom() ~ L^-1 * .(")")),
                  ylim = ylimMolar
               ),
               molarArgs
            )
         );
         axis.POSIXct(
            side = 1,
            x = time,
            format = timeAxisFormat,
            padj = padj
         );
         par(new = TRUE);
         do.call(
            what = plot.default, 
            args = c(
               list(
                  x = time,
                  xlab = "",
                  y = yMass,
                  ylim = ylimMass,
                  ylab = "",
                  type = "n",
                  axes = FALSE
               ),
               massArgs
            )
         );
         axis(
            side = 4
         );
         mtext(
            text = bquote(Nitrate ~ conc. ~ .("(") * mg ~ N ~ L^-1 * .(")")),
            side = 4,
            line = 3
         );
      },
      # Method DataFileSUNA2019$createSignal ####
      #
      #' @description 
      #'   Create a signal object from the SUNA data
      #' 
      #' @param table
      #'   Data frame on which the signal is based.   
      #' @param metadata
      #'   Optional metadata object describing the SUNA data.
      #'   Defaults to NULL.
      #' @param instrument
      #'   Optional character string identifying instrument used.
      #'   Defaults to "SUNA Unknown Serial Number".
      #' @param dataLayer
      #'   Optional data layer name in metadata for the SUNA data.
      #'   Defaults to "root".
      #'   
      #' @return 
      #'   A signal object with the SUNA data. 
      #'   
      createSignal = function
      (
         table,
         metadata = NULL,
         instrument = "SUNA Unknown Serial Number",
         dataLayer = "root"
      )
      {
         if(!is.null(metadata)) {
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "timeString",
               name = "Time in string format",
               units = "Gregorian UTC month/day/year Hour:minute:second",
               dimensions = "Time",
               stat = "instantaneous",
               instrument = instrument
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "nitrateMolar",
               name = "Nitrate conc.",
               units = "micromoles per liter",
               dimensions = "Count per Volume",
               stat = "instataneous",
               instrument = instrument,
               description = paste(
                  "In situ molar concentration of nitrate in water.",
                  "Ultraviolet light absorbance of water in the SUNA",
                  "measurement window is used to estimate nitrate",
                  "concentration corrected for the influence of",
                  "organic matter."
               )
            );
            metadata$addVariable(
               category = "what",
               context = "PropertiesWereEvaluated",
               header = "nitrateMass",
               name = "Nitrate conc.",
               units = "milligrams per liter",
               dimensions = "Mass per Volume",
               stat = "instataneous",
               instrument = instrument,
               description = paste(
                  "In situ mass concentration of nitrate in water.",
                  "Ultraviolet light absorbance of water in the SUNA",
                  "measurement window is used to estimate nitrate",
                  "concentration corrected for the influence of",
                  "organic matter."
               )
            );
         }
         
         metaColumns <- self$metaColumns;
         metaColumns["dataLayer",] <- dataLayer;
         
         return( 
            SignalTable$new(
               dataframe = table,
               meta = metadata,
               metaColumns = metaColumns,
               tz = self$timeZone,
               timeVariableName = "timeString",
               format = self$timeFormat
            )
         );
         
      }
   )
)
