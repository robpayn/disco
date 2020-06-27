# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 Class DataFileHoboEC2019 ####

#
#' @export
#'
#' @title
#'   A campbell data file class (R6 Class)
#'
#' @description
#'   Provides utilities for managing data in a Campbell Scientific
#'   datalogger file, with the file structure typical in 2019.
#'
DataFileHoboEC2019 <- R6Class(
  classname = "DataFileHoboEC2019",
  inherit = TableDataFile,
  public = list(
    
    #' @field lowRange
    #' Logical value indicating if low range was measured
    lowRange = NULL,
    
    #' @field fullRange
    #' Logical value indicating if low range was measured
    fullRange = NULL,
    
    #' @field factoryCalib
    #' Logical value indicating if the factory calibration was used
    #' with the data assistant
    factoryCalib = NULL,
    
    #' @field standardCalib
    #' Logical value indicating if a standard calibration was used with
    #' the data assistant
    standardCalib = NULL,
    
    # Method DataFileHoboEC2019$new ####
    #
    #' @description
    #'   Constructs a new instance of the class DataFileCampbell2019
    #'
    #' @param filePath
    #'   Character string representing the path to the file
    #' @param instrument
    #'   Optional instrument used to collect the data
    #'   Defaults to "Hobo EC meter, Unknown serial"
    #' @param timeVariableName
    #'   Optional character string for the time variable header in the table
    #'   Defaults to "timeString"
    #' @param numMetaRows
    #'   Optional integer representing the number of rows of metadata at the top
    #'   of the file.
    #'   Defaults to 2.
    #' @param lowRange
    #'   Optional logical value specifying if low range values were recorded
    #'   Defaults to TRUE
    #' @param fullRange
    #'   Optional logical value specifying if full range values were recorded
    #'   Defaults to TRUE
    #' @param factoryCalib
    #'   Optional logical value specifying if factory calibration was applied
    #'   Defaults to TRUE
    #' @param standardCalib
    #'   Optional logical value specifying if standard calibration was applied
    #'   Defaults to TRUE
    #' @param metaColumns
    #'   Optional data frame with context for each column.
    #'   Defaults to the results of calling "getMetaColumns" function
    #' @param delimiter
    #'   Optional character string representing the column delimiter
    #'   Defaults to a comma
    #' @param timeZone
    #'   Optional character string representing the time zone.
    #'   Defaults to "UTC".
    #' @param timeFormat
    #'   character string representing the time format (strptime)
    #'   Defaults to Year(4-digit)-month-day Hour(24-hour):minute:second
    #' @param numMetaRows
    #'   Optional integer representing the number of rows of metadata at the top
    #'   of the file.
    #'   Defaults to 4.
    #'
    initialize = function
    (
      filePath,
      instrument = "Hobo EC meter, Unknown serial",
      timeVariableName = "timeString",
      numMetaRows = 2,
      lowRange = TRUE,
      fullRange = TRUE,
      factoryCalib = TRUE,
      standardCalib = TRUE,
      addMetaColumns = list(),
      metaColumns = self$getMetacolumns(
        lowRange = lowRange,
        fullRange = fullRange,
        factoryCalib = factoryCalib,
        standardCalib = standardCalib,
        addMetacolumns = addMetacolumns
      ),
      delimiter = ",",
      timeZone = "UTC",
      timeFormat = "%Y-%m-%d %H:%M:%S",
      metadata = NULL
    )
    {
      super$initialize(
        instrument = instrument,
        filePath = filePath,
        timeVariableName = timeVariableName,
        numMetaRows = numMetaRows,
        metaColumns = metaColumns,
        delimiter = delimiter,
        timeZone = timeZone,
        timeFormat = timeFormat,
        metadata = metadata
      );
      self$lowRange <- lowRange;
      self$fullRange <- fullRange;
      self$factoryCalib <- factoryCalib;
      self$standardCalib <- standardCalib;
    },
    
    getMetacolumns = function
    (
      lowRange,
      fullRange,
      factoryCalib,
      standardCalib,
      addMetacolumns = list()
    )
    {
      metaColumns <- data.frame(
        recordNo = c("Number", "Category"),
        timeString = c("Gregorian UTC Date/Time", "Time"),
        row.names = c("units", "dimensions")
      )
      if(lowRange) {
        metaColumns$lowRangeEC = c("microsiemens per centimeter", "conductance per length");
      }
      if(fullRange) {
        metaColumns$fullRangeEC = c("microsiemens per centimeter", "conductance per length");
      }
      metaColumns$temperature = c("degrees celsius", "temperature");
      if(lowRange && factoryCalib) {
        metaColumns$lowRangeSPCFactory = c("microsiemens per centimeter", "conductance per length");
      }
      if(lowRange && standardCalib) {
        metaColumns$lowRangeSPCStandard = c("microsiemens per centimeter", "conductance per length");
      }
      if(fullRange && factoryCalib) {
        metaColumns$fullRangeSPCFactory = c("microsiemens per centimeter", "conductance per length");
      }
      if(fullRange && standardCalib) {
        metaColumns$fullRangeSPCStandard = c("microsiemens per centimeter", "conductance per length");
      }
      return(metaColumns);
    },

    # Method DataFileHoboEC2019$addMetadata ####
    #
    #' @description
    #'   Read the data file to populate the signal attribute.
    #'
    #' @param metadata
    #'   Metadata object.
    #' @param instrument
    #'   Character string describing the instrument used to collect the data file.
    #'
    addMetadata = function(metadata, instrument)
    {
      
      metadata$addVariable(
        category = "what",
        context = "PropertiesWereEvaluated",
        header = "timeString",
        name = "Time in string format",
        units = "Gregorian UTC Year-month-day Hour:minute:second",
        dimensions = "Time",
        stat = "instantaneous",
        instrument = instrument
      );
      
      if (self$lowRange) {
        metadata$addVariable(
          category = "what",
          context = "PropertiesWereEvaluated",
          header = "lowRangeEC",
          name = "Low-Range Electrical Conductivity",
          units = "microsiemens per centimeter",
          dimensions = "conductance per length",
          stat = "instataneous",
          instrument = instrument,
          description = paste(
            "Reduced range conductivity measurement.",
            " Used for higher precision measurements of lower conductivities."
          )
        );
      }
      
      if (self$fullRange) {
        metadata$addVariable(
          category = "what",
          context = "PropertiesWereEvaluated",
          header = "fullRangeEC",
          name = "Full Range Electrical Conductivity",
          units = "microsiemens per centimeter",
          dimensions = "conductance per length",
          stat = "instataneous",
          instrument = instrument,
          description = paste(
            "Full range conductivity measurement.",
            " Used for measurement of larger conductivities."
          )
        );
      }
      
      metadata$addVariable(
        category = "what",
        context = "PropertiesWereEvaluated",
        header = "Temperature",
        name = "temperature",
        units = "Degrees C",
        dimensions = "temperature",
        stat = "instantaneous",
        instrument = instrument
      );

      if(self$lowRange && self$factoryCalib) {
        metadata$addVariable(
          category = "what",
          context = "PropertiesWereEvaluated",
          header = "lowRangeSPCFactory",
          name = "Low range specific conductivity",
          units = "microsiemens per centimeter",
          dimensions = "conductance per length",
          stat = "instataneous",
          instrument = instrument,
          description = paste(
            "Low range specific conductivity using factory calibration"
          )
        );
      }
      if(self$lowRange && self$standardCalib) {
        metadata$addVariable(
          category = "what",
          context = "PropertiesWereEvaluated",
          header = "lowRangeSPCStandard",
          name = "Low range specific conductivity",
          units = "microsiemens per centimeter",
          dimensions = "conductance per length",
          stat = "instataneous",
          instrument = instrument,
          description = paste(
            "Low range specific conductivity using calibration to a standard"
          )
        );
      }
      if(self$fullRange && self$factoryCalib) {
        metadata$addVariable(
          category = "what",
          context = "PropertiesWereEvaluated",
          header = "fullRangeSPCFactory",
          name = "Full range specific conductivity",
          units = "microsiemens per centimeter",
          dimensions = "conductance per length",
          stat = "instataneous",
          instrument = instrument,
          description = paste(
            "Full range specific conductivity using factory calibration"
          )
        );
      }
      if(self$fullRange && self$standardCalib) {
        metadata$addVariable(
          category = "what",
          context = "PropertiesWereEvaluated",
          header = "fullRangeSPCStandard",
          name = "Full range specific conductivity",
          units = "microsiemens per centimeter",
          dimensions = "conductance per length",
          stat = "instataneous",
          instrument = instrument,
          description = paste(
            "Full range specific conductivity using calibration to a standard"
          )
        );
      }
    
    },
    
    # Method DataFileHoboEC2019$plotEC ####
    #
    #' @description
    #'   Plot the Conductivity data. Select particular data from the entire dataset.
    #'   Must initialize plot separately if plotting multiple datasets.
    #'
    #' @param signal
    #'   Optional reference to a signal to plot.
    #'   Defaults to self$signal.
    #'
    #' @param mar
    #'   Optional vector of values to use as margins passed to par.
    #'   Defaults to c(4.5, 4.5, 1, 4.5).
    #'
    #' @param time
    #'   Optional vector for time axis.
    #'   Default value is the time associated with this object.
    #'
    #' @param timeZone
    #'   Optional time zone for time axis.
    #'   Default value is the time zone associated with this object.
    #'
    #' @param timeAxisFormat
    #'   Optional character string giving the time format for the labels on the time axis (strptime)
    #'   Defaults to "<Month abbrev.> <Day of month> Hour(24-hour):minute".
    #'
    #' @param timepadj
    #'   Optional adjustment (axis argument) to the location of the labels on the time axis.
    #'   Defaults to 0.5.
    #'
    #' @param yData
    #'   Full set of data that could be plotted.
    #'   Defaults to all data, excluding time and record number.
    #'
    #' @param plotLR
    #'   Low Range Conductivity plot type parameter. Used to select data to be plotted.
    #'   Defaults to "p" and will plot. Set to "n" to supress plot.
    #'
    #' @param plotFR
    #'   Full Range Conductivity plot type parameter. Used to select data to be plotted.
    #'   Defaults to "p" and will plot. Set to "n" to supress plot.
    #'
    #' @param plotFSP
    #'   Factory Calibration Specific Conductivity plot type parameter. Used to select data to be plotted.
    #'   Defaults to "p" and will plot. Set to "n" to supress plot.
    #'
    #' @param plotSSP
    #'   Standard Calibration Specific Conductivity plot type parameter. Used to select data to be plotted.
    #'   Defaults to "p" and will plot. Set to "n" to supress plot.
    #'
    #' @param legend
    #'   Legend label parameter.
    #'   Defaults to NULL and will use dataframe column names.
    #'
    #' @param shapes
    #'   Shapes vector if different datapoint shapes are used.
    #'   Defaults to NULL. Follows pch() shape naming conventions.
    #'
    #' @param extraArgs
    #'   Optional additional parameters passed on to the call to plot.default.
    #'   Defaults to an empty list.
    #'
    #' @return
    #'   No defined return value.
    #'
    
    plotEC = function
    (
      signal = self$signal,
      mar = c(4.5, 4.5, 1.5, 4.5),
      time = signal$time,
      timeZone = self$timeZone,
      timeAxisFormat = "%e %b %H:%M",
      timepadj = 0.5,
      plot_LR = TRUE,
      plot_FR = TRUE,
      plot_FSP = TRUE,
      plot_SSP = TRUE,
      legend = TRUE,
      shapes = NULL,
      extraArgs = list()
    )
    {
      stopifnot(
        plot_LR == TRUE || plot_LR == FALSE,
        plot_FR == TRUE || plot_FR == FALSE,
        plot_FSP == TRUE || plot_FSP == FALSE,
        plot_SSP == TRUE || plot_SSP == FALSE
      )
    
      plot_LR <- if(plot_LR == TRUE) {
        "p"
      } else {
        "n"
      }
      plot_FR <- if(plot_FR == TRUE) {
        "p"
      } else {
        "n"
      }
      plot_FSP <- if(plot_FSP == TRUE) {
        "p"
      } else {
        "n"
      }
      plot_SSP <- if(plot_SSP == TRUE) {
        "p"
      } else {
        "n"
      }
    
      attributes(time)$tzone <- timeZone;
      par(mar = mar);
      color <- c(
        orange = "#E69F00",
        skyblue = "#56B4E9",
        vermillion = "#D55E00",
        reddishPurple = "#CC79A7"
      )
    
      #Create named vector with plot values
      to_plot <- c(
        lowRangeEC = plot_LR,
        fullRangeEC = plot_FR,
        factorySPC = plot_FSP,
        standardSPC = plot_SSP
      )
    
      # Load in ONLY data that will be plotted.
      yData <- signal$table$data[,names(to_plot[to_plot == "p"])]
    
      # Set up initial plot area/boundaries. Do not plot anything.
      # Y values are not used in this function .
      do.call(
        what = plot.default,
        args = c(
          list(
            type = "n",
            x = time,
            xaxt = "n",
            xlab = sprintf("Time (%s Time Zone)", timeZone),
            y = signal$table$data$recordNo,
            ylab = bquote(Electrical ~ Conductivity ~ .("(") * mu * S ~ cm^-1 * .(")")),
            ylim = c(
              min(yData),
              1.15 * max(yData)
            )
          ),
          extraArgs
        )
      );
      axis.POSIXct(
        side = 1,
        x = time,
        padj = timepadj,
        format = timeAxisFormat
      );
    
      # Loop to plot all selected data
      i <- 1
      while (i <= length(yData)) {
        points(
          x = time,
          y = yData[,i],
          col = color[i],
          pch = shapes
        )
        i = i + 1
      }
    
      # Create legend
    
      if (legend != FALSE){
      legend(
        x = "topleft",
        legend = if (!isTRUE(legend)){
          legend
        } else{
          names(yData)
        },
        pch = c(
          rep(1,length(yData)),
          unique(shapes)
              ),
        col = c(
          color[1:i-1],
          rep("black",length(shapes))
              ),
        bty = "n"
      )
      } else {
      }
    },
    # Method DataFileHoboEC2019$plotTemp ####
    #
    #' @description
    #'   Plot the temperature data
    #'
    #' @param signal
    #'   Optional signal to plot.
    #'   Defaults to the signal attribute of this object.
    #' @param mar
    #'   Optional vector of values to use as margins passed to par.
    #'   Defaults to c(4.5, 4.5, 1, 4.5).
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
    #'   Optional vector for the temperature.
    #'   Default value is the temperature from the signal argument above.
    #' @param ylim
    #'   Optional axis limits for the temperature.
    #'   Default value is the min and max of ytemp
    #' @param ylab
    #'   Optional label for the y axis.
    #'   Default value is formatted Temperature with units of degrees Celsius
    #' @param extraArgs
    #'   Optional additional parameters passed on to the call to plot.default.
    #'   (main, sub, yaxt, etc)
    #'
    #' @return
    #'   No defined return value.
    #'
    plotTemp = function
    (
      signal = self$signal,
      mar = c(4.5,4.5,1,4.5),
      time = signal$time,
      timeZone = attributes(signal$time)$tzone,
      timeLab = sprintf("Time (%s Time Zone)", timeZone),
      timeAxisFormat = "%e %b \n %H:%M",
      timepadj = 0.5,
      ytemp = signal$getVariable("temperature"),
      ylim = c(
        min(ytemp),
        max(ytemp)
      ),
      ylab = bquote(Temperature ~ .("(") * degree * C * .(")")),
      extraArgs = list()
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
          xlab = timeLab,
          y = ytemp,
          ylim = ylim,
          ylab = ylab
          ),
          extraArgs
        )
      )
      axis.POSIXct(
        side = 1,
        x = time,
        padj = timepadj,
        format = timeAxisFormat
      );
    }
   )
)
