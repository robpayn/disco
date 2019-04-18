rm(list = ls());
library(R6);
library(inferno);
library(metab);
library(disco);

solarRadiation <- SolarRadiation$new(
   latitude = 46.59566,
   longitude = -112.94713,
   differenceFromGMT = -7,
   adjustDST = 1
);

for (name in c("upstream", "downstream")) {

   signal <- SignalDataFrame$constructFromCSV(
      fileName = sprintf(
         "./input/%s",
         name
         )
   );

   pdf(file = sprintf("./output/summary_%s.pdf", name));
   par(
      mfrow = c(3,2),
      mar = c(4, 4, 2, 4) + 0.1,
      oma = c(0, 0, 2, 0)
      );

   ld <- rbind.data.frame(
      doObs = list(
         name = "DO Obs",
         lty = NA,
         pch = 1,
         col = "black",
         lwd = 1
      ),
      doSat = list(
         name = "DO Sat",
         lty = "dotted",
         pch = NA,
         col = "black",
         lwd = 2
      ),
      par = list(
         name = "PAR",
         lty = "dashed",
         pch = NA,
         col = "red",
         lwd = 2
      ),
      parExt = list(
         name = "Ext. Insol.",
         lty = "dotted",
         pch = NA,
         col = "red",
         lwd = 2
      ),
      stringsAsFactors = FALSE
   );

   plotCount = 6;

   for (day in 10:15) {

      minTime = sprintf(
         "2014-09-%2d 06:00:00",
         day
      );
      maxTime = sprintf(
         "2014-09-%2d 06:00:00",
         day + 1
      );

      daySignal <- signal$getWindow(
         minTime = minTime,
         maxTime = maxTime
      );

      model <- ModelOneStationMetabDo$new(
         dailyGPP = 50,
         dailyER = -50,
         k600 = 3,
         airPressure = 0.84,
         time = daySignal$time,
         initialDO = daySignal$dataFrame$data$do,
         temp = daySignal$dataFrame$data$temp,
         par = daySignal$dataFrame$data$par,
         stdAirPressure = 1
      );
      model$run();

      timeAxis <- daySignal$time - 6 * 3600;

      daySignal$plot(
         header = "do",
         x = timeAxis,
         xaxt = "n",
         pch = ld["doObs", "pch"],
         col = ld["doObs", "col"],
         lwd = ld["doObs", "lwd"]
      );

      if (plotCount == 6) {

         mtext(
            side = 3,
            outer = TRUE,
            text = signal$dataFrame$meta["where",]$value,
            cex = 0.8
         );
         legend(
            x = min(timeAxis),
            y = max(daySignal$dataFrame$data$do),
            bty = "n",
            legend = ld$name,
            lty = ld$lty,
            pch = ld$pch,
            col = ld$col,
            lwd = ld$lwd
         );
         plotCount = 1;

      } else {

         plotCount = plotCount + 1;

      }

      mtext(
         side = 3,
         line = 0.5,
         text = substr(minTime, 1, 10),
         cex = 0.7
      );
      axis.POSIXct(
         side = 1,
         at = seq(
            as.POSIXct(minTime) - 6 * 3600,
            as.POSIXct(maxTime) - 6 * 3600,
            length.out = 13
            )
      );
      # lines(daySignal$time, model$output$do);
      lines(
         timeAxis,
         model$output$doSat,
         lty = ld["doSat", "lty"],
         col = ld["doSat", "col"],
         lwd = ld["doSat", "lwd"]
      );
      par(new = TRUE);
      daySignal$plot(
         header = "par",
         x = timeAxis,
         type = "l",
         lty = ld["par", "lty"],
         col = ld["par", "col"],
         lwd = ld["par", "lwd"],
         yaxt = "n",
         ylab = "",
         xaxt = "n",
         xlab = ""
      );
      axis(
         side = 4
      );
      par(new = TRUE);
      parExt <- solarRadiation$getExtraterrestrialInsolation(timeAxis);
      plot(
         x = timeAxis,
         y = parExt,
         type = "l",
         lty = ld["parExt", "lty"],
         col = ld["parExt", "col"],
         lwd = ld["parExt", "lwd"],
         yaxt = "n",
         ylab = "",
         xaxt = "n",
         xlab = ""
      );
      mtext(
         text = sprintf(
            "par (%s)",
            daySignal$dataFrame$metaColumns["par",]$units
            ),
         side = 4,
         line = 2.5,
         cex = 0.7
      );
   }

   dev.off();

}
