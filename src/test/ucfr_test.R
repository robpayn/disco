rm(list = ls());
library(R6);
library(infmod);
library(metab);
library(workflow);

for (name in c("upstream", "downstream")) {

   signal <- SignalDataFrame$constructFromCSV(
      signal = SignalDataFrame$new(),
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
      list(
         name = "DO Obs",
         lty = NA,
         pch = 1,
         col = "black",
         lwd = 1
      ),
      list(
         name = "DO Sat",
         lty = "dotted",
         pch = NA,
         col = "red",
         lwd = 2
      ),
      list(
         name = "PAR",
         lty = "dashed",
         pch = NA,
         col = "gold",
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
         initialDO = daySignal$data$do,
         temp = daySignal$data$temp,
         par = daySignal$data$par,
         stdAirPressure = 1
      );
      model$run();

      timeAxis <- daySignal$time - 6 * 3600;

      daySignal$plot(
         header = "do",
         x = timeAxis,
         xaxt = "n"
      );

      if (plotCount == 6) {

         mtext(
            side = 3,
            outer = TRUE,
            text = signal$meta["where",]$value,
            cex = 0.8
         );
         legend(
            x = min(timeAxis),
            y = max(daySignal$data$do),
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
         col = "red",
         lty = "dotted",
         lwd = 2
      );
      par(new = TRUE);
      daySignal$plot(
         header = "par",
         x = timeAxis,
         type = "l",
         col = "gold",
         lty = "dashed",
         lwd = 2,
         yaxt = "n",
         ylab = "",
         xaxt = "n",
         xlab = ""
      );
      axis(
         side = 4
      );
      mtext(
         text = sprintf(
            "par (%s)",
            daySignal$metaColumns["par",]$units
            ),
         side = 4,
         line = 2.5,
         cex = 0.7
      );
   }

   dev.off();

}
