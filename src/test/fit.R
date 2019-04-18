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

   pdf(file = sprintf("./output/fit_%s.pdf", name));
   par(
      mfrow = c(3, 2),
      mar = c(2, 4, 4, 4) + 0.1,
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
      doMod = list(
            name = "DO Mod",
            lty = "solid",
            pch = NA,
            col = "black",
            lwd = 2
         ),
      doSat = list(
            name = "DO Sat",
            lty = "dotted",
            pch = NA,
            col = "black",
            lwd = 2
         ),
      parExt = list(
            name = "Ext. Insol.",
            lty = "dashed",
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

      time <- daySignal$time - 6 * 3600;

      solarRadiation <- SolarRadiation$new(
         latitude = 46.59566,
         longitude = -112.94713,
         differenceFromGMT = -7,
         adjustDST = 1
      );
      parExt <- solarRadiation$getExtraterrestrialInsolation(time);

      model <- ModelOneStationMetabDo$new(
         dailyGPP = 50,
         dailyER = -50,
         k600 = 3,
         airPressure = 0.84,
         time = time,
         initialDO = daySignal$dataFrame$data$do,
         temp = daySignal$dataFrame$data$temp,
         par = parExt,
         stdAirPressure = 1
      );

      objFunc <- LogLikelihood$new(
         model = model,
         parameterTranslator = ParameterTranslatorMetab$new(model),
         predictionExtractor = PredictionExtractorMetabDo$new(model),
         observation = data.frame(do = daySignal$dataFrame$data$do),
         sd = 0.05,
         negate = TRUE
      );

      optimr <- optim(
         par = c(
            50,
            -50,
            3
         ),
         fn = objFunc$propose
      );

      objFunc$propose(optimr$par);

      daySignal$plot(
         header = "do",
         x = time,
         xaxt = "n",
         xlab = "n",
         ylim = c(
               min(objFunc$model$output$do, daySignal$dataFrame$data$do),
               max(objFunc$model$output$do, daySignal$dataFrame$data$do)
            ),
         pch = ld["doObs", "pch"],
         col = ld["doObs", "col"],
         lwd = ld["doObs", "lwd"]
      );
      lines(
         x = time,
         y = objFunc$model$output$do,
         lty = ld["doMod", "lty"],
         col = ld["doMod", "col"],
         lwd = ld["doMod", "lwd"]
      );

      if (plotCount == 6) {

         mtext(
            side = 3,
            outer = TRUE,
            text = signal$dataFrame$meta["where",]$value,
            cex = 0.8
         );
         legend(
            x = min(time),
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
         line = 2,
         text = substr(minTime, 1, 10),
         cex = 0.7
      );
      mtext(
         side = 3,
         line = 1,
         text = sprintf(
               fmt = "GPP = %.3e, ER = %.3e, k600 = %.3e",
               signif(optimr$par[1], 4),
               signif(optimr$par[2], 4),
               signif(optimr$par[3], 4)
            ),
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
         time,
         model$output$doSat,
         lty = ld["doSat", "lty"],
         col = ld["doSat", "col"],
         lwd = ld["doSat", "lwd"]
      );
      par(new = TRUE);
      plot(
         x = time,
         y = parExt,
         type = "l",
         lty = ld["parExt", "lty"],
         col = ld["parExt", "col"],
         lwd = ld["parExt", "lwd"],
         yaxt = "n",
         ylab = "",
         ylim = c(0, 1400),
         xaxt = "n",
         xlab = ""
      );
      axis(
         side = 4,
         col.axis = ld["parExt", "col"]
      );
      mtext(
         text = bquote(paste(
               "Ext. Insol. (W ",
               m^-2,
               ")"
            )),
         side = 4,
         line = 2.5,
         cex = 0.7,
         col = ld["parExt", "col"]
      );
   }

   dev.off();

}
