rm(list = ls());
library(R6);
library(inferno);
library(metab);
library(disco);

name <- "upstream";

signal <- SignalDataFrame$constructFromCSV(
   fileName = sprintf("./input/%s", name)
);

day <- 11;

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
   xaxt = "n"
   );
lines(
   x = time,
   y = model$output$do
   );
axis.POSIXct(
   side = 1,
   at = seq(
      from = as.POSIXct(minTime) - 6 * 3600,
      to = as.POSIXct(maxTime) - 6 * 3600,
      length.out = 13
      )
   );
