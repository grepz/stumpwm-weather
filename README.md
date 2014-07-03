stumpwm-weather
===============

Stumpwm script to get openweather forecasts

* USAGE:
1. Put:

     (load "/path/to/weather.lisp")

   In your ~/.stumpwmrc

2. Set variables:
   Set *openweather-units* to one of the supported values(metric by default)
   Set *openweather-lang* (en by default)
   Set *openweather-loc-defaults* to the list of the cities you are
   interested in, for example '("Irkuts,ru", "Moscow,ru")
   Now you can use command weather to get weather information.
   When asked about type you can choose between current and forecast, where
   current being the information on current weather state and forecast giving
   values for future.

3. Notes:
   Not tested on anything else but sbcl
