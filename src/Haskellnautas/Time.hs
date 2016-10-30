module Haskellnautas.Time
  ( timeLocale
  ) where

import           Data.Time

timeLocale :: TimeLocale
timeLocale =  TimeLocale {
        wDays  = [("domingo",   "dom"),
                  ("lunes",     "lun"),
                  ("martes",    "mar"),
                  ("miércoles", "mié"),
                  ("jueves",    "jue"),
                  ("viernes",   "vie"),
                  ("sábado",    "sáb")],

        months = [("enero",      "ene"),
                  ("febrero",    "feb"),
                  ("marzo",      "mar"),
                  ("abril",      "abr"),
                  ("mayo",       "may"),
                  ("junio",      "jun"),
                  ("julio",      "jul"),
                  ("agosto",     "ago"),
                  ("septiembre", "sep"),
                  ("octubre",    "oct"),
                  ("noviembre",  "nov"),
                  ("diciembre",  "dec")],

        amPm = ("AM", "PM"),
        dateTimeFmt = "%a %e %b %H:%M:%S %Z %Y",
        dateFmt = "%d/%m/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p",
        knownTimeZones =
            [
            TimeZone 0 False "UT",
            TimeZone 0 False "GMT",
            TimeZone (-5 * 60) False "EST",
            TimeZone (-4 * 60) True "EDT",
            TimeZone (-6 * 60) False "CST",
            TimeZone (-5 * 60) True "CDT",
            TimeZone (-7 * 60) False "MST",
            TimeZone (-6 * 60) True "MDT",
            TimeZone (-8 * 60) False "PST",
            TimeZone (-7 * 60) True "PDT"
            ]
        }
