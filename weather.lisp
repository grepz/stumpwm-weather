;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;;
;;; OpenWeather simplified API interface
;;;
;;; Copyright 2014 Stanislav M. Ivankin
;;;
;;; Maintainer:
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;

;;; USAGE:
;;;
;;; Put:
;;;
;;;     (load "/path/to/weather.lisp")
;;;
;;; In your ~/.stumpwmrc
;;;
;;; Set *openweather-units* to one of the supported values(metric by default)
;;; Set *openweather-lang* (en by default)
;;; Set *openweather-loc-defaults* to the list of the cities you are
;;; interested in, for example '("Irkuts,ru", "Moscow,ru")
;;; Now you can use command weather to get weather information.
;;; When asked about type you can choose between current and forecast, where
;;; current being the information on current weather state and forecast giving
;;; values for future.
;;;
;;; NOTES:
;;;
;;; Not tested on anything else but sbcl


(in-package #:stumpwm)

(require :drakma)
(require :yason)
(require :local-time)

(defpackage :stumpwm.contrib.net
  (:use :common-lisp :stumpwm :drakma :yason :local-time)
  (:export #:openweather-units* #:*openweather-lang*
           #:*openweather-loc-defaults*))

(defvar *time-format* '((:hour 2) #\: (:min 2) #\Space
                        :short-weekday #\Space (:day 2) #\Space :short-month))

(defvar *openweather-api-base-link* "http://api.openweathermap.org/data/2.5/")
(defvar *openweather-api-current-data* "weather")
(defvar *openweather-api-forecast-data* "forecast")

(defvar *column-format* '("~20A" "~9A" "~6A" "~8A" "~8A" "~4A"
                                "~10A" "~7A" "~8A" "~8A" "~12A" "~18A"))
(defvar *column-names* '("Date" "Wind,m/s" "T,C" "Tmin,C" "Tmax,C" "H%"
                         "P,mm Hg" "Cld,%" "Rain,3h" "Snow,3h" "Wthr"
                         "Descr"))

(defparameter *openweather-units* "metric")
(defparameter *openweather-lang*  "en")
(defparameter *openweather-loc-defaults* '())

(defun weather-get (place &key (type "current"))
  (let ((data-type (if (string= type "current")
                       *openweather-api-current-data*
                       *openweather-api-forecast-data*)))
    (when data-type
      (multiple-value-bind (stream code)
          (drakma:http-request
           (concatenate 'string *openweather-api-base-link* data-type)
           :method :get :parameters (list (cons "q" place)
                                          (cons "units" *openweather-units*)
                                          (cons "lang" *openweather-lang*)))
        (when (= code 200)
          (yason:parse (flexi-streams:octets-to-string stream)))))))

(defun weather-process (data &key (root t) (weather t) (precip t) (wind t))
  "Returns: Date, Wind speed, Temp, Temp_Min, Temp_Max, Humidity,
  Pressure, Clouds, Rain, Snow, Weather, Description"
  (flet ((process-root (data)
           (list (local-time:format-timestring
                  nil (local-time:unix-to-timestamp
                       (gethash "dt" data)) :format *time-format*)))
         (process-wind (data)
           (let ((wind-data (gethash "wind" data)))
             (list (gethash "speed" wind-data))))
         (process-main (data)
           (let ((main-data (gethash "main" data)))
             (list (gethash "temp" main-data)
                   (gethash "temp_min" main-data)
                   (gethash "temp_max" main-data)
                   (gethash "humidity" main-data)
                   (gethash "pressure" main-data))))
         (process-weather (data)
           (let ((weather-data (car (gethash "weather" data))))
                (list (gethash "main" weather-data)
                      (gethash "description" weather-data))))
         (process-precip (data)
           (let ((rain-data   (gethash "rain"   data))
                 (snow-data   (gethash "snow"   data))
                 (clouds-data (gethash "clouds" data)))
             (list (gethash "all" clouds-data)
                   (if rain-data (gethash "3h" rain-data) nil)
                   (if snow-data (gethash "3h" snow-data) nil)))))
    (concatenate 'list
                 (when root (process-root data))
                 (when wind (process-wind data))
                 (process-main data)
                 (when precip (process-precip data))
                 (when weather (process-weather data)))))

(defun check-code (code)
  (if (stringp code) (string= code "200") (= code 200)))

(defun weather-format (tbl city &key
                             (col-num 11)
                             (col-format *column-format*)
                             (col-names *column-names*))
  (let ((code (gethash "cod" tbl))
        weather-data)
    (when (not (check-code code))
      (return-from weather-format (format nil "No data: ~a" code)))
    (setq weather-data (gethash "list" tbl))
    (when (null weather-data)
      (setq weather-data (list tbl)))
    (concatenate 'string
      (format nil "Data for ~s.~%" city)
      (loop for x from 0 to col-num
         for col = (concatenate 'string col
                                (format nil (nth x col-format)
                                        (nth x col-names)))
         finally (return col)) "~%"
         (loop for hash-entry in weather-data
            for row =
              (concatenate 'string row
                (loop for x from 0 to col-num
                   for col =
                     (concatenate 'string col
                       (format nil (nth x col-format)
                               (nth x (weather-process hash-entry))))
                   finally (return col)) "~%")
            finally (return row)) "~%")))

(defcommand weather () (:rest)
  (let* ((input (completing-read (current-screen) "City,country code: "
                                 *openweather-loc-defaults*))
         (type (completing-read (current-screen) "Type: "
                                '("forecast" "current")
                                :initial-input "current"))
         (result (weather-get input :type type)))
    (message-no-timeout (weather-format result input))))
