#!/usr/bin/env hy
;; Hypo -- Quickly share stuff
;; Copyright (C) 2013  Tom Willemse

;; Hypo is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; Hypo is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with Hypo.  If not, see <http://www.gnu.org/licenses/>.

(import web os
        [gittle [Gittle]])

(try (import [config [*]])
     (catch [ImportError]
       (print "Please copy the config.example.hy to config.hy and set"
              "the values to your preference.")
       (sys.exit 1)))

(def db
  (apply web.database []
         {"dbn" "postgres" "user" *dbuser* "pw" *dbpw* "db" *dbname*}))

(defun get-file [name]
  (let ((res (apply .select
                    [db "hfile" {"shash" name}]
                    {"where" "shash = $shash"})))
    (if res (car res))))

(foreach [f (os.listdir "files/")]
  (let ((file (get-file f)))
    (os.rename (+ "files/" f)
               (+ "files/" file.filename))
    (os.mkdir (+ "files/" f))
    (os.rename (+ "files/" file.filename)
               (+ "files/" f "/" file.filename))
    (let ((repo (Gittle.init (+ "files/" f))))
      (.stage repo [(str file.filename)])
      (apply repo.commit []
             {"name" "System"
              "email" "tom@ryuslash.org"
              "message" "Initial commit for upgrade to git"}))))
