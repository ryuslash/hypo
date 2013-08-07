#!/usr/bin/env hy
;; Hypo -- Sharing is caring.
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

(import web sys os hashlib datetime
        [pygments [highlight]]
        [pygments.lexers [guess-lexer-for-filename]]
        [pygments.formatters [HtmlFormatter]])

(try (import [config [*]])
     (catch [ImportError]
       (print "Please copy the config.example.hy to config.hy and set"
              "the values to your preference.")
       (sys.exit 1)))

(def render (web.template.render "templates/"))
(def urls (, (+ "/" *prefix* "raw/(.*)") "raw"
             (+ "/" *prefix* "dl/(.*)") "download"
             (+ "/" *prefix* "([a-f0-9]{7})$") "html"
             (+ "/" *prefix* "(.*)") "upload"))
(def db
  (kwapply (web.database)
           {"dbn" "postgres" "user" *dbuser* "pw" *dbpw* "db" *dbname*}))

(defun hashes [name]
  (let ((hasher (hashlib.sha1)))
    (hasher.update name)
    (hasher.update (str (datetime.datetime.now)))
    (let ((digest (hasher.hexdigest)))
      (, (slice digest 0 7) digest))))

(defun get-type [ext]
  (cond
   ((in ext (, ".jpg" ".jpeg" ".png" ".gif")) "image")
   (true "text")))

(defun read-file [filename]
  (let (res)
    (with [f (file filename "r")]
          (setv res (f.read)))
    res))

(defun no-such-file []
  "No such file.\n")

(defun get-file [name]
  (let ((res (kwapply (db.select "hfile" {"shash" name})
                      {"where" "shash = $shash"})))
    (if res (car res))))

(defclass raw []
  [[GET (lambda [self name]
          (let ((filename (+ "files/" name))
                (resp (if (os.path.exists filename)
                        (read-file filename))))
            (or resp (no-such-file))))]])

(defclass download []
  [[GET (lambda [self name]
          (let ((hfile (get-file name))
                (filename (+ "files/" name)))
            (if (and hfile (os.path.exists filename))
              (progn
               (web.header "Content-Disposition"
                           (+ "attachment; filename=\""
                              hfile.filename "\""))
               (read-file filename))
              (no-such-file))))]])

(defclass html []
  [[GET (lambda [self name]
          (let ((hfile (get-file name))
                (filename (+ "files/" name)))
            (if (and hfile (os.path.exists filename))
              (cond
               ((= hfile.type "text")
                (progn
                 (let ((content (read-file filename))
                       (lexer (guess-lexer-for-filename
                               hfile.filename content))
                       (formatter (HtmlFormatter)))
                   (kwapply (render.main)
                            {"content" (highlight content lexer
                                                  formatter)
                             "style" (formatter.get-style-defs
                                      ".highlight")
                             "file" hfile})
                   )))
               ((= hfile.type "image")
                (kwapply (render.main)
                         {"content" (kwapply (render.image)
                                             {"name" name})
                          "style" ""
                          "file" hfile})))
              (no-such-file))))]

   [DELETE (lambda [self name]
             (let ((filename (+ "files/" name)))
               (kwapply (db.delete "hfile")
                        {"where" (+ "shash = '" name "'")})
               (if (os.path.exists filename)
                 (os.remove filename)
                 (no-such-file))))]])

(defclass upload []
  [[PUT (lambda [self name]
          (let ((h (hashes name)))
            (with [f (file (+ "files/" (get h 0)) "w")]
                  (.write f (web.data)))
            (kwapply (db.insert "hfile")
                     {"shash" (get h 0)
                      "hash" (get h 1)
                      "filename" name
                      "type" (get-type (get (os.path.splitext name) 1))})
            (+ web.ctx.home "/" *prefix* (get h 0) "\n")))]])

(when (= __name__ "__main__")
  (let ((sys.argv (slice sys.argv 1))
        (app (web.application urls (globals))))
    (.run app)))
