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

(import web sys os hashlib datetime shutil
        [pygments [highlight]]
        [pygments.lexers [get-lexer-by-name guess-lexer-for-filename]]
        [pygments.formatters [HtmlFormatter]]
        [pygments.util [ClassNotFound]]
        [gittle [Gittle]])

(try (import [config [*]])
     (catch [ImportError]
       (print "Please copy the config.example.hy to config.hy and set"
              "the values to your preference.")
       (sys.exit 1)))

(def render (web.template.render "templates/"))
(def urls (, (+ "/" *prefix*) "index"
             (+ "/" *prefix* "raw/(.*)") "raw"
             (+ "/" *prefix* "dl/(.*)") "download"
             (+ "/" *prefix* "([a-f0-9]{7})$") "html"
             (+ "/" *prefix* "upload/(.*)") "upload"))

(defun hashes [name]
  (let ((hasher (hashlib.sha1)))
    (hasher.update name)
    (hasher.update (str (datetime.datetime.now)))
    (let ((digest (hasher.hexdigest)))
      (, (slice digest 0 7) digest))))

(defun get-type [ext]
  (cond
   [(in (.lower ext) (, ".jpg" ".jpeg" ".png" ".gif")) "image"]
   [True "text"]))

(defn get-content-type [ext]
  (let ((lowered (ext.lower)))
   (cond
    [(in lowered (, ".jpg" ".jpeg")) "image/jpeg"]
    [".png" "image/png"]
    [".gif" "image/gif"]
    [True "text/plain"])))

(defun read-file [filename]
  (let (res)
    (with [[f (file filename "r")]]
          (setv res (f.read)))
    res))

(defun no-such-file []
  (setv web.ctx.status (str "404 Not Found"))
  (render.notfound))

(defun get-lexer [filename content]
  "Try to guess the correct lexer by FILENAME and CONTENT.

If no lexer is found fallback onto the text lexer."
  (try (guess-lexer-for-filename filename content)
       (catch [ClassNotFound]
         (get-lexer-by-name "text"))))

(defun get-raw [self name]
  (let ((dirname (+ "files/" (os.path.dirname name)))
        (basename (os.path.basename name))
        (ext (get (os.path.splitext basename) 0))
        (repo (and (os.path.exists dirname)
                   (Gittle dirname)))
        (resp (if repo
                (get (.commit-file repo (str "HEAD") basename)
                     "data"))))
    (web.header "Content-Type" (get-content-type ext))
    (or resp (no-such-file))))

(defun get-attachment [self name]
  (let ((dirname (+ "files/" (os.path.dirname name)))
        (repo (and (os.path.exists dirname)
                   (Gittle dirname))))
    (if repo
      (progn
       (web.header "Content-Disposition"
                   (+ "attachment; filename=\"" name "\""))
       (get (.commit-file repo "HEAD" (os.path.basename name)) "data"))
      (no-such-file))))

(defun render-file [hash repo ref filename]
  (if (not (os.path.isdir filename))
    (let ((content (get (.commit-file repo ref filename) "data"))
          (lexer (get-lexer filename content))
          (formatter (HtmlFormatter))
          (kwargs {"file" filename "hash" hash}))
      (.update
       kwargs (if (in (.lower (get (os.path.splitext filename) 1))
                    [".png" ".jpg" ".jpeg" ".gif"])
              {"content" (apply render.image []
                                {"name" filename
                                 "hash" hash})
               "style" ""}
              {"content" (highlight content lexer formatter)
               "style" (formatter.get-style-defs ".highlight")}))
      (apply render.main [] kwargs))
    ""))

(defun get-html [self name]
  (let ((dirname (+ "files/" name))
        (repo (and (os.path.exists dirname)
                   (Gittle dirname))))
    (if repo
      (car (list-comp (render-file name repo (str "HEAD") f)
                      [f (.iterkeys (.commit-tree repo (str "HEAD")))]
                      (not (or (= f ".")
                               (= f "..")))))
      (no-such-file))))

(defun delete-dir [self name]
  (let ((dirname (+ "files/" name)))
    (if (os.path.exists dirname)
      (do (shutil.rmtree dirname)
          (+ "Successfully removed " name "\n"))
      (no-such-file))))

(defun upload-file [self name]
  (let ((h (hashes name))
        (dirname (+ "files/" (get h 0))))
    (os.makedirs dirname)
    (with [[f (file (+ dirname "/" name) "w")]]
          (.write f (web.data)))
    (let ((repo (Gittle.init dirname)))
      (.stage repo [(str name)])
      (apply repo.commit []
             {"name" "Hypo"
              "email" "hypo@ryuslash.org"
              "message" "Initial commit"}))
    (setv web.ctx.status (str "201 Created"))
    (web.header "Content-Type" "text/plain")
    (+ web.ctx.home "/" *prefix* (get h 0) "\n")))

(defun upload-file-post [self name]
  (let ((input (apply web.input [] {(str "myfile") {}}))
        (upload (get input (str "myfile")))
        (h (hashes upload.filename))
        (dirname (+ "files/" (get h 0))))
    (os.makedirs dirname)
    (with [[f (file (+ dirname "/" upload.filename) "w")]]
          (f.write upload.value))
    (let ((repo (Gittle.init dirname)))
      (repo.stage [upload.filename])
      (apply repo.commit []
             {"name" "Hypo"
              "email" "hypo@ryuslash.org"
              "message" "Initial commit"}))
    (raise (web.found (+ "/" (get h 0))))))

(defun upload-form [self name]
  (render.upload-form))

(defclass raw []
  [[GET get-raw]])

(defclass download []
  [[GET get-attachment]])

(defclass html []
  [[GET get-html]
   [DELETE delete-dir]])

(defclass upload []
  [[GET upload-form]
   [POST upload-file-post]
   [PUT upload-file]])

(defclass index []
  [[GET (lambda [self] (render.index))]])

(defun get-application []
  (web.application urls (globals)))

(defun hypo-start [argv]
  (let ((sys.argv argv)
        (app (web.application urls (globals))))
    (.run app)))

(defun hypo-start-wsgi [argv]
  (let ((sys.argv argv)
        (app (web.application url (globals))))
    (app.wsgifunc)))
