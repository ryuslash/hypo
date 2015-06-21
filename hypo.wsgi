;; -*- mode: hy; -*-
(import sys [hypo [hypo-start-wsgi]])

(setv application (hypo-start-wsgi (cdr sys.argv)))
