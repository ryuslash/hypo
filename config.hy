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
(import os)

;; Prefix to use when running. This option should contain a trailing
;; `/'. An example would be: if you're running this project under
;; http://example.com/hypo/ you should use "hypo/".
(def *prefix* (os.getenv "HYPO_PREFIX" ""))
