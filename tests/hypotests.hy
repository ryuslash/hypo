(import hypo)
(import [nose.tools [assert-equal assert-is-instance assert-in with-setup]])
(import [mock [Mock patch]])
(import datetime)
(import web)

(def -original-datetime datetime.datetime)

(defclass fake-File []
  [[read (lambda [self] "Something read")]])

(defclass fake-file []
  [[--init-- (lambda [self file-name mode])]
   [--enter-- (lambda [self] (fake-File))]
   [--exit-- (lambda [self exc-type exc-value traceback] nil)]])

(defun test-hashes []
  (let [[fake-date (Mock)]]
    (setv fake-date.now.return-value (datetime.datetime 1 1 1))
    (with [[dt (patch "datetime.datetime" fake-date)]]
      (assert-equal
       (hypo.hashes "foo")
       (, "d3e482a" "d3e482adaaddb0dca4c5adf7ad90cfe97ca6f3be")))))

(defun test-types []
  (assert-equal (hypo.get-type ".jpg") "image")
  (assert-equal (hypo.get-type "foo") "text")
  (assert-equal (hypo.get-type ".JPG") "image"))

(defun test-reading []
  (with [[(patch.dict (get (globals) "__builtins__") {"file" fake-file})]]
        (assert-equal (hypo.read-file "foo") "Something read")))

(defun test-no-such-file []
  (let [[result (hypo.no-such-file)]]
    (assert-is-instance result web.template.TemplateResult)
    (assert-in "Not Found" (get result "__body__"))))

(defun test-get-lexer []
  (assert-equal (. (hypo.get-lexer "foo.el" "") name) "Common Lisp")
  (assert-equal (. (hypo.get-lexer "foo" "") name) "Text only"))
