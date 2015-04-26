(import [paste.fixture [TestApp]])
(import [nose.tools [*]])
(import [hypo [get-application]])

(defn test-name []
  "The root page shows the application name"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (response (test-app.get "/")))
    (assert-equal response.status 200)
    (.mustcontain response "Hypo")))

(defn test-upload []
  "Uploading a file returns a URL"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "foo.py") (str "somecontent"))])
        (response (apply test-app.put ["/test_code.hy"]
                         {"upload_files" test-files})))
    (assert-equal response.status 201)
    (.mustcontain response "http://localhost/")))

(defn test-view []
  "After uploading a file it can be viewed"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "foo.py") (str "somecontent"))])
        (upload-response
         (apply test-app.put ["/test_code.hy"]
                {"upload_files" test-files}))
        (view-url (.strip upload-response.body))
        (view-path (view-url.replace "http://localhost" ""))
        (view-response (test-app.get view-path)))
    (assert-equal view-response.status 200)
    (.mustcontain view-response "test_code.hy" "somecontent")))
