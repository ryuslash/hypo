(import [paste.fixture [TestApp]])
(import [nose.tools [*]])
(import [hypo [get-application]])

(defn test-name []
  "The root page shows the application name"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (response (test-app.get "/")))
    (assert-equal response.status 200)
    (.mustcontain response "Hypo")))

(defn test-text-file-upload []
  "Uploading a text file returns a URL"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "foo.py") (str "somecontent"))])
        (response (apply test-app.put ["/upload/test_code.hy"]
                         {"upload_files" test-files})))
    (assert-equal response.status 201)
    (.mustcontain response "http://localhost/")))

(defn test-image-file-upload []
  "Uploading an image file returns a URL"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "tests/files/gradient.jpg"))])
        (response (apply test-app.put ["/upload/gradient.jpg"]
                         {"upload_files" test-files})))
    (assert-equal response.status 201)
    (.mustcontain response "http://localhost/")))

(defn test-text-view []
  "After uploading a text file it can be viewed"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "foo.py") (str "somecontent"))])
        (upload-response
         (apply test-app.put ["/upload/test_code.hy"]
                {"upload_files" test-files}))
        (view-url (.strip upload-response.body))
        (view-path (view-url.replace "http://localhost" ""))
        (view-response (test-app.get view-path)))
    (assert-equal view-response.status 200)
    (view-response.mustcontain "test_code.hy" "somecontent")))

(defn test-image-view []
  "After uploading an image file it can be viewed"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "tests/files/gradient.jpg"))])
        (upload-response
         (apply test-app.put ["/upload/gradient.jpg"]
                {"upload_files" test-files}))
        (view-url (.strip upload-response.body))
        (view-path (view-url.replace "http://localhost" ""))
        (view-response (test-app.get view-path)))
    (assert-equal view-response.status 200)
    (view-response.mustcontain "gradient.jpg" "<img src=\"raw/")))

(defn test-image-raw-view []
  "After uploading an image file it can be viewed"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "tests/files/gradient.jpg"))])
        (upload-response
         (apply test-app.put ["/upload/gradient.jpg"]
                {"upload_files" test-files}))
        (view-url (.strip upload-response.body))
        (view-path (view-url.replace "http://localhost" ""))
        (view-response (test-app.get (+ "/raw" view-path "/gradient.jpg"))))
    (assert-equal view-response.status 200)))
