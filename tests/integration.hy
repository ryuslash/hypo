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

(defn test-text-raw-view []
  "After uploading a text file it can be viewed in raw form"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "foo.py") (str "somecontent"))])
        (upload-response
         (apply test-app.put ["/upload/test_code.hy"]
                {"upload_files" test-files}))
        (view-url (upload-response.body.strip))
        (view-path (view-url.replace "http://localhost" ""))
        (view-response (test-app.get (+ "/raw" view-path "/test_code.hy"))))
    (assert-equal view-response.status 200)
    (assert-equal (view-response.header "Content-Type") "text/plain")
    (view-response.mustcontain "somecontent")))

(defn test-image-raw-view []
  "After uploading an image file it can be viewed in raw form"
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "file") (str "tests/files/gradient.jpg"))])
        (upload-response
         (apply test-app.put ["/upload/gradient.jpg"]
                {"upload_files" test-files}))
        (view-url (.strip upload-response.body))
        (view-path (view-url.replace "http://localhost" ""))
        (view-response (test-app.get (+ "/raw" view-path "/gradient.jpg"))))
    (assert-equal view-response.status 200)
    (assert-equal (view-response.header "Content-Type") "image/jpeg")))

(defn test-web-upload []
  "There is a page to upload files through a form."
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (response (test-app.get "/upload/")))
    (response.mustcontain
     "<form method=\"POST\" enctype=\"multipart/form-data\""
     "<input name=\"myfile\""
     "<button type=\"submit\"")))

(defn test-form-upload []
  "Uploading an image through a POST request will redirect to its page."
  (let ((test-app (TestApp (.wsgifunc (get-application))))
        (test-files [(, (str "myfile") (str "something.txt") (str "somecontents"))])
        (response
         (apply test-app.post ["/upload/"] {"upload_files" test-files})))
    (assert-equal response.status 302)
    (setv response (response.follow))
    (assert-equal response.status 200)
    (response.mustcontain "somecontents")))
