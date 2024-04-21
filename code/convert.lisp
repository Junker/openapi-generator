(cl:in-package :openapi-generator)

(defun convert-by-url (url &key (converter-url *converter-url*))
  (dex:get
   (render-uri
    (quri:merge-uris (concat "api/convert?url="
                             (quri:url-encode url))
                     converter-url))))

(defun convert-by-content (&key content (content-type "application/json")
                             (converter-url *converter-url*))
  (dex:post
   (quri:render-uri (quri:merge-uris "api/convert" converter-url))
   :content content
   :headers (remove-empty-values
             (list
              (cons "Content-Type"
                    (cond
                      (content-type
                       (if (find content-type
                                 (list "application/json" "application/yaml")
                                 :test (function string-equal))
                           content-type
                           (warn
                            "The body type ~A is not mentioned. Valid content-types are: ~A"
                            content-type "application/json application/yaml")))
                      (t "application/json")))))))

(defun convert-to-openapi-3 (&key url content pathname (content-type "json") (converter-url *converter-url*))
  (when (and url content pathname)
    (error "You can only supply either url or content, not both simultaneously."))
  (let ((content-type
          (case-using (function string-equal) content-type
            ("json"
             "application/json")
            (("yaml" "yml")
             "application/yaml"))))
    (cond (url
           (convert-by-url url :converter-url converter-url))
          (content
           (convert-by-content :content content :content-type content-type :converter-url converter-url))
          (pathname
           (convert-by-content :content (from-file pathname) :content-type content-type :converter-url converter-url))
          (t
           (error "You have to supply either url, content, or pathname.")))))
