(defpackage :clown-blog
  (:use :cl :alexandria :clown-publishers)
  (:import-from :spinneret :with-html-string)
  (:import-from #:clown *conf* conf-merge conf)
  (:export
   *debug-transpiles*
   *css-vars* css-var css-color font-css top-level-css adjustable-width-css to-css-str button-css
   post-id post-slug post-title post-tags post-category post-published-at post-html-content fetch-posts
   post-public-path
   publish-blog))
(in-package #:clown-blog)

(setf *conf* (conf-merge
              `(:author nil
                :avatar nil
                :twitter nil
                :linkedin nil
                :github nil
                :handle nil
                :resume nil
                :dest "./_site/"
                :static-dirs nil
                :site-url nil
                :mixpanel-token nil
                :rss-max-posts 10
                :control-tags ("blog-post" "published")
                :exclude-tags ("draft")
                :unlisted-categories nil)))

(defparameter *debug-transpiles* t)

(defun post-public-path (post)
  (with-accessors ((cat post-category) (slug post-slug)) post
    (clown:join-paths (or cat "") slug "/")))
