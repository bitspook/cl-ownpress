(defpackage :clown-blog
  (:use :cl :alexandria :clown-publishers)
  (:import-from :spinneret :with-html-string)
  (:import-from #:clown *conf* conf-merge conf)
  (:export
   post-id post-slug post-title post-tags post-category post-published-at post-html-content
   fetch-posts fetch-recent-posts fetch-unlisted-posts
   post-public-path
   blog-theme theme-home-template theme-listing-template theme-post-template theme-assets-dir
   publish-blog publish-listing publish-post publish-rss-feed))
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
                :unlisted-categories nil
                :theme nil)))

(defun post-public-path (post)
  (with-accessors ((cat post-category) (slug post-slug)) post
    (clown:join-paths (or cat "") slug "/")))

(defclass blog-theme ()
  ((home :initform (error "Home view is required")
         :initarg :home
         :accessor theme-home-template)
   (listing :initform (error "Listing view is required")
            :initarg :listing
            :accessor theme-listing-template)
   (post :initform (error "Post view is required")
         :initarg :post
         :accessor theme-post-template)
   (assets-dir :initform (error "Assets directory is required")
               :initarg :assets-dir
               :accessor theme-assets-dir)))
