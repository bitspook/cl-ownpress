(defpackage #:clown-publishers.blog
  (:use #:cl #:clown #:clown-theme #:clown-blog)
  (:export
   *conf* conf conf-merge
   publish-static publish-file publish-html-file
   blog-theme theme-home-template theme-listing-template theme-post-template theme-assets-dir
   publish-blog publish-listing publish-post publish-rss-feed
   *debug-transpiles*
   default-theme))
(in-package #:clown-publishers.blog)

(defparameter default-theme default-theme:theme)

