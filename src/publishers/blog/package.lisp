(defpackage #:clown-pubishers.blog
  (:nicknames #:clown-blog)
  (:use :cl :alexandria)
  (:use #:clown-publishers)
  (:import-from #:clown *conf* conf-merge conf)
  (:import-from #:clown-blog.theme theme theme-home-template
                theme-listing-template theme-post-template theme-assets-dir
                theme-about-me-template theme-projects-listing-template theme-project-template
                with-html-string compile-and-write-lass-blocks)
  (:export
   post-id post-slug post-title post-tags post-category post-published-at post-html-content
   fetch-posts fetch-recent-posts fetch-unlisted-posts
   post-public-path
   project-id project-html-description project-html-content project-docs project-slug project-name
   project-issue-tracker project-languages project-public-path project-tagline
   project-source-code project-updated-at project-oracle-spec
   fetch-projects fetch-all-projects
   publish-blog publish-listing publish-post publish-rss-feed))
(in-package #:clown-blog)

(setf *conf* (conf-merge
              `(:author nil
                :avatar nil
                :mastodon nil
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
                :about-me-short nil
                :unlisted-categories nil
                :theme nil)))

(defun post-public-path (post)
  (with-accessors ((cat post-category) (slug post-slug)) post
    (clown:join-paths "/" (or cat "") slug "/")))
