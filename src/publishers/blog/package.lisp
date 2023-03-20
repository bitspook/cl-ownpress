(defpackage #:clown-pubishers.blog
  (:nicknames #:clown-blog)
  (:use :cl :serapeum/bundle :40ants-doc)
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

(defsection @publishers-clown-blog (:title "clown-blog")
  "Publish a static HTML blog from your collected blog posts, notes and projects."

  "## Themes
A blog is published based on a theme.

Check the [blog-themes](/blog-themes) section for available themes, and how to create your own."

  "## API"
  (*conf* variable)

  "### Utilities"
  (publish-blog function))

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

(defun copy-dirs (src dest)
  "Recursively copy SRC to DEST. It uses *nix `cp' under the hood, so don't use it
on Windows maybe."
  (uiop:run-program (format nil "cp -r ~a ~a" (ppath:join src "*") dest)
                    :output *standard-output*
                    :error-output *standard-output*))

(defun publish-static (dir)
  "Recursively copy DIR into the published destination so they can be accessible
as static assets."
  (ensure-directories-exist (conf :dest))
  (copy-dirs dir (conf :dest)))

(defun publish-file (filepath content)
  "Publish CONTENT to a file named FILEPATH.
FILEPATH is a path relative to the published destination."
  (let ((filepath (clown:join-paths (conf :dest) filepath)))
    (ensure-directories-exist filepath)
    (str:to-file filepath content)))

(defun publish-html-file (filepath html &key (clean-urls? t))
  "Publish HTML content (string) to FILEPATH file. If `CLEAN-URLS?` is non `nil', it
ensures that FILEPATH is a clean HTML url (i.e it looks like FILEPATH/index.html)."
  (let* ((filepath (if (and clean-urls? (not (string= (ppath:basename filepath) "index.html")))
                   (clown:join-paths filepath "/index.html")
                   filepath)))
    (publish-file filepath html)))

(defun post-public-path (post)
  (with-accessors ((cat post-category) (slug post-slug)) post
    (clown:join-paths "/" (or cat "") slug "/")))
