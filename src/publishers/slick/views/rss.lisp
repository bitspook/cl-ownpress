(in-package :clown-slick.views)

(defun rss-post-str (post)
  (let ((post-url (clown:join-paths (conf :site-url) (clown:post-output-path post))))
    (format nil "<entry>~
                 <title>~a</title>~
                 <link href=\"~a\"></link>~
                 <id>~a</id>~
                 <updated>~a</updated>~
                 <summary></summary>~
               </entry>"
            (clown:post-title post)
            post-url
            post-url
            (clown:post-published-at post))))

(defun rss-str (posts &key title feed-url feed-updated-at)
  (format nil "<feed xmlns=\"http://www.w3.org/2005/Atom\">~
                              <link href=\"~a\"></link>~
                              <link href=\"~a\" rel=\"self\"></link>~
                              <updated>~a</updated>~
                              <author><name>~a</name></author>~
                              <id>~a</id>~
                              <title>~a</title>~
                              ~{~a~}~
                            </feed>"
          (conf :site-url) feed-url feed-updated-at
          (conf :author) (conf :site-url) title
          (mapcar #'rss-post-str posts)))
