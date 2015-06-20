;; blog.el

(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/blog/org/"
         :publishing-directory "~/code/html/site/out/blog/"
         :recursive t

         :base-extension "org"
         :html-extension "html"

         :publishing-function (org-html-publish-to-html)

         :html-preamble nil
         :html-postamble nil

         :html-doctype "html5"
         :with-toc nil
         :section-numbers nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         :html-head "<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=560\">

<link rel=\"stylesheet\" href=\"../css/normalize.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"../css/markdown.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"../css/animate.min.css\" type=\"text/css\" />"

         :auto-sitemap t
         :sitemap-title "My blog"
         :sitemap-filename "blogmap"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%t (%d)" ;write title and date in sitemap
         )))

(provide 'bryan-blog)
