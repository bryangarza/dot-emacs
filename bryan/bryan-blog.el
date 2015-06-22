;; blog.el

(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/bryangarza.github.io/org/blog/"
         :publishing-directory "~/bryangarza.github.io/blog/"
         :recursive t

         :base-extension "org"
         :html-extension "html"

         :publishing-function (org-html-publish-to-html)

         :html-preamble nil
         :html-postamble "<div id=\"footer\">
<div id=\"footer-hr\"></div>
<p><a href=\"https://github.com/bryangarza/\">GitHub</a>, <a href=\"https://twitter.com/bryangarza\">Twitter</a>, <a href=\"https://medium.com/@bryangarza\">Medium</a>, and <a href=\"https://www.quora.com/Bryan-Garza\">Quora</a>. <a href=\"contact.html\"><strong>Contact</strong></a> me!</p></div>"

         :html-doctype "html5"
         :with-toc nil
         :section-numbers nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         :html-head "<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=560\">

<link rel=\"stylesheet\" href=\"../css/normalize.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"../css/org.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"../css/animate.min.css\" type=\"text/css\" />"

         :auto-sitemap t
         :sitemap-title "Blog"
         :sitemap-filename "blogmap"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%t → %d" ;write title and date in sitemap
         :sitemap-date-format "%B %d, %Y"
         )

        ("pages"
         :base-directory "~/bryangarza.github.io/org/pages/"
         :publishing-directory "~/bryangarza.github.io/"
         :recursive t

         :base-extension "org"
         :html-extension "html"

         :publishing-function (org-html-publish-to-html)

         :html-preamble nil
         :html-postamble "<div id=\"footer\">
<div id=\"footer-hr\"></div>
<p><a href=\"https://github.com/bryangarza/\">GitHub</a>, <a href=\"https://twitter.com/bryangarza\">Twitter</a>, <a href=\"https://medium.com/@bryangarza\">Medium</a>, and <a href=\"https://www.quora.com/Bryan-Garza\">Quora</a>. <a href=\"contact.html\"><strong>Contact</strong></a> me!</p></div>"

         :html-doctype "html5"
         :with-toc nil
         :section-numbers nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         :html-head "<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=560\">

<link rel=\"stylesheet\" href=\"css/normalize.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/org.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/animate.min.css\" type=\"text/css\" />"
         )))

(provide 'bryan-blog)
