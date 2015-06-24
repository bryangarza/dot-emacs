;; blog.el

(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/bryangarza.github.io/org/"
         :publishing-directory "~/bryangarza.github.io/"
         :recursive t

         :base-extension "org"
         :html-extension "html"

         :publishing-function (org-html-publish-to-html)

         :html-preamble nil
         :html-postamble "
<div id=\"footer\">
<div id=\"footer-hr\"></div>
<p>
∴ <a href=\"/\">Home</a>, <a href=\"about.html\">About</a>, <a href=\"contact.html\">Contact</a>, <a href=\"https://github.com/bryangarza/\">GitHub</a>, <a href=\"https://twitter.com/bryangarza\">Twitter</a>, and <a href=\"https://www.quora.com/Bryan-Garza\">Quora</a></p></div>"

         :html-doctype "html5"
         :with-toc nil
         :with-timestamps t
         :section-numbers nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         :html-head "<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=560\">

<link rel=\"stylesheet\" href=\"css/normalize.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/org.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/tables.css\" type=\"text/css\" />"

         :auto-sitemap nil
         )))
