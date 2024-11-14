;;; init-net.el --- Net Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

;; url

(defcustom mn/img-cdn-server-list
  '("https://images.weserv.nl?url=${href_ue}"
    "https://imageproxy.pimg.tw/resize?url=${href_ue}")
  "Public Image CDN server."
  :group 'my
  :type '(repeat string))

(defcustom mn/img-cdn-server (car mn/img-cdn-server-list)
  "Default Image CDN server."
  :group 'my
  :type 'string)

;; https://hub.zzzr.eu.org/curl/curl/wiki/DNS-over-HTTPS E
(defcustom mn/doh-server-list '("9.9.9.9/dns-query"
                                "doh.bortzmeyer.fr"
                                "dns.digitalsize.net/dns-query"
                                "1.1.1.1/dns-query"
                                "1.12.12.12/dns-query"
                                "223.6.6.6/dns-query")
  "Public DOH Server."
  :group 'my
  :type '(repeat string))

(defcustom mn/doh-server (concat "https://" (car mn/doh-server-list))
  "Default DOH Server."
  :group 'my
  :type 'string)

(defun mn/default-callback (&rest args)
  "Test default callback."
  (message (if args "Up" "Down")))

(defun mn/internet-up-p (&optional host callback)
  "Test connectivity via ping."
  (interactive)
  (let* ((args (if mn/sys-winnt-p '("-n" "1" "-w" "1") '("-c1" "-W1")))
         (proc (apply #'start-process "internet-test" nil "ping"
                      (if host host "baidu.com") args))
         (callback (if callback callback 'mn/default-callback)))
    (set-process-sentinel proc (lambda (proc signal)
                                 (apply callback
                                        (if (= 0 (process-exit-status proc))
                                            '(t) nil))))))

(defun mn/url-up-p (url &rest cbargs)
  "Test connectivity via curl."
  (let* ((callback (plist-get cbargs :callback))
         (max-time (plist-get cbargs :max-time))
         (doh-url (plist-get cbargs :doh-url))
         (proxy (plist-get cbargs :proxy))
         (args `("-kqsLm" ,(if max-time (int-to-string max-time) "10")
                 ,@(when doh-url (list "--doh-url" doh-url))
                 ,@(when proxy (list "-x" proxy))
                 "-o/dev/null" "-w%{http_code}" ,url))
         (proc (apply #'start-process "url-test" nil "curl" args))
         (callback (if callback callback 'mn/default-callback)))
    (set-process-filter proc (lambda (proc line)
                               (apply callback
                                      (if (string= "200" line)
                                          '(t) nil))))))

(defun mn/doh-up-p (&optional doh-url callback)
  "Test DOH availability."
  (interactive)
  (let ((args `(:doh-url ,(if doh-url doh-url mn/doh-server)
                         :callback ,(when callback callback))))
    (apply #'mn/url-up-p "https://www.baidu.com" :max-time 3 args)))

(defun mn/advice-url-retrieve-with-timeout (orig-fun &rest args)
  "Use `url-queue-retrieve' instead of `url-retrieve'."
  (cl-flet ((url-retrieve #'url-queue-retrieve)
            (url-queue-timeout 30))
    (apply orig-fun args)))

(defun mn/url-http-parse-response ()
  "Parse http response, From 'https://emacs-china.org/t/elisp-http/18432/2'."
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (let ((headers `(("Status"
                    . ,(progn
                         (re-search-forward "^[^:]+? \\([[:digit:]]+\\)"
                                            (pos-eol) t 1)
                         (match-string 1)))))
        body)
    (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                              url-http-end-of-headers t)
      (push (cons (match-string 1)
                  (match-string 2))
            headers))
    (setq headers (nreverse headers))
    (goto-char (1+ url-http-end-of-headers))
    (setq body (buffer-substring (point) (point-max)))
    (list headers body)))


;; proxy

(defcustom mn/block-domain
  '("blogs.reimu.net")
  "Domain blocked."
  :group 'my
  :type '(repeat string))

(defcustom mn/proxy-domain
  '("duckduckgo.com"
    "github.com"
    "github.io"
    "google.com"
    "google.com.hk"
    "googleapis.com"
    "wikipedia.org")
  "Domain proxyed."
  :group 'my
  :type '(repeat string))

(defcustom mn/img-proxy-domain
  '("saxyit.com" "reimu.net" "img.piclabo.xyz" "trts.baishancdnx.cn"
    "www.skidrowreloaded.com" "riotpixels.net" "www.hacg.mov"
    "mooc-image.nosdn.127.net")
  "Img domain proxyed."
  :group 'my
  :type '(repeat string))

(defcustom mn/block-domain-regexp
  (rx (| ?. bos) (regex (regexp-opt mn/block-domain)) eos)
  "Regexp for domain blocked."
  :group 'my
  :type 'regexp)

(defcustom mn/proxy-domain-regexp
  (rx (| ?. bos) (regex (regexp-opt mn/proxy-domain)) eos)
  "Regexp for domain through proxy."
  :group 'my
  :type 'regexp)

(defcustom mn/img-proxy-domain-regexp
  (rx (| ?. bos) (regex (regexp-opt mn/img-proxy-domain)) eos)
  "Regexp for img domain through proxy."
  :group 'my
  :type 'regexp)

(defcustom mn/centaur-proxy "127.0.0.1:10808"
  "Set HTTP/HTTPS proxy."
  :group 'my
  :type 'string)

(defcustom mn/centaur-socks-proxy "127.0.0.1:10807"
  "Set SOCKS proxy."
  :group 'my
  :type 'string)

(defun mn/curl-parameters-dwim (url &optional blockable-p &rest args)
  "Generate explicit parameters for curl."
  (let ((url url)
        (host (url-host (url-generic-parse-url url)))
        parameters)
    (cond
     ((and blockable-p
           (string-match-p mn/block-domain-regexp host))
      (setq url "127.0.0.1"))
     ((string-match-p mn/proxy-domain-regexp host)
      (setq parameters (cons (format "-xhttp://%s" mn/centaur-proxy)
                             parameters))))
    (list url parameters)))

(setq url-proxy-services
        `(("http" . ,mn/centaur-proxy)
          ("https" . ,mn/centaur-proxy)
          ("yes_proxy" . ,mn/proxy-domain-regexp)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))

(defun mn/url-find-proxy-for-url (urlobj host)
  "Determine proxy settings for URL based on host and proxy services.

This function is designed to be used as a custom
`url-proxy-locator'.  It checks if the HOST matches the
'yes_proxy' pattern in `url-proxy-services'.  If there's a match
and a corresponding proxy is found for the URL type, it returns a
proxy string. Otherwise, it returns \"DIRECT\" for direct
connection.

Arguments:
URLOBJ: A URL object as returned by `url-generic-parse-url'.
HOST: A string representing the hostname.

Returns:
A string either in the format \"PROXY host:port\" or \"DIRECT\"."
  (if-let* ((s (alist-get "yes_proxy" url-proxy-services nil nil #'equal))
            ((string-match-p s host))
            (proxy (cdr (assoc (url-type urlobj) url-proxy-services))))
      (concat "PROXY " proxy)
    "DIRECT"))
(setq url-proxy-locator #'mn/url-find-proxy-for-url)

(defun mn/minify-js-buffer ()
  "Minimize JavaScript content in the current buffer by removing comments
and extra whitespace."
  (interactive)
  (save-excursion
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      ;; Remove multi-line comments
      (setq content (replace-regexp-in-string "/\\*\\(.\\|\n\\)*?\\*/" "" content))
      ;; Remove single-line comments
      (setq content (replace-regexp-in-string "//.*" "" content))
      ;; Remove unnecessary whitespace (newlines, tabs, spaces)
      (setq content (replace-regexp-in-string "[ \t\n]+" " " content))
      ;; Remove whitespace around specific punctuation
      (setq content (replace-regexp-in-string "\\s-*\\([{}();,:+]\\)\\s-*" "\\1" content))
      ;; Remove extra spaces at the beginning and end of the buffer
      (setq content (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" content))
      ;; Clear buffer and insert the optimized content
      (delete-region (point-min) (point-max))
      (insert content))))

(with-eval-after-load 'autoinsert
  (defvar mn/proxy-data-file (expand-file-name "data/proxy.gpg" auto-insert-directory)
    "File for store proxy data."))

(defun mn/add-domain-to-proxy (url)
  "Add a domain to the SOCKS5 proxy rules in `mn/proxy-data-file`.

This function takes a URL as input, extracts the domain, and adds it to
the SOCKS5 proxy rules in the proxy data file. If the URL does not start
with 'http', it prepends 'http://' to it. The function then reads the
proxy data file, updates the SOCKS5 rules with the new domain, removes
duplicates, sorts the rules, and saves the updated proxy data file.

Arguments: url -- The URL from which to extract the domain to be added
  to the proxy rules."
  (interactive "surl: ")
  (unless (string-prefix-p "http" url)
    (setq url (concat "http://" url)))
  (with-current-buffer (find-file-noselect mn/proxy-data-file)
    (goto-char 1)
    (let* ((host (url-host (url-generic-parse-url url)))
           (domain (if (string-match "\\(?:[[:alnum:]-]+\\.\\)?\\([[:alnum:]-]+\\.[[:alnum:]]+\\)$" host)
                       (match-string 1 host)
                     host))
           (proxy-rules (read (current-buffer)))
           (host-rules (alist-get "hostRule" proxy-rules nil nil #'equal))
           (socks5-rule (cl-find-if (lambda (a) (string-prefix-p "SOCKS5 " a)) host-rules :key #'car)))
      (setcdr socks5-rule (cons (downcase domain) (cdr socks5-rule)))
      (dolist (h host-rules)
        (cl-delete-duplicates h :test #'string=)
        (cl-sort (cdr h) #'string<))
      (erase-buffer)
      (insert (prin1-to-string proxy-rules))
      (save-buffer)
      (message "%s added to proxy." domain)))
  (mn/generate-pac-file))

(defun mn/generate-pac-file ()
  "Generate a PAC file from proxy rules and save it to a specified location.

This function reads proxy rules from `mn/proxy-data-file`, processes the
host and IP rules, and inserts them into a PAC file template. The
resulting PAC file is then minified and saved to the specified location.

Steps:
1. Read the proxy rules from `mn/proxy-data-file`.
2. Extract host and IP rules.
3. Insert the rules into a PAC file template.
4. Minify the JavaScript in the buffer.
5. Save the buffer content to `~/www/wpad.dat` as the PAC file.

ref:
chrome://net-internals#proxy
https://support.microsoft.com/en-us/topic/how-to-disable-automatic-proxy-caching-in-internet-explorer-92735c9c-8a26-d0d8-7f8a-1b46595cbaba"
  (with-temp-buffer
    (insert-file-contents-literally mn/proxy-data-file)
    (when-let* (((string-suffix-p ".gpg" mn/proxy-data-file))
               (epa-replace-original-text t))
      (epa-decrypt-region (point-min) (point-max)))
    (let* ((proxy-rules (read (current-buffer)))
           (host-rules (alist-get "hostRule" proxy-rules nil nil #'equal))
           (ip-rules (alist-get "ipRule" proxy-rules nil nil #'equal)))
      (erase-buffer)
      (insert-file-contents-literally
       (expand-file-name "wpad.pac" auto-insert-directory))
      (delete-region (point-min) (pos-eol))
      (insert "var hostRulesMap="
              (json-encode
               (mapcan (lambda (a)
                         (let ((action (car a)))
                           (mapcar (lambda (b) (cons b action)) (cdr a))))
                       host-rules))
              ";var ipRulesMap=" (json-encode ip-rules) ";"))
    (mn/minify-js-buffer)
    (write-region nil nil (expand-file-name "~/www/wpad.dat"))))

(defun mn/proxy-up-p (&optional proxy callback)
  "Test Proxy availability."
  (interactive)
  (let ((args `(:proxy ,(if proxy proxy
                          (concat "http://" mn/centaur-proxy))
                       :callback ,(when callback callback))))
    (apply #'mn/url-up-p "https://www.baidu.com" :max-time 2 args)))

(defun mn/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" mn/centaur-proxy)
    (message "No HTTP proxy")))

(defun mn/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,mn/centaur-proxy)
          ("https" . ,mn/centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (mn/proxy-http-show))

(defun mn/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (mn/proxy-http-show))

(defun mn/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (mn/proxy-http-disable)
    (mn/proxy-http-enable)))

(defun mn/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun mn/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string mn/centaur-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" mn/centaur-socks-proxy))
  (mn/proxy-socks-show))

(defun mn/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (mn/proxy-socks-show))

(defun mn/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (mn/proxy-socks-enable)))


;; Newsticker

(defcustom mn/rss-bridge-list '("rss-bridge.kkky.fun:2053"
                                "rss-bridge.org/bridge01"
                                "rss.nixnet.services"
                                "wtf.roflcopter.fr/rss-bridge"
                                "rssbridge.flossboxin.org.in")
  "Public RSS-Bridge Server."
  :group 'my
  :type '(repeat string))

(defcustom mn/rss-hub-list '("rsshub.kkky.fun/"
                             "rsshub.rssforever.com/"
                             "rsshub.feeded.xyz/"
                             "hub.slarker.me/"
                             "rsshub.liumingye.cn/"
                             "rsshub-instance.zeabur.app/"
                             "rss.fatpandac.com/"
                             "rsshub.pseudoyu.com/"
                             "rsshub.friesport.ac.cn/"
                             "rsshub.atgw.io/")
  "Public RSSHub Server."
  :group 'my
  :type '(repeat string))

(defcustom mn/rss-bridge-server (concat "https://" (car mn/rss-bridge-list))
  "RSS bridge default server."
  :group 'my
  :type 'string)

(defcustom mn/rss-hub-server (concat "https://" (car mn/rss-hub-list))
  "RSS hub default server."
  :group 'my
  :type 'string)

(defun mn/rss-bridge-generator (bridge &optional proxy cache-timeout)
  "Generate atom feed via rss-bridge."
  (if-let* ((obj (url-generic-parse-url mn/rss-bridge-server))
            (request (format "%s/?action=display&format=Atom&bridge=%s&"
                             mn/rss-bridge-server bridge))
            (found (car (auth-source-search :host (url-host obj)
                                            :port (url-portspec obj)
                                            :max 1))))
      (replace-regexp-in-string
       "\\`https://\\(.+\\)"
       (format "https://%s:%s@\\1&_noproxy=%s&_cache_timeout=%d&"
               (plist-get found :user) (auth-info-password found)
               (if proxy "off" "on") (if cache-timeout cache-timeout 1800))
       request)
    request))

(defun mn/rss-bridge-wp (blog limit &optional content)
  "Returns the newest full posts of a WordPress powered website."
  (concat (mn/rss-bridge-generator "WordPressBridge")
          (url-build-query-string
           (cl-delete-if
            #'null
            `(("url" ,blog)
              ("limit" ,limit)
              ("content_selector" ,content))
            :key #'cadr))))
;; todo
;; (defun mn/rss-bridge-filter ())

(defun mn/rss-bridge-reducer (feed percentage)
  "Choose a percentage of a feed you want to see."
  (concat (mn/rss-bridge-generator "FeedReducerBridge")
          (url-build-query-string
           (cl-delete-if
            #'null
            `(("url" ,feed)
              ("percentage" ,percentage))
            :key #'cadr))))

(defun mn/rss-bridge-css-expander (feed limit content &optional
                                        content-cleanup
                                        dont-expand-metadata
                                        discard-thumbnail)
  "Expand any site RSS feed using CSS selectors."
  (concat "https://rss-bridge.org/bridge01/?action=display&format=Atom&bridge=CssSelectorFeedExpanderBridge&"
          ;; (mn/rss-bridge-generator "CssSelectorFeedExpanderBridge")
          (url-build-query-string
           (cl-delete-if
            #'null
            `(("feed" ,feed)
              ("limit" ,limit)
              ("content_selector" ,content)
              ("content_cleanup" ,content-cleanup)
              ("dont_expand_metadata" ,(when dont-expand-metadata "on"))
              ("discard_thumbnail" ,(when discard-thumbnail "on")))
            :key #'cadr))))

(cl-defun mn/rss-bridge-css-selector
    ( home limit entry load-pages &key content title time time-fmt url
      url-pattern title-cleanup content-cleanup cookie author cat rm-style)
  "Convert any site to RSS feed using CSS selectors. The bridge first
selects the element describing the article entries. It then extracts
the links to the articles from these elements. It then, depending on
the setting 'load_pages', either parses the selected elements,
or downloads the page for each article and parses those. Parsing the
elements or page is done using the provided selectors."
  (concat (mn/rss-bridge-generator "CssSelectorComplexBridge")
          (url-build-query-string
           (cl-delete-if
            #'null
            `(("home_page" ,home)
              ("limit" ,limit)
              ("entry_element_selector" ,entry)
              ("use_article_pages" ,(when load-pages "on"))
              ("article_page_content_selector" ,content)
              ("title_selector" ,title)
              ("time_selector" ,time)
              ("time_format" ,time-fmt)
              ("url_selector" ,url)
              ("url_pattern" ,url-pattern)
              ("title_cleanup" ,title-cleanup)
              ("content_cleanup" ,content-cleanup)
              ("cookie" ,cookie)
              ("author_selector" ,author)
              ("category_selector" ,cat)
              ("remove_styling" ,(when rm-style "on")))
            :key #'cadr))))

(defun mn/rss-bridge-merger (feeds limit name)
  "This bridge merges two or more feeds into a single feed. Max 10
items are fetched from each feed."
  (when (> (length feeds) 10)
    (user-error "Feed: %s is reach Max feeds limit."
                (propertize "aaa" 'face '(:inherit 'font-lock-warning-face))))
  (concat (mn/rss-bridge-generator "FeedMergeBridge")
          (let ((m 0))
            (mapconcat
             (lambda (feed)
               (setq m (+ m 1))
               (concat "&feed_" (number-to-string m) "="
                       (url-hexify-string feed)))
             feeds))
          "&limit=" (number-to-string limit)
          "&feed_name=" (url-hexify-string name)))

(cl-defun mn/rss-hub-generator
    ( router &key fmt limit full brief unsort opencc scihub f-uncase f
      f-title f-desc f-author f-cat f-time fo fo-title fo-desc fo-author
      fo-cat img-tp domain)
  "Generate feed via RSSHub."
  (let* ((main (concat mn/rss-hub-server router
                       (when fmt (concat "." fmt))))
         (url (url-generic-parse-url main))
         (code (auth-source-pick-first-password :host (url-host url))))
    (concat main "?"
            (url-build-query-string
             (cl-delete-if
              #'null
              `(("limit" ,limit)
                ("mode" ,(when full "fulltext"))
                ("brief" ,brief)
                ("sorted" ,(when unsort "false"))
                ("opencc" ,opencc)
                ("scihub" ,(when scihub 1))
                ("filter_case_sensitive" ,(when f-uncase "false"))
                ("filter" ,f)
                ("filter_title" ,f-title)
                ("filter_description" ,f-desc)
                ("filter_author" ,f-author)
                ("filter_category" ,f-cat)
                ("filter_time" ,f-time)
                ("filterout" ,fo)
                ("filterout_title" ,fo-title)
                ("filterout_description" ,fo-desc)
                ("filterout_author" ,fo-author)
                ("filterout_category" ,fo-cat)
                ;; ("image_hotlink_template" ,mn/img-cdn-server)
                ("domain" ,domain)
                ("code" ,(md5 (concat (url-filename url) code))))
              :key #'cadr)))))

(cl-defun mn/rss-hub-transform
    ( url s-fmt &key title item item-title item-title-a item-link
      item-link-a item-desc iten-desc-a item-pub item-pub-a extra)
  "Pass URL and transformation rules to convert HTML/JSON into RSS."
  (apply
   #'mn/rss-hub-generator
   (format "rsshub/transform/%s/%s/%s"
           s-fmt (url-hexify-string url)
           (url-build-query-string
            (cl-delete-if
             #'null
             `(("title" ,title)
               ("item" ,item)
               ("itemTitle" ,item-title)
               ("itemTitleAttr" ,item-title-a)
               ("itemLink" ,item-link)
               ("itemLinkAttr" ,item-link-a)
               ("itemDesc" ,item-desc)
               ("itemDescAttr" ,item-desc-a)
               ("itemPubDate" ,item-pub)
               ("itemPubDateAttr" ,item-pub-a))
             :key #'cadr)))
   extra))

(defun mn/atom-builder (title link entrys &optional id updated author)
  "Create brief atom feeds."
  (if-let* ((dir (expand-file-name "builder" newsticker-dir))
            (mkdir (or (make-directory dir t) t))
            (log-path (expand-file-name (concat title (if entrys ".xml" ".err")) dir))
            (err entrys))
      (let ((updated (or updated (format-time-string "%FT%T%z")))
            (author (or author (url-host (url-generic-parse-url link))))
            (id (or id (concat "urn:uuid:" (my/generate-uuid (concat title link updated)))))
            (link (string-replace "&" "&amp;" link)))
        (erase-buffer)
        (insert-file-contents-literally (expand-file-name "atom.xml" auto-insert-directory))
        (goto-char (point-min))
        (search-forward "{{title}}" nil nil 1) (replace-match title t t)
        (search-forward "{{link}}" nil nil 1) (replace-match link t t)
        (search-forward "{{updated}}" nil nil 1) (replace-match updated t t)
        (search-forward "{{author}}" nil nil 1) (replace-match author t t)
        (search-forward "{{id}}" nil nil 1) (replace-match id t t)
        (re-search-forward "<entry>[^z-a]*</entry>" nil nil 1)
        (with-restriction (match-beginning 0) (match-end 0)
          (let ((entry (buffer-string)))
            (delete-region (point-min) (point-max))
            (mapc
             (lambda (a)
               (let* ((min (point))
                      (updated (or (plist-get a :updated) updated))
                      (author (or (plist-get a :author) author))
                      (title (or (plist-get a :title) title))
                      (link (or (string-replace "&" "&amp;" (plist-get a :link)) link))
                      (id (or (plist-get a :id)
                              (concat "urn:uuid:"
                                      (my/generate-uuid
                                       (concat title link updated)))))
                      (content (plist-get a :content))
                      (category (plist-get a :category)))
                 (insert (concat "\n  " entry))
                 (search-backward "<category><![CDATA[{{category}}]]></category>")
                 (replace-match
                  (if category
                      (mapconcat
                       (lambda (a) (format "<category><![CDATA[%s]]></category>" a))
                       category "\n    ")
                    "")
                  t t)
                 (search-backward "{{content}}") (replace-match content t t)
                 (search-backward "{{updated}}") (replace-match updated t t)
                 (search-backward "{{id}}") (replace-match id t t)
                 (search-backward "{{link}}") (replace-match link t t)
                 (search-backward "{{author}}") (replace-match author t t)
                 (search-backward "{{title}}") (replace-match title t t))
               (goto-char (point-max)))
             entrys)))
        (write-region 1 (point-max) log-path))
    (write-region 1 (point-max) log-path)
    (kill-current-buffer)
    (user-error "*Feed %s is broken.*" title)))

(defun mn/atom-boss-builder (title url buf)
  "Generate atom feeds for Boss ZhiPin."
  (with-current-buffer buf
    (mn/atom-builder
     title url
     (progn
       (goto-char 1)
       (mapcar
        (lambda (a)
          `( :updated
             ,(format-time-string
               "%FT%T%z"
               (time-convert (/ (gethash "lastModifyTime" a) 1000)))
             :author ,(gethash "brandName" a)
             :link ,(format "https://www.zhipin.com/job_detail/%s.html"
                            (gethash "encryptJobId" a))
             :category ,(gethash "welfareList" a)
             :content ,(mapconcat #'identity (vconcat
                                              (make-vector 1 "<h1>jobLabels</h1>")
                                              (gethash "jobLabels" a)
                                              (make-vector 1 "<br><h1>Skills</h1>")
                                              (gethash "skills" a))
                                  "<br>")
             :title ,(format "%s-%s" (gethash "cityName" a)
                             (gethash "jobName" a))))
        (gethash "jobList" (gethash "zpData" (json-parse-buffer))))))))

(defun mn/newsticker--sentinel (process event)
  "Sentinel for extracting news titles from an text buffer.
Argument PROCESS is the process which has just changed its state.
Argument EVENT tells what has happened to the process."
  (let* ((p-status (process-status process))
         (exit-status (process-exit-status process))
         (feed-name (process-get  process 'nt-feed-name))
         (feed-channel (process-get  process 'nt-feed-channel))
         (feed-limit (process-get  process 'nt-feed-limit))
         (command (process-command process))
         (feed-url (car (last command)))
         (buffer (process-buffer process)))
    (when (and (eq p-status 'exit)
               (= exit-status 0))
      (apply (intern (format "my/net-atom-%s-builder" feed-channel))
             (list feed-name feed-url buffer))
      (newsticker--sentinel-work event t feed-name command buffer))))

(defun mn/newsticker--url-stuff-it (channel &optional title args)
  "Generate url for feeds."
  (pcase channel
    ("boss"
     (concat "https://www.zhipin.com/wapi/zpgeek/search/joblist.json?"
             (url-build-query-string
              (seq-filter
               (lambda (a) (not (eq (cadr a) nil)))
               `(("salary" ,(plist-get args :salary))
                 ("jobType" ,(plist-get args :jobtype))
                 ("position" ,(plist-get args :position))
                 ("stage" ,(plist-get args :stage))
                 ("scale" ,(plist-get args :scale))
                 ("industry" ,(plist-get args :industry))
                 ("degree" ,(plist-get args :degree))
                 ("partTime" ,(plist-get args :parttime))
                 ("payType" ,(plist-get args :paytype))
                 ("experience" ,(plist-get args :experience))
                 ("city" ,(plist-get args :city))
                 ("query" ,(plist-get args :query))
                 ("scene" ,(plist-get args :scene))
                 ("pageSize" ,(plist-get args :pagesize))
                 ("page" ,(plist-get args :page))
                 ("multiSubway" ,(plist-get args :multisubway))
                 ("multiBusinessDistrict" ,(plist-get args :multibusinessdistrict)))))))))

(defun mn/newsticker--get-news-by-build
    (feed-name channel &optional curl-arguments limit extras)
  "Newsticker build atom feeds."
  (let ((buffername (concat " *newsticker-curl-" feed-name "*"))
        (url (mn/newsticker--url-stuff-it channel feed-name extras)))
    (with-current-buffer (get-buffer-create buffername)
      (erase-buffer)
      ;; throw an error if there is an old curl-process around
      (if (get-process feed-name)
          (error "Another curl-process is running for %s" feed-name))
      ;; start curl
      (let* ((args (append (or curl-arguments newsticker-wget-arguments)
                           ;; curl silence progress bar
                           (list "-s" url)))
             (proc (apply #'start-process feed-name buffername
                          newsticker-wget-name args)))
        (set-process-coding-system proc 'utf-8 'utf-8)
        (set-process-sentinel proc #'mn/newsticker--sentinel)
        (process-put proc 'nt-feed-name feed-name)
        (process-put proc 'nt-feed-channel channel)
        (process-put proc 'nt-feed-limit limit)
        (setq newsticker--process-ids (cons (process-id proc)
                                            newsticker--process-ids))
        (force-mode-line-update)))))

(defun mn/advice-newsticker--get-news-by-wget (args)
  (setcar (cddr args)
          (append (caddr args)
                  (cadr (mn/curl-parameters-dwim (cadr args)))))
  args)

(defun mn/newsticker-treeview-prev-page ()
  "Scroll item buffer."
  (interactive)
  (save-selected-window
    (select-window (newsticker--treeview-item-window) t)
    (condition-case nil
        (scroll-down nil)
      (error
       (goto-char (point-max))))))

(defun mn/advice-newsticker-save-item (feed item)
  "Save FEED ITEM."
  (interactive)
  (let ((filename
         (read-string "Filename: "
                      (file-name-concat newsticker-dir
                                        "saved"
                                        (replace-regexp-in-string
                                         "[: ]+" "_"
                                         (concat feed "--"
                                                 (newsticker--title item)
                                                 ".html"))))))
    (with-temp-buffer
      (insert (newsticker--desc item))
      (write-file filename t))))

(with-eval-after-load 'newsticker
  (setq newsticker-wget-arguments
        (append `("--doh-url" ,mn/doh-server) newsticker-wget-arguments))

  (define-advice newsticker--image-download-by-wget
      (:around (orig-fun &rest args) with-default-dir)
    (let ((default-directory newsticker-dir))
      (apply orig-fun args)))

  (define-advice newsticker--get-news-by-funcall
      (:around (orig-fun feed-name function) build-feeds)
    "Get feeds maybe by build atom feeds.
     '((\"zzz\" ignore 1 3600 (\"-c\" \"3\") \"boss\" 10 (:dd 3)))"
    (if-let* ((item (assoc feed-name newsticker-url-list-defaults)))
        (mn/newsticker--get-news-by-build
         feed-name (nth 5 item) (nth 4 item) (nth 6 item) (nth 7 item))
      (funcall orig-fun feed-name function)))
  
  (advice-add 'newsticker--image-download-by-url :around #'mn/advice-url-retrieve-with-timeout)
  (advice-add 'newsticker--get-news-by-wget :filter-args #'mn/advice-newsticker--get-news-by-wget)
  (advice-add 'newsticker-save-item :before-until #'mn/advice-newsticker-save-item)
  (dolist (fn '(newsticker--image-sentinel newsticker--sentinel-work))
    (advice-add fn :around #'my/advice-silence-messages))
  (make-directory (file-name-concat newsticker-dir "saved") t)
  (bind-keys
   :map newsticker-treeview-mode-map
   ("DEL" . mn/newsticker-treeview-prev-page)))


;; eww

(defun mn/url-redirect (url)
  (cond
   ((string-match "^https://github.com/\\(.+\\)/commit/\\(\\w+\\)$" url)
    (format "https://github.com/%s/commit/%s.patch"
            (match-string 1 url)
            (match-string 2 url)))
   ((string-match "^https://github.com/\\(.+\\)/pull/\\([[:digit:]]+\\)$" url)
    (format "https://github.com/%s/pull/%s.patch"
            (match-string 1 url)
            (match-string 2 url)))
   ((string-match "^https://github.com/\\(.+\\)/blob/\\(.+\\)" url)
    (format "https://github.com/%s/raw/%s"
            (match-string 1 url)
            (match-string 2 url)))
   ((string-prefix-p "https://www.reddit.com" url)
    (replace-regexp-in-string "^https://www.reddit.com"
                              "https://old.reddit.com" url))
   (t url)))

(defun mn/eww-render-hook()
  "This function is intended to be added to `eww-after-render-hook'.
It applies specific rendering or major modes based on the URL or content
of the current EWW buffer.

The function checks the URL and applies the following rules:
-  For specific documentation websites (manned.org, nixos.org, mojeek.com,
   wireshark.org, nginx.org, learn.microsoft.com ...), it applies
   `eww-readable' to improve readability.
-  For '.patch' files, it switches to `diff-mode'.
-  For '.el' files, it switches to `emacs-lisp-mode'.
-  For '.rs' files, it switches to `rust-ts-mode'.
-  For '.go' files, it switches to `go-ts-mode'."
  (when-let* ((url (plist-get eww-data :url))
             (source (plist-get eww-data :source)))
    (pcase url
      ("https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/windows-commands")
      ((rx bos "http" (? ?s) "://"
           (| "manned.org/man/" "nixos.org/manual/nix/" "www.mojeek.com/search?"
              "www.wireshark.org/docs/wsug_html_chunked/"
              (: "nginx.org/en/docs/" (+ anychar) ".html")
              "learn.microsoft.com/en-us/windows-server/administration/windows-commands/"))
       (eww-readable))
      ((rx ?. "patch" eos) (diff-mode))
      ((rx ?. "el" eos) (emacs-lisp-mode))
      ((rx ?. "rs" eos) (rust-ts-mode))
      ((rx ?. "go" eos) (go-ts-mode)))))

(with-eval-after-load 'eww
  (define-advice eww--dwim-expand-url
      (:before-until (&rest args) other-search-prefix)
    "Expand URL with custom prefixes before falling back to original function.

This advice intercepts calls to `eww--dwim-expand-url' and checks
if the URL starts with certain prefixes. For example, if a match is found, it
expands the URL according to predefined rules:

- \" c \" prefix is expanded to \"https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/\"
- \" m \" prefix is expanded to \"https://manned.org/man/\"

If no custom prefix matches, it calls the original function."
    (let ((url (string-trim-right (car args))))
      (pcase url
        ((rx bos " m " (+ (in alnum "\\-_")) eos)
         (replace-regexp-in-string "\\` m " "https://manned.org/man/" url))
        ((rx bos " c " (+ (in alnum)) eos)
         (replace-regexp-in-string "\\` c " "https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/" url)))))
  (define-advice eww-retrieve (:around (orig-fun &rest args) curl-args)
    "Append curl arguments to eww-retrieve-command when retrieving."
    (if-let* ((url (car args))
              ((string-match-p
                (rx ?. (| "pdf" "tar.gz") eos) url)))
        (apply orig-fun args)
      (let ((eww-retrieve-command
             (append `(,newsticker-wget-name ,@newsticker-wget-arguments "-o-")
                     (cadr (mn/curl-parameters-dwim url)))))
        (apply orig-fun args))))
  (add-to-list 'eww-url-transformers 'mn/url-redirect)
  (add-hook 'eww-after-render-hook #'mn/eww-render-hook))


;; Aria2

(defcustom mn/aria2-conf-file (expand-file-name "aria2.conf" "~/.aria2")
  "Default aria2 configuration file path."
  :type '(string))

(defun mn/get-bt-tracker (url)
  "Get BT tracker from https://github.com/XIU2/TrackersListCollection/blob/master/README-ZH.md."
  (when (and (file-exists-p mn/aria2-conf-file)
             (not (find-buffer-visiting mn/aria2-conf-file))
             (time-less-p
              (time-add
               (file-attribute-modification-time (file-attributes mn/aria2-conf-file))
               (* 60 60 12))
              (current-time)))
    (make-process
     :name "mn/get-bt-tracker"
     :buffer "mn/get-bt-tracker"
     :command `(,newsticker-wget-name ,@newsticker-wget-arguments ,url)
     :sentinel #'mn/sentinel-get-bt-tracker)))

(defun mn/sentinel-get-bt-tracker (proc event)
  "Write tracker to aria2 conf file."
  (when (string= event "finished\n")
     (with-current-buffer (process-buffer proc)
       (when-let* ((getp (search-backward "announce" nil t))
                  (start (pos-bol))
                  (end (pos-eol)))
         (with-current-buffer (find-file-noselect mn/aria2-conf-file)
           (goto-char (point-min))
           (re-search-forward "^bt-tracker=")
           (delete-region (point) (pos-eol))
           (insert-buffer-substring (process-buffer proc) start end)
           (save-buffer)
           (kill-buffer)))
         (kill-buffer))))

(with-eval-after-load 'aria2
  (mn/get-bt-tracker "https://gitea.com/XIU2/TrackersListCollection/raw/branch/master/best_aria2.txt"))


;; alist

(defvar mn/alist-data-directory
  (pcase system-type
    ('windows-nt (substitute-in-file-name "$USERPROFILE/scoop/persist/alist/"))
    (_ (expand-file-name "~/.config/alist/")))
  "Where alist store data.")

(defun mn/start-alist ()
  "Start alist server."
  (interactive)
  (rename-file (expand-file-name "log/log.log" my/alist-data-directory)
               (file-name-concat my/alist-data-directory "log" (format-time-string "%+4Y-%m-%d-%H-%M")))
  (start-process "alist" nil "alist" "server"
                 "--data" my/alist-data-directory)
  (pcase system-type
    ('android (android-notifications-notify
               :title "alist"
               :body "Click to stop alist."
               :on-action (lambda (a b)
                            (call-process "pkill" nil nil nil "alist"))))))


;; tcpdump

(defun mn/string-to-hex (str)
  "Convert string to hex string, each byte separated with spaces"
  (mapconcat (lambda (c) (format "%02x" c))
             (string-to-vector str) ""))

(defun mn/get-chunk-size (remaining-bytes)
  "Determine appropriate chunk size (1/2/4) for remaining bytes"
  (cond
   ((>= remaining-bytes 4) 4)
   ((= remaining-bytes 3) (list 2 1))  ; split into 2+1
   ((= remaining-bytes 2) 2)
   ((= remaining-bytes 1) 1)
   (t nil)))

(defun mn/smart-split-hex (hex-str)
  "Split hex string into chunks of valid sizes (1/2/4 bytes)"
  (let ((result '())
        (str hex-str))
    (while (not (string-empty-p str))
      (let* ((remaining-bytes (/ (length str) 2))
             (chunk-size (if (listp (mn/get-chunk-size remaining-bytes))
                             (car (mn/get-chunk-size remaining-bytes))
                           (mn/get-chunk-size remaining-bytes)))
             (hex-size (* chunk-size 2)))
        (push (cons chunk-size (substring str 0 hex-size)) result)
        (setq str (substring str hex-size))))
    ;; Handle the case where we need an extra 1-byte chunk
    (when (= (/ (length hex-str) 2) 3)
      (let ((last-chunk (substring hex-str 4 6)))
        (push (cons 1 last-chunk) result)))
    (nreverse result)))

(cl-defun mn/string-to-tcpdump-filter (string &optional (offset 0))
  "It takes the string you enter, splits it into 1, 2, or 4 byte chunks,
converts them to numbers, and creates a capture filter that matches
those numbers at the offset you provide.

ref: https://www.wireshark.org/tools/string-cf.html
But the website have a bug, maybe it add offset by string instead of
number."
  (let* ((hex-str (mn/string-to-hex string))
         (chunks (mn/smart-split-hex hex-str))
         (header-calc "((tcp[12:1] & 0xf0) >> 2)"))
    (mapconcat
     (lambda (chunk)
       (let* ((chunk-size (car chunk))
              (chunk-hex (cdr chunk))
              (filter
               (format "tcp[(%s) + %d:%d] = 0x%s"
                       header-calc
                       offset
                       chunk-size
                       chunk-hex)))
         (setq offset (+ offset chunk-size))
         filter))
     chunks " && ")))

(provide 'init-net)
;;; init-net.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mn/" . "my/net-"))
;; End:
