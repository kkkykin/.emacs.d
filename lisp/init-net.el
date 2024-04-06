;;; init-net.el --- Net Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
(defcustom mn/doh-server-list '("https://9.9.9.9/dns-query"
                                "https://doh.bortzmeyer.fr"
                                "https://dns.digitalsize.net/dns-query"
                                "https://1.1.1.1/dns-query"
                                "https://1.12.12.12/dns-query"
                                "https://223.6.6.6/dns-query")
  "Public DOH Server."
  :group 'my
  :type '(repeat string))

(defcustom mn/doh-server (car mn/doh-server-list)
  "Default DOH Server."
  :group 'my
  :type 'string)

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

(defcustom mn/block-domain (rx (| ?. bos) (| "blogs.reimu.net") eos)
  "Domain blocked."
  :group 'my
  :type 'regexp)

(defcustom mn/proxy-domain
  (rx (| ?. bos)
      (| "duckduckgo.com" "google.com" "wikipedia.org")
      eos)
  "Domain through proxy."
  :group 'my
  :type 'regexp)

(defcustom mn/img-proxy-domain
  (rx
   (| ?. bos)
   (| "saxyit.com" "reimu.net" "img.piclabo.xyz" "trts.baishancdnx.cn"
      "www.skidrowreloaded.com" "riotpixels.net" "www.hacg.mov"
      "mooc-image.nosdn.127.net")
   eos)
  "Img domain through proxy."
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
           (string-match-p mn/block-domain host))
      (setq url "127.0.0.1"))
     ((string-match-p mn/proxy-domain host)
      (setq parameters (cons (format "-xhttp://%s" mn/centaur-proxy)
                             parameters))))
    (list url parameters)))

(defun mn/advice-url-retrieve (orig-fun &rest args)
  "Block, proxy, transform url."
  (let* ((url (car args))
         (host (url-host (url-generic-parse-url url))))
    (cond ((string-match-p mn/block-domain host) nil)
          ((string-match-p mn/proxy-domain host)
           (let ((url-proxy-services
                  `(("http" . ,mn/centaur-proxy)
                    ("https" . ,mn/centaur-proxy))))
             (apply orig-fun args)))
          ((string-match-p mn/img-proxy-domain host)
           (setcar args (string-replace "${href_ue}"
                                        (url-hexify-string url)
                                        mn/img-cdn-server))
           (apply orig-fun args))
          (t (apply orig-fun args)))))
(advice-add #'url-retrieve-internal :around #'my/advice-url-retrieve)

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

(defcustom mn/rss-bridge-list '("https://rss-bridge.org/bridge01/"
                                ;; "https://rss-bridge.lewd.tech/"
                                "https://rss.nixnet.services/"
                                "https://wtf.roflcopter.fr/rss-bridge/"
                                "https://rssbridge.flossboxin.org.in/")
  "Public RSS-Bridge Server."
  :group 'my
  :type '(repeat string))

(defcustom mn/rss-hub-list '("https://rsshub.zzzr.eu.org/"
                             "https://rsshub.rssforever.com/"
                             "https://rsshub.feeded.xyz/"
                             "https://hub.slarker.me/"
                             "https://rsshub.liumingye.cn/"
                             "https://rsshub-instance.zeabur.app/"
                             "https://rss.fatpandac.com/"
                             "https://rsshub.pseudoyu.com/"
                             "https://rsshub.friesport.ac.cn/"
                             "https://rsshub.atgw.io/")
  "Public RSSHub Server."
  :group 'my
  :type '(repeat string))

(defcustom mn/rss-bridge-server (car mn/rss-bridge-list)
  "RSS bridge default server."
  :group 'my
  :type 'string)

(defcustom mn/rss-hub-server (car mn/rss-hub-list)
  "RSS hub default server."
  :group 'my
  :type 'string)

(defun mn/rss-bridge-generator (bridge)
  "Generate atom feed via rss-bridge."
  (concat mn/rss-bridge-server
          "?action=display&format=Atom&bridge=" bridge))

(defun mn/rss-bridge-wp (blog limit &optional content)
  "Returns the newest full posts of a WordPress powered website."
  (concat (mn/rss-bridge-generator "WordPressBridge")
          "&url=" (url-hexify-string blog)
          "&limit=" (number-to-string limit)
          (when content (concat "&content_selector=" (url-hexify-string content)))))
;; todo
;; (defun mn/rss-bridge-filter ())

(defun mn/rss-bridge-reducer (feed percentage)
  "Choose a percentage of a feed you want to see."
  (concat (mn/rss-bridge-generator "FeedReducerBridge")
          "&url=" (url-hexify-string feed)
          "&percentage=" (number-to-string percentage)))

(defun mn/rss-bridge-css-expander (feed limit content &optional
                                        content-cleanup
                                        dont-expand-metadata
                                        discard-thumbnail)
  "Expand any site RSS feed using CSS selectors."
  (concat (mn/rss-bridge-generator "CssSelectorFeedExpanderBridge")
          "&feed=" (url-hexify-string feed)
          "&limit=" (number-to-string limit)
          "&content_selector=" (url-hexify-string content)
          (when content-cleanup (concat "&content_cleanup=" (url-hexify-string content-cleanup)))
          (concat "&dont_expand_metadata=" (when dont-expand-metadata "on"))
          (concat "&discard_thumbnail=" (when discard-thumbnail "on"))))

(defun mn/rss-bridge-css-selector (home limit entry load-pages &rest args)
  "Convert any site to RSS feed using CSS selectors. The bridge first
selects the element describing the article entries. It then extracts
the links to the articles from these elements. It then, depending on
the setting 'load_pages', either parses the selected elements,
or downloads the page for each article and parses those. Parsing the
elements or page is done using the provided selectors."
  (let ((content (plist-get args :content))
        (title (plist-get args :title))
        (time (plist-get args :time))
        (time-fmt (plist-get args :time-fmt))
        (url (plist-get args :url))
        (url-pattern (plist-get args :url-pattern))
        (title-cleanup (plist-get args :title-cleanup))
        (content-cleanup (plist-get args :content-cleanup))
        (cookie (plist-get args :cookie))
        (author (plist-get args :author))
        (cat (plist-get args :cat))
        (rm-style (plist-get args :rm-style)))
    (concat (mn/rss-bridge-generator "CssSelectorComplexBridge")
            "&home_page=" (url-hexify-string home)
            "&limit=" (number-to-string limit)
            "&entry_element_selector=" (url-hexify-string entry)
            "&use_article_pages=" (when load-pages "on")
            (when content (concat "&article_page_content_selector=" (url-hexify-string content)))
            (when title (concat "&title_selector=" (url-hexify-string title)))
            (when time (concat "&time_selector=" (url-hexify-string time)))
            (when time-fmt (concat "&time_format=" (url-hexify-string time-fmt)))
            (when url (concat "&url_selector=" (url-hexify-string url)))
            (when url-pattern (concat "&url_pattern=" (url-hexify-string url-pattern)))
            (when title-cleanup (concat "&title_cleanup=" (url-hexify-string title-cleanup)))
            (when content-cleanup (concat "&content_cleanup=" (url-hexify-string content-cleanup)))
            (when cookie (concat "&cookie=" (url-hexify-string cookie)))
            (when author (concat "&author_selector=" (url-hexify-string author)))
            (when cat (concat "&category_selector=" (url-hexify-string cat)))
            (concat "&remove_styling=" (when rm-style "on")))))

(defun mn/rss-bridge-merger (feeds limit name)
  "This bridge merges two or more feeds into a single feed. Max 10
items are fetched from each feed."
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

(defun mn/rss-hub-generator (router &rest args)
  "Generate feed via RSSHub."
  (let* ((fmt (plist-get args :fmt))
         (main (concat mn/rss-hub-server router
                       (when fmt (concat "." fmt))))
         (url (url-generic-parse-url main))
         (limit (plist-get args :limit))
         (full (plist-get args :full))
         (brief (plist-get args :brief))
         (unsort (plist-get args :unsort))
         (opencc (plist-get args :opencc))
         (scihub (plist-get args :scihub))
         (f-uncase (plist-get args :f-uncase))
         (f (plist-get args :f))
         (f-title (plist-get args :f-title))
         (f-desc (plist-get args :f-desc))
         (f-author (plist-get args :f-author))
         (f-cat (plist-get args :f-cat))
         (f-time (plist-get args :f-time))
         (fo (plist-get args :fo))
         (fo-title (plist-get args :fo-title))
         (fo-desc (plist-get args :fo-desc))
         (fo-author (plist-get args :fo-author))
         (fo-cat (plist-get args :fo-cat))
         (img-tp (plist-get args :img-tp))
         (domain (plist-get args :domain))
         (code (auth-source-pick-first-password :host (url-host url))))
    (concat main "?"
            (string-join
             (delq
              nil
              `(,(when limit (concat "limit=" (number-to-string limit)))
                ,(when full "mode=fulltext")
                ,(when brief (concat "brief=" (number-to-string brief)))
                ,(when unsort "sorted=false")
                ,(when opencc (concat "opencc=" opencc))
                ,(when scihub "scihub=1")
                ,(when f-uncase "filter_case_sensitive=false")
                ,(when f (concat "filter=" (url-hexify-string f)))
                ,(when f-title (concat "filter_title=" (url-hexify-string f-title)))
                ,(when f-desc (concat "filter_description=" (url-hexify-string f-desc)))
                ,(when f-author (concat "filter_author=" (url-hexify-string f-author)))
                ,(when f-cat (concat "filter_category=" (url-hexify-string f-cat)))
                ,(when f-time (concat "filter_time=" (url-hexify-string f-time)))
                ,(when fo (concat "filterout=" (url-hexify-string fo)))
                ,(when fo-title (concat "filterout_title=" (url-hexify-string fo-title)))
                ,(when fo-desc (concat "filterout_description=" (url-hexify-string fo-desc)))
                ,(when fo-author (concat "filterout_author=" (url-hexify-string fo-author)))
                ,(when fo-cat (concat "filterout_category=" (url-hexify-string fo-cat)))
                ,(when img-tp (concat "image_hotlink_template=" (url-hexify-string mn/img-cdn-server)))
                ,(when domain (concat "domain=" (url-hexify-string domain)))
                ,(when code (concat "code=" (md5 (concat (url-filename url) code))))))
             "&"))))

(defun mn/rss-hub-transform (url s-fmt &rest args)
  "Pass URL and transformation rules to convert HTML/JSON into RSS."
  (let ((title (plist-get args :t))
        (item (plist-get args :i))
        (item-title (plist-get args :it))
        (item-title-a (plist-get args :ita))
        (item-link (plist-get args :il))
        (item-link-a (plist-get args :ila))
        (item-desc (plist-get args :id))
        (item-desc-a (plist-get args :ida))
        (item-pub (plist-get args :ip))
        (item-pub-a (plist-get args :ipa))
        (extra (plist-get args :extra)))
    (apply
     #'mn/rss-hub-generator
     (concat "rsshub/transform/" s-fmt "/" (url-hexify-string url) "/"
             (string-join
              (delq
               nil
               `(,(when title (concat "title=" (url-hexify-string title)))
                 ,(when item (concat "item=" (url-hexify-string item)))
                 ,(when item-title (concat "itemTitle=" (url-hexify-string item-title)))
                 ,(when item-title-a (concat "itemTitleAttr=" (url-hexify-string item-title-a)))
                 ,(when item-link (concat "itemLink=" (url-hexify-string item-link)))
                 ,(when item-link-a (concat "itemLinkAttr=" (url-hexify-string item-link-a)))
                 ,(when item-desc (concat "itemDesc=" (url-hexify-string item-desc)))
                 ,(when item-desc-a (concat "itemDescAttr=" (url-hexify-string item-desc-a)))
                 ,(when item-pub (concat "itemPubDate=" (url-hexify-string item-pub)))
                 ,(when item-pub-a (concat "itemPubDateAttr=" (url-hexify-string item-pub-a)))))
              "&"))
     extra)
    ))

(defun mn/advice-newsticker-list-set-start-time (&rest args)
  "Newsticker retrieve feeds with interval start time."
  (let ((counter 0))
    (mapc (lambda (x)
            (setcar (cddr x) counter)
            (setq counter (+ counter 10)))
          newsticker-url-list)))

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
  (advice-add 'newsticker-start :before #'mn/advice-newsticker-list-set-start-time)
  (advice-add 'newsticker--image-download-by-url :around #'mn/advice-url-retrieve-with-timeout)
  (advice-add 'newsticker--get-news-by-wget :filter-args #'mn/advice-newsticker--get-news-by-wget)
  (advice-add 'newsticker-save-item :before-until #'mn/advice-newsticker-save-item)
  (with-eval-after-load 'eww
    (setq eww-retrieve-command (cons newsticker-wget-name newsticker-wget-arguments))))

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

(defun mn/advice-eww--dwim-expand-url (orig-fun &rest args)
  "Maybe use other search-prefix instead of eww-search-prefix."
  (let ((url (string-trim (car args))))
    (cond ((string-match-p "\\`man [[:alpha:][:digit:]\\-_]+\\'" url)
           (string-replace "man " "https://manned.org/man/" url))
          (t (apply orig-fun args)))))

(defun mn/eww-render-hook()
  (when-let ((url (plist-get eww-data :url))
             (source (plist-get eww-data :source)))
    (cond
     ((string-match-p
       (concat
        "^https?://"
        (rx
         (| "manned.org/man/" "nixos.org/manual/nix/" "www.mojeek.com/search?"
            "www.wireshark.org/docs/wsug_html_chunked/"
            (: "nginx.org/en/docs/" (+ anychar) ".html"))))
       url)
      (eww-readable))
     ((string-suffix-p ".patch" url) (diff-mode))
     ((string-suffix-p ".el" url) (emacs-lisp-mode))
     ((string-suffix-p ".rs" url) (rust-ts-mode))
     ((string-suffix-p ".go" url) (go-ts-mode)))))

(defun mn/advice-eww-retrieve (orig-fun &rest args)
  "Append curl arguments to eww-retrieve-command when retrieving."
  (let ((eww-retrieve-command
         (append eww-retrieve-command
                 (cadr (mn/curl-parameters-dwim (car args))))))
    (apply orig-fun args)))

(with-eval-after-load 'eww
  (advice-add 'eww--dwim-expand-url :around 'mn/advice-eww--dwim-expand-url)
  (advice-add 'eww-retrieve :around 'mn/advice-eww-retrieve)
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
       (when-let ((getp (search-backward "announce" nil t))
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

(provide 'init-net)
;;; init-net.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mn/" . "my/net-"))
;; End:
