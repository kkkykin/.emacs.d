;;; init-net.el --- Net Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'nsm)

;; ipv6
(defun zn/has-public-ipv6-addr-p ()
  "Return t if at least one interface has a routable (public) IPv6 address."
  (let* (addrs
         (current-ip (network-interface-list nil 'ipv6))
         (effective-ip
          (or (and (fboundp 'zr-android-wifi-connection-info)
                   (zr-android-wifi-connection-info 'ipv6)
                   (cl-remove-if (apply-partially #'string-prefix-p "rmnet_data")
                                 current-ip :key #'car))
              current-ip))
         (reserved
          '(;; ::1/128
            ([0 0 0 0 0 0 0 1] [65535 65535 65535 65535 65535 65535 65535 65535])
            ;; fe80::/10
            ([65152 0 0 0 0 0 0 0] [64512 0 0 0 0 0 0 0])
            ;; fc00::/7
            ([64512 0 0 0 0 0 0 0] [65408 0 0 0 0 0 0 0])
            ;; ff00::/8 (multicast)
            ([65280 0 0 0 0 0 0 0] [65280 0 0 0 0 0 0 0])
            ;; ::/128 (unspecified)
            ([0 0 0 0 0 0 0 0] [65535 65535 65535 65535 65535 65535 65535 65535]))))
    ;; 收集所有IPv6地址（兼容不同格式）
    (dolist (iface-addr effective-ip)
      (let ((v (cdr iface-addr)))
        (cond
         ((= (length v) 9)              ; 标准格式：8个16位 + 前缀
          (push (seq-subseq v 0 8) addrs))
         ((= (length v) 8)              ; 纯地址格式
          (push v addrs)))))
    ;; 无地址则直接返回nil
    (if (null addrs)
        nil
      ;; 检查是否有地址不属于任何保留网络
      (cl-loop for addr in addrs
               unless (cl-loop for (net mask) in reserved
                               thereis (nsm-network-same-subnet net mask addr))
               return t))))

;; url

(defcustom zn/img-cdn-server-list
  '("https://images.weserv.nl?url=${href_ue}"
    "https://imageproxy.pimg.tw/resize?url=${href_ue}")
  "Public Image CDN server."
  :group 'my
  :type '(repeat string))

(defcustom zn/img-cdn-server (car zn/img-cdn-server-list)
  "Default Image CDN server."
  :group 'my
  :type 'string)

;; https://hub.zzzr.eu.org/curl/curl/wiki/DNS-over-HTTPS E
(defcustom zn/doh-server-list '("9.9.9.9/dns-query"
                                "doh.bortzmeyer.fr"
                                "dns.digitalsize.net/dns-query"
                                "1.1.1.1/dns-query"
                                "1.12.12.12/dns-query"
                                "223.6.6.6/dns-query")
  "Public DOH Server."
  :group 'my
  :type '(repeat string))

(defcustom zn/doh-server (concat "https://" (car zn/doh-server-list))
  "Default DOH Server."
  :group 'my
  :type 'string)

(defun zn/default-callback (&rest args)
  "Test default callback."
  (message (if args "Up" "Down")))

(defun zn/internet-up-p (&optional host callback)
  "Test connectivity via ping."
  (interactive)
  (let* ((args (if zn/sys-winnt-p '("-n" "1" "-w" "1") '("-c1" "-W1")))
         (proc (apply #'start-process "internet-test" nil "ping"
                      (if host host "baidu.com") args))
         (callback (if callback callback 'zn/default-callback)))
    (set-process-sentinel proc (lambda (proc signal)
                                 (apply callback
                                        (if (= 0 (process-exit-status proc))
                                            '(t) nil))))))

(defun zn/url-up-p (url &rest cbargs)
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
         (callback (if callback callback 'zn/default-callback)))
    (set-process-filter proc (lambda (proc line)
                               (apply callback
                                      (if (string= "200" line)
                                          '(t) nil))))))

(defun zn/doh-up-p (&optional doh-url callback)
  "Test DOH availability."
  (interactive)
  (let ((args `(:doh-url ,(if doh-url doh-url zn/doh-server)
                         :callback ,(when callback callback))))
    (apply #'zn/url-up-p "https://www.baidu.com" :max-time 3 args)))

(defun zn/advice-url-retrieve-with-timeout (orig-fun &rest args)
  "Use `url-queue-retrieve' instead of `url-retrieve'."
  (cl-flet ((url-retrieve #'url-queue-retrieve)
            (url-queue-timeout 30))
    (apply orig-fun args)))

(define-advice url-retrieve-internal (:around (orig-fun &rest args) custom)
  "Custom retrieve by url."
  (pcase (car args)
    ((rx bos "https://attach.52pojie.cn/")
     (let ((url-request-extra-headers
            (append
             '(("Referer" . "https://www.52pojie.cn/"))
             url-request-extra-headers)))
       (apply orig-fun args)))
    (_ (apply orig-fun args))))

(defun zn/url-http-parse-response ()
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

(define-multisession-variable zn/proxy-rules-hash
  (make-hash-table :test 'equal)
  "Hash table for storing domain proxy rules."
  :package "init-net"
  :key "proxy")

(define-multisession-variable zn/proxy-rules-patterns nil
  "List of regexp proxy rules."
  :package "init-net"
  :key "proxy")

(defun zn/generate-ipv4-mask (prefix-len)
  "Generate IPv4 netmask vector for given prefix length."
  (let ((mask (make-vector 4 0)))
    (dotimes (i 4)
      (let ((bits (- prefix-len (* i 8))))
        (cond 
         ((>= bits 8) (aset mask i 255))
         ((> bits 0) (aset mask i (logand (lsh 255 (- 8 bits)) 255)))
         (t (aset mask i 0)))))
    mask))

(defun zn/generate-ipv6-mask (prefix-len)
  "Generate IPv6 netmask vector for given prefix length."
  (let* ((mask (make-vector 8 0))
         (full-groups (/ prefix-len 16))
         (remainder-bits (% prefix-len 16)))
    ;; Fill full groups
    (dotimes (i full-groups)
      (aset mask i 65535))  ; 0xFFFF
    
    ;; Handle partial group if any
    (when (> remainder-bits 0)
      (aset mask full-groups 
            (logand 65535
                   (ash #xFFFF (- remainder-bits 16)))))
    mask))

(defun zn/cidr-to-ip-mask (cidr)
  "Convert CIDR notation (IPv4 or IPv6) to a list of IP and netmask vectors."
  (if (string-match "\\(.+\\)/\\([0-9]+\\)" cidr)
      (let* ((ip (match-string 1 cidr))
             (prefix-len (string-to-number (match-string 2 cidr)))
             (is-ipv6 (string-match ":" ip))
             (ip-vec (substring
                      (car (network-lookup-address-info
                            ip (if is-ipv6 'ipv6 'ipv4) 'numeric))
                      0 -1))
             (mask (if is-ipv6 (zn/generate-ipv6-mask prefix-len)
                     (zn/generate-ipv4-mask prefix-len))))
        (cons ip-vec mask))
    (error "Invalid CIDR notation")))

(defun zn/init-proxy-rules-1 (rules)
  "Initialize proxy rules from the given rules list."
  (let ((proxys (gethash "proxy" rules))
        (autoproxy_hosts (gethash "autoproxy_hosts" rules))
        (hash (make-hash-table :test 'equal))
        pattern)
    (dotimes (i (length proxys))
      (let ((proxy (elt proxys i))
            (hosts (elt autoproxy_hosts i))
            wildcards)
        (mapc (lambda (host)
                (if (seq-position host ?*)
                    (push host wildcards)
                  (puthash host proxy hash)))
              hosts)
        (when wildcards
          (push (cons (zr-wildcards-to-regexp wildcards) proxy)
                pattern))))
    (setf (multisession-value zn/proxy-rules-hash) hash
          (multisession-value zn/proxy-rules-patterns) pattern)))

(defun zn/match-proxy-rule (urlobj host)
  "Match URL against proxy rules. Returns proxy string or \"DIRECT\"."
  (let* ((hash (multisession-value zn/proxy-rules-hash))
         (pattern (multisession-value zn/proxy-rules-patterns))
         (parts (split-string host "\\."))
         (len (length parts)))
    (or (catch 'found
          (while (> len 0)
            (if-let* ((fd (gethash (string-join (last parts len) ".") hash)))
                (throw 'found fd)
              (setq len (1- len))))
          (dolist (r pattern)
            (when (string-match-p (car r) host)
              (throw 'found (cdr r)))))
        "DIRECT")))

(defvar zn/url-history '())
(defun zn/url-proxy-locator (urlobj host)
  "Determine proxy settings for URL based on host and proxy services."
  (when debug-on-error
    (push (cons (float-time) (url-recreate-url urlobj)) zn/url-history))
  (replace-regexp-in-string "^SOCKS5 " "PROXY " (zn/match-proxy-rule urlobj host)))
(setq url-proxy-locator #'zn/url-proxy-locator)

(defun zn/curl-parameters-dwim (url &rest args)
  "Generate explicit parameters for curl."
  (let* ((urlobj (url-generic-parse-url url))
         (proxy (zn/match-proxy-rule urlobj (url-host urlobj)))
         parameters)
    (if (string= "DIRECT" proxy)
        (setq parameters (list "-x" ""))
      (let* ((u (string-split proxy " "))
             (prefix (if (string= "PROXY" (car u)) "http://" "socks5h://")))
        (setq parameters (list (concat "-x" prefix (cadr u))))))
    parameters))

(with-eval-after-load 'plz
  (define-advice plz (:around (fn method url &rest args) append-arg)
    (let ((plz-curl-default-args
           (append (zn/curl-parameters-dwim url)
                   plz-curl-default-args)))
      (apply fn method url args))))

(defcustom zn/pac-data-file
  (expand-file-name "surfingkeys/pac.json.gpg" zr-dotfiles-dir)
  "PAC data file path."
  :type 'file)
(add-to-list 'zr-dotfiles-dir-followd-by-vars 'zn/pac-data-file)

(defun zn/read-proxy-rules ()
  (with-temp-buffer
    (insert-file-contents-literally zn/pac-data-file)
    (when (string-match-p epa-file-name-regexp zn/pac-data-file)
      (let ((epa-replace-original-text t))
        (epa-decrypt-region (point-min) (point-max))))
    (goto-char 1)
    (json-parse-buffer)))

(defun zn/init-proxy-rules ()
  (interactive)
  (zn/init-proxy-rules-1 (zn/read-proxy-rules)))

(defun zn/add-domain-to-proxy (hostname)
  "Add a domain to the proxy rules.

This function takes a HOSTNAME as input and adds it to the first proxy
rules in the proxy data file."
  (interactive "shostname: ")
  (with-current-buffer (find-file-noselect zn/pac-data-file)
    (goto-char 1)
    (let* ((cfg (json-parse-buffer))
           (autoproxy_hosts (gethash "autoproxy_hosts" cfg)))
      (setf (aref autoproxy_hosts 0)
            (vconcat (vector hostname) (aref autoproxy_hosts 0)))
      (puthash "autoproxy_hosts"
               (cl-map #'vector (lambda (h)
                                  (cl-sort
                                   (cl-remove-duplicates h :test #'string=)
                                   #'string<))
                       autoproxy_hosts)
               cfg)
      (erase-buffer)
      (insert (json-serialize cfg))
      (save-buffer)
      (zn/init-proxy-rules-1 cfg)
      (message "%s added to proxy." hostname))
    (zn/generate-pac-file)))

(defun zn/generate-pac-file ()
  "Generate a PAC file by tangle surfingkeys config.

ref:
chrome://net-internals#proxy
https://support.microsoft.com/en-us/topic/how-to-disable-automatic-proxy-caching-in-internet-explorer-92735c9c-8a26-d0d8-7f8a-1b46595cbaba"
  (let (org-confirm-babel-evaluate)
    (org-babel-tangle-file
     (expand-file-name
      "surfingkeys/20241214T081602--surfingkeys__browser.org"
      zr-dotfiles-dir)
     nil "^javascript$")))

(defun zn/proxy-up-p (&optional proxy callback)
  "Test Proxy availability."
  (interactive)
  (let ((args `(:proxy ,(if proxy proxy
                          (concat "http://" zn/centaur-proxy))
                       :callback ,(when callback callback))))
    (apply #'zn/url-up-p "https://www.baidu.com" :max-time 2 args)))

(defun zn/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" zn/centaur-proxy)
    (message "No HTTP proxy")))

(defun zn/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,zn/centaur-proxy)
          ("https" . ,zn/centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (zn/proxy-http-show))

(defun zn/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (zn/proxy-http-show))

(defun zn/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (zn/proxy-http-disable)
    (zn/proxy-http-enable)))

(defun zn/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun zn/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string zn/centaur-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" zn/centaur-socks-proxy))
  (zn/proxy-socks-show))

(defun zn/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (zn/proxy-socks-show))

(defun zn/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (zn/proxy-socks-enable)))


;; Newsticker

(defcustom zn/rss-bridge-list '("rss-bridge.kkky.fun:2053"
                                "rss-bridge.org/bridge01"
                                "rss.nixnet.services"
                                "wtf.roflcopter.fr/rss-bridge"
                                "rssbridge.flossboxin.org.in")
  "Public RSS-Bridge Server."
  :group 'my
  :type '(repeat string))

(defcustom zn/rss-hub-list '("rsshub.email-once.com/"
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

(defcustom zn/rss-bridge-server (concat "https://" (car zn/rss-bridge-list))
  "RSS bridge default server."
  :group 'my
  :type 'string)

(defcustom zn/rss-hub-server (concat "https://" (car zn/rss-hub-list))
  "RSS hub default server."
  :group 'my
  :type 'string)

(defun zn/rss-bridge-generator (bridge &optional proxy cache-timeout)
  "Generate atom feed via rss-bridge."
  (if-let* ((obj (url-generic-parse-url zn/rss-bridge-server))
            (request (format "%s/?action=display&format=Atom&bridge=%s&"
                             zn/rss-bridge-server bridge))
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

(defun zn/rss-bridge-wp (blog limit &optional content)
  "Returns the newest full posts of a WordPress powered website."
  (concat (zn/rss-bridge-generator "WordPressBridge")
          (url-build-query-string
           (cl-delete-if
            #'null
            `(("url" ,blog)
              ("limit" ,limit)
              ("content_selector" ,content))
            :key #'cadr))))
;; todo
;; (defun zn/rss-bridge-filter ())

(defun zn/rss-bridge-reducer (feed percentage)
  "Choose a percentage of a feed you want to see."
  (concat (zn/rss-bridge-generator "FeedReducerBridge")
          (url-build-query-string
           (cl-delete-if
            #'null
            `(("url" ,feed)
              ("percentage" ,percentage))
            :key #'cadr))))

(defun zn/rss-bridge-css-expander (feed limit content &optional
                                        content-cleanup
                                        dont-expand-metadata
                                        discard-thumbnail)
  "Expand any site RSS feed using CSS selectors."
  (concat "https://rss-bridge.org/bridge01/?action=display&format=Atom&bridge=CssSelectorFeedExpanderBridge&"
          ;; (zn/rss-bridge-generator "CssSelectorFeedExpanderBridge")
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

(cl-defun zn/rss-bridge-css-selector
    ( home limit entry load-pages &key content title time time-fmt url
      url-pattern title-cleanup content-cleanup cookie author cat rm-style)
  "Convert any site to RSS feed using CSS selectors. The bridge first
selects the element describing the article entries. It then extracts
the links to the articles from these elements. It then, depending on
the setting 'load_pages', either parses the selected elements,
or downloads the page for each article and parses those. Parsing the
elements or page is done using the provided selectors."
  (concat (zn/rss-bridge-generator "CssSelectorComplexBridge")
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

(defun zn/rss-bridge-merger (feeds limit name)
  "This bridge merges two or more feeds into a single feed. Max 10
items are fetched from each feed."
  (when (> (length feeds) 10)
    (user-error "Feed: %s is reach Max feeds limit."
                (propertize name 'face '(:inherit 'font-lock-warning-face))))
  (concat (zn/rss-bridge-generator "FeedMergeBridge")
          (let ((m 0))
            (mapconcat
             (lambda (feed)
               (setq m (+ m 1))
               (concat "&feed_" (number-to-string m) "="
                       (url-hexify-string feed)))
             feeds))
          "&limit=" (number-to-string limit)
          "&feed_name=" (url-hexify-string name)))

(cl-defun zn/rss-hub-generator
    ( router &key fmt limit full brief unsort opencc scihub f-uncase f
      f-title f-desc f-author f-cat f-time fo fo-title fo-desc fo-author
      fo-cat img-tp domain)
  "Generate feed via RSSHub."
  (let* ((main (concat zn/rss-hub-server router
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
                ;; ("image_hotlink_template" ,zn/img-cdn-server)
                ("domain" ,domain)
                ("code" ,(md5 (concat (url-filename url) code))))
              :key #'cadr)))))

(cl-defun zn/rss-hub-transform
    ( url s-fmt &key title item item-title item-title-a item-link
      item-link-a item-desc iten-desc-a item-pub item-pub-a extra)
  "Pass URL and transformation rules to convert HTML/JSON into RSS."
  (apply
   #'zn/rss-hub-generator
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

(defun zn/atom-builder (title link entrys &optional id updated author)
  "Create brief atom feeds."
  (if-let* ((dir (expand-file-name "builder" newsticker-dir))
            (mkdir (or (make-directory dir t) t))
            (log-path (expand-file-name (concat title (if entrys ".xml" ".err")) dir))
            (err entrys))
      (let ((updated (or updated (format-time-string "%FT%T%z")))
            (author (or author (url-host (url-generic-parse-url link))))
            (id (or id (concat "urn:uuid:" (zr-generate-uuid (concat title link updated)))))
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
                                      (zr-generate-uuid
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

(defun zn/atom-boss-builder (title url buf)
  "Generate atom feeds for Boss ZhiPin."
  (with-current-buffer buf
    (zn/atom-builder
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

(defun zn/newsticker--sentinel (process event)
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
      (apply (intern (format "zr-net-atom-%s-builder" feed-channel))
             (list feed-name feed-url buffer))
      (newsticker--sentinel-work event t feed-name command buffer))))

(defun zn/newsticker--url-stuff-it (channel &optional title args)
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

(defun zn/newsticker--get-news-by-build
    (feed-name channel &optional curl-arguments limit extras)
  "Newsticker build atom feeds."
  (let ((buffername (concat " *newsticker-curl-" feed-name "*"))
        (url (zn/newsticker--url-stuff-it channel feed-name extras)))
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
        (set-process-sentinel proc #'zn/newsticker--sentinel)
        (process-put proc 'nt-feed-name feed-name)
        (process-put proc 'nt-feed-channel channel)
        (process-put proc 'nt-feed-limit limit)
        (setq newsticker--process-ids (cons (process-id proc)
                                            newsticker--process-ids))
        (force-mode-line-update)))))

(defun zn/advice-newsticker--get-news-by-wget (args)
  (setcar (cddr args)
          (append (caddr args)
                  (zn/curl-parameters-dwim (cadr args))))
  args)

(defun zn/newsticker-treeview-prev-page ()
  "Scroll item buffer."
  (interactive)
  (save-selected-window
    (select-window (newsticker--treeview-item-window) t)
    (condition-case nil
        (scroll-down nil)
      (error
       (goto-char (point-max))))))

(defun zn/advice-newsticker-save-item (feed item)
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
        (append `("--doh-url" ,zn/doh-server) newsticker-wget-arguments))

  (define-advice newsticker--get-news-by-funcall
      (:around (orig-fun feed-name function) build-feeds)
    "Get feeds maybe by build atom feeds.
     '((\"zzz\" ignore 1 3600 (\"-c\" \"3\") \"boss\" 10 (:dd 3)))"
    (if-let* ((item (assoc feed-name newsticker-url-list-defaults)))
        (zn/newsticker--get-news-by-build
         feed-name (nth 5 item) (nth 4 item) (nth 6 item) (nth 7 item))
      (funcall orig-fun feed-name function)))
  
  (advice-add 'newsticker--get-news-by-wget :filter-args #'zn/advice-newsticker--get-news-by-wget)
  (advice-add 'newsticker-save-item :before-until #'zn/advice-newsticker-save-item)
  (dolist (fn '(newsticker--image-sentinel newsticker--sentinel-work))
    (advice-add fn :around #'zr-advice-silence-messages))
  (make-directory (file-name-concat newsticker-dir "saved") t)
  (bind-keys
   :map newsticker-treeview-mode-map
   ("DEL" . zn/newsticker-treeview-prev-page)))


;; eww

(defvar zn/url-auth-urls nil
  "List of URL regexp that require authentication.
When `zn/url-retrieve-with-auth' is called with a URL that starts
with any of these prefixes, it will attempt to look up authentication
credentials.")

(defun zn/url-get-auths (url)
  "Retrieve authentication credentials for URL from auth-source.
Parses URL and searches auth-source (e.g., .authinfo.gpg) for matching
credentials for the host and port."
  (let ((obj (url-generic-parse-url url)))
    (auth-source-search :max 1
                        :host (url-host obj)
                        :port (url-port obj))))

(defun zn/url-retrieve-with-auth (url)
  "Retrieve URL with authentication if needed.
If URL starts with any prefix in `zn/url-auth-urls', looks up and
applies authentication credentials from auth-source. Otherwise,
returns the URL unchanged.

Returns either the original URL or a new URL string with embedded
authentication credentials."
  (if (cl-find-if (lambda (u) (string-match-p u url))
                  zn/url-auth-urls)
      (let ((auth (car (zn/url-get-auths url)))
            (obj (url-generic-parse-url url)))
        (setf (url-user obj) (plist-get auth :user))
        (setf (url-password obj) (auth-info-password auth))
        (url-recreate-url obj))
    url))

(defun zn/url-redirect (url)
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

(defun zn/eww-render-hook()
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
  (add-to-list 'eww-url-transformers 'zn/url-redirect)
  (add-to-list 'eww-url-transformers 'zn/url-retrieve-with-auth)
  (add-hook 'eww-after-render-hook #'zn/eww-render-hook))


;; Aria2

(defcustom zn/aria2-conf-file (expand-file-name "aria2.conf" "~/.aria2")
  "Default aria2 configuration file path."
  :type '(string))

(defun zn/get-bt-tracker (url)
  "Get BT tracker from https://github.com/XIU2/TrackersListCollection/blob/master/README-ZH.md."
  (when (and (file-exists-p zn/aria2-conf-file)
             (not (find-buffer-visiting zn/aria2-conf-file))
             (time-less-p
              (time-add
               (file-attribute-modification-time (file-attributes zn/aria2-conf-file))
               (* 60 60 12))
              (current-time)))
    (make-process
     :name "zn/get-bt-tracker"
     :buffer "zn/get-bt-tracker"
     :command `(,newsticker-wget-name ,@newsticker-wget-arguments ,url)
     :sentinel #'zn/sentinel-get-bt-tracker)))

(defun zn/sentinel-get-bt-tracker (proc event)
  "Write tracker to aria2 conf file."
  (when (string= event "finished\n")
     (with-current-buffer (process-buffer proc)
       (when-let* ((getp (search-backward "announce" nil t))
                  (start (pos-bol))
                  (end (pos-eol)))
         (with-current-buffer (find-file-noselect zn/aria2-conf-file)
           (goto-char (point-min))
           (re-search-forward "^bt-tracker=")
           (delete-region (point) (pos-eol))
           (insert-buffer-substring (process-buffer proc) start end)
           (save-buffer)
           (kill-buffer)))
         (kill-buffer))))

(with-eval-after-load 'aria2
  (zn/get-bt-tracker "https://gitea.com/XIU2/TrackersListCollection/raw/branch/master/best_aria2.txt"))


;; alist

(defvar zn/alist-data-directory
  (pcase system-type
    ('windows-nt (substitute-in-file-name "$USERPROFILE/scoop/persist/alist/"))
    (_ (expand-file-name "~/.config/alist/")))
  "Where alist store data.")

(defun zn/start-alist ()
  "Start alist server."
  (interactive)
  (rename-file (expand-file-name "log/log.log" zr-alist-data-directory)
               (file-name-concat zr-alist-data-directory "log" (format-time-string "%+4Y-%m-%d-%H-%M")))
  (start-process "alist" nil "alist" "server"
                 "--data" zr-alist-data-directory)
  (pcase system-type
    ('android (android-notifications-notify
               :title "alist"
               :body "Click to stop alist."
               :on-action (lambda (a b)
                            (call-process "pkill" nil nil nil "alist"))))))


;; tcpdump

(defun zn/string-to-hex (str)
  "Convert string to hex string, each byte separated with spaces"
  (mapconcat (lambda (c) (format "%02x" c))
             (string-to-vector str) ""))

(defun zn/get-chunk-size (remaining-bytes)
  "Determine appropriate chunk size (1/2/4) for remaining bytes"
  (cond
   ((>= remaining-bytes 4) 4)
   ((= remaining-bytes 3) (list 2 1))  ; split into 2+1
   ((= remaining-bytes 2) 2)
   ((= remaining-bytes 1) 1)
   (t nil)))

(defun zn/smart-split-hex (hex-str)
  "Split hex string into chunks of valid sizes (1/2/4 bytes)"
  (let ((result '())
        (str hex-str))
    (while (not (string-empty-p str))
      (let* ((remaining-bytes (/ (length str) 2))
             (chunk-size (if (listp (zn/get-chunk-size remaining-bytes))
                             (car (zn/get-chunk-size remaining-bytes))
                           (zn/get-chunk-size remaining-bytes)))
             (hex-size (* chunk-size 2)))
        (push (cons chunk-size (substring str 0 hex-size)) result)
        (setq str (substring str hex-size))))
    ;; Handle the case where we need an extra 1-byte chunk
    (when (= (/ (length hex-str) 2) 3)
      (let ((last-chunk (substring hex-str 4 6)))
        (push (cons 1 last-chunk) result)))
    (nreverse result)))

(cl-defun zn/string-to-tcpdump-filter (string &optional (offset 0))
  "It takes the string you enter, splits it into 1, 2, or 4 byte chunks,
converts them to numbers, and creates a capture filter that matches
those numbers at the offset you provide.

ref: https://www.wireshark.org/tools/string-cf.html
But the website have a bug, maybe it add offset by string instead of
number."
  (let* ((hex-str (zn/string-to-hex string))
         (chunks (zn/smart-split-hex hex-str))
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


;; daemon

(defun zn/manage-daemon (name proc timeout)
  "Manage a daemon process named NAME and kill it after TIMEOUT if its buffer is inactive."
  (let ((buffer (process-buffer proc))
        timer)
    (set-process-query-on-exit-flag proc nil)
    (with-current-buffer buffer
      (unless (local-variable-p 'last-change)
        (make-local-variable 'last-change))
      (setq last-change (buffer-modified-tick))
      (setq timer
            (run-at-time
             timeout timeout
             (lambda ()
               (with-current-buffer buffer
                 (when (and (process-live-p proc)
                            (= last-change (buffer-modified-tick buffer)))
                   (interrupt-process proc)
                   (kill-process proc)
                   (message (format-time-string "%H:%M:%S %%s process killed due to inactivity.") name))
                 (setq last-change (buffer-modified-tick)))))))
    (when timer
      (set-process-sentinel
       proc
       (lambda (proc event)
         (when timer (cancel-timer timer))
         (pcase (process-status proc)
           ((or 'exit 'signal)
            (message "%s process exited." name))
           ('failed
            (message "%s process failed to start." name))))))))

(defun zn/trojan-go-daemon ()
  "Start trojan-go daemon and kill it if the log buffer is inactive."
  (interactive)
  (let* ((default-directory (expand-file-name "~/.config/trojan-go"))
         (proc (start-process "trojan-go-daemon" "*trojan-go*"
                              "trojan-go" "-config" "config.json")))
    (if proc
        (zn/manage-daemon "trojan-go" proc (* 60 5))
      (message "Failed to start trojan-go-daemon."))))

(provide 'init-net)
;;; init-net.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("zn/" . "zr-net-"))
;; End:
