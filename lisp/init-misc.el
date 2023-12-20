;;; init-misc.el --- Misc Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/global-prefix-map (make-sparse-keymap)
  "A keymap for myself.")

(defvar my/mpc-prefix-map (make-sparse-keymap)
  "A keymap for mpc.")

(defvar my/org-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(defcustom my/fonts-list
  '("LXGW WenKai Mono" "Sarasa Mono SC" "Unifont-JP" "UnifontExMono")
  "prefered fonts"
  :group 'my
  :type '(repeat string))

(defcustom my/termux "/data/data/com.termux/files"
  "termux root path"
  :group 'my
  :type '(string))

(defcustom my/rss-bridge-list '("https://rss-bridge.org/bridge01/"
                                ;; "https://rss-bridge.lewd.tech/"
                                "https://rss.nixnet.services/"
                                "https://wtf.roflcopter.fr/rss-bridge/"
                                "https://rssbridge.flossboxin.org.in/")
  "Public RSS-Bridge Server."
  :group 'my
  :type '(repeat string))

(defcustom my/rss-hub-list '("https://rsshub.zzzr.eu.org/"
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

(defcustom my/rss-bridge-server (car my/rss-bridge-list)
  "RSS bridge default server."
  :group 'my
  :type 'string)

(defcustom my/rss-hub-server (car my/rss-hub-list)
  "RSS hub default server."
  :group 'my
  :type 'string)

(defcustom my/img-cdn-server-list
  '("https://images.weserv.nl?url=${href_ue}"
    "https://imageproxy.pimg.tw/resize?url=${href_ue}")
  "Public Image CDN server."
  :group 'my
  :type '(repeat string))

(defcustom my/img-cdn-server "https://imageproxy.pimg.tw/resize?url=${href_ue}"
  "Default Image CDN server."
  :group 'my
  :type 'string)

(defcustom my/url-redirect-list
  `(("^https://github.com/\\(.+\\)/commit/\\(\\w+\\)$" .
     ;; 针对单个 commit
     (lambda (url)
       (format "https://github.com/%s/commit/%s.patch"
               (match-string 1 url)
               (match-string 2 url))))
    ("^https://github.com/\\(.+\\)/pull/\\([[:digit:]]+\\)$" .
     ;; 针对单个 Pull Request
     (lambda (url)
       (format "https://github.com/%s/pull/%s.patch"
               (match-string 1 url)
               (match-string 2 url))))
    ("^https://github.com/\\(.+\\)/blob/\\(.+\\)" .
     ;; 针对单个文件
     (lambda (url)
       (format "https://github.com/%s/raw/%s"
               (match-string 1 url)
               (match-string 2 url)))))
  "Refind webpage experience: from https://emacstalk.codeberg.page/post/018/"
  :group 'my
  :type '(repeat regexp))

(defcustom my/doh-server-list '("https://1.1.1.1/dns-query"
                                "https://1.12.12.12/dns-query"
                                "https://223.6.6.6/dns-query")
  "Public DOH Server."
  :group 'my
  :type '(repeat string))

(defcustom my/doh-server (car my/doh-server-list)
  "Default DOH Server."
  :group 'my
  :type 'string)

(defcustom my/proxy-domain '("\\(\\.\\|^\\)google.com$")
  "Domain through proxy."
  :group 'my
  :type '(repeat regexp))

(defcustom my/centaur-proxy "127.0.0.1:10807"
  "Set HTTP/HTTPS proxy."
  :group 'my
  :type 'string)

(defcustom my/centaur-socks-proxy "127.0.0.1:10808"
  "Set SOCKS proxy."
  :group 'my
  :type 'string)

(defun my/setup-faces ()
  "Randomize setup faces."
  (when (display-graphic-p)
    (let* ((fonts (remove (face-attribute 'default :family) my/fonts-list))
           (font (cl-loop for ft = (seq-random-elt fonts)
                          always fonts
                          do (delete ft fonts)
                          when (find-font (font-spec :name ft))
                          return ft))
           (size (cond ((string= font "LXGW WenKai Mono") '("26" "14" "18"))
                       ((string= font "Sarasa Mono SC") '("24" "14" "17"))
                       ((string-prefix-p "Unifont" font) '("26" "14" "18"))))
           (height (cond ((string= font "LXGW WenKai Mono") '(198 108 140))
                         ((string= font "Sarasa Mono SC") '(188 108 130))
                         ((string-prefix-p "Unifont" font) '(198 108 142)))))
      ;; (add-to-list 'default-frame-alist
      ;;              `(font . ,(cond ((< (display-pixel-width) 1920) (concat font "-" (car size)))
      ;;                              ((> (display-pixel-width) 1920) (concat font "-" (caddr size)))
      ;;                              (t (concat font "-" (cadr size))))))
      (set-face-attribute 'default nil :font font :height
                          (cond ((< (display-pixel-width) 1920) (car height))
                                ((> (display-pixel-width) 1920) (caddr height))
                                (t (cadr height)))))

    (load-theme 'leuven)))

(defun my/advice-silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN.
https://scripter.co/using-emacs-advice-to-silence-messages-from-functions"
  (let ((inhibit-message t)    ;Don't show the messages in Echo area
        (message-log-max nil)) ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))

(defun my/insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
  (interactive "P")
  (insert-pair arg ?\' ?\'))

(defun my/insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(defun my/insert-curlybracket (&optional arg)
  "Enclose following ARG sexps in curlybracket.
Leave point after open-bracket."
  (interactive "P")
  (insert-pair arg ?\{ ?\}))

(defun my/run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))

(defun my/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (if (string= shell-file-name "C:\\Windows\\system32\\bash.exe")
      (setq shell-file-name my/vanilla-shell)
    (setq my/vanilla-shell shell-file-name
          shell-file-name "C:\\Windows\\system32\\bash.exe")))

(defun my/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))

;; todo
(defun my/mpv-intent (scheme &optional sub)
  "http://mpv-android.github.io/mpv-android/intent.html
am: [-e|--es <EXTRA_KEY> <EXTRA_STRING_VALUE> ...]  filepath
scheme: rtmp, rtmps, rtp, rtsp, mms, mmst, mmsh, tcp, udp,
content, file, http, https example:
am start -n is.xyz.mpv/is.xyz.mpv.MPVActivity -e filepath
file:///sdcard/Music/local

optional: - decode_mode (Byte): if set to 2, hardware decoding
will be disabled - subs (ParcelableArray of Uri): list of
subtitle URIs to be added as additional tracks -
subs.enable (ParcelableArray of Uri): specifies which of the
subtitles should be selected by default, subset of previous array
- position (Int): starting point of video playback in
milliseconds"
  (when (file-exists-p scheme)
    (let ((url (concat "file://" scheme)))
      (start-process "" nil "am" "start" "-n"
                     "is.xyz.mpv/is.xyz.mpv.MPVActivity"
                     "-e" "filepath" url))))

(defun my/fooview-run (cmd)
  "Run fooview action."
  (start-process "fooview-run" nil "am" "start" "-a"
                 "com.fooview.android.intent.RUN_WORKFLOW" "-e" "action"
                 cmd "com.fooview.android.fooview/.ShortcutProxyActivity"))

(defun my/rish-run (cmd)
  "Run command with rish."
  (start-process "rish-run" nil "rish" "-c" cmd))

(defun my/normal-keyboard ()
  "Enable normal keyboard on android."
  (my/rish-run "ime set com.samsung.android.honeyboard/.service.HoneyBoardService"))

(defun my/bare-keyboard ()
  "Enable bare keyboard on android."
  (my/rish-run "ime set keepass2android.keepass2android/keepass2android.softkeyboard.KP2AKeyboard"))

(defun my/rss-bridge-generator (bridge)
  "Generate atom feed via rss-bridge."
  (concat my/rss-bridge-server
          "?action=display&format=Atom&bridge=" bridge))

(defun my/rss-bridge-wp (blog limit &optional content)
  "Returns the newest full posts of a WordPress powered website."
  (concat (my/rss-bridge-generator "WordPressBridge")
          "&url=" (url-hexify-string blog)
          "&limit=" (number-to-string limit)
          (when content (concat "&content_selector=" (url-hexify-string content)))))
;; todo
;; (defun my/rss-bridge-filter ())

(defun my/rss-bridge-reducer (feed percentage)
  "Choose a percentage of a feed you want to see."
  (concat (my/rss-bridge-generator "FeedReducerBridge")
          "&url=" (url-hexify-string feed)
          "&percentage=" (number-to-string percentage)))

(defun my/rss-bridge-css-expander (feed limit content &optional
                                        content-cleanup
                                        dont-expand-metadata
                                        discard-thumbnail)
  "Expand any site RSS feed using CSS selectors."
  (concat (my/rss-bridge-generator "CssSelectorFeedExpanderBridge")
          "&feed=" (url-hexify-string feed)
          "&limit=" (number-to-string limit)
          "&content_selector=" (url-hexify-string content)
          (when content-cleanup (concat "&content_cleanup=" (url-hexify-string content-cleanup)))
          (concat "&dont_expand_metadata=" (when dont-expand-metadata "on"))
          (concat "&discard_thumbnail=" (when discard-thumbnail "on"))))

(defun my/rss-bridge-css-selector (home limit entry load-pages &rest args)
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
    (concat (my/rss-bridge-generator "CssSelectorComplexBridge")
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

(defun my/rss-bridge-merger (feeds limit name)
  "This bridge merges two or more feeds into a single feed. Max 10
items are fetched from each feed."
  (concat (my/rss-bridge-generator "FeedMergeBridge")
          (let ((m 0))
            (mapconcat
             (lambda (feed)
               (setq m (+ m 1))
               (concat "&feed_" (number-to-string m) "="
                       (url-hexify-string feed)))
             feeds))
          "&limit=" (number-to-string limit)
          "&feed_name=" (url-hexify-string name)))

(defun my/rss-hub-generator (router &rest args)
  "Generate feed via RSSHub."
  (let* ((fmt (plist-get args :fmt))
         (main (concat my/rss-hub-server router
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
                ,(when img-tp (concat "image_hotlink_template=" (url-hexify-string my/img-cdn-server)))
                ,(when domain (concat "domain=" (url-hexify-string domain)))
                ,(when code (concat "code=" (md5 (concat (url-filename url) code))))))
             "&"))))

(defun my/rss-hub-transform (url s-fmt &rest args)
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
     #'my/rss-hub-generator
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

(defun my/advice-newsticker-start (&optional _do-not-complain-if-running)
  (let ((running (newsticker-running-p)))
    (unless running
      (newsticker--cache-read))
    ;; start retrieval timers -- one timer for each feed
    (let ((counter 0))
      (dolist (feed (append newsticker-url-list-defaults newsticker-url-list))
        (setq counter (+ counter 1))
        (run-at-time (* counter 10) nil 'newsticker--start-feed feed)))
    (unless running
      (run-hooks 'newsticker-start-hook)
      (message "Newsticker started!"))))

(defun my/advice-newsticker--get-news-by-wget (args)
  (let ((host (url-host (url-generic-parse-url (cadr args))))
        (domains my/proxy-domain)
        (wget-arguments (caddr args)))
    (catch 'aaa
      (while domains
        (if (string-match-p (car domains) host)
            (progn
              (setf (caddr args)
                    (append wget-arguments
                            `("-x"
                              ,(concat "http://emacs@"
                                       my/centaur-proxy))))
              (throw 'aaa "a"))
          (setq domains (cdr domains))))))
  args)

(defun my/newsticker-treeview-prev-page ()
  "Scroll item buffer."
  (interactive)
  (save-selected-window
    (select-window (newsticker--treeview-item-window) t)
    (condition-case nil
        (scroll-down nil)
      (error
       (goto-char (point-max))))))

(defun my/advice-newsticker-save-item (feed item)
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

(defun my/advice-url-redirect (fn url &rest args)
  (catch 'ret
    (dolist (redirect-rule my/url-redirect-list)
      (let* ((regexp (car redirect-rule))
             (redirect-fn (cdr redirect-rule))
             (inhibit-message t))
        (when-let* ((matched-groups (string-match regexp url)))
          (setq url (funcall redirect-fn url))
          (message "Redirect URL to %s" url)
          (throw 'ret url)))))
  (apply fn url args))

(defun my/eww-render-hook()
  (let ((url (plist-get eww-data :url)))
    (cond
     ((string-suffix-p ".patch" url) (diff-mode))
     ((string-suffix-p ".el" url) (emacs-lisp-mode))
     ((string-suffix-p ".rs" url) (rust-mode))
     ((string-suffix-p ".go" url) (go-mode))
     (t (when (and (plist-get eww-data :source)
                   ;; 排除微信公众号内的文章
                   (not (string-match-p "weixin\\.qq\\.com" url)))
          (eww-readable))))))

(defun my/dired-dwim ()
  "start process with current file"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (when (eq filename nil)
      (setq filename default-directory))
    (my/mpv-intent filename))
  )

(defun my/mpv-image ()
  "mpv play current directory with miniplayer"
  (interactive)
  (async-shell-command
   (concat
    "mpv --ontop --autofit=30% --geometry=100%:20% --shuffle --image-display-duration=60 --no-osc --no-osd-bar --cursor-autohide=no --no-input-cursor \""
    default-directory "\"")
   "mpv"))

(defun my/dired-duplicate-file (arg)
  "Duplicate a file from dired with an incremented number.
If ARG is provided, it sets the counter.
https://www.emacs.dyerdwelling.family/emacs/20231013153639-emacs--more-flexible-duplicate-thing-function/"
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
        (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))

(defun my/advice-image-dired-create-thumb-maybe-gs (oldfun &rest args)
  (when (string= (file-name-extension (car args)) "pdf")
    (let ((image-dired-cmd-create-thumbnail-program "gs")
          (image-dired-cmd-create-thumbnail-options '("-sDEVICE=jpeg" "-dSAFER" "-r20" "-o" "%t" "%f")))
      (apply oldfun args))))

(defun my/internet-up-p (&optional host callback)
  "Test connectivity via ping."
  (let* ((args (if (eq system-type 'windows-nt)
                   '("-n" "1" "-w" "1") '("-c1" "-W1")))
         (proc (apply #'start-process "internet-test" nil "ping"
                      (if host host "baidu.com") args))
         (callback (if callback callback
                     (lambda (&rest cbargs)
                       (message (if cbargs "Up" "Down"))))))
    (set-process-sentinel proc (lambda (proc signal)
                                 (apply callback
                                        (if (= 0 (process-exit-status proc))
                                            '(t) nil))))))

(defun my/url-up-p (url &optional callback doh)
  "Test connectivity via curl."
  (let* ((args `("-kqsLm10" "-o/dev/null" "-w%{http_code}" ,url
                 ,@(when doh (list "--doh-url" doh))))
         (proc (apply #'start-process "url-test" nil "curl" args))
         (callback (if callback callback
                     (lambda (&rest cbargs)
                       (message (if cbargs "Up" "Down"))))))
    (set-process-filter proc (lambda (proc line)
                               (apply callback
                                      (if (string= "200" line)
                                          '(t) nil))))))

(defun my/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" my/centaur-proxy)
    (message "No HTTP proxy")))

(defun my/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my/centaur-proxy)
          ("https" . ,my/centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (my/proxy-http-show))

(defun my/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (my/proxy-http-show))

(defun my/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (my/proxy-http-disable)
    (my/proxy-http-enable)))

(defun my/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun my/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string my/centaur-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" my/centaur-socks-proxy))
  (my/proxy-socks-show))

(defun my/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (my/proxy-socks-show))

(defun my/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (my/proxy-socks-enable)))

(defun my/url-http-parse-response ()
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

(provide 'init-misc)
;;; init-misc.el ends here
