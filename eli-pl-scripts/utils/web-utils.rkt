#lang at-exp racket/base

(require scribble/html (rename-in scribble/html [begin/text text])
         racket/date racket/path racket/file racket/port scheme/nest
         racket/cmdline racket/system racket/runtime-path racket/list
         racket/stxparam
         (for-syntax racket/base syntax/name)
         "course-utils.rkt")

(provide (except-out (all-from-out scribble/html)
                     pre script/inline) ; rewrapped to have no encodings
         (except-out (all-from-out racket/base)
                     #%module-begin     ; auto-run
                     date)              ; html thing instead
         (all-from-out racket/runtime-path "course-utils.rkt"))

;; ----------------------------------------------------------------------------
;; Customizations

(define customizations (make-hasheq))
(define-syntax-rule (defcustoms c ...)
  (begin (hash-set! customizations 'c #f) ...
         (define-syntax c
           (syntax-id-rules (set!)
             [(set! c v) (hash-set! customizations 'c v)]
             [(custom x (... ...)) ((hash-ref customizations 'c) x (... ...))]
             [custom (hash-ref customizations 'c)]))
         ...
         (provide c ...)))

(defcustoms *publish-dirs* *publish?* *private-dir* *title-prefix* *icon*)

;; ----------------------------------------------------------------------------
;; Utilities

(provide ->string)
(define (->string x)
  (cond [(string? x) x]
        [(path?   x) (path->string x)]
        [(bytes?  x) (bytes->string/utf-8 x)]
        [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [else (error '->string "don't know what to do with ~e" x)]))

(provide output->string)
(define (output->string x) (with-output-to-string (λ () (output x))))

;; used to make anchor labels
(define (section->label title)
  (regexp-replace* #rx"[^a-z0-9_]+" (string-downcase title) ""))

(provide counter)
(define (counter [c 1])
  (define (counter)
    (begin0 c
      (cond [(number? c) (set! c (add1 c))]
            [(char? c) (set! c (integer->char (add1 (char->integer c))))]
            [else (error 'counter "bad initial value: ~e" c)])))
  counter)

;; generic properties
(provide prop-set! prop-ref with-props)
(define-values [prop-set! prop-ref]
  (let ([t (make-weak-hasheq)])
    (values
     (λ (obj prop val . more)
       (let ([t (hash-ref! t obj make-hash)])
         (let loop ([prop prop] [val val] [more more])
           (hash-set! t prop val)
           (when (pair? more) (loop (car more) (cadr more) (cddr more))))))
     (λ (obj prop [default #f])
       (hash-ref (hash-ref! t obj make-hash) prop default)))))
(define-syntax with-props
  (syntax-rules ()
    [(with-props obj) obj]
    [(with-props obj k v more ...)
     (let ([o obj])
       (prop-set! o k v more ...)
       o)]))

;; ----------------------------------------------------------------------------
;; Referable resources, taken from `meta/web/common/utils', added keyword
;; ability.

;; resources with a specific referrer; if the referrer is `values', return a
;; plain resource (which behaves the same)
(provide resource/referrer url-of)
(struct referable (referrer resource) #:property prop:procedure 0)
(define (resource/referrer path renderer referrer)
  (define url (resource path renderer))
  (if (eq? referrer values)
    url
    (referable (make-keyword-procedure
                (λ (kws kw-args . xs)
                  (keyword-apply referrer kws kw-args (url) xs)))
               url)))
(define (url-of referable [absolute? #f])
  (cond [(referable? referable) ((referable-resource referable) absolute?)]
        [(resource? referable)  (referable absolute?)]
        [else (raise-type-error 'url-of "referable" referable)]))

;; ----------------------------------------------------------------------------
;; Date utilities

(define date-re
  (regexp (string-append "^([0-9][0-9][0-9][0-9])-([0-9]+)-([0-9]+)"
                         "(?: ([0-9]+):([0-9]+))?$")))

(provide now-seconds now-datestr)
(define-values [now-seconds now-year now-datestr]
  (let* ([s (current-seconds)] [d (seconds->date s)])
    (values s (date-year d)
            (let ([p (λ (f)
                       (let ([n (f d)])
                         (format "~a~a" (if (< n 10) "0" "") n)))])
              (format "~a-~a-~a" (p date-year) (p date-month) (p date-day))))))

(provide time-too-far?)
(define seconds-in-month (* 30 24 60 60))
(define (time-too-far? secs)
  (define range (* 4 seconds-in-month))
  (cond [(< range (- secs now-seconds)) 'future]
        [(< range (- now-seconds secs)) 'past]
        [else #f]))

(define (parse-datestr str)
  (map (λ (x) (and x (string->number x)))
       (cdr (or (regexp-match date-re str)
                (error 'parse-datestr "bad date: ~e" str)))))

(define datestr-or-numbers->seconds
  (case-lambda
    [(datestr)
     (apply datestr-or-numbers->seconds (parse-datestr datestr))]
    [(year month day hour min)
     (find-seconds 0 (or min 0) (or hour 0) day month year)]))

(provide datestr->seconds) ; unary only version of the above
(define (datestr->seconds str) (datestr-or-numbers->seconds str))

(provide datestr->string)
(define (datestr->string str)
  (define date-nums (parse-datestr str))
  (define-values [year month day hour min] (apply values date-nums))
  (let* ([year? (not (= year now-year))]
         [secs  (apply datestr-or-numbers->seconds date-nums)]
         [date  (seconds->date secs)]
         [far?  (time-too-far? secs)])
    (case far?
      [(future)
       (error 'datestr->string "date too far into the future: ~e" str)]
      [(past)
       (error 'datestr->string "date too far into the past: ~s" str)])
    (string-append
     (vector-ref #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
                   "Friday" "Saturday")
                 (date-week-day date))
     ", " (vector-ref #("January" "February" "March" "April" "May" "June"
                        "July" "August" "September" "October" "November"
                        "December")
                      (sub1 month))
     " " (number->string day)
     (case (+ (modulo day 10) (if (<= 10 day 19) 10 0))
       [(1) "st"] [(2) "nd"] [(3) "rd"] [else "th"])
     (if year? (format " ~a" year) "")
     (if hour
       (format ", ~a:~a~a~a"
               (if (< hour 13) hour (modulo hour 12))
               (if (< min 10) "0" "") min
               (let ([small-min? (< min 10)])
                 (cond [(and (= hour 0) small-min?) "midnight"]
                       [(and (= hour 12) small-min?) "noon"]
                       [(<= 0 hour 11) "am"]
                       [else "pm"])))
       ""))))

(provide (rename-out [date* date]))
(define (date* str) (span class: 'date (datestr->string str)))

;; ----------------------------------------------------------------------------
;; Encoders

;; custom writer:
;; * `' and ``'' turn to [lr][sd]quo
;; * -- and --- turn to ndash and mdash
;; * \* is bull, \. is sdot, \  is nbsp etc
;; * \x for anything else is just that character
(define ((make-custom-writer rx) str p [start 0] [end (string-length str)])
  (let loop ([start start])
    (when (< start end)
      (let ([m (regexp-match-positions rx str start end p)])
        (when m
          (let ([m0 (caar m)])
            (write-string
             (case (string-ref str m0)
               [(#\&) "&amp;"]
               [(#\<) "&lt;"]
               [(#\>) "&gt;"]
               [(#\") "&quot;"]
               [(#\`) (if (= 1 (- (cdar m) m0)) "&lsquo;" "&ldquo;")]
               [(#\') (if (= 1 (- (cdar m) m0)) "&rsquo;" "&rdquo;")]
               [(#\-) (if (= 2 (- (cdar m) m0)) "&ndash;" "&mdash;")]
               [(#\\) (case (string-ref str (add1 m0))
                        [(#\space) "&nbsp;"]
                        [(#\*) "&bull;"]
                        [(#\-) "&mdash;"]
                        [(#\C) "&copy;"]
                        [(#\R) "&reg;"]
                        [(#\T) "&trade;"]
                        [(#\.) "&sdot;"]
                        [(#\n) "\\n"] ; convenient for \n in code
                        ;; not really needed, but common so make it fast
                        [(#\\) "\\"]
                        [(#\;) ";"]
                        [else (error 'html-writer "unknown escape sequence: ~a"
                                     str)
                              (string (string-ref str (add1 m0)))])])
             p))
          (loop (cdar m)))))))

(define markdown-writer ; all tex-like markups
  (make-custom-writer #rx"[&<>\"]|``?|''?|---?|\\\\."))

(define plain-writer ; just the bare html encoding
  (make-custom-writer #rx"[&<>\"]"))

(define (plain-text body)
  (with-writer plain-writer body))

;; ----------------------------------------------------------------------------
;; HTML wrappers for raw text (only plain html encoding)

(define-syntax-rule (raw-wrap-reprovide name)
  (begin (provide (rename-out [wrapped name]))
         (define (wrapped . args) (plain-text (apply name args)))))
(raw-wrap-reprovide pre)
(raw-wrap-reprovide script/inline)

;; ----------------------------------------------------------------------------
;; HTML gadgets

(provide itemize enumerate @item>)
(define item> 'item>)
(define ((lister base) #:type [type #f] #:start [start #f] #:br [br 0] . body)
  ;; strips off spaces at the edges of items (there is still a potential
  ;; problem with an outdented `@item>' leading to spaces in text after
  ;; newlines, but that's fine)
  (define (whitespace? str)
    (and (string? str) (regexp-match? #px"^\\s*$" str)))
  (define (clean-prefix str)
    (if (string? str) (regexp-replace #px"^\\s+" str "") str))
  (define (clean-suffix str)
    (if (string? str) (regexp-replace #px"\\s+$" str "") str))
  (define items
    (let loop ([args body] [cur #f] [r '()])
      (define (addcur)
        (if (not cur)
          r
          (cons (reverse
                 (let loop ([cur cur])
                   (cond [(null? cur) cur]
                         [(whitespace? (car cur)) (loop (cdr cur))]
                         [else (cons (clean-suffix (car cur)) (cdr cur))])))
                r)))
      (if (null? args)
        (reverse (addcur))
        (let ([1st (car args)]
              [loop (λ (cur r) (loop (cdr args) cur r))])
          (cond [(eq? 1st item>) (loop #f (addcur))]
                [cur (loop (cons 1st (or cur '())) r)]
                [(whitespace? 1st) (loop cur r)]
                [else (loop (cons (clean-prefix 1st) '()) r)])))))
  (let* ([br (and (br . > . 0) (format "margin-bottom: ~aex;" br))]
         [li (if br (λ xs (apply li style: br xs)) li)])
    (apply base `(,@(if type  `(,type: ,type)   '())
                  ,@(if start `(,start: ,start) '())
                  ,@(add-newlines (map li items))))))
(define itemize   (lister ul))
(define enumerate (lister ol))

(provide boxed)
(define (boxed #:width [width #f] #:align [align #f]
               #:fg [fg "black"] #:bg [bg #f]
               . content)
  (span style:
        (list "display: block; border-style: solid; border-width: 2px;"
              " padding: 8px; border-color: "fg";"
              (and bg (list " background-color: "bg";"))
              (and width (list " width: "width"; "))
              (and align (list " text-align: "align";")))
    content))

(provide codebox)
(define (codebox . body)
  (table (tr (td (plain-text (apply pre class: 'codebox body))))))

(provide c)
(define (c . body)
  ;; tried wrapping in lsquo/rsquo, looked too noisy
  (plain-text (apply span class: 'code body)))

(provide ang)
(define l< (entity 'lang))
(define r> (entity 'rang))
(define (ang . xs) `(,l< ,@xs ,r>))

(provide v)
(define (v var #:key [key #f] #:sub [idx #f])
  (let ([m (and (not idx)
                (string? var)
                (or (regexp-match #rx"^(.*?)_([^_]+)$" var)
                    (regexp-match #rx"^(.*?)([0-9]+)$" var)))])
    (if m
      (v (cadr m) #:sub (caddr m))
      (span class: 'var
            var (and idx (small (small (small (sub idx)))))))))

(provide vars)
(define (vars var #:sep [sep " "] #:idx [idx "n"])
  (list (v var #:sub 1)
        sep (v var #:sub 2)
        sep (v "...")
        sep (v var #:sub idx)))

(provide semi)
(define (semi . body)
  (list "; " (apply span class: 'var body)))

(provide mailto)
(define (mailto email [label #f])
  (a href: (list "mailto:" email) (or label (tt email))))

(provide red)
(define (red . body)
  (apply span style: "color: red; font-weight: bold;" body))

(provide warn-line)
(define (warn-line . body) (apply div class: 'warn-line body))

(provide vspace)
(define (vspace n) (printonly (make-list n (br))))

(provide printonly screenonly)
(define (printonly  . body) (apply div class: 'printonly  body))
(define (screenonly . body) (apply div class: 'screenonly body))

(provide newpage-or-br)
(define (newpage-or-br)
  (list (screenonly (br) (br))
        (printonly (div style: "page-break-after: always;" nbsp))))

;; to include source files in exam texts
(provide include-file)
(define (include-file file #:regexp [regexp #f] #:replace [replace values])
  (delay
    (let* ([file (force file)]
           [file (cond [(path-string? file) file]
                       [(prop-ref file 'source)]
                       [else (error 'include-file
                                    "not a path or an object with a source: ~e"
                                    file)])]
           [str (file->string file)]
           [str (regexp-replace* #rx"[\\]" str "\\\\\\\\")]
           [str (if regexp
                  (cadr (or (regexp-match regexp str)
                            (error 'include-file "no match for ~s in ~a"
                                   regexp file)))
                  str)]
           [str (replace str)])
      ;; (small (small (pre str)))
      (pre str))))

;; ----------------------------------------------------------------------------
;; Navigation stuff

(provide *navbar-pages*) ; make it possible to actually add navbar pages
(define *navbar-pages* (make-parameter '()))

(define (make-navbar current-page)
  (list
   (ul class: 'navbar
       (and ((length (*navbar-pages*)) . > . 1)
            (map (λ (n) (li class: (and (eq? n current-page) 'curpagelink) n))
                 (*navbar-pages*))))
   *search-box*))

(define *search-box*
  (delay
    (list @input[id: 'search-box type: 'text value: "" autocomplete: 'off
                 size: 20 onchange: "return search_for(this.value);"]
          ;; need an absolutely positioned wrapper for some reason
          @div[style: "position: absolute;"]{
            @div[class: 'autocomplete id: 'search-completions
                 style: "display: none;"]}
          @script/inline[type: "text/javascript" language: "JavaScript"]{
            @; could do this in anchors.js, but then it happens only after
            @; a noticeable time
            document.getElementById("search-box").style.display = "inline";
          })))

;; anchors for quick searching (for pages and sections)
(define *anchors* '())
(define (add-anchors! title/s link #:anchor [anchor #f] #:prefix [prefix #f])
  (unless (null? title/s)
    (define titles (if (list? title/s) (reverse title/s) (list title/s)))
    (define (link*) (list (url-of link) (and anchor (list "#" anchor))))
    (set! *anchors*
          (append
           (map (if prefix
                  (λ (title) (cons (list prefix " > " title) link*))
                  (λ (title) (cons title link*)))
                titles)
           *anchors*))))
(define (make-epilogue)
  (define (make-array name fun)
    @list{var @name = [
            @(add-between (map fun (reverse *anchors*)) ",\n")]})
  (plain*
   #:file "anchors.js"
   @text{
     @make-array['search_strings (λ (a) @list{"@(car a)"})];
     @make-array['search_anchors (λ (a) @list{"/@((cdr a))"})];

     @; Gets two strings and looks for the first being a subsequence of the
     @; second (contains all chars, in order), returns the continuous ranges
     @; where s1 is in s2 as an array of two-item arrays, or [] if s1 is not a
     @; subsequence of s2; search going forward, then back in case there is a
     @; tighter match (since the results are ordered by gap sizes), and then
     @; going forward again so if there are two ways for a match the more
     @; natural one is chosen.
     function my_compare(s1, s2) {
       var i1 = 0, i2 = 0, start = 0, r, result = [];
       @; first scan: going forward, and abort if not a subsequence
       while (i1 < s1.length) {
         r = s2.indexOf(s1[i1], i2);
         if (r < 0) return [];
         i1++, i2=r+1;
       }
       @; second scan: from the end, going back (no need to check for a -1 now)
       i1-=2, i2-=2;
       while (i1 >= 0) {
         r = s2.lastIndexOf(s1[i1],i2);
         i1--, i2=r;
       }
       @; last scan: forward again, and collect match ranges now
       i1=0, start=i2;
       while (i1 < s1.length) {
         r = s2.indexOf(s1[i1], i2);
         if (r < 0) return [];
         if (r > i2) {
           if (start < i2) result.push([start,i2]);
           start = r;
         }
         i1++, i2=r+1;
       }
       if (start < i2) result.push([start,i2]);
       return result;
     }
     @;{
     function T(s1, s2, r) {
       if (my_compare(s1,s2).join(";") != r)
         alert("Test failure for: my_compare("+s1+","+s2+")");
     }
     // basic tests
     T("",    "",    "");
     T("x",   "abc", "");
     T("a",   "abc", "0,1");
     T("b",   "abc", "1,2");
     T("c",   "abc", "2,3");
     T("ab",  "abc", "0,2");
     T("bc",  "abc", "1,3");
     T("ac",  "abc", "0,1;2,3");
     T("ac",  "abc", "0,1;2,3");
     T("abc", "abc", "0,3");
     T("ca",  "abc", "");
     T("abc", "ab",  "");
     // finds a tighter match when there is one
     T("abc", "aaabc", "2,5");
     T("abc", "a abc", "2,5");
     T("abc", "a  bc", "0,1;3,5");
     // but still goes forward with how it searches
     T("abc", "aaabbbccc", "2,4;6,7");
     }@;

     function my_selector(inst) {
       var search = inst.getToken().toLowerCase();
       var options = inst.options.array;
       var result = [], i, j, r;
       for (i=0@";" i<options.length@";" i++) {
         r = my_compare(search, options[i].toLowerCase());
         if (r.length > 0) result.push([options[i],r]);
       }
       result.sort(function(x,y){
         @; 1. longer sequences win (=> shorter range arrays)
         @; if (x[1].length != y[1].length)
         @;   return (x[1].length < y[1].length) ? -1 : 1;
         @; No -- generalize: shorter gaps win
         var d = 0, i;
         for (i=x[1].length-1@";"i>0@";"i--) d += (x[1][i][0] - x[1][i-1][1]);
         for (i=y[1].length-1@";"i>0@";"i--) d -= (y[1][i][0] - y[1][i-1][1]);
         if (d != 0) return (d<0) ? -1 : 1;
         @; 2. matches for more content win (=> shorter texts)
         if (x[0].length != y[0].length)
           return (x[0].length < y[0].length) ? -1 : 1;
         @; 3. a match wins if it starts at the front (=> earlier start)
         if (x[1][0][0] != y[1][0][0])
           return (x[1][0][0] < y[1][0][0]) ? -1 : 1;
         @; Could also add that a match wins if it begins at a word boundary,
         @; or better if more word boundaries in the match correspond to word
         @; boundaries where they're found.  But skip that for now.
         return (x[0] <= y[0]) ? -1 : 1;
       });
       var truncated = (result.length > inst.options.choices);
       if (truncated) result = result.slice(0, inst.options.choices);
       for (i=0@";" i<result.length@";" i++) {
         var str = result[i][0], range = result[i][1];
         r = "";
         for (j=0@";" j<range.length-1@";" j++)
           r = r + "<u>" + str.substring(range[j][0],range[j][1])
                 + "</u>" + str.substring(range[j][1],range[j+1][0]);
         j = range.length-1;
         result[i] = "<li>" + str.substring(0,range[0][0]) + r +
                     "<u>" + str.substring(range[j][0],range[j][1]) +
                     "</u>" + str.substring(range[j][1]) + "</li>";
       }
       if (truncated) result.push("<li>...truncated...</li>");
       return "<ul>" + result.join('') + "</ul>";
     }

     var searcher =
       new Autocompleter.Local(
         "search-box", "search-completions", search_strings,
         { ignoreCase: true, fullSearch: true, selector: my_selector,
           choices: 12 });
     searcher.oldSelectEntry = searcher.selectEntry;
     searcher.selectEntry = function() {
       var r = this.oldSelectEntry();
       search_for(document.getElementById("search-box").value);
       return r;
     }

     function search_for(str) {
       for (var i=0@";" i<search_strings.length@";" i++) {
         if (search_strings[i] == str) {
           location.href = search_anchors[i];
           return true;
         }
       }
       return false;
     }
   })
  (render-all))

(define-runtime-path js-dir "../content/js")
(define *footer*
  (delay (for ([f (in-list (directory-list js-dir))])
           (filer (build-path js-dir f) #:path (format "js/~a" f)))
         (list @script[src: "/js/prototype.js" type: "text/javascript"]
               @script[src: "/js/scriptaculous.js?load=effects,controls"
                       type: "text/javascript"]
               @script[src: "/anchors.js" type: "text/javascript"])))

;; ----------------------------------------------------------------------------
;; Resources: html pages, text files, plain files to be copied

(provide page plain filer)

(define-syntax-parameter this-page (λ (stx) #'#f))

(define-for-syntax (process-contents layouter stx xs)
  (let loop ([xs xs] [kws '()] [id? #f] [title #f])
    (syntax-case xs ()
      [(k v . xs) (keyword? (syntax-e #'k))
       (let ([k* (syntax-e #'k)])
         (loop #'xs (list* #'v #'k kws)
               (or id?   (and (eq? '#:id    k*) #'v))
               (or title (and (eq? '#:title k*) #'v))))]
      [_ (with-syntax ([layouter layouter]
                       [name (syntax-local-infer-name stx)]
                       [(x ...) xs]
                       [title (or title #'#f)])
           (with-syntax ([(k ...) (if id?
                                    (reverse kws)
                                    #`(#:id 'name #,@(reverse kws)))]
                         [body
                          #'(syntax-parameterize ; bind `this-page'
                                ([this-page (make-rename-transformer #'name)])
                              (λ () ; delay body
                                (text x ...)))]) ; allow definitions
             #'(letrec ([name (layouter k ... body)]) name)))])))

(define-syntax (page stx)
  (syntax-case stx ()
    [(_ . xs) (process-contents #'page* stx #'xs)]))

(define-syntax (plain stx)
  (syntax-case stx ()
    [(_ . xs) (process-contents #'plain* stx #'xs)]))

;; for plain text files
(define (plain* #:id [id #f] #:suffix [suffix #f]
                #:file
                [file (if (and id suffix)
                        (format "~a.~a" (force id) suffix)
                        (error 'plain
                               "missing `#:file', or `#:id' and `#:suffix'"))]
                #:referrer [referrer values]
                #:newline  [newline? #t]
                . content)
  (resource/referrer file
                     (file-writer output (list content (and newline? "\n")))
                     referrer))

(define focus-bgcolor      "#e0e060")
(define header-bgcolor     "#e0e0ff")
(define header-bordercolor "#c0c0ff")
(define main-style
  @plain[#:suffix "css"]{
    @; === general stuff ===
    html {
      @; force a scrollbar to avoid horizontal jitter
      overflow-y: scroll;
    }
    body {
      font-family: arial, sans-serif;
      color: black;
      background-color: white;
    }
    @; === visible headers ===
    h1 { font-size: 220%@";" }
    h2 { font-size: 160%@";" }
    h3 { font-size: 120%@";" }
    h1, h2, h3 {
      font-family: "Arial Black", arial, sans-serif;
      font-weight: bolder;
      background-color: @header-bgcolor;
      border: 2px solid @header-bordercolor;
      display: block;
      padding: 0.25ex 0.5em;
    }
    .subtitle {
      font-style: italic;
      font-size: 40%;
      margin: 0 0 0 1em;
      padding: 0;
    }
    @; === default cute link colors ===
    a:link, a:visited {
      color: #502010;
      background-color: transparent;
    }
    a:active, a:hover, a:focus {
      color: #600000;
      background-color: @focus-bgcolor;
    }
    @; === various elements ===
    .date {
      font-style:  italic;
      white-space: nowrap;
    }
    .code {
      font-family: monospace;
      white-space: pre;
    }
    .codebox {
      font-family: monospace;
      background-color: #e0e0e0;
      width: auto;
      margin: 10px 40px;
      padding: 4px;
      @; border-style: double solid;
      @; border-width: thick thin;
      border-style: solid none;
      border-width: thin;
      border-color: black;
      white-space: pre;
    }
    .var {
      font-family: serif;
      font-style:  italic;
      white-space: nowrap;
    }
    .warn-line {
      background-color: #ff8080;
      font-weight: bold;
      font-size: 125%;
      text-align: center;
      margin: 1ex 0;
      padding: 4px;
    }
    @; === navbar ===
    .navbar {
      text-align: right;
      margin: 0;
      padding: 4px 0;
      border-bottom: 2px solid @header-bordercolor;
      @; must specify all, and use px size, otherwise chrome is problematic
      font: bold 12px "Arial Black", arial, sans-serif;
    }
    .navbar li {
      list-style: none;
      display: inline;
      margin: 0 6px;
    }
    .navbar li a {
      margin: 0;
      padding: 4px 0.5em;
      width: 100%;
      background-color: @header-bordercolor;
      border: 2px solid @header-bordercolor;
      text-decoration: none;
    }
    .navbar .curpagelink a {
      background-color: @header-bgcolor;
      border-bottom: 2px solid @header-bgcolor;
    }
    .navbar a {
      text-decoration: none;
      font-weight: bold;
    }
    .navbar a:active, .navbar a:hover, .navbar a:focus {
      background-color: @focus-bgcolor;
    }
    @; and a class for the top h1 to avoid a top line
    @"@"media screen {
      .pagehead {
        margin-top: 0;
        border-top: none;
      }
    }
    @; === table of contents (at page top) ===
    .tocbar {
      color: black;
      background-color: #e8e8ff;
      text-decoration: none;
      font-size: 82.5%;
      font-weight: bold;
      margin: 0ex 0em;
      padding: 1ex 2em;
    }
    .tocbar ul {
      list-style: none;
      margin: 0;
      padding: 0;
    }
    .tocbar ul a {
      width: 90%;
      display: block;
    }
    .tocbar a {
      text-decoration: none;
      font-weight: bold;
    }
    .tocbar a:link, .tocbar a:visited {
      color: #103010;
      background-color: transparent;
    }
    .tocbar a:active, .tocbar a:hover, .tocbar a:focus {
      color: #600000;
      background-color: @focus-bgcolor;
    }
    @; === search box & completions ===
    #search-box {
      position: absolute;
      top: 42px;
      right: 16px;
      margin: 0;
      padding: 0;
      display: none;
      vertical-align: top;
      z-index: 99;
      background-color: #eee;
    }
    div .autocomplete {
      margin: 0;
      padding: 0;
      @; this is used only for the background box
      width: 100%;
      background-color: #eee;
      border: 1px solid #888;
      position: absolute;
      font-weight: normal;
      font-size: 75%;
    }
    div .autocomplete ul {
      margin: 0;
      padding: 0;
      list-style: none;
    }
    div .autocomplete ul li.selected {
      color: #ff2;
      background-color: #44c;
    }
    div .autocomplete ul li {
      margin: 0;
      padding: 2px;
      display: block;
      @; height: 2ex;
      @; white-space: nowrap;
      list-style: none;
      cursor: pointer;
      border: 0;
    }
    @; === contents for screen/printouts only ===
    .screenonly { display: compact@";" }
    .printonly  { display: none@";"    }
    @"@"media print {
      .screenonly { display: none@";"  }
      .printonly  { display: block@";" }
      @; black links
      a:link, a:visited, a:active, a:hover { color: black@";" }
      @; hide navs
      .navbar, .tocbar { display: none@";" }
    }
  })

(define page-header ; static part of the header that is used for all pages
  (list @meta[name: "generator" content: "Racket"]
        @meta[http-equiv: "Content-Type" content: "text/html; charset=utf-8"]
        (delay
          (and *icon*
               (list @link[rel: "shortcut icon" href: *icon* type: "image/png"]
                     @link[rel: "icon" href: *icon* type: "image/png"])))
        @link[rel: 'stylesheet type: 'text/css href: main-style]))

(define (page*
         #:id [id #f]
         #:file [file (if id (format "~a.html" (force id))
                          (error 'page "missing `#:file' or `#:id'"))]
         #:prefix [prefix *title-prefix*]
         #:title [label (if id
                          (string-titlecase
                           (regexp-replace* #rx"-" (->string id) " "))
                          (error 'page "missing `#:file' or `#:title'"))]
         #:anchor-names [anchor-names (list label)]
         #:doctitle [doctitle (if prefix (list prefix ": " label) label)]
         #:wintitle [wintitle (if prefix (list prefix " - " label) label)]
         #:anchortitle [anchortitle label]
         #:linkname [linkname label]
         #:subtitle [subtitle #f]
         #:description [desc subtitle]
         #:in [param #f] ; parameter-like procedure to add new page into
         #:referrer [referrer #f]
         #:subpage-of [super #f]
         #:extra-headers [extra-headers #f]
         content)
  (define (page)
    (xhtml (head (title wintitle) page-header extra-headers)
           (body (make-navbar (or super this))
                 (h1 class: 'pagehead
                     doctitle (and subtitle (div class: 'subtitle subtitle)))
                 content
                 @*footer*)))
  (define this
    (resource/referrer
     file
     (file-writer output-xml page)
     (or referrer
         (λ (url #:sec [sec #f] #:get-desc [d? #f] . label)
           (if d?
             desc
             (a href: (if sec (list url "#" (section->label sec)) url)
                title: (cond [(and sec desc) @list{@desc, @sec}]
                             [sec @list{@linkname, @sec}]
                             [else desc])
                (if (null? label) (or sec linkname) label)))))))
  (when param (param (append (param) (list this))))
  (add-anchors! anchor-names this)
  (with-props this 'anchortitle anchortitle))

(define-syntax (filer stx)
  (syntax-case stx ()
    [(_ path x ...)
     (let* ([base (syntax-source #'path)]
            [base (let-values ([(dir name dir?) (split-path base)]) dir)]
            [base (datum->syntax #'path base)])
       (with-syntax ([base base])
         #'(filer* (path->complete-path path base) x ...)))]))

(define (filer* source
                #:path [path (let-values ([(dir name dir?)
                                           (split-path source)])
                               (path->string name))]
                #:referrer [referrer values]
                #:anchor-names [anchor-names '()])
  (define this
    (with-props (resource/referrer path
                                   (λ (target) (copy-file source target))
                                   referrer)
                'source source))
  (add-anchors! anchor-names this)
  this)

;; ----------------------------------------------------------------------------
;; Sections and similar constructs

;; Call this with a userid symbol, and the student information will be used
;; instead of a `Name:' field in exam forms, and in question/section headers
(provide student!)
(define (student! id)
  (*student-info* (list (big (big (tt (strong id))))
                        (user-substs id "{Full Name}")
                        (tt (user-substs id "<{Email}>")))))
(define *student-info* (make-parameter #f))

(define (header left #:right [right #f] #:label [label #f])
  (define name-style "text-align: right; font-size: 75%")
  (list (let ([si (*student-info*)])
          (and si (apply (λ (id name email) (div style: name-style name)) si)))
        (and label (a name: label ""))
        (h2 left (and right (div style: "float: right;"
                                 (span style: "font-size: 75%;" right))))))

;; make a `section' thing with a table of contents
(provide sections)
(define (make-sectioner newpages? this-page)
  (let ([sections '()])
    (values
     (λ (#:anchor-names [anchor-names #f] #:newpage [newpage? newpages?]
         #:extra-anchor-names [extra-anchors '()]
         . title)
       (define (->list x) (if (list? x) x (list x)))
       (define label (section->label (output->string title)))
       (define a-names
         (if anchor-names (->list anchor-names) (and this-page (list title))))
       (when a-names
         (if this-page
           (add-anchors! (append a-names (->list extra-anchors)) this-page
                         #:prefix (prop-ref this-page 'anchortitle)
                         #:anchor label)
           (error 'section "#:anchor-names used with an unknown page: ~s"
                  anchor-names)))
       (set! sections
             (cons (a href: (list "#" label) (entity 'rArr) nbsp title)
                   sections))
       (list (if newpages? (newpage-or-br) (list (br)))
             (header title #:label label)))
     (λ () (div class: 'tocbar (ul (map li (reverse sections))))))))
(define-syntax (sections stx)
  (define (make-it stx toc? newpages?)
    (with-syntax ([sec (datum->syntax stx 'section)]
                  [make #`(make-sectioner #,newpages? this-page)])
      (if toc?
        #'(begin (define-values [sec sections] make) sections)
        #'(define-values [sec sections] make))))
  (syntax-case stx ()
    [(s #:no-toc #:newpages) (make-it #'s #f #t)]
    [(s #:newpages #:no-toc) (make-it #'s #f #t)]
    [(s #:no-toc)            (make-it #'s #f #f)]
    [(s #:newpages)          (make-it #'s #t #t)]
    [(s)                     (make-it #'s #t #f)]
    [_ (identifier? stx)     (make-it stx #t #f)]))

;; same as sectioner, but for exam questions
(provide exam-front)
(define (make-exam-questioner)
  (define questions '())
  (define question-counter 0)
  (define (question title score . body)
    (let ([num (begin (set! question-counter (add1 question-counter))
                      question-counter)])
      (set! questions (cons (list title num score) questions))
      (list newpage-or-br
       (header (list "Question "num": "title)
               #:right (span style: '("color: white; background-color: black;"
                                      " white-space: nowrap;")
                         nbsp score " pts" nbsp)
               #:label num)
       br
       body)))
  (define (total-score) (apply + (map caddr questions)))
  (define (exam-front . body)
    (table align: 'center width: "96%" border: 0
      (tr (td nbsp))
      (tr (td align: 'center
            (cond [(*student-info*) =>
                   (λ (i)
                     (let-values ([(id name email) (apply values i)])
                       (table cellpadding: 10 cellspacing: 0
                              border: 1 bordercolor: 'black
                         (tr (td (div align: 'center id br name br email))))))]
                  [else (table width: "100%" border: 1 bordercolor: 'black
                               frame: 'below rules: 'none
                          (tr (td align: 'left "Name:")))])
            (br)
            (and (pair? body)
                 (div align: 'left (h2 "Administrative") (p body))))
          (td align: 'right
            (λ ()
              (table border: 1 bordercolor: "black" rules: 'rows
                     cellspacing: 0 cellpadding: 10
                (tr bgcolor: "#d0d0d0"
                  (td align: 'left "Question")
                  (td nbsp "Grade" nbsp)
                  (td nbsp))
                (map (λ (q)
                       (let ([title (car q)]
                             [num (cadr q)]
                             [score (caddr q)])
                         (if (eq? 'total title)
                           (tr bgcolor: "#d0d0d0"
                             (td align: 'right (b (i "Total")))
                             (td nbsp)
                             (td align: 'right "/" total-score))
                           (tr (td align: 'left style: "white-space: nowrap;"
                                 (a style: "text-decoration: none;"
                                    href: `("#",num) `("(",num") ",title)))
                               (td nbsp)
                               (td align: 'right "/" score)))))
                     (reverse (cons '(total #t #t) questions)))))))
      (tr (td nbsp))))
  (values question total-score exam-front))
(define-syntax (exam-front stx)
  (syntax-case stx ()
    [(s body ...)
     (with-syntax ([question    (datum->syntax #'s 'question)]
                   [total-score (datum->syntax #'s 'total-score)])
       #'(begin (define-values [question total-score exam-front]
                  (make-exam-questioner))
                (exam-front body ...)))]))

;; ----------------------------------------------------------------------------
;; Rendering & publishing

(define (rm path)
  (cond [(or (equal? "." path)
             (equal? (normalize-path path)
                     (normalize-path (current-directory))))
         (printf "Not removing ~a (same as current directory)\n" path)]
        [(directory-exists? path)
         (printf "Removing directory ~a\n" path)
         (delete-directory/files path)]
        [(file-exists? path)
         (printf "Removing file ~a\n" path)
         (delete-file path)]))

(define (rmd dir) (rm dir) (make-directory dir))

(define rsync
  (let ([rsync (find-executable-path "rsync" #f)])
    (define (->rsync-path x)
      (regexp-replace #rx"/?$" (->string x) "/"))
    (λ (from to)
      (printf "Copying ~a -> ~a\n" from to)
      (system* rsync "-aqze" "ssh" "--delete"
               (->rsync-path from) (->rsync-path to)))))

(provide (rename-out [module-begin #%module-begin]))
(define-syntax-rule (module-begin expr ...)
  (#%module-begin expr ... (run)))

(define (run)
  (command-line
    #:once-any
    [("+p" "++publish") "publish web pages"       (set! *publish?* #t)]
    [("-p" "--publish") "don't publish (default)" (set! *publish?* #f)])
  (unless *private-dir* (error "no working directory set"))
  (rmd *private-dir*)
  (parameterize ([current-directory *private-dir*]
                 [xml-writer markdown-writer])
    #; ; not needed -- all are resources
    (nest ([for ([file (in-list (directory-list *extras-dir*))])]
           [let ([name (path-element->string file)]
                 [source (build-path *extras-dir* file)])]
           [when (file-exists? source)])
      (resource name (λ (target) (copy-file source target))))
    (render-all)
    (make-epilogue))
  (when *publish?*
    (printf "Publishing...\n")
    (for ([dir (in-list (or *publish-dirs* '()))])
      (rsync *private-dir* (expand-user-path dir)))))
