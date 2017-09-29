#! /usr/bin/env racket

#lang racket
(require racket/date)
(require racket/cmdline)
(require racket/file)
(require racket)
(require (planet clements/sxml2:1:=3))
(require (planet jim/sxml-match:1:1/sxml-match))


(define verbose? (make-parameter #f))
(define device-range (make-parameter 9))
(define device-pfx (make-parameter 10000))

(define bdd-file
  (command-line
   #:once-each
   [("-v") "Verbose mode" (verbose? #t)]
   [("-r") 
    n
    "Create <n> devices range."
    (device-range (string->number n))]
   [("-p") 
    n
    "Set device prefix (eg: 10000)."
    (device-pfx (string->number n))]
   #:args
   (str) str))


(when (verbose?) (printf "file=~a range=~a ~a\n" bdd-file (device-range) " : verbose mode."))



(define bdd (ssax:xml->sxml
   (open-input-file bdd-file) '()))




(define xml-temp (make-temporary-file "~a.xml"))
(define xml-port (open-output-file xml-temp #:exists 'replace))



(define dblist (map cadr ((sxpath "//@DBID") bdd)))
(define max-dbid (foldl max 0 (map string->number dblist)))
(define min-dbid (foldl min 999999 (map string->number dblist)))

(define (mgetdbid)
  (let ((mdbid max-dbid))
    (lambda ()
      (set! mdbid (+ 1 mdbid))
      (number->string mdbid))))
(define getdbid (mgetdbid))




(define (make-place place-name place-id place-dbid dn-id)
  `(CfgPlace
    (@
     (state "1")
     (ownerDBID "CfgTenant101")
     (name ,place-name)
     (id ,place-id)
     (folderDBID "CfgFolder164")
     (DBID ,place-dbid))
    (DNDBIDs (CfgPlaceDN (@ (linkDBID ,dn-id))))))

 (define (make-agent agent-name agent-id agent-dbid agent-lastname agent-firstname employee-id place-id dn-name)
  `(CfgAgent
    (@
     (state "1")
     (placeDBID ,place-id)
     (ownerDBID "CfgTenant101")
     (name ,agent-name)
     (lastName ,agent-lastname)
     (isAgent "2")
     (id ,agent-id)
     (folderDBID "CfgFolder162")
     (firstName ,agent-lastname)
     (employeeID ,employee-id)
     (DBID ,agent-dbid))
    (userProperties
     (list_pair
      (@ (key "ess-attributes"))
      (str_pair (@ (value "TRUNK_2401_OXE") (key "ess-associated-pbx-trunk")))
      (str_pair (@ (value "0") (key "ess-busy-threshold")))
      (str_pair (@ (value "ess-lock-profile-basic") (key "ess-lock-profile")))
      (str_pair (@ (value ,agent-name) (key "ess-namepart-uri")))
      (str_pair (@ (value "1001") (key "ess-organization-upper-level-id")))
      (str_pair (@ (value "15") (key "ess-presentation-timer"))))
     (list_pair
      (@ (key "ess-routing-rules"))
      (str_pair
       (@
        (value
         ,(format "<rule active=\"true\" type=\"PRESENTATION\" id=\"1\"><actionPart><routeTo type=\"PRESENTATION\" forking=\"PARALLEL\"><dest dn=\"~a\"/></routeTo></actionPart></rule>" dn-name))
        (key "ess-routing-rule-0")))))))


 (define (make-DN dn-name dn-id dn-dbid)
  `(CfgDN
    (@
     (useOverride "1")
     (type "1")
     (switchSpecificType "1")
     (state "1")
     (routeType "1")
     (registerAll "2")
     (ownerDBID "CfgSwitch101")
     (name ,dn-name)
     (id ,dn-id)
     (folderDBID "CfgFolder208")
     (DBID ,dn-dbid))
    (userProperties
     (list_pair (@ (key "ess-attributes")) (str_pair (@ (value "REGULAR-SIP") (key "ess-extension-subtype"))))
     (list_pair
      (@ (key "TServer"))
      (str_pair (@ (value "register, invite, publish, subscribe, message") (key "authenticate-requests")))
      (str_pair (@ (value "") (key "contact")))
      (str_pair (@ (value "false") (key "dual-dialog-enabled")))
      (str_pair (@ (value "123456") (key "password")))
      (str_pair (@ (value "true") (key "refer-enabled")))
      (str_pair (@ (value "true") (key "ring-tone-on-make-call")))
      (str_pair (@ (value "2") (key "sip-busy-type")))
      (str_pair (@ (value "talk,hold") (key "sip-cti-control")))
      (str_pair (@ (value "true") (key "sip-enable-moh")))
      (str_pair (@ (value "2") (key "sip-replaces-mode")))
      (str_pair (@ (value "true") (key "transfer-complete-by-refer")))
      (str_pair (@ (value "true") (key "use-contact-as-dn")))))))

 
(define (add-one number)
  (let*
      ([dn-number (+ 1000 (device-pfx) number)]
       [dn-DBID (getdbid)]
       [dn-id (format "CfgDN~a" dn-DBID)]
       [dn-name (format "~a" dn-number)]
       [agent-DBID (getdbid)]
       [agent-id (format "CfgAgent~a" agent-DBID)]
       [agent-name (format "ESSuser~a" number)]
       [employee-id (format "~a" (+ (device-pfx) number))]
       [agent-lastname (format "L~a" number)]
       [agent-firstname (format "F~a" number)]
       [place-DBID (getdbid)]
       [place-id (format "CfgPlace~a" place-DBID)]
       [place-name (format "Place~a" dn-number)])
    (list (make-place place-name place-id place-DBID dn-id)
          (make-DN dn-name dn-id dn-DBID)
          (make-agent agent-name agent-id agent-DBID agent-lastname agent-firstname employee-id place-id dn-name))))
    




(define new-items
  (map add-one (stream->list (in-range 0 (device-range) 1))))
(define new-places (append '("//CfgPlace[last()]" insert-following) (map car new-items)))
(define new-DNs (append '("//CfgDN[last()]" insert-following) (map cadr new-items)))
(define new-agents (append '("//CfgAgent[last()]" insert-following) (map caddr new-items)))

(define (add-dn  y)
  ((sxml:modify new-DNs) y))
(define (add-places y)
  ((sxml:modify new-places) y))
(define (add-agents y)
  ((sxml:modify new-agents) y))

(srl:sxml->xml  (add-places (add-agents (add-dn bdd))) xml-port)
(when (verbose?) (printf "~a => ~a DBID(~a/~a/~a)\n" bdd-file xml-temp min-dbid max-dbid (getdbid))) 