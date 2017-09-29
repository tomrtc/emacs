#lang racket

(require (planet clements/sxml2:1:=3))
(require (planet jim/sxml-match:1:1/sxml-match))

(define bdd (ssax:xml->sxml
   (open-input-file "d:/Documents and Settings/tomaset1.AD2/Desktop/gmf/bdd.xml") '((cs . "http://www.genesyslab.com/cs" ))))


(define devices ((sxpath '(// CfgDN ) '(( cs . "cs"))) bdd))
(define apps ((sxpath '(// CfgServer) '(( cs . "cs"))) bdd))

(define bddnodn  bdd)
(define (nm x) (sxml:attr x 'name))
(define (dbid x) (sxml:attr x 'DBID))
(map nm apps)

;; (srl:sxml->xml bddnodn "d:/Documents and Settings/tomaset1.AD2/Desktop/gmf/bddnodn2.xml")

((sxpath "//CfgDN/userProperties/list_pair/str_pair/key" ) bdd)

((sxpath "//CfgDN/userProperties/list_pair[@key = 'TServer']" ) bdd)
((sxpath "//CfgDN[@DBID > 219]" '[(cs . "cs")])
 bdd)

(define dbids (map dbid devices))
(define max-dbid (foldl max 0 (map string->number dbids)))

(define withoutDN ((sxml:modify '("//CfgDN" delete)) bdd))
(define withouAgent ((sxml:modify '("//CfgAgent" delete)) withoutDN))

(srl:sxml->xml withouAgent "d:/Documents and Settings/tomaset1.AD2/Desktop/gmf/bddempty.xml")
