#lang racket

(provide current-user
         salt)


(require
  web-server/http/id-cookie
  mc-data/models)

;Check the login cookie
(define (current-user req)
  (define u
    (request-id-cookie req
                       #:name "current-user"
                       #:key (string->bytes/utf-8 
			       (salt))))

  (if (or (not u) 
	  (string=? u ""))
    #f
    (find-user-by-name u)))

(define (salt)
  (or (getenv "SECRET_SALT")
      "dev_salt"))


