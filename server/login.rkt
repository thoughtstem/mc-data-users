#lang racket

(provide login-page welcome-page
         try-login 
         logout!
         change-password.json 
         requires-login)

(require web-server/servlet
         (except-in website-js header select)
         mc-data/models
         webapp/server/util
         web-server/http/id-cookie
	 net/uri-codec
         (prefix-in view: mc-data-users/views/main)
         mc-data-users/server/util)


(define (login-page req)
  (define user (current-user req))

  (if user
      (welcome! user)
      (response/html/content
	(view:login-page))))

(define (requires-login request-handler)
  (lambda (req . rest)
    (if (not (current-user req))
	(login-page req)
        (apply request-handler req rest))))


(define (try-login req)
  (define user
    (authenticate-user! req))

  (if user 
    (welcome! user)
    (response/html/content
      (view:denied-page))))


(define (welcome-page req)
  (response/html/content
    (view:welcome-page req)))


;Set the login cookie
(define (welcome! user)
  (define auth-cookie
    (make-id-cookie
      "current-user"
      (user-name user)
      #:key (string->bytes/utf-8 
              (salt))))

  (redirect-to
    "/welcome"
    see-other
    #:headers
    (list (cookie->header auth-cookie))))

(define (authenticate-user! req)
  (define posted-data
    (bytes->string/utf-8
      (request-post-data/raw req)))

  (define data-hash
    (apply hash
           (apply append
                  (map (curryr string-split "=")
                       (string-split posted-data
                                     "&")))))

  (define password
    (uri-decode 
      (hash-ref data-hash "password")))

  (define username
    (hash-ref data-hash "name"))

  (define user
    (find-user-by-name username))  

  (define authenticated?
    (and user
         (check-user-password 
           user    
           password)))

  ;TODO: Make a user more than just a name...
  (if authenticated?
    (find-user-by-name (hash-ref data-hash "name"))
    #f))

;Set the logout cookie
(define (logout! req)
  (define auth-cookie
    (make-id-cookie
      "current-user"
      ""
      #:key (string->bytes/utf-8 (salt))))

  (redirect-to
    "/login"
    see-other
    #:headers
    (list (cookie->header auth-cookie))))

(define (change-password.json req)
  (define user (current-user req))

  (define params (request->params req))
  (define json (hash-ref params 'json))
  (define new-password (hash-ref json 'password))

  (define new-user
    (change-user-password! user new-password))
  
  (if (and new-user (user? new-user))
    (response/jsexpr #t)
    (response/jsexpr #f)))









