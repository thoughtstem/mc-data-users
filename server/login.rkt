#lang at-exp racket

(provide login-page welcome-page
         try-login 
         logout!
	 current-user
	 requires-login
         change-password.json )

(require web-server/servlet
         (except-in website-js header select)
         mc-data/models
         mc-data/server/util
         web-server/http/id-cookie
	 net/uri-codec)

(define (requires-login request-handler)
  (lambda (req . rest)
    (if (not (current-user req))
	(login-page req)
        (apply request-handler req rest))))

(define (login-page req)
  (define user (current-user req))

  (if user
      (welcome! user)
      (response/html/content
	(container class: "m-5"
		   (h1 "Login")
		   (form action: "login" 'method: "POST"
			 (div
			   (label "Name"))
			 (div
			   (input type: "text" name: "name"))
			 (div
			   (label "Password"))
			 (div
			   (input type: "password" name: "password"))
			 (div
			   (button-primary type: "submit" "Login!")))))))

(define (try-login req)
  (define user
    (authenticate-user! req))

  (if user 
    (welcome! user)
    (response/html/content
      (container class: "m-5"
	(div "Denied")
	(div
	  (a href: "/login" "Try again?"))))))


(define (welcome-page req)
  (response/html/content
    (container class: "m-5"
               (card-group
                 (card
                   (card-body
                     (div "Welcome: "
                          (user-name (current-user req)))
                     (div
                       (a href: "/logout"
                          "Logout?"))))
                 (change-password-widget)
                 (card (card-header "Useful links") 
                       (card-body
                         (div (a href: "/people" "People"))
                         (div (a href: "/courses" "Courses"))
                         (div (a href: "/orders" "Orders"))
                         (div (a href: "/badges" "Badges"))
                         (div (a href: "/identities" "Identities"))))))))



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

(define (change-password-widget)
  (enclose
    (card (card-header "Change password?")
          (card-body
            (div
              (label "New Password"))
            (div
              (input id: (ns "password") type: "password" name: "password"))
            (div
              (button-primary on-click: (call 'try_change)
                "Change!")
              (span id: (ns "feedback")
                    ""))))
    (script ([password_field_id (ns "password")]
             [feedback_field_id (ns "feedback")])
            (function (try_change)
              @js{
                var password = @getEl{@password_field_id}.value;
                @getEl{@feedback_field_id}.innerHTML = "One moment..."
                fetch('/change-password.json', 
                      {method: "POST", 
                       headers: {
                         'Content-Type': 'application/json',
                       },
                       body: JSON.stringify({password: password})})
                  .then((response) => {
                    return response.json();
                  })
                  .then((myJson) => {
                    if(myJson){
                      @getEl{@feedback_field_id}.innerHTML = "Success!"
                    }else{
                      @getEl{@feedback_field_id}.innerHTML = "Something went wrong..."
                    }
                  });
              }))))







