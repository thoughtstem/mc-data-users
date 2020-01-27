#lang at-exp racket

(provide login-page
         denied-page
         welcome-page)

(require website-js
         (prefix-in model: mc-data/models)
         webapp/server/util 
         mc-data-users/server/util)

(define (login-page)
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
                     (button-primary type: "submit" "Login!")))))

(define (denied-page)
  (container class: "m-5"
             (div "Denied")
             (div
               (a href: "/login" "Try again?"))))

(define (welcome-page req)
  (container class: "m-5"
             (card-group
               (card
                 (card-body
                   (div "Welcome: "
                        (model:user-name (current-user req)))
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
                       (div (a href: "/identities" "Identities")))))))


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
              }    

                      ))))


