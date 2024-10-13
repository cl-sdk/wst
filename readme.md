# wst (web server tools)

Tools to help you build a web application in Common Lisp.

### available systems

#### wst.routing

Main system. Manages the routes used in a application.

```lisp
(add-route 'something "/something" :GET (lambda () "something"))
(add-route 'post-something "/something/:number" :POST (lambda () "something"))
(remove-route 'something)

(condition-handler (lambda (request response err) nil))

;; anything else that is a GET request is handled like this.
(any-route-handler :GET (lambda (request reponse) nil))

(route-uri-of (find-route-by-name 'post-something) (list 1))
;; => "/something/1"
```

#### wst.routing.dsl

A dsl to build passes (filters and middlewares).

```lisp
(defparameter authentication 
  `(wrap
    :before check-if-user-already-authenticated
    :route (group
            (route :post api-sign-up "/sign-up" api-sign-up-controller)
            (route :post api-log-in "/log-in" api-log-in-controller)
            (route :post api-log-out "/log-out" api-log-out-controller))))

(defparameter users
  `(group
    (wrap
     :before add-session-csrf
     :route (group
             (route :POST create-user "/user" create-user-responder)
             (route :PATCH edit-user "/user/:user-id" edit-user-responder)))
    (route :GET find-user "/user/:user-id" find-user-responder)
    (route :DELETE delete-user "/user/:user-id" delete-user-responder)
    (route :GET list-account-users "/users" list-account-users-responder)))

(defparameter groups 
  `(group
    (wrap
     :before add-session-csrf
     :route (group
             (route :POST create-group "/group" create-group-responder)
             (route :PATCH edit-group "/group/:group-code" edit-group-responder))
     (route :GET find-group "/group/:group-code" find-group-responder)
     (route :DELETE delete-group "/group/:group-code" delete-group-responder)
     (route :GET list-account-groups "/groups" list-account-groups-responder))))

(defparameter api
  `(wrap
    :before (list acquire-request-connection manage-session)
    :after (list http-response-cookies->set-cookie release-request-connection)
    :route (resource "/api/v1"
                     ,authentication
                     (wrap
                      :before ,(require-authorization (niav.configurations:jwt-public-key))
                      :route (group ,users ,groups)))))
```

This will build a list of routes in the following form.

```lisp
#<route uri=/api/v1/sign-up
        method=:POST
        callback=(compose acquire-request-connection
                          manage-session
                          check-if-user-already-authenticated
                          api-sign-up-controller
                          http-response-cookies->set-cookie
                          release-request-connection)>
```

# license

Unlicense.

See [license](https://github.com/cl-sdk/wst/blob/development/license).
