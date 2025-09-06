(DEFPACKAGE #:WST.HTTP
  (:USE #:CL))

(IN-PACKAGE :WST.HTTP)

(DEFVAR +HTTP-STATUS-100+ 100)

(EXPORT '+HTTP-STATUS-100+)

(DEFVAR HTTP-STATUS-100
  '(100 "Continue"
    "The server has received the request headers and the client should proceed to send the request body."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/100"))

(EXPORT 'HTTP-STATUS-100)

(DEFVAR +HTTP-STATUS-101+ 101)

(EXPORT '+HTTP-STATUS-101+)

(DEFVAR HTTP-STATUS-101
  '(101 "Switching Protocols"
    "The server agrees to switch protocols as requested by the client."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/101"))

(EXPORT 'HTTP-STATUS-101)

(DEFVAR +HTTP-STATUS-102+ 102)

(EXPORT '+HTTP-STATUS-102+)

(DEFVAR HTTP-STATUS-102
  '(102 "Processing"
    "The server is processing the request but no response is available yet."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/102"))

(EXPORT 'HTTP-STATUS-102)

(DEFVAR +HTTP-STATUS-103+ 103)

(EXPORT '+HTTP-STATUS-103+)

(DEFVAR HTTP-STATUS-103
  '(103 "Early Hints"
    "Used to return some response headers before the final HTTP message."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/103"))

(EXPORT 'HTTP-STATUS-103)

(DEFVAR +HTTP-STATUS-200+ 200)

(EXPORT '+HTTP-STATUS-200+)

(DEFVAR HTTP-STATUS-200
  '(200 "OK" "The request has succeeded."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/200"))

(EXPORT 'HTTP-STATUS-200)

(DEFVAR +HTTP-STATUS-201+ 201)

(EXPORT '+HTTP-STATUS-201+)

(DEFVAR HTTP-STATUS-201
  '(201 "Created"
    "The request has been fulfilled, resulting in the creation of a new resource."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/201"))

(EXPORT 'HTTP-STATUS-201)

(DEFVAR +HTTP-STATUS-202+ 202)

(EXPORT '+HTTP-STATUS-202+)

(DEFVAR HTTP-STATUS-202
  '(202 "Accepted"
    "The request has been accepted for processing, but processing is not complete."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/202"))

(EXPORT 'HTTP-STATUS-202)

(DEFVAR +HTTP-STATUS-203+ 203)

(EXPORT '+HTTP-STATUS-203+)

(DEFVAR HTTP-STATUS-203
  '(203 "Non-Authoritative Information"
    "The server successfully processed the request but is returning information from another source."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/203"))

(EXPORT 'HTTP-STATUS-203)

(DEFVAR +HTTP-STATUS-204+ 204)

(EXPORT '+HTTP-STATUS-204+)

(DEFVAR HTTP-STATUS-204
  '(204 "No Content"
    "The server successfully processed the request but is not returning any content."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/204"))

(EXPORT 'HTTP-STATUS-204)

(DEFVAR +HTTP-STATUS-205+ 205)

(EXPORT '+HTTP-STATUS-205+)

(DEFVAR HTTP-STATUS-205
  '(205 "Reset Content"
    "The server successfully processed the request but requires the client to reset the document view."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/205"))

(EXPORT 'HTTP-STATUS-205)

(DEFVAR +HTTP-STATUS-206+ 206)

(EXPORT '+HTTP-STATUS-206+)

(DEFVAR HTTP-STATUS-206
  '(206 "Partial Content"
    "The server is delivering only part of the resource due to a range header sent by the client."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/206"))

(EXPORT 'HTTP-STATUS-206)

(DEFVAR +HTTP-STATUS-300+ 300)

(EXPORT '+HTTP-STATUS-300+)

(DEFVAR HTTP-STATUS-300
  '(300 "Multiple Choices"
    "The request has multiple possible responses, and the user or agent must choose one."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/300"))

(EXPORT 'HTTP-STATUS-300)

(DEFVAR +HTTP-STATUS-301+ 301)

(EXPORT '+HTTP-STATUS-301+)

(DEFVAR HTTP-STATUS-301
  '(301 "Moved Permanently"
    "The requested resource has been permanently moved to a new URL."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/301"))

(EXPORT 'HTTP-STATUS-301)

(DEFVAR +HTTP-STATUS-302+ 302)

(EXPORT '+HTTP-STATUS-302+)

(DEFVAR HTTP-STATUS-302
  '(302 "Found"
    "The requested resource resides temporarily under a different URL."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/302"))

(EXPORT 'HTTP-STATUS-302)

(DEFVAR +HTTP-STATUS-303+ 303)

(EXPORT '+HTTP-STATUS-303+)

(DEFVAR HTTP-STATUS-303
  '(303 "See Other"
    "The response to the request can be found under a different URL using a GET method."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/303"))

(EXPORT 'HTTP-STATUS-303)

(DEFVAR +HTTP-STATUS-304+ 304)

(EXPORT '+HTTP-STATUS-304+)

(DEFVAR HTTP-STATUS-304
  '(304 "Not Modified"
    "The resource has not been modified since the last request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/304"))

(EXPORT 'HTTP-STATUS-304)

(DEFVAR +HTTP-STATUS-305+ 305)

(EXPORT '+HTTP-STATUS-305+)

(DEFVAR HTTP-STATUS-305
  '(305 "Use Proxy" "The requested resource must be accessed through a proxy."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/305"))

(EXPORT 'HTTP-STATUS-305)

(DEFVAR +HTTP-STATUS-307+ 307)

(EXPORT '+HTTP-STATUS-307+)

(DEFVAR HTTP-STATUS-307
  '(307 "Temporary Redirect"
    "The requested resource resides temporarily under a different URL, and the original method should be used."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/307"))

(EXPORT 'HTTP-STATUS-307)

(DEFVAR +HTTP-STATUS-308+ 308)

(EXPORT '+HTTP-STATUS-308+)

(DEFVAR HTTP-STATUS-308
  '(308 "Permanent Redirect"
    "The requested resource has been permanently moved to a new URL, and the original method should be used."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/308"))

(EXPORT 'HTTP-STATUS-308)

(DEFVAR +HTTP-STATUS-400+ 400)

(EXPORT '+HTTP-STATUS-400+)

(DEFVAR HTTP-STATUS-400
  '(400 "Bad Request"
    "The server cannot process the request due to a client error."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/400"))

(EXPORT 'HTTP-STATUS-400)

(DEFVAR +HTTP-STATUS-401+ 401)

(EXPORT '+HTTP-STATUS-401+)

(DEFVAR HTTP-STATUS-401
  '(401 "Unauthorized" "The request requires user authentication."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/401"))

(EXPORT 'HTTP-STATUS-401)

(DEFVAR +HTTP-STATUS-402+ 402)

(EXPORT '+HTTP-STATUS-402+)

(DEFVAR HTTP-STATUS-402
  '(402 "Payment Required"
    "Reserved for future use, often associated with payment requirements."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/402"))

(EXPORT 'HTTP-STATUS-402)

(DEFVAR +HTTP-STATUS-403+ 403)

(EXPORT '+HTTP-STATUS-403+)

(DEFVAR HTTP-STATUS-403
  '(403 "Forbidden"
    "The client does not have permission to access the requested resource."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/403"))

(EXPORT 'HTTP-STATUS-403)

(DEFVAR +HTTP-STATUS-404+ 404)

(EXPORT '+HTTP-STATUS-404+)

(DEFVAR HTTP-STATUS-404
  '(404 "Not Found" "The server cannot find the requested resource."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/404"))

(EXPORT 'HTTP-STATUS-404)

(DEFVAR +HTTP-STATUS-405+ 405)

(EXPORT '+HTTP-STATUS-405+)

(DEFVAR HTTP-STATUS-405
  '(405 "Method Not Allowed"
    "The request method is not supported for the requested resource."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/405"))

(EXPORT 'HTTP-STATUS-405)

(DEFVAR +HTTP-STATUS-406+ 406)

(EXPORT '+HTTP-STATUS-406+)

(DEFVAR HTTP-STATUS-406
  '(406 "Not Acceptable"
    "The server cannot produce a response matching the client's Accept headers."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/406"))

(EXPORT 'HTTP-STATUS-406)

(DEFVAR +HTTP-STATUS-407+ 407)

(EXPORT '+HTTP-STATUS-407+)

(DEFVAR HTTP-STATUS-407
  '(407 "Proxy Authentication Required"
    "The client must authenticate with a proxy before the request can proceed."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/407"))

(EXPORT 'HTTP-STATUS-407)

(DEFVAR +HTTP-STATUS-408+ 408)

(EXPORT '+HTTP-STATUS-408+)

(DEFVAR HTTP-STATUS-408
  '(408 "Request Timeout"
    "The server timed out waiting for the client's request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/408"))

(EXPORT 'HTTP-STATUS-408)

(DEFVAR +HTTP-STATUS-409+ 409)

(EXPORT '+HTTP-STATUS-409+)

(DEFVAR HTTP-STATUS-409
  '(409 "Conflict"
    "The request could not be completed due to a conflict with the current state of the resource."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/409"))

(EXPORT 'HTTP-STATUS-409)

(DEFVAR +HTTP-STATUS-410+ 410)

(EXPORT '+HTTP-STATUS-410+)

(DEFVAR HTTP-STATUS-410
  '(410 "Gone"
    "The requested resource is no longer available and will not be available again."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/410"))

(EXPORT 'HTTP-STATUS-410)

(DEFVAR +HTTP-STATUS-411+ 411)

(EXPORT '+HTTP-STATUS-411+)

(DEFVAR HTTP-STATUS-411
  '(411 "Length Required"
    "The server requires a Content-Length header in the request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/411"))

(EXPORT 'HTTP-STATUS-411)

(DEFVAR +HTTP-STATUS-412+ 412)

(EXPORT '+HTTP-STATUS-412+)

(DEFVAR HTTP-STATUS-412
  '(412 "Precondition Failed"
    "One or more conditions in the request header fields evaluated to false."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/412"))

(EXPORT 'HTTP-STATUS-412)

(DEFVAR +HTTP-STATUS-413+ 413)

(EXPORT '+HTTP-STATUS-413+)

(DEFVAR HTTP-STATUS-413
  '(413 "Payload Too Large"
    "The request entity is larger than the server is able to process."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/413"))

(EXPORT 'HTTP-STATUS-413)

(DEFVAR +HTTP-STATUS-414+ 414)

(EXPORT '+HTTP-STATUS-414+)

(DEFVAR HTTP-STATUS-414
  '(414 "URI Too Long"
    "The URI requested by the client is too long for the server to handle."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/414"))

(EXPORT 'HTTP-STATUS-414)

(DEFVAR +HTTP-STATUS-415+ 415)

(EXPORT '+HTTP-STATUS-415+)

(DEFVAR HTTP-STATUS-415
  '(415 "Unsupported Media Type"
    "The media format of the requested data is not supported by the server."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/415"))

(EXPORT 'HTTP-STATUS-415)

(DEFVAR +HTTP-STATUS-416+ 416)

(EXPORT '+HTTP-STATUS-416+)

(DEFVAR HTTP-STATUS-416
  '(416 "Range Not Satisfiable"
    "The range specified in the request's Range header cannot be fulfilled."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/416"))

(EXPORT 'HTTP-STATUS-416)

(DEFVAR +HTTP-STATUS-417+ 417)

(EXPORT '+HTTP-STATUS-417+)

(DEFVAR HTTP-STATUS-417
  '(417 "Expectation Failed"
    "The server cannot meet the requirements of the Expect request-header field."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/417"))

(EXPORT 'HTTP-STATUS-417)

(DEFVAR +HTTP-STATUS-418+ 418)

(EXPORT '+HTTP-STATUS-418+)

(DEFVAR HTTP-STATUS-418
  '(418 "I'm a teapot"
    "The server refuses to brew coffee because it is a teapot (April Fools' joke)."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/418"))

(EXPORT 'HTTP-STATUS-418)

(DEFVAR +HTTP-STATUS-421+ 421)

(EXPORT '+HTTP-STATUS-421+)

(DEFVAR HTTP-STATUS-421
  '(421 "Misdirected Request"
    "The request was directed to a server that is not able to produce a response."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/421"))

(EXPORT 'HTTP-STATUS-421)

(DEFVAR +HTTP-STATUS-422+ 422)

(EXPORT '+HTTP-STATUS-422+)

(DEFVAR HTTP-STATUS-422
  '(422 "Unprocessable Entity"
    "The request was well-formed but unable to be processed due to semantic errors."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/422"))

(EXPORT 'HTTP-STATUS-422)

(DEFVAR +HTTP-STATUS-423+ 423)

(EXPORT '+HTTP-STATUS-423+)

(DEFVAR HTTP-STATUS-423
  '(423 "Locked" "The resource being accessed is locked."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/423"))

(EXPORT 'HTTP-STATUS-423)

(DEFVAR +HTTP-STATUS-424+ 424)

(EXPORT '+HTTP-STATUS-424+)

(DEFVAR HTTP-STATUS-424
  '(424 "Failed Dependency"
    "The request failed because it depended on another request that failed."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/424"))

(EXPORT 'HTTP-STATUS-424)

(DEFVAR +HTTP-STATUS-425+ 425)

(EXPORT '+HTTP-STATUS-425+)

(DEFVAR HTTP-STATUS-425
  '(425 "Too Early"
    "The server is unwilling to process a request that might be replayed."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/425"))

(EXPORT 'HTTP-STATUS-425)

(DEFVAR +HTTP-STATUS-426+ 426)

(EXPORT '+HTTP-STATUS-426+)

(DEFVAR HTTP-STATUS-426
  '(426 "Upgrade Required"
    "The client must switch to a different protocol, such as TLS/1.0."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/426"))

(EXPORT 'HTTP-STATUS-426)

(DEFVAR +HTTP-STATUS-428+ 428)

(EXPORT '+HTTP-STATUS-428+)

(DEFVAR HTTP-STATUS-428
  '(428 "Precondition Required"
    "The server requires the request to be conditional."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/428"))

(EXPORT 'HTTP-STATUS-428)

(DEFVAR +HTTP-STATUS-429+ 429)

(EXPORT '+HTTP-STATUS-429+)

(DEFVAR HTTP-STATUS-429
  '(429 "Too Many Requests"
    "The client has sent too many requests in a given amount of time."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/429"))

(EXPORT 'HTTP-STATUS-429)

(DEFVAR +HTTP-STATUS-431+ 431)

(EXPORT '+HTTP-STATUS-431+)

(DEFVAR HTTP-STATUS-431
  '(431 "Request Header Fields Too Large"
    "The server is unwilling to process the request because its header fields are too large."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/431"))

(EXPORT 'HTTP-STATUS-431)

(DEFVAR +HTTP-STATUS-451+ 451)

(EXPORT '+HTTP-STATUS-451+)

(DEFVAR HTTP-STATUS-451
  '(451 "Unavailable For Legal Reasons"
    "The resource is unavailable due to a legal demand."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/451"))

(EXPORT 'HTTP-STATUS-451)

(DEFVAR +HTTP-STATUS-500+ 500)

(EXPORT '+HTTP-STATUS-500+)

(DEFVAR HTTP-STATUS-500
  '(500 "Internal Server Error"
    "The server encountered an unexpected condition that prevented it from fulfilling the request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/500"))

(EXPORT 'HTTP-STATUS-500)

(DEFVAR +HTTP-STATUS-501+ 501)

(EXPORT '+HTTP-STATUS-501+)

(DEFVAR HTTP-STATUS-501
  '(501 "Not Implemented"
    "The server does not support the functionality required to fulfill the request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/501"))

(EXPORT 'HTTP-STATUS-501)

(DEFVAR +HTTP-STATUS-502+ 502)

(EXPORT '+HTTP-STATUS-502+)

(DEFVAR HTTP-STATUS-502
  '(502 "Bad Gateway"
    "The server, acting as a gateway, received an invalid response from an upstream server."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/502"))

(EXPORT 'HTTP-STATUS-502)

(DEFVAR +HTTP-STATUS-503+ 503)

(EXPORT '+HTTP-STATUS-503+)

(DEFVAR HTTP-STATUS-503
  '(503 "Service Unavailable"
    "The server is temporarily unable to handle the request due to maintenance or overloading."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/503"))

(EXPORT 'HTTP-STATUS-503)

(DEFVAR +HTTP-STATUS-504+ 504)

(EXPORT '+HTTP-STATUS-504+)

(DEFVAR HTTP-STATUS-504
  '(504 "Gateway Timeout"
    "The server, acting as a gateway, did not receive a timely response from an upstream server."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/504"))

(EXPORT 'HTTP-STATUS-504)

(DEFVAR +HTTP-STATUS-505+ 505)

(EXPORT '+HTTP-STATUS-505+)

(DEFVAR HTTP-STATUS-505
  '(505 "HTTP Version Not Supported"
    "The server does not support the HTTP protocol version used in the request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/505"))

(EXPORT 'HTTP-STATUS-505)

(DEFVAR +HTTP-STATUS-506+ 506)

(EXPORT '+HTTP-STATUS-506+)

(DEFVAR HTTP-STATUS-506
  '(506 "Variant Also Negotiates"
    "The server has an internal configuration error related to content negotiation."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/506"))

(EXPORT 'HTTP-STATUS-506)

(DEFVAR +HTTP-STATUS-507+ 507)

(EXPORT '+HTTP-STATUS-507+)

(DEFVAR HTTP-STATUS-507
  '(507 "Insufficient Storage"
    "The server is unable to store the representation needed to complete the request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/507"))

(EXPORT 'HTTP-STATUS-507)

(DEFVAR +HTTP-STATUS-508+ 508)

(EXPORT '+HTTP-STATUS-508+)

(DEFVAR HTTP-STATUS-508
  '(508 "Loop Detected"
    "The server detected an infinite loop while processing the request."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/508"))

(EXPORT 'HTTP-STATUS-508)

(DEFVAR +HTTP-STATUS-510+ 510)

(EXPORT '+HTTP-STATUS-510+)

(DEFVAR HTTP-STATUS-510
  '(510 "Not Extended"
    "Further extensions to the request are required for the server to fulfill it."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/510"))

(EXPORT 'HTTP-STATUS-510)

(DEFVAR +HTTP-STATUS-511+ 511)

(EXPORT '+HTTP-STATUS-511+)

(DEFVAR HTTP-STATUS-511
  '(511 "Network Authentication Required"
    "The client needs to authenticate to gain network access."
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/511"))

(EXPORT 'HTTP-STATUS-511)
