(defpackage #:wst.rate-limit.store
  (:use #:cl)
  (:documentation "Storage backend protocol for wst.rate-limit tracking.

Implementations specialise on the first argument (STORE) to provide pluggable
persistence for fixed-window counters.

The three operations a backend must implement are:

  • FETCH-WINDOW
    Retrieve the current window entry for a key.

    Syntax:
      (fetch-window store key)

    - STORE – The backend object.
    - KEY   – An arbitrary value identifying the rate-limit bucket (must be
              comparable with EQUAL or the backend's own equality test).

    Returns two values:
      1. COUNT      – The number of calls recorded in the current window, or
                      NIL if no entry exists for KEY.
      2. START-TIME – The universal-time timestamp when the current window
                      began, or NIL if no entry exists.


  • SAVE-WINDOW
    Persist (or overwrite) the window entry for a key.

    Syntax:
      (save-window store key count start-time)

    - STORE      – The backend object.
    - KEY        – The rate-limit bucket identifier.
    - COUNT      – The updated call count to store.
    - START-TIME – The universal-time timestamp when the window began.

    The return value is implementation-defined and should not be relied upon.


  • DELETE-WINDOW
    Remove the window entry for a key.

    Syntax:
      (delete-window store key)

    - STORE – The backend object.
    - KEY   – The rate-limit bucket identifier whose entry should be removed.

    The return value is implementation-defined and should not be relied upon.")
  (:export
   #:fetch-window
   #:save-window
   #:delete-window))

(in-package #:wst.rate-limit.store)

(defgeneric fetch-window (store key)
  (:documentation "Retrieves the current window state for KEY from STORE.

Returns two values: COUNT and START-TIME, or (NIL NIL) if no entry exists.

- STORE – The storage backend.
- KEY   – The rate-limit bucket identifier."))

(defgeneric save-window (store key count start-time)
  (:documentation "Persists the window state for KEY in STORE.

- STORE      – The storage backend.
- KEY        – The rate-limit bucket identifier.
- COUNT      – Number of calls recorded in the current window.
- START-TIME – Universal-time timestamp when the current window began."))

(defgeneric delete-window (store key)
  (:documentation "Removes the window entry for KEY from STORE.

- STORE – The storage backend.
- KEY   – The rate-limit bucket identifier to evict."))
