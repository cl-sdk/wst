(defpackage #:wst.rate-limit.memory-store
  (:use #:cl)
  (:documentation "Built-in in-memory storage backend for wst.rate-limit.

Provides MEMORY-STORE, a hash-table-backed implementation of the
wst.rate-limit.store protocol (fetch-window / save-window / delete-window).
Suitable for single-process use; not thread-safe.")
  (:import-from #:wst.rate-limit.store
                #:fetch-window
                #:save-window
                #:delete-window)
  (:export
   #:memory-store))

(in-package #:wst.rate-limit.memory-store)

(defclass memory-store ()
  ((table :initform (make-hash-table :test #'equal)
          :reader memory-store-table))
  (:documentation "A simple in-memory rate-limit store backed by a hash table.
Suitable for single-process use; not thread-safe."))

(defmethod wst.rate-limit.store:fetch-window ((store memory-store) key)
  "Returns (values count start-time) if an entry for KEY exists, (values nil nil) otherwise."
  (let ((entry (gethash key (memory-store-table store))))
    (if entry
        (values (car entry) (cdr entry))
        (values nil nil))))

(defmethod wst.rate-limit.store:save-window ((store memory-store) key count start-time)
  "Stores COUNT and START-TIME for KEY in the hash table."
  (setf (gethash key (memory-store-table store)) (cons count start-time)))

(defmethod wst.rate-limit.store:delete-window ((store memory-store) key)
  "Removes the entry for KEY from the hash table."
  (remhash key (memory-store-table store)))
