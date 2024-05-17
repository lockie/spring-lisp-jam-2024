;;;; This priority queue implementation is heavily based on
;;;; pettomato-indexed-priority-queue
;;;; Copyright (c) 2012 Austin Haas <austin@pettomato.com>
;;;; which is in turn based on code from Peter Norvig's AIMA book
;;;; Copyright (c) 1998-2002 by Peter Norvig.

(in-package #:prejam-2024)


(define-condition empty-queue-error (error)
  ()
  (:documentation "Signaled when an operation depends on a non-empty queue."))

(declaim (inline parent))
(defun parent (i) (declare (fixnum i)) (the fixnum (floor (- i 1) 2)))
(declaim (inline left))
(defun left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(declaim (inline right))
(defun right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap priorities i N)
  "Assume that the children of i are heaps, but that heap[i] may be
worse than its children. If it is, move heap[i] down where it
belongs. [Page 130 CL&R 2nd ed.]."
  (declare (type (simple-array fixnum) heap)
           (type (simple-array single-float) priorities)
           (type fixnum i N))
  (loop :with item :of-type fixnum := (aref heap i)
        :with item-priority :of-type single-float := (aref priorities i)
        :for l :of-type fixnum := (left i)
        :for r :of-type fixnum := (right i)
        :for best :of-type fixnum := i
        :for best-item :of-type fixnum := item
        :for best-priority :of-type single-float := item-priority
        :do (when (< l N)
              (let ((left-item (aref heap l))
                    (left-priority (aref priorities l)))
                (when (< left-priority item-priority)
                  (setf best l
                        best-item left-item
                        best-priority left-priority))))
           (when (< r N)
             (let ((right-item (aref heap r))
                   (right-priority (aref priorities r)))
               (when (< right-priority best-priority)
                 (setf best r
                       best-item right-item
                       best-priority right-priority))))
           (cond ((/= best i)
                  (setf (aref heap i) best-item
                        (aref priorities i) best-priority
                        i best))
                 (t
                  (setf (aref heap i) item
                        (aref priorities i) item-priority)
                  (return)))))

(declaim (ftype (function ((simple-array fixnum)
                           (simple-array single-float)
                           array-length))
                improve-key))
(defun improve-key (heap priorities i)
  "The item at i may be better than its parent. Promote the item until
it is in the correct position."
  (let ((item (aref heap i))
        (item-priority (aref priorities i)))
    (loop :for index :of-type fixnum := i :then parent-index
          :while (> index 0)
          :for parent-index :of-type fixnum := (parent index)
          :for parent :of-type fixnum := (aref heap parent-index)
          :while (< item-priority (aref priorities parent-index))
          :do
             (setf (aref heap index) parent
                   (aref priorities index) (aref priorities parent-index))
          :finally
             (setf (aref heap index) item
                   (aref priorities index) item-priority))))

(defstruct q
  (items nil :type (simple-array fixnum))
  (priorities nil :type (simple-array single-float))
  ;; actual number of elements; length might be bigger
  (size 0 :type array-length))

(declaim (inline make-queue))
(defun make-queue (items priorities)
  (make-q :items items :priorities priorities))

(declaim (inline queue-empty-p)
         (ftype (function (q) boolean) queue-empty-p))
(defun queue-empty-p (q)
  "Are there no items in the queue?"
  (zerop (q-size q)))

(declaim (ftype (function (q) fixnum) queue-pop))
(defun queue-pop (q)
  "Remove the element from the front of the queue and return
it. Signals empty-queue-error if the queue is empty. [Page 139 CL&R
2nd ed.]."
  (if (queue-empty-p q)
      (error 'empty-queue-error)
      (let* ((size (q-size q))
             (heap (q-items q))
             (priorities (q-priorities q))
             (min (aref heap 0)))
        (when (> size 1)
          (setf (aref heap 0) (aref heap (1- size)))
          (setf (aref priorities 0) (aref priorities (1- size))))
        (when (plusp size)
          (setf size (1- size)
                (q-size q) size))
        (heapify heap (q-priorities q) 0 size)
        min)))

(declaim (ftype (function (q fixnum single-float) q) queue-insert))
(defun queue-insert (q item priority)
  "Insert the item by priority according to the compare
function. Returns the queue. [Page 140 CL&R 2nd ed.]."
  (let ((heap (q-items q))
        (priorities (q-priorities q))
        (size (q-size q)))
    (when (>= size (length heap))
      (let ((new-size (ecs::new-capacity size)))
        (setf heap
              (ecs::adjust-array* heap
                                  new-size :element-type 'fixnum)
              (q-items q) heap
              priorities
              (ecs::adjust-array* priorities
                                  new-size :element-type 'single-float)
              (q-priorities q) priorities)))

    (setf (aref heap size) item
          (aref priorities size) priority)
    (improve-key heap priorities size)
    (incf (q-size q))
    q))
