#||
CSCI 5511
programming assignment

Wei-Wei Wu
wuxx1045
5052973

12/05/17

missionaries and cannibals problem
||#



; (defvar *actions* '((1 0 1) (0 1 1) (2 0 1) (0 2 1) (1 1 1)))


;; all possible actions for a boat of size 6
(defvar *actions* '((1 0 1) (2 0 1) (3 0 1) (4 0 1) (5 0 1) (6 0 1)
                    (0 1 1) (0 2 1) (0 3 1) (0 4 1) (0 5 1) (0 6 1)
                    (1 1 1) (2 2 1) (2 2 1)
                    (5 1 1) (4 2 1) (4 1 1) (3 2 1) (3 1 1) (2 1 1)))


;; ending state is where the left size is all zero.
;; same for all starting state
(defvar *ending-state* '(0 0 0))

;; add or subtract
;; initially we subtract
(defvar *subtract* t)

;; helper function to run the problem and print out the results
(defun run-cannibals (staring-state)
  (setq *subtract* t)
  (print-steps (breadth-first-search staring-state) staring-state))


;; print the state
(defun print-state (state)
    (format t "(~d ~d ~d)~%" (car state) (cadr state) (caddr state)))

;; print a list of states
(defun print-list (list)
    (loop for state in list do
        (print-state state))
    (terpri))

;; given the results. pretty print it to better visualize
(defun print-steps (result starting-state)
    (format t "Starting: left, m: ~d c:~d~%" (car starting-state) (cadr starting-state))
    (format t "Starting: right, m: ~d c:~d~%" (car *ending-state*) (cadr *ending-state*))
    (terpri)
    (setq total (car starting-state))
    (setq reversed (reverse result))
    (loop for i from 0 to (- (length reversed) 2) do
        (setq curr (nth i reversed))
        (setq next (nth (+ 1 i) reversed))
        ;; if we have the boat
        (if (not (zerop (caddr curr)))
            (format t "move ~d missionaries and ~d cannibals from left to right.~%" (- (car curr) (car next)) (- (cadr curr) (cadr next)))
            (format t "move ~d missionaries and ~d cannibals from right to left.~%" (- (car next) (car curr)) (- (cadr next) (cadr curr))))
        (format t "Result: left, m: ~d c: ~d~%" (car next) (cadr next))
        (format t "Result: right, m: ~d c: ~d~%" (- total (car next)) (- total (cadr next)))
        (terpri)))

;; check if this state is the end state (0, 0, 0)
(defun is-end-state (state)
    (loop for num in state do
        (if (not (zerop num))
            (return-from is-end-state nil)))
    t)

;; check that this state is between starting state or ending state
(defun is-valid (state starting-state)
    (setq zipped (mapcar #'list state starting-state *ending-state*))
    ;; if cannibal count is greater than missionary count
    ;; on left or right side this is an invalid state
    (setq total (car starting-state))
    (setq missionaries (car state))
    (setq cannibals (cadr state))
    (setq left-side (and (> cannibals missionaries) (not (zerop missionaries))))
    (setq right-side (and (> (- total cannibals) (- total missionaries)) (not (zerop (- total missionaries)))))
    (if (or left-side right-side)
        (return-from is-valid nil))
    (dolist (item zipped)
        ;; if it is greater than the starting state
        ;; or less than the ending state
        ;; this is an invalid state
        (cond ( (> (car item) (cadr item))
                (return-from is-valid nil))
              ( (< (car item) (caddr item))
                (return-from is-valid nil))))
    t)

;; generate the successor states
(defun successors (state starting-state)
    (setq result nil)
    (loop for action in *actions* do
        (if *subtract*
            (setq new-state (mapcar #'- state action))
            (setq new-state (mapcar #'+ state action)))
        (if (is-valid new-state starting-state)
            (push new-state result)))
    result)

;; check if a nested list contains a list using equals
(defun contains (test-state list-state)
    (loop for state in list-state do
        (if (equal test-state state)
            (return-from contains t)))
    nil)

;;; do a level order traversal of the tree
(defun breadth-first-search (start-state)
    ;; queue is a list of lists of traversals
    (setq queue nil)
    (setq traversed nil)
    (setq root nil)
    (push start-state traversed)
    (push start-state root)
    (push root queue)

    ;; do dfs until the queue is empty
    (loop while (not (null queue)) do

        ;; temp queue is empty
        (setq tmp-queue nil)

        ;; expand every node in the queue
        (loop for current-state in queue do
            ;; get the end element
            (setq end-element (car current-state))

            ;; compare to end state
            ;; if it is the end state then return the current state
            (if (is-end-state end-element)
                (return-from breadth-first-search current-state))

            ;; add all the valid successors in the queue
            (loop for successor in (successors end-element start-state) do

                ;; if its not in traversed
                (if (not (contains successor traversed))
                    (progn
                        ;; add it to traversed
                        (push successor traversed)
                        ;; add the new state to the tmp queue
                        (setq tmp-state current-state)
                        (push successor tmp-state)
                        (push tmp-state tmp-queue)))))
          ;; set the queue to the tmp queue
          (setq queue tmp-queue)

          ;; we alternate subtracting and adding
          (setq *subtract* (not *subtract*)))
    nil)

;; call the function on the starting state and print out the result
(format t "15 missionaries and 15 cannibals~%")
(defvar *starting-state* '(15 15 1))
(run-cannibals *starting-state*)

(terpri)
(terpri)
(terpri)

(format t "24 missionaries and 24 cannibals~%")
(setq *starting-state* '(24 24 1))
(run-cannibals *starting-state*)
