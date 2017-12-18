#||
CSCI 5511
programming assignment

Wei-Wei Wu
wuxx1045
5052973

12/05/17

9-puzzels problem
||#



;; empty space is 0
(defvar *E* 0)

;; representing the states a list of length 9 instead of 3x3 matrix.
;; using helper functions to infer where the columns and rows are
;; easier to do computations
(defvar *infeasible-state* '(1 1 1 1 1 1 1 1 0))

(defvar *starting-state* '(0 1 3 4 2 5 7 8 6))

(defvar *ending-state* '(1 2 3 4 5 6 7 8 0))

;; counter to keep track of how many nodes are expanded
(defvar *counter* 0)

;; helper function to run the whole problem
(defun run-puzzles (starting-state)
    (setq *counter* 0)
    (pretty-print-path (a-star-search starting-state)))

;; structure repsresenting a state node
;; stores state, G val, H val
(defstruct state-node state G H)

;; convert pos to row coord
(defun get-row (pos)
    (floor (/ pos 3.0)))

;; convert pos to col coord
(defun get-col (pos)
    (mod pos 3))

;; convert row col coords to pos. returns 0-9
(defun get-pos (row col)
    (+ (* 3 row) col))

;; check if a number is between 0 and 3
(defun between-0-3 (num)
    (and (>= num 0) (< num 3)))

;; substitute the nth value in list. returns a new list
(defun substitute-nth (val n list)
    (loop for i from 0 for j in list collect (if (= i n) val j)))

;; print a state (list)
(defun print-state (state)
  (loop for num in state do
      (format t "~d" num))
  (format t "~%"))

;; print a state node
(defun print-state-node (node)
    (print-state (state-node-state node))
    (format t "G: ~d~%" (state-node-G node))
    (format t "H: ~d~%" (state-node-H node)))

;; print a list of state nodes
(defun print-node-list (list)
    (loop for node in list do
        (print-state-node node)))

;; function to pretty print a state in a 3x3 matrix
(defun pretty-print (state)
    (loop for i from 0 to (- (length state) 1) do
        (if (zerop (nth i state))
            (format t "~s " 'E)
            (format t "~d " (nth i state)))
        (if (zerop (mod (+ i 1) 3))
            (format t "~%"))))

;; pretty print the whole path
(defun pretty-print-path (path)
    ;; cannot be solved if the path is nil
    (if (null path)
      (format t "Puzzle cannot be solved~%"))
    (setq reversed (reverse path))
    (loop for node in reversed do
        (pretty-print (state-node-state node))
        (terpri)))

;; get the minimun f val node in the queue
(defun get-minimum-node-index (queue)
    (setq min-val 10000000)
    (setq min-index 0)
    ;; go through every path in the queue
    (loop for i from 0 to (- (length queue) 1) do
        (setq path (nth i queue))
        (setq node (car path))
        (setq f-val (+ (state-node-G node) (state-node-H node)))
        (if (< f-val min-val)
            (progn
                (setq min-val f-val)
                (setq min-index i))))
    min-index)

;; remove the nth item in the list
;; return the new list
(defun remove-nth (n list)
    (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

;; check if a nested list contains a list using equals
(defun contains (test-state list-state)
    (loop for state in list-state do
        (if (equal test-state state)
            (return-from contains t)))
    nil)

;; heurstic function. Counts the number of mismatched cells
(defun heuristic (state)
    (setq count 0)
    (loop for i from 0 to (- (length state) 1) do
        (if (not (eq (nth i *ending-state*) (nth i state)))
            (setq count (+ 1 count))))
    count)

;; find the pos that is empy in the state returns 0-9
(defun find-empty-pos (state)
    (loop for i from 0 to (- (length state) 1) do
        (if (eq (nth i state) *E*)
            (return-from find-empty-pos i)))
    0)

;; get all successors of a node
(defun successors (node)
    (setq result nil)
    (setq empty-pos (find-empty-pos (state-node-state node)))
    (setq row (get-row empty-pos))
    (setq col (get-col empty-pos))

    ;; go in the four cardinal directions
    (loop for direction in '((1 0) (-1 0) (0 1) (0 -1)) do
        (setq new-row (+ row (car direction)))
        (setq new-col (+ col (cadr direction)))

        ;; if its valid
        (if (and (between-0-3 new-row) (between-0-3 new-col))
            (progn
                ;; this is pos that the E will swap with
                (setq new-empty-pos (get-pos new-row new-col))
                (setq val (nth new-empty-pos (state-node-state node)))
                ;; swap the values at those places
                (setq tmp-state (substitute-nth val empty-pos (state-node-state node)))
                (setq tmp-state (substitute-nth 0 new-empty-pos tmp-state))
                ;; create a new node with the new state
                ;; G is prev G + 1
                ;; H is the output of the heuristic function
                (setq new-node (make-state-node :state tmp-state :G (+ 1 (state-node-G node)) :H (heuristic tmp-state)))
                (push new-node result))))
    result)

;; a star search implementation using bfs
(defun a-star-search (start-state)
    (setq queue nil)
    (setq traversed nil)
    (setq root nil)
    ;; initial root node G=0
    (setq start-node (make-state-node :state start-state :G 0 :H (heuristic start-state)))
    (push start-state traversed)
    (push start-node root)
    (push root queue)

    ;; do the a star search while the queue is not epty
    (loop while (not (null queue)) do
        ;; current is the node in queue with the lowest f value
        (setq min-index (get-minimum-node-index queue))
        (setq current-path (nth min-index queue))
        (setq current-node (car current-path))
        ;; if current state is end-state
        ;; return the current node
        (if (equal *ending-state* (state-node-state current-node))
            (return-from a-star-search current-path))
        ;; remove current node from queue
        (setq queue (remove-nth min-index queue))
        ;; add the current state to traversed
        (push (state-node-state current-node) traversed)
        ;; increment counter
        (setq *counter* (+ 1 *counter*))
        ;; for each successor expand
        (loop for successor in (successors current-node) do
            ;; if it is not traversed
            (if (not (contains (state-node-state successor) traversed))
                (progn
                    ;; add this state to traversed
                    (push (state-node-state successor) traversed)
                    ;; clone the current node and add the successor to front
                    (setq tmp-path current-path)
                    (push successor tmp-path)
                    ;; add to queue
                    (push tmp-path queue)))))
    nil)

;; call a star and print the results
(run-puzzles *starting-state*)
(format t "Number of nodes expanded: ~d~%" *counter*)
(terpri)

(format t "infeasible puzzle~%")
(pretty-print *infeasible-state*)
(terpri)

(run-puzzles *infeasible-state*)
(terpri)
(format t "Number of nodes expanded: ~d~%" *counter*)
