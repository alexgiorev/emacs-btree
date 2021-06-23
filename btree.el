(load "~/.emacs.d/misc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree

(defun btree (cmp min-degree &optional root)
  (setq root (or root (btree--node)))
  ;; root min-degree cmp
  (record 'btree root min-degree cmp))

(defun btree-from-org-tree (org-tree &optional keyfunc cmp min-degree)
  "Assumes that (org-kill-is-subtree-p ORG-TREE) and that there is only one
root. KEYFUNC is a function which is called when point is at the beginning of an
org node in the current buffer, and parses out the keys for the
node. `btree-from-org-tree' just parses ORG-TREE, it does not ensure that the
result will posses the B-tree properties. You can use `btree-check' for that."
  (setq cmp (or cmp btree--default-cmp)
        min-degree (or min-degree btree--default-min-degree)
        keyfunc (or keyfunc 'btree--org-read-sexp))
  (let* ((entries (btree--entries org-tree))
         (root (cdar entries))
         (tree (btree cmp min-degree root)))
    ;; at the end of this loop, ROOT will be the root and all parent/child
    ;; relationships between the nodes will be formed
    (while entries
      (let* ((entry (pop entries))
             (depth (car entry)) (node (cdr entry))
             (entries2 entries))
        (while (and entries2 (< depth (caar entries2)))
          ;; if we are positioned on a child of NODE...
          (if (= (caar entries2) (1+ depth))
              (btree--node-append-child node (cdar entries2)))
          ;; continue with next entry
          (setq entries2 (cdr entries2)))))
    tree))

(defun btree--org-read-sexp ()
  "Works on org entries whose title is an S-Expression"
  (read (nth 4 (org-heading-components))))

(defun btree--entries (org-tree)
  "Returns a list of entries of the form (DEPTH . BTREE-NODE).
Assumes that (org-kill-is-subtree-p ORG-TREE). At the time of call, `keyfunc'
stores the function which converts an entry to the corresponding B-tree node."
  (with-temp-buffer
    (org-mode)
    (insert org-tree)
    (let (entries)
      (setq extract-entry
            (lambda ()
              (let* ((components (org-heading-components))
                     (depth (nth 0 components))
                     (keys (funcall keyfunc))
                     (node (btree--node nil keys nil)))
                (push (cons depth node) entries))))
      (org-map-region extract-entry (point-min) (point-max))
      (reverse entries))))

(defun btree-depth-first-walk (btree-node-fn btree &optional return)
  "Traverses the tree in depth-first order and calls the function BTREE-NODE-FN
  on each node of BTREE. To end the walk prematurely, use (throw 'btree-end-walk
  value), in which case VALUE will be returned. If the walk is doesn't end
  prematurely, the RETURN argument will be the return value of the
  function. During the walk, BTREE-NODE-FN can access the current depth through
  the variable `btree-walk-current-depth'."
  (catch 'btree-end-walk
    (btree--walk-node (btree-root btree) 0)
    return))

(defun btree--walk-node (node btree-walk-current-depth)
  (funcall btree-node-fn node)
  (dolist (child (btree--node-children node))
    (btree--walk-node child (1+ btree-walk-current-depth))))

(defun btree-map-keys (btree-key-fn btree)
  "Goes through the keys of BTREE _in order_ and calls BTREE-KEY-FN on
each. BTREE-KEY-FN is expected to be a function which accepts a single (key)
argument. Its return value is ignored. This function expects taht BTREE is an
actual B-tree, in that it satisfies the B-tree properties, in particular that
the number of keys is one less the number of children."
  (btree--map-node-keys (btree-root btree)))

(defun btree--map-node-keys (node)
  (let ((keys (btree--node-keys node))
        (children (btree--node-children node)))
    (if (not children)
        (mapcar btree-key-fn keys)
      (while keys
        (btree--map-node-keys (car children))
        (btree-key-fn (car keys))
        (setq keys (cdr keys) children (cdr children)))
      ;; at this point `keys' is nil, but `children' has the last child, because
      ;; of the B-tree property that the number of keys is one less the number
      ;; of children. So all that is left to do is to visit the child.
      (btree--map-node-keys (car children)))))

(defun btree-check (btree)
  "Checks if the BTREE is truly a B-tree, if it satisfies the properties which
define a B-tree as such."
  (and (btree--check-depth btree)
       (btree--check-degrees btree)))

(defun btree--check-depth (btree)
  "Checks if all leaves of `btree' have the same depth"
  (let (; set by the first leaf encountered during the walk
        leaf-depth)
    (btree-depth-first-walk 'btree--check-leaf-depth btree t)))

(defun btree--check-leaf-depth (node)
  (if (btree--node-leafp node)
      (if leaf-depth
          ;; this is not the first leaf
          (if (/= leaf-depth btree-walk-current-depth)
              (throw 'btree-end-walk 'nil))
        ;; NODE is the first leaf encountered during this walk
        (setq leaf-depth btree-walk-current-depth))))

(defun btree--check-degrees (btree)
  "Checks if the degree of each non-root node is within the interval 
[MIN-DEGREE MAX-DEGREE], and if the root's degree does not exceed
MAX-DEGREE. There is also a check if the number of keys is one less than the
number of children."
  (let ((min-degree (btree-min-degree btree))
        (max-degree (btree-max-degree btree)))
    (btree-depth-first-walk 'btree--check-node-degree btree t)))

(defun btree--check-node-degree (node)
  "Assumes that when called `min-degree' and `max-degree' are set according to
the btree which is being traversed."
  (let ((min-degree (if (not (btree--node-parent node)) 1 min-degree))
        (degree (length (btree--node-children node)))
        (key-count (length (btree--node-keys node))))
    (unless (or (and (btree--node-leafp node)
                     (<= 1 key-count (1- max-degree)))
                (and (<= min-degree degree max-degree)
                     (= (length (btree--node-keys node)) (1- degree))))
      (throw 'btree-end-walk nil))))

(defun btree--check-order (btree)
  TODO)

;;;;;;;;;;;;;;;;;;;;
;; getters

(defun btree-root (btree)
  (aref btree 1))

(defun btree-set-root (btree root)
  (aset btree 1 root))

(defun btree-min-degree (btree)
  (aref btree 2))

(defun btree-max-degree (btree)
  (* 2 (btree-min-degree btree)))

(defun btree-min-keys (btree)
  (1- (btree-min-degree btree)))

(defun btree-max-keys (btree)
  (1- (btree-max-degree btree)))

(defun btree-cmp (btree)
  (aref btree 3))

;;;;;;;;;;;;;;;;;;;;
;; operations

(defun btree-insert (btree key)
  (let* ((cmp (btree-cmp btree))
         (node (btree--find-leaf btree key))
         parent)
    (btree--node-insert-key node key)
    (while (when (btree--node-overflowing-p node)
             (setq parent (btree--node-parent node))
             (btree--node-split node)
             (when parent (setq node parent))))))

(defun btree-insert-org-tree (btree &optional node-to-org)
  "Inserts into the current buffer at point an org-mode representation of
  BTREE. NODE-TO-ORG is a function which transforms a B-tree node into an
  org-mode node, but it doesn't do anything for the children."
  (setq node-to-org (or node-to-org btree--node-to-org))
  (btree-depth-first-walk 'btree--insert-org-node btree))

(defun btree--insert-org-node (node)
  (insert (funcall node-to-org node (1+ btree-walk-current-depth))))

(defun btree--node-to-org-print (node level)
  (concat (make-string level ?*)
          (format " %s\n"
                  (btree--node-keys node))))

(defvar btree--node-to-org 'btree--node-to-org-print
  "The default function which converts a B-tree node to an org-mode entry")

(defun btree--find-leaf (btree key)
  "Finds the leftmost leaf in BTREE into which KEY belongs. If KEY is in some
leaf, this will return the leftmost such leaf. Assumes `cmp' is set."
  (let ((node (btree-root btree)))
    (while (not (btree--node-leafp node))
      (setq node (btree--find-child node key)))
    node))

(defun btree-search (btree key)
  "Returns the first node in BTREE which has KEY"
  ;; not sure if this function is useful, so I won't do it now
  TODO)

(defun btree-delete (btree key)
  TODO)

(defun btree--find-child (node key)
  "Returns the child of NODE which we should visit to continue the search for
KEY. Assumes that at the time of call `cmp' stores the key comparison
function. If NODE is a leaf, returns `nil'"
  (unless (btree--node-leafp node)
    (let ((keys (btree--node-keys node))
          (children (btree--node-children node)))
      (while (and keys (eq (funcall cmp (car keys) key)'<))
        (setq keys (cdr keys)
              children (cdr children)))
      (car children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes. Many node functions assume that `btree' stores the B-tree which the
;; node is a part of. This is sensible because normally we call a node function
;; in the context of some tree.

(defun btree--node (&optional parent keys children)
  (record 'btree-node parent keys children))

(defun btree--node-parent (node)
  (aref node 1))

(defun btree--node-set-parent (node parent)
  (aset node 1 parent))

(defun btree--node-keys (node)
  (aref node 2))

(defun btree--node-set-keys (node keys)
  (aset node 2 keys))

(defun btree--node-children (node)
  (aref node 3))

(defun btree--node-set-children (node children)
  (aset node 3 children))

(defun btree--node-append-child (node child)
  (let ((children (btree--node-children node)))
    (if children
        (nconc children (list child))
      (btree--node-set-children node (list child)))
    (btree--node-set-parent child node)))

(defun btree--node-leafp (node)
  (not (btree--node-children node)))

(defun btree--node-depth (node)
  (let ((depth -1))
    (while node
      (setq depth (1+ depth))
      (setq node (btree--node-parent node)))
    depth))

(defun btree--node-insert-key (node key)
  "Inserts KEY into NODE's keys in the correct position.
Assumes that `cmp' is set."
  (let ((keys (btree--node-keys node))
        next-key)
    (if (not keys)
        (btree--node-set-keys node (list key))
      (if (memq (funcall cmp key (car keys)) '(< =))
          (btree--node-set-keys node (cons key keys))
        ;; at the end of this loop, `keys' will be the cell after which we
        ;; should insert KEY
        (while (and (setq next-key (cadr keys))
                    (eq (funcall cmp key next-key) '>))
          (setq keys (cdr keys)))
        (setcdr keys (cons key (cdr keys)))))))

(defun btree--node-overflowing-p (node)
  (>= (length (btree--node-keys node))
     (btree-max-degree btree)))

(defun btree--node-split (node)
  (let* ((parent (btree--node-parent node))
         (split-keys (btree--node-split-keys (btree--node-keys node)))
         (left-keys (car split-keys)) (right-keys (caddr split-keys))
         (parent-key (cadr split-keys))
         (split-children (btree--node-split-children
                          (btree--node-children node)))
         (left-children (car split-children))
         (right-children (cadr split-children))
         (left-node (btree--node nil left-keys left-children))
         (right-node (btree--node nil right-keys right-children)))
    (dolist (child left-children)
      (btree--node-set-parent child left-node))
    (dolist (child right-children)
      (btree--node-set-parent child right-node))
    ;; insert key and new children into parent. If the parent is a new node, set
    ;; it up as the root of the tree
    (if (not parent)
        (progn
          (setq parent
                (btree--node
                 nil (list parent-key) (list left-node right-node)))
          (btree-set-root btree parent))
      (btree--node-insert-key parent parent-key)
      (btree--node-split-replace-child parent node left-node right-node))
    ;; set parent of new children
    (btree--node-set-parent left-node parent)
    (btree--node-set-parent right-node parent)))

(defun btree--node-split-keys (keys)
  "Returns a list that represents the triple (left-child-keys parent-key
right-child-keys). Then KEYS has an even length, the left child will have one
more key than the right child."
  (let* ((length (length keys))
         (split-index (ceiling length 2))
         (left-split (butlast keys split-index))
         (right-split (last keys split-index)))
    (list left-split (car right-split) (cdr right-split))))

(defun btree--node-split-children (children)
  "Returns a list representing the pair (left-children right-children). When
CHILDREN has an odd length, the left list will have one more child than the
right one"
  (let* ((split-point (floor (length children) 2)))
    (list (butlast children split-point) (last children split-point))))
         

(defun btree--node-split-replace-child (node child left right)
  "Assumes that CHILD is a child of NODE. Removes CHILD from the children list
and inserts LEFT and RIGHT in its place."
  (let ((children (btree--node-children node)))
    (if (eq child (car children))
        (btree--node-set-children
         node (cons left (cons right (cdr children))))
      (while (not (eq (cadr children) child))
        (setq children (cdr children)))
      (setcdr children (cons left (cons right (cddr children)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comparison functions

(defun btree-cmp-int (int1 int2)
  (cond ((< int1 int2) '<)
        ((> int1 int2) '>)
        (t '=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(defun baddr (btree &rest indexes)
  "Easy access to nodes. For example, if you start from the root and want to
access the second child, then its third child, then its fifth child, you call
(baddr btree 1 2 4)"
  (let ((node (btree-root btree)))
    (dolist (index indexes)
      (setq node (nth index (btree--node-children node))))
    node))

(defun btree-from-random-sequence (low high &optional cmp min-degree)
  "Creates the sequence [low...high], randomizes it and inserts each element
into a B-tree which is then returned."
  (let* ((cmp (or cmp btree--default-cmp))
         (min-degree (or min-degree btree--default-min-degree))
         (result (btree cmp min-degree)))
    (dolist (key (shuffled-number-sequence low high))
      (btree-insert result key))
    result))

(defvar btree--default-cmp 'btree-cmp-int)
(defvar btree--default-min-degree 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some org-mode commands which are useful for my own experiments
(defun btree-ints-before ()
  "Counts the number of integers before point on the same line."
  (interactive)
  (save-excursion
    (let ((count 0)
          (limit (point)))
      (beginning-of-line)
      (while (re-search-forward "[[:digit:]]+" limit t)
        (setq count (1+ count)))
      (message "%s" count))))

(define-key org-mode-map "\C-c1" 'btree-ints-before)
