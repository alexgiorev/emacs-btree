(load "~/.emacs.d/misc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree

(defun btree (cmp min-degree &optional root)
  (setq root (or root (btree--node)))
  ;; root min-degree cmp
  (record 'btree root min-degree cmp))

(defun btree-from-org-tree (cmp min-degree org-tree keyfunc)
  "Assumes that (org-kill-is-subtree-p ORG-TREE) and that there is only one
root. KEYFUNC is a function which accepts a string (the title of an org node)
and returns a list of keys. This function just parses ORG-TREE, it does not
ensure that the result will posses the B-tree properties. You can use
`btree-check' for that."
  (let* ((entries (btree--entries org-tree))
         (root (cdar entries))
         (tree (btree cmp min-degree root)))
    ;; at the end of this loop, ROOT will be the root and all parent/child
    ;; relationships will be formed
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

(defun btree--entries (org-tree)
  "Returns a list of entries of the form (DEPTH . BTREE-NODE).
Assumes that (org-kill-is-subtree-p ORG-TREE). At the time of call, `keyfunc'
stores the function which converts the title of an entry to a list of keys."
  (with-temp-buffer
    (org-mode)
    (insert org-tree)
    (let (entries)
      (setq extract-entry
            (lambda ()
              (let* ((components (org-heading-components))
                     (depth (nth 0 components))
                     (title (nth 4 components))
                     (keys (funcall keyfunc title))
                     (node (btree--node nil keys nil)))
                (push (cons depth node) entries))))
      (org-map-region extract-entry (point-min) (point-max))
      (reverse entries))))

(defun btree--org-ints (title)
  "Works on titles which are space-separated lists of integers. Returns that
list of integers."
  (read (concat "(" title ")")))

(defun btree-check (btree)
  "Checks if the BTREE is truly a B-tree, if it satisfies the properties which
define a B-tree as such."
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
  (btree--insert-org-tree-node (btree-root btree) 1))

(defun btree--insert-org-tree-node (node level)
  (insert (funcall node-to-org node level))
  (setq level (1+ level))
  (dolist (child (btree--node-children node))
    (btree--insert-org-tree-node child level)))

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

(defun btree-from-random-sequence (low high)
  "Creates the sequence [low...high], randomizes it and inserts each element
into a B-tree which is then returned."
  (let ((result (btree 'btree-cmp-int 10)))
    (dolist (key (shuffled-number-sequence low high))
      (btree-insert result key))
    result))

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