; Nodes' struct
(defstruct h-node 
  (element nil :type t)
  (encoding nil :type (or null bit-vector))
  (weight 0 :type number)
  (left nil :type (or null h-node))
  (right nil :type (or null h-node)))

; Function to create Huffman Tree according to given paragraph
(defun huff-tree (seq &key)
  (multiple-value-bind (nodes queue)
    (huff-nodes seq)
    (do () ((endp (rest queue)) (values nodes (first queue)))
    (destructuring-bind (node1 node2 &rest queue-rest) queue
        (let ((node3 (make-h-node
                   :left node1
                   :right node2
                   :weight (+ (h-node-weight node1)
                              (h-node-weight node2)))))
    (setf queue (merge 'list (list node3) queue-rest '< :key 'h-node-weight))))
    )
  )
)

; Function to create Huffman Nodes with hash table according to given paragraph
(defun huff-nodes (seq &key)
  (let* ((length (length seq))
    (increment (/ 1 length))
    (nodes (make-hash-table :size length))
    (queue '()))
      (map nil #'(lambda (element)
                   (multiple-value-bind (node presentp) (gethash element nodes)
                     (if presentp
                       (incf (h-node-weight node) increment)
                       (let ((node (make-h-node :weight increment :element element)))
                         (setf (gethash element nodes) node
                               queue (list* node queue)))))) seq)
      (values nodes (sort queue '< :key 'h-node-weight))
    )
)

; Function to create Huffman Code according to given paragraph
(defun huff-code (seq &key)
  (multiple-value-bind (nodes tree)
    (huff-tree seq)
    (labels ((hc (node length bits)
                (let ((left (h-node-left node))
                     (right (h-node-right node)))
                (cond
                    ((and (null left) (null right))
                    (setf (h-node-encoding node)
                         (make-array length :element-type 'bit :initial-contents (reverse bits))))
                    (t (hc left (1+ length) (list* 0 bits))
                     (hc right (1+ length) (list* 1 bits)))))))
    (hc tree 0 '()) nodes)
  )
)

; Functions to split string with given seperators
(defun splitf (string &optional (separator sep) (cns nil)) 
  	(let ((n (position separator string
		:from-end t
        :test #'(lambda (x y)(find y x :test #'string=)))))
	    (if n
	      (splitf (subseq string 0 n) separator (cons (subseq string (1+ n)) cns))
	      (cons string cns))
  )
)

(defun splits (string &optional (separator sep))
  (splitf string separator)
)
 
; Function to delete first parameter of given text 
(defun del(txt) 
  (progn
   (setf txt (remove (aref (write-to-string txt ) 0) (write-to-string txt )))
   )
)

; Function to hash table to a list
(defun hash-to-list (tbl) 
  (let ((lst nil))
    (maphash (lambda (key val)
               (push (cons key val) lst))
             tbl)
    lst))

; Function to read contents from file
(defun read-contents (filename)
  (with-open-file (stream filename)
    (let ((cnt_fl (make-string (file-length stream))))
      (read-sequence cnt_fl stream) cnt_fl)))

; Function to contents into hash table then convert into association list
(defun sort-codes (nodes &optional)
  (setq table (make-hash-table))
  (loop for node being each hash-value of nodes
        do (setf(gethash (parse-integer (remove #\*(del(h-node-encoding node)))) table) (cadr(splits (del(h-node-element node)) "\\")))      
  )
  (setf malst (hash-to-list table))
  (sort malst (lambda (a b) (<(length(write-to-string(car a))) (length(write-to-string(car b)))))))

; Function to write huffman codes to the output file according to input file
(defun write-to-file ()
	(with-open-file 
	   (stream "huffman_codes.txt" ; Output file
	       :direction :output    
	       :if-exists :overwrite ; Overwrite if file exists
	       :if-does-not-exist :create) ; Create if file doesn't exist
	    (sort-codes(huff-code(read-contents "paragraph.txt")))
	    (loop for (a . b) in malst
	    do (format stream "~a: ~11t~a~%" b a))         
	)    
)

(write-to-file)