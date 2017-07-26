;;;
;;; Graph Bipartitioning
;;; Alice Forehand
;;;

(defvar pop-size) ;; for gls
(defvar p-mutate) ;; for ils
(defvar graph) ;; to be bound with edge lists parsed from text files
(defvar label) ;;for file naming
(defvar evals 0) ;; fitness evaluation counter
(defvar last-improvement 0) ;; gets the evaluation count when a solution improves to measure time of convergence
(defconstant runs 30) ;; how many times to run each algorithm
(defconstant max-swaps 50) ;; number of swaps without improvement allowed before a local optimum is declared
(defconstant max-iterations 100) ;; for metaheuristics
(defconstant part-size 250) ;; size of a partition
(defconstant pops '(25 50)) ;; list of population sizes for gls
(defconstant mutes '(0.01 0.03 0.05)) ;; list of mutation sizes for ils

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performs all runs with all settings;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-all ()
  (run-graphs 0))

(defun run-graphs (index)
  (unless (> index 1)
    (cond ((= index 1) 
	   (setq graph (make-graph "U500.txt"))
	   (setq label 'U500))
	  ((= index 0) 
	   (setq graph (make-graph "G500.txt"))
	   (setq label 'G500)))
    ;(run-mls)
    ;(run-ils 0)
    (run-gls 0)
    (run-graphs (+ index 1))))

(defun run-mls ()
  (let ((mls-solutions (mls 0 nil)))))
    ;(write-to-file (format nil "mls-solutions-~S.txt" label)
;		   (format nil "~S" mls-solutions)))); records the best solutions from each run

(defun run-ils (index)
  (when (< index (length mutes))
    (setq p-mutate (nth index mutes))
    (let ((ils-solutions (ils 0 nil)))
     ; (write-to-file (format nil "ils-solutions-p~S-~S.txt" p-mutate label) 
;		     (format nil "~S" ils-solutions)))
    (run-ils (+ index 1)))))

(defun run-gls (index)
  (when (< index (length pops))
    (setq pop-size (nth index pops))
    (let ((gls-solutions (gls 0 nil)))
     ; (write-to-file (format nil "gls-solutions-pop~S-~S.txt" pop-size label) 
;		     (format nil "~S" gls-solutions)))
    (run-gls (+ index 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performs local search ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun local-search ()
  (local-search-aux (make-solution) 0)) ;enters the local search loop with a random solution


(defun local-search-aux (old-solution no-improvement)
  (cond ((= no-improvement max-swaps) old-solution) ;if an improvement hasn't been found in a while, finish
	(t
	 (let* ((element-a (nth (random part-size) (first old-solution))) ;swaps single elements between partitions
		(element-b (nth (random part-size) (second old-solution)))
		(new-solution (swap-sublists (list element-a) (list element-b) old-solution)))
	   (setq new-solution (make-pretty new-solution nil)) ;evaluates fitness and sorts the elements of the partitions
	   (cond ((<= (third new-solution) (third old-solution)) ;if the new solution has fewer cut edges
		  (local-search-aux new-solution 0))             ;continue searching from it
		 (t                                                          ;otherwise
		  (local-search-aux old-solution (+ no-improvement 1)))))))) ;try again with the old solution

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generates a random solution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-solution ()
  (let* ((indeces (index-list (* part-size 2))) ;makes a list whose elements are vertecies
	 (solution (sublist part-size indeces))) ;splits the vertex list into two random subsets
    (append solution (list (fitness solution 0))))) ;sorts and evaluates fitness

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaulates the fitness of the solution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fitness (solution fitness)
  (setq evals (+ evals 1))
  (cond ((equal (first solution) nil) fitness)
	(t
	 (let ((edges (second (nth (- (first (first solution)) 1) graph)))) ;finds the list of edges for a given vertex
	   (fitness (cons (rest (first solution)) (rest solution))
		    (+ fitness (fitness-aux (second solution) edges 0))))))) ;fitness is the sum of the cuts

(defun fitness-aux (partition edges cuts)
  (cond ((equal edges nil) cuts)
	((member (first edges) partition) ;if the members of the edge list of a partition-a vertex are members of partition-b
	 (fitness-aux partition (rest edges) (+ cuts 1))) ;increase the cut count
	(t
	 (fitness-aux partition (rest edges) cuts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performs genetic local search ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gls (index final-solutions)
  (setq evals 0)
  (setq last-improvement 0)
  (cond ((= index runs) final-solutions)
	(t
	 (gls (+ index 1) (append final-solutions (list (gls-aux (populate nil) 0 nil)))))))

(defun gls-aux (population iteration best-solutions)
  (cond ((= iteration max-iterations) 
	 (let ((content (list (third (first population)) last-improvement)))
	   (append-file (format nil "gls-solutions-pop~S-~S.txt" pop-size label)
			(format nil "~S" content))))
	(t
	 (sort-by-fitness population)
	 (let ((child (crossover (first (sublist 2 population))))) ; crossover on two random parents
	   (setq child (append child (list (fitness child 0)))) ;evaluate fitness
	   (setq child (local-search-aux child 0)) ;perform local search
	   (cond ((and (<= (third child) 
			   (third (nth (- (length population) 1) population))) ;if child is more fit than least fit solution
		       (not-duplicate child population))                       ;and is not already present
		  (setq last-improvement evals)                                ;update last-improvement
		  (gls-aux (cons child (butlast population))                   ;insert child and remove least fit solution
			   (+ iteration 1)
			   (append best-solutions (list (first population))))) 
		 (t
		  (gls-aux population 
			   (+ iteration 1)
			   (append best-solutions (list (first population))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generates a population ;;
;; of local optima        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun populate (population)
  (cond ((= (length population) pop-size) population)
	(t
	 (populate (append population (list (local-search)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; performs crossover ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun crossover (parents)
  (cond ((>= (length (intersection (first (first parents))            ;if the first partition of the
				   (first (second parents))))         ;parents have more than half 
	     part-size)                                               ;their elements in common
	 (crossover-aux (intersection (first (first parents))         ;preserve all common 
				      (first (second parents)))       ;elements during crossover
			(intersection (second (first parents))        ;and randomly divide the
				      (second (second parents)))      ;remaining elements between
			(set-exclusive-or (first (first parents))     ;the children
					  (first (second parents)))))
	(t
	 (crossover-aux (intersection (first (first parents))         ;otherwise do the same 
				      (second (second parents)))      ;switching partitions
			(intersection (second (first parents)) 
				      (first (second parents)))
			(set-exclusive-or (first (first parents)) 
					  (second (second parents)))))))

(defun crossover-aux (child-a child-b to-be-divided) ;randomly divides the unshared elements between partitions
  (let ((divided (sublist (floor (/ (length to-be-divided) 2)) to-be-divided)))
    (list (append child-a (first divided)) (append child-b (second divided)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns true if the child is   ;;
;; not a member of the population ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun not-duplicate (child population)
  (cond ((equal population nil) T)
	((equal (first child) (first (first population))) NIL)  ;checks if the solution is already in the population
	((equal (first child) (second (first population))) NIL) ;even in opposite partitions
	(t
	 (not-duplicate child (rest population)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterated local search ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ils (index final-solutions)
  (setq evals 0)
  (setq last-improvement 0)
  (cond ((= index runs) final-solutions)
	(t
	 (ils (+ index 1) (append final-solutions (list (ils-aux (local-search) 0 nil)))))))

(defun ils-aux (old-solution iteration best-solutions)
  (cond ((= iteration max-iterations)
	 (let ((content (list (third old-solution) last-improvement)))
	   (append-file (format nil "ils-solutions-p~S-~S.txt" p-mutate label)
			   (format nil "~S" content))))
	(t
	 (let* ((swaps (get-swaps old-solution)) ;pieces to perturbate
		(new-solution (swap-sublists (first swaps) (second swaps) old-solution))) ;perturbation step 
	   (setq new-solution (append new-solution (list (fitness new-solution 0)))) ;fitness
	   (setq new-solution (local-search-aux new-solution 0)) ;local search 
	   (cond ((< (third new-solution) (third old-solution)) ;replace old solution if better
		  (setq last-improvement evals)                 ;update last-improvement
		  (ils-aux new-solution 
			   (+ iteration 1)
			   (append best-solutions (list new-solution))))
		 (t
		  (ils-aux old-solution 
			   (+ iteration 1)
			   (append best-solutions (list old-solution)))))))))
  
(defun get-swaps (old-solution) ;gets a random list of elements to be swapped
  (list (first (sublist (floor (* p-mutate part-size)) (first old-solution)))
	(first (sublist (floor (* p-mutate part-size)) (second old-solution)))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-start local search ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mls (index final-solutions)
  (format t "index: ~S~%" index)
  (setq evals 0)
  (cond ((= index runs) final-solutions)
	(t
	 (mls (+ index 1) (append final-solutions (list (mls-aux nil 0)))))))

(defun mls-aux (solutions iteration)
  (format t "iteration:~S~%" iteration)
  (cond ((= iteration max-iterations)
	 (sort-by-fitness solutions)
	 (let ((content (list (third (first solutions)) evals)))
	   (append-file (format nil "mls-solutions-~S.txt" label)
			   (format nil "~S" content))))
	(t
	 (mls-aux (append solutions (list (local-search))) (+ iteration 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gets the solution in the proper format ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pretty (old-solution new-solution)
  (setq new-solution (append new-solution (list (sort (copy-list (first old-solution)) #'<)))) ;destructive functions are stupid
  (setq new-solution (append new-solution (list (sort (copy-list (second old-solution)) #'<))))
  (append new-solution (list (fitness new-solution 0))))

;;;;;;;;;;;;;;;;;;;;;;;
;; sublist generator ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sublist (n L)
  (sublist-helper n nil L)) ;returns a list containing n randomly selected elements of list L

(defun sublist-helper (n new-list L)
  (cond ((= n 0)  (list new-list L))
	(t
	 (let ((index (random (length L))))
	   (sublist-helper (- n 1) 
			   (cons (nth index L) new-list)
			   (append (subseq L 0 index) (subseq L (+ index 1))))))))

;;;;;;;;;;;;;;;;;;;;
;; swaps sublists ;;
;;;;;;;;;;;;;;;;;;;;

(defun swap-sublists (swap-a swap-b old-solution)
  (append (list (swap-sublist swap-b swap-a (first old-solution)))
	  (list (swap-sublist swap-a swap-b (second old-solution)))))

(defun swap-sublist (swap-in swap-out list)
  (append swap-in (remove-if #'(lambda (x) (member x swap-out)) list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; index list generator ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun index-list (length) 
  (index-list-aux length nil)) ;generates a list of indeces

(defun index-list-aux (length indeces)
  (cond ((= length 0) indeces)
	(t
	 (index-list-aux (- length 1) (cons length indeces)))))

;;;;;;;;;;;;
;; sorter ;;
;;;;;;;;;;;;

(defun sort-by-fitness (L)
  (sort L #'< :key #'third))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; writes data to file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-to-file (name content)
  (with-open-file (stream (format nil (concatenate 'string "C:/Users/Sagan/Desktop/asdf/" name))
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create)
    (format stream content))
  name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appends data to a file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun append-file (name content)
  (with-open-file (stream (format nil (concatenate 'string "C:/Users/Sagan/Desktop/asdf/" name))
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
    (format stream content))
  name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parses a graph string into an edge list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-graph (name)
  (let ((data (get-graph name))) ;gets string
    (setq data (subseq data 0 (position #\e data))) ;removes the end of file junk
    (setq data (split-by-one-space data)) ;delimits the string by spaces
    (setq data (remove-if #'(lambda (x) (= (length x) 0)) data)) ;removes empty strings made by the previous step
    (make-graph-aux data nil nil)))

(defun make-graph-aux (data data-list vertex) ;turns delimited string into a list of verteces and edges
  (cond ((eq data nil) data-list)
	((eq (char (first data) 0) #\()
	 (let* ((edge-list (fill-edges (rest (rest data)) vertex nil)))
	   (make-graph-aux (subseq data (+ (length (second edge-list)) 2))
			   (append data-list (list edge-list))
			   vertex)))
	(t
	 (make-graph-aux (rest data) data-list (list (parse-integer (first data)))))))


(defun fill-edges (data vertex edge-list) ;makes a list of a vertex and its edges
  (let ((string-length (length (first data))))
    (cond ((eq data nil)
	   (append vertex (list edge-list)))
	  ((eq (char (first data) 0) #\Newline) 
	   (append vertex (list (list nil))))
	  ((eq (char (first data) (- string-length 1)) #\Newline)
	   (setq edge-list (append edge-list (list (parse-integer (subseq (first data) 0 (- string-length 1))))))
	   (append  vertex (list edge-list)))
	  (t
	   (fill-edges (rest data) vertex (append edge-list (list (parse-integer (first data)))))))))


(defun get-graph (name) ;retrieves graph string from text file
  (with-open-file (stream (format nil (concatenate 'string "C:/Users/Sagan/Desktop/asdf/" name))
			  :direction :input)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun split-by-one-space (string) ;delimits string with spaces
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))
