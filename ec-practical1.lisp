;;;
;;;
;;; 22/12/12
;;; executes 50 runs of each parameter setting for a given varied parameter
;;; 22/12/12
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings key:                  ;;
;;  functions:                    ;;
;;   0: uniform-co                ;;
;;   1: linear-co                 ;;
;;   2: tight-deceptive-tf        ;;
;;   3: rand-deceptive-tf         ;;
;;   4: tight-non-deceptive-tf    ;;
;;   5: rand-non-deceptive-tf     ;;
;;  parameter settings:           ;;
;;   a: vary population size      ;;
;;   b: vary pc                   ;;
;;   c: vary tournament sizes     ;;
;;   d: vary crossover type       ;;
;;   e: vary maximum iterations   ;;
;;   f: run default settings      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant runs 1)
(defconstant default-pop 500)
(defconstant default-pc 1)
(defconstant default-tourney 2)
(defconstant default-cross '2-point)
(defconstant default-iterations 250)

(defun run-all-settings ()
  (let ((funcs '(4 5))
	(params '(d)))
    (iterate-funcs funcs params 0)))

(defun iterate-funcs (funcs params index)
  (cond ((= index (length funcs)) (print "FINISHED"))
	(t
	 (iterate-params (nth index funcs) params 0)
	 (iterate-funcs funcs params (+ index 1)))))

(defun iterate-params (func params index)
  (cond ((= index (length params)) (format t "function ~S finished.~%" func))
	(t
	 (test-settings func (nth index params))
	 (format t "parameter setting ~S finished.~%" (nth index params))
	 (iterate-params func params (+ index 1)))))

(defun test-settings (func param)
  (let ((fit-funcs '(uniform-co 
		     linear-co 
		     tight-deceptive-tf 
		     rand-deceptive-tf 
		     tight-non-deceptive-tf 
		     rand-non-deceptive-tf))
	(param-alist '((a . (300 350 400)); I altered these values 
		       (b . (0 0.5 1)) ;depending on what I was testing
		       (c . (1 2 5))
		       (d . (2-point uniform)) 
		       (e . (50 100 200 500 1000)) 
		       (f . (1))))
	(optimum 100))
    (cond ((= func 1) (setf optimum 5050))
	  (t (setf optimum 100)))
    (test-settings-aux param (nth func fit-funcs) (rest (assoc 'a param-alist)) 
		       (rest (assoc 'b param-alist)) (rest (assoc 'c param-alist))
		       (rest (assoc 'd param-alist)) (rest (assoc 'e param-alist))
		       optimum (length (rest (assoc param param-alist))) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runs GA with varied settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-settings-aux (param fit-func pops pc tourney-sizes cross-types 
			  max-iterations optimum index solutions)
  (cond ((= index 0) (write-to-file (format nil "~S-~S.txt" fit-func param) 
				    (format nil "~S" solutions)))
	(t
	 (cond ((equal param 'a) 
		(setf solutions 
		      (append (list (run param 
					 fit-func 
					 (nth (- (length pops) index) pops) 
					 default-pc 
					 default-tourney 
					 default-cross
					 default-iterations 
					 optimum 
					 runs 
					 nil)) 
			      solutions)))

	       ((equal param 'b) 
		(setf solutions 
		      (append (list (run param 
					 fit-func 
					 default-pop
					 (nth (- (length pc) index) pc) 
					 default-tourney 
					 default-cross
					 default-iterations 
					 optimum 
					 runs 
					 nil)) 
			      solutions)))

	       ((equal param 'c) 
		(setf solutions 
		      (append (list (run param 
					 fit-func 
					 default-pop
					 default-pc
					 (nth (- (length tourney-sizes) index) tourney-sizes) 
					 default-cross
					 default-iterations 
					 optimum 
					 runs 
					 nil)) 
			      solutions)))

	       ((equal param 'd) 
		(setf solutions 
		      (append (list (run param 
					 fit-func 
					 default-pop
					 default-pc 
					 default-tourney 
					 (nth (- (length cross-types) index) cross-types) 
					 default-iterations 
					 optimum
					 runs
					 nil)) 
			      solutions)))

	       ((equal param 'e) 
		(setf solutions 
		      (append (list (run param 
					 fit-func 
					 default-pop 
					 default-pc 
					 default-tourney
					 default-cross
					 (nth (- (length max-iterations) index) max-iterations) 
					 optimum
					 runs 
					 nil))
			      solutions)))

	       ((equal param 'f) 
		(setf solutions 
		      (append (list (run param 
					 fit-func 
					 default-pop 
					 default-pc 
					 default-tourney
					 default-cross
					 default-iterations
					 optimum
					 runs 
					 nil))
			      solutions))))
	 (test-settings-aux param fit-func pops pc tourney-sizes cross-types 
			    max-iterations optimum (- index 1) solutions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; executes runs with the given parameter setting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run (param fitness-function population-size pc tournament-size 
	    crossover-type max-iterations optimum number solutions)
  (let ((population (populate population-size))); generates the population
    (cond ((= number 0) solutions); terminates when all runs complete
	  (t
	   (setf population (run-loop param; enters a single run
				      fitness-function 
				      population-size 
				      pc 
				      tournament-size 
				      crossover-type 
				      optimum 
				      max-iterations 
				      population 
				      number))
	   (run param 
		fitness-function 
		population-size 
		pc 
		tournament-size 
		crossover-type
		max-iterations 
		optimum 
		(- number 1)
		(cons (get-stats population) solutions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; executes a single run ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-loop (param fitness-function population-size pc tournament-size 
		 crossover-type optimum max-iterations population print-run)
  (fitness fitness-function population); evaluates fitness
  (sort-by-fitness population); sorts population by ascending fitness
  (when (= print-run 1) 
    (append-file (format nil "~S-~S-single-run.txt" fitness-function param) 
		 (format nil "~S" (get-stats population)))); records the data of one run
  (cond ((= (second (nth (- population-size 1) population)) optimum) 
	 population); terminates when optimum is found
	((= max-iterations 0) 
	 population); terminates when the maximum iterations is reached 
	(t
	 (let* ((parents (tournament population tournament-size))
		(child-strings (crossover parents pc crossover-type)))
	   (mutate child-strings pc)
	   (run-loop param fitness-function population-size pc tournament-size 
		     crossover-type optimum (- max-iterations 1) child-strings print-run)))))

;;;;;;;;;;;;;;;;
;; population ;;                         
;;;;;;;;;;;;;;;;

(defun populate (size)
  (populate-aux size nil))

(defun populate-aux (size population)
  (cond ((= size 0) population)
	(t (populate-aux (- size 1) (cons (individual) population)))))

;;;;;;;;;;;;;;;;
;; individual ;;
;;;;;;;;;;;;;;;;

(defun individual ()
  (individual-aux 100 nil))

(defun individual-aux (length string)
  (cond ((= length 0) string)
	(t
	 (individual-aux (- length 1) (cons (random 2) string)))))

;;;;;;;;;;;;;
;; fitness ;;
;;;;;;;;;;;;;

(defun fitness (func population)
  (fitness-aux func population 0))

(defun fitness-aux (func population index)
  (cond ((= index (length population)) population)
	(t
	 (setf (nth index population) (funcall func (nth index population) 0 0))
	 (fitness-aux func population (+ index 1)))))

(defun uniform-co (individual fitness index)
  (cond ((= index (length individual)) (list individual fitness))
	(t
	 (uniform-co individual (+ fitness (nth index individual)) (+ index 1)))))

(defun linear-co (individual fitness index)
  (cond ((= index (length individual)) (list individual fitness))
	(t
	 (linear-co individual (+ fitness (* (+ index 1) (nth index individual))) (+ index 1)))))

(defun tight-deceptive-tf (individual fitness index)
  (cond ((= index (length individual)) (list individual fitness))
	(t
	 (tight-deceptive-tf individual 
		       (+ fitness (trap-aux (subseq individual index (+ index 4)) 4 1))
		       (+ index 4)))))

(defun rand-deceptive-tf (individual fitness index)
  (declare (ignore index))
  (list individual (rand-deceptive-tf-aux individual fitness)))

(defun rand-deceptive-tf-aux (individual fitness)
  (cond ((= (length individual) 0) fitness)
	(t
	 (let ((lists (sublist 4 individual)))
	   (rand-deceptive-tf-aux (second lists) 
				    (+ fitness (trap-aux (first lists) 4 1)))))))

(defun tight-non-deceptive-tf (individual fitness index)
  (cond ((= index (length individual)) (list individual fitness))
	(t
	 (tight-non-deceptive-tf individual 
		       (+ fitness (trap-aux (subseq individual index (+ index 4)) 4 2.5))
		       (+ index 4)))))

(defun rand-non-deceptive-tf (individual fitness index)
  (declare (ignore index))
  (list individual (rand-non-deceptive-tf-aux individual fitness)))

(defun rand-non-deceptive-tf-aux (individual fitness)
  (cond ((= (length individual) 0) fitness)
	(t
	 (let ((lists (sublist 4 individual)))
	   (rand-non-deceptive-tf-aux (second lists) 
				    (+ fitness (trap-aux (first lists) 4 2.5)))))))
(defun trap-aux (bits k d)
  (cond ((= (second (uniform-co bits 0 0)) k) k)
	(t (- k d (* (/ (- k d) (- k 1)) (second (uniform-co bits 0 0)))))))

;;;;;;;;;;;;;;;;
;; tournament ;;
;;;;;;;;;;;;;;;;

;; returns a list of (length population) parents generated by 
;; size tournament selection  
(defun tournament (population size)
  (tournament-aux population size nil))

;; makes sublists of length size of the population, sorts by fitness
;; and adds the last (most fit) member to the list of parents
;; until the parents list is the same length as the population list
(defun tournament-aux (population size parents)
  (cond ((= (length population) (length parents)) parents)
	(t (tournament-aux
	    population 
	    size 
	    (cons (nth (- size 1) (sort-by-fitness (first (sublist size population)))) 
		  parents)))))

;;;;;;;;;;;;;;;
;; crossover ;;
;;;;;;;;;;;;;;;

;; selects pairs of parents and performs crossover with pc probability
;; returns a list of child bitstrings to be mutated
;; I just go through parents in order since it was randomly generated
;; type is either '2-point or 'uniform
(defun crossover (parents pc type)
  (funcall type parents pc nil))

;; performs 2-point crossover on pairs of parents with pc probability
;; adds pairs of parents to child-strings uncrossed with 1-pc probability
(defun 2-point (parents pc child-strings)
  (cond ((null parents) child-strings)
	(t 
	 (let* ((string1 (first (first parents)))
		(string2 (first (second parents)))
		(point1 (+ (random (- (length string1) 2)) 1))  ; this is so subseq called in
		(point2 (+ (random (- (length string1) 2)) 1))  ; 2-point-aux is never nil
		(new-strings (2-point-aux string1 string2 (min point1 point2) 
					  (max point1 point2))))
	   (cond ((<= (random 1.0) pc)
		  (2-point (rest (rest parents)) 
			   pc 
			   (append child-strings new-strings)))
		 (t 
		  (2-point (rest (rest parents)) 
			   pc 
			   (append child-strings (append (list string1) (list string2))))))))))

;; performs the crossovers of individual parents
;; returns a list of two new child strings
(defun 2-point-aux (parent1 parent2 start end)
  (list (append (subseq parent1 0 start) 
		(append (subseq parent2 start end) (subseq parent1 end)))
	(append (subseq parent2 0 start) 
		(append (subseq parent1 start end) (subseq parent2 end)))))
  
;; performs uniform crossover on pairs of parents with pc probability
;; adds pairs of parents to child-strings uncrossed with 1-pc probability 
(defun uniform (parents pc child-strings)
  (cond ((null parents) child-strings)
	(t 
	 (let* ((string1 (first (first parents)))
		(string2 (first (second parents)))
		(new-strings (uniform-aux string1 
					  string2
					  nil
					  nil)))
	   (cond ((<= (random 1.0) pc)
		  (uniform (rest (rest parents)) 
			   pc 
			   (append child-strings new-strings)))
		 (t 
		  (uniform (rest (rest parents)) 
			   pc 
			   (append child-strings (append (list string1) (list string2))))))))))

;; performs uniform crossover on individual parents 
;; returns a list of two new child strings  
(defun uniform-aux (parent1 parent2 child1 child2)
  (cond ((null parent1) (list child1 child2))
	(t
	 (cond ((= (random 2) 0)
		(uniform-aux (rest parent1) 
			     (rest parent2)
			     (append child1 (list (first parent1)))
			     (append child2 (list (first parent2)))))
	       (t
		(uniform-aux (rest parent1)
			     (rest parent2)
			     (append child1 (list (first parent2)))
			     (append child2 (list (first parent1)))))))))

;;;;;;;;;;;;;;
;; mutation ;;
;;;;;;;;;;;;;;

;; mutates the members of child-strings with (1-pc) probability
;; the number of bits to be mutated is proportional to the negative 
;; log2 of a random number between approximately 0 and 1
(defun mutate (child-strings pc)
  (mutate-aux child-strings pc 0))

(defun mutate-aux (child-strings pc index)
  (cond ((= index (length child-strings)) child-strings)
	((>= (random 1.0) pc)
	 (let* ((individual (nth index child-strings))
		(bits-to-mutate (abs (floor (log (+ (random (- 1 1e-10)) 1e-10) 2)))) 
					;number of bits to be mutated
		(indeces-of-mutation 
		 (sort (first (sublist bits-to-mutate 
				(index-list (- (length individual) 1)))) #'<)))
					;list of indeces of bits to be mutated
	   (mutate-individual individual indeces-of-mutation))
	 (mutate-aux child-strings  pc (+ index 1)))
	(t (mutate-aux child-strings  pc (+ index 1)))))
	   

(defun mutate-individual (string indeces-of-mutation)
  (cond ((null indeces-of-mutation) string)
	(t
	 (setf (nth (first indeces-of-mutation) string) 
	       (abs (- (nth (first indeces-of-mutation) string) 1)))
	 (mutate-individual string (rest indeces-of-mutation)))))
	 	 
;;;;;;;;;;;;
;; sorter ;;
;;;;;;;;;;;;

(defun sort-by-fitness (L)
  (sort L #'< :key #'second))

;;;;;;;;;;;;;;;;;;;;;;;
;; sublist generator ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sublist (n L)
  "returns a list containing n randomly selected elements of list L"
  (sublist-helper n nil L))

(defun sublist-helper (n new-list L)
  (cond ((= n 0)  (list new-list L))
	(t
	 (let ((index (random (length L))))
	   (sublist-helper (- n 1) 
			   (cons (nth index L) new-list)
			   (append (subseq L 0 index) (subseq L (+ index 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; index list generator ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generates a list of indeces
(defun index-list (length)
  (index-list-aux length nil))

(defun index-list-aux (length indeces)
  (cond ((= length 0) indeces)
	(t
	 (index-list-aux (- length 1) (cons length indeces)))))

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
    (format stream (concatenate 'string content "~%")))
  name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gets stats of a poulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-stats (population)
  (let* ((max (- (length population) 1))
	 (med (ceiling (/ max 2)))
	 (q1 (ceiling (/ med 2)))
	 (q3 (+ q1 med)))
    (list (second (first population))
	  (second (nth q1 population))
	  (second (nth med population))
	  (second (nth q3 population))
	  (second (nth max population)))))
