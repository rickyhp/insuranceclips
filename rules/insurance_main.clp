
;;;======================================================
;;;   Insurance Expert System
;;;
;;;======================================================

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

;; to store the current goal e.g. health insurance plans
(deftemplate current_goal (slot goal) (slot cf))
(deftemplate new_goal (slot goal) (slot cf))

;; to store the current facts with certainty factors
(deftemplate current_fact (slot fact) (slot cf))

(deftemplate MAIN::text-for-id
   (slot id)
   (slot text))

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (multislot display-answers)
   (slot state (default middle)))
   
(deftemplate state-list
   (slot current)
   (multislot sequence))
  
(deffacts startup
   (state-list))

;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

(deffunction MAIN::find-text-for-id (?id)
   ;; Search for the text-for-id fact
   ;; with the same id as ?id
   (bind ?fact
      (find-fact ((?f text-for-id))
                  (eq ?f:id ?id)))
   (if ?fact
      then
      (fact-slot-value (nth$ 1 ?fact) text)
      else
      ?id))
      
(deffunction MAIN::translate-av (?values)
   ;; Create the return value
   (bind ?result (create$))
   ;; Iterate over each of the allowed-values
   (progn$ (?v ?values)
      ;; Find the associated text-for-id fact
      (bind ?nv
         (find-text-for-id ?v))
      ;; Add the text to the return value
      (bind ?result (create$ ?result ?nv)))
   ;; Return the return value
   ?result)

(deffunction MAIN::replace-spaces (?str)
   (bind ?len (str-length ?str))
   (bind ?i (str-index " " ?str))
   (while (neq ?i FALSE)
      (bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
      (bind ?i (str-index " " ?str)))
   ?str)

(deffunction MAIN::sym-cat-multifield (?values)
   (bind ?rv (create$))
   (progn$ (?v ?values)
      (bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
   ?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
   (bind ?rv "")
   (bind ?first TRUE)
   (progn$ (?v ?mv)
      (if ?first
         then
         (bind ?first FALSE)
         (bind ?rv (str-cat ?v))
         else
         (bind ?rv (str-cat ?rv ?delimiter ?v))))
   ?rv)

;;;*****************
;;;* STATE METHODS *
;;;*****************
      
;;; CGI target

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t "state=greeting" crlf)
   (printout t "display=" ?display crlf)
   (printout t "variable=greeting" crlf)
   (printout t "validAnswers=yes" crlf)
   (printout t "displayAnswers=yes" crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id InsuranceExpert) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (printout t "state=interview" crlf)
   (printout t "display=" ?message crlf)  
   (printout t "variable=" ?relation-asserted crlf)
   (printout t "validAnswers=" (multifield-to-delimited-string ?valid-answers ":") crlf)
   (printout t "displayAnswers=" (multifield-to-delimited-string ?display-answers ":") crlf) 
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id InsuranceExpert) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?display LEXEME)
                         (?cf NUMBER))
   (printout t "state=conclusion" crlf)
   (printout t "display=" ?display crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id InsuranceExpert) crlf)
   (printout t "cf=" ?cf crlf)
   (halt))

;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""
  (not (greeting yes))
  =>
  (handle-state greeting
                (find-text-for-id WelcomeMessage)
                greeting
                (create$)))

;;;********************
;;;* CERTAINTY FACTORS*
;;;********************

;;;***********************************************************************
;;; combine POSITIVE (or ZERO) certainty factors for multiple conclusions*
;;; cf(cf1,cf2) = cf1 + cf2 * (1- cf1)                                   *
;;;***********************************************************************

(defrule combine-positive-cf
	?f1 <- (current_goal (goal ?g)(cf ?cf1&:(>= ?cf1 0)))
	?f2 <- (new_goal (goal ?g)(cf ?cf2&:(>= ?cf2 0)))
  =>
  	(retract ?f2) ; removes new_goal
	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
	(printout t "B-1= " ?cf1 crlf) ;;; for debugging
	(printout t "B-2= " ?cf2 crlf) ;;; for debugging
	(printout t "B1-B2 combined = " (+ ?cf1 (* ?cf2 (- 1 ?cf1))) crlf)
)

;;;***********************************************************************
;;;combine NEGATIVE certainty factors for multiple conclusions           *
;;;cf(cf1,cf2) = cf1 + cf2 * (1+cf1)                                     *
;;;***********************************************************************
(defrule combine-negative-cf
 	(declare (salience -1))
	?f1 <- (current_goal (goal ?g)(cf ?cf1&:(< ?cf1 0)))
  	?f2 <- (new_goal (goal ?g)(cf ?cf2&:(< ?cf2 0)))
  =>
  	(retract ?f2) ; removes new_goal
	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
)

;;;***********************************************************************
;combine POSITIVE & NEGATIVE certainty factors for multiple conclusions  *
;cf(cf1,cf2) = (cf1 + cf2)/ 1- MIN(|cf1|, |cf1|)                         *
;;;***********************************************************************
(defrule combine-pos-neg-cf
 	(declare (salience -1))
  	?f1 <- (current_goal (goal ?g) (cf ?cf1))
  	?f2 <- (new_goal (goal ?g) (cf ?cf2))
  	(test (< (* ?cf1 ?cf2) 0))
  =>
  	(retract ?f2) ; removes new_goal
	(modify ?f1 (cf =(/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2))))))
)

;;;***************
;;;* QUERY RULES *
;;;***************

;; initialise current goal when a new_goal is asserted
(defrule initialise-current-goal
	?newg <- (new_goal (goal ?ng) (cf ?cfng))
    (not (current_goal (goal ?cg) (cf ?cfg)))
=> 	(assert (current_goal (goal ?ng) (cf ?cfng)))
	(retract ?newg)
)

(defrule determine-gender ""

   (greeting yes)
   (not (gender ?))
   
   =>
   
   (bind ?answers (create$ male female))
   (handle-state interview
                 (find-text-for-id gender.query)
                 gender
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-age ""

   (gender ?)
   (not (age ?))
   
   =>

   (bind ?answers (create$ below17 bet17and55 above55))
   (handle-state interview
                 (find-text-for-id age.query)
                 age
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-income ""

   (age ?)
   (not (income ?))
   
   =>
   
   (bind ?answers (create$ below2k bet2kand7k above7k))
   (handle-state interview
                 (find-text-for-id income.query)
                 income
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
                 
(defrule determine-marital ""

   (age ?)
   (income ?)
   (not (marital ?))
   
   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 (find-text-for-id marital.query)
                 marital
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-smoking ""

   (age ?)
   (income ?)
   (marital ?)
   (not (smoking ?))
   
   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 (find-text-for-id smoke.query)
                 smoking
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
                 
;;;********************
;;;* FACTS CF *
;;;********************

(defrule smoker ""
    (declare (salience 99))
    (smoking yes)
    =>
    (assert(current_fact (fact smoking) (cf 0.9)))
)
(defrule non-smoker ""
    (declare (salience 99))
    (smoking no)
    =>
    (assert(current_fact (fact smoking) (cf 0.8)))
)
(defrule is-man ""
    (declare (salience 99))
    (gender male)
    =>
    (assert(current_fact (fact gender) (cf 0.8)))
)
(defrule is-woman ""
    (declare (salience 99))
    (gender female)
    =>
    (assert(current_fact (fact gender) (cf 0.7)))
)
(defrule is-older-than-55 ""
    (declare (salience 99))
    (age above55)
    =>
    (assert(current_fact (fact age) (cf 0.8)))
)
(defrule is-mid-age ""
    (declare (salience 99))
    (age bet17and55)
    =>
    (assert(current_fact (fact age) (cf 0.7)))
)
(defrule is-young ""
    (declare (salience 99))
    (age below17)
    =>
    (assert(current_fact (fact age) (cf 0.2)))
)

;;;********************
;;;* CONCLUSIONS *
;;;********************

(defrule critical-care-conclusions ""
   (declare (salience 99))
   (current_fact (fact gender) (cf ?cf-g))
   (current_fact (fact age) (cf ?cf-a))
   (or(income bet2kand7k)(income above7k))
   (marital ?)
   (current_fact (fact smoking) (cf ?cf-s))
   
   =>
   
   (assert (new_goal (goal criticalcare) (cf (* (min ?cf-s ?cf-g ?cf-a) 0.95))))
   
)

(defrule high-cf-goal-exists ""
    (current_goal (goal criticalcare) (cf ?cf-cc))
    
   =>
    (if (>= ?cf-cc 0.6)
        then (handle-state conclusion (find-text-for-id criticalcare) ?cf-cc))
)

(defrule no-conclusions ""
   (declare (salience -99))
   (gender ?)
   (age ?)
   (income ?)
   =>
   (handle-state conclusion (find-text-for-id none) 0)
)
