
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
   (not (no-conclusion ?))
   
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
                 
(defrule determine-citizenship ""

   (age ?)
   (income ?)
   (marital ?)
   (smoking ?)
   (not (citizenship ?))
   
   =>
   
   (bind ?answers (create$ singaporean pr foreigner))
   (handle-state interview
                 (find-text-for-id citizenship.query)
                 citizenship
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-race ""

   (age ?)
   (income ?)
   (marital ?)
   (smoking ?)
   (citizenship ?)
   (not (race ?))
   
   =>
   
   (bind ?answers (create$ chinese malay indian caucasian others))
   (handle-state interview
                 (find-text-for-id race.query)
                 race
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;; Hospitalization Plan
(defrule have-hospitalization-plan-qn ""

   (age ?)
   (income ?)
   (marital ?)
   (smoking ?)
   (citizenship ?)
   (race ?)
   (not (have-hospitalization-plan-ans ?))
=>	
   (bind ?answers (create$ yes no))
   (handle-state interview
                 (find-text-for-id hospital-plan.query)
                 have-hospitalization-plan-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;; View other plans
(defrule have-hospitalization-plan-yes
   (have-hospitalization-plan-ans yes)
   (not (view-other-plans-ans ?))
=>	
   (bind ?answers (create$ yes no))
   (handle-state interview
                 (find-text-for-id view-other-plans.query)
                 view-other-plans-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;; Do you prefer just the standard hospitalization plan or do you prefer a more holistic hospitalization plan?
(defrule standard-vs-holistic-qn
    (or(have-hospitalization-plan-ans no)(view-other-plans-ans yes))
    (not (standard-vs-holistic-ans ?))
=>	
    (bind ?answers (create$ standard holistic))
    (handle-state interview
                 (find-text-for-id stan-vs-holistic-qn.query)
                 standard-vs-holistic-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;; In the event you are warded, do you prefer private or public hospitals?
(defrule private-vs-public-qn
	(standard-vs-holistic-ans holistic)
    (not (private-vs-public-ans ?))
=>	
    (bind ?answers (create$ private public anything))
    (handle-state interview
                 (find-text-for-id private-vs-public-qn.query)
                 private-vs-public-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;; Do you prefer Class B1 wards or Class A wards?
(defrule B1-vs-A-qn
	(private-vs-public-ans public)
    (not(B1-vs-A-ans ?))
=>	
    (bind ?answers (create$ classb1 classa anything))
    (handle-state interview
                 (find-text-for-id B1-vs-A-qn.query)
                 B1-vs-A-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;; How much S$ per day do you want in the event of confinement in community hospital?
(defrule confinement-in-community-hospital-qn
	(or (private-vs-public-ans ?) (B1-vs-A-ans ?))
    (B1-vs-A-ans ?)
    (not (confinement-in-community-hospital-ans ?))
=>	
    (bind ?answers (create$ low moderate high anything))
    (handle-state interview
                 (find-text-for-id confinement-in-community-hospital-qn.query)
                 confinement-in-community-hospital-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much S$ per period do you want in the event of diagnose with congenital abnormalities?
(defrule congenital-abnormalities-qn
	(confinement-in-community-hospital-ans ?)
    (not (congenital-abnormalities-ans ?))
=>	
    (bind ?answers (create$ low moderate high anything))
    (handle-state interview
                 (find-text-for-id congenital-abnormalities-qn.query)
                 congenital-abnormalities-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much S$ do you want per living organ donor transplant?
(defrule organ-transplant-qn
	(congenital-abnormalities-ans ?)
    (not (organ-transplant-ans ?))
=>	
    (bind ?answers (create$ low moderate high anything))
    (handle-state interview
                 (find-text-for-id organ-transplant-qn.query)
                 organ-transplant-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much S$ do you want in the event of psychiatric treatment?
(defrule psychiatric-treatment-qn
	(organ-transplant-ans ?)
    (not (psychiatric-treatment-ans ?))
=>	
    (bind ?answers (create$ low moderate high anything))
    (handle-state interview
                 (find-text-for-id psychiatric-treatment-qn.query)
                 psychiatric-treatment-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much final expenses benefit do you want?
(defrule final-expenses-benefit-qn
	(psychiatric-treatment-ans ?)
    (not (final-expenses-benefit-ans ?))
=>	
    (bind ?answers (create$ low moderate high anything))
    (handle-state interview
                 (find-text-for-id final-expenses-benefit-qn.query)
                 final-expenses-benefit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much annual benefit limit do you want?
(defrule annual-benefit-limit-qn
	(final-expenses-benefit-ans ?)
    (not (annual-benefit-limit-ans ?))
=>	
    (bind ?answers (create$ low moderate high anything))
    (handle-state interview
                 (find-text-for-id annual-benefit-limit-qn.query)
                 annual-benefit-limit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)
                 
;;; Do you want additional cash to complement your hospitalization plan?
(defrule additional-cash-qn
	(supreme-medicash start)
    (not (additional-cash-ans ?))
=>	
	(bind ?answers (create$ yes no))
    (handle-state interview
                 (find-text-for-id additional-cash-qn.query)
                 additional-cash-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)






;;; Do you want to see additional benefits on top of your hospitalization plan?
;;; go to critical care
(defrule additional-plan-qn
	(standard-vs-holistic-ans standard)
    (not (additional-plan-ans ?))
=>	
	(bind ?answers (create$ yes no))
    (handle-state interview
                 (find-text-for-id additional-plan-qn.query)
                 additional-plan-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Critical care Plan
(defrule drinking-habits ""

   (additional-plan-ans yes)
   (not (drinkhabits ?))
   
   =>
   
   (bind ?answers (create$ everyday onceortwice seldom no))
   (handle-state interview
                 (find-text-for-id drinkhabits.query)
                 drinkhabits
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule travel-habits ""

   (drinkhabits ?)
   (not (travelhabits ?))
   
   =>
   
   (bind ?answers (create$ everyday onceortwice seldom no))
   (handle-state interview
                 (find-text-for-id travelhabits.query)
                 travelhabits
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
(defrule is-drink-everyday ""
    (declare (salience 99))
    (drinkhabits everyday)
    =>
    (assert(current_fact (fact drinkhabits) (cf 0.9)))
)
(defrule is-drink-sometimes ""
    (declare (salience 99))
    (drinkhabits onceortwice)
    =>
    (assert(current_fact (fact drinkhabits) (cf 0.8)))
)
(defrule is-drink-seldom ""
    (declare (salience 99))
    (drinkhabits seldom)
    =>
    (assert(current_fact (fact drinkhabits) (cf 0.7)))
)
(defrule is-drink-no ""
    (declare (salience 99))
    (drinkhabits no)
    =>
    (assert(current_fact (fact drinkhabits) (cf 0.6)))
)
(defrule is-travel-everyday ""
    (declare (salience 99))
    (travelhabits everyday)
    =>
    (assert(current_fact (fact travelhabits) (cf 0.8)))
)
(defrule is-travel-sometimes ""
    (declare (salience 99))
    (travelhabits onceortwice)
    =>
    (assert(current_fact (fact travelhabits) (cf 0.7)))
)
(defrule is-travel-seldom ""
    (declare (salience 99))
    (travelhabits seldom)
    =>
    (assert(current_fact (fact travelhabits) (cf 0.6)))
)
(defrule is-travel-no ""
    (declare (salience 99))
    (travelhabits no)
    =>
    (assert(current_fact (fact travelhabits) (cf 0.6)))
)

;;; All Low - B1 Plus CF
(defrule class-B1
	(or (or (or (or (or (B1-vs-A-ans classb1) (confinement-in-community-hospital-ans low)) (congenital-abnormalities-ans low)) (organ-transplant-ans low)) (psychiatric-treatment-ans low)) (final-expenses-benefit low))
=>	
    (assert (supreme-medicash start))
	(assert (current_fact (fact supreme-health-B-plus) (cf 0.6)))
)

;;; All Moderate - A Plus CF
(defrule class-A
	(or (or (or (or (or (B1-vs-A-ans A) (confinement-in-community-hospital-ans Moderate)) (congenital-abnormalities-ans Moderate)) (organ-transplant-ans Moderate)) (psychiatric-treatment-ans Moderate)) (final-expenses-benefit Moderate))
=>	
    (assert (supreme-medicash start))
	(assert (current_fact (fact supreme-health-A-plus) (cf 0.6)))
)
    
;;; All High - P Plus CF
(defrule class-P
	(or (or (or (or (or (private-vs-public-ans private) (confinement-in-community-hospital-ans high)) (congenital-abnormalities-ans high)) (organ-transplant-ans high)) (psychiatric-treatment-ans high)) (final-expenses-benefit high))
=>	
    (assert (supreme-medicash start))
	(assert (current_fact (fact supreme-health-P-plus) (cf 0.6)))
)
    
;;;********************
;;;* CONCLUSIONS *
;;;********************

(defrule view-other-plans-no ""
   (view-other-plans-ans no)
   
   =>
   (assert (no-conclusion true))
)

;;; Standard vs. Holistic --> Standard
(defrule standard-hospitalization-plan-conclusions
	(standard-vs-holistic-ans ?)
    (additional-plan-ans no)
   =>	
    (assert (new_goal (goal supreme-health-standard) (cf 1.0)))
)
    
;;; Supreme Health and Critical Care
(defrule supremehealth-criticalcare-conclusions
	(current_fact (fact gender) (cf ?cf-g))
    (current_fact (fact age) (cf ?cf-a))
    (or(income bet2kand7k)(income above7k))
    (marital ?)
    (citizenship ?)
    (race ?)
    (current_fact (fact drinkhabits) (cf ?cf-d))
    (current_fact (fact travelhabits) (cf ?cf-t))
    (current_fact (fact smoking) (cf ?cf-s))
    (standard-vs-holistic-ans ?)
    (additional-plan-ans yes)
    
   =>	
    
    (assert (new_goal (goal supreme-health-standard-criticalcare) (cf (* (min ?cf-s ?cf-g ?cf-a ?cf-d ?cf-t) 0.95))))
)

;;; critical illness plan
;;;(defrule critical-care-conclusions ""
;;;   (declare (salience 99))
;;;   =>
;;;   (assert (new_goal (goal criticalcare) (cf 1.0)))
;;;)

;;; if income below 2k, no need to recommend critical care adv
(defrule less-income-conclusions ""
   (declare (salience 98))
   (income below2k)
   
   =>
   (assert (no-conclusion true))
)

;;; Supreme Health Standard
(defrule supreme-health-standard-exists ""
       (current_goal (goal supreme-health-standard) (cf ?cf-shs))
   =>
       (if (>= ?cf-shs 0.5) then
            (handle-state conclusion (find-text-for-id supreme-health-standard) ?cf-shs)
       )
)

;;;(defrule critical-care-exists ""
;;;       (current_goal (goal criticalcare) (cf ?cf-cc))
;;;   =>
;;;       (if (>= ?cf-cc 0.5) then
;;;            (handle-state conclusion (find-text-for-id criticalcare) ?cf-cc)
;;;       )
;;;)

(defrule health-criticalcare-exists ""
       (current_goal (goal supreme-health-standard-criticalcare) (cf ?cf-shscc))
   =>
       (handle-state conclusion (find-text-for-id supreme-health-standard-criticalcare) ?cf-shscc)
)

;;; no recommendations
(defrule no-conclusions ""
   (declare (salience -99))
   (no-conclusion true)
   =>
   (handle-state conclusion (find-text-for-id none) 0)
)
