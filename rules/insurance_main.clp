
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

(deffacts load-facts
	(current_fact (fact Supreme-Health-Standard-Plan) (cf 0.5))
	(current_fact (fact Supreme-Health-B-PLUS) (cf 0.5))
	(current_fact (fact Supreme-Health-A-PLUS) (cf 0.5))
	(current_fact (fact Supreme-Health-P-PLUS) (cf 0.5))
	(current_fact (fact B-PLUS-SILVER) (cf 0.5))
	(current_fact (fact A-PLUS-GOLD) (cf 0.5))
	(current_fact (fact P-PLUS-PLATINUM-LITE) (cf 0.5))
	(current_fact (fact P-PLUS-PLATINUM) (cf 0.5))
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf 0.5))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf 0.5))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf 0.5))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf 0.5))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.5))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.5))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf 0.5))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf 0.5))
	(current_fact (fact Supreme-MediCash-Plan-A) (cf 0.5))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf 0.5))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf 0.5))

	(current_goal (goal Supreme-Health-Standard-Plan) (cf 0.5))
	(current_goal (goal Supreme-Health-B-PLUS) (cf 0.5))
	(current_goal (goal Supreme-Health-A-PLUS) (cf 0.5))
	(current_goal (goal Supreme-Health-P-PLUS) (cf 0.5))
	(current_goal (goal B-PLUS-SILVER) (cf 0.5))
	(current_goal (goal A-PLUS-GOLD) (cf 0.5))
	(current_goal (goal P-PLUS-PLATINUM-LITE) (cf 0.5))
	(current_goal (goal P-PLUS-PLATINUM) (cf 0.5))
	(current_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.5))
	(current_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.5))
	(current_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.5))
	(current_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.5))
	(current_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.5))
	(current_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.5))
	(current_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.5))
	(current_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.5))
	(current_goal (goal Supreme-MediCash-Plan-A) (cf 0.5))
	(current_goal (goal Supreme-MediCash-Plan-B) (cf 0.5))
	(current_goal (goal Supreme-MediCash-Plan-C) (cf 0.5))
    
    (current_goal (goal Critical-Care-Plan) (cf 0.5))
)

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

;;; Do you prefer (1) Restructured Hospitals, Class B1 Wards, (2) Restructured Hospitals, Class A Wards, or (3) Private Hospitals? (1/2/3)
(defrule hospital-ward-class-qn
	(standard-vs-holistic-ans holistic)
    (not (hospital-ward-class-ans ?))
=>	
    (bind ?answers (create$ one two three))
    (handle-state interview
                 (find-text-for-id hospital-ward-class-qn.query)
                 hospital-ward-class-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much S$ per day do you want in the event of confinement in community hospital?
(defrule confinement-in-community-hospital-qn
	(hospital-ward-class-ans ?)
    (not (confinement-in-community-hospital-ans ?))
=>	
    (bind ?answers (create$ low moderate high))
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
    (bind ?answers (create$ low moderate high))
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
    (bind ?answers (create$ low moderate high))
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
    (bind ?answers (create$ low moderate high))
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
    (bind ?answers (create$ low moderate high))
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
    (bind ?answers (create$ low moderate high))
    (handle-state interview
                 (find-text-for-id annual-benefit-limit-qn.query)
                 annual-benefit-limit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you want a complete coverage (i.e. including deductible, co-insurance, comprehensive benefits etc.)?
(defrule complete-coverage-qn
	(annual-benefit-limit-ans ?)
    (not (complete-coverage-ans ?))
=>	
    (bind ?answers (create$ yes no))
    (handle-state interview
                 (find-text-for-id complete-coverage-qn.query)
                 complete-coverage-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)
 
;;; Do you want to extend medical coverage worldwide? (Yes/No)
(defrule worldwide-medical-coverage-qn
	(complete-coverage-ans ?)
    (not (worldwide-medical-coverage-ans ?))
=>	
	(bind ?answers (create$ yes no))
    (handle-state interview
                 (find-text-for-id worldwide-medical-coverage-qn.query)
                 worldwide-medical-coverage-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you prefer higher or lower hospital cash incentive? (Higher/Lower)
(defrule additional-cash-qn
	(worldwide-medical-coverage-ans ?)
    (not (additional-cash-ans ?))
=>	
	(bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id additional-cash-qn.query)
                 additional-cash-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much daily hospital cash benefit would you like in the event of hospitalization?
(defrule daily-hospital-cash-benefit-qn
	(additional-cash-ans ?)
    (not (daily-hospital-cash-benefit-ans ?))
=>	
    (bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id daily-hospital-cash-benefit-qn.query)
                 daily-hospital-cash-benefit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)


;;; Do you prefer higher or lower claims for cancer treatment?
(defrule cancer-treatment-qn
	(daily-hospital-cash-benefit-ans ?)
    (not (cancer-treatment-ans ?))
=>	
    (bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id cancer-treatment-qn.query)
                 cancer-treatment-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you want emergency assistance services?
(defrule emergency-assistance-services-qn
	(cancer-treatment-ans ?)
    (not (emergency-assistance-services-ans ?))
=>	
    (bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id emergency-assistance-services-qn.query)
                 emergency-assistance-services-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)



;;; Do you want to see additional benefits on top of your hospitalization plan?
;;; go to critical care specific rules
(defrule additional-plan-qn
	(or (standard-vs-holistic-ans standard) (and (standard-vs-holistic-ans ?)(additional-cash-ans no)))
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




;;;**********************
;;;* FACTS AND GOALS CF *
;;;**********************

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



;;; Supreme MediCash Plan A
(defrule supreme-medicash-plan-A
	(or (daily-hospital-cash-benefit-ans low) (recuperation-benefit-ans low))
=>	
    (assert (total-health start))
    (assert (current_fact (fact supreme-medicash-A) (cf 1.0)))
)

;;; Supreme Health Standard and Medicash Plan A
(defrule supremehealth-medicashplanA-conclusions
    (standard-vs-holistic-ans standard)
    (additional-plan-ans yes)
    (current_fact (fact supreme-medicash-A) (cf ?cf-sma))
    
   =>	
    
    (assert (new_goal (goal supreme-health-standard-medicash-A) (cf ?cf-sma)))
)

;;; Supreme Health Standard and Critical Care
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
    (standard-vs-holistic-ans standard)
    (additional-plan-ans yes)
    
   =>	
    
    (assert (new_goal (goal supreme-health-standard-criticalcare) (cf (* (min ?cf-s ?cf-g ?cf-a ?cf-d ?cf-t) 0.95))))
)

;;; Supreme Health B Plus
(defrule supremehealth-bplus-conclusions
    (additional-plan-ans no)
    (current_fact (fact supreme-health-B-plus) (cf ?cf-shbp))
    
   =>	
    
    (assert (new_goal (goal supreme-health-B-plus) (cf ?cf-shbp)))
)

;;; Supreme Health A Plus
(defrule supremehealth-aplus-conclusions
    (additional-plan-ans no)
    (current_fact (fact supreme-health-A-plus) (cf ?cf-shap))
    
   =>	
    
    (assert (new_goal (goal supreme-health-A-plus) (cf ?cf-shap)))
)

;;; Supreme Health B Plus and Critical Care
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
    (current_fact (fact supreme-health-B-plus)(cf ?cf-shbp))
    
   =>	
    
    (assert (new_goal (goal supreme-health-B-plus-criticalcare) (cf (* (min ?cf-s ?cf-g ?cf-a ?cf-d ?cf-t ?cf-shbp) 0.95))))
)

;;; standard vs comprehensive CF
(defrule standard-vs-comprehensive-cf
    (current_fact (fact Supreme-Health-Standard-Plan) (cf ?cf-Supreme-Health-Standard-Plan))
    (current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
    (standard-vs-holistic-ans ?response)
=>	
	(switch ?response
		(case standard then
						(assert (new_goal (goal Supreme-Health-Standard-Plan) (cf (* ?cf-Supreme-Health-Standard-Plan 0.6)))))
		(case holistic then
						(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.6))))
						(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.6))))
						(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.6))))
		)
	)
)

;;; Hospital/Ward Class Entitlement CF
(defrule hospital-ward-class-cf
	(standard-vs-holistic-ans holistic)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
    (hospital-ward-class-ans ?response)
=>	

(switch ?response
		(case one then	(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.9)))))
		(case two then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.9)))))
		(case three then	(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.9)))))
	)
)

;;; Confinement in Community Hospital
(defrule confinement-in-community-hospital-cf
	(standard-vs-holistic-ans holistic)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	
    (switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Congenital Abnormalities
(defrule congenital-abnormalities-cf
	(standard-vs-holistic-ans holistic)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	
	(switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Living Organ Donor Transplant
(defrule living-organ-donor-transplant-cf
	(standard-vs-holistic-ans holistic)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	
	(switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Final Expenses Benefit
(defrule final-expenses-benefit-cf
	(standard-vs-holistic-ans holistic)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	
    (switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Annual Benefit Limit
(defrule annual-benefit-limit-cf
	(standard-vs-holistic-ans holistic)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	
    (assert (Total-Health-start start))
	(switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Do you want a complete coverage?
(defrule complete-coverage-cf
    (Total-Health-start start)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(current_fact (fact B-PLUS-SILVER) (cf ?cf-B-PLUS-SILVER))
	(current_fact (fact A-PLUS-GOLD) (cf ?cf-A-PLUS-GOLD))
	(current_fact (fact P-PLUS-PLATINUM-LITE) (cf ?cf-P-PLUS-PLATINUM-LITE))
	(current_fact (fact P-PLUS-PLATINUM) (cf ?cf-P-PLUS-PLATINUM))
=>	
    (switch ?response
		(case Yes then		(assert (complete-coverage-ans ?response))
					(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS -0.6))))
					(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS -0.6))))
					(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS -0.6))))
					(assert (new_goal (goal B-PLUS-SILVER) (cf (* ?cf-Supreme-Health-B-PLUS 0.6))))
					(assert (new_goal (goal A-PLUS-GOLD) (cf (* ?cf-A-PLUS-GOLD 0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf (* ?cf-P-PLUS-PLATINUM-LITE 0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM) (cf (* ?cf-P-PLUS-PLATINUM 0.6))))
					(assert (new_goal (goal B-PLUS-SILVER) (cf ?cf-Supreme-Health-B-PLUS)))						;;; ERROR!!!
					(assert (new_goal (goal A-PLUS-GOLD) (cf ?cf-Supreme-Health-A-PLUS)))						;;; ERROR!!!
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf ?cf-Supreme-Health-P-PLUS)))					;;; ERROR!!!
					(assert (new_goal (goal P-PLUS-PLATINUM) (cf ?cf-Supreme-Health-P-PLUS)))					;;; ERROR!!!
		)
		(case No then		(assert (Critical-Care-Advantage-start start)))									;;; NOTE!!!
	)
)

;; Do you prefer higher or lower hospital cash incentive? (Higher/Lower)
(defrule hospital-cash-incentive-cf
	(complete-coverage-ans yes)
	(current_fact (fact P-PLUS-PLATINUM-LITE) (cf ?cf-P-PLUS-PLATINUM-LITE))
	(current_fact (fact P-PLUS-PLATINUM) (cf ?cf-P-PLUS-PLATINUM))
=>	
    (assert (Total-Health-Plus-start start))
	(switch ?response
		(case low then		(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf (* ?cf-P-PLUS-PLATINUM-LITE 0.6)))))
		(case high then		(assert (new_goal (goal P-PLUS-PLATINUM) (cf (* ?cf-P-PLUS-PLATINUM 0.6)))))
	)
)

;;; Do you want to extend medical coverage worldwide?
(defrule worldwide-medical-coverage-cf
	(Total-Health-Plus-start start)
	(current_fact (fact B-PLUS-SILVER) (cf ?cf-B-PLUS-SILVER))
	(current_fact (fact A-PLUS-GOLD) (cf ?cf-A-PLUS-GOLD))
	(current_fact (fact P-PLUS-PLATINUM-LITE) (cf ?cf-P-PLUS-PLATINUM-LITE))
	(current_fact (fact P-PLUS-PLATINUM) (cf ?cf-P-PLUS-PLATINUM))
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
    (worldwide-medical-coverage-qn ?response)
=>	
    (switch ?response
		(case yes then		
					(assert (new_goal (goal B-PLUS-SILVER) (cf (* ?cf-B-PLUS-SILVER -0.6))))
					(assert (new_goal (goal A-PLUS-GOLD) (cf (* ?cf-A-PLUS-GOLD -0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf (* ?cf-P-PLUS-PLATINUM-LITE -0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM) (cf (* ?cf-P-PLUS-PLATINUM -0.6))))
					(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf (* ?cf-B-PLUS-SILVER-ESSENTIAL 0.6))))
					(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf (* ?cf-B-PLUS-SILVER-ADVANCE 0.6))))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf (* ?cf-A-PLUS-GOLD-ESSENTIAL 0.6))))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf (* ?cf-A-PLUS-GOLD-ADVANCE 0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL 0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ADVANCE 0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-ESSENTIAL 0.6))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-ADVANCE 0.6))))
					(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER)))					;;; ERROR!!!
					(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER)))						;;; ERROR!!!
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD)))						;;; ERROR!!!
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD)))						;;; ERROR!!!
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE)))				;;; ERROR!!!
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE)))				;;; ERROR!!!
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM)))					;;; ERROR!!!
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM)))					;;; ERROR!!!
		)
		(case no then		(assert (Critical-Care-Advantage-start start)))									;;; NOTE!!!
	)
)

;;; Daily Hospital Income Benefit
(defrule daily-hospital-income-benefit-cf
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
    (daily-hospital-cash-benefit-ans ?response)
=>	
    (switch ?response
		(case low then	
                    (assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf (* ?cf-B-PLUS-SILVER-ESSENTIAL 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf (* ?cf-A-PLUS-GOLD-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-ESSENTIAL 0.4)))))
		(case high then	
                    (assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf (* ?cf-B-PLUS-SILVER-ADVANCE 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf (* ?cf-A-PLUS-GOLD-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-ADVANCE 0.4)))))
	)
)

;;; Cancer Treatment
(defrule cancer-treatment-cf
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
    (cancer-treatment-ans ?response)
=>	
	(switch ?response
		(case low then	
                    (assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf (* ?cf-B-PLUS-SILVER-ESSENTIAL 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf (* ?cf-A-PLUS-GOLD-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-ESSENTIAL 0.4)))))
		(case high then	
                    (assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf (* ?cf-B-PLUS-SILVER-ADVANCE 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf (* ?cf-A-PLUS-GOLD-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-ADVANCE 0.4)))))
	)
)

;;; Emergency Assistance Services
(defrule emergency-assistance-services-qn
	(worldwide-medical-coverage-qn Yes)
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
=>	(printout t crlf "Do you want emergency assistance services? (Yes/No)")
	(bind ?response (read))
	(switch ?response
		(case No then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf (* ?cf-B-PLUS-SILVER-ESSENTIAL 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf (* ?cf-A-PLUS-GOLD-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-ESSENTIAL 0.4)))))
		(case Yes then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf (* ?cf-B-PLUS-SILVER-ADVANCE 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf (* ?cf-A-PLUS-GOLD-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-ADVANCE 0.4)))))
	)
)

;;; Additional Annual Benefit Limit
(defrule additional-annual-benefit-limit-qn
	(worldwide-medical-coverage-qn Yes)
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
=>	(printout t crlf "Do you prefer higher or lower additional annual benefit limit? (Higher/Lower)")
	(bind ?response (read))
	(switch ?response
		(case Lower then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf (* ?cf-B-PLUS-SILVER-ESSENTIAL 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf (* ?cf-A-PLUS-GOLD-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf (* ?cf-P-PLUS-PLATINUM-ESSENTIAL 0.4)))))
		(case Higher then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf (* ?cf-B-PLUS-SILVER-ADVANCE 0.4))))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf (* ?cf-A-PLUS-GOLD-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-LITE-ADVANCE 0.4))))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf (* ?cf-P-PLUS-PLATINUM-ADVANCE 0.4)))))
	)
)
;;;********************
;;;* CONCLUSIONS *
;;;********************

(defrule view-other-plans-no ""
   (view-other-plans-ans no)
   
   =>
   (assert (no-conclusion true))
)

;;; Supreme Health A-Plus
(defrule supreme-health-aplus-exists ""
       (current_goal (goal Supreme-Health-A-PLUS) (cf ?cf-shs))
   =>
       (if (>= ?cf-shs 0.9) then
            (handle-state conclusion (find-text-for-id supreme-health-A-plus) ?cf-shs)
       )
)

;;; Supreme Health B-Plus
(defrule supreme-health-bplus-exists ""
       (current_goal (goal Supreme-Health-B-PLUS) (cf ?cf-shs))
   =>
       (if (>= ?cf-shs 0.7) then
            (handle-state conclusion (find-text-for-id supreme-health-B-plus) ?cf-shs)
       )
)

;;; Supreme Health Standard
(defrule supreme-health-standard-exists ""
       (current_goal (goal Supreme-Health-Standard-Plan) (cf ?cf-shs))
   =>
       (if (>= ?cf-shs 0.7) then
            (handle-state conclusion (find-text-for-id supreme-health-standard) ?cf-shs)
       )
)

(defrule health-standard-criticalcare-exists ""
       (current_goal (goal supreme-health-standard-criticalcare) (cf ?cf-shscc))
   =>
       (handle-state conclusion (find-text-for-id supreme-health-standard-criticalcare) ?cf-shscc)
)

(defrule health-standard-medicashplanA-exists ""
       (current_goal (goal supreme-health-standard-medicash-A) (cf ?cf-sma))
   =>
       (handle-state conclusion (find-text-for-id supreme-health-standard-medicash-A) ?cf-sma)
)

(defrule health-B-plus-criticalcare-exists ""
       (current_goal (goal supreme-health-B-plus-criticalcare) (cf ?cf-shbp))
   =>
       (handle-state conclusion (find-text-for-id supreme-health-B-plus-criticalcare) ?cf-shbp)
)

;;; if income below 2k, married, no need to recommend
(defrule less-income-conclusions ""
   (declare (salience 98))
   (income below2k)
   (marital yes)
   =>
   (assert (no-conclusion true))
)

;;; no recommendations
(defrule no-conclusions ""
   (declare (salience -99))
   (no-conclusion true)
   =>
   (handle-state conclusion (find-text-for-id none) 0)
)