
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

;;;(load-facts c:\inetpub\wwwroot\insurance\initfacts.txt)

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


;;;***************
;;;* QUERY RULES *
;;;***************

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
                 (translate-av ?answers))
)

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
 
;;; Do you prefer higher or lower hospital cash incentive? (Higher/Lower)
(defrule hospital-cash-incentive-qn
	(complete-coverage-ans yes)
	(recommend-P-PLUS true)
    (not (hospital-cash-incentive-ans ?))
=>	
	(bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id hospital-cash-incentive-qn.query)
                 hospital-cash-incentive-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you want to extend medical coverage worldwide? (Yes/No)
(defrule worldwide-medical-coverage-qn
	(complete-coverage-ans yes)
	(hospital-cash-incentive-ans ?)
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

;;; Do you prefer higher or lower daily hospital income benefit?
(defrule  daily-hospital-income-benefit-qn
	(complete-coverage-ans yes)
	(hospital-cash-incentive-ans ?)
	(worldwide-medical-coverage-ans yes)
    (not (daily-hospital-income-benefit-ans ?))
=>	
	(bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id worldwide-medical-coverage-qn.query)
                 daily-hospital-income-benefit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you want daily cash benefit for each day spend in hospital (Yes/No)?
(defrule daily-cash-benefit-qn
	(or (standard-vs-holistic-ans standard)(complete-coverage-ans no))
    (not (daily-cash-benefit-ans ?))
=>	
    (bind ?answers (create$ yes no))
    (handle-state interview
                 (find-text-for-id daily-cash-benefit-qn.query)
                 daily-cash-benefit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Daily Hospital Cash Benefit - Illness
(defrule daily-hospital-cash-benefit-illness-qn
	(daily-cash-benefit-ans yes)
	(not (daily-hospital-cash-benefit-illness-ans ?))
=>	
    (bind ?answers (create$ low moderate high))
    (handle-state interview
                 (find-text-for-id daily-hospital-cash-benefit-illness-qn.query)
                 daily-hospital-cash-benefit-illness-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Daily Hospital Cash Benefit - Accident
(defrule daily-hospital-cash-benefit-accident-qn
	(daily-hospital-cash-benefit-illness-ans ?)
	(not (daily-hospital-cash-benefit-accident-ans ?))
=>	
    (bind ?answers (create$ low moderate high))
    (handle-state interview
                 (find-text-for-id daily-hospital-cash-benefit-accident-qn.query)
                 daily-hospital-cash-benefit-accident-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Daily Hospital Cash Benefit - ICU
(defrule daily-hospital-cash-benefit-ICU-qn
	(daily-hospital-cash-benefit-accident-ans ?)
	(not (daily-hospital-cash-benefit-ICU-ans ?))
=>	
    (bind ?answers (create$ low moderate high))
    (handle-state interview
                 (find-text-for-id daily-hospital-cash-benefit-ICU-qn.query)
                 daily-hospital-cash-benefit-ICU-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you prefer higher or lower claims for cancer treatment?
(defrule cancer-treatment-qn
	(daily-hospital-income-benefit-ans ?)
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
    (bind ?answers (create$ yes no))
    (handle-state interview
                 (find-text-for-id emergency-assistance-services-qn.query)
                 emergency-assistance-services-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you prefer higher or lower additional annual benefit limit?
(defrule additional-annual-benefit-limit-qn	
	(emergency-assistance-services-ans ?)
    (not (additional-annual-benefit-limit-ans ?))
=>	
    (bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id additional-annual-benefit-limit-qn.query)
                 additional-annual-benefit-limit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you prefer higher or lower additional lifetime benefit limit?
(defrule additional-lifetime-benefit-limit-qn
	(additional-annual-benefit-limit-ans ?)
    (not (additional-lifetime-benefit-limit-ans ?))
=>	
    (bind ?answers (create$ high low))
    (handle-state interview
                 (find-text-for-id additional-lifetime-benefit-limit-qn.query)
                 additional-lifetime-benefit-limit-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much recuperation benefit (non-surgical hospitalisation) do you prefer? (Low/Medium/High)
(defrule recuperation-benefit-non-surgical-hospitalisation-qn
	(daily-hospital-cash-benefit-ICU-ans ?)
    (not (recuperation-benefit-non-surgical-hospitalisation-ans ?))
=>	
    (bind ?answers (create$ high moderate low))
    (handle-state interview
                 (find-text-for-id recuperation-benefit-non-surgical-hospitalisation-qn.query)
                 recuperation-benefit-non-surgical-hospitalisation-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; How much recuperation benefit (post surgery) do you prefer? (Low/Medium/High)
(defrule recuperation-benefit-post-surgery-qn
	(recuperation-benefit-non-surgical-hospitalisation-ans ?)
    (not (recuperation-benefit-post-surgery-ans ?))
=>	
    (bind ?answers (create$ high moderate low))
    (handle-state interview
                 (find-text-for-id recuperation-benefit-post-surgery-qn.query)
                 recuperation-benefit-post-surgery-ans
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

;;; Do you want to see additional benefits on top of your hospitalization plan?
;;; go to critical care specific rules
(defrule additional-plan-qn
   (and (standard-vs-holistic-ans ?)(daily-cash-benefit-ans no))
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

   (or (additional-plan-ans yes)(Critical-Care-Advantage-start start))
   (not (drinkhabits ?))
   
   =>
   
   (bind ?answers (create$ everyday onceortwice seldom no))
   (handle-state interview
                 (find-text-for-id drinkhabits.query)
                 drinkhabits
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers))
)

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
                 (translate-av ?answers))
)

;;;*****************************
;;;* Common FACTS AND GOALS CF *
;;;*****************************

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

;;;**************************************************************
;;;	Rules for Supreme Health Policy CF Calculations
;;;**************************************************************

;;;********************
;;;* CERTAINTY FACTORS*
;;;********************

;; initialise current goal when a new_goal is asserted
(defrule initialise-current-goal	
	(not (current_goal (goal ?cg) (cf ?cfg)))
	?newg <- (new_goal (goal ?ng) (cf ?cfng))
=> 	(assert (current_goal (goal ?ng) (cf ?cfng)))
	(retract ?newg)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine POSITIVE (or ZERO) certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1- cf1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine NEGATIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1+cf1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-negative-cf
	?f1 <- (current_goal (goal ?g)(cf ?cf1&:(< ?cf1 0)))
  	?f2 <- (new_goal (goal ?g)(cf ?cf2&:(< ?cf2 0)))
  =>
  	(retract ?f2) ; removes new_goal
	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine POSITIVE & NEGATIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = (cf1 + cf2)/ 1- MIN(|cf1|, |cf1|)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-pos-neg-cf
  	?f1 <- (current_goal (goal ?g) (cf ?cf1))
  	?f2 <- (new_goal (goal ?g) (cf ?cf2))
  	(test (< (* ?cf1 ?cf2) 0))
  =>
  	(retract ?f2) ; removes new_goal
	(modify ?f1 (cf =(/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2))))))
)

;;; standard vs comprehensive CF
(defrule standard-vs-comprehensive-cf
    (current_fact (fact Supreme-Health-Standard-Plan) (cf ?cf-Supreme-Health-Standard-Plan))
    (current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
    (standard-vs-holistic-ans ?response)
	(daily-cash-benefit-ans no)
    (additional-plan-ans no) 	
=>	
	(switch ?response
		(case standard then
			(assert (new_goal (goal Supreme-Health-Standard-Plan) (cf (* ?cf-Supreme-Health-Standard-Plan 0.6))))
		)
		(case holistic then
			(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.6))))
			(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.6))))
			(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.6))))
		)
	)	
)

;;; Hospital/Ward Class Entitlement CF
(defrule hospital-ward-class-cf	
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))	
	(standard-vs-holistic-ans holistic)
    (hospital-ward-class-ans ?response)
=>				
	(switch ?response
		(case one then
			(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.9))))						
		)
		(case two then
			(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.9))))
		)
		(case three then
			(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.9))))
		)		
	)
)

;;; Confinement in Community Hospital
(defrule confinement-in-community-hospital-cf	
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))	
	(confinement-in-community-hospital-ans ?response)
=>	
    (switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Congenital Abnormalities
(defrule congenital-abnormalities-cf	
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(congenital-abnormalities-ans ?response)
=>	
	(switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Living Organ Donor Transplant
(defrule living-organ-donor-transplant-cf	
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(living-organ-donor-transplant-ans ?response)
=>	
	(switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Final Expenses Benefit
(defrule final-expenses-benefit-cf	
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(final-expenses-benefit-ans ?response)
=>	
    (switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case moderate then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Annual Benefit Limit
(defrule annual-benefit-limit-cf	
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(annual-benefit-limit-ans ?response)
=>	
    (assert (Total-Health-start start))
	(assert (determine-supreme-health-start start))
	(switch ?response
		(case low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case high then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)


;;;*****************************************************
;;;	Supreme Health Policy Conclusions
;;;*****************************************************

;;; Supreme Health Standard-Plan
(defrule supremehealth-standard-conclusions
	(standard-vs-holistic-ans standard)
	(daily-cash-benefit-ans no)
    (additional-plan-ans no)      
	(current_goal (goal Supreme-Health-Standard-Plan) (cf ?cf-shsp))
   =>    
    (handle-state conclusion (find-text-for-id supreme-health-standard) ?cf-shsp)
)

;;; Supreme Health Policy (Comprehensive)
;;; Determine Supreme Health Policy e.g. A, B or P Plus
(defrule supremehealth-comprehensive-policy-conclusions
	(standard-vs-holistic-ans holistic)
	(hospital-ward-class-ans one)		    
	(confinement-in-community-hospital-ans low)
	(congenital-abnormalities-ans low)
	(organ-transplant-ans low)
	(psychiatric-treatment-ans low)
	(final-expenses-benefit-ans low)
	(annual-benefit-limit-ans low)
	(complete-coverage-ans no)
	(daily-cash-benefit-ans no)
	(additional-plan-ans no)
   =>
	(handle-state conclusion (find-text-for-id supreme-health-B-plus) 0.9)
)


;;;********************
;;;* CONCLUSIONS *
;;;********************

(defrule view-other-plans-no ""
   (view-other-plans-ans no)
   
   =>
   (assert (no-conclusion true))
)

;;; Supreme Health B-Plus and Total Health Silver
(defrule supreme-health-b-plus-silver-exists ""
       (current_goal (goal B-PLUS-SILVER) (cf ?cf-shs))	   
	   (conclude-recommendation true)
   =>
       (handle-state conclusion (find-text-for-id supreme-health-B-plus-silver) ?cf-shs)
)

;;; Supreme Health Standard-Plan and Critical-Care-Advantage
(defrule supreme-health-standard-criticalcare-exists ""
       (current_goal (goal supreme-health-standard-criticalcare) (cf ?cf-shs))
	   (conclude-recommendation true)
   =>
       (handle-state conclusion (find-text-for-id supreme-health-standard-criticalcare) ?cf-shs)
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