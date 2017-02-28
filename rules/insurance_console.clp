;;; *************************** 
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

;; to store the current goal e.g. health insurance plans
(deftemplate current_goal (slot goal) (slot cf))
(deftemplate new_goal (slot goal) (slot cf))

;; to store the current facts with certainty factors
(deftemplate current_fact (slot fact) (slot cf))

(deftemplate recommendation
	(slot Supreme-Health-Standard-Plan)
	(slot Supreme-Health-B-PLUS)
	(slot Supreme-Health-A-PLUS)
	(slot Supreme-Health-P-PLUS)
	(slot B-PLUS-SILVER)
	(slot A-PLUS-GOLD)
	(slot P-PLUS-PLATINUM-LITE)
	(slot P-PLUS-PLATINUM)
	(slot B-PLUS-SILVER-ESSENTIAL)
	(slot B-PLUS-SILVER-ADVANCE)
	(slot A-PLUS-GOLD-ESSENTIAL)
	(slot A-PLUS-GOLD-ADVANCE)
	(slot P-PLUS-PLATINUM-LITE-ESSENTIAL)
	(slot P-PLUS-PLATINUM-LITE-ADVANCE)
	(slot P-PLUS-PLATINUM-ESSENTIAL)
	(slot P-PLUS-PLATINUM-ADVANCE)
	(slot Supreme-MediCash-Plan-A)
	(slot Supreme-MediCash-Plan-B)
	(slot Supreme-MediCash-Plan-C)
	(slot Critical-Care-Advantage)
)

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
	(current_fact (fact Critical-Care-Advantage) (cf 0.5))

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
	(current_goal (goal Critical-Care-Advantage) (cf 0.5))
)

(defrule initialise-current-goal
	(new_goal (goal ?ng) (cf ?cfng))
	(not (current_goal (goal ?cg) (cf ?cfg)))
	?newg <- (new_goal (goal ?ng) (cf ?cfng))
=> 	(assert (current_goal (goal ?ng) (cf ?cfng)))
	(retract ?newg)
)

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

;;; Greetings
(defrule greeting-qn
	(declare (salience 1000))
	(not (greeting ?))	
=>	(printout t crlf "Welcome to GE Life Insurance System, press any key and ENTER to continue" crlf)	
	(bind ?response (read))	
	(assert (greeting yes))
)

;;; Gender
(defrule gender-qn
	(greeting yes)
	(not (gender ?))
=>	(printout t crlf "What is your gender? ((m)ale/(f)emale)" crlf)
	(bind ?response (read))
	(assert (gender ?response))
)

;;; Age
(defrule age-qn
	(gender ?)
	(not (age ?))
=>	(printout t crlf "Please tell us your age range: (1) below 17 (2) between 18 and 39 (3) between 40 and 45 (4) above 45" crlf)
	(bind ?response (read))
	(assert (age ?response))
)

;; Income
(defrule determine-income
    (age ?)
    (not (income ?))
   =>   
    (printout t crlf "Please indicate your income range: (1) below 2000 (2) between 2000-7000 (3) above 7000" crlf)
	(bind ?response (read))
	(assert (income ?response))
)

;; Marital status
(defrule determine-marital-status
    (income ?)
    (not (marital-status ?))
   =>   
    (printout t crlf "Are you married? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert (marital-status ?response))
)

;; Smoking
(defrule determine-smoking
    (marital-status ?)
    (not (smoking ?))
   =>   
    (printout t crlf "Are you smoking? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert (smoking ?response))
)

;; Regular exercise
(defrule determine-exercise
    (smoking ?)
    (not (regular-exercise ?))
   =>   
    (printout t crlf "Do you exercise regularly? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert (regular-exercise ?response))
)


;;; Do you prefer a standard hospitalisation plan or a comprehensive hospitalisation plan?
(defrule standard-vs-comprehensive-qn
	(current_fact (fact Supreme-Health-Standard-Plan) (cf ?cf-Supreme-Health-Standard-Plan))
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(marital-status ?)
	(not (standard-vs-comprehensive-ans ?))
=>	(printout t crlf "Do you prefer a standard hospitalisation plan or a comprehensive hospitalisation plan? ((s)tandard/(c)omprehensive)" crlf)
	(bind ?response (read))
	(switch ?response
		(case s then	(assert (Supreme-MediCash-start start))
						(assert (standard-vs-comprehensive-ans Standard))
						(assert (new_goal (goal Supreme-Health-Standard-Plan) (cf (* ?cf-Supreme-Health-Standard-Plan 0.6)))))
		(case c then	(assert (standard-vs-comprehensive-ans Comprehensive))
						(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.6))))
						(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.6))))
						(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.6))))
		)
	)
)

;;; Hospital/Ward Class Entitlement
(defrule hospital-ward-class-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "Do you prefer (1) Restructured Hospitals, Class B1 Wards, (2) Restructured Hospitals, Class A Wards, or (3) Private Hospitals? (1/2/3)" crlf)
	(bind ?response (read))
	(switch ?response
		(case 1 then	(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.9)))))
		(case 2 then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.9)))))
		(case 3 then	(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.9)))))
	)
)

;;; Confinement in Community Hospital
(defrule confinement-in-community-hospital-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "How much S$ do you prefer in the event of confinement in community hospital? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Congenital Abnormalities
(defrule congenital-abnormalities-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "How much S$ do you prefer in the event of diagnose with congenital abnormalities? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Living Organ Donor Transplant
(defrule living-organ-donor-transplant-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "How much S$ do you prefer per living organ donor transplant? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Psychiatric Treatment
(defrule psychiatric-treatment-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "How much S$ do you prefer in the event of psychiatric treatment? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Final Expenses Benefit
(defrule final-expenses-benefit-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "How much final expenses benefit do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Annual Benefit Limit
(defrule annual-benefit-limit-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(printout t crlf "How much annual benefit limit do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (Total-Health-start start))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf (* ?cf-Supreme-Health-B-PLUS 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf (* ?cf-Supreme-Health-A-PLUS 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf (* ?cf-Supreme-Health-P-PLUS 0.4)))))
	)
)

;;; Do you want a complete coverage?
(defrule complete-coverage-qn
	(Total-Health-start start)
	(current_fact (fact Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_fact (fact Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_fact (fact Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(current_fact (fact B-PLUS-SILVER) (cf ?cf-B-PLUS-SILVER))
	(current_fact (fact A-PLUS-GOLD) (cf ?cf-A-PLUS-GOLD))
	(current_fact (fact P-PLUS-PLATINUM-LITE) (cf ?cf-P-PLUS-PLATINUM-LITE))
	(current_fact (fact P-PLUS-PLATINUM) (cf ?cf-P-PLUS-PLATINUM))
=>	(printout t crlf "Do you want a complete coverage (i.e. including deductible, co-insurance, comprehensive benefits etc.)? (Yes/No)" crlf)
	(bind ?response (read))
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
		(case No then		(assert (Critical-Care-Advantage start)))									;;; NOTE!!!
	)
)

(defrule hospital-cash-incentive-qn
	(complete-coverage-ans Yes)
	(current_fact (fact P-PLUS-PLATINUM-LITE) (cf ?cf-P-PLUS-PLATINUM-LITE))
	(current_fact (fact P-PLUS-PLATINUM) (cf ?cf-P-PLUS-PLATINUM))
=>	(printout t crlf "Do you prefer higher or lower hospital cash incentive? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (Total-Health-Plus-start start))
	(switch ?response
		(case Lower then		(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf (* ?cf-P-PLUS-PLATINUM-LITE 0.6)))))
		(case Higher then		(assert (new_goal (goal P-PLUS-PLATINUM) (cf (* ?cf-P-PLUS-PLATINUM 0.6)))))
	)
)

;;; Do you want to extend medical coverage worldwide?
(defrule worldwide-medical-coverage-qn
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
=>	(printout t crlf "Do you want to extend medical coverage worldwide? (Yes/No)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Yes then		(assert (worldwide-medical-coverage-qn ?response))
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
		(case No then		(assert (Critical-Care-Advantage-start start)))									;;; NOTE!!!
	)
)

;;; Daily Hospital Income Benefit
(defrule daily-hospital-income-benefit-qn
	(worldwide-medical-coverage-qn Yes)
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
=>	(printout t crlf "Do you prefer higher or lower daily hospital income benefit? (Higher/Lower)" crlf)
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

;;; Cancer Treatment
(defrule cancer-treatment-qn
	(worldwide-medical-coverage-qn Yes)
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
=>	(printout t crlf "Do you prefer higher or lower claims for cancer treatment? (Higher/Lower)" crlf)
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
=>	(printout t crlf "Do you want emergency assistance services? (Yes/No)" crlf)
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
=>	(printout t crlf "Do you prefer higher or lower additional annual benefit limit? (Higher/Lower)" crlf)
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

;;; Additional Lifetime Benefit Limit
(defrule additional-lifetime-benefit-limit-qn
	(worldwide-medical-coverage-qn Yes)
	(current_fact (fact B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_fact (fact B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_fact (fact A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_fact (fact A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_fact (fact P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_fact (fact P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
=>	(printout t crlf "Do you prefer higher or lower additional lifetime benefit limit? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (Supreme-MediCash-start start))
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

;;; Do you want daily cash benefit for each day spend in hospital?
(defrule daily-cash-benefit-qn
	(Supreme-MediCash-start start)
	(current_fact (fact Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
=>	(printout t crlf "Do you want daily cash benefit for each day spend in hospital? ((y)es/(n)o)" crlf)
	(bind ?response (read))
	(switch ?response
		(case y then (assert (daily-cash-benefit-ans ?response))
					(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf (* ?cf-Supreme-MediCash-Plan-A 0.6))))
					(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf (* ?cf-Supreme-MediCash-Plan-B 0.6))))
					(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf (* ?cf-Supreme-MediCash-Plan-C 0.6)))))
		(case n then (assert (Critical-Care-Advantage start)))
	)
)

;;; Daily Hospital Cash Benefit - Illness
(defrule daily-hospital-cash-benefit-illness-qn
	(daily-cash-benefit-ans Yes)
	(current_fact (fact Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
=>	(printout t crlf "How much daily hospital cash benefit (illness) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf (* ?cf-Supreme-MediCash-Plan-A 0.3)))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf (* ?cf-Supreme-MediCash-Plan-B 0.3)))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf (* ?cf-Supreme-MediCash-Plan-C 0.3)))))
	)
)

;;; Daily Hospital Cash Benefit - Accident
(defrule daily-hospital-cash-benefit-accident-qn
	(daily-cash-benefit-ans Yes)
	(current_fact (fact Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
=>	(printout t crlf "How much daily hospital cash benefit (accident) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf (* ?cf-Supreme-MediCash-Plan-A 0.4)))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf (* ?cf-Supreme-MediCash-Plan-B 0.4)))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf (* ?cf-Supreme-MediCash-Plan-C 0.4)))))
	)
)

;;; Daily Hospital Cash Benefit - ICU
(defrule daily-hospital-cash-benefit-ICU-qn
	(daily-cash-benefit-ans Yes)
	(current_fact (fact Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
=>	(printout t crlf "How much daily hospital cash benefit (ICU) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf (* ?cf-Supreme-MediCash-Plan-A 0.5)))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf (* ?cf-Supreme-MediCash-Plan-B 0.5)))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf (* ?cf-Supreme-MediCash-Plan-C 0.5)))))
	)
)

;;; Recuperation Benefit - Non-Surgical Hospitalisation
(defrule recuperation-benefit-non-surgical-hospitalisation-qn
	(daily-cash-benefit-ans Yes)
	(current_fact (fact Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
=>	(printout t crlf "How much recuperation benefit (non-surgical hospitalisation) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf (* ?cf-Supreme-MediCash-Plan-A 0.3)))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf (* ?cf-Supreme-MediCash-Plan-B 0.3)))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf (* ?cf-Supreme-MediCash-Plan-C 0.3)))))
	)
)

;;; Recuperation Benefit - Post Surgery
(defrule recuperation-benefit-post-surgery-qn
	(daily-cash-benefit-ans Yes)
	(current_fact (fact Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_fact (fact Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_fact (fact Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
=>	(printout t crlf "How much recuperation benefit (post surgery) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (Critical-Care-Advantage-start start))													;;; NOTE!!!
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf (* ?cf-Supreme-MediCash-Plan-A 0.5)))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf (* ?cf-Supreme-MediCash-Plan-B 0.5)))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf (* ?cf-Supreme-MediCash-Plan-C 0.5)))))
	)
)

;;; Do you want to see additional benefits on top of your hospitalization plan?
;;; go to critical care specific rules
(defrule additional-plan-qn
   (Critical-Care-Advantage start)
   (not (additional-plan-ans ?))
   (current_fact (fact Critical-Care-Advantage) (cf ?cf-Critical-Care-Advantage))
=>	
	(printout t crlf "Do you want to see additional benefits on top of your hospitalization plan? ((y)es/(n)o)" crlf)
	(bind ?response (read))
	(assert(additional-plan-ans ?response))
	(if (eq ?response y)
		then (			
			assert(new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.5)))
		)
	)
)

;;;***********************************************************
;;;	Critical care Plan
;;;***********************************************************
(defrule drinking-habits ""
   (additional-plan-ans y)
   (not (drinkhabits ?))
   (current_fact (fact Critical-Care-Advantage) (cf ?cf-Critical-Care-Advantage))
   =>
    (printout t crlf "Are you drinking regularly? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert(drinkhabits ?response))
	(if(eq ?response y)
		then (assert (new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.7))))
	)
)

(defrule travel-frequency ""
   (drinkhabits y)   
   (current_fact (fact Critical-Care-Advantage) (cf ?cf-Critical-Care-Advantage))
   =>
    (printout t crlf "How often do you travel in a month? (e)veryday (o)nceortwice (s)eldom (n)o" crlf)
	(bind ?response (read))
	(assert(travel-frequency ?response))
	(switch ?response
		(case e then (assert (new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.8)))))
		(case o then (assert (new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.65)))))
		(case s then (assert (new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.60)))))
		(case n then (assert (new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.5)))))
	)
)

(defrule medical-condition ""
   (travel-frequency ?)
   (not (medical-condition ?))
   (current_fact (fact Critical-Care-Advantage) (cf ?cf-Critical-Care-Advantage))
   =>
    (printout t crlf "Do you have any existing medical condition? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert(medical-condition ?response))
	(if(eq ?response y)
		then (assert (new_goal (goal Critical-Care-Advantage) (cf (* ?cf-Critical-Care-Advantage 0.8))))
    )
)


;;; Recommendation
(defrule compile_recommendations
	(current_goal (goal Supreme-Health-Standard-Plan) (cf ?cf-Supreme-Health-Standard-Plan))
	(current_goal (goal Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_goal (goal Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_goal (goal Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(current_goal (goal B-PLUS-SILVER) (cf ?cf-B-PLUS-SILVER))
	(current_goal (goal A-PLUS-GOLD) (cf ?cf-A-PLUS-GOLD))
	(current_goal (goal P-PLUS-PLATINUM-LITE) (cf ?cf-P-PLUS-PLATINUM-LITE))
	(current_goal (goal P-PLUS-PLATINUM) (cf ?cf-P-PLUS-PLATINUM))
	(current_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf ?cf-B-PLUS-SILVER-ESSENTIAL))
	(current_goal (goal B-PLUS-SILVER-ADVANCE) (cf ?cf-B-PLUS-SILVER-ADVANCE))
	(current_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf ?cf-A-PLUS-GOLD-ESSENTIAL))
	(current_goal (goal A-PLUS-GOLD-ADVANCE) (cf ?cf-A-PLUS-GOLD-ADVANCE))
	(current_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL))
	(current_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-LITE-ADVANCE))
	(current_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf ?cf-P-PLUS-PLATINUM-ESSENTIAL))
	(current_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf ?cf-P-PLUS-PLATINUM-ADVANCE))
	(current_goal (goal Supreme-MediCash-Plan-A) (cf ?cf-Supreme-MediCash-Plan-A))
	(current_goal (goal Supreme-MediCash-Plan-B) (cf ?cf-Supreme-MediCash-Plan-B))
	(current_goal (goal Supreme-MediCash-Plan-C) (cf ?cf-Supreme-MediCash-Plan-C))
	(current_goal (goal Critical-Care-Advantage) (cf ?cf-Critical-Care-Advantage))
=>	(assert (recommendation
		(Supreme-Health-Standard-Plan ?cf-Supreme-Health-Standard-Plan)
		(Supreme-Health-B-PLUS ?cf-Supreme-Health-B-PLUS)
		(Supreme-Health-A-PLUS ?cf-Supreme-Health-A-PLUS)
		(Supreme-Health-P-PLUS ?cf-Supreme-Health-P-PLUS)
		(B-PLUS-SILVER ?cf-B-PLUS-SILVER)
		(A-PLUS-GOLD ?cf-A-PLUS-GOLD)
		(P-PLUS-PLATINUM-LITE ?cf-P-PLUS-PLATINUM-LITE)
		(P-PLUS-PLATINUM ?cf-P-PLUS-PLATINUM)
		(B-PLUS-SILVER-ESSENTIAL ?cf-B-PLUS-SILVER-ESSENTIAL)
		(B-PLUS-SILVER-ADVANCE ?cf-B-PLUS-SILVER-ADVANCE)
		(A-PLUS-GOLD-ESSENTIAL ?cf-A-PLUS-GOLD-ESSENTIAL)
		(A-PLUS-GOLD-ADVANCE ?cf-A-PLUS-GOLD-ADVANCE)
		(P-PLUS-PLATINUM-LITE-ESSENTIAL ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL)
		(P-PLUS-PLATINUM-LITE-ADVANCE ?cf-P-PLUS-PLATINUM-LITE-ADVANCE)
		(P-PLUS-PLATINUM-ESSENTIAL ?cf-P-PLUS-PLATINUM-ESSENTIAL)
		(P-PLUS-PLATINUM-ADVANCE ?cf-P-PLUS-PLATINUM-ADVANCE)
		(Supreme-MediCash-Plan-A ?cf-Supreme-MediCash-Plan-A)
		(Supreme-MediCash-Plan-B ?cf-Supreme-MediCash-Plan-B)
		(Supreme-MediCash-Plan-C ?cf-Supreme-MediCash-Plan-C)
		(Critical-Care-Advantage ?cf-Critical-Care-Advantage)
	))

	(printout t crlf "Recommendation:")
	(printout t crlf "Supreme-Health-Standard-Plan: " ?cf-Supreme-Health-Standard-Plan)
	(printout t crlf "Supreme-Health-B-PLUS: " ?cf-Supreme-Health-B-PLUS)
	(printout t crlf "Supreme-Health-A-PLUS: " ?cf-Supreme-Health-A-PLUS)
	(printout t crlf "Supreme-Health-P-PLUS: " ?cf-Supreme-Health-P-PLUS)
	(printout t crlf "Supreme-Health-B-PLUS-Total-Health-SILVER: " ?cf-B-PLUS-SILVER)
	(printout t crlf "Supreme-Health-A-PLUS-Total-Health-GOLD: " ?cf-A-PLUS-GOLD)
	(printout t crlf "Supreme-Health-P-PLUS-Total-Health-PLATINUM-LITE: " ?cf-P-PLUS-PLATINUM-LITE)
	(printout t crlf "Supreme-Health-P-PLUS-Total-Health-PLATINUM: " ?cf-P-PLUS-PLATINUM)
	(printout t crlf "Supreme-Health-B-PLUS-Total-Health-SILVER-Total-Health-Plus-ESSENTIAL: " ?cf-B-PLUS-SILVER-ESSENTIAL)
	(printout t crlf "Supreme-Health-B-PLUS-Total-Health-SILVER-Total-Health-Plus-ADVANCE: " ?cf-B-PLUS-SILVER-ADVANCE)
	(printout t crlf "Supreme-Health-A-PLUS-Total-Health-GOLD-Total-Health-Plus-ESSENTIAL: " ?cf-A-PLUS-GOLD-ESSENTIAL)
	(printout t crlf "Supreme-Health-A-PLUS-Total-Health-GOLD-Total-Health-Plus-ADVANCE: " ?cf-A-PLUS-GOLD-ADVANCE)
	(printout t crlf "Supreme-Health-P-PLUS-Total-Health-PLATINUM-LITE-Total-Health-Plus-ESSENTIAL: " ?cf-P-PLUS-PLATINUM-LITE-ESSENTIAL)
	(printout t crlf "Supreme-Health-P-PLUS-Total-Health-PLATINUM-LITE-Total-Health-Plus-ADVANCE: " ?cf-P-PLUS-PLATINUM-LITE-ADVANCE)
	(printout t crlf "Supreme-Health-P-PLUS-Total-Health-PLATINUM-Total-Health-Plus-ESSENTIAL: " ?cf-P-PLUS-PLATINUM-ESSENTIAL)
	(printout t crlf "Supreme-Health-P-PLUS-Total-Health-PLATINUM-Total-Health-Plus-ADVANCE: " ?cf-P-PLUS-PLATINUM-ADVANCE)
	(printout t crlf "Supreme-MediCash-Plan-A: " ?cf-Supreme-MediCash-Plan-A)
	(printout t crlf "Supreme-MediCash-Plan-B: " ?cf-Supreme-MediCash-Plan-B)
	(printout t crlf "Supreme-MediCash-Plan-C: " ?cf-Supreme-MediCash-Plan-C crlf)
	(printout t crlf "Critical-Care-Advantage: " ?cf-Critical-Care-Advantage crlf)
)





