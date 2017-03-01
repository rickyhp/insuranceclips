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
	(slot Basic_ElderShield)
	(slot ElderShield_Comprehensive-3ADL)
	(slot ElderShield_Comprehensive-2ADL)
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
	(marital-status ?)
	(not (standard-vs-comprehensive-ans ?))
=>	(printout t crlf "Do you prefer a standard hospitalisation plan or a comprehensive hospitalisation plan? ((s)tandard/(c)omprehensive)" crlf)
	(bind ?response (read))
	(switch ?response
		(case s then	(assert (Supreme-MediCash-start start))
						(assert (standard-vs-comprehensive-ans Standard))
						(assert (current_goal (goal Supreme-Health-Standard-Plan) (cf 0.65)))
						(assert (current_goal (goal Supreme-Health-B-PLUS) (cf -1.0)))
						(assert (current_goal (goal Supreme-Health-A-PLUS) (cf -1.0)))
						(assert (current_goal (goal Supreme-Health-P-PLUS) (cf -1.0)))
		)
		(case c then	(assert (standard-vs-comprehensive-ans Comprehensive))
						(assert (current_goal (goal Supreme-Health-Standard-Plan) (cf -1.0)))
						(assert (current_goal (goal Supreme-Health-B-PLUS) (cf 0.1)))
						(assert (current_goal (goal Supreme-Health-A-PLUS) (cf 0.1)))
						(assert (current_goal (goal Supreme-Health-P-PLUS) (cf 0.1)))
		)
	)
)

;;; Hospital/Ward Class Entitlement
(defrule hospital-ward-class-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(not (hospital-ward-class-ans ?))	
=>	(printout t crlf "Do you prefer (1) Restructured Hospitals, Class B1 Wards, (2) Restructured Hospitals, Class A Wards, or (3) Private Hospitals? (1/2/3)" crlf)
	(bind ?response (read))
	(assert (hospital-ward-class-ans ?response))
	(switch ?response
		(case 1 then	(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.9))))
		(case 2 then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.9))))
		(case 3 then	(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.9))))
	)
)

;;; Confinement in Community Hospital
(defrule confinement-in-community-hospital-qn
	(standard-vs-comprehensive-ans Comprehensive)		
	(not (confinement-in-community-hospital-ans ?))
=>	(printout t crlf "How much S$ do you prefer in the event of confinement in community hospital? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (confinement-in-community-hospital-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.4))))
	)
)

;;; Congenital Abnormalities
(defrule congenital-abnormalities-qn
	(standard-vs-comprehensive-ans Comprehensive)	
	(not (congenital-abnormalities-ans ?))
=>	(printout t crlf "How much S$ do you prefer in the event of diagnose with congenital abnormalities? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (congenital-abnormalities-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.4))))
	)
)

;;; Living Organ Donor Transplant
(defrule living-organ-donor-transplant-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(not (living-organ-donor-transplant-ans ?))
=>	(printout t crlf "How much S$ do you prefer per living organ donor transplant? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (living-organ-donor-transplant-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.4))))
	)
)

;;; Psychiatric Treatment
(defrule psychiatric-treatment-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(not (psychiatric-treatment-ans ?))
=>	(printout t crlf "How much S$ do you prefer in the event of psychiatric treatment? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (psychiatric-treatment-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.4))))
	)
)

;;; Final Expenses Benefit
(defrule final-expenses-benefit-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(not (final-expenses-benefit-ans ?))
=>	(printout t crlf "How much final expenses benefit do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (final-expenses-benefit-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.4))))
	)
)

;;; Annual Benefit Limit
(defrule annual-benefit-limit-qn
	(standard-vs-comprehensive-ans Comprehensive)
	(not (annual-benefit-limit-ans ?))
=>	(printout t crlf "How much annual benefit limit do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (Total-Health-start start))
	(assert (annual-benefit-limit-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-Health-B-PLUS) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-Health-A-PLUS) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-Health-P-PLUS) (cf 0.4))))
	)
)

;;; Do you want a complete coverage?
(defrule complete-coverage-qn
	(Total-Health-start start)
	(not (complete-coverage-ans ?))
=>	(printout t crlf "Do you want a complete coverage (i.e. including deductible, co-insurance, comprehensive benefits etc.)? (Yes/No)" crlf)
	(bind ?response (read))
	(assert (complete-coverage-ans ?response))
	(switch ?response
		(case Yes then		(assert (complete-coverage-ans ?response))
					(assert (new_goal (goal Supreme-Health-B-PLUS) (cf -0.6)))
					(assert (new_goal (goal Supreme-Health-A-PLUS) (cf -0.6)))
					(assert (new_goal (goal Supreme-Health-P-PLUS) (cf -0.6)))
					(assert (new_goal (goal B-PLUS-SILVER) (cf 0.6)))
					(assert (new_goal (goal A-PLUS-GOLD) (cf 0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf 0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM) (cf 0.6)))					
		)
		(case No then		(assert (Critical-Care-Advantage start)))
	)
)

(defrule hospital-cash-incentive-qn
	(complete-coverage-ans Yes)
	(not (hospital-cash-incentive-ans ?))
=>	(printout t crlf "Do you prefer higher or lower hospital cash incentive? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (Total-Health-Plus-start start))
	(assert (hospital-cash-incentive-ans ?response))
	(switch ?response
		(case Lower then		(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf 0.6))))
		(case Higher then		(assert (new_goal (goal P-PLUS-PLATINUM) (cf 0.6))))
	)
)

;;; Do you want to extend medical coverage worldwide?
(defrule worldwide-medical-coverage-qn
	(Total-Health-Plus-start start)
	(not (worldwide-medical-coverage-ans ?))
=>	(printout t crlf "Do you want to extend medical coverage worldwide? (Yes/No)" crlf)
	(bind ?response (read))
	(assert (worldwide-medical-coverage-ans ?response))
	(switch ?response
		(case Yes then
					(assert (new_goal (goal B-PLUS-SILVER) (cf -0.6)))
					(assert (new_goal (goal A-PLUS-GOLD) (cf -0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE) (cf -0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM) (cf -0.6)))
					(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.6)))
					(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.6)))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.6)))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.6)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.6)))					
		)
		(case No then		(assert (Critical-Care-Advantage-start start)))
	)
)

;;; Daily Hospital Income Benefit
(defrule daily-hospital-income-benefit-qn
	(worldwide-medical-coverage-qn Yes)
	(not (daily-hospital-income-benefit-ans ?))
=>	(printout t crlf "Do you prefer higher or lower daily hospital income benefit? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (daily-hospital-income-benefit-ans ?response))
	(switch ?response
		(case Lower then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.4))))
		(case Higher then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.4))))
	)
)

;;; Cancer Treatment
(defrule cancer-treatment-qn
	(worldwide-medical-coverage-qn Yes)
	(not (cancer-treatment-ans ?))	
=>	(printout t crlf "Do you prefer higher or lower claims for cancer treatment? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (cancer-treatment-ans ?response))
	(switch ?response
		(case Lower then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.4))))
		(case Higher then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.4))))
	)
)

;;; Emergency Assistance Services
(defrule emergency-assistance-services-qn
	(worldwide-medical-coverage-qn Yes)
	(not (emergency-assistance-services-ans ?))
=>	(printout t crlf "Do you want emergency assistance services? (Yes/No)" crlf)
	(bind ?response (read))
	(assert (emergency-assistance-services-ans ?response))
	(switch ?response
		(case No then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.4))))
		(case Yes then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.4))))
	)
)

;;; Additional Annual Benefit Limit
(defrule additional-annual-benefit-limit-qn
	(worldwide-medical-coverage-qn Yes)
	(not (additional-annual-benefit-limit-ans ?))
=>	(printout t crlf "Do you prefer higher or lower additional annual benefit limit? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (additional-annual-benefit-limit-ans ?response))
	(switch ?response
		(case Lower then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.4))))
		(case Higher then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.4))))
	)
)

;;; Additional Lifetime Benefit Limit
(defrule additional-lifetime-benefit-limit-qn
	(worldwide-medical-coverage-qn Yes)
	(not (additional-lifetime-benefit-limit-ans ?))
=>	(printout t crlf "Do you prefer higher or lower additional lifetime benefit limit? (Higher/Lower)" crlf)
	(bind ?response (read))
	(assert (Supreme-MediCash-start start))
	(assert (additional-lifetime-benefit-limit-ans ?response))
	(switch ?response
		(case Lower then	(assert (new_goal (goal B-PLUS-SILVER-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ESSENTIAL) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ESSENTIAL) (cf 0.4))))
		(case Higher then	(assert (new_goal (goal B-PLUS-SILVER-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal A-PLUS-GOLD-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-LITE-ADVANCE) (cf 0.4)))
					(assert (new_goal (goal P-PLUS-PLATINUM-ADVANCE) (cf 0.4))))
	)
)

;;; Do you want daily cash benefit for each day spend in hospital?
(defrule daily-cash-benefit-qn
	(Supreme-MediCash-start start)
	(not (daily-cash-benefit-ans ?))
=>	(printout t crlf "Do you want daily cash benefit for each day spend in hospital? ((y)es/(n)o)" crlf)
	(bind ?response (read))
	(assert (daily-cash-benefit-ans ?response))
	(switch ?response
		(case y then (assert (new_goal (goal Supreme-MediCash-Plan-A) (cf 0.6)))
					 (assert (new_goal (goal Supreme-MediCash-Plan-B) (cf 0.6)))
					 (assert (new_goal (goal Supreme-MediCash-Plan-C) (cf 0.6))))
		(case n then (assert (Critical-Care-Advantage start)))
	)
)

;;; Daily Hospital Cash Benefit - Illness
(defrule daily-hospital-cash-benefit-illness-qn
	(daily-cash-benefit-ans y)
	(not (daily-hospital-cash-benefit-illness-ans ?))
=>	(printout t crlf "How much daily hospital cash benefit (illness) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (daily-hospital-cash-benefit-illness-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf 0.3))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf 0.3))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf 0.3))))
	)
)

;;; Daily Hospital Cash Benefit - Accident
(defrule daily-hospital-cash-benefit-accident-qn
	(daily-cash-benefit-ans Yes)
	(not (daily-hospital-cash-benefit-accident-ans ?))
=>	(printout t crlf "How much daily hospital cash benefit (accident) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (daily-hospital-cash-benefit-accident-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf 0.4))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf 0.4))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf 0.4))))
	)
)

;;; Daily Hospital Cash Benefit - ICU
(defrule daily-hospital-cash-benefit-ICU-qn
	(daily-cash-benefit-ans Yes)
	(not (daily-hospital-cash-benefit-ICU-ans ?))
=>	(printout t crlf "How much daily hospital cash benefit (ICU) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (daily-hospital-cash-benefit-ICU-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf 0.5))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf 0.5))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf 0.5))))
	)
)

;;; Recuperation Benefit - Non-Surgical Hospitalisation
(defrule recuperation-benefit-non-surgical-hospitalisation-qn
	(daily-cash-benefit-ans Yes)
	(not (recuperation-benefit-non-surgical-hospitalisation-ans ?))
=>	(printout t crlf "How much recuperation benefit (non-surgical hospitalisation) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (recuperation-benefit-non-surgical-hospitalisation-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf 0.3))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf 0.3))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf 0.3))))
	)
)

;;; Recuperation Benefit - Post Surgery
(defrule recuperation-benefit-post-surgery-qn
	(daily-cash-benefit-ans Yes)
	(not (recuperation-benefit-post-surgery-ans ?))
=>	(printout t crlf "How much recuperation benefit (post surgery) do you prefer? (Low/Medium/High)" crlf)
	(bind ?response (read))
	(assert (Critical-Care-Advantage-start start))													;;; NOTE!!!
	(assert (recuperation-benefit-non-surgical-hospitalisation-ans ?response))
	(switch ?response
		(case Low then		(assert (new_goal (goal Supreme-MediCash-Plan-A) (cf 0.5))))
		(case Medium then	(assert (new_goal (goal Supreme-MediCash-Plan-B) (cf 0.5))))
		(case High then		(assert (new_goal (goal Supreme-MediCash-Plan-C) (cf 0.5))))
	)
)

;;; Do you want to see additional benefits on top of your hospitalization plan?
;;; go to critical care specific rules
(defrule additional-plan-qn
   (Critical-Care-Advantage start)
   (not (additional-plan-ans ?))
=>	
	(printout t crlf "Do you want to see additional benefits on top of your hospitalization plan? ((y)es/(n)o)" crlf)
	(bind ?response (read))
	(assert(additional-plan-ans ?response))
	(assert (current_goal (goal Critical-Care-Advantage) (cf 0.1)))
)

;;;***********************************************************
;;;	Critical care Plan
;;;***********************************************************
(defrule drinking-habits ""
   (additional-plan-ans y)
   (not (drinkhabits ?))   
   =>
    (printout t crlf "Are you drinking regularly? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert(drinkhabits ?response))
	(if(eq ?response y)
		then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.7)))
	)
)

(defrule travel-frequency ""
   (drinkhabits y)      
   =>
    (printout t crlf "How often do you travel in a month? (e)veryday (o)nceortwice (s)eldom (n)o" crlf)
	(bind ?response (read))
	(assert(travel-frequency ?response))
	(switch ?response
		(case e then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.8))))
		(case o then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.65))))
		(case s then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.60))))
		(case n then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.5))))
	)
)

(defrule medical-condition ""
   (travel-frequency ?)
   (not (medical-condition ?))   
   =>
    (printout t crlf "Do you have any existing medical condition? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert(medical-condition ?response))
	(if(eq ?response y)
		then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.8)))
    )
)

(defrule parent-medical-condition ""
   (medical-condition  ?)
   (not (parent-medical-condition ?))   
   =>
    (printout t crlf "Does any of your parents have or had critical medical condition? (y)es/(n)o" crlf)
	(bind ?response (read))
	(assert(parent-medical-condition ?response))
	(if(eq ?response y)
		then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.7)))
    )
)

(defrule high-stress-job ""
   (parent-medical-condition ?)
   (not (high-stress-job ?))   
   =>
    (printout t crlf "How do you scale your current job stress level? (1-10)" crlf)
	(bind ?response (read))
	(assert(high-stress-job ?response))
	(if(>= ?response 5)
		then (assert (new_goal (goal Critical-Care-Advantage) (cf 0.8)))
		else (assert (new_goal (goal Critical-Care-Advantage) (cf 0.6)))
    )
)

;;; Recommendation 
;;; To-Do : separate by product category (Ricky)
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
	(current_goal (goal Basic_ElderShield) (cf ?cf-basic))
	(current_goal (goal ElderShield_Comprehensive-3ADL) (cf ?cf-3adl))
	(current_goal (goal ElderShield_Comprehensive-2ADL) (cf ?cf-2adl))
	
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
		(Basic_ElderShield ?cf-basic) 
		(ElderShield_Comprehensive-3ADL ?cf-3adl) 
		(ElderShield_Comprehensive-2ADL ?cf-2adl)		
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
	(printout t crlf "Basic ElderShield               : " (integer (* ?cf-basic 100)) "%")
	(printout t crlf "ElderShield Comprehensive 3 ADLs: " (integer (* ?cf-3adl 100)) "%")
	(printout t crlf "ElderShield Comprehensive 2 ADLs: " (integer (* ?cf-2adl 100)) "%" crlf)
)

;;;***************************************************************
;;;	Supreme Health Policy Only Recommendation Category
;;;***************************************************************

(defrule compile_supremehealth_only_recommendations
	(current_goal (goal Supreme-Health-Standard-Plan) (cf ?cf-Supreme-Health-Standard-Plan))
	(current_goal (goal Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_goal (goal Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_goal (goal Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
=>	(assert (recommendation
		(Supreme-Health-Standard-Plan ?cf-Supreme-Health-Standard-Plan)
		(Supreme-Health-B-PLUS ?cf-Supreme-Health-B-PLUS)
		(Supreme-Health-A-PLUS ?cf-Supreme-Health-A-PLUS)
		(Supreme-Health-P-PLUS ?cf-Supreme-Health-P-PLUS)
	))
	(printout t crlf "Recommendation:")
	(printout t crlf "Supreme-Health-Standard-Plan: " ?cf-Supreme-Health-Standard-Plan)
	(printout t crlf "Supreme-Health-B-PLUS: " ?cf-Supreme-Health-B-PLUS)
	(printout t crlf "Supreme-Health-A-PLUS: " ?cf-Supreme-Health-A-PLUS)
	(printout t crlf "Supreme-Health-P-PLUS: " ?cf-Supreme-Health-P-PLUS)
)

;;;***************************************************************
;;;	Supreme Health Policy + Critical Care Recommendation Category
;;;***************************************************************

(defrule compile_supremehealth_criticalcare_recommendations
	(current_goal (goal Supreme-Health-Standard-Plan) (cf ?cf-Supreme-Health-Standard-Plan))
	(current_goal (goal Supreme-Health-B-PLUS) (cf ?cf-Supreme-Health-B-PLUS))
	(current_goal (goal Supreme-Health-A-PLUS) (cf ?cf-Supreme-Health-A-PLUS))
	(current_goal (goal Supreme-Health-P-PLUS) (cf ?cf-Supreme-Health-P-PLUS))
	(current_goal (goal Critical-Care-Advantage) (cf ?cf-Critical-Care-Advantage))
=>	(assert (recommendation
		(Supreme-Health-Standard-Plan ?cf-Supreme-Health-Standard-Plan)
		(Supreme-Health-B-PLUS ?cf-Supreme-Health-B-PLUS)
		(Supreme-Health-A-PLUS ?cf-Supreme-Health-A-PLUS)
		(Supreme-Health-P-PLUS ?cf-Supreme-Health-P-PLUS)
		(Critical-Care-Advantage ?cf-Critical-Care-Advantage)
	))
	(printout t crlf "Recommendation:")
	(printout t crlf "Supreme-Health-Standard-Plan: " ?cf-Supreme-Health-Standard-Plan)	
	(printout t crlf "Supreme-Health-B-PLUS: " ?cf-Supreme-Health-B-PLUS)
	(printout t crlf "Supreme-Health-A-PLUS: " ?cf-Supreme-Health-A-PLUS)
	(printout t crlf "Supreme-Health-P-PLUS: " ?cf-Supreme-Health-P-PLUS)
	(printout t crlf "Critical-Care-Advantage: " ?cf-Critical-Care-Advantage crlf)	
)

;;;***********************************************************
;;;	Long Term Care Plan
;;;***********************************************************

;;;Rule 1
;; Are you >= 40 years old?
(defrule age>=40-years-old 
	(age ?a)
	=>
	(switch ?a
		(case 1 then	(assert (current_fact (fact age) (cf -1.0))))
		(case 2 then	(assert (current_fact (fact age) (cf -1.0))))
		(case 3 then	(assert (current_fact (fact age) (cf 1.0))))
		(case 4 then	(assert (current_fact (fact age) (cf 1.0))))
	)
)


;;;Rule 2
;; Are you Singapore Citizen or Singapore PR?
(defrule citizenship=SC_SPR
	(current_fact (fact age) (cf 1.0))
=>	(printout t "Singapore Citizen or Singapore PR? (y)es/(n)o" crlf)
	(bind ?answer (read))
	(if (eq ?answer y) 
	then
		(assert (current_fact (fact citizenship) (cf 1.0)))
	else
		(assert (current_fact (fact citizenship) (cf -1.0)))	
	)
)


;;;Rule 3
;; Do you have any pre-existing disability?
(defrule pre-existing-disability-yes
	(current_fact (fact age) (cf 1.0))
	(current_fact (fact citizenship) (cf 1.0))
=>	(printout t "Do you have any pre-existing disability? (y)es/(n)o" crlf)
	(bind ?answer (read))
	(if (eq ?answer n) 
	then
		(assert (current_fact (fact no_disability) (cf 1.0)))
	else
		(assert (current_fact (fact no_disability) (cf -1.0)))	
	)
)


;Rule 4
;; Do you want payout to commence when unable to perform 2 instead of 3 ADLs?
(defrule want-payout-commence-2-ADL
	(current_fact (fact age) (cf 1.0))
	(current_fact (fact citizenship) (cf 1.0))
	(current_fact (fact no_disability) (cf 1.0))

=>	(printout t "Do you want payout to commence when unable to perform 2 instead of 3 ADL (Activities of Daily Living)? (y)es/(n)o" crlf)
	(bind ?answer (read))
	(if (eq ?answer y) 
	then
    	(assert(current_fact (fact ADL) (cf 1.0)))
	else
		(assert(current_fact (fact ADL) (cf 0.0)))
	)
)


;Rule 5
;; Do you want payout for > 6 years? 
(defrule want-payout->6-years
	(current_fact (fact age) (cf 1.0))
	(current_fact (fact citizenship) (cf 1.0))
	(current_fact (fact no_disability) (cf 1.0))

=>	(printout t "Do you want payout for > 6 years? (y)es/(n)o" crlf)
	(bind ?answer (read))
	(if (eq ?answer y) 
	then
		(assert (current_fact (fact payout_>6years) (cf 1.0)))
	else
		(assert (current_fact (fact payout_>6years) (cf 0.0)))
	)
)


;Rule 6
;; Do you want payout amount of > $400?
(defrule want-payout-amount->400
	(current_fact (fact age) (cf 1.0))
	(current_fact (fact citizenship) (cf 1.0))
	(current_fact (fact no_disability) (cf 1.0))
=>	(printout t "Do you want payout amount of > $400? (y)es/(n)o" crlf)
	(bind ?answer (read))
	(if (eq ?answer y) 
	then
		(assert (current_fact (fact payout_amount_>400) (cf 1.0)))
	else
		(assert (current_fact (fact payout_amount_>400) (cf 0.0)))
	)
)


;;;*******************
;;;* CONCLUSIONS *****
;;;*******************

(defrule conclusion-age-not-eligible-ElderShield
	(current_fact (fact age) (cf -1.0))
=> 
	(printout t "Your age is not eligible for ElderShield Plan" crlf)
	(assert (current_goal (goal Basic_ElderShield) (cf 0.0)))
	(assert (current_goal (goal ElderShield_Comprehensive-3ADL) (cf 0.0)))
	(assert (current_goal (goal ElderShield_Comprehensive-2ADL) (cf 0.0)))  
)


(defrule conclusion-citizenship-not-eligible-ElderShield
   (current_fact (fact citizenship) (cf -1.0))
=> 
   (printout t "Your Citizenship is not eligible for ElderShield Plan" crlf)
	(assert (current_goal (goal Basic_ElderShield) (cf 0.0)))
	(assert (current_goal (goal ElderShield_Comprehensive-3ADL) (cf 0.0)))
	(assert (current_goal (goal ElderShield_Comprehensive-2ADL) (cf 0.0)))  
)


(defrule conclusion-contact-insurance-adviser
   (current_fact (fact no_disability) (cf -1.0))
=> 
   (printout t "Please contact your insurance adviser regarding your pre-existing disability condition." crlf)
   	(assert (current_goal (goal Basic_ElderShield) (cf 0.0)))
	(assert (current_goal (goal ElderShield_Comprehensive-3ADL) (cf 0.0)))
	(assert (current_goal (goal ElderShield_Comprehensive-2ADL) (cf 0.0)))  
)



(defrule conclusion-ElderShield
   (not(current_fact (fact age) (cf -1.0)))
   (current_fact (fact ADL) (cf ?cf-adl))
   (current_fact (fact payout_>6years) (cf ?cf-year))
   (current_fact (fact payout_amount_>400) (cf ?cf-amount)) 
   => 
	(assert (current_goal (goal Basic_ElderShield) (cf (/ (+ (- 1.0 ?cf-adl) (- 1.0 ?cf-year) (- 1.0 ?cf-amount)) 3))))
	(assert (current_goal (goal ElderShield_Comprehensive-3ADL) (cf (/ (+ (- 1.0 ?cf-adl) (+ 0.0 ?cf-year) (+ 0.0 ?cf-amount)) 3))))
	(assert (current_goal (goal ElderShield_Comprehensive-2ADL) (cf (/ (+ (+ 0.0 ?cf-adl) (+ 0.0 ?cf-year) (+ 0.0 ?cf-amount)) 3))))  
)





