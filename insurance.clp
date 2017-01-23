;; This program is written by 
;; Ricky Putra, Steven, Arnon, Kai Xiang, Bryan, Joyce
;; 2017
;; ISS NUS (Copyright)

;; to store the current goal - "buy-insurance"
(deftemplate current_goal 
	(slot goal) (slot cf))
    
;; to store the current facts
;; age, sex, dependents, income, parents_have_critical_diseases, etc
(deftemplate current_fact 
	(slot fact) (slot cf))

;; facts initial values
(deffacts load-facts
	(current_fact (fact age) (cf 0))
	(current_fact (fact sex) (cf 0))
	(current_fact (fact dependents) (cf 0))
	(current_fact (fact income) (cf 0))
	(current_fact (fact parents_have_critical_diseases) (cf 0))
)

;;;;;;;;;;;;;;;;;;;; BUSINESS RULES ;;;;;;;;;;;;;;;;