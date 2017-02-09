;;;***************************
;;;* DEFFACTS KNOWLEDGE BASE *
;;;***************************

(deffacts MAIN::knowledge-base
   (welcome (message WelcomeMessage))
   (goal (variable type.product))
   (legalanswers (values yes no))
   
   ;; 1. income
   (rule (if income is no) 
         (then type.product is none))
   (question (variable income)
             (query income.query))

   ;; 2. age         
   (rule (if income is yes and age is 1) 
         (then type.product is none))
   (rule (if income is yes and age is 2) 
         (then type.product is criticalcare))
   (rule (if income is yes and age is 3) 
         (then type.product is none))
   (question (variable age)
             (query age.query))
    
   (answer (variable type.product)))