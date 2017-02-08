;;;***************************
;;;* DEFFACTS KNOWLEDGE BASE *
;;;***************************

(deffacts MAIN::knowledge-base
   (welcome (message WelcomeMessage))
   (goal (variable type.product))
   (legalanswers (values yes no))
   (rule (if income is yes) 
         (then type.product is criticalcare))
   (rule (if income is no) 
         (then type.product is none))
   (question (variable income)
             (query income.query))
   (answer (variable type.product)))