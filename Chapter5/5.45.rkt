#lang planet neil/sicp

a:
;                |  Maximum depth  |  Number of pushes
;-----------------------------------------------------
;  interpreted   |      5n+3       |      32n-16	
;-----------------------------------------------------
;   compiled     |      3n-1       |      6n+1
;-----------------------------------------------------
;special-purpose |      2n-2       |      2n-2


;total-pushes: 
; compiled/interpretation =  3/16
; special/interpretation  =  1/16
;maximum-depth: 
; compiled/interpretation =  3/5
; special/interpretation  =  2/5


b:

;using open-coded:

;    |  Maximum depth  |  Number of pushes
;-----------------------------------------
;    |      2n-2       |      2n+3	

