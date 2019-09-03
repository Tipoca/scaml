# Compilation algorithm

## Basics

C(E, e) = v::E   where v is evaluation of e under E

## 

C(E, x) = DIP^(x-1) ; DUP 
    where x is i-th item in E.

C(E, const) = PUSH const

C(E, e1 e2) = o1 ; o2 ; EXEC
    where 
		C(E, e1) = o1
		C(E, e2) = o2
   
C(E, \x.e1)  => LAMBDA _ _ o1
    where
		C(x::E, e1) = o1
		

   

