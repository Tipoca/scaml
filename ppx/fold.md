# How to fold

-  ``ITER body``: Apply the body expression to each element of a set.
   The body sequence has access to the stack.

::

    :: (set 'elt) : 'A   ->  'A
       iff body :: [ 'elt : 'A -> 'A ]

    > ITER body / {} : S  =>  S
    > ITER body / { hd ; <tl> } : S  =>  body; ITER body / hd : { <tl> } : S

ITER body / { hd ; <tl> } : acc : S 
body ; ITER (body) / hd : { <tl> } : acc : S
body' ; ITER (body) / hd : acc : { <tl> } : S    body = DIP {SWAP} ; body'
SWAP ; ITER (body) / acc : { <tl> } : S          body' = body'' ; SWAP
ITER (body) / { <tl> } : acc : S


ITER { DIP { SWAP }; body'' ; SWAP } / { hd; <tl> } : acc : S
DIP { SWAP }; body'' ; SWAP ; ITER { DIP { SWAP }; body'' ; SWAP } / hd : { < tl >} : acc S
body'' ; SWAP ; ITER { DIP { SWAP }; body'' ; SWAP } / hd : acc : { < tl > } : S
SWAP ; ITER { DIP { SWAP }; body'' ; SWAP } / acc' : { < tl > } : S
ITER { DIP { SWAP }; body'' ; SWAP } / { < tl > } : acc' : S
...
ITER { DIP { SWAP }; body'' ; SWAP } / {} : acc' : S
{} / acc' : S

## w/o closure

MKL : 'a -> 'acc -> 'acc

X = { DIP { SWAP }; MKL ; EXEC; SWAP ; EXEC ; SWAP } 

ITER X
/ { hd; <tl> } : acc : S

DIP { SWAP }; MKL ; EXEC; SWAP ; EXEC ; SWAP ; ITER X
/ hd : { < tl >} : acc : S

MKL ; EXEC; SWAP ; EXEC ; SWAP ; ITER X
/ hd : acc : { < tl > } : S

EXEC ; SWAP ; EXEC ; SWAP ; ITER X
/ hd : lambda : acc : { < tl > } : S

SWAP ; EXEC ; SWAP ; ITER X
/ lambda2 : acc : { < tl > } : S

EXEC ; SWAP ; ITER X
/ acc : lambda2 : { < tl > } : S

SWAP ; ITER X
/ acc2 : { < tl > } : S

ITER X
/ { < tl > } : acc2 : S

...

ITER X
/ {} : acc' : S

{} 
/ acc' : S

## w/ closure

MKL : 'a -> 'acc -> 'acc

X = { DIP { SWAP }; MKL ; EXEC2; SWAP ; EXEC2 ; SWAP } 

EXEC2 = EXEC if the MKL makes a non closure
EXEC2 = [ DIP [ DUP ; CDR ; DIP [ CAR ] ]; PAIR; EXEC ]

ITER X
/ { hd; <tl> } : acc : S

DIP { SWAP }; MKL ; EXEC2; SWAP ; EXEC2 ; SWAP ; ITER X
/ hd : { < tl >} : acc : S

MKL ; EXEC2; SWAP ; EXEC2 ; SWAP ; ITER X
/ hd : acc : { < tl > } : S

EXEC2 ; SWAP ; EXEC2 ; SWAP ; ITER X
/ hd : lambda/closure : acc : { < tl > } : S

SWAP ; EXEC2 ; SWAP ; ITER X
/ lambda2/closure2 : acc : { < tl > } : S

EXEC2 ; SWAP ; ITER X
/ acc : lambda2 : { < tl > } : S

SWAP ; ITER X
/ acc2 : { < tl > } : S

ITER X
/ { < tl > } : acc2 : S

...

ITER X
/ {} : acc' : S

{} 
/ acc' : S
