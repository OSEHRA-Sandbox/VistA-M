DINIT902 ;GFT/GFT-DIALOG FILE INITS ;Jan 05, 2015 
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
 F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) S @X=Y
Q Q
 ;;^UTILITY(U,$J,.84,7064,3,1,0)
 ;;=1^'FIRST' OR 'THEN'
 ;;^UTILITY(U,$J,.84,7064,3,2,0)
 ;;=2^'FIELD' or similar
 ;;^UTILITY(U,$J,.84,7064,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7064,5,1,0)
 ;;=DIP2^2
 ;;^UTILITY(U,$J,.84,7065,0)
 ;;=7065^2^^2^THIS IS THE ADVERB 'FIRST'
 ;;^UTILITY(U,$J,.84,7065,2,0)
 ;;=^^1^1^2991028^
 ;;^UTILITY(U,$J,.84,7065,2,1,0)
 ;;=First
 ;;^UTILITY(U,$J,.84,7065,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7065,5,1,0)
 ;;=DIP2^2
 ;;^UTILITY(U,$J,.84,7066,0)
 ;;=7066^2^^2
 ;;^UTILITY(U,$J,.84,7066,1,0)
 ;;=^^1^1^2991028^
 ;;^UTILITY(U,$J,.84,7066,1,1,0)
 ;;=THIS IS THE ADVERB 'THEN'
 ;;^UTILITY(U,$J,.84,7066,2,0)
 ;;=^^1^1^2991028^
 ;;^UTILITY(U,$J,.84,7066,2,1,0)
 ;;=Then
 ;;^UTILITY(U,$J,.84,7066,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7066,5,1,0)
 ;;=DIP2^2
 ;;^UTILITY(U,$J,.84,7068,0)
 ;;=7068^2^y^2
 ;;^UTILITY(U,$J,.84,7068,2,0)
 ;;=^^1^1^2991013^
 ;;^UTILITY(U,$J,.84,7068,2,1,0)
 ;;=Start with |1|
 ;;^UTILITY(U,$J,.84,7068,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7068,3,1,0)
 ;;=1^NAME OF FIELD WHOSE INITAL VALUE WE ARE ASKING FOR
 ;;^UTILITY(U,$J,.84,7068,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7068,5,1,0)
 ;;=DIP1
 ;;^UTILITY(U,$J,.84,7069,0)
 ;;=7069^2^y^2
 ;;^UTILITY(U,$J,.84,7069,2,0)
 ;;=^^1^1^2991013^
 ;;^UTILITY(U,$J,.84,7069,2,1,0)
 ;;=Go to |1|
 ;;^UTILITY(U,$J,.84,7069,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7069,3,1,0)
 ;;=1^NAME OF FIELD WHOSE LAST VALUE WE ARE ASKING FOR
 ;;^UTILITY(U,$J,.84,7069,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7069,5,1,0)
 ;;=DIP1
 ;;^UTILITY(U,$J,.84,7070,0)
 ;;=7070^2^^2
 ;;^UTILITY(U,$J,.84,7070,1,0)
 ;;=^^1^1^2991012^
 ;;^UTILITY(U,$J,.84,7070,1,1,0)
 ;;=As in START WITH: FIRST
 ;;^UTILITY(U,$J,.84,7070,2,0)
 ;;=^^1^1^2991012^^
 ;;^UTILITY(U,$J,.84,7070,2,1,0)
 ;;=FIRST
 ;;^UTILITY(U,$J,.84,7070,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7070,5,1,0)
 ;;=DIP1
 ;;^UTILITY(U,$J,.84,7071,0)
 ;;=7071^2^^2
 ;;^UTILITY(U,$J,.84,7071,1,0)
 ;;=^^1^1^2991012^
 ;;^UTILITY(U,$J,.84,7071,1,1,0)
 ;;=As in GO TO: LAST
 ;;^UTILITY(U,$J,.84,7071,2,0)
 ;;=^^1^1^2991012^
 ;;^UTILITY(U,$J,.84,7071,2,1,0)
 ;;=LAST
 ;;^UTILITY(U,$J,.84,7071,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7071,5,1,0)
 ;;=DIP1
 ;;^UTILITY(U,$J,.84,7080,0)
 ;;=7080^2^^2
 ;;^UTILITY(U,$J,.84,7080,2,0)
 ;;=^^1^1^2991008^
 ;;^UTILITY(U,$J,.84,7080,2,1,0)
 ;;=NULL-STRING
 ;;^UTILITY(U,$J,.84,7080,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7080,5,1,0)
 ;;=DIWE11
 ;;^UTILITY(U,$J,.84,7081,0)
 ;;=7081^2^y^2
 ;;^UTILITY(U,$J,.84,7081,2,0)
 ;;=^^1^1^2991028^^^
 ;;^UTILITY(U,$J,.84,7081,2,1,0)
 ;;=|1| by |2|
 ;;^UTILITY(U,$J,.84,7081,3,0)
 ;;=^.845^2^2
 ;;^UTILITY(U,$J,.84,7081,3,1,0)
 ;;=1^NAME OF FIELD BEING COUNTED IN HISTOGRAM
 ;;^UTILITY(U,$J,.84,7081,3,2,0)
 ;;=2^NEW OF FIELD SUBTOTALLED IN HISTOGRAM
 ;;^UTILITY(U,$J,.84,7081,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7081,5,1,0)
 ;;=DIH
 ;;^UTILITY(U,$J,.84,7085,0)
 ;;=7085^2^^2
 ;;^UTILITY(U,$J,.84,7085,2,0)
 ;;=^^1^1^2990903^
 ;;^UTILITY(U,$J,.84,7085,2,1,0)
 ;;=Deviation
 ;;^UTILITY(U,$J,.84,7085,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7085,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7086,0)
 ;;=7086^2^^2
 ;;^UTILITY(U,$J,.84,7086,2,0)
 ;;=^^1^1^2990903^^
 ;;^UTILITY(U,$J,.84,7086,2,1,0)
 ;;=Maximum
 ;;^UTILITY(U,$J,.84,7086,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7086,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7087,0)
 ;;=7087^2^^2
 ;;^UTILITY(U,$J,.84,7087,2,0)
 ;;=^^1^1^2990903^
 ;;^UTILITY(U,$J,.84,7087,2,1,0)
 ;;=Minimum
 ;;^UTILITY(U,$J,.84,7087,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7087,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7088,0)
 ;;=7088^2^^2
 ;;^UTILITY(U,$J,.84,7088,2,0)
 ;;=^^1^1^2990903^
 ;;^UTILITY(U,$J,.84,7088,2,1,0)
 ;;=Mean
 ;;^UTILITY(U,$J,.84,7088,3,0)
 ;;=^.845
 ;;^UTILITY(U,$J,.84,7088,5,0)
 ;;=^.841^2^2
 ;;^UTILITY(U,$J,.84,7088,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7088,5,2,0)
 ;;=DIH
 ;;^UTILITY(U,$J,.84,7089,0)
 ;;=7089^2^^2
 ;;^UTILITY(U,$J,.84,7089,2,0)
 ;;=^^1^1^2990903^
 ;;^UTILITY(U,$J,.84,7089,2,1,0)
 ;;=Count
 ;;^UTILITY(U,$J,.84,7089,5,0)
 ;;=^.841^2^2
 ;;^UTILITY(U,$J,.84,7089,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7089,5,2,0)
 ;;=DIH
 ;;^UTILITY(U,$J,.84,7090,0)
 ;;=7090^2^^2
 ;;^UTILITY(U,$J,.84,7090,2,0)
 ;;=^^1^1^2990903^
 ;;^UTILITY(U,$J,.84,7090,2,1,0)
 ;;=Total
 ;;^UTILITY(U,$J,.84,7090,5,0)
 ;;=^.841^2^2
 ;;^UTILITY(U,$J,.84,7090,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7090,5,2,0)
 ;;=DIH
 ;;^UTILITY(U,$J,.84,7091,0)
 ;;=7091^2^y^2
 ;;^UTILITY(U,$J,.84,7091,2,0)
 ;;=^^1^1^2990902^^
 ;;^UTILITY(U,$J,.84,7091,2,1,0)
 ;;=Previous specification: '|1|'
 ;;^UTILITY(U,$J,.84,7091,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7091,3,1,0)
 ;;=1^PHRASE DESCRIBING SORT SPECIFICATION , LIKE "DATE null"
 ;;^UTILITY(U,$J,.84,7091,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7091,5,1,0)
 ;;=DIP1
 ;;^UTILITY(U,$J,.84,7092,0)
 ;;=7092^2^y^2
 ;;^UTILITY(U,$J,.84,7092,2,0)
 ;;=^^1^1^2990902^
 ;;^UTILITY(U,$J,.84,7092,2,1,0)
 ;;=|1| is null
 ;;^UTILITY(U,$J,.84,7092,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7092,3,1,0)
 ;;=1^FIELD NAME
 ;;^UTILITY(U,$J,.84,7092,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7092,5,1,0)
 ;;=DIP12
 ;;^UTILITY(U,$J,.84,7093,0)
 ;;=7093^2^y^2
 ;;^UTILITY(U,$J,.84,7093,2,0)
 ;;=^^1^1^2990902^^
 ;;^UTILITY(U,$J,.84,7093,2,1,0)
 ;;= |1| not null
 ;;^UTILITY(U,$J,.84,7093,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7093,3,1,0)
 ;;=1^FIELD NAME
 ;;^UTILITY(U,$J,.84,7093,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7093,5,1,0)
 ;;=DIP12
 ;;^UTILITY(U,$J,.84,7094,0)
 ;;=7094^2^^2
 ;;^UTILITY(U,$J,.84,7094,2,0)
 ;;=^^1^1^2990902^^^
 ;;^UTILITY(U,$J,.84,7094,2,1,0)
 ;;= (includes null values)
 ;;^UTILITY(U,$J,.84,7094,3,0)
 ;;=^.845^^0
 ;;^UTILITY(U,$J,.84,7094,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7094,5,1,0)
 ;;=DIP12^ALL
 ;;^UTILITY(U,$J,.84,7095,0)
 ;;=7095^2^y^2
 ;;^UTILITY(U,$J,.84,7095,1,0)
 ;;=^^1^1^2990823^^
 ;;^UTILITY(U,$J,.84,7095,1,1,0)
 ;;="Page 1" or whatever, at the top of each output page
 ;;^UTILITY(U,$J,.84,7095,2,0)
 ;;=^^1^1^2990823^^^
 ;;^UTILITY(U,$J,.84,7095,2,1,0)
 ;;=PAGE |1|
 ;;^UTILITY(U,$J,.84,7095,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7095,3,1,0)
 ;;=1^PAGE NUMBER
 ;;^UTILITY(U,$J,.84,7095,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7095,5,1,0)
 ;;=DIP5^EGP
 ;;^UTILITY(U,$J,.84,7096,0)
 ;;=7096^2^^2
 ;;^UTILITY(U,$J,.84,7096,2,0)
 ;;=^^1^1^2990531^
 ;;^UTILITY(U,$J,.84,7096,2,1,0)
 ;;=START at PAGE: 
 ;;^UTILITY(U,$J,.84,7096,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7096,5,1,0)
 ;;=DIP3
 ;;^UTILITY(U,$J,.84,7097,0)
 ;;=7097^2^^2
 ;;^UTILITY(U,$J,.84,7097,1,0)
 ;;=^^1^1^2991024^^^
 ;;^UTILITY(U,$J,.84,7097,1,1,0)
 ;;=AS IN 'REPLACE : END'
 ;;^UTILITY(U,$J,.84,7097,2,0)
 ;;=^^1^1^2991024^^
 ;;^UTILITY(U,$J,.84,7097,2,1,0)
 ;;=END
 ;;^UTILITY(U,$J,.84,7097,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7097,5,1,0)
 ;;=DIR2
 ;;^UTILITY(U,$J,.84,7098,0)
 ;;=7098^2^y^2^SUB
 ;;^UTILITY(U,$J,.84,7098,1,0)
 ;;=^^1^1^2991008^^
 ;;^UTILITY(U,$J,.84,7098,1,1,0)
 ;;=TAKES THE WORD 'TOTAL' AND TURNS IT INTO 'SUBTOTAL', 'COUNT' INTO 'SUBCOUNT', ETC
 ;;^UTILITY(U,$J,.84,7098,2,0)
 ;;=^^1^1^2991008^^
 ;;^UTILITY(U,$J,.84,7098,2,1,0)
 ;;=Sub-|1|
 ;;^UTILITY(U,$J,.84,7098,3,0)
 ;;=^.845^1^1
 ;;^UTILITY(U,$J,.84,7098,3,1,0)
 ;;=1^WORD LIKE 'TOTAL'
 ;;^UTILITY(U,$J,.84,7098,5,0)
 ;;=^.841^1^1
 ;;^UTILITY(U,$J,.84,7098,5,1,0)
 ;;=DIO3
 ;;^UTILITY(U,$J,.84,7099,0)
 ;;=7099^2^^2
 ;;^UTILITY(U,$J,.84,7099,1,0)
 ;;=^^1^1^2991218^^^^
 ;;^UTILITY(U,$J,.84,7099,1,1,0)
 ;;=The term for INTERNAL ENTRY NUMBER.
 ;;^UTILITY(U,$J,.84,7099,2,0)
 ;;=^^1^1^2991218^^^^
 ;;^UTILITY(U,$J,.84,7099,2,1,0)
 ;;=NUMBER
 ;;^UTILITY(U,$J,.84,7099,5,0)
 ;;=^.841^5^5
 ;;^UTILITY(U,$J,.84,7099,5,1,0)
 ;;=DICQ
 ;;^UTILITY(U,$J,.84,7099,5,2,0)
 ;;=DIP^NUM
 ;;^UTILITY(U,$J,.84,7099,5,3,0)
 ;;=DIP22^0
 ;;^UTILITY(U,$J,.84,7099,5,4,0)
 ;;=DICOMP0^N+1
 ;;^UTILITY(U,$J,.84,7099,5,5,0)
 ;;=DIQ
 ;;^UTILITY(U,$J,.84,8000,0)
 ;;=8000^2^^2
 ;;^UTILITY(U,$J,.84,8000,2,0)
 ;;=^^1^1^2991028^
