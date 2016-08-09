DINIT125 ;SFISC/MKO-SORT TEMPLATE FILE ;1:13 PM  13 Nov 1998
 ;;22.2;VA FileMan;;Jan 05, 2016;Build 42
 ;;Per VA Directive 6402, this routine should not be modified.
 ;;Submitted to OSEHRA 5 January 2015 by the VISTA Expertise Network.
 ;;Based on Medsphere Systems Corporation's MSC FileMan 1051.
 ;;Licensed under the terms of the Apache License, Version 2.0.
 ;
 F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) S @X=Y
 G ^DINIT126
Q Q
 ;;^DD(.4014,14,21,1,0)
 ;;=Fields are assigned a probability rating based on the number of hits
 ;;^DD(.4014,14,21,2,0)
 ;;=found for the query (or sort) condition.  The probability rating is used
 ;;^DD(.4014,14,21,3,0)
 ;;=to determine the order in which query conditions should be executed
 ;;^DD(.4014,14,21,4,0)
 ;;=during the sort phase.  Fields with a higher probability rating are
 ;;^DD(.4014,14,21,5,0)
 ;;=executed first to most quickly cut down the number of records that have
 ;;^DD(.4014,14,21,6,0)
 ;;=to be processed.
 ;;^DD(.4014,14,23,0)
 ;;=^^1^1^2930125^
 ;;^DD(.4014,14,23,1,0)
 ;;=Calculated by a call to the FileMan Query Optimizer routine ^DIOQ.
 ;;^DD(.4014,14,"DT")
 ;;=2930125
 ;;^DD(.4014,15,0)
 ;;=DATA TYPE FOR SORTING^P.81'^DI(.81,^0;10^Q
 ;;^DD(.4014,15,21,0)
 ;;=^^5^5^2930514^
 ;;^DD(.4014,15,21,1,0)
 ;;=This pointer to the FileMan DATA TYPE file is entered automatically by
 ;;^DD(.4014,15,21,2,0)
 ;;=FileMan during the sort/print.  Note that if sorting by a pointer or a
 ;;^DD(.4014,15,21,3,0)
 ;;=variable pointer, FileMan will follow the pointer chain until it gets to
 ;;^DD(.4014,15,21,4,0)
 ;;=one of the other data types, in order to determine how to correctly set up
 ;;^DD(.4014,15,21,5,0)
 ;;=the sort logic.
 ;;^DD(.4014,15,23,0)
 ;;=^^1^1^2930514^
 ;;^DD(.4014,15,23,1,0)
 ;;=Pointer to DATA TYPE file, derived by FileMan in routine DTYP^DIP1.
 ;;^DD(.4014,15,"DT")
 ;;=2930514
 ;;^DD(.4014,16,0)
 ;;=COMPUTED FIELD CODE^K^^CM;E1,245^K:$L(X)>245 X D:$D(X) ^DIM
 ;;^DD(.4014,16,3)
 ;;=This is Standard MUMPS code, generated for sorting by computed fields or pointer fields.
 ;;^DD(.4014,16,9)
 ;;=@
 ;;^DD(.4014,16,21,0)
 ;;=^^3^3^2930201^
 ;;^DD(.4014,16,21,1,0)
 ;;=This field contains MUMPS code used to find the actual value of a field
 ;;^DD(.4014,16,21,2,0)
 ;;=that is computed or a pointer.  The code is generated by DICOMP.  This
 ;;^DD(.4014,16,21,3,0)
 ;;=code may execute code in OVERFLOW nodes as well.
 ;;^DD(.4014,16,23,0)
 ;;=^^1^1^2930201^
 ;;^DD(.4014,16,23,1,0)
 ;;=Generated by DICOMP.  Put into the DPP array in C^DIP0.
 ;;^DD(.4014,16,"DT")
 ;;=2930201
 ;;^DD(.4014,17,0)
 ;;=MULTIPLE FIELD DATA^.40141^^1;0
 ;;^DD(.4014,18,0)
 ;;=RELATIONAL JUMP FIELD DATA^.401418^^2;0
 ;;^DD(.4014,19,0)
 ;;=OVERFLOW DATA^.401419^^3;0
 ;;^DD(.4014,19,21,0)
 ;;=^^5^5^2930201^
 ;;^DD(.4014,19,21,1,0)
 ;;=This field contains the first subscript from the part of the DPP array
 ;;^DD(.4014,19,21,2,0)
 ;;=that contains overflow code executed when sorting by a field that is
 ;;^DD(.4014,19,21,3,0)
 ;;=gotten to relationally or a computed field.  Overflow code is generated
 ;;^DD(.4014,19,21,4,0)
 ;;=when needed by DICOMP.  This field will typically look something like
 ;;^DD(.4014,19,21,5,0)
 ;;="OVF0".
 ;;^DD(.4014,19,23,0)
 ;;=^^1^1^2930201^
 ;;^DD(.4014,19,23,1,0)
 ;;=Generated by DICOMP from DIP0 during the sort/print option.
 ;;^DD(.4014,19,"DT")
 ;;=2930201
 ;;^DD(.4014,20,0)
 ;;=SUBHEADER OUTPUT TRANSFORM^K^^OUT;E1,245^K:$L(X)>245 X D:$D(X) ^DIM
 ;;^DD(.4014,20,3)
 ;;=This is Standard MUMPS code.  This is used only when sorting by a user-specified cross-reference in input variable BY(0).
 ;;^DD(.4014,20,9)
 ;;=@
 ;;^DD(.4014,20,21,0)
 ;;=^^6^6^2930204^
 ;;^DD(.4014,20,21,1,0)
 ;;=Defined only when using the BY(0) input variable to the FileMan print,
 ;;^DD(.4014,20,21,2,0)
 ;;=EN1^DIP, which allows the user to specify a cross-reference to sort on.
 ;;^DD(.4014,20,21,3,0)
 ;;=The user is allowed to specify MUMPS code that can be used as an output
 ;;^DD(.4014,20,21,4,0)
 ;;=transform for any of the subheaders (i.e., subscripts in the
 ;;^DD(.4014,20,21,5,0)
 ;;=cross-reference) in the S input array.  This output transform code is
 ;;^DD(.4014,20,21,6,0)
 ;;=stored in this field.
 ;;^DD(.4014,20,23,0)
 ;;=^^4^4^2930204^
 ;;^DD(.4014,20,23,1,0)
 ;;=Stores output transform code from the third piece of S(0,N) where N is
 ;;^DD(.4014,20,23,2,0)
 ;;=the sort level.  This is an input array used in conjunction with BY(0)
 ;;^DD(.4014,20,23,3,0)
 ;;=when user specifies a specific cross-reference to use for the sort, in
 ;;^DD(.4014,20,23,4,0)
 ;;=in the FileMan print routine EN1^DIP.
 ;;^DD(.4014,20,"DT")
 ;;=2930204
 ;;^DD(.4014,21,0)
 ;;=TEXT SORT FLAG^S^SORT:SORT LIKE TEXT;RANGE:TREAT RANGE LIKE TEXT;^SRTTXT;1^Q
 ;;^DD(.4014,21,21,0)
 ;;=^^12^12^2931221^
 ;;^DD(.4014,21,21,1,0)
 ;;=This flag will be set in one of two cases.
 ;;^DD(.4014,21,21,2,0)
 ;;= 1) If the user entered the ;TXT qualifier, the flag will be set to
 ;;^DD(.4014,21,21,3,0)
 ;;="SORT", and will cause a space to be inserted at the beginning of each
 ;;^DD(.4014,21,21,4,0)
 ;;=sort value, causing even numeric fields to be sorted as if they were text.
 ;;^DD(.4014,21,21,5,0)
 ;;= 2) If the user entered a FROM or TO value that is a non-canonic number,
 ;;^DD(.4014,21,21,6,0)
 ;;=the flag will be set to RANGE, and will cause sort values that are numeric
 ;;^DD(.4014,21,21,7,0)
 ;;=to be treated as if they were text, when seeing whether they fall within
 ;;^DD(.4014,21,21,8,0)
 ;;=the from/to range.  However, they will still sort like numbers (MUMPS sort
 ;;^DD(.4014,21,21,9,0)
 ;;=sequence).
 ;;^DD(.4014,21,21,10,0)
 ;;= 
 ;;^DD(.4014,21,21,11,0)
 ;;=The flag is set automatically when the user is entering the sort fields in
 ;;^DD(.4014,21,21,12,0)
 ;;=^DIP, and the from/to values in ^DIP1.
 ;;^DD(.4014,21,"DT")
 ;;=2931221
 ;;^DD(.40141,0)
 ;;=MULTIPLE FIELD DATA SUB-FIELD^^1^2
 ;;^DD(.40141,0,"DT")
 ;;=2930201
 ;;^DD(.40141,0,"IX","B",.40141,.01)
 ;;=
 ;;^DD(.40141,0,"NM","MULTIPLE FIELD DATA")
 ;;=
 ;;^DD(.40141,0,"UP")
 ;;=.4014
 ;;^DD(.40141,.01,0)
 ;;=MULT.FILE OR SUBFILE NO.^MNJ13,5^^0;1^K:+X'=X!(X>9999999.99999)!(X<0)!(X?.E1"."6N.N) X
 ;;^DD(.40141,.01,1,0)
 ;;=^.1
 ;;^DD(.40141,.01,1,1,0)
 ;;=.40141^B
 ;;^DD(.40141,.01,1,1,1)
 ;;=S ^DIBT(DA(2),2,DA(1),1,"B",$E(X,1,30),DA)=""
 ;;^DD(.40141,.01,1,1,2)
 ;;=K ^DIBT(DA(2),2,DA(1),1,"B",$E(X,1,30),DA)
 ;;^DD(.40141,.01,3)
 ;;=Type a Number between 0 and 9999999.99999, 5 Decimal Digits.  This is the file/subfile number when sorting by a multiple field.
 ;;^DD(.40141,.01,21,0)
 ;;=^^4^4^2930201^
 ;;^DD(.40141,.01,21,1,0)
 ;;=All files or subfiles needed to get back up to the top level from a
 ;;^DD(.40141,.01,21,2,0)
 ;;=multiple field will be represented by an entry in this field.  The
 ;;^DD(.40141,.01,21,3,0)
 ;;=file or subfile number will be used as a subscript in the DPP array
