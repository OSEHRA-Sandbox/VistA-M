ENTEXT ;(WASH ISC)/DLM-Break Text Into 80 Char Segments ;8-28-84
 ;;7.0;ENGINEERING;;Aug 17, 1993
 ;EXPECTS STRING IN 'ENORD' - LENGTH OF LINE IN 'ENLTH'
 ;RETURNS NUMBER OF SEGMENTS IN 'ENNU'
 ;  ENORD(1) - ENORD(ENNU)
CKL ;BRK ORDER STR
 S S=1,ENNU=1
 S Z=ENLTH,ENORD(ENNU)=""
CL1 S L=$F(ENORD," ",Z) I L=0&(S'=1) S ENORD(ENNU)=ENORD(ENNU)_$E(ENORD,Z-1,$L(ENORD)) G CL2
 S ENORD(ENNU)=ENORD(ENNU)_$E(ENORD,S,L-2)
 I $L(ENORD(ENNU))>(ENLTH-5) S ENNU=ENNU+1,ENORD(ENNU)=""
 S S=L-1,Z=L
 G CL1
CL2 K S,L,Z,ENLTH Q
