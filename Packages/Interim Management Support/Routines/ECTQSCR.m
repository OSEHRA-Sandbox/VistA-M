ECTQSCR ;B'ham ISC/PTD-List QA Screens ;01/29/91 08:00
V ;;1.05;INTERIM MANAGEMENT SUPPORT;;
 I '$D(^QA(741.1)) W *7,!!?29,"OPTION IS UNAVAILABLE!",!,"The 'QA Occurrence Screen Criteria' File - #741.1 is not loaded on your system.",!! S XQUIT="" Q
DIP S L=0,DIC="^QA(741.1,",BY=".01",FLDS=".01;S;L15;""SCREEN NUMBER"",1;""DESCRIPTION OF SCREEN""",(FR,TO)="",DHD="QA OCCURRENCE SCREENS"
 D EN1^DIP
EXIT K BY,DIC,DHD,FLDS,FR,L,P,TO,X
 Q
 ;
