FSCRPCSL ;SLC/STAFF-NOIS RPC Save List ;2/4/97  20:52
 ;;1.1;NOIS;;Sep 06, 1998
 ;
TEMP(IN,OUT) ; from FSCRPX (RPCSaveTempList)
 N CALL,LINE,LIST,LISTFLAG,LISTSNUM,SAVETYPE
 S LISTFLAG=+$G(^TMP("FSCRPC",$J,"INPUT",1)),LIST=+$P($G(^(1)),U,2),SAVETYPE=$P($G(^(1)),U,3)
 D MRU^FSCMR(DUZ,LIST)
 I SAVETYPE="REPLACE" D
 .S CALL=0 F  S CALL=$O(^FSCD("LISTS","ALC",LIST,CALL)) Q:CALL<1  S LISTSNUM=+$O(^(CALL,0)) I LISTSNUM D DELETE^FSCLMPS(LISTSNUM)
 I LISTFLAG D
 .S CALL=0 F  S CALL=$O(^TMP("FSC CURRENT LIST",$J,"C",CALL)) Q:CALL<1  D ADD^FSCLMPS(CALL,LIST)
 E  D
 .S LINE=1 F  S LINE=$O(^TMP("FSCRPC",$J,"INPUT",LINE)) Q:LINE<1  S CALL=+^(LINE) I CALL D ADD^FSCLMPS(CALL,LIST)
 Q
