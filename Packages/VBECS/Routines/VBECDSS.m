VBECDSS ;;HOIFO/BNT-VBECS DSS Extract ;December 05, 2006
 ;;1.0;VBECS;**1,27**;June 1, 2007;Build 1
 ;
 ; Note: This routine supports data exchange with an FDA registered
 ; medical device. As such, it may not be changed in any way without
 ; prior written approval from the medical device manufacturer.
 ; 
 QUIT
 ;
EN(RESULT,PARAMS) ; Main Entry point for RPC
 ; -- RPC:   VBECS DSS EXTRACT
 ; -- INPUT: PARAMS Array containing values to be inserted or updated
 ; --        in the VBECS DSS EXTRACT File (#6002.03).
 ; 
 N I,X,Y,VBECRSLT,VBECLINE,FDA,VBERR
 I '$D(DT) D DT^DICRW
 S VBECRSLT="^TMP(""VBEC_DSS_EXTRACT"",$J)",VBECLINE=0,RESULT=$NA(@VBECRSLT)
 K @RESULT
 I '$D(PARAMS("TRANSACTION ID")) D RESULT(0) Q
 S VBECON=$$FIND1^DIC(6002.03,,,PARAMS("TRANSACTION ID"),,.ERROR)
 S VBECIEN=$S(VBECON:VBECON_",",1:"+1,")
 S FDA(1,6002.03,VBECIEN,.01)=PARAMS("TRANSACTION ID")
 I $D(PARAMS("DFN")),PARAMS("DFN") S FDA(1,6002.03,VBECIEN,1)=$G(PARAMS("DFN"))
 I $D(PARAMS("ORDERING LOCATION")) S FDA(1,6002.03,VBECIEN,2)=$G(PARAMS("ORDERING LOCATION"))
 I $D(PARAMS("TRANSFUSION LOCATION")) S FDA(1,6002.03,VBECIEN,3)=$G(PARAMS("TRANSFUSION LOCATION"))
 ;I $D(PARAMS("FACILITY")) S VBFAC=$$TSTRIP($G(PARAMS("FACILITY"))),VBFAC=$$FIND1^DIC(4,,"MX",VBFAC,,,"VBERR") D
 ;. S FDA(1,6002.03,VBECIEN,3)=VBFAC
 I $D(PARAMS("PHYSICIAN")) S FDA(1,6002.03,VBECIEN,4)=$G(PARAMS("PHYSICIAN"))
 I $D(PARAMS("ORDERING PROVIDER")) S FDA(1,6002.03,VBECIEN,5)=$G(PARAMS("ORDERING PROVIDER"))
 I $D(PARAMS("PRODUCT NAME")) S FDA(1,6002.03,VBECIEN,6)=$G(PARAMS("PRODUCT NAME"))
 I $D(PARAMS("COMPONENT ABBREVIATION")) S FDA(1,6002.03,VBECIEN,7)=$G(PARAMS("COMPONENT ABBREVIATION"))
 I $D(PARAMS("NUMBER OF UNITS")) S FDA(1,6002.03,VBECIEN,8)=$G(PARAMS("NUMBER OF UNITS"))
 I $D(PARAMS("TRANSFUSION DATE")) S FDA(1,6002.03,VBECIEN,9)=+$G(PARAMS("TRANSFUSION DATE"))
 S FDA(1,6002.03,VBECIEN,10)=$$NOW^XLFDT
 I $D(PARAMS("VOLUME")) S FDA(1,6002.03,VBECIEN,11)=$G(PARAMS("VOLUME"))
 I $D(PARAMS("REACTION TYPE")) S FDA(1,6002.03,VBECIEN,12)=$E($G(PARAMS("REACTION TYPE")),1,10) ;Limit input per conversation with David on 1/19
 I $D(PARAMS("UNIT MODIFICATION")) S FDA(1,6002.03,VBECIEN,13)=$E($G(PARAMS("UNIT MODIFICATION")),1,6) ;Limit input per conversation with David on 1/19
 I $D(PARAMS("REACTION")) S FDA(1,6002.03,VBECIEN,14)=$S($G(PARAMS("REACTION"))="True":"1",$G(PARAMS("REACTION"))="False":"",1:"") ;Leave room for a False code if necessary.
 I 'VBECON D UPDATE^DIE("","FDA(1)",,"VBERR") ;W !,"NEW" ;For new record
 I VBECON S:$G(FDA(1,6002.03,VBECIEN,1)) FDA(1,6002.03,VBECIEN,1)="`"_FDA(1,6002.03,VBECIEN,1) D FILE^DIE("E","FDA(1)","VBERR") ;W !,"UPDATE" zw  ;To update existing record
 D RESULT(1)
 Q
 ;
TEST ; Test the update
 ;36785EB0-8BCF-4407-A6C8-0002329A4ABF
 ;1375FB52-EC30-453D-8FC7-00026DA1229A
 ;8EE67641-0BA6-4A0C-84B4-0003E3F95011
 ;AD082B9A-6EC8-4EA9-9B99-0005AA8FFBD7
 ;09F8325B-0E93-4EB8-A315-000615AF3F9C
 ;C6F51B1E-11B7-49F9-B919-000AA5E3EC82
 ;5B8CC626-C32D-4AE2-B13D-000A65A3C04E
 ;411AF601-822E-4F38-A0CE-001661042A6E
 ;E66DD96E-AA27-4F12-96F6-00171F5D13EE
 S PARAMS("TRANSACTION ID")="E66DD96E-AA27-4F12-96F6-00171F5D13EE"
 S PARAMS("DFN")=51
 ;S PARAMS("NAME")="BILLYFRED"
 S PARAMS("FACILITY")="589   "
 S PARAMS("PHYSICIAN")="DOC,ONE"
 S PARAMS("ORDERING PROVIDER")=53296
 S PARAMS("PRODUCT NAME")="RED BLOOD CELLS"
 S PARAMS("COMPONENT ABBREVIATION")="RBC"
 S PARAMS("NUMBER OF UNITS")=6
 S PARAMS("TRANSFUSION DATE")=DT
 S PARAMS("VOLUME")="500"
 S PARAMS("REACTION TYPE")="FEBRILE"
 S PARAMS("UNIT MODIFICATION")="IWG"
 S PARAMS("REACTION")="True"
 Q
 ;
RESULT(VALUE) ; -- Add return value
 SET VBECLINE=VBECLINE+1
 SET @VBECRSLT@(VBECLINE)="<ReturnValue><SuccessIndicator>"_VALUE_"</SuccessIndicator></ReturnValue>"
 QUIT
TSTRIP(X) ;Strip trailing spaces
 F  Q:X'?.E1" "  S X=$E(X,1,$L(X)-1)
 Q X
