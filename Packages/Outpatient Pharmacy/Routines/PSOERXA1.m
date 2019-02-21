PSOERXA1 ;ALB/BWF - eRx Utilities/RPC's ; 8/3/2016 5:14pm
 ;;7.0;OUTPATIENT PHARMACY;**467,520,508**;DEC 1997;Build 295
 ;
 Q
 ; File incoming XML into appropriate file
 ; XML - xml text
 ; PRCHK - provider check information
 ; PACHK - patient check information
 ; DACHK - drug auto check
 ; STATION - station #
 ; DIV - institution name^NPI
 ; ERXHID - eRx processing hub id^CANCEL/CHANGE REQUEST DENIED BY HUB (1=YES)
 ; ERXVALS - code values for NIST codes
 ; XML2 - structured sig from the medication prescribed segment
 ; VADAT - DUZ^RXIEN
INCERX(RES,XML,PRCHK,PACHK,DACHK,STATION,DIV,ERXHID,ERXVALS,XML2,VADAT) ;
 N CURREC,FDA,EIEN,ERRTXT,ERRSEQ,PACNT,PASCNT,PAICN,PAIEN,VAINST,NPI,VAOI,VPATINST
 S NPI=$P($G(DIV),U,2)
 S CURREC=$$PARSE(.XML,.ERXVALS,NPI,.XML2)
 I $P(CURREC,U)<1 D  Q
 .I $L($P(CURREC,U,2)) S RES=CURREC Q
 .S RES="0^XML received. Error creating or finding associated record in the ERX Holding queue." Q
 S EIEN=CURREC
 S CURREC=CURREC_","
 ; if this is an outbound message, file the users DUZ and quit back the response. no drug, patient, or provider auto checks will occur
 I $D(VADAT) D  Q
 .I $P($G(VADAT),U)>1 D
 ..S FDA(52.49,CURREC,51.1)=DUZ D FILE^DIE(,"FDA") K FDA
 .I $P(VADAT,U,2) D
 ..S FDA(52.49,CURREC,.13)=$P(VADAT,U,2) D FILE^DIE(,"FDA") K FDA
 .S RES="1^Erx Received."
 ; Process auto-validation results. only log positive results for now
 K FDA
 I $P($G(VADAT),U) S RES="1^Message Filed." Q
 I $G(DACHK("success"))="true" D
 .I $G(DACHK("IEN")) D
 ..S FDA(52.49,CURREC,1.4)=1
 ..S FDA(52.49,CURREC,3.2)=DACHK("IEN")
 ..S FDA(52.49,CURREC,44)=1
 ..S VAOI=$$GET1^DIQ(50,DACHK("IEN"),2.1,"I")
 ..S VPATINST=$$GET1^DIQ(50.7,VAOI,7,"E")
 ..I $L(VPATINST) S FDA(52.49,CURREC,27)=VPATINST
 I $G(DACHK("success"))="false" D
 .S ERRTXT=$G(DACHK("error"))
 .S ERRSEQ=$$ERRSEQ^PSOERXU1(EIEN) Q:'ERRSEQ
 .D FILERR^PSOERXU1(CURREC,ERRSEQ,"D","E",ERRTXT)
 I $G(PRCHK("success"))="true" D
 .I PRCHK("IEN") D
 ..S FDA(52.49,CURREC,1.2)=1
 ..S FDA(52.49,CURREC,2.3)=PRCHK("IEN")
 I $G(PRCHK("success"))="false" D
 .S ERRTXT=$G(PRCHK("error"))
 .S ERRSEQ=$$ERRSEQ^PSOERXU1(EIEN) Q:'ERRSEQ
 .D FILERR^PSOERXU1(CURREC,ERRSEQ,"PR","E",ERRTXT)
 I $G(PACHK("MVIerror"))']"" D
 .S PAICN=+$P($G(PACHK("ICN")),"V")
 .I PAICN D
 ..S (PAIEN,PACNT)=0 F  S PAIEN=$O(^DPT("AICN",PAICN,PAIEN)) Q:'PAIEN  D
 ...S PACNT=PACNT+1
 ...; revisit in future build - if we find more than one match in the local system, do we log some sort of an error?
 .I $G(PACNT)=1 D  Q
 ..S FDA(52.49,CURREC,1.6)=1
 ..S FDA(52.49,CURREC,.05)=$O(^DPT("AICN",PAICN,0))
 .I $L(PACHK("ssn")) D
 ..S (PASCNT,PAIEN)=0 F  S PAIEN=$O(^DPT("SSN",$TR(PACHK("ssn"),"-",""),PAIEN)) Q:'PAIEN  D
 ...S PASCNT=PASCNT+1
 .I $G(PASCNT)=1 D  Q
 ..S FDA(52.49,CURREC,1.6)=1
 ..S FDA(52.49,CURREC,.05)=$O(^DPT("SSN",$TR(PACHK("ssn"),"-",""),0))
 I $D(FDA) D FILE^DIE(,"FDA") K FDA
 I $G(PACHK("success"))="false" D
 .; file e&e error
 .S ERRTXT=$G(PACHK("EandEerror")) I ERRTXT]"" D
 ..S ERRSEQ=$$ERRSEQ^PSOERXU1(EIEN) Q:'ERRSEQ
 ..D FILERR^PSOERXU1(CURREC,ERRSEQ,"PA","E",ERRTXT)
 .; file mvi error
 .S ERRTXT=$G(PACHK("MVIerror")) I ERRTXT]"" D
 ..S ERRSEQ=$$ERRSEQ^PSOERXU1(EIEN) Q:'ERRSEQ
 ..D FILERR^PSOERXU1(CURREC,ERRSEQ,"PA","E",ERRTXT)
 S RES="1^Erx Received."
 Q
PARSE(STREAM,ERXVALS,NPI,STREAM2) ;
 N %XML,GL,VAINST,MTYPE,HUBDENY
 S GL=$NA(^TMP($J,"PSOERXO1"))
 K @GL
 N STATUS,READER,XOBERR,S,ATTR,READER2,XOBERR2,STATUS2
 S STREAM=$TR(STREAM,"^","")
 I $L(STREAM2) S STREAM2=$TR(STREAM2,"^","")
 S STATUS=##class(%XML.TextReader).ParseStream(STREAM,.READER,,,,,1)
 I $L(STREAM2) S STATUS2=##class(%XML.TextReader).ParseStream(STREAM2,.READER2,,,,,1)
 I $$STATCHK^XOBWLIB(STATUS,.XOBERR,1) D
 .N BREAK
 .S BREAK=0 F  Q:BREAK||READER.EOF||'READER.Read()  D
 ..N X,PUSHED,PARENT
 ..I READER.AttributeCount D
 ...S PARENT=READER.LocalName
 ...D SPUSH(.S,PARENT) S PUSHED=1
 ...F ATTR=1:1:READER.AttributeCount D
 ....D READER.MoveToAttributeIndex(ATTR)
 ....I READER.NodeType="attribute" D APUT(.S,READER.Value,READER.LocalName)
 ..I READER.NodeType="element",'$G(PUSHED) D SPUSH(.S,READER.LocalName)
 ..; PSO*7*508 - if the type is an element, and is an empty element, put it in the global.
 ..I READER.NodeType="element",READER.IsEmptyElement D SPUT(.S,"")
 ..I READER.NodeType="endelement" D SPOP(.S,.X)
 ..I READER.NodeType="chars" D SPUT(.S,READER.Value)
 I $D(STATUS2) D
 .I $$STATCHK^XOBWLIB(STATUS2,.XOBERR2,1) D
 ..N BREAK,S
 ..S BREAK=0 F  Q:BREAK||READER2.EOF||'READER2.Read()  D
 ...N X,PUSHED,PARENT
 ...I READER2.AttributeCount D
 ....S PARENT=READER2.LocalName
 ....D SPUSH(.S,PARENT) S PUSHED=1
 ....F ATTR=1:1:READER2.AttributeCount D
 .....D READER2.MoveToAttributeIndex(ATTR)
 .....I READER2.NodeType="attribute" D APUT(.S,READER2.Value,READER2.LocalName)
 ...I READER2.NodeType="element",'$G(PUSHED) D SPUSH(.S,READER2.LocalName)
 ...; PSO*7*508 - if the type is an element, and is an empty element, put it in the global.
 ...I READER.NodeType="element",READER.IsEmptyElement D SPUT(.S,"")
 ...I READER2.NodeType="endelement" D SPOP(.S,.X)
 ...I READER2.NodeType="chars" D SPUT(.S,READER2.Value)
 S MTYPE=$O(^TMP($J,"PSOERXO1","Message",0,"Body",0,"")) Q:MTYPE']"" "0^Message type could not be identified."
 ;I '$L(NPI) S NPI=$G(^TMP($J,"PSOERXO1","Message",0,"Body",0,"NewRx",0,"Pharmacy",0,"Identification",0,"NPI",0))
 I '$L(NPI) S NPI=$G(^TMP($J,"PSOERXO1","Message",0,"Body",0,MTYPE,0,"Pharmacy",0,"Identification",0,"NPI",0))
 I '$L(NPI) Q "0^Missing NPI. Institution could not be resolved. eRx not filed."
 S VAINST=$$FIND1^DIC(4,,"O",NPI,"ANPI")
 I '$G(VAINST) Q "0^Institution could not be resolved. eRx not filed."
 N NERXIEN,ERR,PATIEN
 S NERXIEN=$$HDR(MTYPE)
 I $P(NERXIEN,U)<1 Q NERXIEN
 I $G(VAINST) S FDA(52.49,NERXIEN_",",24.1)=VAINST D FILE^DIE(,"FDA") K FDA
 ; if message type is 'Error', do not try to file the other components.
 I MTYPE["Error" D  Q NERXIEN
 .S PATIEN=$$GETPAT^PSOERXU5(NERXIEN) Q:'PATIEN
 .S FDA(52.49,NERXIEN_",",.04)=PATIEN D FILE^DIE(,"FDA") K FDA
 D PAT(NERXIEN,MTYPE),BFC^PSOERXA5(NERXIEN),PHR^PSOERXA2(NERXIEN,MTYPE),PRE^PSOERXA2(NERXIEN,MTYPE)
 D MED^PSOERXA3(NERXIEN,.ERXVALS,MTYPE),OBS(NERXIEN,MTYPE),SUP^PSOERXA2(NERXIEN,MTYPE)
 D MEDDISP^PSOERXA5(NERXIEN,MTYPE)
 I MTYPE="RefillResponse" D REFRESP^PSOERXA5(NERXIEN,MTYPE)
 I MTYPE["Cancel" D
 .S HUBDENY=$P(ERXHID,U,2)
 .D CANRX^PSOERXA5(NERXIEN,MTYPE,HUBDENY,VAINST)
 ; facility/request have no where to go at this point in time??
 ;/BLB PSO*7.0*520 - BEGIN CHANGE
 D FAC^PSOERXA2(NERXIEN)
 ;/BLB/ - END CHANGE
 Q NERXIEN
HDR(MTYPE) ; header information
 N GL,GL2,FQUAL,TQUAL,FROM,TO,MID,PONUM,SRTID,SSTID,SENTTIME,RTMID,FDA,ERXIEN,FMID,NEWERX,MES,ERXIENS,SSSID,SRSID,MTVALS
 N UPMTYPE,DONE,I,ERXISTAT,MTCODE,COMPSTR,RTHID,RTHIEN,RTMIEN
 S GL=$NA(^TMP($J,"PSOERXO1","Message",0,"Header",0))
 S GL2=$NA(^TMP($J,"PSOERXO1","Message","A","Qualifier","Header","A","Qualifier"))
 ; from and to qualifiers
 S FQUAL=$G(@GL2@("From","A","Qualifier"))
 S TQUAL=$G(@GL2@("To","A","Qualifier"))
 ; from, to, message id, prescriber order number
 S FROM=$G(@GL@("From",0))
 S TO=$G(@GL@("To",0))
 S MID=$G(@GL@("MessageID",0))
 ; set up the full message id
 S FMID=MID
 S ERXIENS="+1,"
 ; quit and return a message back if this eRx exists.
 I $D(^PS(52.49,"FMID",$P(ERXHID,U))) D  Q MES
 .S MES="0^This message already exists. Changes must occur via a change request XML message."
 S PONUM=$G(@GL@("PrescriberOrderNumber",0))
 ; security receiver tertiary identification
 S SRSID=$G(@GL@("Security",0,"Receiver",0,"SecondaryIdentification",0))
 S SRTID=$G(@GL@("Security",0,"Receiver",0,"TertiaryIdentification,",0))
 ; security sender tertiary identification
 S SSSID=$G(@GL@("Security",0,"Sender",0,"SecondaryIdentification",0))
 S SSTID=$G(@GL@("Security",0,"Sender",0,"TertiaryIdentification,",0))
 ; convert senttime to file manager dt/tm
 S SENTTIME=$G(@GL@("SentTime",0)),SENTTIME=$$CONVDTTM^PSOERXA1(SENTTIME)
 S RTMID=$G(@GL@("RelatesToMessageID",0))
 S RTHID=$P(ERXHID,U,3)
 S RTHIEN=""
 I $L(RTHID) S RTHIEN=$O(^PS(52.49,"FMID",RTHID,0))
 D FIELD^DID(52.49,.08,"","POINTER","MTVALS")
 S UPMTYPE=$$UP^XLFSTR(MTYPE)
 S DONE=0
 F I=1:1 D  Q:DONE
 .S COMPSTR=$P(MTVALS("POINTER"),";",I)
 .I COMPSTR="" S DONE=1 Q
 .I COMPSTR[UPMTYPE S MTCODE=$P(COMPSTR,":"),DONE=1
 I $G(MTCODE)']"" Q "0^Message type could not be resolved."
 S FDA(52.49,ERXIENS,.08)=MTCODE
 ; erx hub message id
 S FDA(52.49,ERXIENS,.01)=$P(ERXHID,U)
 ; change healthcare message id
 S FDA(52.49,ERXIENS,25)=FMID
 S FDA(52.49,ERXIENS,.02)=RTMID
 S FDA(52.49,ERXIENS,.03)=$$NOW^XLFDT
 S FDA(52.49,ERXIENS,.09)=PONUM
 ;RELATES TO HUB ID
 S FDA(52.49,ERXIENS,.14)=RTHID
 S ERXISTAT=$$GETSTAT^PSOERXU2(MTCODE,RTHIEN,RTMID)
 S FDA(52.49,ERXIENS,1)=ERXISTAT
 S FDA(52.49,ERXIENS,22.1)=FROM
 S FDA(52.49,ERXIENS,22.2)=FQUAL
 S FDA(52.49,ERXIENS,22.3)=TO
 S FDA(52.49,ERXIENS,22.4)=TQUAL
 S FDA(52.49,ERXIENS,22.5)=SENTTIME
 S FDA(52.49,ERXIENS,24.3)=SSSID
 S FDA(52.49,ERXIENS,24.4)=SSTID
 S FDA(52.49,ERXIENS,24.5)=SRSID
 S FDA(52.49,ERXIENS,24.6)=SRTID
 ; if this is an existing record, file the updates to the erx and return the IEN
 D UPDATE^DIE(,"FDA","NEWERX","EERR") K FDA
 S ERXIEN=""
 S ERXIEN=$O(NEWERX(0)),ERXIEN=$G(NEWERX(ERXIEN))
 I 'ERXIEN Q ""
 I $G(RTHIEN)]"" D
 .N REFREQ,NRXIEN
 .S NRXIEN=$$FINDNRX^PSOERXU6(ERXIEN)
 .I MTCODE="RE" D
 ..S REFREQ=$$GETREQ^PSOERXU2(ERXIEN)
 ..I REFREQ S NRXIEN=$$FINDNRX^PSOERXU6(REFREQ)
 ..I $D(^PS(52.49,NRXIEN,201,"B",ERXIEN)) Q
 ..I $G(NRXIEN) S FDA2(52.49201,"+1,"_NRXIEN_",",.01)=ERXIEN D UPDATE^DIE(,"FDA2") K FDA2
 .; link this message to the original
 .I $G(NRXIEN) D
 ..I $D(^PS(52.49,NRXIEN,201,"B",ERXIEN)) Q
 ..S FDA2(52.49201,"+1,"_NRXIEN_",",.01)=ERXIEN D UPDATE^DIE(,"FDA2") K FDA2
 .I '$D(^PS(52.49,RTHIEN,201,"B",ERXIEN)) D
 ..S FDA2(52.49201,"+1,"_RTHIEN_",",.01)=ERXIEN D UPDATE^DIE(,"FDA2") K FDA2
 .; link original message to this erxien
 .I '$D(^PS(52.49,ERXIEN,201,"B",RTHIEN)) D
 ..S FDA2(52.49201,"+1,"_ERXIEN_",",.01)=RTHIEN D UPDATE^DIE(,"FDA2") K FDA2
 I MTYPE["Error" D ERR^PSOERXU2(ERXIEN,MTYPE)
 ; Future consideration - XSD shows digital signature. Do we need to collect this?
 Q ERXIEN
OBS(ERXIEN,MTYPE) ; Observation
 N GL,I,LAST,DIM,MSOURCE,MUNIT,OBSDT,MVAL,OBSNOTE,OBSCNT,F,EIENS,FDA
 S GL=$NA(^TMP($J,"PSOERXO1","Message",0,"Body",0,MTYPE,0,"Observation",0))
 S F=52.4914,EIENS=ERXIEN_","
 S I=-1,OBSCNT=0 F  S I=$O(@GL@("Measurement",I)) Q:I=""  D
 .S OBSCNT=OBSCNT+1,FDA(F,"+1,"_EIENS,.01)=OBSCNT
 .S DIM=$G(@GL@("Measurement",I,"Dimension",0)),FDA(F,"+1,"_EIENS,.02)=DIM
 .S MSOURCE=$G(@GL@("Measurement",I,"MeasurementSourceCode",0)),FDA(F,"+1,"_EIENS,.06)=MSOURCE
 .S MUNIT=$G(@GL@("Measurement",I,"MeasurementUnitCode",0)),FDA(F,"+1,"_EIENS,.07)=MUNIT
 .; Future enhancement - we have a field for MeasurementDataQualifier (.05) - is this valid?
 .S OBSDT=$G(@GL@("Measurement",I,"ObservationDate",0,"Date",0)),OBSDT=$$CONVDTTM^PSOERXA1(OBSDT),FDA(F,"+1,"_EIENS,.04)=OBSDT
 .S MVAL=$G(@GL@("Measurement",I,"Value",0)),FDA(F,"+1,"_EIENS,.03)=MVAL
 .D UPDATE^DIE(,"FDA") K FDA
 S OBSNOTE=$G(@GL@("ObservationNotes",0)),FDA(52.49,EIENS,15)=OBSNOTE D FILE^DIE(,"FDA") K FDA
 Q
PAT(ERXIEN,MTYPE) ; patient
 N GL,AL1,AL2,CITY,STATE,ZIP,LN,FN,MN,PREF,SUFF,COMQUAL,COMVAL,PLQUAL,DOB,GEN,PRELATE,IDDONE,CDONE,I,C,CQUAL,CVAL
 N IDNM,IDVAL,PFN,ERXPAT,NEWPAT,F,EIENS,FDA,IDFND,SRCH,PIENS,NPIEN,PATSSN,PREL,SIEN
 S F=52.46
 S EIENS=ERXIEN_","
 S GL=$NA(^TMP($J,"PSOERXO1","Message",0,"Body",0,MTYPE,0,"Patient",0))
 S PREL=$G(@GL@("PatientRelationship",0))
 S FN=$$UP^XLFSTR($G(@GL@("Name",0,"FirstName",0)))
 S LN=$$UP^XLFSTR($G(@GL@("Name",0,"LastName",0)))
 S MN=$$UP^XLFSTR($G(@GL@("Name",0,"MiddleName",0)))
 S PFN=LN_","_FN_$S(MN]"":" "_MN,1:"")
 S SUFF=$$UP^XLFSTR($G(@GL@("Name",0,"Suffix",0)))
 S PREF=$$UP^XLFSTR($G(@GL@("Name",0,"Prefix",0)))
 S PRELATE=$G(@GL@("PatientRelationship",0))
 S GEN=$G(@GL@("Gender",0))
 S DOB=$G(@GL@("DateOfBirth",0,"Date",0)),DOB=$$CONVDTTM^PSOERXA1(DOB)
 I DOB<1 S DOB=""
 S AL1=$G(@GL@("Address",0,"AddressLine1",0))
 S AL2=$G(@GL@("Address",0,"AddressLine2",0))
 S CITY=$G(@GL@("Address",0,"City",0))
 S STATE=$G(@GL@("Address",0,"State",0))
 S ZIP=$G(@GL@("Address",0,"ZipCode",0))
 S SIEN=$$STRES^PSOERXA2(ZIP,STATE)
 ; need to check for SSN before trying to match the patient. This needs to be stored in an array for later processing
 ; check 52.46 for a match before filing
 S PATSSN=$G(@GL@("Identification",0,"SocialSecurity",0))
 S ERXPAT=$$FINDPAT^PSOERXU2(PFN,DOB,GEN,$G(PATSSN),$G(AL1)) S PIENS=$S(ERXPAT:ERXPAT_",",1:"+1,")
 ; first, lets set up the main part
 S FDA(F,PIENS,.01)=PFN,FDA(F,PIENS,.02)=LN,FDA(F,PIENS,.03)=FN,FDA(F,PIENS,.04)=MN,FDA(F,PIENS,.05)=SUFF,FDA(F,PIENS,.06)=PREF
 S FDA(F,PIENS,.07)=GEN,FDA(F,PIENS,.08)=DOB
 S FDA(F,PIENS,1.4)=PATSSN,FDA(F,PIENS,1.7)=PREL
 S FDA(F,PIENS,3.1)=AL1,FDA(F,PIENS,3.2)=AL2,FDA(F,PIENS,3.3)=CITY
 S FDA(F,PIENS,3.4)=SIEN
 S FDA(F,PIENS,3.5)=ZIP
 I PIENS["+" D  Q
 .D UPDATE^DIE(,"FDA","NEWPAT") K FDA
 .S NPIEN=$O(NEWPAT(0)),NPIEN=$G(NEWPAT(NPIEN))
 .Q:'NPIEN
 .S NPIEN=NPIEN
 .D PATC(NPIEN)
 .S FDA(52.49,EIENS,.04)=NPIEN D FILE^DIE(,"FDA") K FDA
 D FILE^DIE(,"FDA") K FDA D PATC(ERXPAT)
 S FDA(52.49,EIENS,.04)=ERXPAT D FILE^DIE(,"FDA") K FDA
 Q
PATC(IEN) ; patient communication
 N IENS,CQUAL,CVAL,COMARY,FDA,SRCH,IDFND,IDNM,IDVAL,IDARY,PATSSN
 Q:'IEN
 S IENS=IEN_","
 ; Kill off existing communication values
 K ^PS(52.46,IEN,3)
 S C=-1 F  S C=$O(@GL@("CommunicationNumbers",0,"Communication",C)) Q:C=""  D
 .S CQUAL=$G(@GL@("CommunicationNumbers",0,"Communication",C,"Qualifier",0))
 .S CVAL=$G(@GL@("CommunicationNumbers",0,"Communication",C,"Number",0))
 .S COMARY(CQUAL)=CVAL
 .S FDA(52.462,"+1,"_IENS,.01)=CVAL
 .S FDA(52.462,"+1,"_IENS,.02)=CQUAL
 .D UPDATE^DIE(,"FDA") K FDA
 ; kill existing identification values in multiple
 K ^PS(52.46,IEN,5)
 S IDNM="" F  S IDNM=$O(@GL@("Identification",0,IDNM)) Q:IDNM=""  D
 .S IDVAL=$G(@GL@("Identification",0,IDNM,0))
 .I IDNM="SocialSecurity" S PATSSN=IDVAL
 .S IDARY(IDNM)=IDVAL
 .S IDFND=0
 .S SRCH=0 F  S SRCH=$O(^PS(52.46,IEN,5,SRCH)) Q:'SRCH  D
 ..I $$GET1^DIQ(52.465,SRCH_","_IEN_",",.01)=IDNM D
 ...S IDFND=1
 ...S FDA(52.465,SRCH_","_IEN_",",.02)=IDVAL D FILE^DIE(,"FDA") K FDA
 .Q:IDFND
 .S FDA(52.465,"+1,"_IEN_",",.01)=IDNM
 .S FDA(52.465,"+1,"_IEN_",",.02)=IDVAL
 .D UPDATE^DIE(,"FDA") K FDA
 I $G(PATSSN)]"" S FDA(52.46,IENS,1.4)=PATSSN D FILE^DIE(,"FDA") K FDA
 Q
SPUSH(S,X) ;places X on the stack S and returns the current level of the stack
 N I S I=$O(S(""),-1)+1,S(I)=X
 Q I
 ;
SPOP(S,X) ;removes the top item from the stack S and put it into the variable X and returns the level that X was at
 N I S I=$O(S(""),-1)
 I I S X=S(I) K S(I)
 N J S J=$O(S(I),-1) I J S S(J,X)=$G(S(J,X))+1
 Q I
 ;
SPEEK(S,X) ;same as SPOP except the top item is not removed
 N I S I=$O(S(""),-1)
 I I S X=S(I)
 Q I
 ;
SPUT(S,X) ;implementation specific, uses the stack to form a global node
 N I,STR
 S X=$TR(X,";","")
 S STR=$P(GL,")")
 S I=0 F  S I=$O(S(I)) Q:'I  D
 .S STR=STR_","_""""_S(I)_""""_","
 .N NUM S NUM=0
 .I $D(S(I-1,S(I))) S NUM=+$G(S(I-1,S(I)))
 .S STR=STR_NUM
 S STR=STR_")"
 I $D(@STR) S @STR=@STR_X
 I '$D(@STR) S @STR=X
 Q STR
APUT(S,X,LN) ; what am i doing here?
 N I,STR
 S X=$TR(X,";","")
 S STR=$P(GL,")")
 S I=0 F  S I=$O(S(I)) Q:'I  D
 .S STR=STR_","_""""_S(I)_""""_","
 .N NUM S NUM="""A"""
 .;I $D(S(I-1,S(I))) S NUM=+$G(S(I-1,S(I)))
 .;S STR=STR_NUM
 .S STR=STR_NUM_","_""""_LN_""""
 S STR=STR_")"
 I $D(@STR) S @STR=@STR_X
 I '$D(@STR) S @STR=X
 Q STR
 ;
 ; VAL - value to resolve
 ; TYPE - This is the code type, which will tell which 'C' index type to get the code from
PRESOLV(VAL,TYPE) ;
 N MATCH
 S MATCH=""
 Q:'$L(TYPE)!('$L(VAL)) "" ; avoid null subscript
 S MATCH=$O(^PS(52.45,"C",TYPE,VAL,0))
 ; return the match found, null if no match
 Q MATCH
CONVDTTM(VAL) ;
 N EDATE,ETIME,X,ETZ,Y
 I '$L(VAL) Q ""
 S EDATE=$P(VAL,"T"),ETIME=$P(VAL,"T",2)
 ; split off time zone
 S ETZ=$P(ETIME,".",2)
 S ETIME=$P(ETIME,".")
 S X=EDATE D ^%DT I 'Y Q ""
 S VAL=Y_$S($L(ETIME):"."_$TR(ETIME,":",""),1:"")
 Q VAL
