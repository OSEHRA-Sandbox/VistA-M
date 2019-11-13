MPIFXMLI ;ALB/CKN - MPIF IMPLICIT/EXPLICIT ADD ; 9/6/19 5:05pm
 ;;1.0;MASTER PATIENT INDEX VISTA;**61,71**;30 Apr 99;Build 2
 Q
GETICN(MPIDATA,MPIARR) ; Explicit add for user traits - ICN is returned
 N MPIXML,MPIXMLR
 K MPIDATA
 S MPIXML=$$XMLBLD(.MPIARR)
 D POST^MPIFHWSC(MPIXML,.MPIXMLR)
 I '$D(MPIXMLR) S MPIDATA("ICN")=-1,MPIDATA("ERRTXT")="No results returned" Q
 D PARSE(.MPIDATA,.MPIXMLR)
 Q
 ;
XMLBLD(MPIARR) ; setup xml for explicit Add
 ; MPIARR - Array contains traits for ADD
 ;    Returns XML for explicit ADD
 N MPIXML,MPISITE,QUOTE,MPITHRES,MPIDT,MPIDUZ,MPIPRID,ADDTYPE,PATARR
 S QUOTE="""",ADDTYPE=$S(MPIARR("AddType")="Explicit":"ADD ICN OVERRIDE",1:"ADD ICN") ;Setting add type on implicit or explicit flag
 M PATARR=MPIARR(1)
 S MPISITE=$P($$SITE^VASITE,"^",3)
 S MPIPRID=$P($$PARAM^HLCS2,"^",3)
 S MPIDT=$$FMTHL7^XLFDT($$NOW^XLFDT)
 S MPIDUZ=$P(^VA(200,DUZ,0),"^") D STDNAME^XLFNAME(.MPIDUZ,"C")
 S MPITHRES=80
 ;
 S MPIXML="<IDM_REQUEST type="_QUOTE_"ADD_PROFILE"_QUOTE_"><METADATA>"
 S MPIXML=MPIXML_"<FIELD name="_QUOTE_"PROCESSINGID"_QUOTE_" value="
 S MPIXML=MPIXML_QUOTE_MPIPRID_QUOTE_"/><FIELD name="_QUOTE_"SENDINGFACILITY"
 S MPIXML=MPIXML_QUOTE_" value="_QUOTE_MPISITE_QUOTE_"/><FIELD name="_QUOTE
 S MPIXML=MPIXML_"SENDINGAPPLICATIONNAME"_QUOTE_" value="_QUOTE_"VistAEnterpriseReg"_QUOTE
 I $G(MPIARR("mcid"))'="" D
 .S MPIXML=MPIXML_"/><FIELD name="_QUOTE_"attentionLine"_QUOTE
 .S MPIXML=MPIXML_" value="_QUOTE_MPIARR("mcid")_QUOTE ;search token
 ;**71 - Story 841885 (ckn)
 S MPIXML=MPIXML_"/><FIELD name="_QUOTE_"selectedIdentifier"_QUOTE
 S MPIXML=MPIXML_" value="_QUOTE_$G(MPIARR("SelIdentifier"))_QUOTE
 S MPIXML=MPIXML_"/></METADATA><IDMHEADER><SENDING_FACILITY>"_MPISITE
 S MPIXML=MPIXML_"</SENDING_FACILITY><PROCESSING_ID>"_MPIPRID_"</PROCESSING_ID>"
 S MPIXML=MPIXML_"<TRIGGER><ACTOR>"
 S MPIXML=MPIXML_"<IDENTIFIER type='PN'><ID>"_DUZ_"</ID><SOURCE>"_MPISITE_"</SOURCE>"
 S MPIXML=MPIXML_"<ISSUER>USVHA</ISSUER></IDENTIFIER><NAME type='U'>"
 S MPIXML=MPIXML_"<LASTNAME>"_$G(MPIDUZ("FAMILY"))_"</LASTNAME>"
 S MPIXML=MPIXML_"<FIRSTNAME>"_$G(MPIDUZ("GIVEN"))_"</FIRSTNAME>"
 S MPIXML=MPIXML_"</NAME></ACTOR></TRIGGER></IDMHEADER><ARGUMENTS>"
 S MPIXML=MPIXML_"<ARGUMENT name="_QUOTE_"ACTIVEPROFILE"_QUOTE_"><PROFILE>"
 ;Name Traits
 S MPIXML=MPIXML_"<NAME type="_QUOTE_"L"_QUOTE_">"
 D IFADD("FirstName",.PATARR,.MPIXML,"FIRSTNAME")
 D IFADD("MiddleName",.PATARR,.MPIXML,"MIDDLENAME")
 D IFADD("Suffix",.PATARR,.MPIXML,"SUFFIX")
 D IFADD("Surname",.PATARR,.MPIXML,"LASTNAME")
 S MPIXML=MPIXML_"</NAME>"
 ;Source ID
 S MPIXML=MPIXML_"<IDENTIFIER type='PI'><ID>"_$G(PATARR("DFN"))_"</ID>"
 S MPIXML=MPIXML_"<SOURCE>"_MPISITE_"</SOURCE><ISSUER>USVHA</ISSUER></IDENTIFIER>"
 I $G(PATARR("SSN"))'="" D
 .S MPIXML=MPIXML_"<IDENTIFIER type='SS' subtype='ACTIVE'>"
 .S MPIXML=MPIXML_"<ID>"_PATARR("SSN")_"</ID>"
 .S MPIXML=MPIXML_"<ISSUER>USSSA</ISSUER></IDENTIFIER>"
 I $G(PATARR("DOB"))'="" D
 .S MPIXML=MPIXML_"<ATTRIBUTE type='DOB'><VALUE>"_$$FMTHL7^XLFDT(PATARR("DOB"))
 .S MPIXML=MPIXML_"</VALUE></ATTRIBUTE>"
 I $G(PATARR("Gender"))'="" D
 .S MPIXML=MPIXML_"<ATTRIBUTE type='GENDER'><VALUE>"
 .S MPIXML=MPIXML_PATARR("Gender")_"</VALUE></ATTRIBUTE>"
 I $G(PATARR("MMN"))'="" D
 . S MPIXML=MPIXML_"<ATTRIBUTE type='MMN'>"
 . S MPIXML=MPIXML_"<VALUE>"_PATARR("MMN")_"</VALUE></ATTRIBUTE>"
 I $G(PATARR("MBI"))'="" D
 . S MPIXML=MPIXML_"<ATTRIBUTE type='MULTIBIRTH'>"
 . S MPIXML=MPIXML_"<VALUE>"_PATARR("MBI")_"</VALUE></ATTRIBUTE>"
 ;POB stuff
 S PATARR("MPIVar")=$$CONV($G(PATARR("POBCity")))
 I PATARR("MPIVar")'=""!($G(PATARR("POBState"))'="") D
 . S MPIXML=MPIXML_"<ADDRESS type='N'>"
 . D IFADD("MPIVar",.PATARR,.MPIXML,"CITY")
 . D IFADD("POBState",.PATARR,.MPIXML,"STATE")
 . S MPIXML=MPIXML_"</ADDRESS>"
 ;address stuff
 N PROVINCE,PCODE
 S PROVINCE=$G(PATARR("ResAddProvince")),PCODE=$G(PATARR("ResAddPCode"))
 I $G(PATARR("ResAddL1"))'=""!($G(PATARR("ResAddL2"))'="")!($G(PATARR("ResAddCity"))'="")!($G(PATARR("ResAddZip4"))'="")!($G(PATARR("ResAddL3"))'="")!($G(PATARR("ResAddState"))'="")!(PROVINCE'="")!(PCODE'="") D
 . S MPIXML=MPIXML_"<ADDRESS type='P'>"
 . S PATARR("MPIVar")=$$CONV($G(PATARR("ResAddL1")))
 . D IFADD("MPIVar",.PATARR,.MPIXML,"STREET1")
 . S PATARR("MPIVar")=$$CONV($G(PATARR("ResAddL2")))
 . D IFADD("MPIVar",.PATARR,.MPIXML,"STREET2")
 . S PATARR("MPIVar")=$$CONV($G(PATARR("ResAddL3")))
 . D IFADD("MPIVar",.PATARR,.MPIXML,"STREET3")
 . S PATARR("MPIVar")=$$CONV($G(PATARR("ResAddCity")))
 . D IFADD("MPIVar",.PATARR,.MPIXML,"CITY")
 . D IFADD("ResAddState",.PATARR,.MPIXML,"STATE")
 . D IFADD("ResAddZip4",.PATARR,.MPIXML,"ZIPCODE")
 . D IFADD("ResAddProvince",.PATARR,.MPIXML,"PROVINCECODE")
 . D IFADD("ResAddPCode",.PATARR,.MPIXML,"POSTALCODE")
 . D IFADD("ResAddCountry",.PATARR,.MPIXML,"COUNTRY")
 . S MPIXML=MPIXML_"</ADDRESS>"
 ; phone
 I $G(PATARR("ResPhone"))'=""&($G(PATARR("ResPhone"))'["""") D
 . S PATARR("MPIVar")=$$CONV($G(PATARR("ResPhone")))
 . I PATARR("MPIVar")'="" D
 .. S MPIXML=MPIXML_"<PHONE type='HOME'><NUMBER>"
 .. S MPIXML=MPIXML_PATARR("MPIVar")_"</NUMBER></PHONE>"
 ; date of death
 I $G(PATARR("DOD"))'="" D
 . S MPIXML=MPIXML_"<ATTRIBUTE type='DOD'><VALUE>"
 . S MPIXML=MPIXML_$$FMTHL7^XLFDT(PATARR("DOD"))_"</VALUE></ATTRIBUTE>"
 S MPIXML=MPIXML_"</PROFILE></ARGUMENT>"
 S MPIXML=MPIXML_"<ARGUMENT name="_QUOTE_"ADDTYPE"_QUOTE_">"
 S MPIXML=MPIXML_"<VALUE>"_ADDTYPE_"</VALUE></ARGUMENT>"
 S MPIXML=MPIXML_"</ARGUMENTS></IDM_REQUEST>"
 Q MPIXML
 ;
IFADD(MPIVAR,PATARR,MPIXML,MPIXMLN) ;check if there, if so add it to the XML
 ; MPIVAR is the PATARR variable name
 ; MPIXMLN is the name of the XML to encase
 ; modifies MPIXML to add if it is there
 I $G(PATARR(MPIVAR))'="" D
 . S MPIXML=MPIXML_"<"_MPIXMLN_">"_PATARR(MPIVAR)_"</"_MPIXMLN_">"
 Q
CONV(FIELD) ;check for &, ', > and <
 I FIELD["&" S FIELD=$P(FIELD,"&")_"&amp;"_$P(FIELD,"&",2)
 I FIELD["'" S FIELD=$P(FIELD,"'")_"&apos;"_$P(FIELD,"'",2)
 S:(FIELD["<") FIELD=$$CONVA(FIELD,"<")
 S:(FIELD[">") FIELD=$$CONVA(FIELD,">")
 Q FIELD
CONVA(FIELD,ENCHAR) ;handle <<pob city>>
 N I,X,VAL
 S VAL="",I=$L(FIELD,ENCHAR) F X=1:1:I S VAL=VAL_$P(FIELD,ENCHAR,X)
 Q VAL
PARSE(MPIDATA,MPIXML) ;Parse XML for results
 ;
 ; EN^MXMLPRSE - #4149
 ;
 K ^TMP($J,"MPIFXMLI")
 N MPICB,MPIUSE,MPIVAR,MPIIDN,MPILOC
 S MPICB("STARTELEMENT")="SE^MPIFXMLI"
 S MPICB("CHARACTERS")="VALUE^MPIFXMLI"
 S ^TMP($J,"MPIFXMLI",1)=MPIXML
 D EN^MXMLPRSE($NA(^TMP($J,"MPIFXMLI")),.MPICB)
 K ^TMP($J,"MPIFXMLI")
 Q
SE(MPIN,MPIA) ; - used for the parser to call back with STARTELEMENT
 ; just to protect the process
 S MPIN=$G(MPIN)
 S MPIA("type")=$G(MPIA("type"))
 ;No need for these tags
 I MPIN="IDM_RESPONSE"!(MPIN="RESULT")!(MPIN="ERROR") Q
 S MPILOC="MPIDATA("
 I MPIN="TEXT" D  Q
 . S MPIVAR="""ERRTXT"")",MPIDATA("ICN")=-1
 I MPIN="ID" S MPIVAR="""ICN"")" Q
 Q
 ;
VALUE(MPIT) ;used by the parser to call back with CHARACTERS
 S:$D(MPIVAR) @(MPILOC_MPIVAR)=MPIT K MPIVAR Q
 Q
