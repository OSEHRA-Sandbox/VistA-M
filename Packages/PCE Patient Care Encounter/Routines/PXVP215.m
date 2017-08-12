PXVP215 ;BPFO/LMT - PX*1*215 KIDS Routine ;04/05/2016
 ;;1.0;PCE PATIENT CARE ENCOUNTER;**215**;Aug 12, 1996;Build 10
 ;
POST ; KIDS Post install for PX*1*215
 D BMES("*** Post install started ***")
 ;
 S DIU=9000010.123,DIU(0)="S" D EN^DIU2 K DIU ; Delete Coding System multiple from file #9000010.12
 D CODEMAP ; Generate PCE Code Mapping Report
 D LOAD9206 ; Populate data for #920.6
 ;
 D BMES("*** Post install completed ***")
 Q
 ;
 ;---------------------------------------------------------------------
CODEMAP ; Generate PCE Code Mapping Report
 ;
 N PXERRLN,PXERRNUM,PXERRTXT,PXFROM,PXFROMEN,PXIEN,PXIMM,PXINCLUDE,PXINST,PXINSTNAME,PXINSTNUM
 N PXINSTR,PXLINE,PXMGROUP,PXMSGSUB,PXMSGTO,PXNODE,PXREMMGIEN,PXTO,PXTOEN,XMERR
 ;
 D BMES("*** Generating PCE CODE MAPPING Report ***")
 ;
 K ^TMP("PX215PCM",$J)
 ;
 S PXINST=$$KSP^XUPARAM("INST")
 S PXINSTNAME=$$NAME^XUAF4(PXINST)
 S PXINSTNUM=$$STA^XUAF4(PXINST)
 ;
 S PXLINE=10 ; Start at #10, as the preamble part of the message will be in lines 1-10
 ;
 S PXIEN=0
 F  S PXIEN=$O(^PXD(811.1,PXIEN)) Q:'PXIEN  D
 . S PXNODE=$G(^PXD(811.1,PXIEN,0))
 . ;
 . I '$P(PXNODE,U,5) Q
 . ;
 . S PXFROM=$P(PXNODE,U,3)
 . S PXINCLUDE=0
 . I PXFROM="CPT" S PXINCLUDE=1
 . I PXFROM="IMM" D
 . . S PXIMM=+$P(PXNODE,U,1)
 . . I $$IMMSTAT^PXAPIIM(PXIMM)'="A" S PXINCLUDE=1
 . ;
 . I 'PXINCLUDE Q
 . ;
 . S PXFROMEN=+$P(PXNODE,U,1)
 . S PXTOEN=+$P(PXNODE,U,2)
 . S PXTO=$P(PXNODE,U,4)
 . I PXFROM="IMM" S PXFROMEN=$P($G(^AUTTIMM(PXFROMEN,0)),U,1)
 . I PXFROM="CPT" S PXFROMEN=$$CODEC^ICPTCOD(PXFROMEN)
 . I (PXFROMEN="")!(PXFROMEN=-1) Q
 . I PXTO="IMM" S PXTOEN=$P($G(^AUTTIMM(PXTOEN,0)),U,1)
 . I PXTO="CPT" S PXTOEN=$$CODEC^ICPTCOD(PXTOEN)
 . S ^TMP("PX215PCM",$J,$$LINE())=PXINSTNUM_U_PXINSTNAME_U_PXFROM_U_PXFROMEN_U_PXTO_U_PXTOEN
 ;
 I '$D(^TMP("PX215PCM",$J)) D  Q
 . D MES("No entries to report.")
 ;
 S ^TMP("PX215PCM",$J,1)="This report was generated by the PX*1*215 Post-Install. It contains a list of"
 S ^TMP("PX215PCM",$J,3)="entries from the PCE CODE MAPPING file (#811.1) for your review."
 S ^TMP("PX215PCM",$J,4)=" "
 S ^TMP("PX215PCM",$J,5)="This report lists entries where both: "
 S ^TMP("PX215PCM",$J,6)="   1. The FROM field (#.03) equals ""CPT"" OR the immunization in the File Entry"
 S ^TMP("PX215PCM",$J,7)="      field (#.01) is inactive."
 S ^TMP("PX215PCM",$J,8)="   2. And the ON/OFF FLAG field (#.05) equals ""ON""."
 S ^TMP("PX215PCM",$J,9)=" "
 S ^TMP("PX215PCM",$J,10)="Station #^Station Name^From^From Code^To^To Code"
 ;
 S PXMSGSUB=PXINSTNUM_" PCE CODE MAPPING DATA FOR REVIEW"
 S PXMSGTO(DUZ)=""
 S PXREMMGIEN=$$GET1^DIQ(800,1_",",3)
 I PXREMMGIEN'="" D
 . S PXMGROUP="G."_$$GET1^DIQ(3.8,PXREMMGIEN_",",.01)
 . S PXMSGTO(PXMGROUP)=""
 I $$PROD^XUPROD(),$G(DUZ("AG"))="V" D
 . S PXMSGTO("TEITELBAUM.LEVI@FORUM.DOMAIN.EXT")=""
 . S PXMSGTO("SILVERMAN.ROBERT@FORUM.DOMAIN.EXT")=""
 . S PXMSGTO("BRYAN.VOLPP@DOMAIN.EXT")=""
 S PXINSTR("FROM")="PX*1*215 POST-INSTALL"
 S PXINSTR("ADDR FLAGS")="R"
 D SENDMSG^XMXAPI(DUZ,PXMSGSUB,"^TMP(""PX215PCM"",$J)",.PXMSGTO,.PXINSTR,.PXMSGNUM)
 D MES("Emailed report message (#"_$G(PXMSGNUM)_")")
 I $G(XMERR),$D(^TMP("XMERR",$J)) D
 . D MES("The following errors occurred while sending the email:")
 . S PXERRNUM=0
 . F  S PXERRNUM=$O(^TMP("XMERR",$J,PXERRNUM)) Q:'PXERRNUM  D
 . . S PXERRTXT="  "_PXERRNUM_". "
 . . S PXERRLN=0
 . . F  S PXERRLN=$O(^TMP("XMERR",$J,PXERRNUM,"TEXT",PXERRLN)) Q:'PXERRLN  D
 . . . S PXERRTXT=PXERRTXT_$G(^TMP("XMERR",$J,PXERRNUM,"TEXT",PXERRLN))
 . . . D MES(PXERRTXT)
 . . . S PXERRTXT="     "
 ;
 K ^TMP("PX215PCM",$J)
 ;
 Q
 ;
LINE() ;
 S PXLINE=PXLINE+1
 Q PXLINE
 ;-------------------------------------------------------------------------
 ;
LOAD9206 ; Populate data for #920.6
 ;
 N PXERRMSG,PXFDA,PXI,PXIENS,PXROUTE,PXROUTENM,PXROUTES,PXSITE,PXSITEHL,PXSITES
 ;
 D BMES("*** Populating data for #920.6 ***")
 ;
 I $O(^PXV(920.6,0)) D  Q
 . D MES("Data already populated... no need to populate again.")
 ;
 S PXROUTES("INTRADERMAL")="LA^LLFA^LT^RA^RLFA^RT"
 S PXROUTES("PERCUTANEOUS")="LA^LLFA^LT^RA^RLFA^RT"
 S PXROUTES("SUBCUTANEOUS")="LA^LLFA^LT^RA^RLFA^RT"
 S PXROUTES("INTRAMUSCULAR")="LD^LG^LVL^RD^RG^RVL^RVG^LVG"
 S PXROUTES("NASAL")="BN^LN^RN"
 S PXROUTES("ORAL")=""
 ;
 S PXROUTENM=""
 F  S PXROUTENM=$O(PXROUTES(PXROUTENM)) Q:PXROUTENM=""  D
 . K PXFDA,PXERRMSG,PXIENS
 . D CLEAN^DILF
 . ;
 . S PXROUTE=$O(^PXV(920.2,"B",PXROUTENM,0))
 . I 'PXROUTE D  Q
 . . D MES("Can't add mappings for route '"_PXROUTENM_"', as no #920.2 entry was found for it.")
 . ;
 . S PXFDA(1,920.6,"?+1,",.01)=PXROUTE
 . S PXIENS(1)=PXROUTE
 . ;
 . S PXSITES=$G(PXROUTES(PXROUTENM))
 . F PXI=1:1 S PXSITEHL=$P(PXSITES,U,PXI) Q:PXSITEHL=""  D
 . . S PXSITE=$O(^PXV(920.3,"B",PXSITEHL,0))
 . . I 'PXSITE D  Q
 . . . D MES("Can't add site '"_PXSITEHL_"', as no #920.3 entry was found for it.")
 . . ;
 . . S PXFDA(1,920.61,"?+"_(PXI+1)_",?+1,",.01)=PXSITE
 . ;
 . D UPDATE^DIE("","PXFDA(1)","PXIENS")
 . I $D(^TMP("DIERR",$J)) D
 . . D MES("The following errors occurred:")
 . . D MSG^DIALOG("AE",.PXERRMSG)
 . . D MES^XPDUTL(.PXERRMSG)
 . ;
 . K PXFDA,PXERRMSG,PXIENS
 . D CLEAN^DILF
 ;
 Q
 ;
 ;-------------------------------------------------------------------------
 ;
BMES(STR) ;
 ; Write string
 D BMES^XPDUTL($$TRIM^XLFSTR($$CJ^XLFSTR(STR,$G(IOM,80)),"R"," "))
 Q
MES(STR) ;
 ; Write string
 D MES^XPDUTL($$TRIM^XLFSTR($$CJ^XLFSTR(STR,$G(IOM,80)),"R"," "))
 Q
