GMRCAAC ;SLC/DLT - Administrative Complete action consult logic ;7/16/98  01:47
 ;;3.0;CONSULT/REQUEST TRACKING;**4,12,53,46**;DEC 27, 1997;Build 23
COMP(GMRCO) ;Clerk action to Complete an order
 ;GMRCO is the selected consult
 K GMRCQUT,GMRCQIT
 I '+$G(GMRCO) D SELECT^GMRCA2(.GMRCO) I $D(GMRCQUT) D END Q
 I '+$G(GMRCO) D END S GMRCQUT=1 Q
 ;
 N GMRC,GMRCSTS,GMRCQUT
 S GMRC(0)=$G(^GMR(123,+GMRCO,0)) Q:GMRC(0)=""
 ;
 ;Completion action restricted if status is 1,2,or 13
 S GMRCSTS=$P(GMRC(0),"^",12)
 I $S(GMRCSTS<3:1,GMRCSTS=13:1,1:0) D  Q
 . N GMRCMSG
 . S GMRCMSG="This order has already been "_$S(GMRCSTS=1:"discontinued",GMRCSTS=2:"completed",1:"cancelled")_"!"
 . D EXAC^GMRCADC(GMRCMSG)
 . S GMRCQUT=1
 . D END
 ;
 ;Get the provider, activity date, and significant findings
 N DFN,ORIFN,ORGY,GMRCSF,GMRCSTS,GMRCA,GMRCDR,GMRCORNP,GMRCAD,GMRCADUZ
 S ORGY="",(GMRCIFN,ORIFN)=$P(GMRC(0),"^",3),GMRCORVP=$P(GMRC(0),"^",2)_";DPT("
 S DFN=+GMRCORVP
 N GETPROV D GETPROV^GMRCAU I $G(GMRCQIT)="Q" D END Q
 S GMRCAD=$$GETDT^GMRCUTL1 I GMRCAD="^" S GMRCQIT="Q" D END Q
 S GMRCSFO=$P(GMRC(0),"^",19)
 S GMRCSF=$$GETSIGF^GMRCASF(GMRCSFO) I GMRCSF=0 D END Q
 ;
 ;Update the Activity Log for an audit trail
 S GMRCSTS=2,GMRCA=10
 N GMRCQUIT S GMRCOM=1 D AUDIT^GMRCP I +$G(GMRCQUIT)=1 Q
 I $G(GMRCERR)=1 S GMRCMSG=GMRCERMS D EXAC^GMRCADC(GMRCMSG) Q
 ;Update status, last action and significant findings
 S GMRCDR="8////^S X=GMRCSTS;9////^S X=GMRCA;15////^S X=GMRCSF"
 D STATUS^GMRCP
 I $G(GMRCERR)=1 S GMRCMSG=GMRCERMS D  Q
 . N DA,DIK
 . D EXAC^GMRCADC(GMRCMSG)
 . S DA=$O(^GMR(123,+GMRCO,40,"A"),-1),DA(1)=+GMRCO,DIK="^GMR(123,"_DA(1)_",40," D ^DIK K DIK
 ;
 D EN^GMRCHL7(DFN,GMRCO,$G(GMRCTYPE),$G(GMRCRB),"RE",GMRCORNP,$G(GMRCVSIT),.GMRCOM,,$G(GMRCAD))
 S GMRCADUZ=""
 I $P(^GMR(123,GMRCO,0),"^",14),$P(^GMR(123,GMRCO,0),"^",14)'=DUZ S GMRCADUZ($P(^(0),"^",14))=""
 S GMRCORTX="Completed Consult "_$$ORTX^GMRCAU(GMRCO)_$S(GMRCSF="Y":" with Sig Findings",GMRCSF="N":" with no Sig Findings",1:"")
 D MSG^GMRCP($P(^GMR(123,GMRCO,0),"^",2),GMRCORTX,+GMRCO,23,.GMRCADUZ,0)
 Q
 ;
NOUPD ;Exit without making an update
 S GMRCMSG="Completion activity ignored."
 D EXAC^GMRCADC(GMRCMSG)
 Q
END K DUOUT,X,Y,GMRCPL,GMRCPLI,GMRCURG,GMRCURGI,GMRCPRI,XQORM
 I '$D(GMRCNM) K GMRCVP
 K GMRCDT,GMRCAD,GMRCL,GMRCTYPE
 I '$D(GMRCO) S (GMRCO,ORIFN)=""
 K GMRCIFN,GMRCMSG,GMRCORTX,GMRCSA,GMRCSTS,GMRCADUZ,STS
 Q
