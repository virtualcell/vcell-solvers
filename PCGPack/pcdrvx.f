      SUBROUTINEPCDRVX(N,IJA,A,RHS,X,ISP,RSP,METHOD,PRECON,PCSTOP,
     *PCMV)
      IMPLICITDOUBLEPRECISION(A-H,O-Z)
      INTEGERN,IJA(*),ISP(*)
      DOUBLEPRECISIONA(*),RHS(*),X(*),RSP(*)
      EXTERNALMETHOD,PRECON,PCSTOP,PCMV
      INTEGERIERR,ITMAX,ITEST,KMAX,IFL,IPRE,PBLK,LUDONE,IBLK,
     *MNEED,ITS,RSDONE,NOVERF,RED1,RED2,IERR2,IFRMAT
      COMMON/PCCOM1/IERR,ITMAX,ITEST,KMAX,IFL,IPRE,PBLK,
     *LUDONE,IBLK,MNEED,ITS,RSDONE,NOVERF,RED1,RED2,IERR2,
     *IFRMAT
      SAVE/PCCOM1/
      INTEGERIPRE1,IPRE2,RATIO
      COMMON/PCCOM3/IPRE1,IPRE2,RATIO
      SAVE/PCCOM3/
      INTEGERISRT,IJASGN,ISX0,IFORM
      COMMON/PCVERI/ISRT,IJASGN,ISX0,IFORM
      SAVE/PCVERI/
      DOUBLEPRECISIONDMY
      INTEGERNADR,NFREE,Z0
      INTEGERMFREE,MUNTCH,MPERM,MSCR,PFREE,PUNTCH,PPERM,PSCR,
     *EFREE,EUNTCH,EPERM,ESCR
      NADR=ISP(2)
      NFREE=ISP(1)
      Z0=0
      ISP(4)=NADR
      CALLPCIIPR
      IF(LUDONE.EQ.1)THEN
      PFREE=ISP(5)
      PUNTCH=ISP(7)
      PPERM=NFREE-PFREE
      PSCR=PFREE-PUNTCH
      ISP(2)=ISP(6)
      ISP(1)=PFREE
      IF(IJASGN.EQ.0)THEN
      CALLPCIABS(N,IJA)
      ENDIF
      ELSE
      CALLPRECON(Z0,N,IJA,A,RHS,ISP,RSP)
      PFREE=ISP(1)
      PUNTCH=ISP(3)
      PPERM=NFREE-PFREE
      PSCR=PFREE-PUNTCH
      ISP(5)=PFREE
      ISP(6)=ISP(2)
      ISP(7)=PUNTCH
      ENDIF
      IF(PUNTCH.LT.0)THEN
      MNEED=-PUNTCH
      IERR=3
      RETURN
      ENDIF
      IF(IERR.NE.0)THEN
      RETURN
      ENDIF
      CALLPCSTOP(Z0,N,IJA,A,RHS,X,DMY,ISP,RSP,PRECON)
      EFREE=ISP(1)
      EUNTCH=ISP(3)
      EPERM=PFREE-EFREE
      ESCR=EFREE-EUNTCH
      IF(EUNTCH.LT.0)THEN
      MNEED=-EUNTCH
      IERR=4
      RETURN
      ENDIF
      CALLMETHOD(N,IJA,A,RHS,X,ISP,RSP,PRECON,PCSTOP,PCMV)
      MFREE=ISP(1)
      MUNTCH=ISP(3)
      MPERM=EFREE-MFREE
      MSCR=MFREE-MUNTCH
      MNEED=PPERM+MAX(PSCR,EPERM+MAX(ESCR,MPERM+MSCR))
      IF(MUNTCH.LT.0)THEN
      MNEED=-MUNTCH
      IERR=2
      ENDIF
      RETURN
      END
