      SUBROUTINEPCSC2B(IBLKN,IBTRM,IRTBM,IBTRMC,NBLCKB,C,IC,IJA
     *,A,V1,V2)
      IMPLICITDOUBLEPRECISION(A-H,O-Z)
      INTEGERIBLKN,IBTRM(*),IRTBM(*),IBTRMC(*),NBLCKB,C(*),
     *IC(*),IJA(*)
      DOUBLEPRECISIONA(*),V1(*),V2(*)
      LOGICALLOWER,UPPER,FULL,HALF,SYMMAT
      COMMON/PCVERL/LOWER,UPPER,FULL,HALF,SYMMAT
      SAVE/PCVERL/
      INTEGERI,II,J,K,NBLACK,JJ,MI,MJ,KK,MM,ILOW
      NBLACK=IBTRMC(NBLCKB+1)-1
      IF(HALF)THEN
      ILOW=1
      ELSE
      ILOW=NBLCKB+1
      ENDIF
      DO1I=ILOW,IBLKN
      II=C(I)
      KK=IBTRMC(I)-IBTRM(II)
      DO3K=IBTRM(II),IBTRM(II+1)-1
      MI=KK+K
CDIR$ IVDEP
CVDIR IGNORE RECRDEPS
      DO5J=IJA(K),IJA(K+1)-1
      MM=IRTBM(IJA(J))
      JJ=IC(MM)
      MJ=IBTRMC(JJ)+IJA(J)-IBTRM(MM)
      IF(MI.GT.NBLACK.AND.MJ.LE.NBLACK)THEN
      V2(MI-NBLACK)=V2(MI-NBLACK)-A(J)*V1(MJ)
      ELSE
      IF(MI.LE.NBLACK.AND.MJ.GT.NBLACK)THEN
      V2(MJ-NBLACK)=V2(MJ-NBLACK)-A(J)*V1(MI)
      ENDIF
      ENDIF
5     CONTINUE
3     CONTINUE
1     CONTINUE
      RETURN
      END