      SUBROUTINEPCRDPT(N,IBLKN,IBLKS,IBTRM,IRTBM,IBTRMC,NBLCKB,
     *NBLACK,C,IC,IPVT,IJA,A,RHS,D2I,Y,X)
      IMPLICITDOUBLEPRECISION(A-H,O-Z)
      INTEGERN,IBLKN,IBLKS(*),IBTRM(*),IRTBM(*),IBTRMC(*),
     *NBLCKB,NBLACK,C(*),IC(*),IPVT(*),IJA(*)
      DOUBLEPRECISIONA(*),RHS(*),D2I(*),Y(*),X(*)
      INTEGERONE
      ONE=1
      CALLDCOPY(N,RHS,ONE,X,ONE)
      CALLPCIPRM(NBLCKB,IBTRM,C,Y,X)
      CALLPCRC2B(IBLKN,IBTRM,IRTBM,IBTRMC,NBLCKB,C,IC,IJA,A,
     *X)
      CALLPCD2I(IBLKN,IBLKS,IBTRM,IBTRMC,NBLCKB,C,IPVT,D2I,X)
      RETURN
      END
