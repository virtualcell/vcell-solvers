      SUBROUTINEPCMVC(N,IJA,A,V,W)
      IMPLICITDOUBLEPRECISION(A-H,O-Z)
      INTEGERIJA(*),N
      DOUBLEPRECISIONV(*),W(*),A(*)
      INTEGERI,J
      DO10I=1,N
      W(I)=A(I)*V(I)
 10   CONTINUE
      DO30I=1,N
CDIR$ IVDEP
CVDIR IGNORE RECRDEPS
      DO20J=IJA(I),IJA(I+1)-1
      W(IJA(J))=V(I)*A(J)+W(IJA(J))
 20   CONTINUE
 30   CONTINUE
      RETURN
      END
