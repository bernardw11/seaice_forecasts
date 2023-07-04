C   IMSL ROUTINE NAME   - VXMUL
C
C-----------------------------------------------------------------------
C
C   COMPUTER            - ELXSI/DOUBLE
C
C   LATEST REVISION     - JANUARY 1, 1978
C
C   PURPOSE             - EXTENDED PRECISION MULTIPLY
C
C   USAGE               - CALL VXMUL (A,B,ACC)
C
C   ARGUMENTS    A      - INPUT DOUBLE PRECISION NUMBER
C                B      - INPUT DOUBLE PRECISION NUMBER
C                ACC    - ACCUMULATOR. (INPUT AND OUTPUT)
C                           ACC IS A DOUBLE PRECISION VECTOR OF LENGTH
C                           2.  ON OUTPUT, ACC CONTAINS THE SUM OF
C                           INPUT ACC AND A*B.
C
C   PRECISION/HARDWARE  - DOUBLE/H32
C                       - NOT AVAILABLE/H36,H48,H60
C
C   REQD. IMSL ROUTINES - VXADD
C
C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
C
C   REMARKS      VXMUL ADDS THE PRODUCT A*B TO THE EXTENDED PRECISION
C                ACCUMULATOR, ACC. THE SUBROUTINE ASSUMES THAT AN
C                EXTENDED PRECISION NUMBER IS ALREADY IN THE
C                ACCUMULATOR.  THEREFORE, BEFORE THE FIRST CALL TO
C                VXMUL, ACC(1) AND ACC(2) MUST BE SET TO ZERO.
C
C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C
C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VXMUL (A,B,ACC)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION   A,B,ACC(2)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I,J
      DOUBLE PRECISION   X(2),H(2),T(2),P,Q,R
      DOUBLE PRECISION   S,TOP,BOT
      EQUIVALENCE        (P,X(1)),(Q,X(2)),(R,T(2))
      DATA               S/268435456.D0/,TOP/6.3D29/,BOT/2.12D-22/
C                                  FIRST EXECUTABLE STATEMENT
      J = 0
      X(1) = A
      X(2) = B
      DO 15 I=1,2
         IF (DABS(X(I)).LE.TOP) GO TO 5
C                                  SCALE DOWN BIG X
         J = J+1
         X(I) = X(I)/S**2
    5    IF (DABS(X(I)).GT.BOT) GO TO 10
C                                  SCALE UP SMALL X
         J = J-1
         X(I) = X(I)*S**2
C                                  ROUND OFF HALF OF BITS IN X TO GET H
   10    R = S*X(I)+X(I)
         H(I) = (X(I)-R)+R
         T(I) = X(I)-H(I)
   15 CONTINUE
C                                  PREPARE X(1)*X(2)=P+Q EXACTLY
      P = X(1)*X(2)
      Q = H(1)*H(2)
      IF (Q.EQ.0.D0) GO TO 20
C                                  IN CASE Q UNDERFLOWS TO ZERO
      Q = ((H(1)*T(2)+H(2)*T(1))+(Q-P))+T(1)*T(2)
C                                  AVOID UNNECESSARY RESCALING
   20 IF(J.EQ.0 .OR. (P.EQ.0.D0 .AND. Q.EQ.0.D0)) GO TO 25
C                                  ELSE UNDO SCALING, PERHAPS
C                                  OVER OR UNDERFLOWING
      R = S**(J+J)
      P = P*R
      Q = Q*R
   25 CALL VXADD(P,ACC)
      CALL VXADD(Q,ACC)
      RETURN
      END
