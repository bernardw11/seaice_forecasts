C   IMSL ROUTINE NAME   - VXADD
C
C-----------------------------------------------------------------------
C
C   COMPUTER            - ELXSI/DOUBLE
C
C   LATEST REVISION     - JANUARY 1, 1978
C
C   PURPOSE             - EXTENDED PRECISION ADD
C
C   USAGE               - CALL VXADD (A,ACC)
C
C   ARGUMENTS    A      - DOUBLE PRECISION NUMBER TO BE ADDED TO THE
C                           ACCUMULATOR. (INPUT)
C                ACC    - ACCUMULATOR. (INPUT AND OUTPUT)
C                           ACC IS A DOUBLE PRECISION VECTOR OF LENGTH
C                           2. ON OUTPUT, ACC CONTAINS THE SUM OF
C                           INPUT ACC AND A.
C
C   PRECISION/HARDWARE  - DOUBLE/H32
C                       - NOT AVAILABLE/H36,H48,H60
C
C   REQD. IMSL ROUTINES - NONE REQUIRED
C
C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
C
C   REMARKS      VXADD ADDS THE DOUBLE PRECISION NUMBER A TO THE
C                EXTENDED PRECISION ACCUMULATOR, ACC. THE SUBROUTINE
C                ASSUMES THAT AN EXTENDED PRECISION NUMBER IS ALREADY IN
C                THE ACCUMULATOR. THEREFORE, BEFORE THE FIRST CALL TO
C                VXADD, ACC(1) AND ACC(2) MUST BE SET TO ZERO.
C
C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C
C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VXADD(A,ACC)
C
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION   A,ACC(2)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION   X,Y,Z,ZZ
C                                  FIRST EXECUTABLE STATEMENT
      X = ACC(1)
      Y = A
      IF (DABS(ACC(1)).GE.DABS(A)) GO TO 1
      X = A
      Y = ACC(1)
    1 Z = (ACC(2)+Y)+X
      ZZ = ((X-Z)+Y)+ACC(2)
      ACC(1) = Z+ZZ
      ACC(2) = (Z-ACC(1))+ZZ
      RETURN
      END
