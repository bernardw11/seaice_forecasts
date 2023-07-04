C   IMSL ROUTINE NAME   - VXSTO
C
C-----------------------------------------------------------------------
C
C   COMPUTER            - ELXSI/DOUBLE
C
C   LATEST REVISION     - JANUARY 1, 1978
C
C   PURPOSE             - DOUBLE PRECISION STORE.
C
C   USAGE               - CALL VXSTO(ACC,D)
C
C   ARGUMENTS    ACC    - ACCUMULATOR. (INPUT)
C                           ACC IS A DOUBLE PRECISION VECTOR OF LENGTH
C                           2. ACC IS ASSUMED TO BE THE RESULT OF
C                           CALLING VXADD OR VXMUL TO PERFORM EXTENDED
C                           PRECISION OPERATIONS.
C                D      - DOUBLE PRECISION SCALAR. (OUTPUT)
C                           ON OUTPUT, D CONTAINS A DOUBLE PRECISION
C                           APPROXIMATION TO THE VALUE OF THE EXTENDED
C                           PRECISION ACCUMULATOR.
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
C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C
C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VXSTO (ACC,D)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION   ACC(2),D
C                                  FIRST EXECUTABLE STATEMENT
      D = ACC(1)+ACC(2)
      RETURN
      END
