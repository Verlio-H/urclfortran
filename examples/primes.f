      PROGRAM PRIME
C THIS PROGRAM WRITTEN BY CHATGPT
      INTEGER I, J, K
      DO 10 I = 2, 100
         K = 0
         DO 20 J = 2, I/2
            IF (I / J * J .EQ. I) THEN
               K = 1
               GO TO 30
            END IF
 20      CONTINUE
         IF (K .EQ. 0) THEN
            PRINT *, I
         END IF
 30   CONTINUE
 10   CONTINUE
      END