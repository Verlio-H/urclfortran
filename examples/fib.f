      PROGRAM FIBB
C PROGRAM WRITTEN MOSTLY BY EMM
      INTEGER CURR, PREV, TMP
      CURR = 0
      PREV = 1
      TMP = 0
      DO I = 0, 10
          TMP = CURR
          CURR = CURR + PREV
          PREV = TMP
          PRINT *, CURR
      END DO
      END