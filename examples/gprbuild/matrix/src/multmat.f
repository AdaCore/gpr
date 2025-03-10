
C     Simple matrix multiplication in Fortran 77
      LOGICAL FUNCTION MULTMAT (A, m, n, B, o, p, C)
      INTEGER m, n, o, p, i, j, k
      REAL A(m,n), B(o,p), C(m,p)
      REAL t
      IF (n .NE. o) THEN
         MULTMAT = .FALSE.
         RETURN
      ENDIF
      DO 320 i=1, m
         DO 310 j=1, p
            t = 0
            DO 300 k=1, n
               t = t + A(i, k) * B(k, j)
 300        CONTINUE
            C(i, j) = t
 310     CONTINUE
 320  CONTINUE
      MULTMAT = .TRUE.
      RETURN
      END

