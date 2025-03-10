C--------------------------------------------
      SUBROUTINE  PRINT_MATR (A,N,M)
C--------------------------------------------------------------------
      INTEGER N, M
      INTEGER A (N,M)
       
C     BEGIN 
         DO 10 I = 1,N
            PRINT *, (A (I, J), J=1,M)
C            DO 11 J = 1,M
C               PRINT *, A (I, J) 
C  11        CONTINUE 
  10     CONTINUE
      END 
C--------------------------------------------
      SUBROUTINE  INIT (A,N, M, VAL)
C--------------------------------------------------------------------
      INTEGER N, M, VAL
      INTEGER A (N,M)
       
C     BEGIN 
         DO 20 I = 1,N
            DO 21 J = 1,M
               A (I, J) = VAL
  21        CONTINUE
  20     CONTINUE
      END 
C--------------------------------------------
      SUBROUTINE  ADD (A,B,C,N,M, LIG)
C--------------------------------------------------------------------
      INTEGER N, M, LIG
      INTEGER A (N,M), B(N,M), C (N,M)
       
C     BEGIN 
         DO 30 J = 1,M
            C (LIG, J) = A (LIG, J) + B (LIG, J)
  30     CONTINUE
      END 

C--------------------------------------------
      PROGRAM  ESS
C--------------------------------------------------------------------
C     BEGIN
       EXTERNAL ADD
       INTEGER N,M, A(400, 10000), B(400, 10000), C (400, 10000)
       N = 400
       M = 10000
C
C  first interface
C
       CALL ADAINIT
       CALL INIT (A, N, M, 2)
       CALL INIT (B, N, M, 8)
       CALL PARLOOP6 (1, N, I, ADD, A, B, C, N, M, I)
C       CALL PRINT_MATR (C, N, M)
C
C  seconde interface
C
       CALL INIT (A, N, M, 20)
       CALL INIT (B, N, M, 18)
       call INITSYNC (ISYNCH)
       DO 40 I= 1,N
         CALL POST6 (ISYNCH, I, ADD, A, B, C, N, M, I)
 40    CONTINUE
       CALL WAITSYNC (ISYNCH)
C       CALL PRINT_MATR (C, N, M)
       CALL STOP
       CALL ADAFINAL
      END 


