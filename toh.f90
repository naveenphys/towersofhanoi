!     Towers of Hanoi: you have to move a stack of disks from one position to
!     another, using an auxiliary position. All disks have different sizes, and
!     you can never stack a larger disk on a smaller one. You can only move a
!     single disk at the time, and that should be at the top of a stack, and
!     placed on top of another stack. Consider a recursive approach.
!     Compile the program using:
!     gfortran -cpp -DNMAX=xx question_04.f90

#ifndef NMAX
#define NMAX 10
#endif /*NMAX*/

#define MOVE_SUCCESS 0
#define MOVE_INVALID_P_EQ_Q 1
#define MOVE_INVALID_A_IS_EMPTY 2
#define MOVE_INVALID_B_IS_FULL 3
#define MOVE_INVALID_A_GT_B 4


      PROGRAM question_04
      USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: I2 => INT16, I8 => INT64
      IMPLICIT NONE
      TYPE :: tower_t(n)
        INTEGER, LEN          :: n
        INTEGER, DIMENSION(n) :: disks ! Disks as an array of integers.
        INTEGER               :: n_now ! Number of disks now.
        INTEGER               :: n_top ! Disk on the top.
      END TYPE tower_t
      TYPE :: towers_t(n, m)
        INTEGER, LEN                        :: n
        INTEGER, LEN                        :: m
        TYPE(tower_t(n=NMAX)), DIMENSION(m) :: t ! m towers
        INTEGER                             :: n_accept
        INTEGER                             :: n_reject
      END TYPE towers_t
      INTEGER, PARAMETER                  :: L = 1
      INTEGER, PARAMETER                  :: C = 2
      INTEGER, PARAMETER                  :: R = 3
      INTEGER, PARAMETER                  :: L2C = 1
      INTEGER, PARAMETER                  :: C2L = -1
      INTEGER, PARAMETER                  :: L2R = 2
      INTEGER, PARAMETER                  :: R2L = -2
      INTEGER, PARAMETER                  :: C2R = 3
      INTEGER, PARAMETER                  :: R2C = -3
      TYPE(towers_t(n=NMAX,m=3))          :: towers
      INTEGER                             :: ierr
      INTEGER                             :: unit_no
      INTEGER                             :: istat
      CHARACTER(LEN=128)                  :: errmsg


      WRITE(UNIT=*,FMT='(A,I3)') &
      'Initial number of disks: ', SIZE(towers%t(L)%disks)

      CALL init(towers)
      CALL show(towers)

      OPEN(NEWUNIT=unit_no, FILE='moves.dat', STATUS='REPLACE', &
      ACTION='WRITE', ACCESS = 'SEQUENTIAL', IOSTAT=istat, IOMSG=errmsg)
      IF (istat .NE. 0) STOP 'Runtime error in OPEN statement'

      ASSOCIATE(th => towers)
      DO
        ! Move L<->C
100     CALL move(L, C, th, ierr)
        IF (ierr .NE. 0) THEN
          CALL move(C, L, th, ierr)
          IF (ierr .EQ. 0) THEN
            WRITE(UNIT=unit_no, FMT='(I2)') C2L
            IF (th%t(C)%n_now .NE. NMAX .AND. th%t(R)%n_now .NE. NMAX) GOTO 200
            EXIT
          END IF
        ELSE
          WRITE(UNIT=unit_no, FMT='(I2)') L2C
          IF (th%t(C)%n_now .NE. NMAX .AND. th%t(R)%n_now .NE. NMAX) GOTO 200
          EXIT
        END IF
        ! Move L<->R
200    CALL move(L, R, th, ierr)
        IF (ierr .NE. 0) THEN
          CALL move(R, L, th, ierr)
          IF (ierr .EQ. 0) THEN
            WRITE(UNIT=unit_no, FMT='(I2)') R2L
            IF (th%t(C)%n_now .NE. NMAX .AND. th%t(R)%n_now .NE. NMAX) GOTO 300
            EXIT
          END IF
        ELSE
          WRITE(UNIT=unit_no, FMT='(I2)') L2R
          IF (th%t(C)%n_now .NE. NMAX .AND. th%t(R)%n_now .NE. NMAX) GOTO 300
          EXIT
        END IF
        ! Move C<->R
300     CALL move(C, R, th, ierr)
        IF (ierr .NE. 0) THEN
          CALL move(R, C, th, ierr)
          IF (ierr .EQ. 0) THEN
            WRITE(UNIT=unit_no, FMT='(I2)') R2C
            IF (th%t(C)%n_now .NE. NMAX .AND. th%t(R)%n_now .NE. NMAX) GOTO 100
            EXIT
          END IF
        ELSE
          WRITE(UNIT=unit_no, FMT='(I2)') C2R
          IF (th%t(C)%n_now .NE. NMAX .AND. th%t(R)%n_now .NE. NMAX) GOTO 100
          EXIT
        END IF
      END DO
      END ASSOCIATE

      CLOSE(UNIT=unit_no, IOSTAT=istat, IOMSG=errmsg)
      IF (istat .NE. 0) STOP 'Runtime error in CLOSE statement'
      WRITE(*,FMT='(A,I6)') 'n_accept:', towers%n_accept
      WRITE(*,FMT='(A,I6)') 'n_reject:', towers%n_reject


      CONTAINS

      SUBROUTINE init(towers)
      IMPLICIT NONE
      INTEGER, PARAMETER         :: L = 1
      INTEGER, PARAMETER         :: C = 2
      INTEGER, PARAMETER         :: R = 3
      TYPE(towers_t(n=NMAX,m=3)) :: towers
      INTEGER                    :: i

      ASSOCIATE(l => towers%t(L), c => towers%t(C), r => towers%t(R) )
      l%n_now = NMAX
      l%disks = [(NMAX-(i-1),i=1,NMAX)]
      PRINT*, l%disks
      l%n_top = l%disks(NMAX)
      c%n_now = 0
      c%disks = [(0,i=1,NMAX)]
      c%n_top = 0
      r%n_now = 0
      c%disks = [(0,i=1,NMAX)]
      r%n_top = 0
      END ASSOCIATE

      END SUBROUTINE

      SUBROUTINE move(P, Q, towers, ierr)
      IMPLICIT NONE
      INTEGER, PARAMETER                        :: L = 1
      INTEGER, PARAMETER                        :: C = 2
      INTEGER, PARAMETER                        :: R = 3
      INTEGER,                    INTENT(IN)    :: P
      INTEGER,                    INTENT(IN)    :: Q
      TYPE(towers_t(n=NMAX,m=3)), INTENT(INOUT) :: towers
      INTEGER,                    INTENT(OUT)   :: ierr

      ierr = MOVE_SUCCESS

      ! Moving a disk on top of itself is invalid.
      IF (P .EQ. Q) THEN
        ierr = MOVE_INVALID_P_EQ_Q
        towers%n_reject = towers%n_reject + 1
        RETURN
      END IF

      ASSOCIATE(A => towers%t(P), B => towers%t(Q))

        ! Moving from an empty tower is invalid.
        IF (A%n_now .EQ. 0) THEN
          ierr = MOVE_INVALID_A_IS_EMPTY
          towers%n_reject = towers%n_reject + 1
          RETURN
        END IF

        ! Moving on top of a complete tower is invalid.
        IF (B%n_now .EQ. NMAX) THEN
          ierr = MOVE_INVALID_B_IS_FULL
          towers%n_reject = towers%n_reject + 1
          RETURN
        END IF

        ! Moving a large value on top of a small value is invalid.
        IF (A%n_top .GT. B%n_top .AND. B%n_top .NE. 0) THEN
          ierr = MOVE_INVALID_A_GT_B
          towers%n_reject = towers%n_reject + 1
          RETURN
        END IF

        B%n_now = B%n_now + 1
        B%disks(B%n_now) = A%disks(A%n_now)
        A%n_now = A%n_now - 1
        A%n_top = A%disks(A%n_now)
        B%n_top = B%disks(B%n_now)
        A%disks(A%n_now+1:NMAX) = 0
        B%disks(B%n_now+1:NMAX) = 0
        towers%n_accept = towers%n_accept + 1
      END ASSOCIATE
      CALL show(towers)

      END SUBROUTINE move

      SUBROUTINE show(towers)
      IMPLICIT NONE
      TYPE(towers_t(n=NMAX,m=3)), INTENT(IN) :: towers
      INTEGER                                :: i
      INTEGER                                :: j
      INTEGER                                :: k

      DO i = NMAX, 1, -1
        DO j = 1, 3
          IF (towers%t(j)%n_now .NE. 0) THEN
            k = towers%t(j)%disks(i)
            WRITE(*,FMT='(A)',ADVANCE='NO') &
            REPEAT(' ',4)//REPEAT('*',k)//REPEAT(' ',NMAX-k)
          ELSE
            WRITE(*,FMT='(A)',ADVANCE='NO') REPEAT(' ',4)//REPEAT(' ',NMAX)
          END IF
        END DO
        WRITE(*,FMT='(A)')NEW_LINE('A')
      END DO

      WRITE(*,FMT='(A)') &
      '....'//REPEAT("=",NMAX)// &
      '....'//REPEAT("=",NMAX)// &
      '....'//REPEAT("=",NMAX)//'....'

      END SUBROUTINE show

      END PROGRAM question_04
