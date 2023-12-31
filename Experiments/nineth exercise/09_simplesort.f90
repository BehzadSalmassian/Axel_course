
! pick two randomly chosen elements in array 'dat'
! and swap them. do this 'count' times.



PROGRAM real_sort
  USE sorting
  USE list_tools
  IMPLICIT NONE

  INTEGER :: num, i, n, length
  REAL,ALLOCATABLE,DIMENSION(:) :: dat
  REAL :: time1, time2, rv
  logical :: Ascendsort
  INTEGER,PARAMETER,DIMENSION(7) :: sizes = (/ &
      500,1000,2000,5000,10000,20000,50000 /)


  ! initialize pseudo random number generator
  CALL RANDOM_SEED()

  ! loop over various pre-defined array dimensions for sorting benchmarks.
  DO n=1,SIZE(sizes,1)
      num = sizes(n)
      ALLOCATE(dat(num))

      ! fill array with uniform distributed random numbers
      DO i=1,num
          CALL RANDOM_NUMBER(rv)
          dat(i) = (ISHFT(HUGE(i),-7)*0.000001*rv)
      END DO

      ! call sort algorithm and measure the time spent on it.
      CALL CPU_TIME(time1)
      CALL simplesort(dat)
      Ascendsort= is_sorted (dat,sizes(n))
      if (Ascendsort.eqv..false.) then
            print *, Ascendsort
            exit
      end if

      CALL CPU_TIME(time2)
      WRITE(*,FMT=666) num, 'unsorted random', time2-time1

      ! call sort again on the already sorted data
      CALL CPU_TIME(time1)
      CALL simplesort(dat)
      Ascendsort= is_sorted (dat,sizes(n))
      if (Ascendsort.eqv..false.) then
             print *, Ascendsort
             exit
        end if

      CALL CPU_TIME(time2)
      WRITE(*,FMT=666) num, 'already sorted', time2-time1

      ! swap a few elements of the sorted array and sort one more time
      CALL swap(dat,INT(LOG(REAL(num))))
      CALL CPU_TIME(time1)
      CALL simplesort(dat)
      Ascendsort= is_sorted (dat,sizes(n))
      if (Ascendsort.eqv..false.) then
              print *, Ascendsort
              exit
          end if
      CALL CPU_TIME(time2)
      WRITE(*,FMT=666) num, 'mostly sorted', time2-time1

      ! release storage
      DEALLOCATE(dat)
  END DO

666 FORMAT (' Sorting',I8,1X,A15,1X,'elements took:',F12.6,' seconds')
END PROGRAM real_sort
