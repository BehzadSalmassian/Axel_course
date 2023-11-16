! ... (your existing code)

INTEGER, PARAMETER, DIMENSION(4) :: sizes = (/ 500, 1000, 2000, 5000 /)

INTERFACE
  ! Modify the interface to accept arrays directly, not a derived type
  FUNCTION is_sorted(arr, size, dirorder) RESULT(AscendSort)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(in) :: arr
    INTEGER, INTENT(in) :: size
    CHARACTER(*), INTENT(in), OPTIONAL :: dirorder
    LOGICAL :: AscendSort
  END FUNCTION is_sorted
END INTERFACE

! ... (your existing code)

DO n = 1, SIZE(sizes, 1)
  num = sizes(n)
  ALLOCATE(dat(num))

  ! ... (your existing code)

  ! call sort algorithm and measure the time spent on it.
  CALL CPU_TIME(time1)
  CALL simplesort(dat)
  AscendSort = is_sorted(dat, num)  ! Use the correct arguments
  CALL CPU_TIME(time2)
  WRITE(*, FMT=666) num, 'unsorted random', time2 - time1

  ! call sort again on the already sorted data
  CALL CPU_TIME(time1)
  CALL simplesort(dat)
  AscendSort = is_sorted(dat, num)  ! Use the correct arguments
  CALL CPU_TIME(time2)
  WRITE(*, FMT=666) num, 'already sorted', time2 - time1

  ! swap a few elements of the sorted array and sort one more time
  CALL swap(dat, INT(LOG(REAL(num))))
  CALL CPU_TIME(time1)
  CALL simplesort(dat)
  AscendSort = is_sorted(dat, num)  ! Use the correct arguments
  CALL CPU_TIME(time2)
  WRITE(*, FMT=666) num, 'mostly sorted', time2 - time1

  ! release storage
  DEALLOCATE(dat)
END DO

666 FORMAT (' Sorting', I8, 1X, A15, 1X, 'elements took:', F12.6, ' seconds')
END PROGRAM real_sort
