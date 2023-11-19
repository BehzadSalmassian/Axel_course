MODULE sorting
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: simplesort, BubbleSort, InsertionSort, QuickSort, MergeSort, HybridSort
CONTAINS

  ! pathetically bad sorting algorithm:
  ! loop over all unique pairs and swap the values
  ! if the left element is larger than the right one.
  SUBROUTINE simplesort(dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, i, j
    REAL :: tmp

    num = SIZE(dat,1)
    IF (num < 2) RETURN
    DO i=1,num-1
        DO j=i+1,num
            IF (dat(i) > dat(j)) THEN
                tmp = dat(i)
                dat(i) = dat(j)
                dat(j) = tmp
            END IF
        END DO
    END DO
  END SUBROUTINE simplesort


SUBROUTINE MergeSort(dat)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(inout) :: dat
    INTEGER :: num, i, j, m, d, dd
    REAL :: tmp
    num = SIZE(dat, 1)

    d = num
    dd = 0

    !-------first step-------------
    DO i=0, num/2
    IF (dat((2*i)+1)>dat((2*i)+2) .AND. (2*i)+2 <= num) THEN
        tmp=dat((2*i)+2)
        dat((2*i)+2)=dat((2*i)+1)
        dat((2*i)+1)=tmp
        END IF
    END Do
  !  print*, "Initial Matrix is"
 !   print*, dat

    !----------------
    DO WHILE (d /= 0)
        d = d / 2
        dd = dd + 1
    END DO
    !----------------

    m = 1

    !-----------------------
    DO i = 1, dd
        m = m * 2
        DO j = 1, num, 2 * m
                CALL merge(dat, j, MIN(j + m - 1, num), MIN(j + 2 * m - 1, num))
        END DO
    END DO
    !-----------------------
 !   print*, "Sorted Matrix is"
  !  print*, dat

CONTAINS

    ! Helper subroutine to merge two sorted subarrays
    SUBROUTINE merge(dat, left, mid, right)
        REAL, DIMENSION(:), INTENT(inout) :: dat
        INTEGER, INTENT(in) :: left, mid, right
        INTEGER :: i, j, k
        REAL, DIMENSION(mid - left + 1) :: leftArray
        REAL, DIMENSION(right - mid) :: rightArray

        ! Copy data to temporary arrays
        DO i = 1, mid - left + 1
            leftArray(i) = dat(left + i - 1)


        END DO

        DO i = 1, right - mid
            rightArray(i) = dat(mid + i)


        END DO

        i = 1
        j = 1
        k = left

        DO WHILE (i <= mid - left + 1 .AND. j <= right - mid)
            IF (leftArray(i) <= rightArray(j)) THEN
                dat(k) = leftArray(i)
                i = i + 1
            ELSE
                dat(k) = rightArray(j)
                j = j + 1
            END IF
            k = k + 1
        END DO

        ! Copy any remaining elements from the left subarray
        DO WHILE (i <= mid - left + 1)
            dat(k) = leftArray(i)
            k = k + 1
            i = i + 1
        END DO
    END SUBROUTINE merge

END SUBROUTINE MergeSort











!-------------------------Hybrid

SUBROUTINE HybridSort(dat)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(inout) :: dat
    INTEGER :: num, i, j, m, d, dd, k,f,kk
    REAL :: tmp, key
    num = SIZE(dat, 1)

    d = num
    dd = 0
    f=num/32
    !-------first step-------------
 !   print*, dat

Do k=0,f
    kk=k*32
  !  print*,"k",k, "kk", kk, "i",1+kk
        DO j=1, 32
            DO i=1+kk, 32+kk-j

                IF ((dat(i))>(dat(i+1)) .and. i<=num) THEN
                    tmp=0
                    tmp=dat(i)

                    dat(i)=dat(i+1)
                    dat(i+1)=tmp

                END IF
            END DO
        END Do
  !      print*, dat(1+kk:32+kk)
END DO


! print*, dat
  !  print*, "Initial Matrix is"

!
    !----------------
    DO WHILE (d /= 0)
        d = d / 2
        dd = dd + 1
    END DO
    !----------------
!----starting from 16*2
    m = 16

    !-----------------------
    DO i = 6, dd
        m = m * 2
!        print*, m
        DO j = 1, num, 2 * m
                CALL merge(dat, j, MIN(j + m - 1, num), MIN(j + 2 * m - 1, num))
          !      print*, dat
        END DO
    END DO
    !-----------------------
   ! print*, "Sorted Matrix is"
   ! print*, dat

CONTAINS

    ! Helper subroutine to merge two sorted subarrays
    SUBROUTINE merge(dat, left, mid, right)
        REAL, DIMENSION(:), INTENT(inout) :: dat
        INTEGER, INTENT(in) :: left, mid, right
        INTEGER :: i, j, k
        REAL, DIMENSION(mid - left + 1) :: leftArray
        REAL, DIMENSION(right - mid) :: rightArray

        ! Copy data to temporary arrays
        DO i = 1, mid - left + 1
            leftArray(i) = dat(left + i - 1)


        END DO

        DO i = 1, right - mid
            rightArray(i) = dat(mid + i)


        END DO

        i = 1
        j = 1
        k = left

        DO WHILE (i <= mid - left + 1 .AND. j <= right - mid)
            IF (leftArray(i) <= rightArray(j)) THEN
                dat(k) = leftArray(i)
                i = i + 1
            ELSE
                dat(k) = rightArray(j)
                j = j + 1
            END IF
            k = k + 1
        END DO

        ! Copy any remaining elements from the left subarray
        DO WHILE (i <= mid - left + 1)
            dat(k) = leftArray(i)
            k = k + 1
            i = i + 1
        END DO
    END SUBROUTINE merge

END SUBROUTINE HybridSort



















SUBROUTINE BubbleSort (dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, i, j
    REAL :: tmp

    num = SIZE(dat,1)
    IF (num < 2) RETURN
    DO i=num+1, 1, -1

   ! sorted flag = false
        DO j=1,i
            IF (dat(j) > dat(j+1)) THEN
                tmp = dat(j)
                dat(j) = dat(j+1)
                dat(j+1) = tmp

    !            sorted flag =true
            END IF
        END DO
     !   if flag false break

    END DO
END SUBROUTINE BubbleSort




SUBROUTINE InsertionSort (dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, i, j, key
    REAL :: tmp

    num = SIZE(dat,1)
    IF (num < 2) RETURN
    DO i=2, num
      key=dat(i)
      j=i-1
      DO WHILE (dat(j)>key .and. j>0)
                dat(j+1) = dat(j)
                j=j-1
                dat(j+1) = key
      END DO
    END DO
END SUBROUTINE InsertionSort


!SUBROUTINE QuickSort (dat)
!    IMPLICIT NONE
!    REAL,DIMENSION(:),INTENT(inout) :: dat
!    INTEGER :: num, i, j, pivot, change,changecheker
!    REAL :: tmp,rdn
!    change=0
!    changecheker=0
!    num = SIZE(dat,1)
!    print*, dat
!    IF (num < 2) RETURN
!
!    call random_number(rdn)
!    pivot = rdn*num
!
!    print*, dat(pivot)
!  DO WHILE (changecheker-change<1000)
!      DO i=1,num
!        DO j=1,num
!            IF (dat(pivot)<dat(i) .AND. i<pivot ) THEN
!                tmp=dat(i)
!                dat(i)=dat(pivot)
!                dat(pivot)=tmp
!                change=changecheker
!                print*, "i"
!                print*, pivot
!                print*, pivot
!
!            END IF
!            IF (dat(pivot)>dat(j) .AND. j>pivot) THEN
!                tmp=dat(j)
!                dat(j)=dat(pivot)
!                dat(pivot)=tmp
!                change=changecheker
!                print*, "j"
!                print*, pivot
!                print*, pivot
!
!            END IF
!      END DO
!    END DO
!            changecheker=changecheker+1
!    call random_number(rdn)
!    pivot = rdn*num
!    END DO
!  print*, dat
!
! END SUBROUTINE QuickSort



  ! quicksort implementation via recursion
  ! top-level takes whole array, recursions work on subsets.
  ! pick pivot element and recursively sort the two sublists.
  SUBROUTINE quicksort(dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, p

    num = SIZE(dat,1)
    IF (num < 2) RETURN

    p = select_pivot(dat,1,num)
    CALL quicksortt_recurse(dat,1,p-1)
    CALL quicksortt_recurse(dat,p+1,num)
  END SUBROUTINE quicksort


  RECURSIVE SUBROUTINE quicksort_recurse(dat,left,right)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER,INTENT(in) :: left, right
    INTEGER :: p

    IF (left < right) THEN
        p = select_pivot(dat,left,right)
        CALL quicksort_recurse(dat,left,p-1)
        CALL quicksort_recurse(dat,p+1,right)
    END IF
  END SUBROUTINE quicksort_recurse


  RECURSIVE SUBROUTINE quicksortt_recurse(dat,left,right)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER,INTENT(in) :: left, right
    INTEGER :: p

    IF (left < right) THEN
        p = select_pivot(dat,left,right)
        CALL quicksortt_recurse(dat,left,p-1)
        CALL quicksortt_recurse(dat,p+1,right)
    END IF
  END SUBROUTINE quicksortt_recurse

  ! core step in quicksort. pick pivot value. then swap
  ! array elements so that smaller values are to the left of
  ! it and all larger values to the right. store pivot in
  ! the remaining spot. this element is now in its final location.
  ! return the index of the pivot element.
  ! The choice of the pivot is arbitrary, but crucial for getting
  ! good performance with presorted data.
  RECURSIVE FUNCTION select_pivot(dat,left,right) RESULT(i)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: i, j, right, left
    REAL :: tmp, pivot

    ! this is the classic choice of pivot element, assuming random data
    pivot = dat(right)
    ! an element in the middle is a much better choice for presorted data
    ! swap it with the rightmost element.
    ! uncomment this
     pivot = dat((left+right)/2)
     dat((left+right)/2) = dat(right)
    i = left
    DO j=left,right-1
        IF (pivot > dat(j)) THEN
            tmp = dat(i)
            dat(i) = dat(j)
            dat(j) = tmp
            i = i+1
        END IF
    END DO
    dat(right) = dat(i)
    dat(i) = pivot
  END FUNCTION select_pivot

END MODULE sorting
