!
! simple hash algorithm for integers for power of 2 size ranges

! multiply key with a large prime number that has bits set in every byte
!  and use the top N bits of the result.

! - size must be the next power of two from the desired hash range
! - get hash quickly through bitshift and bitmask
!

function inthash(key, size) result(hash)
  integer, intent(in) :: key, size
  integer :: hash, downshift, mask, i

  ! Initial hash calculation
  hash = key * 1103515249

  ! Calculate downshift and mask
  downshift = 0
  mask = size - 1

  do i = size, 2, -i/2
    downshift = downshift + 1
  end do

  ! Apply downshift and mask
  hash = ieor(shiftrr(hash, downshift), mask)

  ! Ensure the hash is non-negative
  if (hash < 0) then
    hash = 0
  end if
end function inthash

