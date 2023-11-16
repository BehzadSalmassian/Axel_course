!!---------------------FifthExercise-------------------!!
!!---------------------FirstDefinition-----------------!!
program FifthExercise
  implicit none

!!---------------------ParametersInitialization---------!!
  real, dimension(:), allocatable :: Data
  integer :: ArraySize, i
  real :: SumAll, ExpectedSum
  character(len=50) :: fileName, Checker
  logical :: SumMatch, AscendSort, DescendSort

!!--------------------CommandlineFileName-----------------!!
  if (command_argument_count() > 0) then
    call get_command_argument(1, fileName)

  else
    print *, "Error: File name not provided in the command line."
    stop
  end if

!!--------------------OpenFileReadSize------------------!!
  open(unit=5, file=fileName, status='old', action='read')
  read(5, *) ArraySize

!!--------------------ReadDataAllocateMemory------------!!
  allocate(Data(ArraySize))
  read(5, *) Data

!!--------------------ReadStoreExpectedSum--------------!!
  read(5, *) ExpectedSum

!!--------------------ReadStoreExpectedSum--------------!!
!!  print *, "For Check Ascending Write 1 For Check Descending Write 2 :"
  read(5, '(A1)') Checker

!!--------------------CloseFile-------------------------!!
  close(5)

!!--------------------SwitchCase------------------------!!
  SELECT CASE (Checker)
!!-------------
  CASE ('1')
!!--------------------CheckAscendSort----------------!!
    AscendSort = .true.
    do i = 1, ArraySize-1
      if (Data(i) > Data(i+1)) then
        AscendSort = .false.
        exit
      end if
    end do
    print *, "Is Ascending Sorted:", AscendSort

!!-------------
  CASE ('2')
!!--------------------CheckDescendSort----------------!!
    DescendSort = .true.
    do i = 1, ArraySize-1
      if (Data(i) < Data(i+1)) then
        DescendSort = .false.
        exit
      end if
    end do
    print *, "Is Descending Sorted:", DescendSort

!!--------------
  CASE DEFAULT
    print *, "Undefined"
!!--------------
  END SELECT

!!--------------------CalculateSumAll-------------------!!
  SumAll = sum(Data)

!!--------------------CheckSumMatch---------------------!!
  SumMatch = (SumAll == ExpectedSum)

!!--------------------OutputResults---------------------!!
!!  print *, "Size of the array:", ArraySize
!!  print *, "Check Sum Match:", SumMatch
!!  print *, "Computed and Expected Sum:", SumAll
!!  print *, "Is Ascending Sorted:", AscendSort
!!  print *, "Is Descending Sorted:", DescendSort

!!--------------------IfPrint---------------------------!!
  if (.not. SumMatch) then
    print *, "Expected Sum:", ExpectedSum
    print *, "Computed Sum:", SumAll
  end if

!!--------------------EndCode---------------------------!!
end program FifthExercise
