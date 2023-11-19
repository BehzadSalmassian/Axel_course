module list_types
  implicit none

  type :: pair
    integer :: key
    real :: value
  end type

  type Node
    integer :: data
    type(Node), pointer :: next => null()

  end type Node

  contains

  subroutine appendNode(listHead, value)
    type(Node), pointer :: listHead
    integer, intent(in) :: value

    type(Node), pointer :: newNode, current

!---------------------Allocate memory for the new node
    allocate(newNode)

!-------------------Initialize the new node
    newNode%data = value
    newNode%next => null()

    ! If the list is empty, set the head to the new node
    if (.not. associated(listHead)) then
      listHead => newNode
    else
      ! Otherwise, find the end of the list and append the new node
      current => listHead
      do while (associated(current%next))
        current => current%next
      end do
      current%next => newNode
    end if
  end subroutine appendNode

end module list_types
