module myobject_abs
  implicit none

  type, abstract :: mytype_abs
    integer :: a
  contains
    procedure (mytype_printa), deferred :: printa
    procedure (mytype_add2a), deferred :: add2a
  end type

  abstract interface
     subroutine mytype_printa(self)
        import mytype_abs
        class (mytype_abs),intent(in) :: self
     end subroutine

     subroutine mytype_add2a(self, b)
        import mytype_abs
        class (mytype_abs),intent(inout) :: self
        integer, intent(in) :: b
     end subroutine
  end interface

end module
