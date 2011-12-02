! { dg-do compile }
!
! PR 49638: [OOP] length parameter is ignored when overriding type bound character functions with constant length.
!
! Original test case contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module m

  implicit none

  type :: t1
   contains
     procedure, nopass :: a => a1
     procedure, nopass :: b => b1
     procedure, nopass :: c => c1
     procedure, nopass :: d => d1
     procedure, nopass :: e => e1
  end type

  type, extends(t1) :: t2
   contains
     procedure, nopass :: a => a2  ! { dg-error "Character length mismatch" }
     procedure, nopass :: b => b2  ! { dg-error "should have matching result types and ranks" }
     procedure, nopass :: c => c2  ! { dg-warning "Possible character length mismatch" }
     procedure, nopass :: d => d2  ! valid, check for commutativity (+,*)
     procedure, nopass :: e => e2  ! { dg-warning "Possible character length mismatch" }
  end type

contains

  function a1 ()
    character(len=6) :: a1
  end function

  function a2 ()
    character(len=7) :: a2
  end function

  function b1 ()
    integer :: b1
  end function

  function b2 ()
    integer, dimension(2) :: b2
  end function

  function c1 (x)
    integer, intent(in) :: x
    character(2*x) :: c1
  end function

  function c2 (x)
    integer, intent(in) :: x
    character(3*x) :: c2
  end function

  function d1 (y)
    integer, intent(in) :: y
    character(2*y+1) :: d1
  end function

  function d2 (y)
    integer, intent(in) :: y
    character(1+y*2) :: d2
  end function

  function e1 (z)
    integer, intent(in) :: z
    character(3) :: e1
  end function

  function e2 (z)
    integer, intent(in) :: z
    character(z) :: e2
  end function

end module m




module w1

 implicit none

 integer :: n = 1

 type :: tt1
 contains
   procedure, nopass :: aa => aa1
 end type

contains

 function aa1 (m)
  integer, intent(in) :: m
  character(n+m) :: aa1
 end function

end module w1


module w2

 use w1, only : tt1

 implicit none

 integer :: n = 2

 type, extends(tt1) :: tt2
 contains
   procedure, nopass :: aa => aa2  ! { dg-warning "Possible character length mismatch" }
 end type

contains

 function aa2 (m)
  integer, intent(in) :: m
  character(n+m) :: aa2
 end function

end module w2

! { dg-final { cleanup-modules "m w1 w2" } }
