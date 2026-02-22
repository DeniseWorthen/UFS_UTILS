module assertion_mod

  use gengrid_kinds, only : dbl_kind, int_kind, real_kind

  implicit none

  private
  public :: assert_equal

  interface assert_equal
     module procedure assert_int
     module procedure assert_real
     module procedure assert_double
  end interface assert_equal

contains
  subroutine assert_int(actual, expected, msg, rc, returnmsg)
    integer(int_kind), intent(in)  :: actual, expected
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = (actual == expected)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       write(returnmsg, '(2(a,i0))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    end if
  end subroutine assert_int

  subroutine assert_real(actual, expected, tol, msg, rc, returnmsg)
    real(real_kind),   intent(in)  :: actual, expected, tol
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = (abs(actual - expected) <= tol)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       write(returnmsg, '(2(a,g15.8))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    end if
  end subroutine assert_real

  subroutine assert_double(actual, expected, tol, msg, rc, returnmsg)
    real(dbl_kind),    intent(in)  :: actual, expected, tol
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = (abs(actual - expected) <= tol)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       write(returnmsg, '(2(a,g20.13))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    end if
  end subroutine assert_double
end module assertion_mod
