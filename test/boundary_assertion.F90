module m_boundary_assertion
    use m_boundary_base
    use futils, only : str
    implicit none

    private
    public assert_record
    public assert_logical
    
contains

    subroutine assert_logical(val, lgc, message)
        logical, intent(in) :: val
        logical, intent(in) :: lgc
        character(len=*), intent(in), optional :: message

        character(len=50) :: message_

        if (present(message)) then
            message_ = message
        else
            message_ = str(val)//' == '//str(lgc)//'(expected)'
        end if

        if (val .eqv. lgc) then
            print *, 'ok'
        else
            print *, "AssertionError: "//message_
        end if
    end subroutine

    subroutine assert_record(record, is_collided, position, t, message)
        type(t_CollisionRecord), intent(in) :: record
        logical, intent(in) :: is_collided
        double precision, intent(in), optional :: position(3)
        double precision, intent(in), optional :: t
        character(len=*), intent(in), optional :: message
        ! character, allocatable :: message_(:)
        character(len=100) :: message_

        if (present(message)) then
            message_ = message
        else
            message_ = record%to_string()
        end if

        if (record%is_collided .neqv. is_collided) then
            print *, "AssertionError (is_collided): "//message_
            return
        end if

        if (present(position)) then
            if (.not. sum(abs(record%position - position)) <= 0d0) then
                print *, "AssertionError (position): "//message_
                return
            end if
        end if

        if (present(t)) then
            if (abs(record%t-t) > 0d0) then
                print *, "AssertionError (t): "//message_
                return
            end if
        end if

        print *, 'ok'
    end subroutine
end module