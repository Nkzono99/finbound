module m_boundary_assertion
    use m_boundary_base
    implicit none

    private
    public assert_record
    
contains

    subroutine assert_record(record, is_collided, position, t, message)
        type(t_CollisionRecord), intent(in) :: record
        logical, intent(in) :: is_collided
        double precision, intent(in), optional :: position(3)
        double precision, intent(in), optional :: t
        character(len=*), intent(in), optional :: message
        ! character, allocatable :: message_(:)
        character(len=50) :: message_

        if (present(message)) then
            message_ = message
        else
            message_ = record%to_string()
        end if

        if (record%is_collided /= is_collided) then
            print *, "AssertionError (is_collided): "//message_
            return
        end if

        if (present(position)) then
            if (.not. all(record%position == position)) then
                print *, "AssertionError (position): "//message_
                return
            end if
        end if

        if (present(t)) then
            if (record%t /= t) then
                print *, "AssertionError (t): "//message_
                return
            end if
        end if

        print *, 'ok'
    end subroutine
end module