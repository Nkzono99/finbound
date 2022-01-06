program test_boundary_list
    use m_boundary_base
    use m_plane_boundary
    use m_boundary_list
    implicit none

    print *, "test_boundary_list"

    call test_add_boundary

contains

    subroutine test_add_boundary
        type(t_BoundaryList) :: list
        class(t_Boundary), pointer :: pbound
        type(t_PlaneXYZ), pointer :: plane

        integer :: i
        double precision :: sum_pos_test, sum_pos

        list = new_BoundaryList()

        sum_pos_test = 0.0d0
        do i = 1, 100
            allocate (plane)
            plane = new_planeX(i*1.0d0)
            pbound => plane
            call list%add_boundary(pbound)

            sum_pos_test = sum_pos_test + i*1.0d0
        end do

        sum_pos = 0.0d0
        do i = 1, 100
            select type (p => list%boundaries(i)%ref)
            type is (t_PlaneXYZ)
                sum_pos = sum_pos + p%pos
            end select
        end do

        if (abs(sum_pos - sum_pos_test) <= 0.0d0) then
            print *, 'ok'
        else
            print *, "AssersionError: test add_boundary failed."
        end if

        call list%destroy
    end subroutine

end program
