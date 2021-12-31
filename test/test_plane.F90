program test_plane
    use m_boundary_base
    use m_plane_boundary
    use m_boundary_assertion
    implicit none

    call test_planeXYZ_check_collision

contains

    subroutine test_planeXYZ_check_collision
        type(t_PlaneXYZ) :: plane
        double precision :: p1(3), p2(3)
        type(t_CollisionRecord) :: record

        ! p1  |  p2
        plane = new_planeX(0.5d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [2.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 0.0d0, 0.0d0], 0.25d0)

        ! p2 | p1
        plane = new_planeX(0.5d0)
        p1 = [2.0d0, 0.0d0, 0.0d0]
        p2 = [0.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 0.0d0, 0.0d0], 0.75d0)

        ! p1 p2 |
        plane = new_planeX(0.5d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [0.48d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! | p1 p2
        plane = new_planeX(0.5d0)
        p1 = [0.51d0, 0.0d0, 0.0d0]
        p2 = [1.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p1 |
        !    | p2
        plane = new_planeX(0.5d0)
        p1 = [0.0d0, 0.0d0, 2.0d0]
        p2 = [2.0d0, 4.0d0, 4.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 1.0d0, 2.5d0], 0.25d0)
    end subroutine

end program
