program test_sphere
    use m_boundary_base
    use m_sphere_boundary
    use m_boundary_assertion
    implicit none

    print *, "test_sphere_check_collision"
    call test_sphere_check_collision

contains

    subroutine test_sphere_check_collision
        use m_vector
        type(t_Sphere) :: sphere
        double precision :: p1(3), p2(3)
        type(t_CollisionRecord) :: record

        ! p1 p2 | |
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [-2d0, 1d0, 1d0]
        p2 = [-1.5d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p1 | p2 |
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [-2d0, 1d0, 1d0]
        p2 = [0d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .true., [-1d0, 1d0, 1d0], 0.50d0)

        ! p1 | | p2
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [-2d0, 1d0, 1d0]
        p2 = [3d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .true., [-1d0, 1d0, 1d0], 0.2d0)

        ! | p1 | p2
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [0d0, 1d0, 1d0]
        p2 = [4d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .true., [3d0, 1d0, 1d0], 0.75d0)

        ! | | p1 p2
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [4d0, 1d0, 1d0]
        p2 = [5d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p2 p1 | |
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [-1.5d0, 1d0, 1d0]
        p2 = [-2d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p2 | p1 |
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [0d0, 1d0, 1d0]
        p2 = [-2d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .true., [-1d0, 1d0, 1d0], 0.5d0)

        ! p2 | | p1
        sphere = new_Sphere([1d0, 1d0, 1d0], 2d0)
        p1 = [3d0, 1d0, 1d0]
        p2 = [-2d0, 1d0, 1d0]
        record = sphere%check_collision(p1, p2)
        call assert_record(record, .true., [3d0, 1d0, 1d0], 0d0)
    end subroutine
end program
