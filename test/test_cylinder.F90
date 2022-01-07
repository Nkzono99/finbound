program test_cylinder
    use m_boundary_base
    use m_cylinder_boundary
    use m_boundary_assertion
    implicit none

    print *, "test_cylinderXYZ_check_collision"
    call test_cylinderXYZ_check_collision

    print *, "test_cylinderXYZ_is_overlap"
    call test_cylinderXYZ_is_overlap

contains

    subroutine test_cylinderXYZ_check_collision
        type(t_CylinderXYZ) :: cylinder
        double precision :: origin(3)
        double precision :: p1(3), p2(3)
        type(t_CollisionRecord) :: record

        ! p1  |  p2  |
        origin = [0.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderX(origin, 1.d0, 5.0d0)
        p1 = [1.0d0, -1.0d0, 1.0d0]
        p2 = [1.0d0, 1.0d0, 1.0d0]
        record = cylinder%check_collision(p1, p2)
        call assert_record(record, .true., [1.0d0, 0.0d0, 1.0d0], 0.5d0)

        ! p2  |  p1  |
        origin = [0.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderX(origin, 1.d0, 5.0d0)
        p1 = [1.0d0, 1.0d0, 1.0d0]
        p2 = [1.0d0, -1.0d0, 1.0d0]
        record = cylinder%check_collision(p1, p2)
        call assert_record(record, .true., [1.0d0, 0.0d0, 1.0d0], 0.5d0)

        ! p1 p2 |   |
        origin = [0.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderX(origin, 1.d0, 5.0d0)
        p1 = [1.0d0, -1.0d0, 1.0d0]
        p2 = [1.0d0, -2.0d0, 1.0d0]
        record = cylinder%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! |   | p1 p2
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderX(origin, 1.0d0, 5.0d0)
        p1 = [1.0d0, 3.0d0, 1.0d0]
        p2 = [1.0d0, 4.0d0, 1.0d0]
        record = cylinder%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p1 |  | p2
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderX(origin, 1.0d0, 5.0d0)
        p1 = [1.0d0, -1.0d0, 1.0d0]
        p2 = [1.0d0, 3.0d0, 1.0d0]
        record = cylinder%check_collision(p1, p2)
        call assert_record(record, .true., [1.0d0, 0.0d0, 1.0d0], 0.25d0)

        ! Y-axis
        ! p1  |  p2  |
        origin = [1.0d0, 0.0d0, 1.0d0]
        cylinder = new_cylinderY(origin, 1.d0, 5.0d0)
        p1 = [-1.0d0, 1.0d0, 1.0d0]
        p2 = [1.0d0, 1.0d0, 1.0d0]
        record = cylinder%check_collision(p1, p2)
        call assert_record(record, .true., [0.0d0, 1.0d0, 1.0d0], 0.5d0)
    end subroutine

    subroutine test_cylinderXYZ_is_overlap
        type(t_CylinderXYZ) :: cylinder
        double precision :: origin(3)
        double precision :: sdoms(2, 3)

        double precision :: extent(2, 3)

        extent = reshape([[0, 0], [0, 0], [0, 0]], [2, 3])

        ! Z: W--|--|--W
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderZ(origin, 1.0d0, 5.0d0)
        sdoms = reshape([[-1.0d0, 3.0d0], [0.0d0, 2.0d0], [0.0d0, 2.0d0]], [2, 3])
        call assert_logical(cylinder%is_overlap(sdoms, extent=extent), .true.)

        ! Z: W-W | |
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderZ(origin, 1.0d0, 5.0d0)
        sdoms = reshape([[-2.0d0, -1.0d0], [0.0d0, 2.0d0], [0.0d0, 2.0d0]], [2, 3])
        call assert_logical(cylinder%is_overlap(sdoms, extent=extent), .false.)

        ! Z: | | W-W
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderZ(origin, 1.0d0, 5.0d0)
        sdoms = reshape([[3.0d0, 4.0d0], [0.0d0, 2.0d0], [0.0d0, 2.0d0]], [2, 3])
        call assert_logical(cylinder%is_overlap(sdoms, extent=extent), .false.)

        ! Y: W--|--|--W
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderY(origin, 1.0d0, 5.0d0)
        sdoms = reshape([[-1.0d0, 3.0d0], [0.0d0, 2.0d0], [0.0d0, 2.0d0]], [2, 3])
        call assert_logical(cylinder%is_overlap(sdoms, extent=extent), .true.)

        ! Y: W-W | |
        origin = [1.0d0, 1.0d0, 1.0d0]
        cylinder = new_cylinderZ(origin, 1.0d0, 5.0d0)
        sdoms = reshape([[-2.0d0, -1.0d0], [0.0d0, 2.0d0], [0.0d0, 2.0d0]], [2, 3])
        call assert_logical(cylinder%is_overlap(sdoms, extent=extent), .false.)
    end subroutine

end program
