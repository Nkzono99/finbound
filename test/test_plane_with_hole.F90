program test_plane_with_hole
    use m_boundary_base
    use m_plane_with_hole_boundary
    use m_boundary_assertion
    implicit none

    print *, "test_planeXYZWithCircleHole_check_collision"
    call test_planeXYZ_check_collision

    print *, "test_planeXYZWithCircleHole_is_overlap"
    call test_planeXYZ_is_overlap

contains

    subroutine test_planeXYZ_check_collision
        type(t_PlaneXYZWithCircleHole) :: plane
        double precision :: origin(3)
        double precision :: p1(3), p2(3)
        type(t_CollisionRecord) :: record

        ! p1  |  p2
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [2.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 0.0d0, 0.0d0], 0.25d0)

        ! p2 | p1
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        p1 = [2.0d0, 0.0d0, 0.0d0]
        p2 = [0.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 0.0d0, 0.0d0], 0.75d0)

        ! p1 p2 |
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [0.48d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! | p1 p2
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        p1 = [0.51d0, 0.0d0, 0.0d0]
        p2 = [1.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p1 |
        !    | p2
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        p1 = [0.0d0, 0.0d0, 2.0d0]
        p2 = [2.0d0, 4.0d0, 4.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 1.0d0, 2.5d0], 0.25d0)

        !     |
        ! p1     p2
        !     |
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, 1.0d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [2.0d0, 0.0d0, 0.0d0]
        record = plane%check_collision(p1, p2)
        call assert_record(record, .false.)
    end subroutine

    subroutine test_planeXYZ_is_overlap
        type(t_PlaneXYZWithCircleHole) :: plane
        double precision :: origin(3)
        double precision :: sdoms(2, 3)

        ! X: W  |  W
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(plane%is_overlap(sdoms), .true.)

        ! X: W W |
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        sdoms = reshape([[-2.0d0, -1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(plane%is_overlap(sdoms), .false.)

        ! X: | W W
        origin = [0.5d0, 0.0d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleX(origin, -1.0d0)
        sdoms = reshape([[3.0d0, 4.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(plane%is_overlap(sdoms), .false.)

        ! Y: W  |  W
        origin = [0.0d0, 0.5d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleY(origin, -1.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(plane%is_overlap(sdoms), .true.)

        ! Y: W W |
        origin = [0.0d0, 0.5d0, 0.0d0]
        plane = new_planeXYZWithCircleHoleY(origin, -1.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [-2.0d0, -1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(plane%is_overlap(sdoms), .false., message="Y: W W |")
    end subroutine

end program
