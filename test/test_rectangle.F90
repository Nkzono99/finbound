program test_rectangle
    use m_boundary_base
    use m_rectangle_boundary
    use m_boundary_assertion
    implicit none

    print *, "test_rectangleXYZ_check_collision"
    call test_rectangleXYZ_check_collision

    print *, "test_rectangleXYZ_is_overlap"
    call test_rectangleXYZ_is_overlap

contains

    subroutine test_rectangleXYZ_check_collision
        type(t_RectangleXYZ) :: rectangle
        double precision :: origin(3)
        double precision :: p1(3), p2(3)
        type(t_CollisionRecord) :: record

        ! p1  |  p2
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [2.0d0, 0.0d0, 0.0d0]
        record = rectangle%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 0.0d0, 0.0d0], 0.25d0)

        ! p2 | p1
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        p1 = [2.0d0, 0.0d0, 0.0d0]
        p2 = [0.0d0, 0.0d0, 0.0d0]
        record = rectangle%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 0.0d0, 0.0d0], 0.75d0)

        ! p1 p2 |
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        p1 = [0.0d0, 0.0d0, 0.0d0]
        p2 = [0.48d0, 0.0d0, 0.0d0]
        record = rectangle%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! | p1 p2
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        p1 = [0.51d0, 0.0d0, 0.0d0]
        p2 = [1.0d0, 0.0d0, 0.0d0]
        record = rectangle%check_collision(p1, p2)
        call assert_record(record, .false.)

        ! p1 |
        !    | p2
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 5.0d0)
        p1 = [0.0d0, 0.0d0, 2.0d0]
        p2 = [2.0d0, 4.0d0, 4.0d0]
        record = rectangle%check_collision(p1, p2)
        call assert_record(record, .true., [0.5d0, 1.0d0, 2.5d0], 0.25d0)

        ! p1 |
        !    | p2
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        p1 = [0.0d0, 0.0d0, 2.0d0]
        p2 = [2.0d0, 4.0d0, 4.0d0]
        record = rectangle%check_collision(p1, p2)
        call assert_record(record, .false.)
    end subroutine

    subroutine test_rectangleXYZ_is_overlap
        type(t_RectangleXYZ) :: rectangle
        double precision :: origin(3)
        double precision :: sdoms(2, 3)

        ! X: W  |  W
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .true.)

        ! X: W W |
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        sdoms = reshape([[-2.0d0, -1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .false.)

        ! X: | W W
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        sdoms = reshape([[3.0d0, 4.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .false.)

        ! Y: W  |  W
        origin = [-1.0d0, 0.5d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .true.)

        ! Y: W W |
        origin = [-1.0d0, 0.5d0, -1.0d0]
        rectangle = new_rectangleX(origin, 2.0d0, 2.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [-2.0d0, -1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .false.)

        ! X: W     W
        !       |
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 0.5d0, 2.0d0)
        sdoms = reshape([[0.0d0, 1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .true.)

        ! X: W W 
        !         |
        origin = [0.5d0, -1.0d0, -1.0d0]
        rectangle = new_rectangleX(origin, 0.5d0, 2.0d0)
        sdoms = reshape([[-2.0d0, -1.0d0], [0.0d0, 1.0d0], [0.0d0, 1.0d0]], [2, 3])
        call assert_logical(rectangle%is_overlap(sdoms), .false.)
    end subroutine

end program
