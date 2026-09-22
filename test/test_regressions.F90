program test_regressions
    use finbound
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none

    integer :: failures = 0, checks = 0
    double precision, parameter :: zero(3) = 0d0, no_extent(2, 3) = 0d0
    double precision, parameter :: pi = acos(-1d0)

    call test_polygon_positions
    call test_rotation
    call test_overlap
    call test_degenerate_motion
    call test_quadrics
    call test_camera
    call test_records

    print *, checks, ' checks;', failures, ' failures'
    if (failures /= 0) error stop 1

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label
        checks = checks + 1
        if (.not. condition) then
            failures = failures + 1
            print *, 'FAIL: ', label
        end if
    end subroutine

    subroutine vector_close(actual, expected, label)
        double precision, intent(in) :: actual(3), expected(3)
        character(*), intent(in) :: label
        call check(all(ieee_is_finite(actual)), label//' finite')
        if (all(ieee_is_finite(actual))) then
            call check(maxval(abs(actual - expected)) < 1d-10, label)
        end if
    end subroutine

    subroutine collision_at(boundary, p1, p2, expected_t, expected_pos, label)
        class(t_Boundary), intent(in) :: boundary
        double precision, intent(in) :: p1(3), p2(3), expected_t, expected_pos(3)
        character(*), intent(in) :: label
        type(t_CollisionRecord) :: record
        record = boundary%check_collision(p1, p2)
        call check(record%is_collided, label//' collision')
        if (.not. record%is_collided) return
        call check(abs(record%t - expected_t) < 1d-10, label//' t')
        call vector_close(record%position, expected_pos, label//' position')
    end subroutine

    subroutine test_polygon_positions
        type(t_Triangle) :: triangle
        type(t_Rectangle) :: rectangle
        double precision :: vertices(3, 4)
        vertices(:, 1) = [0d0, 0d0, 2d0]
        vertices(:, 2) = [2d0, 0d0, 2d0]
        vertices(:, 3) = [2d0, 2d0, 2d0]
        vertices(:, 4) = [0d0, 2d0, 2d0]
        triangle = new_Triangle(vertices(:, 1:3))
        rectangle = new_Rectangle(vertices)
        call collision_at(triangle, [1.5d0, 0.5d0, 0d0], [1.5d0, 0.5d0, 4d0], &
                          0.5d0, [1.5d0, 0.5d0, 2d0], 'triangle')
        call collision_at(rectangle, [0.5d0, 1.5d0, 0d0], [0.5d0, 1.5d0, 4d0], &
                          0.5d0, [0.5d0, 1.5d0, 2d0], 'rectangle second triangle')
        call check_metadata(triangle, [1.5d0, 0.5d0, 0d0], [1.5d0, 0.5d0, 4d0], 'triangle')
        call check_metadata(rectangle, [0.5d0, 1.5d0, 0d0], [0.5d0, 1.5d0, 4d0], 'rectangle')
    end subroutine

    subroutine test_rotation
        type(t_PlaneXYZ), target :: plane
        type(t_Sphere), target :: sphere
        class(t_Boundary), pointer :: boundary
        type(t_BoundaryRotationXYZ) :: rotation
        type(t_HitRecord) :: hit
        double precision :: box(2, 3), extent(2, 3)
        plane = new_planeX(0d0)
        boundary => plane
        rotation = new_BoundaryRotationZ(boundary, pi/2d0, [3d0, 4d0, 5d0])
        ! futils uses clockwise rotations for a positive angle.
        call vector_close(rotation%pnormal([3d0, 4d0, 5d0]), [0d0, -1d0, 0d0], 'rotated normal')
        call collision_at(rotation, [3d0, 3d0, 5d0], [3d0, 5d0, 5d0], &
                          0.5d0, [3d0, 4d0, 5d0], 'rotated plane')
        hit = rotation%hit(new_Ray([3d0, 5d0, 5d0], [0d0, -1d0, 0d0]))
        call check(hit%is_hit, 'rotated ray hit')
        if (hit%is_hit) call vector_close(hit%n, [0d0, 1d0, 0d0], 'rotated ray normal')
        hit = rotation%hit(new_Ray([3d0, 5d0, 5d0], [0d0, 1d0, 0d0]))
        call check(.not. hit%is_hit, 'rotated ray miss')
        box = reshape([2.9d0, 3.1d0, 3.9d0, 4.1d0, 4.9d0, 5.1d0], [2, 3])
        call check(rotation%is_overlap(box, no_extent), 'rotated overlap uses local coordinates')
        box(:, 2) = [6d0, 7d0]
        call check(.not. rotation%is_overlap(box, no_extent), 'rotated overlap outside')
        box(:, 2) = [4.2d0, 4.3d0]
        extent = 0d0
        extent(1, 2) = 0.3d0
        call check(rotation%is_overlap(box, extent), 'rotated overlap asymmetric extent')

        ! Two opposite corners do not bound a rotated box: all eight are needed.
        sphere = new_Sphere([1.2d0, 0d0, 0d0], 0.1d0)
        boundary => sphere
        rotation = new_BoundaryRotationZ(boundary, pi/4d0)
        box = reshape([-1d0, 1d0, -1d0, 1d0, -0.1d0, 0.1d0], [2, 3])
        call check(rotation%is_overlap(box, no_extent), 'rotated box corner bounds')

        plane = new_planeX(0d0)
        boundary => plane
        rotation = new_BoundaryRotationZ(boundary, pi/2d0, [1d20, 0d0, 0d0])
        call vector_close(rotation%pnormal([1d20, 0d0, 0d0]), [0d0, -1d0, 0d0], 'large origin normal')
        hit = rotation%hit(new_Ray([1d20, 2d0, 0d0], [0d0, -1d-10, 0d0]))
        call check(hit%is_hit, 'rotated small ray direction')
        if (hit%is_hit) then
            call check(abs(hit%t/2d10 - 1d0) < 1d-12, 'rotated small direction t')
            call vector_close(hit%n, [0d0, 1d0, 0d0], 'large origin ray normal')
        end if
    end subroutine

    subroutine test_overlap
        type(t_CircleXYZ) :: circle
        type(t_DonutXYZ) :: donut
        type(t_Plane) :: plane
        type(t_CutSphereXYZ) :: sphere
        double precision :: box(2, 3), extent(2, 3)
        integer :: axis, transverse
        do axis = 1, 3
            circle = new_CircleXYZ(axis, zero, 2d0)
            donut = new_DonutXYZ(axis, zero, 2d0, 1d0)
            box(1, :) = -0.1d0
            box(2, :) = 0.1d0
            box(:, axis) = [3d0, 4d0]
            call check(.not. circle%is_overlap(box, no_extent), 'circle distant axial box')
            call check(.not. donut%is_overlap(box, no_extent), 'donut distant axial box')
            box(:, axis) = [-0.1d0, 0.1d0]
            transverse = mod(axis, 3) + 1
            box(:, transverse) = [-1.5d0, -1.4d0]
            call check(circle%is_overlap(box, no_extent), 'circle negative radial box')
            call check(donut%is_overlap(box, no_extent), 'donut negative radial box')
            box(:, transverse) = [-4d0, -3d0]
            call check(.not. circle%is_overlap(box, no_extent), 'circle distant radial box')
            call check(.not. donut%is_overlap(box, no_extent), 'donut distant radial box')
            extent = 0d0
            extent(2, transverse) = 1.5d0
            call check(circle%is_overlap(box, extent), 'circle extent respected')
            call check(donut%is_overlap(box, extent), 'donut extent respected')
        end do
        plane = new_plane(zero, [1d0, -1d0, 0d0])
        box(1, :) = 0d0
        box(2, :) = 1d0
        call check(plane%is_overlap(box, no_extent), 'plane through first box corner')
        plane = new_plane(zero, [1d0, 0d0, 0d0])
        call check(plane%is_overlap(box, no_extent), 'plane touching box face')
        box(:, 1) = [2d0, 3d0]
        call check(.not. plane%is_overlap(box, no_extent), 'plane outside box')
        sphere = new_CutSphereZ(zero, 1d0, -1d0, 1d0)
        box = reshape([1.5d0, 1.6d0, -0.1d0, 0.1d0, -0.1d0, 0.1d0], [2, 3])
        call check(.not. sphere%is_overlap(box, no_extent), 'cut sphere extent applied once')
    end subroutine

    subroutine no_motion(boundary, p, direction, label)
        class(t_Boundary), intent(in) :: boundary
        double precision, intent(in) :: p(3), direction(3)
        character(*), intent(in) :: label
        type(t_CollisionRecord) :: record
        type(t_HitRecord) :: hit
        record = boundary%check_collision(p, p + direction)
        hit = boundary%hit(new_Ray(p, direction))
        call check(.not. record%is_collided, label//' segment miss')
        call check(.not. hit%is_hit, label//' ray miss')
    end subroutine

    subroutine test_degenerate_motion
        type(t_CircleXYZ) :: circle
        type(t_DonutXYZ) :: donut
        type(t_PlaneXYZWithCircleHole) :: hole
        type(t_CylinderXYZ) :: cylinder
        type(t_Sphere) :: sphere
        type(t_CutSphereXYZ) :: cut_sphere
        type(t_EllipsoidXYZ) :: ellipsoid
        type(t_HyperboloidXYZ) :: hyperboloid
        type(t_HitRecord) :: hit
        circle = new_CircleZ(zero, 2d0)
        donut = new_DonutZ(zero, 2d0, 1d0)
        hole = new_planeXYZWithCircleHoleZ(zero, 1d0)
        cylinder = new_cylinderZ(zero, 1d0, 2d0)
        sphere = new_Sphere(zero, 1d0)
        cut_sphere = new_CutSphereZ(zero, 1d0, -1d0, 1d0)
        ellipsoid = new_ellipsoidZ(zero, 1d0, 2d0, 2d0)
        hyperboloid = new_hyperboloidZ(zero, 2d0, 1d0, 2d0)
        call no_motion(circle, zero, [1d0, 0d0, 0d0], 'coplanar circle')
        call no_motion(donut, [1.5d0, 0d0, 0d0], [0d0, 1d0, 0d0], 'coplanar donut')
        call no_motion(hole, [2d0, 0d0, 0d0], [0d0, 1d0, 0d0], 'coplanar hole')
        call no_motion(circle, [0d0, 0d0, 1d0], zero, 'stationary circle')
        call no_motion(donut, [1.5d0, 0d0, 1d0], zero, 'stationary donut')
        call no_motion(hole, [2d0, 0d0, 1d0], zero, 'stationary hole')
        call no_motion(cylinder, zero, [0d0, 0d0, 1d0], 'axial cylinder')
        call no_motion(sphere, zero, zero, 'stationary sphere')
        call no_motion(cut_sphere, zero, zero, 'stationary cut sphere')
        call no_motion(ellipsoid, zero, zero, 'stationary ellipsoid')
        call no_motion(hyperboloid, zero, zero, 'stationary hyperboloid')
        hit = sphere%hit(new_Ray([-2d0, 1d0, 0d0], [1d0, 0d0, 0d0]))
        call check(hit%is_hit, 'tangent sphere hit')
        if (hit%is_hit) call vector_close(hit%n, [0d0, 1d0, 0d0], 'tangent sphere normal')
        hit = circle%hit(new_Ray([1d0, 0d0, 0d0], [0d0, 0d0, 1d0]))
        call check(hit%is_hit, 'circle ray starts on surface')
        if (hit%is_hit) call vector_close(hit%n, [0d0, 0d0, 1d0], 'surface start normal')
    end subroutine

    subroutine test_quadrics
        type(t_CylinderXYZ) :: cylinder
        type(t_HyperboloidXYZ) :: hyperboloid
        type(t_Sphere) :: sphere
        type(t_CollisionRecord) :: record
        type(t_HitRecord) :: hit
        cylinder = new_cylinderZ(zero, 1d0, 1d0)
        call collision_at(cylinder, [-2d0, 0d0, -1d0], [2d0, 0d0, 1d0], &
                          0.75d0, [1d0, 0d0, 0.5d0], 'cylinder second root in height')
        hyperboloid = new_hyperboloidZ(zero, sqrt(2d0), 1d0, 2d0)
        call collision_at(hyperboloid, [0.5d0, 0d0, 0d0], [1.5d0, 0d0, 1d0], &
                          0.75d0, [1.25d0, 0d0, 0.75d0], 'nearly linear hyperboloid intersection')
        ! Set an exactly representable surface x*x + y*y - z*z = 1.
        hyperboloid%c = 1d0
        call collision_at(hyperboloid, [0.5d0, 0d0, 0d0], [1.5d0, 0d0, 1d0], &
                          0.75d0, [1.25d0, 0d0, 0.75d0], 'linear hyperboloid intersection')
        hit = hyperboloid%hit(new_Ray([0.5d0, 0d0, 0d0], [1d0, 0d0, 1d0]))
        call check(hit%is_hit, 'linear hyperboloid ray')
        if (hit%is_hit) call vector_close(hit%position, [1.25d0, 0d0, 0.75d0], 'linear hyperboloid position')
        call no_motion(hyperboloid, zero, [1d0, 0d0, 1d0], 'parallel hyperboloid no root')
        sphere = new_Sphere(zero, 1d0)
        hit = sphere%hit(new_Ray([1d0 + 1d-8, 0d0, 0d0], [-1d-9, 0d0, 0d0]))
        call check(hit%is_hit, 'sphere small direction')
        if (hit%is_hit) then
            call check(abs(hit%t - 10d0) < 1d-5, 'sphere small direction t')
            call vector_close(hit%position, [1d0, 0d0, 0d0], 'sphere small direction position')
        end if
        record = sphere%check_collision([1d0 + 1d-8, 0d0, 0d0], [1d0 - 1d-8, 0d0, 0d0])
        call check(record%is_collided, 'sphere short segment')
        if (record%is_collided) call vector_close(record%position, [1d0, 0d0, 0d0], 'sphere short segment position')
    end subroutine

    subroutine test_camera
        type(t_ParallelCamera) :: camera
        type(t_Ray) :: ray
        camera = new_ParallelCamera(0d0, 0d0, 3, 5, 7)
        ray = camera%generate_randray([0.5d0, 0.5d0])
        call check(abs(ray%origin(1) - 1.5d0) < 1d-10, 'odd camera x center')
        call check(abs(ray%origin(2) - 2.5d0) < 1d-10, 'odd camera y center')
        camera = new_ParallelCamera(0d0, 0d0, 50000, 50000, 50000)
        call check(abs(camera%S/7.5d9 - 1d0) < 1d-12, 'camera area integer overflow')
        call check(abs(camera%Sl/2.5d9 - 1d0) < 1d-12, 'camera projected area integer overflow')
        camera = new_ParallelCamera_optimized(0d0, 0d0, reshape([1, 4, 2, 7, 3, 10], [2, 3]))
        call check(camera%nx == 3 .and. camera%ny == 5 .and. camera%nz == 7, 'optimized camera dimensions')
    end subroutine

    subroutine test_records
        type(t_BoundaryList) :: list
        class(t_Boundary), pointer :: boundary
        type(t_HitRecord) :: hit
        list = new_BoundaryList(1)
        allocate (t_PlaneXYZ :: boundary)
        select type (boundary)
        type is (t_PlaneXYZ)
            boundary = new_planeX(2d30)
        end select
        call list%add_boundary(boundary)
        hit = list%hit(new_Ray(zero, [1d0, 0d0, 0d0]))
        call check(hit%is_hit, 'list ray has no artificial distance cutoff')
        call list%destroy()

        call check_metadata(new_plane(zero, [0d0, 0d0, 1d0]), [0d0, 0d0, 1d0], &
                            [0d0, 0d0, -1d0], 'plane')
        call check_metadata(new_planeZ(0d0), [0d0, 0d0, 1d0], [0d0, 0d0, -1d0], 'planeXYZ')
        call check_metadata(new_rectangleZ(zero, 2d0, 2d0), [1d0, 1d0, 1d0], &
                            [1d0, 1d0, -1d0], 'rectangleXYZ')
        call check_metadata(new_CircleZ(zero, 2d0), [1d0, 0d0, 1d0], [1d0, 0d0, -1d0], 'circle')
        call check_metadata(new_DonutZ(zero, 2d0, 1d0), [1.5d0, 0d0, 1d0], [1.5d0, 0d0, -1d0], 'donut')
        call check_metadata(new_planeXYZWithCircleHoleZ(zero, 1d0), [2d0, 0d0, 1d0], &
                            [2d0, 0d0, -1d0], 'hole')
        call check_metadata(new_cylinderZ(zero, 1d0, 2d0), [-2d0, 0d0, 1d0], [0d0, 0d0, 1d0], 'cylinder')
        call check_metadata(new_Sphere(zero, 1d0), [-2d0, 0d0, 0d0], zero, 'sphere')
        call check_metadata(new_CutSphereZ(zero, 1d0, -1d0, 1d0), [-2d0, 0d0, 0d0], zero, 'cut sphere')
        call check_metadata(new_ellipsoidZ(zero, 1d0, 2d0, 2d0), [-3d0, 0d0, 0d0], zero, 'ellipsoid')
        call check_metadata(new_hyperboloidZ(zero, 2d0, 1d0, 2d0), [-2d0, 0d0, 0d0], zero, 'hyperboloid')
    end subroutine

    subroutine check_metadata(boundary, p1, p2, label)
        class(t_Boundary), intent(in) :: boundary
        double precision, intent(in) :: p1(3), p2(3)
        character(*), intent(in) :: label
        class(t_Boundary), allocatable :: copy
        type(t_CollisionRecord) :: record
        type(t_HitRecord) :: hit
        allocate (copy, source=boundary)
        copy%priority = 7
        copy%material%tag = 11
        record = copy%check_collision(p1, p2)
        hit = copy%hit(new_Ray(p1, p2 - p1))
        call check(record%is_collided, label//' metadata collision')
        call check(hit%is_hit, label//' metadata hit')
        if (record%is_collided) then
            call check(record%priority == 7, label//' collision priority')
            call check(record%material%tag == 11, label//' collision material')
        end if
        if (hit%is_hit) then
            call check(hit%priority == 7, label//' hit priority')
            call check(hit%material%tag == 11, label//' hit material')
        end if
    end subroutine
end program
