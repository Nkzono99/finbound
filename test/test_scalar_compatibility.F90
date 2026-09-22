module scalar_test_extensions
    use finbound
    implicit none
    type, extends(t_Sphere) :: sphere_normal
    contains
        procedure :: normal => sphere_normal_override
    end type
    type, extends(t_Sphere) :: sphere_pnormal
    contains
        procedure :: pnormal => sphere_pnormal_override
    end type
    type, extends(t_CutSphereXYZ) :: cut_normal
    contains
        procedure :: normal => cut_normal_override
    end type
    type, extends(t_CutSphereXYZ) :: cut_pnormal
    contains
        procedure :: pnormal => cut_pnormal_override
    end type
    type, extends(t_CylinderXYZ) :: cylinder_normal
    contains
        procedure :: normal => cylinder_normal_override
    end type
    type, extends(t_CylinderXYZ) :: cylinder_pnormal
    contains
        procedure :: pnormal => cylinder_pnormal_override
    end type
    type, extends(t_Triangle) :: triangle_normal
    contains
        procedure :: normal => triangle_normal_override
    end type
    type, extends(t_Triangle) :: triangle_pnormal
    contains
        procedure :: pnormal => triangle_pnormal_override
    end type
contains
    pure function sphere_normal_override(self, position_on_boundary, headed_by_vector) result(n)
        class(sphere_normal), intent(in) :: self
        double precision, intent(in) :: position_on_boundary(3)
        double precision, intent(in), optional :: headed_by_vector(3)
        double precision :: n(3)
        n = [2d0, 3d0, 4d0] + position_on_boundary
        if (present(headed_by_vector)) n = n + headed_by_vector
    end function
    pure function sphere_pnormal_override(self, position) result(n)
        class(sphere_pnormal), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: n(3)
        n = [3d0, -4d0, 5d0] + position
    end function
    pure function cut_normal_override(self, position_on_boundary, headed_by_vector) result(n)
        class(cut_normal), intent(in) :: self
        double precision, intent(in) :: position_on_boundary(3)
        double precision, intent(in), optional :: headed_by_vector(3)
        double precision :: n(3)
        n = [2d0, 3d0, 4d0] + position_on_boundary
        if (present(headed_by_vector)) n = n + headed_by_vector
    end function
    pure function cut_pnormal_override(self, position) result(n)
        class(cut_pnormal), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: n(3)
        n = [3d0, -4d0, 5d0] + position
    end function
    pure function cylinder_normal_override(self, position_on_boundary, headed_by_vector) result(n)
        class(cylinder_normal), intent(in) :: self
        double precision, intent(in) :: position_on_boundary(3)
        double precision, intent(in), optional :: headed_by_vector(3)
        double precision :: n(3)
        n = [2d0, 3d0, 4d0] + position_on_boundary
        if (present(headed_by_vector)) n = n + headed_by_vector
    end function
    pure function cylinder_pnormal_override(self, position) result(n)
        class(cylinder_pnormal), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: n(3)
        n = [3d0, -4d0, 5d0] + position
    end function
    pure function triangle_normal_override(self, position_on_boundary, headed_by_vector) result(n)
        class(triangle_normal), intent(in) :: self
        double precision, intent(in) :: position_on_boundary(3)
        double precision, intent(in), optional :: headed_by_vector(3)
        double precision :: n(3)
        n = [2d0, 3d0, 4d0] + position_on_boundary
        if (present(headed_by_vector)) n = n + headed_by_vector
    end function
    pure function triangle_pnormal_override(self, position) result(n)
        class(triangle_pnormal), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: n(3)
        n = [3d0, -4d0, 5d0] + position
    end function
end module

program test_scalar_compatibility
    use scalar_test_extensions
    use m_vector, only: dot, normalized, normalize
    implicit none
    type(t_Sphere) :: sphere
    type(t_CutSphereXYZ) :: cut
    type(t_CylinderXYZ) :: cylinder
    type(t_Triangle) :: triangle
    type(sphere_normal) :: sphere_normal_shape
    type(sphere_pnormal) :: sphere_pnormal_shape
    type(cut_normal) :: cut_normal_shape
    type(cut_pnormal) :: cut_pnormal_shape
    type(cylinder_normal) :: cylinder_normal_shape
    type(cylinder_pnormal) :: cylinder_pnormal_shape
    type(triangle_normal) :: triangle_normal_shape
    type(triangle_pnormal) :: triangle_pnormal_shape
    double precision :: p1(3), p2(3), shift(3), scale, vertex(3, 3), normal(3)
    integer :: i, j, axis, checks = 0

    do i = 1, 1200
        shift = [0.01d0*mod(i, 11), -0.03d0*mod(i, 13), 0.07d0*mod(i, 7)]
        scale = 0.5d0 + 0.03d0*mod(i, 17)
        axis = mod(i, 3) + 1
        sphere = new_Sphere(shift, scale)
        cut = new_CutSphereZ(shift, scale, shift(axis), shift(axis) + scale)
        cut%axis = axis
        cylinder = new_CylinderXYZ(axis, shift, scale, 2d0*scale)
        vertex = reshape([-2d0, -1d0, 0d0, 2d0, -1d0, 0d0, 0d0, 2d0, 0d0], [3, 3])
        vertex = cshift(vertex, axis, dim=1)*scale
        do j = 1, 3
            vertex(:, j) = vertex(:, j) + shift
        end do
        triangle = new_Triangle(vertex)
        ! Direct edits to public geometry, normals, and metadata must be visible.
        sphere%origin = sphere%origin + 0.02d0
        sphere%radius = scale + 0.01d0
        cylinder%height = scale*1.9d0
        triangle%vertex(:, 2) = triangle%vertex(:, 2) + [0.01d0, -0.02d0, 0.03d0]
        triangle%n = triangle%n*(1d0 + 0.01d0*mod(i, 5))
        sphere%priority = i
        cut%priority = i + 1
        cylinder%priority = i + 2
        triangle%priority = i + 3
        sphere%material%tag = -i
        cut%material%tag = -i - 1
        cylinder%material%tag = -i - 2
        triangle%material%tag = -i - 3
        p1 = shift + scale*[dble(mod(i*37, 101))/25d0 - 2d0, &
                           dble(mod(i*71, 103))/26d0 - 2d0, -3d0]
        p2 = shift - 0.7d0*(p1 - shift)
        select case (mod(i, 12))
        case (0)
            p2 = p1                         ! zero motion
        case (1)
            p1 = shift                      ! interior start
        case (2)
            p2 = p1 + [1d0, 0d0, 0d0]      ! parallel to some shapes
        case (3)
            p2 = p1 + (p2 - p1)*0.05d0     ! ray hit beyond the segment
        case (4)
            p2 = p1 - (p2 - p1)             ! directed away
        case (5)
            p1 = vertex(:, 1)               ! vertex/start contact
            p2 = p1 + [0.5d0, -0.3d0, 1d0]
        end select
        call normals(sphere, p1, p2, .false.)
        call normals(cut, p1, p2, .false.)
        call normals(cylinder, p1, p2, .false.)
        call normals(triangle, p1, p2, .false.)
        call triangle_queries(triangle, p1, p2)
        normal = normalized(p1 - sphere%origin)
        if (dot(normal, p2 - p1) < 0d0) normal = -normal
        call check(all(sphere%normal(p1, p2) == normal), 'public sphere normal')
        sphere_normal_shape%t_Sphere = sphere
        call normals(sphere_normal_shape, p1, p2, .true.)
        sphere_pnormal_shape%t_Sphere = sphere
        call normals(sphere_pnormal_shape, p1, p2, .false.)
        cut_normal_shape%t_CutSphereXYZ = cut
        call normals(cut_normal_shape, p1, p2, .true.)
        cut_pnormal_shape%t_CutSphereXYZ = cut
        call normals(cut_pnormal_shape, p1, p2, .false.)
        cylinder_normal_shape%t_CylinderXYZ = cylinder
        call normals(cylinder_normal_shape, p1, p2, .true.)
        cylinder_pnormal_shape%t_CylinderXYZ = cylinder
        call normals(cylinder_pnormal_shape, p1, p2, .false.)
        triangle_normal_shape%t_Triangle = triangle
        call normals(triangle_normal_shape, p1, p2, .true.)
        triangle_pnormal_shape%t_Triangle = triangle
        call normals(triangle_pnormal_shape, p1, p2, .false.)
    end do
    ! Tangency, both roots, endpoints, and normal orientation at t = 0.
    sphere = new_Sphere([0d0, 0d0, 0d0], 1d0)
    cut = new_CutSphereZ([0d0, 0d0, 0d0], 1d0, 0d0, 1d0)
    cylinder = new_CylinderY([0d0, -1d0, 0d0], 1d0, 2d0)
    do i = -1, 1
        p1 = [dble(i), 0d0, -2d0]
        p2 = [dble(i), 0d0, 2d0]
        call normals(sphere, p1, p2, .false.)
        call normals(cut, p1, p2, .false.)
        call normals(cylinder, p1, p2, .false.)
    end do
    p1 = [1d0, 0d0, 0d0]
    p2 = [2d0, 0d0, 0d0]
    call normals(sphere, p1, p2, .false.)
    call normals(cut, p1, p2, .false.)
    call normals(cylinder, p1, p2, .false.)
    triangle = new_Triangle(reshape([0d0, 0d0, 0d0, 1d0, 0d0, 0d0, 0d0, 1d0, 0d0], [3, 3]))
    do i = -1, 1
        p1 = [0.5d0, 0.5d0 + dble(i)*epsilon(1d0), -1d0]
        p2 = p1 + [0d0, 0d0, 1d0]
        call triangle_queries(triangle, p1, p2)
        p1 = [0.2d0, 0.2d0, -5d-11]
        p2 = p1 + [0d0, 0d0, 1d-10*(1d0 + dble(i)*epsilon(1d0))]
        call triangle_queries(triangle, p1, p2)
    end do
    call normal_ranges
    print *, 'Scalar compatibility:', checks, 'checks passed'

contains

    subroutine normal_ranges
        double precision, parameter :: scales(9) = &
            [0d0, tiny(1d0), 1d-200, 1d-70, 1d0, 1d70, 1d200, huge(1d0)/16d0, -1d200]
        double precision :: position(3), expected(3), start(3), finish(3), radius
        type(t_CylinderXYZ) :: tube
        type(t_Sphere) :: ball
        integer :: k, a, b, c
        do a = 1, 3
            b = mod(a, 3) + 1
            c = mod(a + 1, 3) + 1
            tube = new_CylinderXYZ(a, [0d0, 0d0, 0d0], 1d0, 1d0)
            do k = 1, size(scales)
                position = [scales(k), 0.3d0*scales(k), -0.7d0*scales(k)]
                expected(a) = 0d0
                expected(b) = position(b)
                expected(c) = position(c)
                call normalize(expected)  ! original assumed-shape implementation
                call check(all(tube%pnormal(position) == expected), 'cylinder normal scaling')
                call check(all(tube%normal(position) == expected), 'unoriented cylinder normal')
            end do
            do k = -1, 1
                radius = 10d0**(70*k)
                ball = new_Sphere([0d0, 0d0, 0d0], radius)
                tube%radius = radius
                tube%height = 2d0*radius
                start = 0d0
                start(b) = -2d0*radius
                start(a) = radius
                start(c) = 0.3d0*radius
                finish = start
                finish(b) = 2d0*radius
                call normals(tube, start, finish, .false.)
                start(a) = 0.2d0*radius
                finish(a) = 0.2d0*radius
                call normals(ball, start, finish, .false.)
            end do
        end do
        ball%radius = 0d0
        tube%radius = 0d0
        call normals(ball, [-1d0, 0d0, 0d0], [1d0, 0d0, 0d0], .false.)
        call normals(tube, [-1d0, 0d0, 0d0], [1d0, 0d0, 0d0], .false.)
    end subroutine

    subroutine normals(shape, start, finish, overrides_normal)
        class(t_Boundary), intent(in) :: shape
        double precision, intent(in) :: start(3), finish(3)
        logical, intent(in) :: overrides_normal
        type(t_HitRecord) :: hit
        double precision :: expected(3)
        hit = shape%hit(new_Ray(start, finish - start))
        if (.not. hit%is_hit) then
            call check(hit%t == 0d0 .and. all(hit%position == 0d0) .and. all(hit%n == 0d0), 'miss fields')
            return
        end if
        if (overrides_normal) then
            expected = [2d0, 3d0, 4d0] + hit%position + start
        else
            expected = shape%pnormal(hit%position)
            if (dot(expected, start - hit%position) < 0d0) expected = -expected
        end if
        call check(all(hit%n == expected), 'hit normal / extension dispatch')
        call check(hit%priority == shape%priority, 'priority')
        call check(hit%material%tag == shape%material%tag, 'material')
    end subroutine

    subroutine triangle_queries(shape, start, finish)
        class(t_Triangle), intent(in) :: shape
        double precision, intent(in) :: start(3), finish(3)
        type(t_CollisionRecord) :: collision
        type(t_HitRecord) :: hit
        double precision :: t, position(3)
        logical :: found, segment_found
        call reference_triangle(shape%vertex, start, finish - start, found, t)
        segment_found = found .and. t <= 1d0
        collision = shape%check_collision(start, finish)
        hit = shape%hit(new_Ray(start, finish - start))
        call check(collision%is_collided .eqv. segment_found, 'triangle segment decision')
        call check(hit%is_hit .eqv. found, 'triangle ray decision')
        if (found) then
            position = start + (finish - start)*t
            call check(hit%t == t .and. all(hit%position == position), 'triangle ray coordinates')
            call check(hit%priority == shape%priority .and. hit%material%tag == shape%material%tag, 'ray metadata')
            if (segment_found) then
                call check(collision%t == t .and. all(collision%position == position), 'triangle segment coordinates')
                call check(collision%priority == shape%priority, 'segment priority')
                call check(collision%material%tag == shape%material%tag, 'segment material')
            end if
        end if
        if (.not. segment_found) then
            call check(collision%t == -1d0 .and. all(collision%position == 0d0), 'segment miss fields')
        end if
    end subroutine

    subroutine reference_triangle(vertices, origin, direction, found, t)
        ! v0.7.0 evaluation, including array-expression arguments and the
        ! original arithmetic order and edge/parallel thresholds.
        double precision, intent(in) :: vertices(3, 3), origin(3), direction(3)
        logical, intent(out) :: found
        double precision, intent(out) :: t
        double precision :: a(3), b(3), d(3), denominator, u, v
        found = .false.
        t = -1d0
        a = vertices(:, 2) - vertices(:, 1)
        b = vertices(:, 3) - vertices(:, 1)
        denominator = determinant(a, b, -direction)
        if (abs(denominator) <= 1d-10) return
        d = origin - vertices(:, 1)
        u = determinant(d, b, -direction)/denominator
        if (u < 0d0 .or. u > 1d0) return
        v = determinant(a, d, -direction)/denominator
        if (v < 0d0 .or. u + v > 1d0) return
        t = determinant(a, b, d)/denominator
        if (t < 0d0) return
        found = .true.
    end subroutine

    pure function determinant(a, b, c) result(d)
        double precision, intent(in) :: a(3), b(3), c(3)
        double precision :: d
        d = a(1)*b(2)*c(3) + a(2)*b(3)*c(1) + a(3)*b(1)*c(2) &
          - a(1)*b(3)*c(2) - a(2)*b(1)*c(3) - a(3)*b(2)*c(1)
    end function

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label
        checks = checks + 1
        if (.not. condition) then
            print *, 'Scalar mismatch: ', label, ' case/check =', i, checks
            error stop 1
        end if
    end subroutine
end program
