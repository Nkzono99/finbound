program test_rotation_compatibility
    use finbound
    use m_vector, only: rot3d_x, rot3d_y, rot3d_z
    implicit none
    type(t_Sphere), target :: sphere
    class(t_Boundary), pointer :: p
    type(t_BoundaryRotationXYZ) :: rotation
    type(t_CollisionRecord) :: actual, expected
    type(t_HitRecord) :: hit, expected_hit
    type(t_Ray) :: ray, local_ray
    double precision :: p1(3), p2(3), q1(3), q2(3), box(2, 3), expanded(2, 3), bounds(2, 3)
    double precision :: extent(2, 3), corner(3), angle, zero_extent(2, 3)
    integer :: axis, i, ix, iy, iz, checks
    sphere = new_Sphere([1d0, 2d0, 3d0], 1d0)
    p => sphere
    rotation = new_BoundaryRotationX(p, 0d0)
    checks = 0
    zero_extent = 0d0
    extent(1, :) = [0.1d0, 0.2d0, 0.3d0]
    extent(2, :) = [0.3d0, 0.1d0, 0.2d0]
    do axis = 1, 3
        do i = 1, 300
            ! Direct edits to the existing public fields must take effect immediately.
            angle = 0.03d0*i - 4d0
            rotation%axis = axis
            rotation%rotation_rad = angle
            rotation%origin = [0.01d0*i, -0.02d0*i, 0.03d0*i]
            q1 = [1d0 + 0.01d0*mod(i, 130), 2d0, 0d0]
            q2 = q1 + [0d0, 0d0, 6d0]
            p1 = rotate(q1, axis, angle) + rotation%origin
            p2 = rotate(q2, axis, angle) + rotation%origin
            q1 = rotate(p1 - rotation%origin, axis, -angle)
            q2 = rotate(p2 - rotation%origin, axis, -angle)
            expected = sphere%check_collision(q1, q2)
            if (expected%is_collided) expected%position = rotate(expected%position, axis, angle) + rotation%origin
            actual = rotation%check_collision(p1, p2)
            call check(actual%is_collided .eqv. expected%is_collided)
            call check(actual%t == expected%t)
            call check(all(actual%position == expected%position))
            ray = new_Ray(p1, p2 - p1)
            local_ray = new_Ray(q1, rotate(ray%direction, axis, -angle))
            expected_hit = sphere%hit(local_ray)
            if (expected_hit%is_hit) then
                expected_hit%position = rotate(expected_hit%position, axis, angle) + rotation%origin
                expected_hit%n = rotate(expected_hit%n, axis, angle)
            end if
            hit = rotation%hit(ray)
            call check(hit%is_hit .eqv. expected_hit%is_hit)
            call check(hit%t == expected_hit%t)
            call check(all(hit%position == expected_hit%position))
            call check(all(hit%n == expected_hit%n))
            if (hit%is_hit) then
                corner = rotate(hit%position - rotation%origin, axis, -angle)
                corner = sphere%pnormal(corner)
                call check(all(rotation%pnormal(hit%position) == rotate(corner, axis, angle)))
            end if
            box(1, :) = min(p1, p2)
            box(2, :) = max(p1, p2)
            expanded(1, :) = box(1, :) - extent(1, :)
            expanded(2, :) = box(2, :) + extent(2, :)
            bounds(1, :) = huge(1d0)
            bounds(2, :) = -huge(1d0)
            do ix = 1, 2
                do iy = 1, 2
                    do iz = 1, 2
                        corner = rotate([expanded(ix, 1), expanded(iy, 2), expanded(iz, 3)] - rotation%origin, axis, -angle)
                        bounds(1, :) = min(bounds(1, :), corner)
                        bounds(2, :) = max(bounds(2, :), corner)
                    end do
                end do
            end do
            call check(rotation%is_overlap(box, extent) .eqv. sphere%is_overlap(bounds, zero_extent))
        end do
    end do
    print *, 'Rotation compatibility:', checks, 'checks passed'

contains

    function rotate(vector, axis, angle) result(rotated)
        double precision, intent(in) :: vector(3), angle
        integer, intent(in) :: axis
        double precision :: rotated(3)
        select case (axis)
        case (1)
            rotated = rot3d_x(vector, angle)
        case (2)
            rotated = rot3d_y(vector, angle)
        case (3)
            rotated = rot3d_z(vector, angle)
        end select
    end function

    subroutine check(condition)
        logical, intent(in) :: condition
        checks = checks + 1
        if (.not. condition) then
            print *, 'Rotation mismatch: axis, case, check =', axis, i, checks
            error stop 1
        end if
    end subroutine
end program
