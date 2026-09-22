program benchmark_rotation
    use finbound
    use, intrinsic :: iso_fortran_env, only: compiler_version
    implicit none
    integer, parameter :: samples = 4096, loops = 128
    type(t_Sphere), target :: sphere
    class(t_Boundary), pointer :: p
    type(t_BoundaryRotationXYZ) :: rotation
    type(t_CollisionRecord) :: record
    type(t_HitRecord) :: hit
    double precision :: p1(3, samples), p2(3, samples), box(2, 3), extent(2, 3)
    double precision :: started, finished, checksum, times(3), elapsed
    integer :: i, j, mode, repeat
    character(8) :: label
    print *, trim(compiler_version())
    print *, 'query median_seconds checksum'
    sphere = new_Sphere([1d0, 2d0, 3d0], 1d0)
    p => sphere
    rotation = new_BoundaryRotationY(p, 0.7d0, [4d0, 5d0, 6d0])
    do i = 1, samples
        p1(:, i) = [3d0 + mod(i, 37)*0.03d0, 6d0 + mod(i, 71)*0.03d0, 4d0]
        p2(:, i) = p1(:, i) + [0d0, 0d0, 10d0]
    end do
    extent = 0d0
    do mode = 1, 3
        do repeat = 1, 3
            checksum = 0d0
            call cpu_time(started)
            do j = 1, loops
                do i = 1, samples
                    select case (mode)
                    case (1)
                        record = rotation%check_collision(p1(:, i), p2(:, i))
                        if (record%is_collided) checksum = checksum + record%t
                    case (2)
                        hit = rotation%hit(new_Ray(p1(:, i), p2(:, i) - p1(:, i)))
                        if (hit%is_hit) checksum = checksum + hit%t + hit%n(3)
                    case (3)
                        box(1, :) = p1(:, i) + [0d0, 0d0, 4d0]
                        box(2, :) = box(1, :) + 2d0
                        if (rotation%is_overlap(box, extent)) checksum = checksum + 1d0
                    end select
                end do
            end do
            call cpu_time(finished)
            times(repeat) = finished - started
        end do
        elapsed = max(min(times(1), times(2)), min(max(times(1), times(2)), times(3)))
        select case (mode)
        case (1)
            label = 'segment'
        case (2)
            label = 'ray'
        case (3)
            label = 'overlap'
        end select
        print '(a8,1x,f12.6,1x,es24.16)', label, elapsed, checksum
    end do
end program
