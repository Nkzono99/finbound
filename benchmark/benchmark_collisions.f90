program benchmark_collisions
    use finbound
    use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
    implicit none
    integer, parameter :: queries = 10000, repeats = 3
    integer :: nseed, i, case_id, mode, repeat, n, width, ix, iy
    integer, allocatable :: seed(:)
    double precision :: p1(3, queries), p2(3, queries), u(3, queries)
    double precision :: old_times(repeats), new_times(repeats), old_sum, new_sum
    double precision :: started, finished, build_time, old_time, new_time, origin(3)
    integer, parameter :: counts(4) = [16, 256, 4096, 256]
    type(t_BoundaryList) :: list
    type(t_BoundaryBVH) :: index
    class(t_Boundary), pointer :: shape
    character(8) :: layout, query_kind

    print *, trim(compiler_version())
    print *, trim(compiler_options())
    print *, 'queries per measurement:', queries, '; median of', repeats
    print *, 'boundaries layout query build_s linear_s bvh_s speedup checksum'
    call random_seed(size=nseed)
    allocate (seed(nseed))
    seed = [(3571 + 7919*i, i=1,nseed)]
    call random_seed(put=seed)
    call random_number(u)
    do case_id = 1, size(counts)
        n = counts(case_id)
        width = nint(sqrt(dble(n)))
        layout = 'grid'
        if (case_id == 4) layout = 'overlap'
        list = new_BoundaryList(n)
        do i = 1, n
            ix = mod(i - 1, width)
            iy = (i - 1)/width
            origin = [2d0*ix, 2d0*iy, 0d0]
            if (case_id == 4) origin = 0d0
            allocate (t_Sphere :: shape)
            select type (shape)
            type is (t_Sphere)
                shape = new_Sphere(origin, 0.5d0)
            end select
            shape%material%tag = i
            call list%add_boundary(shape)
        end do
        do i = 1, queries
            ix = mod(i - 1, width)
            iy = mod((i - 1)/width, width)
            p1(:, i) = [2d0*ix + 2d0*u(1, i) - 1d0, 2d0*iy + 2d0*u(2, i) - 1d0, -2d0]
            if (case_id == 4) p1(:, i) = [0.2d0*u(1, i) - 0.1d0, 0.2d0*u(2, i) - 0.1d0, -2d0]
            p2(:, i) = p1(:, i) + [0d0, 0d0, 4d0]
        end do
        call cpu_time(started)
        index = new_BoundaryBVH(list)
        call cpu_time(finished)
        build_time = finished - started
        do mode = 1, 2
            query_kind = 'segment'
            if (mode == 2) query_kind = 'ray'
            ! Warm both paths before alternating their measurement order.
            call measure(list, mode, old_time, old_sum)
            call measure(index, mode, new_time, new_sum)
            call verify(old_sum, new_sum)
            do repeat = 1, repeats
                if (mod(repeat, 2) == 1) then
                    call measure(list, mode, old_times(repeat), old_sum)
                    call measure(index, mode, new_times(repeat), new_sum)
                else
                    call measure(index, mode, new_times(repeat), new_sum)
                    call measure(list, mode, old_times(repeat), old_sum)
                end if
                call verify(old_sum, new_sum)
            end do
            old_time = median(old_times)
            new_time = median(new_times)
            print '(i6,1x,a8,1x,a8,3(1x,f12.6),1x,f9.2,1x,es20.12)', &
                n, layout, query_kind, build_time, old_time, new_time, old_time/new_time, new_sum
        end do
        call index%destroy()
        call list%destroy()
    end do

contains

    subroutine measure(boundary, mode, elapsed, checksum)
        class(t_Boundary), intent(in) :: boundary
        integer, intent(in) :: mode
        double precision, intent(out) :: elapsed, checksum
        double precision :: before, after
        type(t_CollisionRecord) :: record
        type(t_HitRecord) :: hit
        integer :: j
        checksum = 0d0
        call cpu_time(before)
        if (mode == 1) then
            do j = 1, queries
                record = boundary%check_collision(p1(:, j), p2(:, j))
                if (record%is_collided) checksum = checksum + record%t + 0.001d0*record%material%tag
            end do
        else
            do j = 1, queries
                hit = boundary%hit(new_Ray(p1(:, j), p2(:, j) - p1(:, j)))
                if (hit%is_hit) checksum = checksum + hit%t + 0.001d0*hit%material%tag
            end do
        end if
        call cpu_time(after)
        elapsed = after - before
    end subroutine

    subroutine verify(a, b)
        double precision, intent(in) :: a, b
        if (a /= b) error stop 'Benchmark result mismatch'
    end subroutine

    function median(values) result(value)
        double precision, intent(in) :: values(3)
        double precision :: value
        value = max(min(values(1), values(2)), min(max(values(1), values(2)), values(3)))
    end function
end program
