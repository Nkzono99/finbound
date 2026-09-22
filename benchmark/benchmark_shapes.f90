program benchmark_shapes
    use finbound
    use, intrinsic :: iso_fortran_env, only: compiler_version
    implicit none
    integer, parameter :: samples = 4096, loops = 256
    type(t_Sphere) :: sphere
    type(t_CutSphereXYZ) :: cut_sphere
    type(t_CylinderXYZ) :: cylinder
    type(t_Triangle) :: triangle
    type(t_Rectangle) :: rectangle
    double precision :: p1(3, samples), p2(3, samples), width, offset
    type(t_Ray) :: rays(samples)
    integer :: i, scene
    character(6), parameter :: scenes(3) = ['hits  ', 'mixed ', 'misses']

    print *, trim(compiler_version())
    print *, 'shape scene query median_seconds checksum hits_per_measurement'
    sphere = new_Sphere([0d0, 0d0, 0d0], 1d0)
    cut_sphere = new_CutSphereZ([0d0, 0d0, 0d0], 1d0, 0d0, 1d0)
    cylinder = new_CylinderY([0d0, -1d0, 0d0], 1d0, 2d0)
    triangle = new_Triangle(reshape([-2d0, -1d0, 0d0, 2d0, -1d0, 0d0, 0d0, 2d0, 0d0], [3, 3]))
    rectangle = new_Rectangle(reshape([-1d0, -1d0, 0d0, 1d0, -1d0, 0d0, &
                                       1d0, 1d0, 0d0, -1d0, 1d0, 0d0], [3, 4]))
    do scene = 1, 3
        width = 0.8d0
        offset = 0d0
        if (scene == 2) width = 3d0
        if (scene == 3) offset = 4d0
        do i = 1, samples
            p1(:, i) = [width*(dble(mod(i*37, 4093))/4093d0 - 0.5d0) + offset, &
                        width*(dble(mod(i*71, 4091))/4091d0 - 0.5d0), -3d0]
            p2(:, i) = p1(:, i) + [0.03d0, -0.01d0, 6d0]
            rays(i) = new_Ray(p1(:, i), p2(:, i) - p1(:, i))
        end do
        call measure(sphere, 'sphere', scenes(scene))
        call measure(cut_sphere, 'cut_sphere', scenes(scene))
        call measure(cylinder, 'cylinder', scenes(scene))
        call measure(triangle, 'triangle', scenes(scene))
        call measure(rectangle, 'rectangle', scenes(scene))
    end do

contains

    subroutine measure(shape, name, scene_name)
        class(t_Boundary), intent(in) :: shape
        character(*), intent(in) :: name, scene_name
        type(t_CollisionRecord) :: collision
        type(t_HitRecord) :: hit
        double precision :: started, finished, checksum, times(3), elapsed
        integer :: mode, repeat, j, k, hits, repetitions
        character(7), parameter :: modes(2) = ['segment', 'ray    ']
        do mode = 1, 2
            do repeat = 0, 3
                ! A full input traversal warms up each path before the timed runs.
                repetitions = loops
                if (repeat == 0) repetitions = 1
                checksum = 0d0
                hits = 0
                call cpu_time(started)
                if (mode == 1) then
                    do j = 1, repetitions
                        do k = 1, samples
                            collision = shape%check_collision(p1(:, k), p2(:, k))
                            if (.not. collision%is_collided) cycle
                            checksum = checksum + collision%t + sum(collision%position)
                            hits = hits + 1
                        end do
                    end do
                else
                    do j = 1, repetitions
                        do k = 1, samples
                            hit = shape%hit(rays(k))
                            if (.not. hit%is_hit) cycle
                            checksum = checksum + hit%t + sum(hit%position) + sum(hit%n)
                            hits = hits + 1
                        end do
                    end do
                end if
                call cpu_time(finished)
                if (repeat > 0) times(repeat) = finished - started
            end do
            elapsed = max(min(times(1), times(2)), min(max(times(1), times(2)), times(3)))
            print '(a12,1x,a6,1x,a7,1x,f12.6,1x,es24.16,1x,i10)', &
                name, scene_name, modes(mode), elapsed, checksum, hits
        end do
    end subroutine
end program
