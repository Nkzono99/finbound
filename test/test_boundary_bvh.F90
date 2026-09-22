module m_bvh_test_extension
    use finbound
    implicit none
    ! User-defined extensions must use the fallback, even if their parent is bounded.
    type, extends(t_Sphere) :: t_CustomSphere
    end type
end module

program test_boundary_bvh
    use finbound
    use m_bvh_test_extension
    implicit none
    integer :: checks = 0

    call test_empty
    call test_mixed_scene
    call test_ties_and_updates
    call test_parallel_and_tangent
    print *, 'BVH equivalence:', checks, 'checks passed'

contains

    subroutine check(condition, message)
        logical, intent(in) :: condition
        character(*), intent(in) :: message
        checks = checks + 1
        if (.not. condition) then
            print *, 'FAIL: ', message
            error stop 1
        end if
    end subroutine

    subroutine compare(list, index, p1, p2)
        type(t_BoundaryList), intent(in) :: list
        type(t_BoundaryBVH), intent(in) :: index
        double precision, intent(in) :: p1(3), p2(3)
        type(t_CollisionRecord) :: a, b
        type(t_HitRecord) :: x, y
        type(t_Ray) :: ray
        a = list%check_collision(p1, p2)
        b = index%check_collision(p1, p2)
        call check(a%is_collided .eqv. b%is_collided, 'segment flag')
        call check(a%t == b%t, 'segment parameter')
        call check(all(a%position == b%position), 'segment position')
        call check(a%priority == b%priority, 'segment priority')
        call check(a%material%tag == b%material%tag, 'segment material')
        ray = new_Ray(p1, p2 - p1)
        x = list%hit(ray)
        y = index%hit(ray)
        call check(x%is_hit .eqv. y%is_hit, 'ray flag')
        call check(x%t == y%t, 'ray parameter')
        call check(all(x%position == y%position), 'ray position')
        call check(all(x%n == y%n), 'ray normal')
        call check(x%priority == y%priority, 'ray priority')
        call check(x%material%tag == y%material%tag, 'ray material')
    end subroutine

    subroutine append(list, shape)
        type(t_BoundaryList), intent(inout) :: list
        class(t_Boundary), intent(in) :: shape
        class(t_Boundary), pointer :: p
        allocate (p, source=shape)
        p%priority = mod(list%nboundaries, 3)
        p%material%tag = list%nboundaries + 1
        call list%add_boundary(p)
    end subroutine

    subroutine test_empty
        type(t_BoundaryList) :: list
        type(t_BoundaryBVH) :: index
        list = new_BoundaryList(0)
        call compare(list, index, [0d0, 0d0, 0d0], [1d0, 0d0, 0d0])
        index = new_BoundaryBVH(list)
        call compare(list, index, [0d0, 0d0, 0d0], [1d0, 0d0, 0d0])
        call index%destroy()
        call index%destroy()
        call index%rebuild()
        call list%destroy()
    end subroutine

    subroutine test_mixed_scene
        type(t_BoundaryList) :: list
        type(t_BoundaryBVH) :: index, copy
        type(t_Sphere), target :: rotated_shapes(3)
        type(t_CustomSphere) :: custom
        type(t_Rectangle) :: rectangle
        class(t_Boundary), pointer :: p, nested
        double precision :: origin(3), vertices(3, 4), p1(3), p2(3), box(2, 3), extent(2, 3)
        double precision :: u(6)
        integer :: axis, i, nseed
        integer, allocatable :: seed(:)
        list = new_BoundaryList()
        do axis = 1, 3
            origin = [4d0*axis, 0d0, 0d0]
            call append(list, new_Sphere(origin, 0.8d0))
            call append(list, new_CutSphereX(origin + [0d0, 3d0, 0d0], 0.8d0, origin(1) - 0.2d0, origin(1) + 0.3d0))
            call append(list, new_cylinderXYZ(axis, origin + [0d0, 6d0, 0d0], 0.8d0, 1.5d0))
            call append(list, new_CircleXYZ(axis, origin + [0d0, 9d0, 0d0], 0.8d0))
            call append(list, new_DonutXYZ(axis, origin + [0d0, 12d0, 0d0], 0.8d0, 0.3d0))
            call append(list, new_rectangleXYZ(axis, origin + [0d0, 15d0, 0d0], 1.5d0, 1d0))
            call append(list, new_ellipsoidXYZ(axis, origin + [0d0, 18d0, 0d0], 0.3d0, 0.8d0, 1.5d0))
            call append(list, new_hyperboloidXYZ(axis, origin + [0d0, 21d0, 0d0], 0.8d0, 0.3d0, 1.5d0))
            rotated_shapes(axis) = new_Sphere([1d0, 2d0, 0d0], 0.8d0)
            p => rotated_shapes(axis)
            call append(list, new_BoundaryRotationXYZ(p, axis, 0.7d0, origin + [0d0, 24d0, 0d0]))
            vertices(:, 1) = origin + [0d0, 27d0, 0d0]
            vertices(:, 2) = origin + [1d0, 27d0, 0.2d0]
            vertices(:, 3) = origin + [1d0, 28d0, 0.2d0]
            vertices(:, 4) = origin + [0d0, 28d0, 0d0]
            call append(list, new_Triangle(vertices(:, 1:3)))
            rectangle = new_Rectangle(vertices + 2d0)
            ! Rectangle collision follows its triangles, not its cached vertex array.
            rectangle%triangles(1)%vertex = rectangle%triangles(1)%vertex + 1d0
            call append(list, rectangle)
        end do
        custom%origin = [2d0, 3d0, 4d0]
        custom%radius = 1d0
        call append(list, custom)
        allocate (t_BoundaryList :: nested)
        select type (nested)
        type is (t_BoundaryList)
            nested = new_BoundaryList()
            call append(nested, new_Sphere([20d0, 20d0, 0d0], 1d0))
        end select
        call list%add_boundary(nested)
        index = new_BoundaryBVH(list)
        call random_seed(size=nseed)
        allocate (seed(nseed))
        seed = [(7919 + i*104729, i=1,nseed)]
        call random_seed(put=seed)
        do i = 1, 12000
            call random_number(u)
            p1 = [-2d0, -2d0, -3d0] + [24d0, 37d0, 6d0]*u(1:3)
            p2 = [-2d0, -2d0, -3d0] + [24d0, 37d0, 6d0]*u(4:6)
            call compare(list, index, p1, p2)
        end do
        ! Infinite boundaries are queried alongside the finite tree.
        call append(list, new_planeZ(-2d0))
        call append(list, new_planeXYZWithCircleHoleX([1d0, 1d0, 1d0], 0.5d0))
        call index%build(list)
        copy = index
        call index%destroy()
        do i = 1, 2000
            call random_number(u)
            p1 = [-2d0, -2d0, -3d0] + [24d0, 37d0, 6d0]*u(1:3)
            p2 = [-2d0, -2d0, -3d0] + [24d0, 37d0, 6d0]*u(4:6)
            call compare(list, copy, p1, p2)
            box(1, :) = min(p1, p2)
            box(2, :) = max(p1, p2)
            extent = 0.1d0
            call check(list%is_overlap(box, extent) .eqv. copy%is_overlap(box, extent), 'overlap')
        end do
        call copy%destroy()
        call nested%destroy()
        call list%destroy()
    end subroutine

    subroutine test_ties_and_updates
        type(t_BoundaryList) :: list
        type(t_BoundaryBVH) :: index
        type(t_HitRecord) :: hit
        integer :: i
        list = new_BoundaryList()
        do i = 1, 20
            call append(list, new_Sphere([0d0, 0d0, 0d0], 1d0))
            list%boundaries(i)%ref%priority = 0
        end do
        index = new_BoundaryBVH(list)
        call compare(list, index, [-2d0, 0d0, 0d0], [2d0, 0d0, 0d0])
        hit = index%hit(new_Ray([-2d0, 0d0, 0d0], [1d0, 0d0, 0d0]))
        call check(hit%material%tag == 1, 'insertion order breaks exact ties')
        ! Priority/material edits are observed without rebuilding geometry.
        list%boundaries(17)%ref%priority = 10
        list%boundaries(17)%ref%material%tag = 900
        call compare(list, index, [-2d0, 0d0, 0d0], [2d0, 0d0, 0d0])
        hit = index%hit(new_Ray([-2d0, 0d0, 0d0], [1d0, 0d0, 0d0]))
        call check(hit%material%tag == 900, 'priority breaks exact ties')
        select type (s => list%boundaries(17)%ref)
        type is (t_Sphere)
            s%origin = [50d0, 0d0, 0d0]
        end select
        call index%rebuild()
        call compare(list, index, [48d0, 0d0, 0d0], [52d0, 0d0, 0d0])
        call append(list, new_Sphere([100d0, 0d0, 0d0], 1d0))
        call index%build(list)
        call compare(list, index, [98d0, 0d0, 0d0], [102d0, 0d0, 0d0])
        call index%destroy()
        call list%destroy()

        list = new_BoundaryList()
        call append(list, new_planeX(2d30))
        index = new_BoundaryBVH(list)
        call compare(list, index, [0d0, 0d0, 0d0], [1d0, 0d0, 0d0])
        call index%destroy()
        call list%destroy()

        list = new_BoundaryList()
        call append(list, new_planeX(dble(1e30)))
        list%boundaries(1)%ref%priority = -10
        index = new_BoundaryBVH(list)
        call compare(list, index, [0d0, 0d0, 0d0], [1d0, 0d0, 0d0])
        call index%destroy()
        call list%destroy()
    end subroutine

    subroutine test_parallel_and_tangent
        type(t_BoundaryList) :: list
        type(t_BoundaryBVH) :: index
        list = new_BoundaryList()
        call append(list, new_Sphere([0d0, 0d0, 0d0], 1d0))
        index = new_BoundaryBVH(list)
        call compare(list, index, [-2d0, 1d0, 0d0], [2d0, 1d0, 0d0])
        call compare(list, index, [0d0, 0d0, 0d0], [0d0, 0d0, 0d0])
        call compare(list, index, [2d0, 0d0, 0d0], [2d0, 0d0, 1d0])
        call compare(list, index, [0d0, 0d0, 0d0], [1d-300, 0d0, 0d0])
        call compare(list, index, [0d0, 0d0, 0d0], [2d0, 0d0, 0d0])
        call compare(list, index, [1d0, 0d0, 0d0], [2d0, 0d0, 0d0])
        call compare(list, index, [2d0, 0d0, 0d0], [1d0, 0d0, 0d0])
        call index%destroy()
        call list%destroy()
    end subroutine
end program
