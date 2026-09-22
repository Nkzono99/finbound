module m_boundary_bvh
    use m_boundary, only: t_Boundary, tp_Boundary, t_CollisionRecord
    use m_ray, only: t_Ray, t_HitRecord
    use m_boundary_list, only: t_BoundaryList
    use m_boundary_rotation, only: t_BoundaryRotationXYZ
    use m_sphere_boundary, only: t_Sphere, t_CutSphereXYZ
    use m_cylinder_boundary, only: t_CylinderXYZ
    use m_circle_boundary, only: t_CircleXYZ
    use m_donut_boundary, only: t_DonutXYZ
    use m_rectangleXYZ_boundary, only: t_RectangleXYZ
    use m_triangle_boundary, only: t_Triangle
    use m_rectangle_boundary, only: t_Rectangle
    use m_ellipsoid_boundary, only: t_EllipsoidXYZ
    use m_hyperboloid_boundary, only: t_HyperboloidXYZ
    use m_vector, only: rot3d_x, rot3d_y, rot3d_z
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    private
    public :: t_BoundaryBVH, new_BoundaryBVH

    integer, parameter :: LEAF_SIZE = 4

    type :: t_Node
        double precision :: bounds(2, 3)
        integer :: first = 0, count = 0, escape = 0
    end type

    ! Non-owning index of a fixed list of boundary references. Geometry must
    ! remain alive; call rebuild after editing it, or build after changing the list.
    type, extends(t_Boundary) :: t_BoundaryBVH
        private
        type(tp_Boundary), allocatable :: boundaries(:)
        type(t_Node), allocatable :: nodes(:)
        integer, allocatable :: order(:)
        integer :: nbounded = 0, nnodes = 0
    contains
        procedure :: build => bvh_build
        procedure :: rebuild => bvh_rebuild
        procedure :: destroy => bvh_destroy
        procedure :: check_collision => bvh_check_collision
        procedure :: hit => bvh_hit
        procedure :: is_overlap => bvh_is_overlap
        procedure :: pnormal => bvh_pnormal
    end type

contains

    function new_BoundaryBVH(list) result(obj)
        type(t_BoundaryList), intent(in) :: list
        type(t_BoundaryBVH) :: obj
        call obj%build(list)
    end function

    subroutine bvh_build(self, list)
        class(t_BoundaryBVH), intent(inout) :: self
        type(t_BoundaryList), intent(in) :: list
        integer :: i
        call self%destroy()
        allocate (self%boundaries(list%nboundaries))
        do i = 1, list%nboundaries
            self%boundaries(i)%ref => list%boundaries(i)%ref
        end do
        call self%rebuild()
    end subroutine

    subroutine bvh_rebuild(self)
        class(t_BoundaryBVH), intent(inout) :: self
        double precision, allocatable :: bounds(:, :, :)
        double precision :: padding(3)
        integer :: i, n, tail
        if (allocated(self%nodes)) deallocate (self%nodes)
        if (allocated(self%order)) deallocate (self%order)
        self%nbounded = 0
        self%nnodes = 0
        if (.not. allocated(self%boundaries)) return
        n = size(self%boundaries)
        allocate (bounds(2, 3, n), self%order(n))
        tail = n
        do i = 1, n
            if (boundary_bounds(self%boundaries(i)%ref, bounds(:, :, i))) then
                ! Outward padding prevents rounding at a face from dropping a hit.
                padding = 64d0*epsilon(1d0)*max(1d0, maxval(abs(bounds(:, :, i)), dim=1))
                bounds(1, :, i) = bounds(1, :, i) - padding
                bounds(2, :, i) = bounds(2, :, i) + padding
                self%nbounded = self%nbounded + 1
                self%order(self%nbounded) = i
            else
                self%order(tail) = i
                tail = tail - 1
            end if
        end do
        allocate (self%nodes(2*self%nbounded))
        if (self%nbounded > 0) call build_node(self, bounds, 1, self%nbounded)
    end subroutine

    recursive subroutine build_node(self, bounds, first, last)
        class(t_BoundaryBVH), intent(inout) :: self
        double precision, intent(in) :: bounds(:, :, :)
        integer, intent(in) :: first, last
        integer :: node, i, axis, middle, widest(1)
        double precision :: centers_min(3), centers_max(3), center(3)
        self%nnodes = self%nnodes + 1
        node = self%nnodes
        self%nodes(node)%bounds(1, :) = huge(1d0)
        self%nodes(node)%bounds(2, :) = -huge(1d0)
        centers_min = huge(1d0)
        centers_max = -huge(1d0)
        do i = first, last
            self%nodes(node)%bounds(1, :) = min(self%nodes(node)%bounds(1, :), bounds(1, :, self%order(i)))
            self%nodes(node)%bounds(2, :) = max(self%nodes(node)%bounds(2, :), bounds(2, :, self%order(i)))
            center = 0.5d0*bounds(1, :, self%order(i)) + 0.5d0*bounds(2, :, self%order(i))
            centers_min = min(centers_min, center)
            centers_max = max(centers_max, center)
        end do
        if (last - first + 1 <= LEAF_SIZE) then
            self%nodes(node)%first = first
            self%nodes(node)%count = last - first + 1
        else
            widest = maxloc(centers_max - centers_min)
            axis = widest(1)
            call sort_centers(self%order, bounds, axis, first, last)
            middle = first + (last - first)/2
            call build_node(self, bounds, first, middle)
            call build_node(self, bounds, middle + 1, last)
        end if
        ! Preorder layout allows allocation-free, stack-free query traversal.
        self%nodes(node)%escape = self%nnodes + 1
    end subroutine

    recursive subroutine sort_centers(order, bounds, axis, first, last)
        integer, intent(inout) :: order(:)
        double precision, intent(in) :: bounds(:, :, :)
        integer, intent(in) :: axis, first, last
        integer :: i, j, tmp, pivot_id
        double precision :: pivot
        i = first
        j = last
        pivot_id = order(first + (last - first)/2)
        pivot = 0.5d0*bounds(1, axis, pivot_id) + 0.5d0*bounds(2, axis, pivot_id)
        do
            do while (0.5d0*bounds(1, axis, order(i)) + 0.5d0*bounds(2, axis, order(i)) < pivot)
                i = i + 1
            end do
            do while (0.5d0*bounds(1, axis, order(j)) + 0.5d0*bounds(2, axis, order(j)) > pivot)
                j = j - 1
            end do
            if (i > j) exit
            tmp = order(i)
            order(i) = order(j)
            order(j) = tmp
            i = i + 1
            j = j - 1
            if (i > j) exit
        end do
        if (first < j) call sort_centers(order, bounds, axis, first, j)
        if (i < last) call sort_centers(order, bounds, axis, i, last)
    end subroutine

    pure recursive function bvh_check_collision(self, p1, p2) result(record)
        class(t_BoundaryBVH), intent(in) :: self
        double precision, intent(in) :: p1(3), p2(3)
        type(t_CollisionRecord) :: record, candidate
        double precision :: direction(3), limit
        integer :: node, j, i, best_id
        record%t = 100d0
        if (.not. allocated(self%boundaries)) return
        direction = p2 - p1
        best_id = huge(0)
        limit = 1d0
        ! Infinite planes and unknown user-defined types are always checked.
        do j = self%nbounded + 1, size(self%order)
            i = self%order(j)
            candidate = self%boundaries(i)%ref%check_collision(p1, p2)
            candidate%priority = self%boundaries(i)%ref%priority
            if (.not. candidate%is_collided) cycle
            if (preferred(candidate%t, candidate%priority, i, record%t, record%priority, best_id)) then
                record = candidate
                best_id = i
                limit = min(1d0, record%t)
            end if
        end do
        node = 1
        do while (node <= self%nnodes)
            if (.not. intersects_box(self%nodes(node)%bounds, p1, direction, limit)) then
                node = self%nodes(node)%escape
                cycle
            end if
            do j = self%nodes(node)%first, self%nodes(node)%first + self%nodes(node)%count - 1
                i = self%order(j)
                candidate = self%boundaries(i)%ref%check_collision(p1, p2)
                candidate%priority = self%boundaries(i)%ref%priority
                if (.not. candidate%is_collided) cycle
                if (preferred(candidate%t, candidate%priority, i, record%t, record%priority, best_id)) then
                    record = candidate
                    best_id = i
                    limit = min(1d0, record%t)
                end if
            end do
            node = node + 1
        end do
    end function

    pure recursive function bvh_hit(self, ray) result(hit_record)
        class(t_BoundaryBVH), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record, candidate
        double precision :: limit
        integer :: node, j, i, best_id
        if (.not. allocated(self%boundaries)) return
        if (size(self%boundaries) == 0) return
        hit_record%t = 1e30
        best_id = huge(0)
        limit = huge(1d0)
        do j = self%nbounded + 1, size(self%order)
            i = self%order(j)
            candidate = self%boundaries(i)%ref%hit(ray)
            candidate%priority = self%boundaries(i)%ref%priority
            if (.not. candidate%is_hit) cycle
            if (preferred(candidate%t, candidate%priority, i, hit_record%t, hit_record%priority, best_id)) then
                hit_record = candidate
                best_id = i
                limit = hit_record%t
            end if
        end do
        node = 1
        do while (node <= self%nnodes)
            if (.not. intersects_box(self%nodes(node)%bounds, ray%origin, ray%direction, limit)) then
                node = self%nodes(node)%escape
                cycle
            end if
            do j = self%nodes(node)%first, self%nodes(node)%first + self%nodes(node)%count - 1
                i = self%order(j)
                candidate = self%boundaries(i)%ref%hit(ray)
                candidate%priority = self%boundaries(i)%ref%priority
                if (.not. candidate%is_hit) cycle
                if (preferred(candidate%t, candidate%priority, i, hit_record%t, hit_record%priority, best_id)) then
                    hit_record = candidate
                    best_id = i
                    limit = hit_record%t
                end if
            end do
            node = node + 1
        end do
    end function

    pure logical function preferred(t, priority, id, best_t, best_priority, best_id)
        double precision, intent(in) :: t, best_t
        integer, intent(in) :: priority, id, best_priority, best_id
        if (best_id == huge(0)) then
            preferred = .true.
            return
        end if
        preferred = t < best_t
        if (t == best_t) preferred = priority > best_priority .or. (priority == best_priority .and. id < best_id)
    end function

    pure logical function intersects_box(bounds, origin, direction, limit) result(intersects)
        double precision, intent(in) :: bounds(2, 3), origin(3), direction(3), limit
        double precision :: near, far, a, b
        integer :: axis
        intersects = .false.
        near = 0d0
        far = limit
        do axis = 1, 3
            if (direction(axis) == 0d0) then
                if (origin(axis) < bounds(1, axis) .or. origin(axis) > bounds(2, axis)) return
            else
                a = bounded_ratio(bounds(1, axis) - origin(axis), direction(axis))
                b = bounded_ratio(bounds(2, axis) - origin(axis), direction(axis))
                near = max(near, min(a, b))
                far = min(far, max(a, b))
                if (near > far) return
            end if
        end do
        intersects = .true.
    end function

    pure function bounded_ratio(numerator, denominator) result(value)
        double precision, intent(in) :: numerator, denominator
        double precision :: value
        if (abs(denominator) < 1d0) then
            if (abs(numerator) > huge(1d0)*abs(denominator)) then
                value = sign(huge(1d0), numerator)*sign(1d0, denominator)
                return
            end if
        end if
        value = numerator/denominator
    end function

    pure recursive function bvh_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_BoundaryBVH), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap
        integer :: i
        is_overlap = .false.
        if (.not. allocated(self%boundaries)) return
        ! Preserve each shape's existing conservative overlap semantics.
        do i = 1, size(self%boundaries)
            if (self%boundaries(i)%ref%is_overlap(sdoms, extent)) then
                is_overlap = .true.
                return
            end if
        end do
    end function

    pure recursive function bvh_pnormal(self, position) result(pnormal)
        class(t_BoundaryBVH), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)
        ! As with BoundaryList, use the selected shape or hit_record%n instead.
        pnormal = 0d0
        if (.not. allocated(self%boundaries)) return
        if (size(self%boundaries) > 0) pnormal = self%boundaries(1)%pnormal(position)
    end function

    subroutine bvh_destroy(self)
        class(t_BoundaryBVH), intent(inout) :: self
        if (allocated(self%boundaries)) deallocate (self%boundaries)
        if (allocated(self%nodes)) deallocate (self%nodes)
        if (allocated(self%order)) deallocate (self%order)
        self%nbounded = 0
        self%nnodes = 0
    end subroutine

    recursive function boundary_bounds(boundary, bounds) result(bounded)
        class(t_Boundary), intent(in) :: boundary
        double precision, intent(out) :: bounds(2, 3)
        logical :: bounded
        double precision :: origin(3), radius(3), child(2, 3), corner(3), scale
        integer :: axis, axis1, axis2, i, ix, iy, iz
        bounded = .true.
        select type (b => boundary)
        type is (t_Sphere)
            origin = b%origin
            radius = abs(b%radius)
        type is (t_CutSphereXYZ)
            ! The entire sphere is conservative for any cut interval.
            origin = b%origin
            radius = abs(b%radius)
        type is (t_CylinderXYZ)
            axis = b%axis
            if (axis < 1 .or. axis > 3) then
                bounded = .false.
                return
            end if
            origin = b%origin
            origin(axis) = origin(axis) + 0.5d0*b%height
            radius = abs(b%radius)
            radius(axis) = 0.5d0*abs(b%height)
        type is (t_CircleXYZ)
            axis = b%axis
            if (axis < 1 .or. axis > 3) then
                bounded = .false.
                return
            end if
            origin = b%origin
            radius = abs(b%radius)
            radius(axis) = 0d0
        type is (t_DonutXYZ)
            axis = b%axis
            if (axis < 1 .or. axis > 3) then
                bounded = .false.
                return
            end if
            origin = b%origin
            radius = abs(b%radius)
            radius(axis) = 0d0
        type is (t_RectangleXYZ)
            axis = b%axis
            if (axis < 1 .or. axis > 3) then
                bounded = .false.
                return
            end if
            axis1 = mod(axis, 3) + 1
            axis2 = mod(axis + 1, 3) + 1
            origin = b%origin
            origin(axis1) = origin(axis1) + 0.5d0*b%w1
            origin(axis2) = origin(axis2) + 0.5d0*b%w2
            radius = 0d0
            radius(axis1) = 0.5d0*abs(b%w1)
            radius(axis2) = 0.5d0*abs(b%w2)
        type is (t_Triangle)
            bounds(1, :) = minval(b%vertex, dim=2)
            bounds(2, :) = maxval(b%vertex, dim=2)
            bounded = usable_bounds(bounds)
            return
        type is (t_Rectangle)
            ! Collision uses triangles; their public vertices may be edited.
            bounds(1, :) = min(minval(b%triangles(1)%vertex, dim=2), minval(b%triangles(2)%vertex, dim=2))
            bounds(2, :) = max(maxval(b%triangles(1)%vertex, dim=2), maxval(b%triangles(2)%vertex, dim=2))
            bounded = usable_bounds(bounds)
            return
        type is (t_EllipsoidXYZ)
            axis = b%axis
            if (axis < 1 .or. axis > 3) then
                bounded = .false.
                return
            end if
            origin = b%origin
            radius(axis) = 0.5d0*abs(b%height)
            radius(mod(axis, 3) + 1) = abs(b%a)
            radius(mod(axis + 1, 3) + 1) = abs(b%b)
        type is (t_HyperboloidXYZ)
            axis = b%axis
            bounded = .false.
            if (axis < 1 .or. axis > 3) return
            if (.not. all(ieee_is_finite([b%a, b%b, b%c, b%height]))) return
            if (b%c == 0d0) return
            scale = norm2([1d0, bounded_ratio(0.5d0*b%height, b%c)])
            if (max(abs(b%a), abs(b%b)) > (0.25d0*huge(1d0))/scale) return
            origin = b%origin
            radius(axis) = 0.5d0*abs(b%height)
            radius(mod(axis, 3) + 1) = abs(b%a)*scale
            radius(mod(axis + 1, 3) + 1) = abs(b%b)*scale
        type is (t_BoundaryRotationXYZ)
            bounded = boundary_bounds(b%pboundary, child)
            if (.not. bounded) return
            bounds(1, :) = huge(1d0)
            bounds(2, :) = -huge(1d0)
            do ix = 1, 2
                do iy = 1, 2
                    do iz = 1, 2
                        corner = [child(ix, 1), child(iy, 2), child(iz, 3)]
                        select case (b%axis)
                        case (1)
                            corner = rot3d_x(corner, b%rotation_rad)
                        case (2)
                            corner = rot3d_y(corner, b%rotation_rad)
                        case (3)
                            corner = rot3d_z(corner, b%rotation_rad)
                        end select
                        corner = corner + b%origin
                        bounds(1, :) = min(bounds(1, :), corner)
                        bounds(2, :) = max(bounds(2, :), corner)
                    end do
                end do
            end do
            bounded = usable_bounds(bounds)
            return
        type is (t_BoundaryList)
            bounds(1, :) = huge(1d0)
            bounds(2, :) = -huge(1d0)
            do i = 1, b%nboundaries
                bounded = boundary_bounds(b%boundaries(i)%ref, child)
                if (.not. bounded) return
                bounds(1, :) = min(bounds(1, :), child(1, :))
                bounds(2, :) = max(bounds(2, :), child(2, :))
            end do
            bounded = usable_bounds(bounds)
            return
        type is (tp_Boundary)
            bounded = boundary_bounds(b%ref, bounds)
            return
        class default
            ! Infinite shapes and extensions with arbitrary overridden methods.
            bounded = .false.
            return
        end select
        bounds(1, :) = origin - radius
        bounds(2, :) = origin + radius
        bounded = usable_bounds(bounds)
    end function

    pure logical function usable_bounds(bounds)
        double precision, intent(in) :: bounds(2, 3)
        usable_bounds = all(ieee_is_finite(bounds))
        if (.not. usable_bounds) return
        usable_bounds = all(bounds(1, :) <= bounds(2, :)) .and. all(abs(bounds) < 0.25d0*huge(1d0))
    end function
end module
