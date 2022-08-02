module m_plane_with_hole_boundary
    use m_vector
    use m_boundary_base
    use m_plane_boundary
    implicit none

    type, extends(t_Boundary) :: t_PlaneXYZWithCircleHole
        integer :: axis
        double precision :: origin(3)
        double precision :: radius
    contains
        procedure :: check_collision => planeXYZWithCircleHole_check_collision
        procedure :: hit => planeXYZWithCircleHole_hit
        procedure :: is_overlap => planeXYZWithCircleHole_is_overlap
        procedure :: pnormal => planeXYZWithCircleHole_pnormal
    end type

    private
    public t_PlaneXYZWithCircleHole
    public new_planeXYZWithCircleHole
    public new_planeXYZWithCircleHoleX
    public new_planeXYZWithCircleHoleY
    public new_planeXYZWithCircleHoleZ

contains

    function new_planeXYZWithCircleHole(axis, origin, radius) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        type(t_PlaneXYZWithCircleHole) :: obj

        obj%axis = axis
        obj%origin(1:3) = origin(1:3)
        obj%radius = radius
    end function

    function new_planeXYZWithCircleHoleX(origin, radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        type(t_PlaneXYZWithCircleHole) :: obj

        obj = new_planeXYZWithCircleHole(1, origin, radius)
    end function

    function new_planeXYZWithCircleHoleY(origin, radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        type(t_PlaneXYZWithCircleHole) :: obj

        obj = new_planeXYZWithCircleHole(2, origin, radius)
    end function

    function new_planeXYZWithCircleHoleZ(origin, radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        type(t_PlaneXYZWithCircleHole) :: obj

        obj = new_planeXYZWithCircleHole(3, origin, radius)
    end function

    pure function planeXYZWithCircleHole_check_collision(self, p1, p2) result(record)
        class(t_PlaneXYZWithCircleHole), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: pos_collided(3)

        double precision :: dist, dir
        double precision :: t
        double precision :: r1, r2
        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        dist = self%origin(self%axis) - p1(self%axis)
        dir = p2(self%axis) - p1(self%axis)

        t = dist/dir

        if (t < 0.0d0 .or. 1.0d0 < t) then
            record%is_collided = .false.
            return
        end if

        pos_collided(:) = (1 - t)*p1(:) + t*p2(:) ! p1(:) + (p2(:) - p1(:))*t

        r1 = pos_collided(axis1) - self%origin(axis1)
        r2 = pos_collided(axis2) - self%origin(axis2)
        if (r1*r1 + r2*r2 < self%radius*self%radius) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = t
        record%position(:) = pos_collided(:)
        record%material = self%material
    end function

    pure function planeXYZWithCircleHole_hit(self, ray) result(hit_record)
        class(t_PlaneXYZWithCircleHole), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: dist, dir
        double precision :: t
        double precision :: pos_hit(3)
        double precision :: r1, r2
        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        dist = self%origin(self%axis) - ray%origin(self%axis)
        dir = ray%direction(self%axis)
        if (dist*dir <= 0) then
            hit_record%is_hit = .false.
            return
        end if

        t = abs(dist/dir)
        pos_hit(:) = ray%origin(:) + ray%direction(:)*t

        r1 = pos_hit(axis1) - self%origin(axis1)
        r2 = pos_hit(axis2) - self%origin(axis2)
        if (r1*r1 + r2*r2 < self%radius*self%radius) then
            hit_record%is_hit = .false.
            return
        end if

        hit_record%is_hit = .true.
        hit_record%t = t
        hit_record%position(:) = pos_hit(:)
        hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
        hit_record%material = self%material
    end function

    pure function planeXYZWithCircleHole_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_PlaneXYZWithCircleHole), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)

        extent_ = get_default_extent(extent)

        is_overlap = sdoms(1, self%axis) - extent_(1, self%axis) <= self%origin(self%axis) &
                     .and. self%origin(self%axis) <= sdoms(2, self%axis) + extent_(2, self%axis)
    end function

    pure function planeXYZWithCircleHole_pnormal(self, position) result(pnormal)
        class(t_planeXYZWithCircleHole), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = 0d0
        pnormal(self%axis) = 1d0
    end function

end module
