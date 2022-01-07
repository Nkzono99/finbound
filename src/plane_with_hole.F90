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
        procedure :: is_overlap => planeXYZWithCircleHole_is_overlap
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

        double precision :: d1, d2
        double precision :: r
        double precision :: pos_collided(3)

        d1 = p1(self%axis) - self%origin(self%axis)
        d2 = p2(self%axis) - self%origin(self%axis)
        if (d1*d2 >= 0) then
            record%is_collided = .false.
            return
        end if

        r = abs(d1)/(abs(d1) + abs(d2))
        pos_collided = (p2 - p1)*r + p1

        if (norm(pos_collided - self%origin) < self%radius) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
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
end module