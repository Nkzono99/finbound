module m_plane_boundary
    use m_boundary_base
    use m_vector
    implicit none

    type, extends(t_Boundary) :: t_Plane
        double precision :: origin(3)
        double precision :: perp(3)
    contains
        procedure :: check_collision => plane_check_collision
        procedure :: is_overlap => plane_is_overlap
    end type

    type, extends(t_Boundary) :: t_PlaneXYZ
        integer :: axis
        double precision :: pos
    contains
        procedure :: check_collision => planeXYZ_check_collision
        procedure :: is_overlap => planeXYZ_is_overlap
    end type

    private
    public t_Plane
    public new_plane

    public t_PlaneXYZ
    public new_planeXYZ
    public new_planeX
    public new_planeY
    public new_planeZ

contains

    function new_plane(origin, perp) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: perp(3)
        type(t_Plane) :: obj

        obj%origin = origin
        obj%perp = perp
    end function

    pure function plane_check_collision(self, p1, p2) result(record)
        class(t_Plane), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: d1, d2
        double precision :: r

        d1 = dot(p1 - self%origin, self%perp)
        d2 = dot(p2 - self%origin, self%perp)
        if (d1*d2 >= 0) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = abs(d1)/(abs(d1) + abs(d2))
        record%position = (p2 - p1)*record%t + p1
    end function

    pure function plane_is_overlap(self, sdoms) result(is_overlap)
        class(t_Plane), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        logical :: is_overlap

        double precision :: d
        double precision :: pos(3)
        integer :: i, j, k

        pos = [sdoms(1, 1) - 1, sdoms(1, 2) - 1, sdoms(1, 3) - 1]
        d = dot(pos - self%origin, self%perp)
        do i = 1, 2
            do j = 1, 2
                do k = 1, 2
                    pos = [sdoms(i, 1) + (i - 1.5d0)*2.0d0, &
                           sdoms(j, 2) + (j - 1.5d0)*2.0d0, &
                           sdoms(k, 3) + (k - 1.5d0)*2.0d0]
                    if (d*dot(pos - self%origin, self%perp) < 0.0d0) then
                        is_overlap = .true.
                        return
                    end if
                end do
            end do
        end do

        is_overlap = .false.
    end function

    function new_planeXYZ(axis, pos) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: pos
        type(t_PlaneXYZ) :: obj

        obj%axis = axis
        obj%pos = pos
    end function

    function new_planeX(pos) result(obj)
        double precision, intent(in) :: pos
        type(t_PlaneXYZ) :: obj

        obj = new_planeXYZ(1, pos)
    end function

    function new_planeY(pos) result(obj)
        double precision, intent(in) :: pos
        type(t_PlaneXYZ) :: obj

        obj = new_planeXYZ(2, pos)
    end function

    function new_planeZ(pos) result(obj)
        double precision, intent(in) :: pos
        type(t_PlaneXYZ) :: obj

        obj = new_planeXYZ(3, pos)
    end function

    pure function planeXYZ_check_collision(self, p1, p2) result(record)
        class(t_PlaneXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: d1, d2
        double precision :: r
        double precision :: pos_collided(3)

        d1 = p1(self%axis) - self%pos
        d2 = p2(self%axis) - self%pos
        if (d1*d2 >= 0) then
            record%is_collided = .false.
            return
        end if

        r = abs(d1)/(abs(d1) + abs(d2))
        pos_collided = (p2 - p1)*r + p1

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
    end function

    pure function planeXYZ_is_overlap(self, sdoms) result(is_overlap)
        class(t_PlaneXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        logical :: is_overlap

        is_overlap = sdoms(1, self%axis) - 1 <= self%pos &
                   .and. self%pos <= sdoms(2, self%axis) + 1
    end function

end module
