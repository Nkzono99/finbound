module m_plane_boundary
    use m_boundary_base
    use m_vector
    implicit none

    type, extends(t_Boundary) :: t_Plane
        double precision :: origin(3)
        double precision :: perp(3)
    contains
        procedure :: check_collision => plane_check_collision
        procedure :: hit => plane_hit
        procedure :: is_overlap => plane_is_overlap
        procedure :: pnormal => plane_pnormal
    end type

    type, extends(t_Boundary) :: t_PlaneXYZ
        integer :: axis
        double precision :: pos
    contains
        procedure :: check_collision => planeXYZ_check_collision
        procedure :: hit => planeXYZ_hit
        procedure :: is_overlap => planeXYZ_is_overlap
        procedure :: pnormal => planeXYZ_pnormal
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

        r = abs(d1)/(abs(d1) + abs(d2))

        record%is_collided = .true.
        record%t = r
        record%position = (p2 - p1)*r + p1
        record%material = self%material
    end function

    pure function plane_hit(self, ray) result(hit_record)
        class(t_Plane), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: dist, dir
        double precision :: t
        double precision :: pos_hit(3)

        dist = dot(self%origin(:) - ray%origin(:), self%perp(:))
        dir = dot(ray%direction(:), self%perp(:))
        if (dist*dir <= 0) then
            hit_record%is_hit = .false.
            return
        end if

        t = abs(dist/dir)
        pos_hit(:) = ray%origin(:) + ray%direction(:)*t

        hit_record%is_hit = .true.
        hit_record%t = t
        hit_record%position(:) = pos_hit(:)
        hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
        hit_record%material = self%material
    end function

    pure function plane_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_Plane), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: d
        double precision :: pos(3)
        integer :: ix, iy, iz

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        pos = [sdoms_(1, 1), sdoms_(1, 2), sdoms_(1, 3)]
        d = dot(pos - self%origin, self%perp)
        do ix = 1, 2
            do iy = 1, 2
                do iz = 1, 2
                    pos = [sdoms_(ix, 1), sdoms_(iy, 2), sdoms_(iz, 3)]
                    if (d*dot(pos - self%origin, self%perp) < 0.0d0) then
                        is_overlap = .true.
                        return
                    end if
                end do
            end do
        end do

        is_overlap = .false.
    end function

    pure function plane_pnormal(self, position) result(pnormal)
        class(t_plane), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = self%perp(:)
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
        record%material = self%material
    end function

    pure function planeXYZ_hit(self, ray) result(hit_record)
        class(t_PlaneXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: dist, dir
        double precision :: t
        double precision :: pos_hit(3)

        dist = self%pos - ray%origin(self%axis)
        dir = ray%direction(self%axis)
        if (dist*dir <= 0) then
            hit_record%is_hit = .false.
            return
        end if

        t = abs(dist / dir)
        pos_hit(:) = ray%origin(:) + ray%direction(:)*t

        hit_record%is_hit = .true.
        hit_record%t = t
        hit_record%position(:) = pos_hit(:)
        hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
        hit_record%material = self%material
    end function

    pure function planeXYZ_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_PlaneXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)

        extent_ = get_default_extent(extent)

        is_overlap = sdoms(1, self%axis) - extent_(1, self%axis) <= self%pos &
                   .and. self%pos <= sdoms(2, self%axis) + extent_(2, self%axis)
    end function

    pure function planeXYZ_pnormal(self, position) result(pnormal)
        class(t_planeXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = 0d0
        pnormal(self%axis) = 1d0
    end function

end module
