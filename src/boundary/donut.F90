module m_donut_boundary
    use m_boundary_base
    use m_vector
    implicit none

    type, extends(t_Boundary) :: t_DonutXYZ
        !> Axis (1: x, 2: y, 3: z)
        integer :: axis
        !> Original position.
        double precision :: origin(3)
        double precision :: radius
        double precision :: inner_radius

    contains
        procedure :: check_collision => donutXYZ_check_collision
        procedure :: hit => donutXYZ_hit
        procedure :: is_overlap => donutXYZ_is_overlap
        procedure :: pnormal => donutXYZ_pnormal
    end type

    private
    public t_DonutXYZ
    public new_DonutXYZ
    public new_DonutX
    public new_DonutY
    public new_DonutZ

contains

    pure function new_DonutXYZ(axis, origin, radius, inner_radius) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: inner_radius
        type(t_DonutXYZ) :: obj

        obj%axis = axis
        obj%origin(1:3) = origin(1:3)
        obj%radius = radius
        obj%inner_radius = inner_radius
    end function

    pure function new_DonutX(origin, radius, inner_radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: inner_radius
        type(t_DonutXYZ) :: obj

        obj = new_DonutXYZ(1, origin, radius, inner_radius)
    end function

    pure function new_DonutY(origin, radius, inner_radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: inner_radius
        type(t_DonutXYZ) :: obj

        obj = new_DonutXYZ(2, origin, radius, inner_radius)
    end function

    pure function new_DonutZ(origin, radius, inner_radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: inner_radius
        type(t_DonutXYZ) :: obj

        obj = new_DonutXYZ(3, origin, radius, inner_radius)
    end function

    pure function donutXYZ_check_collision(self, p1, p2) result(record)
        class(t_donutXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: dist, dir
        double precision :: t
        double precision :: pos_collided(3)
        double precision :: r1, r2
        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        dist = self%origin(axis0) - p1(axis0)
        dir = p2(axis0) - p1(axis0)

        t = dist/dir

        if (t < 0.0d0 .or. 1.0d0 < t) then
            record%is_collided = .false.
            return
        end if

        pos_collided = (p2 - p1)*t + p1

        r1 = pos_collided(axis1) - self%origin(axis1)
        r2 = pos_collided(axis2) - self%origin(axis2)
        if (r1*r1 + r2*r2 > self%radius*self%radius .or. &
            r1*r1 + r2*r2 < self%inner_radius*self%inner_radius) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = t
        record%position = pos_collided
        record%material = self%material
    end function

    pure function donutXYZ_hit(self, ray) result(hit_record)
        class(t_DonutXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: dist, dir
        double precision :: t_hit
        double precision :: pos_hit(3)
        double precision :: r1, r2
        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        dist = self%origin(axis0) - ray%origin(axis0)
        dir = ray%direction(axis0)

        t_hit = dist/dir

        if (t_hit < 0.0d0) then
            hit_record%is_hit = .false.
            return
        end if

        pos_hit(:) = ray%origin(:) + ray%direction(:)*t_hit

        r1 = pos_hit(axis1) - self%origin(axis1)
        r2 = pos_hit(axis2) - self%origin(axis2)
        if (r1*r1 + r2*r2 > self%radius*self%radius .or. &
            r1*r1 + r2*r2 < self%inner_radius*self%inner_radius) then
            hit_record%is_hit = .false.
            return
        end if

        hit_record%is_hit = .true.
        hit_record%t = t_hit
        hit_record%position(:) = pos_hit(:)
        hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
        hit_record%material = self%material
    end function

    pure function donutXYZ_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_donutXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        integer :: axis0, axis1, axis2

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) - extent_(2, :)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        if (self%origin(axis0) < sdoms(1, axis0) &
            .or. sdoms(2, axis0) < self%origin(axis0)) then
            is_overlap = .false.
        end if

        if (self%origin(axis1) + self%radius < sdoms(1, axis1) &
            .or. sdoms(2, axis1) < self%origin(axis1)) then
            is_overlap = .false.
        end if

        if (self%origin(axis2) + self%radius < sdoms(1, axis2) &
            .or. sdoms(2, axis2) < self%origin(axis2)) then
            is_overlap = .false.
        end if

        is_overlap = .true.
    end function

    pure function donutXYZ_pnormal(self, position) result(pnormal)
        class(t_DonutXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        pnormal(axis0) = 1
        pnormal(axis1) = 0
        pnormal(axis2) = 0
    end function

end module
