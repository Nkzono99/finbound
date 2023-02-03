module m_rectangleXYZ_boundary
    use m_boundary_base
    implicit none

    type, extends(t_Boundary) :: t_RectangleXYZ
        !> Axis (1: x, 2: y, 3: z)
        integer :: axis
        !> Original position.
        double precision :: origin(3)
        !> Rectangle width in mod(axis+1, 3)+1 axis.
        double precision :: w1
        !> Rectangle width in mod(axis+2, 3)+1 axis.
        double precision :: w2

    contains
        procedure :: check_collision => rectangleXYZ_check_collision
        procedure :: hit => rectangleXYZ_hit
        procedure :: is_overlap => rectangleXYZ_is_overlap
        procedure :: pnormal => rectangleXYZ_pnormal
    end type

    private

    public t_RectangleXYZ
    public new_rectangleXYZ
    public new_rectangleX
    public new_rectangleY
    public new_rectangleZ

contains

    pure function new_rectangleXYZ(axis, origin, w1, w2) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: w1
        double precision, intent(in) :: w2
        type(t_RectangleXYZ) :: obj

        obj%axis = axis
        obj%origin = origin
        obj%w1 = w1
        obj%w2 = w2
    end function

    pure function new_rectangleX(origin, wy, wz) result(obj)
        double precision, intent(in) :: origin(3)
        !> The width in y-axis (= self%w1)
        double precision, intent(in) :: wy
        !> The width in z-axis (= self%w2)
        double precision, intent(in) :: wz
        type(t_RectangleXYZ) :: obj

        obj = new_rectangleXYZ(1, origin, wy, wz)
    end function

    pure function new_rectangleY(origin, wz, wx) result(obj)
        double precision, intent(in) :: origin(3)
        !> The width in z-axis (= self%w1)
        double precision, intent(in) :: wz
        !> The width in x-axis (= self%w2)
        double precision, intent(in) :: wx
        type(t_RectangleXYZ) :: obj

        obj = new_rectangleXYZ(2, origin, wz, wx)
    end function

    pure function new_rectangleZ(origin, wx, wy) result(obj)
        double precision, intent(in) :: origin(3)
        !> The width in x-axis (= self%w1)
        double precision, intent(in) :: wx
        !> The width in y-axis (= self%w2)
        double precision, intent(in) :: wy
        type(t_RectangleXYZ) :: obj

        obj = new_rectangleXYZ(3, origin, wx, wy)
    end function

    pure function rectangleXYZ_check_collision(self, p1, p2) result(record)
        class(t_RectangleXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: d1, d2
        double precision :: r
        double precision :: pos_collided(3)
        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        d1 = p1(axis0) - self%origin(axis0)
        d2 = p2(axis0) - self%origin(axis0)
        if (d1*d2 >= 0) then
            record%is_collided = .false.
            return
        end if

        r = abs(d1)/(abs(d1) + abs(d2))
        pos_collided = (p2 - p1)*r + p1

        if (pos_collided(axis1) < self%origin(axis1) &
            .or. self%origin(axis1) + self%w1 < pos_collided(axis1) &
            .or. pos_collided(axis2) < self%origin(axis2) &
            .or. self%origin(axis2) + self%w2 < pos_collided(axis2)) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
        record%material = self%material
    end function

    pure function rectangleXYZ_hit(self, ray) result(hit_record)
        class(t_RectangleXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: dist, dir
        double precision :: t
        double precision :: pos_hit(3)
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

        if (pos_hit(axis1) < self%origin(axis1) &
            .or. self%origin(axis1) + self%w1 < pos_hit(axis1) &
            .or. pos_hit(axis2) < self%origin(axis2) &
            .or. self%origin(axis2) + self%w2 < pos_hit(axis2)) then
            hit_record%is_hit = .false.
            return
        end if

        hit_record%is_hit = .true.
        hit_record%t = t
        hit_record%position(:) = pos_hit(:)
        hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
        hit_record%material = self%material
    end function

    pure function rectangleXYZ_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_RectangleXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        integer :: axis0, axis1, axis2

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        if (self%origin(axis0) < sdoms_(1, axis0) &
            .or. sdoms_(2, axis0) < self%origin(axis0)) then
            is_overlap = .false.
            return
        end if

        if (self%origin(axis1) + self%w1 < sdoms_(1, axis1) &
            .or. sdoms_(2, axis1) < self%origin(axis1)) then
            is_overlap = .false.
            return
        end if

        if (self%origin(axis2) + self%w2 < sdoms_(1, axis2) &
            .or. sdoms_(2, axis2) < self%origin(axis2)) then
            is_overlap = .false.
            return
        end if

        is_overlap = .true.
    end function

    pure function rectangleXYZ_pnormal(self, position) result(pnormal)
        class(t_RectangleXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = 0d0
        pnormal(self%axis) = 1d0
    end function

end module
