module m_sphere_boundary
    use m_boundary_base
    use m_vector
    implicit none

    type, extends(t_Boundary) :: t_Sphere
        double precision :: origin(3)
        double precision :: radius
    contains
        procedure :: check_collision => sphere_check_collision
        procedure :: is_overlap => shpere_is_overlap
    end type

    type, extends(t_Sphere) :: t_CutSphereXYZ
        integer :: axis
        double precision :: lower
        double precision :: upper
    contains
        procedure :: check_collision => cutSphereXYZ_check_collision
        procedure :: is_overlap => cutSphereXYZ_is_overlap
    end type

    private
    public t_Sphere
    public new_Sphere
    public t_CutSphereXYZ
    public new_CutSphereXYZ
    public new_CutSphereX
    public new_CutSphereY
    public new_CutSphereZ

contains

    function new_Sphere(origin, radius) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        type(t_Sphere) :: obj

        obj%origin(1:3) = origin(1:3)
        obj%radius = radius
    end function

    pure function sphere_check_collision(self, p1, p2) result(record)
        class(t_Sphere), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: q1(3), q2(3)
        double precision :: pos_collided(3)

        double precision :: a, b, c
        double precision :: d2, d
        double precision :: r

        q1 = p1 - self%origin
        q2 = p2 - self%origin

        a = sum(q1*q1 + q2*q2) - 2*sum(q1*q2)
        b = sum(q1*q2) - sum(q1*q1)
        c = sum(q1*q1) - self%radius*self%radius

        d2 = b*b - a*c
        if (d2 < 0) then
            record%is_collided = .false.
            return
        end if

        d = sqrt(d2)
        r = (-b - d)/a
        if (r < 0.0d0 .or. 1.0d0 < r) then
            r = (-b + d)/a
        end if
        if (r < 0.0d0 .or. 1.0d0 < r) then
            record%is_collided = .false.
            return
        end if

        pos_collided = (p2 - p1)*r + p1

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
    end function

    pure function shpere_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_Sphere), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        logical :: origin_in_sdoms_x, origin_in_sdoms_y, origin_in_sdoms_z

        double precision :: pos(3)
        logical :: corner_in_shpere
        integer :: i, j, k

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) - extent_(2, :)

        origin_in_sdoms_x = all(sdoms_(1, :) - [self%radius, 0d0, 0d0] <= self%origin &
                                .and. self%origin <= sdoms_(2, :) + [self%radius, 0d0, 0d0])
        if (origin_in_sdoms_x) then
            is_overlap = .true.
            return
        end if

        origin_in_sdoms_y = all(sdoms_(1, :) - [0d0, self%radius, 0d0] <= self%origin &
                                .and. self%origin <= sdoms_(2, :) + [0d0, self%radius, 0d0])
        if (origin_in_sdoms_y) then
            is_overlap = .true.
            return
        end if

        origin_in_sdoms_z = all(sdoms_(1, :) - [0d0, 0d0, self%radius] <= self%origin &
                                .and. self%origin <= sdoms_(2, :) + [0d0, 0d0, self%radius])
        if (origin_in_sdoms_z) then
            is_overlap = .true.
            return
        end if

        do i = 1, 2
        do j = 1, 2
        do k = 1, 2
            pos = [sdoms_(i, 1), sdoms_(j, 2), sdoms_(k, 3)]
            corner_in_shpere = norm2(pos - self%origin) <= self%radius
            if (corner_in_shpere) then
                is_overlap = .true.
                return
            end if
        end do
        end do
        end do

        is_overlap = .false.
    end function

    function new_CutSphereXYZ(origin, radius, axis, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision ,intent(in) :: radius
        integer, intent(in) :: axis
        double precision, intent(in) :: lower
        double precision, intent(in) :: upper
        type(t_CutSphereXYZ) :: obj

        obj%origin = origin
        obj%radius = radius
        obj%axis = axis
        obj%lower = lower
        obj%upper = upper
    end function

    function new_CutSphereX(origin, radius, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision ,intent(in) :: radius
        double precision, intent(in) :: lower
        double precision, intent(in) :: upper
        type(t_CutSphereXYZ) :: obj

        obj = new_CutSphereXYZ(origin, radius, 1, lower, upper)
    end function

    function new_CutSphereY(origin, radius, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision ,intent(in) :: radius
        double precision, intent(in) :: lower
        double precision, intent(in) :: upper
        type(t_CutSphereXYZ) :: obj

        obj = new_CutSphereXYZ(origin, radius, 2, lower, upper)
    end function

    function new_CutSphereZ(origin, radius, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision ,intent(in) :: radius
        double precision, intent(in) :: lower
        double precision, intent(in) :: upper
        type(t_CutSphereXYZ) :: obj

        obj = new_CutSphereXYZ(origin, radius, 3, lower, upper)
    end function

    pure function cutSphereXYZ_check_collision(self, p1, p2) result(record)
        class(t_CutSphereXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        record = sphere_check_collision(self, p1, p2)
        if (.not. record%is_collided) then
            return
        end if

        if (record%position(self%axis) < self%lower &
            .or. self%upper < record%position(self%axis)) then
            record%is_collided = .false.
            return
        end if
    end function

    pure function cutSphereXYZ_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_CutSphereXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) - extent_(2, :)

        if (sdoms_(2, self%axis) < self%lower &
            .or. self%upper < sdoms_(1, self%axis)) then
            is_overlap = .false.
            return
        end if

        is_overlap = shpere_is_overlap(self, sdoms_)
    end function

end module
