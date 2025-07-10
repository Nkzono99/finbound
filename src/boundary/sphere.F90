module m_sphere_boundary
    use m_boundary_base
    use m_vector
    implicit none

    type, extends(t_Boundary) :: t_Sphere
        double precision :: origin(3)
        double precision :: radius
    contains
        procedure :: check_collision => sphere_check_collision
        procedure :: hit => sphere_hit
        procedure :: is_overlap => sphere_is_overlap
        procedure :: pnormal => sphere_pnormal
    end type

    type, extends(t_Sphere) :: t_CutSphereXYZ
        integer :: axis
        double precision :: lower
        double precision :: upper
    contains
        procedure :: check_collision => cutSphereXYZ_check_collision
        procedure :: hit => cutSphere_hit
        procedure :: is_overlap => cutSphereXYZ_is_overlap
        procedure :: pnormal => cutShpereXYZ_pnormal
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
        double precision :: d2
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

        block
            double precision :: d
            d = sqrt(d2)

            r = (-b - d)/a
            if (r < 0.0d0 .or. 1.0d0 < r) then
                r = (-b + d)/a
            end if
        end block

        if (r < 0.0d0 .or. 1.0d0 < r) then
            record%is_collided = .false.
            return
        end if

        pos_collided = (p2 - p1)*r + p1

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
        record%material = self%material
    end function

    pure function sphere_hit(self, ray) result(hit_record)
        class(t_Sphere), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: pos_hit(3)

        double precision :: q1(3), q2(3)

        double precision :: a, b, c
        double precision :: d2
        double precision :: t

        q1(:) = ray%origin(:) - self%origin(:)
        q2(:) = (ray%origin(:) + ray%direction(:)) - self%origin(:)

        a = sum(q1*q1 + q2*q2) - 2*sum(q1*q2)
        b = sum(q1*q2) - sum(q1*q1)
        c = sum(q1*q1) - self%radius*self%radius

        d2 = b*b - a*c
        if (d2 < 0) then
            hit_record%is_hit = .false.
            return
        end if

        block
            double precision :: d
            d = sqrt(d2)

            t = (-b - d)/a
            if (t < 0.0d0) then
                t = (-b + d)/a
            end if
        end block

        if (t < 0.0d0) then
            hit_record%is_hit = .false.
            return
        end if

        pos_hit(:) = ray%origin(:) + ray%direction(:)*t

        hit_record%is_hit = .true.
        hit_record%t = t
        hit_record%position(:) = pos_hit(:)
        hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
        hit_record%material = self%material
    end function

    pure function sphere_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_Sphere), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)
        double precision :: closest(3)
        double precision :: distance_squared
        integer :: i

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        do i = 1, 3
            if (self%origin(i) < sdoms_(1, i)) then
                closest(i) = sdoms_(1, i)
            else if (self%origin(i) > sdoms_(2, i)) then
                closest(i) = sdoms_(2, i)
            else
                closest(i) = self%origin(i)
            end if
        end do

        distance_squared = sum((closest - self%origin)**2)
        is_overlap = (distance_squared <= self%radius**2)
    end function

    pure function sphere_pnormal(self, position) result(pnormal)
        class(t_Sphere), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = normalized(position(:) - self%origin(:))
    end function

    function new_CutSphereXYZ(origin, radius, axis, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
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
        double precision, intent(in) :: radius
        double precision, intent(in) :: lower
        double precision, intent(in) :: upper
        type(t_CutSphereXYZ) :: obj

        obj = new_CutSphereXYZ(origin, radius, 1, lower, upper)
    end function

    function new_CutSphereY(origin, radius, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: lower
        double precision, intent(in) :: upper
        type(t_CutSphereXYZ) :: obj

        obj = new_CutSphereXYZ(origin, radius, 2, lower, upper)
    end function

    function new_CutSphereZ(origin, radius, lower, upper) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
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

        double precision :: pos_collided(3)

        double precision :: q1(3), q2(3)
        double precision :: a, b, c
        double precision :: d2, d
        double precision :: t

        q1(:) = p1(:) - self%origin(:)
        q2(:) = p2(:) - self%origin(:)

        a = sum(q1*q1 + q2*q2) - 2*sum(q1*q2)
        b = sum(q1*q2) - sum(q1*q1)
        c = sum(q1*q1) - self%radius*self%radius

        d2 = b*b - a*c
        if (d2 < 0) then
            record%is_collided = .false.
            return
        end if

        d = sqrt(d2)
        t = (-b - d)/a
        if (0d0 <= t .and. t <= 1.0d0) then
            pos_collided(:) = (p2(:) - p1(:))*t + p1(:)

            if (self%lower <= pos_collided(self%axis) &
                .and. pos_collided(self%axis) <= self%upper) then
                record%is_collided = .true.
                record%position(:) = pos_collided(:)
                record%t = t
                record%material = self%material
                return
            end if
        end if

        t = (-b + d)/a
        if (0d0 <= t .and. t <= 1.0d0) then
            pos_collided(:) = (p2(:) - p1(:))*t + p1(:)

            if (self%lower <= pos_collided(self%axis) &
                .and. pos_collided(self%axis) <= self%upper) then
                record%is_collided = .true.
                record%position(:) = pos_collided(:)
                record%t = t
                record%material = self%material
                return
            end if
        end if

        record%is_collided = .false.
    end function

    pure function cutSphere_hit(self, ray) result(hit_record)
        class(t_CutSphereXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: pos_hit(3)

        double precision :: q1(3), q2(3)
        double precision :: a, b, c
        double precision :: d2, d
        double precision :: t

        q1(:) = ray%origin(:) - self%origin(:)
        q2(:) = (ray%origin(:) + ray%direction(:)) - self%origin(:)

        a = sum(q1*q1 + q2*q2) - 2*sum(q1*q2)
        b = sum(q1*q2) - sum(q1*q1)
        c = sum(q1*q1) - self%radius*self%radius

        d2 = b*b - a*c
        if (d2 < 0) then
            hit_record%is_hit = .false.
            return
        end if

        d = sqrt(d2)
        t = (-b - d)/a
        if (t >= 0.0d0) then
            pos_hit(:) = ray%origin(:) + ray%direction(:)*t

            if (self%lower <= pos_hit(self%axis) &
                .and. pos_hit(self%axis) <= self%upper) then
                hit_record%is_hit = .true.
                hit_record%position(:) = pos_hit(:)
                hit_record%t = t
                hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
                hit_record%material = self%material
                return
            end if
        end if

        t = (-b + d)/a
        if (t >= 0.0d0) then
            pos_hit(:) = ray%origin(:) + ray%direction(:)*t

            if (self%lower <= pos_hit(self%axis) &
                .and. pos_hit(self%axis) <= self%upper) then
                hit_record%is_hit = .true.
                hit_record%position(:) = pos_hit(:)
                hit_record%t = t
                hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
                hit_record%material = self%material
                return
            end if
        end if

        hit_record%is_hit = .false.
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
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        if (sdoms_(2, self%axis) < self%lower &
            .or. self%upper < sdoms_(1, self%axis)) then
            is_overlap = .false.
            return
        end if

        is_overlap = sphere_is_overlap(self, sdoms_)
    end function

    pure function cutShpereXYZ_pnormal(self, position) result(pnormal)
        class(t_CutSphereXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = normalized(position(:) - self%origin(:))
    end function

end module
