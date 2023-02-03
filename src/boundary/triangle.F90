module m_triangle_boundary
    use m_boundary_base
    use m_vector, only: cross, normalize
    implicit none

    type, extends(t_Boundary) :: t_Triangle
        double precision :: vertex(3, 3)
        double precision :: n(3)

    contains
        procedure :: check_collision => triangle_check_collision
        procedure :: hit => triangle_hit
        procedure :: is_overlap => triangle_is_overlap
        procedure :: pnormal => triangle_pnormal
    end type

    private
    public t_Triangle
    public new_Triangle

contains

    pure function det(a, b, c) result(ret)
        double precision, intent(in) :: a(3), b(3), c(3)

        double precision :: ret

        ret = a(1)*b(2)*c(3) &
              + a(2)*b(3)*c(1) &
              + a(3)*b(1)*c(2) &
              - a(1)*b(3)*c(2) &
              - a(2)*b(1)*c(3) &
              - a(3)*b(2)*c(1)
    end function

    pure function new_Triangle(vertex) result(obj)
        double precision, intent(in) :: vertex(3, 3)

        type(t_Triangle) :: obj

        double precision :: v12(3), v23(3)

        obj%vertex(:, :) = vertex(:, :)

        v12(:) = vertex(:, 2) - vertex(:, 1)
        v23(:) = vertex(:, 3) - vertex(:, 2)
        obj%n(:) = cross(v12, v23)

        call normalize(obj%n(:))
    end function

    pure function triangle_check_collision(self, p1, p2) result(record)
        class(t_Triangle), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: origin(3), dir(3)
        double precision :: d(3)
        double precision :: u, v
        double precision :: t
        double precision :: pos_collided(3)
        double precision :: v12(3), v13(3)

        double precision :: denominator

        origin(:) = p1(:)
        dir(:) = p2(:) - p1(:)
        v12(:) = self%vertex(:, 2) - self%vertex(:, 1)
        v13(:) = self%vertex(:, 3) - self%vertex(:, 1)

        ! Calculate triangle and line intersection p.
        !   p = v1 + v12*u + v13*v (0.0 <= u, v <= 1.0) -- 1.
        !   p = origin + dir*t (0 <= t <= 1) -- 2.
        !
        ! By 1. and 2.
        !   v1 + v12*u + v13*v = origin * dir*t = d
        !
        ! Therefore,
        !   |            |   |u|   | |
        !   |v12 v13 -dir| * |v| = |d|
        !   |            | * |t|   | |
        !
        ! Finally, solve for u, v, t using Cramer's rule.

        denominator = det(v12, v13, -dir)
        if (abs(denominator) <= 1d-10) then
            record%is_collided = .false.
            return
        end if

        d(:) = origin(:) - self%vertex(:, 1)

        u = det(d, v13, -dir)/denominator
        if (u < 0 .or. 1 < u) then
            record%is_collided = .false.
            return
        end if

        v = det(v12, d, -dir)/denominator
        if (v < 0 .or. 1 < u + v) then
            record%is_collided = .false.
            return
        end if

        t = det(v12, v13, d)/denominator

        if (t < 0 .or. 1 < t) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = t
        record%position = pos_collided
        record%material = self%material
    end function

    pure function triangle_hit(self, ray) result(hit_record)
        class(t_Triangle), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: origin(3), dir(3)
        double precision :: d(3)
        double precision :: u, v
        double precision :: t
        double precision :: pos_hit(3)
        double precision :: v12(3), v13(3)

        double precision :: denominator

        origin(:) = ray%origin(:)
        dir(:) = ray%direction(:)
        v12(:) = self%vertex(:, 2) - self%vertex(:, 1)
        v13(:) = self%vertex(:, 3) - self%vertex(:, 1)

        ! Calculate triangle and line intersection p.
        !   p = v1 + v12*u + v13*v (0.0 <= u, v <= 1.0) -- 1.
        !   p = origin + dir*t (0 <= t) -- 2.
        !
        ! By 1. and 2.
        !   v1 + v12*u + v13*v = origin * dir*t = d
        !
        ! Therefore,
        !   |            |   |u|   | |
        !   |v12 v13 -dir| * |v| = |d|
        !   |            | * |t|   | |
        !
        ! Finally, solve for u, v, t using Cramer's rule.

        denominator = det(v12, v13, -dir)
        if (abs(denominator) <= 1d-10) then
            hit_record%is_hit = .false.
            return
        end if
        d(:) = origin(:) - self%vertex(:, 1)

        u = det(d, v13, -dir)/denominator
        if (u < 0 .or. 1 < u) then
            hit_record%is_hit = .false.
            return
        end if

        v = det(v12, d, -dir)/denominator
        if (v < 0 .or. 1 < u + v) then
            hit_record%is_hit = .false.
            return
        end if

        t = det(v12, v13, d)/denominator

        if (t < 0) then
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

    pure function triangle_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_Triangle), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        double precision :: vmin(3), vmax(3)

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        vmin(1) = minval(self%vertex(1, :))
        vmin(2) = minval(self%vertex(2, :))
        vmin(3) = minval(self%vertex(3, :))
        vmax(1) = maxval(self%vertex(1, :))
        vmax(2) = maxval(self%vertex(2, :))
        vmax(3) = maxval(self%vertex(3, :))

        if (vmax(1) < sdoms_(1, 1) .or. sdoms_(2, 1) < vmin(1)) then
            is_overlap = .false.
            return
        end if

        if (vmax(2) < sdoms_(1, 2) .or. sdoms_(2, 2) < vmin(2)) then
            is_overlap = .false.
            return
        end if

        if (vmax(3) < sdoms_(1, 3) .or. sdoms_(2, 3) < vmin(3)) then
            is_overlap = .false.
            return
        end if

        is_overlap = .true.
    end function

    pure function triangle_pnormal(self, position) result(pnormal)
        class(t_Triangle), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = self%n(:)
    end function

end module
