program test_boundary_collision
    use m_boundary_base
    use m_hyperboloid_boundary
    implicit none

    call test_collision

contains

    subroutine test_collision
        class(t_Boundary), pointer :: boundary
        type(t_CollisionRecord) :: record
        type(t_HyperboloidXYZ), target :: hyperboloid

        double precision :: p1(3), p2(3)
        double precision :: z
        double precision :: origin(3)
        double precision :: a, b, c

        origin(:) = [0d0, 0d0, 5d0]
        hyperboloid = new_HyperboloidZ(origin, 10d0, 5d0, 20d0)

        boundary => hyperboloid

        a = hyperboloid%a
        b = hyperboloid%b
        c = hyperboloid%c
        print *, a, b, c
        print *, (10d0**2/a**2 - 10d0**2 / c**2)

        z = 10.1
        p1(:) = [20d0, 0d0, z]
        p2(:) = [0d0, 0d0, z]
        record = boundary%check_collision(p1, p2)

        print *, record%to_string()
    end subroutine
end program
