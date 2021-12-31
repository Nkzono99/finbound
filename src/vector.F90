module m_vector
    implicit none

    interface cross
        module procedure cross3d
    end interface

contains

    pure function dot(a, b) result(ret)
        double precision, intent(in) :: a(:)
        double precision, intent(in) :: b(:)
        double precision :: ret

        ret = sum(a(:)*b(:))
    end function

    pure function cross3d(a, b) result(ret)
        double precision, intent(in) :: a(3)
        double precision, intent(in) :: b(3)
        double precision :: ret(3)

        ret(1) = a(2)*b(3) - a(3)*b(2)
        ret(2) = a(3)*b(1) - a(1)*b(3)
        ret(3) = a(1)*b(2) - a(2)*b(1)
    end function

    pure function norm(a) result(ret)
        double precision, intent(in) :: a(:)
        double precision :: ret

        ret = sqrt(sum(a(:)*a(:)))
    end function

    pure function norm2(a) result(ret)
        double precision, intent(in) :: a(:)
        double precision :: ret

        ret = sum(a(:)*a(:))
    end function

    pure subroutine normalize(a)
        double precision, intent(inout) :: a(:)

        double precision :: nrm

        nrm = norm(a)

        if (nrm /= 0) then
            a = a/nrm
        end if
    end subroutine

    pure function normalized(a) result(ret)
        double precision, intent(in) :: a(:)
        double precision :: ret(3)
        double precision :: nrm

        nrm = norm(a)

        if (nrm /= 0) then
            ret = a/nrm
        else
            ret = 0
        end if
    end function

    pure function perp_vec(a) result(ret)
        double precision, intent(in) :: a(:)
        double precision :: ret(3)

        integer :: i
        integer :: n
        double precision :: d

        n = ubound(a, 1)
        do i = 1, n - 1
            ret(i) = a(i + 1)
        end do
        ret(n) = a(1)

        d = dot(a, ret)
        do i = 1, n
            if (a(i) /= 0) then
                ret(i) = -1.0d0/a(i)*(d - a(i)*ret(i))
                return
            end if
        end do
    end function

    pure function rot3d_x(a, theta) result(ret)
        double precision, intent(in) :: a(3)
        double precision, intent(in) :: theta
        double precision :: ret(3)

        ret(1) = a(1)
        ret(2) = cos(theta)*a(2) + sin(theta)*a(3)
        ret(3) = -sin(theta)*a(2) + cos(theta)*a(3)
    end function

    pure function rot3d_y(a, theta) result(ret)
        double precision, intent(in) :: a(3)
        double precision, intent(in) :: theta
        double precision :: ret(3)

        ret(1) = -sin(theta)*a(3) + cos(theta)*a(1)
        ret(2) = a(2)
        ret(3) = cos(theta)*a(3) + sin(theta)*a(1)
    end function

    pure function rot3d_z(a, theta) result(ret)
        double precision, intent(in) :: a(3)
        double precision, intent(in) :: theta
        double precision :: ret(3)

        ret(1) = cos(theta)*a(1) + sin(theta)*a(2)
        ret(2) = -sin(theta)*a(1) + cos(theta)*a(2)
        ret(3) = a(3)
    end function

end module
