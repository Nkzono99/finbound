module m_ray
    use m_material
    implicit none

    type :: t_Ray
        double precision :: origin(3) ! starting point
        double precision :: direction(3) ! direction

    contains
        procedure :: at => ray_at
    end type

    type :: t_HitRecord
        logical :: is_hit
        double precision :: t = 0.0d0
        double precision :: position(3)
        double precision :: n(3)
        integer :: priority
        type(t_Material) :: material
    end type

    private
    public t_Ray
    public new_Ray
    public t_HitRecord

contains

    function new_Ray(o, d) result(obj)
        double precision, intent(in) :: o(3)
        double precision, intent(in) :: d(3)
        type(t_Ray) :: obj

        obj%origin = o
        obj%direction = d
    end function

    function ray_at(self, t) result(ret)
        class(t_Ray), intent(in) :: self
        double precision, intent(in) :: t

        double precision :: ret(3)

        ret = self%origin + t*self%direction
    end function

end module
