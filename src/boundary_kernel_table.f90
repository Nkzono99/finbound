module m_kernel_table
    use iso_c_binding
    implicit none

    abstract interface
        logical function collide_iface(p1, p2, b_par) bind(C)
            import :: c_ptr, c_double
            real(c_double), intent(in) :: p1(3), p2(3)   !! 始点/終点
            type(c_ptr), value       :: b_par          !! 形状ごとの参数塊
        end function

        logical function hit_iface(ray, b_par) bind(C)
            import :: c_ptr, c_double
            real(c_double), intent(in) :: p1(3), p2(3)   !! 始点/終点
            type(c_ptr), value       :: b_par          !! 形状ごとの参数塊
        end function
    end interface

    type :: t_BoundaryKernel
        procedure(collide_iface), nopass, pointer :: collide => null()
        type(c_ptr)                       :: par = c_null_ptr
    end type

    private
    public :: t_BoundaryKernel
    public :: compile_boundaries

contains

    subroutine compile_boundaries(poly_boundaries, kernels)
        !! 引数: 多態配列  → kernel(:) を新規確保して返す
        use iso_c_binding
        implicit none

        ! ここでは class(*),allocatable を簡易的に扱う
        class(*), dimension(:), intent(in)  :: poly_boundaries
        type(t_BoundaryKernel), allocatable, intent(out) :: kernels(:)

        integer :: nb, i, nsph, ncyl, idx
        !
        ! --- 3‑A. カウント & SoA 領域確保 ----------------
        !
        nb = size(poly_boundaries)
        nsph = count([(allocated(poly_boundaries(i) .as. t_Sphere?.true.:.false.), i=1, nb)])
        ncyl = count([(allocated(poly_boundaries(i) .as. t_Cylinder?.true.:.false.), i=1, nb)])

        allocate (kernels(nb))

        ! SoA 配列確保 (球)
        if (nsph > 0) then
            type(t_SpherePar), pointer :: spar
            allocate (spar)
            spar%n = nsph
            allocate (real(c_double):: spar%cx(nsph), spar%cy(nsph), spar%cz(nsph), spar%r2(nsph))
            kernels(1:nsph)%collide => collide_sphere
            do i = 1, nsph
                idx = i   ! ← collide_sphere から見えるよう USE しても良い
                ! sphere = poly_boundaries(i) as t_Sphere
                spar%cx(i) = sphere%center(1)
                spar%cy(i) = sphere%center(2)
                spar%cz(i) = sphere%center(3)
                spar%r2(i) = sphere%radius**2
                kernels(i)%par = c_loc(spar)
            end do
        end if
    end subroutine
end module

