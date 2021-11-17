!>@file  t_fem_gauss_int_coefs.f90
!!       module t_fem_gauss_int_coefs
!!
!!@author H. Matsui
!!@date   Programmed on Dec. 2003
!!@n      Modified by H. Matsui on Oct., 2006
!
!> @brief  Structure of gauss points for FEM
!!
!!@verbatim
!!      subroutine sel_max_int_point_by_etype(nnod_4_ele, g_FEM)
!!      subroutine set_max_integration_points(num_int, g_FEM)
!!      subroutine alloc_gauss_coef_4_fem(g_FEM)
!!      subroutine alloc_gauss_coef_to_4th(g_FEM)
!!
!!      subroutine dealloc_gauss_coef_4_fem(g_FEM)
!!
!!      subroutine num_of_int_points(g_FEM)
!!      subroutine copy_fem_gauss_int_coefs(org_g_FEM, new_g_FEM)
!!@endverbatim
!
      module t_fem_gauss_int_coefs
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type FEM_gauss_int_coefs
        integer(kind=kint) :: max_int_point = 4
        integer(kind=kint) :: maxtot_int_3d= 100
        integer(kind=kint) :: maxtot_int_2d= 30
        integer(kind=kint) :: maxtot_int_1d= 10
!
        integer(kind=kint), allocatable :: int_start1(:)
        integer(kind=kint), allocatable :: int_start2(:)
        integer(kind=kint), allocatable :: int_start3(:)
!
        real(kind = kreal), allocatable :: owe(:)
        real(kind = kreal), allocatable :: owe2d(:)
        real(kind = kreal), allocatable :: owe3d(:)
      end type FEM_gauss_int_coefs
! 
!-----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!> Set maximum number for integration points of FEM
!
      subroutine sel_max_int_point_by_etype(nnod_4_ele, g_FEM)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
!
!
      if(nnod_4_ele.eq.num_t_quad .or. nnod_4_ele.eq.num_t_lag) then
        call set_max_integration_points(ithree, g_FEM)
      else if(nnod_4_ele .eq. ione) then
        call set_max_integration_points(ione, g_FEM)
      else
        call set_max_integration_points(itwo, g_FEM)
      end if
!
      end subroutine sel_max_int_point_by_etype
!
! ----------------------------------------------------------------------
!
      subroutine set_max_integration_points(num_int, g_FEM)
!
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
      integer(kind = kint), intent(in) :: num_int
!
      g_FEM%max_int_point = num_int
!
      end subroutine set_max_integration_points
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_gauss_coef_4_fem(g_FEM)
!
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
!
      allocate(g_FEM%owe(g_FEM%maxtot_int_1d)  )
      allocate(g_FEM%owe2d(g_FEM%maxtot_int_2d))
      allocate(g_FEM%owe3d(g_FEM%maxtot_int_3d))
!
      allocate(g_FEM%int_start1(g_FEM%max_int_point))
      allocate(g_FEM%int_start2(g_FEM%max_int_point))
      allocate(g_FEM%int_start3(g_FEM%max_int_point))
!
      g_FEM%owe =   0.0d0
      g_FEM%owe2d = 0.0d0
      g_FEM%owe3d = 0.0d0
!
      end subroutine alloc_gauss_coef_4_fem
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coef_4_fem(g_FEM)
!
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
!
!
      if(allocated(g_FEM%owe) .eqv. .FALSE.) return
      deallocate(g_FEM%owe, g_FEM%owe2d, g_FEM%owe3d)
      deallocate(g_FEM%int_start1, g_FEM%int_start2, g_FEM%int_start3)
!
      end subroutine dealloc_gauss_coef_4_fem
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine num_of_int_points(g_FEM)
!
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
!
      integer(kind = kint) :: n
!
!
      g_FEM%maxtot_int_3d = 0
      g_FEM%maxtot_int_2d = 0
      g_FEM%maxtot_int_1d = 0
      do n = 1, g_FEM%max_int_point
        g_FEM%maxtot_int_3d = g_FEM%maxtot_int_3d + n*n*n
        g_FEM%maxtot_int_2d = g_FEM%maxtot_int_2d + n*n
        g_FEM%maxtot_int_1d = g_FEM%maxtot_int_1d + n
      end do
!
      end subroutine num_of_int_points
!
! ----------------------------------------------------------------------
!
      subroutine set_start_addres_4_FEM_int(g_FEM)
!
      type(FEM_gauss_int_coefs), intent(inout) :: g_FEM
      integer(kind = kint) :: n
!
!
      g_FEM%int_start3(1) = 0
      g_FEM%int_start2(1) = 0
      g_FEM%int_start1(1) = 0
      do n = 2, g_FEM%max_int_point
        g_FEM%int_start3(n) = g_FEM%int_start3(n-1) + (n-1)*(n-1)*(n-1)
        g_FEM%int_start2(n) = g_FEM%int_start2(n-1) + (n-1)*(n-1)
        g_FEM%int_start1(n) = g_FEM%int_start1(n-1) + (n-1)
      end do
!
      end subroutine set_start_addres_4_FEM_int
!
!-----------------------------------------------------------------------
!
      subroutine copy_fem_gauss_int_coefs(org_g_FEM, new_g_FEM)
!
      type(FEM_gauss_int_coefs), intent(in) :: org_g_FEM
      type(FEM_gauss_int_coefs), intent(inout) :: new_g_FEM
!
!
      new_g_FEM%max_int_point = org_g_FEM%max_int_point
!
      new_g_FEM%maxtot_int_3d = org_g_FEM%maxtot_int_3d
      new_g_FEM%maxtot_int_2d = org_g_FEM%maxtot_int_2d
      new_g_FEM%maxtot_int_1d = org_g_FEM%maxtot_int_1d
!
      call alloc_gauss_coef_4_fem(new_g_FEM)
!
      new_g_FEM%owe(1:new_g_FEM%maxtot_int_1d) =                        &
     &      org_g_FEM%owe(1:new_g_FEM%maxtot_int_1d)
      new_g_FEM%owe2d(1:new_g_FEM%maxtot_int_2d) =                      &
     &      org_g_FEM%owe2d(1:new_g_FEM%maxtot_int_2d)
      new_g_FEM%owe3d(1:new_g_FEM%maxtot_int_3d) =                      &
     &      org_g_FEM%owe3d(1:new_g_FEM%maxtot_int_3d)
!
      end subroutine copy_fem_gauss_int_coefs
!
!-----------------------------------------------------------------------
!
      end module t_fem_gauss_int_coefs
