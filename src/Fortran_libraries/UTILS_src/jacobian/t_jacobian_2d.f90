!>@file  t_jacobian_2d.f90
!!       module t_jacobian_2d
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief  Structure of 2D Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine alloc_2d_jac_type(nsurf, nnod_4_surf, n_int, jac_2d)
!!        integer(kind = kint), intent(in) :: nsurf, nnod_4_surf
!!        integer(kind = kint), intent(in) :: n_int
!!        type(jacobians_2d), intent(inout) :: jac_2d
!!
!!      subroutine dealloc_2d_jac_type(jac_2d)
!!
!!      subroutine copy_jacobians_2d                                    &
!!     &         (nsurf, nnod_4_surf, jac_2d_org, jac_2d_new)
!!        type(jacobians_2d), intent(in) :: jac_2d_org
!!        type(jacobians_2d), intent(inout) :: jac_2d_new
!!@endverbatim
!
      module t_jacobian_2d
!
      use m_precision
!
      implicit  none
!
!>     Stracture of Jacobians for surface
      type jacobians_2d
!>     Number of Gauss points
        integer(kind=kint) :: ntot_int
!>    Shape function
        real (kind=kreal), allocatable :: an_sf(:,:)
! 
!>    Difference of shape function
        real (kind=kreal), allocatable :: xsf_sf(:,:,:)
!
!>    Jacobian
        real (kind=kreal), allocatable :: xj_sf(:,:)
!>    1 / Jacobian
        real (kind=kreal), allocatable :: axj_sf(:,:)
      end type jacobians_2d
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_2d_jac_type(nsurf, nnod_4_surf, n_int, jac_2d)
!
      integer(kind = kint), intent(in) :: nsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: n_int
!
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      jac_2d%ntot_int = n_int
      allocate(jac_2d%an_sf(nnod_4_surf,jac_2d%ntot_int))
!
      allocate(jac_2d%xsf_sf(nsurf,jac_2d%ntot_int,3))
!
      allocate(jac_2d%xj_sf(nsurf,jac_2d%ntot_int))
      allocate(jac_2d%axj_sf(nsurf,jac_2d%ntot_int))
!
      jac_2d%an_sf = 0.0d0
!
      if (nsurf .gt. 0) then
        jac_2d%xsf_sf = 0.0d0
!
        jac_2d%xj_sf = 0.0d0
        jac_2d%axj_sf = 0.0d0
      end if
!
      end subroutine alloc_2d_jac_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_2d_jac_type(jac_2d)
!
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      if(allocated(jac_2d%an_sf) .eqv. .FALSE.) return
      deallocate(jac_2d%an_sf)
      deallocate(jac_2d%xsf_sf)
!
      deallocate(jac_2d%xj_sf, jac_2d%axj_sf)
!
      end subroutine dealloc_2d_jac_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_jacobians_2d                                      &
     &         (nsurf, nnod_4_surf, jac_2d_org, jac_2d_new)
!
      integer(kind = kint), intent(in) :: nsurf, nnod_4_surf
      type(jacobians_2d), intent(in) :: jac_2d_org
      type(jacobians_2d), intent(inout) :: jac_2d_new
!
!
      call alloc_2d_jac_type(nsurf, nnod_4_surf,                       &
     &    jac_2d_org%ntot_int, jac_2d_new)
!
      jac_2d_new%an_sf   = jac_2d_org%an_sf
      jac_2d_new%xsf_sf  = jac_2d_org%xsf_sf
      jac_2d_new%xj_sf  = jac_2d_org%xj_sf
      jac_2d_new%axj_sf = jac_2d_org%axj_sf
!
       end subroutine copy_jacobians_2d
!
! ----------------------------------------------------------------------
!
      end module t_jacobian_2d
