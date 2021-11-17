!>@file  t_jacobian_1d.f90
!!       module t_jacobian_1d
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief  Structure of 1D Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine alloc_1d_jac_type(nedge, nnod_4_edge, n_int, jac_1d)
!!        integer(kind = kint), intent(in) :: nedge, nnod_4_edge
!!        integer(kind = kint), intent(in) :: n_int
!!        type(jacobians_1d), intent(inout) :: jac_1d
!!
!!      subroutine dealloc_1d_jac_type(jac_1d)
!!
!!      subroutine copy_1d_jacobians                                    &
!!     &         (nedge, nnod_4_edge, jac_org, jac_new)
!!@endverbatim
!
      module t_jacobian_1d
!
      use m_precision
!
      implicit  none
!
!>     Stracture of Jacobians for edge
      type jacobians_1d
!>     Number of Gauss points
        integer(kind = kint) :: ntot_int
!
!>    Shape function
        real (kind=kreal), allocatable :: an_edge(:,:)
! 
!>    Difference of shape function
        real (kind=kreal), allocatable :: xeg_edge(:,:,:)
!
!>    Jacobian
        real (kind=kreal), allocatable :: xj_edge(:,:)
!>    1 / Jacobian
        real (kind=kreal), allocatable :: axj_edge(:,:)
      end type jacobians_1d
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_1d_jac_type(nedge, nnod_4_edge, n_int, jac_1d)
!
      integer(kind = kint), intent(in) :: nedge, nnod_4_edge
      integer(kind = kint), intent(in) :: n_int
!
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      jac_1d%ntot_int = n_int
      allocate(jac_1d%an_edge(nnod_4_edge,jac_1d%ntot_int))
!
      allocate(jac_1d%xeg_edge(nedge,jac_1d%ntot_int,3))
!
      allocate(jac_1d%xj_edge(nedge,jac_1d%ntot_int))
      allocate(jac_1d%axj_edge(nedge,jac_1d%ntot_int))
!
      jac_1d%an_edge = 0.0d0
!
      if (nedge .gt. 0) then
        jac_1d%xeg_edge = 0.0d0
!
        jac_1d%xj_edge = 0.0d0
        jac_1d%axj_edge = 0.0d0
      end if
!
      end subroutine alloc_1d_jac_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_1d_jac_type(jac_1d)
!
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      if(allocated(jac_1d%an_edge) .eqv. .FALSE.) return
      deallocate(jac_1d%an_edge)
      deallocate(jac_1d%xeg_edge)
!
      deallocate(jac_1d%xj_edge, jac_1d%axj_edge)
!
      end subroutine dealloc_1d_jac_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_1d_jacobians                                      &
     &         (nedge, nnod_4_edge, jac_org, jac_new)
!
      integer(kind = kint), intent(in) :: nedge, nnod_4_edge
!
      type(jacobians_1d), intent(in) :: jac_org
      type(jacobians_1d), intent(inout) :: jac_new
!
!
      call alloc_1d_jac_type                                            &
     &   (nedge, nnod_4_edge, jac_org%ntot_int, jac_new)
!
      jac_new%an_edge(1:nnod_4_edge,1:jac_org%ntot_int)                 &
     &      = jac_org%an_edge(1:nnod_4_edge,1:jac_org%ntot_int)
!
!$omp parallel workshare
      jac_new%xeg_edge(1:nedge,1:jac_org%ntot_int,1)                    &
     &      = jac_org%xeg_edge(1:nedge,1:jac_org%ntot_int,1)
      jac_new%xeg_edge(1:nedge,1:jac_org%ntot_int,2)                    &
     &      = jac_org%xeg_edge(1:nedge,1:jac_org%ntot_int,2)
      jac_new%xeg_edge(1:nedge,1:jac_org%ntot_int,3)                    &
     &      = jac_org%xeg_edge(1:nedge,1:jac_org%ntot_int,3)
      jac_new%xj_edge(1:nedge,1:jac_org%ntot_int)                       &
     &      = jac_org%xj_edge(1:nedge,1:jac_org%ntot_int)
      jac_new%axj_edge(1:nedge,1:jac_org%ntot_int)                      &
     &      = jac_org%axj_edge(1:nedge,1:jac_org%ntot_int)
!$omp end parallel workshare
!
       end subroutine copy_1d_jacobians
!
!  ---------------------------------------------------------------------
!
      end module t_jacobian_1d
