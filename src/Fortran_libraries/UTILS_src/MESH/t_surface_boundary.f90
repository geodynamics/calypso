!>@file  t_surface_boundary.f90
!!       module t_surface_boundary
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Structure for boundary condition on surface
!!
!!@verbatim
!!      subroutine alloc_scalar_surf_BC(sf_list)
!!      subroutine dealloc_scalar_surf_BC(sf_list)
!!        type(scalar_surf_BC_list), intent(inout) :: sf_list
!!@endverbatim
!
      module t_surface_boundary
!
      use m_precision
!
      implicit  none
!
!
!>      Structure for scalar's boundary condition on surface
      type scalar_surf_BC_list
!>     number of surface group
        integer (kind=kint) :: ngrp_sf
!>     surface group ID for boundary condition
        integer (kind=kint), allocatable :: igrp_sf(:)
!>     field value for boundary condition
        real (kind=kreal), allocatable :: sf_apt(:)
      end type scalar_surf_BC_list
!
!> @brief Structure for vactor's boundary condition on surface
      type vector_surf_BC_list
!>     maximum number number of surface group
        integer (kind=kint) :: nmax_sf
!>     number of surface group
        integer (kind=kint) :: ngrp_sf(3)
!>     surface group ID for boundary condition
        integer (kind=kint), allocatable :: id_grp_vect_sf(:,:)
!>     vector value for boundary condition
        real (kind=kreal), allocatable :: sf_apt(:,:)
      end type vector_surf_BC_list
!
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine alloc_scalar_surf_BC(sf_list)
!
      type(scalar_surf_BC_list), intent(inout) :: sf_list
!
      allocate( sf_list%igrp_sf(sf_list%ngrp_sf) )
      allocate( sf_list%sf_apt(sf_list%ngrp_sf) )
!
      if (sf_list%ngrp_sf.gt.0) then
        sf_list%igrp_sf = 0
        sf_list%sf_apt = 0.0d0
      end if
!
       end subroutine alloc_scalar_surf_BC
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_scalar_surf_BC(sf_list)
!
      type(scalar_surf_BC_list), intent(inout) :: sf_list
!
      deallocate( sf_list%igrp_sf )
      deallocate( sf_list%sf_apt )
!
       end subroutine dealloc_scalar_surf_BC
!
!-----------------------------------------------------------------------
!
      end module t_surface_boundary
