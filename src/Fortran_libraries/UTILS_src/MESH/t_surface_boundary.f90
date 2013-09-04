!t_surface_boundary.f90
!     module t_surface_boundary
!
!> @brief Structure for boundary condition on surface
!
!     written by H. Matsui on Dec., 2008
!
!      subroutine alloc_scalar_surf_BC(sf_list)
!      subroutine dealloc_scalar_surf_BC(sf_list)
!        type(scalar_surf_BC_list), intent(inout) :: sf_list
!
      module t_surface_boundary
!
      use m_precision
!
      implicit  none
!
!
!> @brief Structure for scalar's boundary condition on surface
      type scalar_surf_BC_list
        integer (kind=kint) :: ngrp_sf
!<     number of surface group
        integer (kind=kint), pointer :: igrp_sf(:)
!<     surface group ID for boundary condition
        real (kind=kreal), pointer :: sf_apt(:)
!<     field value for boundary condition
      end type scalar_surf_BC_list
!
!> @brief Structure for vactor's boundary condition on surface
      type vector_surf_BC_list
        integer (kind=kint) :: nmax_sf
!<     maximum number number of surface group
        integer (kind=kint) :: ngrp_sf(3)
!<     number of surface group
        integer (kind=kint), pointer :: id_grp_vect_sf(:,:)
!<     surface group ID for boundary condition
        real (kind=kreal), pointer :: sf_apt(:,:)
!<     vector value for boundary condition
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
