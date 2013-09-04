!m_surf_data_list.f90
!     module m_surf_data_list
!
!     Written by H. Matsui on Feb., 2009
!
!      subroutine allocate_velo_surf_ctl
!      subroutine allocate_press_surf_ctl
!      subroutine allocate_temp_surf_ctl
!      subroutine allocate_magne_surf_ctl
!      subroutine allocate_vect_p_surf_ctl
!      subroutine allocate_magp_surf_ctl
!      subroutine allocate_current_surf_ctl
!      subroutine allocate_d_scalar_surf_ctl
!
!      subroutine deallocate_velo_surf_ctl
!      subroutine deallocate_press_surf_ctl
!      subroutine deallocate_temp_surf_ctl
!      subroutine deallocate_magne_surf_ctl
!      subroutine deallocate_vect_p_surf_ctl
!      subroutine deallocate_magp_surf_ctl
!      subroutine deallocate_current_surf_ctl
!      subroutine deallocate_composit_surf_ctl
! 
      module m_surf_data_list
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_bc_tq
      real (kind=kreal), allocatable :: bc_tq_magnitude(:)
      integer (kind=kint), allocatable :: ibc_tq_type(:)
      character (len=kchara), allocatable :: bc_tq_name(:)
! 
      integer (kind=kint) :: num_bc_wall
      real (kind=kreal), allocatable :: bc_wall_magnitude(:)
      integer (kind=kint), allocatable :: ibc_wall_type(:)
      character (len=kchara), allocatable :: bc_wall_name(:)
!
      integer (kind=kint) :: num_bc_h_flux
      real (kind=kreal), allocatable :: bc_h_flux_magnitude(:)
      integer (kind=kint), allocatable :: ibc_h_flux_type(:)
      character (len=kchara), allocatable :: bc_h_flux_name(:)
!
!
      integer (kind=kint) :: num_bc_bs
      real (kind=kreal),      allocatable :: bc_bs_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_bs_type(:)
      character (len=kchara), allocatable :: bc_bs_name(:)
!
      integer (kind=kint) :: num_bc_vps
      real (kind=kreal), allocatable :: bc_vps_magnitude(:)
      integer (kind=kint), allocatable :: ibc_vps_type(:)
      character (len=kchara), allocatable :: bc_vps_name(:)
! 
      integer (kind=kint) :: num_bc_js
      real (kind=kreal),      allocatable :: bc_js_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_js_type(:)
      character (len=kchara), allocatable :: bc_js_name(:)
! 
      integer (kind=kint) :: num_surf_magp
      real (kind=kreal), allocatable :: surf_magp_magnitude(:)
      integer (kind=kint), allocatable :: isurf_magp_type(:)
      character (len=kchara), allocatable :: surf_magp_name(:)
! 
!
      integer (kind=kint) :: num_surf_composition
      real (kind=kreal), allocatable :: surf_composit_magnitude(:)
      integer (kind=kint), allocatable :: isurf_composit_type(:)
      character (len=kchara), allocatable :: surf_composit_name(:)
! 
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_velo_surf_ctl
!
      allocate(bc_tq_magnitude(num_bc_tq))
      allocate(ibc_tq_type(num_bc_tq))
      allocate(bc_tq_name(num_bc_tq))
!
      if(num_bc_tq .gt. 0) then
        ibc_tq_type = 0
        bc_tq_magnitude = 0.0d0
      end if
!
      end subroutine allocate_velo_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_press_surf_ctl
!
      allocate(bc_wall_magnitude(num_bc_wall))
      allocate(bc_wall_name(    num_bc_wall))
      allocate(ibc_wall_type(num_bc_wall))
!
      if(num_bc_wall .gt. 0) ibc_wall_type= 0
!
      end subroutine allocate_press_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_temp_surf_ctl
!
      allocate(bc_h_flux_magnitude(num_bc_h_flux))
      allocate(ibc_h_flux_type(num_bc_h_flux))
      allocate(bc_h_flux_name(num_bc_h_flux))
!
      if(num_bc_h_flux .gt. 0) then
        ibc_h_flux_type = 0
        bc_h_flux_magnitude = 0.0d0
      end if
!
      end subroutine allocate_temp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_magne_surf_ctl
!
      allocate(bc_bs_magnitude(num_bc_bs))
      allocate(ibc_bs_type(num_bc_bs))
      allocate(bc_bs_name(num_bc_bs))
!
      if(num_bc_bs .gt. 0) then
        ibc_bs_type = 0
        bc_bs_magnitude = 0.0d0
      end if
!
      end subroutine allocate_magne_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_vect_p_surf_ctl
!
      allocate(bc_vps_magnitude(num_bc_vps))
      allocate(ibc_vps_type(num_bc_vps))
      allocate(bc_vps_name(num_bc_vps))
!
      if(num_bc_vps .gt. 0) then
        ibc_vps_type = 0
        bc_vps_magnitude = 0.0d0
      end if
!
      end subroutine allocate_vect_p_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_magp_surf_ctl
!
      allocate(surf_magp_magnitude(num_surf_magp))
      allocate(isurf_magp_type(num_surf_magp))
      allocate(surf_magp_name(num_surf_magp))
!
      if(num_surf_magp .gt. 0) then
        isurf_magp_type = 0
        surf_magp_magnitude = 0.0d0
      end if
!
      end subroutine allocate_magp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_current_surf_ctl
!
      allocate(bc_js_magnitude(num_bc_js))
      allocate(ibc_js_type(num_bc_js))
      allocate(bc_js_name(num_bc_js))
!
      if(num_bc_js .gt. 0) then
        ibc_js_type = 0
        bc_js_magnitude = 0.0d0
      end if
!
      end subroutine allocate_current_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_d_scalar_surf_ctl
!
        allocate(surf_composit_magnitude(num_surf_composition))
        allocate(isurf_composit_type(num_surf_composition))
        allocate(surf_composit_name(num_surf_composition))
!
      end subroutine allocate_d_scalar_surf_ctl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_velo_surf_ctl
!
        deallocate(bc_tq_magnitude)
        deallocate(ibc_tq_type)
        deallocate(bc_tq_name)
!
      end subroutine deallocate_velo_surf_ctl
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_press_surf_ctl
!
       if (num_bc_wall .ge.0) then
        deallocate(bc_wall_magnitude)
        deallocate(bc_wall_name)
        deallocate(ibc_wall_type)
       end if
!
       end subroutine deallocate_press_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_temp_surf_ctl
!
        deallocate(bc_h_flux_magnitude)
        deallocate(ibc_h_flux_type)
        deallocate(bc_h_flux_name)
!
      end subroutine deallocate_temp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_magne_surf_ctl
!
        deallocate(bc_bs_magnitude)
        deallocate(ibc_bs_type)
        deallocate(bc_bs_name)
!
      end subroutine deallocate_magne_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_vect_p_surf_ctl
!
        deallocate(bc_vps_magnitude)
        deallocate(ibc_vps_type)
        deallocate(bc_vps_name)
!
      end subroutine deallocate_vect_p_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_magp_surf_ctl
!
        deallocate(surf_magp_magnitude)
        deallocate(isurf_magp_type)
        deallocate(surf_magp_name)
!
      end subroutine deallocate_magp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_current_surf_ctl
!
        deallocate(bc_js_magnitude)
        deallocate(ibc_js_type)
        deallocate(bc_js_name)
!
      end subroutine deallocate_current_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_composit_surf_ctl
!
        deallocate(surf_composit_magnitude)
        deallocate(isurf_composit_type)
        deallocate(surf_composit_name)
!
      end subroutine deallocate_composit_surf_ctl
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_list
