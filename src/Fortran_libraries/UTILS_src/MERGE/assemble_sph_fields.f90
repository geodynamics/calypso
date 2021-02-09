!>@file   assemble_sph_fields.f90
!!@brief  module assemble_sph_fields
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief routines for parallel spectr data assemble
!!
!!@verbatim
!!      subroutine share_org_sph_rj_data(org_sph_array)
!!        type(sph_mesh_array), intent(inout) :: org_sph_array
!!      subroutine share_org_spectr_field_names(org_sph_array)
!!        type(sph_mesh_array), intent(inout) :: org_sph_array
!!      subroutine share_new_spectr_field_names(new_sph_data)
!!        type(SPH_mesh_field_data), intent(inout) :: new_sph_data
!!
!!      subroutine load_new_spectr_rj_data                              &
!!     &         (org_sph_array, new_sph_data, j_table)
!!        type(sph_mesh_array), intent(in) :: org_sph_array
!!        type(SPH_mesh_field_data), intent(in) :: new_sph_data
!!        type(rj_assemble_tbl), intent(inout)                          &
!!     &                      :: j_table(org_sph_array%num_pe)
!!
!!      subroutine extend_potential_magne(sph, r_itp, sph_phys)
!!      subroutine extend_inner_core_scalar                             &
!!     &         (field_name, sph, r_itp, sph_phys)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_radial_itp_data), intent(in) :: r_itp
!!        type(phys_data), intent(inout) :: sph_phys
!!@endverbatim
!!
!
      module assemble_sph_fields
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_sph_spectr_data
      use t_SPH_mesh_field_array
      use t_SPH_mesh_field_data
!
      implicit none
!
      private :: extend_potential_magne_type
      private :: extend_inner_core_scl_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_org_sph_rj_data(org_sph_array)
!
      use share_spectr_index_data
!
      type(sph_mesh_array), intent(inout) :: org_sph_array
!
      integer :: ip
!
!
      do ip = 1, org_sph_array%num_pe
        call share_sph_rj_data(ip, org_sph_array%sph(ip))
      end do
!
      end subroutine share_org_sph_rj_data
!
! -----------------------------------------------------------------------
!
      subroutine share_org_spectr_field_names(org_sph_array)
!
      use share_field_data
!
      type(sph_mesh_array), intent(inout) :: org_sph_array
!
      integer :: ip
!
!
      do ip = 1, org_sph_array%num_pe
        call share_phys_field_names(org_sph_array%fld(ip))
      end do
!
      end subroutine share_org_spectr_field_names
!
! -----------------------------------------------------------------------
!
      subroutine share_new_spectr_field_names(new_sph_data)
!
      use share_field_data
!
      type(SPH_mesh_field_data), intent(inout) :: new_sph_data
!
!
      call share_phys_field_names(new_sph_data%fld)
      call alloc_phys_data(new_sph_data%sph%sph_rj%nnod_rj,             &
     &                     new_sph_data%fld)
!
      end subroutine share_new_spectr_field_names
!
! -----------------------------------------------------------------------
!
      subroutine load_new_spectr_rj_data                                &
     &         (org_sph_array, new_sph_data, j_table)
!
      use parallel_assemble_sph
!
      type(sph_mesh_array), intent(in) :: org_sph_array
      type(SPH_mesh_field_data), intent(in) :: new_sph_data
      type(rj_assemble_tbl), intent(inout)                              &
     &                      :: j_table(org_sph_array%num_pe)
!
      integer :: ip
!
!     Construct mode transfer table
      do ip = 1, org_sph_array%num_pe
        call alloc_each_mode_tbl_4_assemble                             &
     &     (org_sph_array%sph(ip), j_table(ip))
        call set_mode_table_4_assemble(org_sph_array%sph(ip),           &
     &      new_sph_data%sph, j_table(ip))
      end do
!
      end subroutine load_new_spectr_rj_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine extend_potential_magne(sph, r_itp, sph_phys)
!
      use extend_potential_field_t
!
      use m_base_field_labels
      use t_sph_spectr_data
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      type(sph_grids), intent(in) :: sph
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(phys_data), intent(inout) :: sph_phys
!
      integer(kind = kint) :: is_magne
      integer(kind = kint) :: i
!
!
      is_magne = 0
      do i = 1, sph_phys%num_phys
        if(sph_phys%phys_name(i) .eq. magnetic_field%name) then
          is_magne = sph_phys%istack_component(i-1) + 1
          exit
        end if
      end do
      if(is_magne .eq. 0) return
!
      call extend_potential_magne_type(is_magne, sph_phys%ntot_phys,    &
     &    sph, r_itp%kr_inner_domain, r_itp%kr_outer_domain,            &
     &    sph_phys%d_fld)
!
      end subroutine extend_potential_magne
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_scalar                               &
     &         (field_name, sph, r_itp, sph_phys)
!
      use t_sph_spectr_data
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      character(len = kchara), intent(in) :: field_name
      type(sph_grids), intent(in) :: sph
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(phys_data), intent(inout) :: sph_phys
!
!
      integer(kind = kint) :: is_field
      integer(kind = kint) :: i
!
!
      is_field = 0
      do i = 1, sph_phys%num_phys
        if(sph_phys%phys_name(i) .eq. field_name) then
          is_field = sph_phys%istack_component(i-1) + 1
          exit
        end if
      end do
      if(is_field .eq. 0) return
!
      call extend_inner_core_scl_type(is_field, sph_phys%ntot_phys,     &
     &    sph, r_itp%kr_inner_domain, sph_phys%d_fld)
!
      end subroutine extend_inner_core_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine extend_potential_magne_type(is_magne,  ntot_phys_rj,   &
     &         sph, kr_inner_domain, kr_outer_domain, d_rj)
!
      use extend_potential_field_t
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: kr_outer_domain
      integer(kind = kint), intent(in) :: kr_inner_domain
      integer(kind = kint), intent(in) :: is_magne, ntot_phys_rj
      real(kind= kreal), intent(inout)                                  &
     &                  :: d_rj(sph%sph_rj%nnod_rj,ntot_phys_rj)
!
!
      if(kr_outer_domain .lt. sph%sph_rj%nidx_rj(1)) then
        call ext_outside_potential_t(sph%sph_rj,                        &
     &      kr_outer_domain, d_rj(1,is_magne))
      end if
      if(kr_inner_domain .gt. 1) then
        call ext_inside_potential_t(sph%sph_rj,                         &
     &      kr_inner_domain, d_rj(1,is_magne))
      end if
!
      end subroutine extend_potential_magne_type
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_scl_type(is_field, ntot_phys_rj,     &
     &          sph, kr_inner_domain, d_rj)
!
      use extend_potential_field_t
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in):: kr_inner_domain
      integer(kind = kint), intent(in) :: is_field, ntot_phys_rj
      real(kind= kreal), intent(inout)                                  &
     &                  :: d_rj(sph%sph_rj%nnod_rj,ntot_phys_rj)
!
!
      if(kr_inner_domain .le. 1) return
        call ext_inside_scalar_t(sph%sph_rj,                            &
     &      kr_inner_domain, d_rj(1,is_field))
!
      end subroutine extend_inner_core_scl_type
!
! -----------------------------------------------------------------------
!
      end module assemble_sph_fields
