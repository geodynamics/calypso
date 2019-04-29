!>@file   assemble_sph_fields.f90
!!@brief  module assemble_sph_fields
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief routines for parallel spectr data assemble
!!
!!@verbatim
!!      subroutine share_org_sph_rj_data(np_sph_org, org_sph_mesh)
!!        type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
!!      subroutine share_org_spectr_field_names                         &
!!     &         (np_sph_org, org_sph_phys)
!!        type(phys_data), intent(inout) :: org_sph_phys(np_sph_org)
!!      subroutine share_new_spectr_field_names                         &
!!     &         (np_sph_new, new_sph_mesh, new_sph_phys)
!!        type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
!!        type(phys_data), intent(inout) :: new_sph_phys(np_sph_new)
!!
!!      subroutine load_new_spectr_rj_data(np_sph_org, np_sph_new,      &
!!     &          org_sph_mesh, new_sph_mesh, j_table)
!!        type(sph_mesh_data), intent(in) :: org_sph_mesh(np_sph_org)
!!        type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
!!        type(rj_assemble_tbl), intent(inout)                          &
!!       &                             :: j_table(np_sph_org,np_sph_new)
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
      subroutine share_org_sph_rj_data(np_sph_org, org_sph_mesh)
!
      use share_spectr_index_data
!
      integer, intent(in) :: np_sph_org
      type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_sph_org
        call share_sph_rj_data(ip, org_sph_mesh(ip))
      end do
!
      end subroutine share_org_sph_rj_data
!
! -----------------------------------------------------------------------
!
      subroutine share_org_spectr_field_names                           &
     &         (np_sph_org, org_sph_phys)
!
      use share_field_data
!
      integer, intent(in) :: np_sph_org
      type(phys_data), intent(inout) :: org_sph_phys(np_sph_org)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_sph_org
        call share_phys_field_names(org_sph_phys(ip))
      end do
!
      end subroutine share_org_spectr_field_names
!
! -----------------------------------------------------------------------
!
      subroutine share_new_spectr_field_names                           &
     &         (np_sph_new, new_sph_mesh, new_sph_phys)
!
      use share_field_data
!
      integer, intent(in) :: np_sph_new
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
      type(phys_data), intent(inout) :: new_sph_phys(np_sph_new)
!
      integer(kind = kint) :: jp
!
!
!
      call share_phys_field_names(new_sph_phys(1))
!
      do jp = 2, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
        call copy_field_name_type(new_sph_phys(1), new_sph_phys(jp))
      end do
!
      do jp = 1, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
         call alloc_phys_data_type                                      &
     &     (new_sph_mesh(jp)%sph%sph_rj%nnod_rj, new_sph_phys(jp))
      end do
!
      end subroutine share_new_spectr_field_names
!
! -----------------------------------------------------------------------
!
      subroutine load_new_spectr_rj_data(np_sph_org, np_sph_new,        &
     &          org_sph_mesh, new_sph_mesh, j_table)
!
      use parallel_assemble_sph
!
      integer, intent(in) :: np_sph_org, np_sph_new
      type(sph_mesh_data), intent(in) :: org_sph_mesh(np_sph_org)
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
      type(rj_assemble_tbl), intent(inout)                              &
     &                             :: j_table(np_sph_org,np_sph_new)
!
      integer(kind = kint) :: iproc, jp, jproc, jrank_new
!
!     Construct mode transfer table
      do jp = 0, (np_sph_new-1) / nprocs
        jrank_new = my_rank + jp * nprocs
        jproc = jrank_new + 1
        if(jrank_new .ge. np_sph_new) cycle
        do iproc = 1, np_sph_org
          call alloc_each_mode_tbl_4_assemble                           &
     &      (org_sph_mesh(iproc)%sph, j_table(iproc,jproc))
          call set_mode_table_4_assemble(org_sph_mesh(iproc)%sph,       &
     &        new_sph_mesh(jproc)%sph, j_table(iproc,jproc))
        end do
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
      use m_phys_labels
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
        if(sph_phys%phys_name(i) .eq. fhd_magne) then
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
      use m_phys_labels
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
