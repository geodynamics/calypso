!>@file   m_merge_spheric_mesh.f90
!!@brief  module m_merge_spheric_mesh
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine alloc_sph_mesh_parallel_merge
!!      subroutine dealloc_sph_mesh_4_merge
!!
!!      subroutine set_local_rj_mesh_4_merge(my_rank, sph_mesh)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_mesh_data), intent(inout) :: sph_mesh
!!      subroutine set_sph_boundary_4_merge(sph_grps,                   &
!!     &          nlayer_ICB, nlayer_CMB)
!!        type(sph_group_data), intent(in) ::  sph_grps
!!        integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
!!@endverbatim
!
      module m_merge_spheric_mesh
!
      use m_precision
      use t_spheric_mesh
      use t_sph_spectr_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_local_rj_mesh_4_merge(my_rank, sph_mesh_set)
!
      use m_node_id_spherical_IO
      use sph_file_IO_select
      use copy_sph_node_type_IO
      use copy_sph_comm_tbl_type_4_IO
      use copy_sph_grps_type_from_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_mesh_data), intent(inout) :: sph_mesh_set
!
!
      call sel_read_spectr_modes_rj_file(my_rank)
!
      call copy_sph_node_type_rj_from_IO                                &
     &    (sph_mesh_set%sph_mesh%l_truncation,                          &
     &     sph_mesh_set%sph_mesh%sph_rj)
      call copy_comm_sph_type_from_IO(my_rank,                          &
     &    sph_mesh_set%sph_mesh%sph_rj%nnod_rj,                         &
     &    sph_mesh_set%sph_comms%comm_rj)
!
      call copy_rj_radial_grp_type_from_IO(sph_mesh_set%sph_grps)
      call copy_rj_sphere_grp_type_from_IO(sph_mesh_set%sph_grps)
!
      end subroutine set_local_rj_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_boundary_4_merge(sph_grps,                     &
     &          nlayer_ICB, nlayer_CMB)
!
      type(sph_group_data), intent(in) ::  sph_grps
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
!
      integer(kind = kint) :: k, kk
!
!
      do k = 1, sph_grps%radial_rj_grp%num_grp
        if     (sph_grps%radial_rj_grp%grp_name(k) .eq. 'ICB'           &
     &     .or. sph_grps%radial_rj_grp%grp_name(k) .eq. 'icb') then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_ICB = sph_grps%radial_rj_grp%item_grp(kk)
        else if(sph_grps%radial_rj_grp%grp_name(k) .eq. 'CMB'           &
     &     .or. sph_grps%radial_rj_grp%grp_name(k) .eq. 'cmb') then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_CMB = sph_grps%radial_rj_grp%item_grp(kk)
        end if
      end do
!
      end subroutine set_sph_boundary_4_merge
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_field_name_assemble_sph(istep_start,              &
     &          org_sph_fst_head, np_sph_org, new_sph,                  &
     &          org_phys, new_phys)
!
      use m_field_data_IO
      use t_spheric_parameter
      use copy_rj_phys_type_4_IO
      use field_IO_select
!
!
      character(len = kchara),  intent(in) :: org_sph_fst_head
      integer(kind = kint),  intent(in) :: istep_start, np_sph_org
      type(sph_grids), intent(in) :: new_sph
!
      type(phys_data), intent(inout) :: org_phys(np_sph_org)
      type(phys_data), intent(inout) :: new_phys
!
      integer(kind = kint) :: ip
!
!
      phys_file_head = org_sph_fst_head
      call sel_read_alloc_step_SPH_file(izero, istep_start)
      call copy_rj_phys_name_t_from_IO                                  &
     &     (new_sph%sph_rj%nnod_rj, new_phys)
      call deallocate_phys_data_IO
      call deallocate_phys_data_name_IO
!
      do ip = 1, np_sph_org
        org_phys(ip)%num_phys =  new_phys%num_phys
        org_phys(ip)%ntot_phys = new_phys%ntot_phys
        call alloc_phys_name_type(org_phys(ip))
!
        org_phys(ip)%num_component =    new_phys%num_component
        org_phys(ip)%istack_component = new_phys%istack_component
        org_phys(ip)%phys_name =        new_phys%phys_name
      end do
!
      end subroutine load_field_name_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_assembled_sph_data(org_sph, new_sph, j_table,      &
     &          r_itp, org_phys, new_phys)
!
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      use parallel_assemble_sph
!
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(phys_data), intent(in) ::    org_phys
!
      type(phys_data), intent(inout) :: new_phys
!
!
      if(r_itp%iflag_same_rgrid .eq. 0) then
        call r_itp_field_data_sph_assemble(org_sph, new_sph, r_itp,     &
     &     j_table, new_phys%ntot_phys, org_phys%d_fld, new_phys%d_fld)
      else
        call copy_field_data_sph_assemble(org_sph, new_sph, j_table,    &
     &      new_phys%ntot_phys, org_phys%d_fld, new_phys%d_fld)
      end if
!
      call copy_field_data_sph_center(org_sph, new_sph, j_table,        &
     &    new_phys%ntot_phys, org_phys%d_fld, new_phys%d_fld)
!
      end subroutine set_assembled_sph_data
!
! -----------------------------------------------------------------------
!
      subroutine const_assembled_sph_data(irank_new, istep,             &
     &          new_sph, r_itp, new_phys)
!
      use m_phys_labels
      use m_control_param_newsph
      use m_field_data_IO
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      use parallel_assemble_sph
      use copy_time_steps_4_restart
      use copy_rj_phys_type_4_IO
      use field_IO_select
!
      integer(kind = kint), intent(in) :: irank_new, istep
      type(sph_grids), intent(in) :: new_sph
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(phys_data), intent(inout) :: new_phys
!
!
     if(r_itp%iflag_same_rgrid .eq. 0) then
!        write(*,*) 'extend_potential_magne'
        call extend_potential_magne(new_sph, r_itp, new_phys)
!            write(*,*) 'extend_inner_core_scalar'
        call extend_inner_core_scalar                                   &
     &      (fhd_temp, new_sph, r_itp, new_phys)
!            write(*,*) 'extend_inner_core_scalar'
        call extend_inner_core_scalar                                   &
     &      (fhd_light, new_sph, r_itp, new_phys)
      end if
!
      if(b_sph_ratio.ne.0.0d0 .or. b_sph_ratio.ne.1.0d0) then
        call mul_sph_magne(b_sph_ratio,                                 &
     &      new_sph%sph_rj%nnod_rj, new_phys%num_phys,                  &
     &      new_phys%ntot_phys, new_phys%istack_component,              &
     &      new_phys%phys_name, new_phys%d_fld)
      end if
!
!
      call copy_time_steps_to_restart
      call copy_rj_all_phys_name_t_to_IO                                &
     &     (new_sph%sph_rj%nnod_rj, new_phys)
!
      phys_file_head = new_sph_fst_head
      numgrid_phys_IO = new_sph%sph_rj%nnod_rj
      call allocate_phys_data_IO
!
      call copy_rj_all_phys_type_to_IO                                  &
     &     (new_sph%sph_rj%nnod_rj, new_phys)
!
      call sel_write_step_SPH_field_file(irank_new, istep)
      call deallocate_phys_data_IO
      call deallocate_phys_data_name_IO
!
      end subroutine const_assembled_sph_data
!
! -----------------------------------------------------------------------
!
      end module m_merge_spheric_mesh
