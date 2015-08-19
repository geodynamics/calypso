!>@file   new_SPH_restart.f90
!!@brief  module new_SPH_restart
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
!!      subroutine load_field_name_assemble_sph                         &
!!     &         (org_sph_fst_head, ifmt_org_sph_fst, istep_start,      &
!!     &          np_sph_org, new_sph, org_phys, new_phys)
!!      subroutine load_org_sph_data(org_sph_fst_head, ifmt_org_sph_fst,&
!!     &          ip, istep, np_sph_org, org_sph, org_phys)
!!      subroutine const_assembled_sph_data                             &
!!     &         (b_ratio, new_sph, r_itp, new_phys, new_fst_IO)
!!@endverbatim
!
      module new_SPH_restart
!
      use m_precision
      use t_spheric_mesh
      use t_sph_spectr_data
      use t_field_data_IO
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
      subroutine load_field_name_assemble_sph                           &
     &         (org_sph_fst_head, ifmt_org_sph_fst, istep_start,        &
     &          np_sph_org, new_sph, org_phys, new_phys)
!
      use calypso_mpi
      use t_spheric_parameter
      use copy_rj_phys_type_4_IO
      use field_IO_select
!
!
      character(len=kchara), intent(in) :: org_sph_fst_head
      integer(kind=kint ), intent(in) :: ifmt_org_sph_fst
!
      integer(kind = kint),  intent(in) :: istep_start, np_sph_org
      type(sph_grids), intent(in) :: new_sph
!
      type(phys_data), intent(inout) :: org_phys(np_sph_org)
      type(phys_data), intent(inout) :: new_phys
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
      integer(kind = kint) :: ip
!
!
      call copy_rst_prefix_and_fmt                                      &
     &   (org_sph_fst_head, ifmt_org_sph_fst, org_fst_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (np_sph_org, izero, istep_start, org_fst_IO)
!
      if(my_rank .eq. 0) then
        call copy_rj_phys_name_t_from_IO                                &
     &     (new_sph%sph_rj%nnod_rj, org_fst_IO, new_phys)
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
      end if
!
      if(my_rank .lt. np_sph_org) then
        call dealloc_phys_data_IO(org_fst_IO)
        call dealloc_phys_name_IO(org_fst_IO)
      end if
!
      end subroutine load_field_name_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_org_sph_data(org_sph_fst_head, ifmt_org_sph_fst,  &
     &          ip, istep, np_sph_org, org_sph, org_phys)
!
      use field_IO_select
      use copy_rj_phys_type_4_IO
      use copy_time_steps_4_restart
!
      character(len=kchara), intent(in) :: org_sph_fst_head
      integer(kind=kint ), intent(in) :: np_sph_org
      integer(kind=kint ), intent(in) :: ifmt_org_sph_fst
!
      integer(kind = kint), intent(in) :: ip, istep
      type(sph_grids), intent(in) :: org_sph
      type(phys_data), intent(inout) :: org_phys
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
      integer(kind = kint) :: irank_org
!
      irank_org = ip - 1
      call copy_rst_prefix_and_fmt                                      &
     &   (org_sph_fst_head, ifmt_org_sph_fst, org_fst_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (np_sph_org, irank_org, istep, org_fst_IO)
!
      if(irank_org .lt. np_sph_org) then
        call copy_time_steps_from_restart
        call alloc_phys_data_type(org_sph%sph_rj%nnod_rj, org_phys)
        call copy_rj_phys_type_from_IO                                  &
     &       (org_sph%sph_rj%nnod_rj, org_fst_IO, org_phys)
!
        call dealloc_phys_data_IO(org_fst_IO)
        call dealloc_phys_name_IO(org_fst_IO)
      end if
!
      end subroutine load_org_sph_data
!
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
! -----------------------------------------------------------------------
!
      subroutine const_assembled_sph_data                               &
     &         (b_ratio, new_sph, r_itp, new_phys, new_fst_IO)
!
      use calypso_mpi
      use m_phys_labels
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      use parallel_assemble_sph
      use copy_time_steps_4_restart
      use copy_rj_phys_type_4_IO
      use field_IO_select
!
      real(kind=kreal ), intent(in) :: b_ratio
!
      type(sph_grids), intent(in) :: new_sph
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(phys_data), intent(inout) :: new_phys
      type(field_IO), intent(inout) :: new_fst_IO
!
!
        if(r_itp%iflag_same_rgrid .eq. 0) then
!        write(*,*) 'extend_potential_magne'
          call extend_potential_magne(new_sph, r_itp, new_phys)
!            write(*,*) 'extend_inner_core_scalar'
          call extend_inner_core_scalar                                 &
     &      (fhd_temp, new_sph, r_itp, new_phys)
!            write(*,*) 'extend_inner_core_scalar'
          call extend_inner_core_scalar                                 &
     &        (fhd_light, new_sph, r_itp, new_phys)
        end if
!
        if(b_ratio.ne.0.0d0 .or. b_ratio.ne.1.0d0) then
          call mul_sph_magne                                            &
     &     (b_ratio, new_sph%sph_rj%nnod_rj, new_phys%num_phys,         &
     &      new_phys%ntot_phys, new_phys%istack_component,              &
     &      new_phys%phys_name, new_phys%d_fld)
        end if
!
!
        call copy_time_steps_to_restart
        call copy_rj_all_phys_name_t_to_IO                              &
     &     (new_sph%sph_rj%nnod_rj, new_phys, new_fst_IO)
!
        new_fst_IO%nnod_IO = new_sph%sph_rj%nnod_rj
        call alloc_phys_data_IO(new_fst_IO)
!
        call copy_rj_all_phys_type_to_IO                                &
     &     (new_sph%sph_rj%nnod_rj, new_phys, new_fst_IO)
!
      end subroutine const_assembled_sph_data
!
! -----------------------------------------------------------------------
!
      end module new_SPH_restart
