!>@file   single_assemble_sph.f90
!!@brief  module single_assemble_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine init_para_assemble_sph
!!      subroutine analyze_para_assemble_sph
!!@endverbatim
!
      program assemble_newsph
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_phys_labels
      use m_merge_spheric_mesh
      use m_control_param_newsph
      use parallel_assemble_sph
      use copy_rj_phys_type_4_IO
!
!
      use m_control_data_4_merge
!
      use m_node_id_spherical_IO
!
      use m_sph_spectr_data
      use m_field_data_IO
      use m_t_step_parameter
      use field_IO_select
      use r_interpolate_marged_sph
      use copy_time_steps_4_restart
      use set_field_file_names
!
      implicit none
!
      type(sph_mesh_data), allocatable, save :: org_sph_mesh(:)
      type(phys_data), allocatable, save ::     org_sph_phys(:)
!
      type(sph_mesh_data), allocatable, save :: new_sph_mesh(:)
      type(phys_data), allocatable, save ::     new_sph_phys(:)
!
      type(sph_radial_itp_data), save :: r_itp
      type(rj_assemble_tbl), allocatable, save :: j_table_s(:,:)
      integer(kind = kint) :: nlayer_ICB_org, nlayer_CMB_org
      integer(kind = kint) :: nlayer_ICB_new, nlayer_CMB_new
!
      integer(kind = kint) :: istep
      integer(kind = kint) :: jp, ip, irank_org
!
!
      write(*,*) 'Simulation start: PE. '
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      allocate( org_sph_mesh(np_sph_org) )
      allocate( org_sph_phys(np_sph_org) )
      allocate( new_sph_mesh(np_sph_new) )
      allocate( new_sph_phys(np_sph_new) )
      allocate(j_table_s(np_sph_new,np_sph_org))
!
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      sph_file_head = org_sph_head
      do ip = 1, np_sph_org
        irank_org = ip - 1
        call set_local_rj_mesh_4_merge(irank_org, org_sph_mesh(ip))
      end do
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      sph_file_head = new_sph_head
      do jp = 1, np_sph_new
        call set_local_rj_mesh_4_merge(jp-1, new_sph_mesh(jp))
!
        do ip = 1, np_sph_org
!     Construct mode transfer table
          call alloc_each_mode_tbl_4_assemble                           &
     &      (org_sph_mesh(ip)%sph_mesh, j_table_s(jp,ip))
          call set_mode_table_4_assemble(org_sph_mesh(ip)%sph_mesh,     &
     &      new_sph_mesh(jp)%sph_mesh, j_table_s(jp,ip))
        end do
      end do
!
!     construct interpolation table
!
      call set_sph_boundary_4_merge(org_sph_mesh(1)%sph_grps,           &
     &    nlayer_ICB_org, nlayer_CMB_org)
      call set_sph_boundary_4_merge(new_sph_mesh(1)%sph_grps,           &
     &    nlayer_ICB_new, nlayer_CMB_new)
!
      call sph_radial_interpolation_coef                                &
     &     (org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      org_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r,             &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r, r_itp)
!
!      Construct field list from spectr file
!
      call load_field_name_assemble_sph                                 &
     &     (istep_start, org_sph_fst_head, np_sph_org,                  &
     &      new_sph_mesh(1)%sph_mesh, org_sph_phys(1), new_sph_phys(1))
!
      do jp = 2, np_sph_new
        call link_field_name_type(new_sph_phys(1), new_sph_phys(jp))
      end do
!
      do jp = 1, np_sph_new
        call alloc_phys_data_type                                       &
     &     (new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(jp))
      end do
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        phys_file_head = org_sph_fst_head
        do ip = 1, np_sph_org
          irank_org = ip - 1
          call sel_read_alloc_step_SPH_file(irank_org, istep)
!
          call copy_time_steps_from_restart
          call alloc_phys_data_type                                     &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
          call copy_rj_phys_type_from_IO                                &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
!
          call deallocate_phys_data_IO
          call deallocate_phys_data_name_IO
!
          do jp = 1, np_sph_new
            call set_assembled_sph_data(org_sph_mesh(ip)%sph_mesh,      &
     &          new_sph_mesh(jp)%sph_mesh, j_table_s(jp,ip), r_itp,     &
     &          org_sph_phys(ip), new_sph_phys(jp))
          end do
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        time = time_init
        i_step_MHD = i_step_init
        do jp = 1, np_sph_new
          call const_assembled_sph_data((jp-1), istep,                  &
     &        new_sph_mesh(jp)%sph_mesh, r_itp, new_sph_phys(jp))
        end do
!
        write(*,*) 'step', istep, 'finish '
      end do
!
      do jp = 1, np_sph_new
        do ip = 1, np_sph_org
          call dealloc_mode_table_4_assemble(j_table_s(jp,ip))
        end do
      end do
      deallocate(j_table_s)
!
      deallocate(org_sph_mesh, org_sph_phys)
      deallocate(new_sph_mesh, new_sph_phys)
!
      if(iflag_delete_org_sph .gt. 0) then
        phys_file_head = org_sph_fst_head
        do istep = istep_start, istep_end, increment_step
          call delete_SPH_fld_file(iflag_field_data_fmt,                &
     &        np_sph_org, istep)
        end do
      end if
!
      end program assemble_newsph
