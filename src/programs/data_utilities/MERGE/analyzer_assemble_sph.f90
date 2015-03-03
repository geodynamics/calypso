!>@file   analyzer_assemble_sph.f90
!!@brief  module analyzer_assemble_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_assemble_sph
!!      subroutine analyze_assemble_sph
!!@endverbatim
!
      module analyzer_assemble_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_merge_spheric_mesh
      use m_control_param_newsph
      use parallel_assemble_sph
      use copy_rj_phys_type_4_IO
      use r_interpolate_marged_sph
!
      implicit none
!
      type(sph_mesh_data), allocatable, save :: org_sph_mesh(:)
      type(phys_data), allocatable, save ::     org_sph_phys(:)
!
      type(sph_mesh_data), allocatable, save :: new_sph_mesh(:)
      type(phys_data), allocatable, save ::     new_sph_phys(:)
!
!
      type(sph_radial_itp_data), save :: r_itp
      type(rj_assemble_tbl), allocatable, save :: j_table(:,:)
      integer(kind = kint) :: nlayer_ICB_org, nlayer_CMB_org
      integer(kind = kint) :: nlayer_ICB_new, nlayer_CMB_new
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_assemble_sph
!
      use m_error_IDs
      use m_control_data_4_merge
!
      use m_node_id_spherical_IO
      use field_IO_select
      use parallel_sph_assemble
!
      integer(kind = kint) :: ip, jp, j, irank_new
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      allocate( org_sph_mesh(np_sph_org) )
      allocate( org_sph_phys(np_sph_org) )
      allocate( new_sph_mesh(np_sph_new) )
      allocate( new_sph_phys(np_sph_new) )
      allocate(j_table(np_sph_org,np_sph_new))
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      sph_file_head = org_sph_head
      call share_sph_rj_data(np_sph_org, org_sph_mesh)
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      sph_file_head = new_sph_head
      do jp = 1, np_sph_new
        irank_new = jp - 1
        if(mod(irank_new,nprocs) .ne. my_rank) cycle
!
        call set_local_rj_mesh_4_merge(irank_new, new_sph_mesh(jp))
!
!     Construct mode transfer table
        do ip = 1, np_sph_org
          call alloc_each_mode_tbl_4_assemble                           &
     &      (org_sph_mesh(ip)%sph_mesh, j_table(ip,jp))
          call set_mode_table_4_assemble(org_sph_mesh(ip)%sph_mesh,     &
     &        new_sph_mesh(jp)%sph_mesh, j_table(ip,jp))
        end do
      end do
!
!     construct interpolation table
!
      if(my_rank .eq. 0) then
        call set_sph_boundary_4_merge(org_sph_mesh(1)%sph_grps,         &
     &      nlayer_ICB_org, nlayer_CMB_org)
        call set_sph_boundary_4_merge(new_sph_mesh(1)%sph_grps,         &
     &      nlayer_ICB_new, nlayer_CMB_new)
!
        call sph_radial_interpolation_coef                              &
     &     (org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      org_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r,             &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r, r_itp)
      end if
!
      call share_r_interpolation_tbl(np_sph_new, new_sph_mesh,          &
     &          r_itp, nlayer_ICB_org, nlayer_CMB_org,                  &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
!      Construct field list from spectr file
!
      if(my_rank .eq. 0) then
        call load_field_name_assemble_sph                               &
     &     (istep_start, org_sph_fst_head, np_sph_org,                  &
     &      new_sph_mesh(1)%sph_mesh, org_sph_phys(1), new_sph_phys(1))
      end if
!
      call share_spectr_field_names(np_sph_org, np_sph_new,             &
     &    new_sph_mesh, org_sph_phys, new_sph_phys)
!
!
!      do jp = 1, np_sph_new
!        if(mod(jp-1,nprocs) .ne. my_rank) cycle
!        do ip = 1, np_sph_org
!          do j = 1, org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(2)
!            if(j_table(ip,jp)%j_org_to_new(j).gt. 0)                   &
!     &          write(50+my_rank,*) jp, ip, j,                         &
!     &                              j_table(ip,jp)%j_org_to_new(j)
!          end do
!        end do
!      end do
!
      end subroutine init_assemble_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_sph
!
      use m_phys_labels
      use m_sph_spectr_data
      use m_field_data_IO
      use m_t_step_parameter
      use field_IO_select
      use r_interpolate_marged_sph
      use copy_time_steps_4_restart
      use set_field_file_names
      use parallel_sph_assemble
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp, irank_org, irank_new, num
!
!
!     ---------------------
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        phys_file_head = org_sph_fst_head
        do ip = 1, np_sph_org
          irank_org = ip - 1
          if(mod(irank_org,nprocs) .ne. my_rank) cycle
!
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
        end do
        call share_time_step_data
!
!     Bloadcast original spectr data
        do ip = 1, np_sph_org
          call share_original_spectr_data(ip, np_sph_org,               &
     &        org_sph_mesh, org_sph_phys)
!
!     Copy spectr data to temporal array
          do jp = 1, np_sph_new
           if(mod(jp-1,nprocs) .ne. my_rank) cycle
!
            call set_assembled_sph_data(org_sph_mesh(ip)%sph_mesh,      &
     &          new_sph_mesh(jp)%sph_mesh, j_table(ip,jp), r_itp,       &
     &          org_sph_phys(ip), new_sph_phys(jp))
          end do
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        time = time_init
        i_step_MHD = i_step_init
        do jp = 1, np_sph_new
          irank_new = jp - 1
          if(mod(irank_new,nprocs) .ne. my_rank) cycle
          call const_assembled_sph_data(irank_new, istep,               &
     &        new_sph_mesh(jp)%sph_mesh, r_itp, new_sph_phys(jp))
        end do
!
        call calypso_mpi_barrier
      end do
!
      do jp = 1, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
        do ip = 1, np_sph_org
          call dealloc_mode_table_4_assemble(j_table(ip,jp))
        end do
      end do
      deallocate(j_table)
!
      deallocate(org_sph_mesh, org_sph_phys)
      deallocate(new_sph_mesh, new_sph_phys)
!
      call calypso_MPI_barrier
!
      if(iflag_delete_org_sph .gt. 0) then
        icou = 0
        phys_file_head = org_sph_fst_head
        do istep = istep_start, istep_end, increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_SPH_fld_file(iflag_field_data_fmt,                &
     &        np_sph_org, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_assemble_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_assemble_sph
