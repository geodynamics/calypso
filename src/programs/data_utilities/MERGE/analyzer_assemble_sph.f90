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
      use new_SPH_restart
      use m_control_param_newsph
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use r_interpolate_marged_sph
      use t_time_data
      use t_field_data_IO
      use t_assembled_field_IO
!
      implicit none
!
      type(time_data), save :: init_t
!
      type(sph_mesh_data), allocatable, save :: org_sph_mesh(:)
      type(phys_data), allocatable, save ::     org_sph_phys(:)
!
      type(sph_mesh_data), allocatable, save :: new_sph_mesh(:)
      type(phys_data), allocatable, save ::     new_sph_phys(:)
!
      integer(kind = kint) :: nloop_new
      type(field_IO), allocatable, save :: new_fst_IO(:)
      type(time_data), save :: fst_time_IO
!
      integer(kind = kint), allocatable :: nnod_list_lc(:)
      integer(kind = kint), allocatable :: nnod_list(:)
      integer(kind = kint_gl), allocatable :: istack_nnod_list(:)
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
      use bcast_4_assemble_sph_ctl
      use sph_file_IO_select
      use field_IO_select
      use parallel_sph_assemble
!
      integer(kind = kint) :: ip, jp, irank_new, jloop
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      if(my_rank .eq. 0) call read_control_assemble_sph
      call bcast_merge_control_data
      call set_control_4_newsph
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
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
      call share_org_sph_rj_data                                        &
     &   (org_sph_head, np_sph_org, org_sph_mesh)
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      call load_new_spectr_rj_data                                      &
     &   (new_sph_head, np_sph_org, np_sph_new,                         &
     &    org_sph_mesh, new_sph_mesh, j_table)
!
!     Share number of nodes for new mesh
!
      nloop_new = (np_sph_new-1)/nprocs + 1
      allocate(new_fst_IO(nloop_new))
!
      allocate(nnod_list_lc(np_sph_new))
      allocate(nnod_list(np_sph_new))
      allocate(istack_nnod_list(0:np_sph_new))
      nnod_list_lc(1:np_sph_new) = 0
      nnod_list(1:np_sph_new) = 0
!
      do jp = 1, np_sph_new
        irank_new = jp - 1
        if(mod(irank_new,nprocs) .ne. my_rank) cycle
        nnod_list_lc(jp) = new_sph_mesh(jp)%sph%sph_rj%nnod_rj
      end do
!
      call MPI_allREDUCE(nnod_list_lc, nnod_list, np_sph_new,           &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      istack_nnod_list(0) = 0
      do jp = 1, np_sph_new
        istack_nnod_list(jp) = istack_nnod_list(jp-1) + nnod_list(jp)
      end do
      do jloop = 1, nloop_new
        call alloc_merged_field_stack(np_sph_new, new_fst_IO(jloop))
        new_fst_IO(jloop)%istack_numnod_IO = istack_nnod_list
      end do
      deallocate(istack_nnod_list)
!
!     construct radial interpolation table
!
      if(my_rank .eq. 0) then
        call set_sph_boundary_4_merge(org_sph_mesh(1)%sph_grps,         &
     &      nlayer_ICB_org, nlayer_CMB_org)
        call set_sph_boundary_4_merge(new_sph_mesh(1)%sph_grps,         &
     &      nlayer_ICB_new, nlayer_CMB_new)
!
        call sph_radial_interpolation_coef                              &
     &     (org_sph_mesh(1)%sph%sph_rj%nidx_rj(1),                      &
     &      org_sph_mesh(1)%sph%sph_rj%radius_1d_rj_r,                  &
     &      new_sph_mesh(1)%sph%sph_rj%nidx_rj(1),                      &
     &      new_sph_mesh(1)%sph%sph_rj%radius_1d_rj_r, r_itp)
      end if
!
      call share_r_interpolation_tbl(np_sph_new, new_sph_mesh,          &
     &          r_itp, nlayer_ICB_org, nlayer_CMB_org,                  &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
!      Construct field list from spectr file
!
      call load_field_name_assemble_sph                                 &
     &   (istep_start, np_sph_org, org_sph_fst_param,                   &
     &    org_sph_phys(1), new_sph_phys(1), fst_time_IO)
!
      call share_spectr_field_names(np_sph_org, np_sph_new,             &
     &    new_sph_mesh, org_sph_phys, new_sph_phys)
!
!
!      do jp = 1, np_sph_new
!        if(mod(jp-1,nprocs) .ne. my_rank) cycle
!        do ip = 1, np_sph_org
!          do j = 1, org_sph_mesh(1)%sph%sph_rj%nidx_rj(2)
!            if(j_table(ip,jp)%j_org_to_new(j).gt. 0)                   &
!     &          write(50+my_rank,*) jp, ip, j,                         &
!     &                              j_table(ip,jp)%j_org_to_new(j)
!          end do
!        end do
!      end do
!      write(*,*) 'init_assemble_sph end'
!
      end subroutine init_assemble_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_sph
!
      use m_phys_labels
      use m_sph_spectr_data
      use r_interpolate_marged_sph
      use set_field_file_names
      use parallel_sph_assemble
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp, irank_new
      integer(kind = kint) :: iloop, jloop
      integer(kind = kint) :: istep_out
!
!
!     ---------------------
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        do iloop = 0, (np_sph_org-1)/nprocs
          irank_new = my_rank + iloop * nprocs
          ip = irank_new + 1
          call load_org_sph_data                                        &
     &       (irank_new, istep, np_sph_org, org_sph_fst_param,          &
     &        org_sph_mesh(ip)%sph, init_t, org_sph_phys(ip))
          call calypso_mpi_barrier
        end do
!
        istep_out = istep
        if(iflag_newtime .gt. 0) then
          istep_out =          istep_new_rst / increment_new_step
          init_t%i_time_step = istep_new_rst
          init_t%time =        time_new
        end if
!
        call share_time_step_data(init_t)
!
!     Bloadcast original spectr data
        do ip = 1, np_sph_org
          call share_original_spectr_data(ip, np_sph_org,               &
     &        org_sph_mesh, org_sph_phys)
!
!     Copy spectr data to temporal array
          do jp = 1, np_sph_new
           if(mod(jp-1,nprocs) .ne. my_rank) cycle
            call set_assembled_sph_data(org_sph_mesh(ip)%sph,           &
     &          new_sph_mesh(jp)%sph, j_table(ip,jp), r_itp,            &
     &          org_sph_phys(ip), new_sph_phys(jp))
          end do
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        do jloop = 1, nloop_new
          irank_new = my_rank + (jloop-1) * nprocs
          jp = irank_new + 1

          if(irank_new .lt. np_sph_new) then
            call const_assembled_sph_data                               &
     &          (b_sph_ratio, init_t, new_sph_mesh(jp)%sph, r_itp,      &
     &           new_sph_phys(jp), new_fst_IO(jloop), fst_time_IO)
          end if
        end do
!
        call sel_write_SPH_assemble_field(np_sph_new, istep_out,        &
     &      nloop_new, new_sph_fst_param, fst_time_IO, new_fst_IO)
!
        do jloop = 1, nloop_new
          irank_new = my_rank + (jloop-1) * nprocs
          if(irank_new .lt. np_sph_new) then
            call dealloc_phys_data_IO(new_fst_IO(jloop))
            call dealloc_phys_name_IO(new_fst_IO(jloop))
          end if
        end do
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
        do istep = istep_start, istep_end, increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_SPH_fld_file                                      &
     &        (org_sph_fst_param, np_sph_org, istep)
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
