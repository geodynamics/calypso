!>@file   parallel_sph_assemble.f90
!!@brief  module parallel_sph_assemble
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief routines for parallel spectr data assemble
!!
!!@verbatim
!!      subroutine share_org_sph_rj_data                                &
!!     &         (org_sph_head, np_sph_org, org_sph_mesh)
!!      subroutine share_spectr_field_names(np_sph_org, np_sph_new,     &
!!     &          new_sph_mesh, org_sph_phys, new_sph_phys)
!!
!!      subroutine load_new_spectr_rj_data                              &
!!     &         (new_sph_head, np_sph_org, np_sph_new,                 &
!!     &          org_sph_mesh, new_sph_mesh, j_table)
!!
!!      subroutine share_r_interpolation_tbl(np_sph_new, new_sph_mesh,  &
!!     &          r_itp, nlayer_ICB_org, nlayer_CMB_org,                &
!!     &          nlayer_ICB_new, nlayer_CMB_new)
!!
!!      subroutine share_time_step_data(time_d)
!!        type(time_data), intent(inout) :: time_d
!!      subroutine share_original_spectr_data(ip, np_sph_org,           &
!!     &          org_sph_mesh, org_sph_phys)
!!@endverbatim
!!
!
      module parallel_sph_assemble
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
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_org_sph_rj_data                                  &
     &         (org_sph_head, np_sph_org, org_sph_mesh)
!
      use new_SPH_restart
      use sph_file_MPI_IO_select
!
      character(len=kchara), intent(in) :: org_sph_head
      integer(kind = kint), intent(in) :: np_sph_org
      type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
!
      integer(kind = kint) :: ip, irank_org
!
!
      call set_local_rj_mesh_4_merge                                    &
     &   (org_sph_head, np_sph_org, org_sph_mesh)
!
      do ip = 1, np_sph_org
        irank_org = mod(ip - 1,nprocs)
!        write(*,*) 'MPI_Bcast irank_sph_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%irank_sph_rj,        &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast nidx_global_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%nidx_global_rj,      &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast nnod_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%nnod_rj,             &
     &      ione, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast nidx_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%nidx_rj,             &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ist_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%ist_rj,              &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ied_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%ied_rj,              &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end do
!
      do ip = 1, np_sph_org
        if(mod(ip-1,nprocs) .eq. my_rank) cycle
!
        call alloc_type_spheric_param_rj(org_sph_mesh(ip)%sph%sph_rj)
        call alloc_type_sph_1d_index_rj(org_sph_mesh(ip)%sph%sph_rj)
      end do
!
      do ip = 1, np_sph_org
        irank_org = mod(ip - 1,nprocs)
!        write(*,*) 'MPI_Bcast idx_global_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%idx_global_rj,       &
     &      itwo*org_sph_mesh(ip)%sph%sph_rj%nnod_rj,                   &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
!        write(*,*) 'MPI_Bcast radius_1d_rj_r', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%radius_1d_rj_r,      &
     &      org_sph_mesh(ip)%sph%sph_rj%nidx_rj(1),                     &
     &      CALYPSO_REAL, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast idx_gl_1d_rj_r', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%idx_gl_1d_rj_r,      &
     &      org_sph_mesh(ip)%sph%sph_rj%nidx_rj(1),                     &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast idx_gl_1d_rj_j', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph%sph_rj%idx_gl_1d_rj_j,      &
     &      ithree*org_sph_mesh(ip)%sph%sph_rj%nidx_rj(2),              &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end do
!
      end subroutine share_org_sph_rj_data
!
! -----------------------------------------------------------------------
!
      subroutine share_spectr_field_names(np_sph_org, np_sph_new,       &
     &          new_sph_mesh, org_sph_phys, new_sph_phys)
!
      integer(kind = kint), intent(in) :: np_sph_org, np_sph_new
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
      type(phys_data), intent(inout) :: org_sph_phys(np_sph_org)
      type(phys_data), intent(inout) :: new_sph_phys(np_sph_new)
!
      integer(kind = kint) :: ip, jp
!
!
      do ip = 1, np_sph_org
!        write(*,*) 'MPI_Bcast num_phys', ip
        call MPI_Bcast(org_sph_phys(ip)%num_phys, ione,                 &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ntot_phys', ip
        call MPI_Bcast(org_sph_phys(ip)%ntot_phys, ione,                &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
        if(my_rank .ne. 0) call alloc_phys_name_type(org_sph_phys(ip))
!
!        write(*,*) 'MPI_Bcast num_component', ip
        call MPI_Bcast(org_sph_phys(ip)%num_component,                  &
     &      org_sph_phys(ip)%num_phys, CALYPSO_INTEGER,                 &
     &      izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast istack_component', ip
        call MPI_Bcast(org_sph_phys(ip)%istack_component,               &
     &      (org_sph_phys(ip)%num_phys+1),                              &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast iflag_monitor', ip
        call MPI_Bcast(org_sph_phys(ip)%iflag_monitor,                  &
     &      org_sph_phys(ip)%num_phys, CALYPSO_INTEGER,                 &
     &      izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast phys_name', ip
        call MPI_Bcast(org_sph_phys(ip)%phys_name,                      &
     &     (org_sph_phys(ip)%num_phys*kchara),                          &
     &      CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      end do
!
!
!        write(*,*) 'MPI_Bcast num_phys', ip
      call MPI_Bcast(new_sph_phys(1)%num_phys, ione, CALYPSO_INTEGER,   &
     &    izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ntot_phys', ip
      call MPI_Bcast(new_sph_phys(1)%ntot_phys, ione, CALYPSO_INTEGER,  &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) then
        call alloc_phys_name_type(new_sph_phys(1))
      end if
!
!        write(*,*) 'MPI_Bcast num_component', ip
      call MPI_Bcast(new_sph_phys(1)%num_component,                     &
     &    new_sph_phys(1)%num_phys, CALYPSO_INTEGER,                    &
     &    izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast istack_component', ip
      call MPI_Bcast(new_sph_phys(1)%istack_component,                  &
     &    (new_sph_phys(1)%num_phys+1),                                 &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast iflag_monitor', ip
      call MPI_Bcast(new_sph_phys(1)%iflag_monitor,                     &
     &    new_sph_phys(1)%num_phys, CALYPSO_INTEGER,                    &
     &    izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast phys_name', ip
      call MPI_Bcast(new_sph_phys(1)%phys_name,                         &
     &   (new_sph_phys(1)%num_phys*kchara),                             &
     &    CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
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
      end subroutine share_spectr_field_names
!
! -----------------------------------------------------------------------
!
      subroutine load_new_spectr_rj_data                                &
     &         (new_sph_head, np_sph_org, np_sph_new,                   &
     &          org_sph_mesh, new_sph_mesh, j_table)
!
      use parallel_assemble_sph
      use new_SPH_restart
      use sph_file_MPI_IO_select
!
      character(len=kchara), intent(in) :: new_sph_head
      integer(kind = kint), intent(in) :: np_sph_org, np_sph_new
      type(sph_mesh_data), intent(in) :: org_sph_mesh(np_sph_org)
      type(sph_mesh_data), intent(inout) :: new_sph_mesh(np_sph_new)
      type(rj_assemble_tbl), intent(inout)                              &
     &                             :: j_table(np_sph_org,np_sph_new)
!
      integer(kind = kint) :: iproc, jp, jproc, jrank_new
!
!
      call set_local_rj_mesh_4_merge                                    &
     &   (new_sph_head, np_sph_new, new_sph_mesh)
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
!
      subroutine share_r_interpolation_tbl(np_sph_new, new_sph_mesh,    &
     &          r_itp, nlayer_ICB_org, nlayer_CMB_org,                  &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
      use r_interpolate_marged_sph
!
      integer(kind = kint), intent(in) :: np_sph_new
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
!
      integer(kind = kint), intent(inout) :: nlayer_ICB_org
      integer(kind = kint), intent(inout) :: nlayer_CMB_org
      integer(kind = kint), intent(inout) :: nlayer_ICB_new
      integer(kind = kint), intent(inout) :: nlayer_CMB_new
      type(sph_radial_itp_data), intent(inout) :: r_itp
!
!
      call MPI_Bcast(nlayer_ICB_org, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nlayer_CMB_org, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nlayer_ICB_new, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nlayer_CMB_new, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .eq. 0) then
        write(*,*) 'nlayer_ICB_org: ', nlayer_ICB_org, nlayer_CMB_org
        write(*,*) 'nlayer_ICB_new: ', nlayer_ICB_new, nlayer_CMB_new
      end if
!
      call MPI_Bcast(r_itp%iflag_same_rgrid, ione, CALYPSO_INTEGER,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_mesh(1)%sph%sph_rj%nidx_rj(1),             &
     &    ione, CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. 0) write(*,*) 'iflag_same_rgrid: ',               &
     &            r_itp%iflag_same_rgrid,                               &
     &            new_sph_mesh(1)%sph%sph_rj%nidx_rj(1)
!
      if(r_itp%iflag_same_rgrid .eq. 0) then
        if(my_rank .ne. 0)  call allocate_radial_itp_tbl                &
     &             (new_sph_mesh(1)%sph%sph_rj%nidx_rj(1), r_itp)
!
        call MPI_Bcast(r_itp%nri_old2new, ione, CALYPSO_INTEGER,        &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%kr_inner_domain, ione, CALYPSO_INTEGER,    &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%kr_outer_domain, ione, CALYPSO_INTEGER,    &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%k_old2new_in, r_itp%nri_old2new,           &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%k_old2new_out, r_itp%nri_old2new,          &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%coef_old2new_in, r_itp%nri_old2new,        &
     &      CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      end if
!
      end subroutine share_r_interpolation_tbl
!
! -----------------------------------------------------------------------
!
      subroutine share_time_step_data(time_d)
!
      use t_time_data
!
      type(time_data), intent(inout) :: time_d
!
!
      call MPI_Bcast(time_d%i_time_step, ione, CALYPSO_INTEGER,         &
     &        izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_d%time, ione, CALYPSO_REAL,                   &
     &        izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_d%dt, ione, CALYPSO_REAL,                     &
     &        izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_time_step_data
!
! -----------------------------------------------------------------------
!
      subroutine share_original_spectr_data(ip, np_sph_org,             &
     &          org_sph_mesh, org_sph_phys)
!
      integer(kind = kint), intent(in) :: ip, np_sph_org
      type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
      type(phys_data), intent(inout) :: org_sph_phys(np_sph_org)
!
      integer(kind = kint) ::  irank_org, num
!
!
      irank_org = mod(ip-1,nprocs)
      if(mod(irank_org,nprocs) .ne. my_rank) then
        call alloc_phys_data_type                                       &
     &     (org_sph_mesh(ip)%sph%sph_rj%nnod_rj, org_sph_phys(ip))
      end if
!
      num = org_sph_mesh(ip)%sph%sph_rj%nnod_rj                         &
     &     * org_sph_phys(1)%ntot_phys
      call MPI_Bcast(org_sph_phys(ip)%d_fld, num, CALYPSO_REAL,         &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_original_spectr_data
!
! -----------------------------------------------------------------------
!
      end module parallel_sph_assemble
