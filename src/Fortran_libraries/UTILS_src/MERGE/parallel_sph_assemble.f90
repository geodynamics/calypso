!>@file   parallel_sph_assemble.f90
!!@brief  module parallel_sph_assemble
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief routines for parallel spectr data assemble
!!
!!@verbatim
!!      subroutine share_sph_rj_data(np_sph_org, org_sph_mesh)
!!      subroutine share_spectr_field_names(np_sph_org, np_sph_new,     &
!!     &          new_sph_mesh, org_sph_phys, new_sph_phys)
!!
!!      subroutine share_r_interpolation_tbl(np_sph_new, new_sph_mesh,  &
!!     &          r_itp, nlayer_ICB_org, nlayer_CMB_org,                &
!!     &          nlayer_ICB_new, nlayer_CMB_new)
!!
!!      subroutine share_time_step_data
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
      use t_spheric_mesh
      use t_sph_spectr_data
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
      subroutine share_sph_rj_data(np_sph_org, org_sph_mesh)
!
      use m_node_id_spherical_IO
      use new_SPH_restart
!
      integer(kind = kint), intent(in) :: np_sph_org
      type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
!
      integer(kind = kint) :: ip, irank_org
!
!
      do ip = 1, np_sph_org
        irank_org = ip - 1
        if(mod(irank_org,nprocs) .ne. my_rank) cycle
!
        call set_local_rj_mesh_4_merge(irank_org, org_sph_mesh(ip))
      end do
!
!
      do ip = 1, np_sph_org
        irank_org = mod(ip - 1,nprocs)
!        write(*,*) 'MPI_Bcast sph_rank_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%sph_rank_rj,    &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast nidx_global_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_global_rj, &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast nnod_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,        &
     &      ione, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast nidx_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj,        &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ist_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%ist_rj,         &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ied_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%ied_rj,         &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end do
!
      do ip = 1, np_sph_org
        if(mod(ip-1,nprocs) .eq. my_rank) cycle
!
        call alloc_type_spheric_param_rj                                &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj)
        call alloc_type_sph_1d_index_rj                                 &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj)
      end do
!
      do ip = 1, np_sph_org
        irank_org = mod(ip - 1,nprocs)
!        write(*,*) 'MPI_Bcast idx_global_rj', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%idx_global_rj,  &
     &      itwo*org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,              &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
!        write(*,*) 'MPI_Bcast radius_1d_rj_r', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%radius_1d_rj_r, &
     &      org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(1),                &
     &      CALYPSO_REAL, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast idx_gl_1d_rj_r', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_r, &
     &      org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(1),                &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast idx_gl_1d_rj_j', ip
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j, &
     &      ithree*org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),         &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end do
!
      end subroutine share_sph_rj_data
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
        call link_field_name_type(new_sph_phys(1), new_sph_phys(jp))
      end do
!
      do jp = 1, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
         call alloc_phys_data_type                                      &
     &     (new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(jp))
      end do
!
      end subroutine share_spectr_field_names
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
      write(*,*) 'nlayer_ICB_org: ', nlayer_ICB_org, nlayer_CMB_org
      write(*,*) 'nlayer_ICB_new: ', nlayer_ICB_new, nlayer_CMB_new
!
      call MPI_Bcast(r_itp%iflag_same_rgrid, ione, CALYPSO_INTEGER,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),        &
     &    ione, CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. 0) write(*,*) 'iflag_same_rgrid: ',               &
     &            r_itp%iflag_same_rgrid,                               &
     &            new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1)
!
      if(r_itp%iflag_same_rgrid .eq. 0) then
        if(my_rank .ne. 0)  call allocate_radial_itp_tbl                &
     &             (new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1), r_itp)
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
      subroutine share_time_step_data
!
      use m_t_int_parameter
      use m_t_step_parameter
!
!
      call MPI_Bcast(i_step_init, ione, CALYPSO_INTEGER,                &
     &        izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_init, ione, CALYPSO_REAL,                     &
     &        izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(dt, ione, CALYPSO_REAL,                            &
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
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
      end if
!
      num = org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj                    &
     &     * org_sph_phys(1)%ntot_phys
      call MPI_Bcast(org_sph_phys(ip)%d_fld, num, CALYPSO_REAL,         &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_original_spectr_data
!
! -----------------------------------------------------------------------
!
      end module parallel_sph_assemble
