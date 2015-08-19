!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_grids(ndomain_sph, comm_rlm)
!!      subroutine para_gen_sph_rtm_grids(ndomain_sph, comm_rtm)
!!
!!      subroutine para_gen_sph_rj_modes(ndomain_sph, comm_rlm)
!!      subroutine para_gen_sph_rtp_grids(ndomain_sph, comm_rtm)
!!
!!      subroutine para_gen_fem_mesh_for_sph(ndomain_sph)
!!
!!      subroutine dealloc_comm_stacks_sph(ndomain_sph, comm_rtm)
!!@endverbatim
!
      module para_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use m_work_time
      use calypso_mpi
!
      use t_sph_trans_comm_tbl
      use set_local_sphere_by_global
!
      implicit none
!
      integer(kind = kint), allocatable :: nneib_rtm_lc(:)
      integer(kind = kint), allocatable :: nneib_rtm_gl(:)
!
      private :: nneib_rtm_lc, nneib_rtm_gl
      private :: allocate_nneib_sph_rtm_tmp
      private :: deallocate_nneib_sph_rtm_tmp
      private :: bcast_comm_stacks_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nneib_sph_rtm_tmp(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
      allocate(nneib_rtm_lc(ndomain_sph))
      allocate(nneib_rtm_gl(ndomain_sph))
      nneib_rtm_lc = 0
      nneib_rtm_gl = 0
!
      end subroutine allocate_nneib_sph_rtm_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nneib_sph_rtm_tmp
!
!
      deallocate(nneib_rtm_lc, nneib_rtm_gl)
!
      end subroutine deallocate_nneib_sph_rtm_tmp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_grids(ndomain_sph, comm_rlm)
!
      use m_sph_trans_comm_table
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use copy_sph_comm_table_4_type
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rlm(ndomain_sph)
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rlm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rlm_modes(ip_rank)
        if(iflag_debug .gt. 0) write(*,*) 'copy_comm_rlm_num_to_type'
        call copy_comm_rlm_num_to_type(comm_rlm(ip))
!
        if(iflag_debug .gt. 0) write(*,*) 'copy_comm_rlm_num_to_type d'
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans(ip_rank)
!
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &          ip_rank, ' is done.'
      end do
      call bcast_comm_stacks_sph(ndomain_sph, comm_rlm)
!
      end subroutine para_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_grids(ndomain_sph, comm_rtm)
!
      use m_sph_trans_comm_table
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use copy_sph_comm_table_4_type
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rtm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rtm_grids(ip_rank)
        call copy_comm_rtm_num_to_type(comm_rtm(ip))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans(ip_rank)
 
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &          ip_rank, ' is done.'
      end do
      call bcast_comm_stacks_sph(ndomain_sph, comm_rtm)
!
      end subroutine para_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_modes(ndomain_sph, comm_rlm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rj_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical modes for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rj_modes(ip_rank, ndomain_sph, comm_rlm)
      end do
      call deallocate_rj_1d_local_idx
!
      end subroutine para_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtp_grids(ndomain_sph, comm_rtm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm)
      end do
      call deallocate_rtp_1d_local_idx
!
      end subroutine para_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_fem_mesh_for_sph(ndomain_sph)
!
      use m_gauss_points
      use m_group_data_sph_specr
      use m_sph_mesh_1d_connect
      use const_1d_ele_connect_4_sph
      use set_local_index_table_sph
      use set_sph_groups
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_excluding_FEM_mesh .gt. 0) return
!
      call allocate_gauss_points(nidx_global_rtp(2))
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call s_const_1d_ele_connect_4_sph
      call set_rj_radial_grp
!
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct FEM mesh for domain ', ip_rank,           &
     &             ' on ', my_rank
!
        call const_fem_mesh_for_sph(ip_rank)
      end do
!
      call deallocate_rj_r_grp_item
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine para_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_comm_stacks_sph(ndomain_sph, comm_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_sph(ndomain_sph)
!
      integer(kind = kint) :: ip, iroot
      integer(kind = kint) :: iflag, i
      type(sph_comm_tbl) :: comm_tmp
!
!
!      if(i_debug .gt. 0) write(*,*) 'barrier', my_rank
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'barrier finished'
!
      call allocate_nneib_sph_rtm_tmp(ndomain_sph)
      do ip = 1, ndomain_sph
        if(mod(ip-1,nprocs) .eq. my_rank) then
          nneib_rtm_lc(ip) = comm_sph(ip)%nneib_domain
        end if
      end do
!
      call MPI_allREDUCE(nneib_rtm_lc(1), nneib_rtm_gl(1),              &
     &      ndomain_sph, CALYPSO_INTEGER, MPI_SUM,                      &
     &      CALYPSO_COMM, ierr_MPI)
!
      do ip = 1, ndomain_sph
        iroot = mod(ip-1,nprocs)
        comm_tmp%nneib_domain = nneib_rtm_gl(ip)
        call alloc_type_sph_comm_stack(comm_tmp)
!
        if(iroot .eq. my_rank) then
          comm_tmp%id_domain(1:comm_sph(ip)%nneib_domain)               &
     &       = comm_sph(ip)%id_domain(1:comm_sph(ip)%nneib_domain)
          comm_tmp%istack_sr(0:comm_sph(ip)%nneib_domain)               &
     &       = comm_sph(ip)%istack_sr(0:comm_sph(ip)%nneib_domain)
        end if
!
        call MPI_Bcast(comm_tmp%id_domain(1), comm_tmp%nneib_domain,    &
     &      CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(comm_tmp%istack_sr(1), comm_tmp%nneib_domain,    &
     &      CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
!
        iflag = 0
        do i = 1, comm_tmp%nneib_domain
          if(mod(comm_tmp%id_domain(i),nprocs) .eq. my_rank) then
            iflag = 1
            exit
          end if
        end do
!
        if(iflag .eq. 0) then
          comm_sph(ip)%nneib_domain = 0
        else if(iroot .ne. my_rank) then
!          write(*,*) 'allocate rtm:', my_rank, ip
          comm_sph(ip)%nneib_domain = comm_tmp%nneib_domain
          call alloc_type_sph_comm_stack(comm_sph(ip))
          comm_sph(ip)%id_domain(1:comm_sph(ip)%nneib_domain)           &
     &       = comm_tmp%id_domain(1:comm_sph(ip)%nneib_domain)
          comm_sph(ip)%istack_sr(0:comm_sph(ip)%nneib_domain)           &
     &       = comm_tmp%istack_sr(0:comm_sph(ip)%nneib_domain)
        end if
!
        call dealloc_type_sph_comm_stack(comm_tmp)
      end do
      call deallocate_nneib_sph_rtm_tmp
!
!      do ip = 1, ndomain_sph
!        write(50+my_rank,*) 'ip', ip
!        write(50+my_rank,*) 'nneib_domain', comm_sph(ip)%nneib_domain
!        write(50+my_rank,*) 'id_domain', comm_sph(ip)%id_domain
!      end do
!
      end subroutine bcast_comm_stacks_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_comm_stacks_sph(ndomain_sph, comm_rtm)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip, iflag, i, irank_tgt
!
!
      do ip = 1, ndomain_sph
        iflag = 0
        do i = 1, comm_rtm(ip)%nneib_domain
          irank_tgt = comm_rtm(ip)%id_domain(i)
          if(mod(irank_tgt,nprocs) .eq. my_rank) then
            iflag = 1
            exit
          end if
        end do
!
        if(iflag .gt. 0) then
!          write(*,*) 'deallocate rtm:', my_rank, ip
          call dealloc_type_sph_comm_stack(comm_rtm(ip))
          comm_rtm(ip)%nneib_domain = 0
        end if
      end do
!
      end subroutine dealloc_comm_stacks_sph
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
