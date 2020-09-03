!>@file   bcast_comm_stacks_sph.f90
!!@brief  module bcast_comm_stacks_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Distribute number of data for communication
!!
!!@verbatim
!!      subroutine mpi_bcast_comm_stacks_sph(ndomain_sph, comm_sph)
!!      subroutine para_bcast_comm_stacks_sph(ndomain_sph, comm_sph)
!!        type(sph_comm_tbl), intent(inout) :: comm_sph(ndomain_sph)
!!      subroutine dealloc_comm_stacks_sph(ndomain_sph, comm_rtm)
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
!!@endverbatim
!
      module bcast_comm_stacks_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_sph_trans_comm_tbl
!
      implicit none
!
      integer(kind = kint), allocatable, private :: nneib_rtm_lc(:)
      integer(kind = kint), allocatable, private :: nneib_rtm_gl(:)
!
      private :: set_bcast_comm_stacks_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mpi_bcast_comm_stacks_sph(ndomain_sph, comm_sph)
!
      use calypso_mpi_int
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_sph(ndomain_sph)
!
!      integer(kind = kint) :: ip
!
!
      call calypso_mpi_barrier
!      if(my_rank .eq. 0) write(*,*) 'barrier finished'
!
      allocate(nneib_rtm_gl(ndomain_sph))
      call calypso_mpi_allgather_one_int                                &
     &  (comm_sph(my_rank+1)%nneib_domain, nneib_rtm_gl)
!
      call set_bcast_comm_stacks_sph                                    &
     &   (ndomain_sph, nneib_rtm_gl, comm_sph)
      deallocate(nneib_rtm_gl)
!
!      do ip = 1, ndomain_sph
!        write(50+my_rank,*) 'ip', ip
!        write(50+my_rank,*) 'nneib_domain', comm_sph(ip)%nneib_domain
!        write(50+my_rank,*) 'id_domain', comm_sph(ip)%id_domain
!      end do
!
      end subroutine mpi_bcast_comm_stacks_sph
!
! ----------------------------------------------------------------------
!
      subroutine para_bcast_comm_stacks_sph(ndomain_sph, comm_sph)
!
      use calypso_mpi_int
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_sph(ndomain_sph)
!
      integer(kind = kint) :: i, ip, jp, num_tmp
!
!
      call calypso_mpi_barrier
!      if(my_rank .eq. 0) write(*,*) 'barrier finished'
!
      allocate(nneib_rtm_gl(ndomain_sph))
!
!$omp parallel workshare
      nneib_rtm_gl(1:ndomain_sph) = 0
!$omp end parallel workshare
!
      allocate(nneib_rtm_lc(nprocs))
      nneib_rtm_gl(1:nprocs) = 0
!
      do i = 0, (ndomain_sph-1)/nprocs
        jp = 1 + my_rank + i * nprocs
        if(jp .le. ndomain_sph) then
          num_tmp = comm_sph(jp)%nneib_domain
        else
          num_tmp = 0
        end if
!
        call calypso_mpi_allgather_one_int(num_tmp, nneib_rtm_lc(1))
!
        do ip = 1, nprocs
          jp = ip + i * nprocs
          if(jp .le. ndomain_sph) nneib_rtm_gl(jp) = nneib_rtm_lc(ip)
        end do
      end do
      call deallocate_nneib_rtm_lc
!
      call set_bcast_comm_stacks_sph                                    &
     &   (ndomain_sph, nneib_rtm_gl, comm_sph)
      deallocate(nneib_rtm_gl)
!
!      do ip = 1, ndomain_sph
!        write(50+my_rank,*) 'ip', ip
!        write(50+my_rank,*) 'nneib_domain', comm_sph(ip)%nneib_domain
!        write(50+my_rank,*) 'id_domain', comm_sph(ip)%id_domain
!      end do
!
      end subroutine para_bcast_comm_stacks_sph
!
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
      subroutine set_bcast_comm_stacks_sph                              &
     &         (ndomain_sph, nneib_rtm_tmp, comm_sph)
!
      use calypso_mpi_int
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint), intent(in) :: nneib_rtm_tmp(ndomain_sph)
      type(sph_comm_tbl), intent(inout) :: comm_sph(ndomain_sph)
!
      integer :: iroot
      integer(kind = kint) :: ip
      integer(kind = kint) :: iflag, i
      type(sph_comm_tbl) :: comm_tmp
!
!
      do ip = 1, ndomain_sph
        iroot = int(mod(ip-1,nprocs))
        comm_tmp%nneib_domain = nneib_rtm_tmp(ip)
        call alloc_sph_comm_stack(comm_tmp)
!
        if(iroot .eq. my_rank) then
          comm_tmp%istack_sr(0) = comm_sph(ip)%istack_sr(0)
          do i = 1, comm_tmp%nneib_domain
            comm_tmp%id_domain(i) = comm_sph(ip)%id_domain(i)
            comm_tmp%istack_sr(i) = comm_sph(ip)%istack_sr(i)
          end do
        end if
!
        call calypso_mpi_bcast_int(comm_tmp%id_domain(1),               &
     &      cast_long(comm_tmp%nneib_domain), iroot)
        call calypso_mpi_bcast_int(comm_tmp%istack_sr(1),               &
     &      cast_long(comm_tmp%nneib_domain), iroot)
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
          call alloc_sph_comm_stack(comm_sph(ip))
          comm_sph(ip)%id_domain(1:comm_sph(ip)%nneib_domain)           &
     &       = comm_tmp%id_domain(1:comm_sph(ip)%nneib_domain)
          comm_sph(ip)%istack_sr(0:comm_sph(ip)%nneib_domain)           &
     &       = comm_tmp%istack_sr(0:comm_sph(ip)%nneib_domain)
        end if
        call calypso_mpi_barrier
!
        call dealloc_type_sph_comm_stack(comm_tmp)
      end do
!
      end subroutine set_bcast_comm_stacks_sph
!
! ----------------------------------------------------------------------
!
      subroutine allocate_nneib_rtm_lc
      allocate(nneib_rtm_lc(nprocs))
      nneib_rtm_gl(1:nprocs) = 0
      end subroutine allocate_nneib_rtm_lc
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_nneib_rtm_lc
      deallocate(nneib_rtm_lc)
      end subroutine deallocate_nneib_rtm_lc
!
! ----------------------------------------------------------------------
!
      end module bcast_comm_stacks_sph
