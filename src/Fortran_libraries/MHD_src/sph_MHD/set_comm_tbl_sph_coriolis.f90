!>@file   set_comm_tbl_sph_coriolis.f90
!!@brief  module set_comm_tbl_sph_coriolis
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Apr., 2010
!
!>@brief Constract communication table to evaluate Coriolis term
!!
!!@verbatim
!!      subroutine set_sr_address_4_coriolis
!!      subroutine const_integral_sph_coriolis
!!@endverbatim
!
      module set_comm_tbl_sph_coriolis
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
!>      Range of local mode in each subdomain to evaluate Coriolis term
      integer(kind = kint), allocatable :: jminmax_coriolis(:,:)
!>      Range of global mode in each subdomain to evaluate Coriolis term
      integer(kind = kint), allocatable :: jminmax_gl(:,:)
!
!>      Start harmonics mode address to send
      integer(kind = kint), allocatable :: ist_send_cor_j(:)
!>      End harmonics mode address to send
      integer(kind = kint), allocatable :: ied_send_cor_j(:)
!>      Start harmonics mode address to recieve
      integer(kind = kint), allocatable :: ist_recv_cor_j(:)
!>      End harmonics mode address to recieve
      integer(kind = kint), allocatable :: ied_recv_cor_j(:)
!
!>      Send flags for Coriolis term
      integer(kind = kint), allocatable :: iflag_send_cor(:)
!>      Recieve flags for Coriolis term
      integer(kind = kint), allocatable :: iflag_recv_cor(:)
!
      private :: alloc_comm_tbl_sph_coriolis
      private :: dealloc_comm_tbl_sph_coriolis
      private :: set_sph_range_coriolis, set_comm_sph_range_coriolis
      private :: count_neib_4_coriolis_sr, count_num_item_4_coriolis_sr
      private :: set_item_4_coriolis_sr, check_comm_tbl_sph_coriolis
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sr_address_4_coriolis
!
      use m_constants
      use m_spheric_parameter
      use m_comm_tbl_sph_coriolis
      use m_integrals_4_sph_coriolis
      use solver_sph_coriolis_sr
!
      integer(kind = kint) :: j, l
!
!
      call alloc_comm_tbl_sph_coriolis(nprocs)
!
      call set_sph_range_coriolis
      call set_comm_sph_range_coriolis
!
!      call check_comm_tbl_sph_coriolis(my_rank, nprocs)
!
!
      call count_neib_4_coriolis_sr
      call alloc_comm_neib_sph_coriolis
!
      call count_num_item_4_coriolis_sr
      call alloc_comm_item_sph_coriolis
!
      call set_item_4_coriolis_sr
!      call check_comm_tbl_sph_cor(my_rank)
!
!
      call alloc_comm_work_sph_coriolis( nidx_rj(1) )
      call allocate_sph_coriolis_data( nidx_rj(1), nidx_rj(2) )
!
      call solver_sph_coriolis_sr_int(nshift_j_cor, nidx_rj(2),         &
     &    idx_gl_1d_rj_j(1,1), nidx_j_cor, idx_gl_cor_j(1,1))
      call solver_sph_coriolis_sr_int(nshift_j_cor, nidx_rj(2),         &
     &    idx_gl_1d_rj_j(1,2), nidx_j_cor, idx_gl_cor_j(1,2))
      call solver_sph_coriolis_sr_int(nshift_j_cor, nidx_rj(2),         &
     &    idx_gl_1d_rj_j(1,3), nidx_j_cor, idx_gl_cor_j(1,3))
!
      do j = 1, nidx_j_cor
        l = idx_gl_cor_j(j,2)
        g_cor_j(j,3) = real(l*(l+1))
      end do
!
!
!      if(i_debug .gt. iflag_full_msg) then
!        write(50+my_rank,*) 'jminmax_gl: ',                             &
!     &      jminmax_gl(1:2,my_rank+1)
!        write(50+my_rank,*) 'jminmax_coriolis: ',                       &
!     &      jminmax_coriolis(1:2,my_rank+1)
!        write(50+my_rank,*) 'nshift_j_cor: ', nshift_j_cor
!        call check_idx_4_sph_coriolis(50+my_rank)
!      end if
!
      call dealloc_comm_tbl_sph_coriolis
!
!      call solver_sph_coriolis_sr_3(nshift_j_cor,                      &
!     &     nidx_rj(2), nidx_rj(1), d_rj(1,ipol%i_velo),                &
!     &     nidx_j_cor, u_sph_coriolis(1,1,1) )
!
      end subroutine set_sr_address_4_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine const_integral_sph_coriolis
!
      use m_spheric_parameter
      use m_integrals_4_sph_coriolis
      use set_integral_sph_coriolis
!
!
      call allocate_int_sph_coriolis( nidx_rj(2) )
!
      call copy_global_sph_id_4_sph_cor(nidx_rj(2),                     &
     &    idx_gl_1d_rj_j(1,1) )
!
      call s_set_integral_sph_coriolis(l_truncation, nidx_rj(2),        &
     &    idx_gl_1d_rj_j(1,1) )
!
      call set_local_sph_coriolis_address(nidx_rj(2))
!
!      call check_int_4_sph_coriolis(my_rank+50, nidx_rj(2),            &
!     &    idx_gl_1d_rj_j(1,1))
!
      end subroutine const_integral_sph_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_comm_tbl_sph_coriolis(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate( jminmax_gl(2,nprocs) )
      allocate( jminmax_coriolis(2,nprocs) )
      jminmax_gl = 0
      jminmax_coriolis = 0
!
      allocate( ist_send_cor_j(nprocs) )
      allocate( ied_send_cor_j(nprocs) )
      allocate( ist_recv_cor_j(nprocs) )
      allocate( ied_recv_cor_j(nprocs) )
      ist_send_cor_j = 0
      ied_send_cor_j = 0
      ist_recv_cor_j = 0
      ied_recv_cor_j = 0
!
      allocate( iflag_send_cor(nprocs) )
      allocate( iflag_recv_cor(nprocs) )
      iflag_send_cor = 0
      iflag_recv_cor = 0
!
      end subroutine alloc_comm_tbl_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_comm_tbl_sph_coriolis
!
!
      deallocate( jminmax_gl )
      deallocate( jminmax_coriolis )
!
      deallocate( ist_send_cor_j )
      deallocate( ied_send_cor_j )
      deallocate( ist_recv_cor_j )
      deallocate( ied_recv_cor_j )
!
      deallocate( iflag_send_cor, iflag_recv_cor )
!
      end subroutine dealloc_comm_tbl_sph_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_range_coriolis
!
      use calypso_mpi
      use m_constants
      use m_spheric_parameter
      use m_comm_tbl_sph_coriolis
      use m_integrals_4_sph_coriolis
!
      integer(kind = kint) :: ip, num, j, jgl
!
!
      ip = my_rank + 1
      num = nidx_rj(2)
      jminmax_gl(1,ip) = idx_gl_1d_rj_j(1,  1)
      jminmax_gl(2,ip) = idx_gl_1d_rj_j(num,1)
      jminmax_coriolis(1,ip) = jminmax_gl(1,ip)
      jminmax_coriolis(2,ip) = jminmax_gl(2,ip)
!
      do j = 1, num
        jgl = idx_gl_1d_rj_j(j,1)
        if(jgl .eq. 0) then
          jminmax_coriolis(1,ip) = min(jminmax_coriolis(1,ip),0)
          jminmax_coriolis(2,ip) = max(jminmax_coriolis(1,ip),0)
        else
          jminmax_coriolis(1,ip) = min(jminmax_coriolis(1,ip),          &
     &                            jgl_kcor(jgl,1,2), jgl_kcor(jgl,2,2), &
     &                            jgl_kcor(jgl,1,1), jgl_kcor(jgl,2,1), &
     &                            jgl_kcor(jgl,3,1), jgl_kcor(jgl,4,1), &
     &                            jgl_kcor(jgl,1,3), jgl_kcor(jgl,2,3), &
     &                            jgl_kcor(jgl,3,3), jgl_kcor(jgl,4,3), &
     &                            jgl_lcor(jgl,1,1), jgl_lcor(jgl,2,1), &
     &                            jgl_lcor(jgl,1,3), jgl_lcor(jgl,2,3), &
     &                            jgl_lcor(jgl,1,2) )
          jminmax_coriolis(2,ip) = max(jminmax_coriolis(2,ip),          &
     &                            jgl_kcor(jgl,1,2), jgl_kcor(jgl,2,2), &
     &                            jgl_kcor(jgl,1,1), jgl_kcor(jgl,2,1), &
     &                            jgl_kcor(jgl,3,1), jgl_kcor(jgl,4,1), &
     &                            jgl_kcor(jgl,1,3), jgl_kcor(jgl,2,3), &
     &                            jgl_kcor(jgl,3,3), jgl_kcor(jgl,4,3), &
     &                            jgl_lcor(jgl,1,1), jgl_lcor(jgl,2,1), &
     &                            jgl_lcor(jgl,1,3), jgl_lcor(jgl,2,3), &
     &                            jgl_lcor(jgl,1,2) )
        end if
      end do
      nidx_j_cor = jminmax_coriolis(2,ip) - jminmax_coriolis(1,ip) + 1
      nshift_j_cor = jminmax_gl(1,ip) - jminmax_coriolis(1,ip)
!
!
      do ip = 1, nprocs
        call MPI_Bcast(jminmax_gl(1,ip), itwo, CALYPSO_INTEGER, (ip-1), &
     &      CALYPSO_COMM, ierr)
        call MPI_Bcast(jminmax_coriolis(1,ip), itwo, CALYPSO_INTEGER,   &
     &      (ip-1), CALYPSO_COMM, ierr)
      end do
!
      end subroutine set_sph_range_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_sph_range_coriolis
!
      use m_constants
      use m_spheric_parameter
      use m_comm_tbl_sph_coriolis
!
      integer(kind = kint) :: ip, num
!
!
      iflag_recv_cor = 0
      iflag_send_cor = 0
      do ip = 1, nprocs
        if(    jminmax_coriolis(1,my_rank+1).le.jminmax_gl(2,ip)        &
     &   .and. jminmax_coriolis(2,my_rank+1).ge.jminmax_gl(1,ip) ) then
         iflag_recv_cor(ip) = 1
        end if
        if(    jminmax_gl(1,my_rank+1).le.jminmax_coriolis(2,ip)        &
     &   .and. jminmax_gl(2,my_rank+1).ge.jminmax_coriolis(1,ip) ) then
         iflag_send_cor(ip) = 1
        end if
      end do
!
      do ip = 1, nprocs
        if( iflag_recv_cor(ip) .eq. 1) then
          ist_recv_cor_j(ip) = jminmax_coriolis(1,my_rank+1)            &
     &                        - jminmax_gl(1,ip) + 1
          ied_recv_cor_j(ip) = jminmax_coriolis(2,my_rank+1)            &
     &                        - jminmax_gl(1,ip) + 1
!
          num = jminmax_gl(2,ip) - jminmax_gl(1,ip) + 1
          if( ist_recv_cor_j(ip) .lt. 1) ist_recv_cor_j(ip) = 1
          if( ied_recv_cor_j(ip) .gt. num) then
            ied_recv_cor_j(ip) = jminmax_gl(2,ip) - jminmax_gl(1,ip) +1
          end if
        else
          ist_recv_cor_j(ip) = 0
          ied_recv_cor_j(ip) = 0
        end if
!
        if( iflag_send_cor(ip) .eq. 1) then
          ist_send_cor_j(ip) =  jminmax_coriolis(1,ip)                  &
     &                        - jminmax_gl(1,my_rank+1) + 1
          ied_send_cor_j(ip) =  jminmax_coriolis(2,ip)                  &
     &                        - jminmax_gl(1,my_rank+1) + 1
          if( ist_send_cor_j(ip) .lt. 1) ist_send_cor_j(ip) = 1
          if( ied_send_cor_j(ip) .gt. nidx_rj(2)) then
            ied_send_cor_j(ip) = nidx_rj(2)
          end if
        else
          ist_send_cor_j(ip) = 0
          ied_send_cor_j(ip) = 0
        end if
      end do
!
      end subroutine set_comm_sph_range_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_neib_4_coriolis_sr
!
      use m_comm_tbl_sph_coriolis
!
      integer(kind = kint) :: ip
!
!
      nneib_send_cor = -1
      nneib_recv_cor = -1
      do ip = 1, nprocs
        nneib_send_cor = nneib_send_cor + iflag_send_cor(ip)
        nneib_recv_cor = nneib_recv_cor + iflag_recv_cor(ip)
      end do
!
      end subroutine count_neib_4_coriolis_sr
!
! -----------------------------------------------------------------------
!
      subroutine count_num_item_4_coriolis_sr
!
      use m_comm_tbl_sph_coriolis
!
      integer(kind = kint) :: ip, icou, jcou
!
!
      icou = 0
      jcou = 0
      do ip = 1, nprocs
        if( (ip-1).ne.my_rank .and. iflag_send_cor(ip).gt.0) then
          icou = icou + 1
          ip_send_cor(icou) =  ip - 1
          istack_send_cor(icou) = istack_send_cor(icou-1)               &
     &           + ied_send_cor_j(ip) - ist_send_cor_j(ip) + 1
        end if
        if( (ip-1).ne.my_rank .and. iflag_recv_cor(ip).gt.0) then
          jcou = jcou + 1
          ip_recv_cor(jcou) = ip - 1
          istack_recv_cor(jcou) = istack_recv_cor(jcou-1)               &
     &           + ied_recv_cor_j(ip) - ist_recv_cor_j(ip) + 1
        end if
      end do
      ntot_send_cor =  istack_send_cor(nneib_send_cor)
      ntot_recv_cor = istack_recv_cor(nneib_recv_cor)
!
!
      end subroutine count_num_item_4_coriolis_sr
!
! -----------------------------------------------------------------------
!
      subroutine set_item_4_coriolis_sr
!
      use m_comm_tbl_sph_coriolis
      use m_integrals_4_sph_coriolis
!
      integer(kind = kint) :: ip, num, inum
      integer(kind = kint) :: jcou, i, j
!
!
      jcou = 0
      do i = 1, nneib_send_cor
        ip = ip_send_cor(i) + 1
        num = istack_send_cor(i) - istack_send_cor(i-1)
        do inum = 1, num
          j = istack_send_cor(i-1) + inum
          idx_send_cor(j) = ist_send_cor_j(ip) + inum - 1
        end do
      end do
!
      do i = 1, nneib_recv_cor
        ip = ip_recv_cor(i) + 1
        num = istack_recv_cor(i) - istack_recv_cor(i-1)
        do inum = 1, num
          j = istack_recv_cor(i-1) + inum
          idx_recv_cor(j)                                               &
     &              = jminmax_gl(1,ip) - jminmax_coriolis(1,my_rank+1)  &
     &               + ist_recv_cor_j(ip) + inum - 1
        end do
      end do
!
      end subroutine set_item_4_coriolis_sr
!
! -----------------------------------------------------------------------
!
      subroutine check_comm_tbl_sph_coriolis(my_rank, nprocs)
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      integer(kind = kint) :: ip
!
!
      write(50+my_rank,'(a)')                                           &
     &       'PE_ID, jmin_gl, jmax_gl, jmin_coriolis, jmax_coriolis'
      do ip = 1, nprocs
        write(50+my_rank,*) (ip-1), jminmax_gl(1:2,ip),                 &
     &                      jminmax_coriolis(1:2,ip)
      end do
!
!
      write(50+my_rank,*) jminmax_gl(1:2,my_rank+1)
      write(50+my_rank,'(a)')                                           &
     &       'PE_ID, ist_send_cor_j, ied_send_cor_j'
      do ip = 1, nprocs
        write(50+my_rank,*) (ip-1), ist_send_cor_j(ip),                 &
     &                  ied_send_cor_j(ip), jminmax_coriolis(1:2,ip)
      end do
!
      write(50+my_rank,*) jminmax_coriolis(1:2,my_rank+1)
      write(50+my_rank,'(a)')                                           &
     &       'PE_ID, ist_recv_cor_j, ied_recv_cor_j'
      do ip = 1, nprocs
        write(50+my_rank,*) (ip-1), ist_recv_cor_j(ip),                 &
     &        ied_recv_cor_j(ip), jminmax_gl(1:2,ip)
      end do
!
      end subroutine check_comm_tbl_sph_coriolis
!
! -----------------------------------------------------------------------
!
      end module set_comm_tbl_sph_coriolis
