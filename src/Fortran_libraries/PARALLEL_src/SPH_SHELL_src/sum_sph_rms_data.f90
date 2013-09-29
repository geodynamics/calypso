!>@file   sum_sph_rms_data.f90
!!@brief      module sum_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine deallocate_rms_sph_local_data
!!      subroutine set_sum_table_4_sph_spectr
!!      subroutine sum_sph_layerd_rms
!!@endverbatim
!
      module sum_sph_rms_data
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), allocatable :: num_mode_sum_l(:)
      integer(kind = kint), allocatable :: num_mode_sum_m(:)
      integer(kind = kint), allocatable :: num_mode_sum_lm(:)
      integer(kind = kint), allocatable :: istack_mode_sum_l(:)
      integer(kind = kint), allocatable :: istack_mode_sum_m(:)
      integer(kind = kint), allocatable :: istack_mode_sum_lm(:)
!
      integer(kind = kint), allocatable :: item_mode_sum_l(:)
      integer(kind = kint), allocatable :: item_mode_sum_m(:)
      integer(kind = kint), allocatable :: item_mode_sum_lm(:)
!
      real(kind = kreal), allocatable :: rms_sph_l_local(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_m_local(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_lm_local(:,:,:)
!
      private :: num_mode_sum_l,  istack_mode_sum_l,  item_mode_sum_l
      private :: num_mode_sum_m,  istack_mode_sum_m,  item_mode_sum_m
      private :: num_mode_sum_lm, istack_mode_sum_lm, item_mode_sum_lm
!
      private :: rms_sph_l_local, rms_sph_m_local, rms_sph_lm_local
!
      private :: allocate_rms_sph_local_data
      private :: sum_sph_rms_by_degree
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_sph_local_data
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: num
!
!
      num = nidx_rj(1)
      allocate( rms_sph_l_local(ntot_rms_rj,0:l_truncation,num) )
      allocate( rms_sph_m_local(ntot_rms_rj,0:l_truncation,num) )
      allocate( rms_sph_lm_local(ntot_rms_rj,0:l_truncation,num) )
      rms_sph_l_local = 0.0d0
      rms_sph_m_local = 0.0d0
      rms_sph_lm_local = 0.0d0
!
!
      num = nidx_rj(2)
      allocate( num_mode_sum_l(0:l_truncation) )
      allocate( num_mode_sum_m(0:l_truncation) )
      allocate( num_mode_sum_lm(0:l_truncation) )
      allocate( istack_mode_sum_l(-1:l_truncation) )
      allocate( istack_mode_sum_m(-1:l_truncation) )
      allocate( istack_mode_sum_lm(-1:l_truncation) )
      allocate( item_mode_sum_l(num) )
      allocate( item_mode_sum_m(num) )
      allocate( item_mode_sum_lm(num) )
!
      num_mode_sum_l =      0
      num_mode_sum_m =      0
      num_mode_sum_lm =     0
      istack_mode_sum_l =  -1
      istack_mode_sum_m =  -1
      istack_mode_sum_lm = -1
      item_mode_sum_l =     0
      item_mode_sum_m =     0
      item_mode_sum_lm =    0
!
      end subroutine allocate_rms_sph_local_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_sph_local_data
!
!
      deallocate( rms_sph_l_local )
      deallocate( rms_sph_m_local )
      deallocate( rms_sph_lm_local )
!
      deallocate(num_mode_sum_l,  istack_mode_sum_l,  item_mode_sum_l )
      deallocate(num_mode_sum_m,  istack_mode_sum_m,  item_mode_sum_m )
      deallocate(num_mode_sum_lm, istack_mode_sum_lm, item_mode_sum_lm)
!
      end subroutine deallocate_rms_sph_local_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sum_table_4_sph_spectr
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: j, lg, mg, lm
      integer(kind = kint) :: icou, lcou, mcou
!
!
     call allocate_rms_sph_local_data
!
      num_mode_sum_l(0:l_truncation) =  0
      num_mode_sum_m(0:l_truncation) =  0
      num_mode_sum_lm(0:l_truncation) = 0
      do j = 1, nidx_rj(2)
        lg = idx_gl_1d_rj_j(j,2)
        mg = idx_gl_1d_rj_j(j,3)
        mg = abs(mg)
        lm = lg - mg
        num_mode_sum_l(lg) =  num_mode_sum_l(lg) +  1
        num_mode_sum_m(mg) =  num_mode_sum_m(mg) +  1
        num_mode_sum_lm(lm) = num_mode_sum_lm(lm) + 1
      end do
!
      istack_mode_sum_l(-1) =  0
      istack_mode_sum_m(-1) =  0
      istack_mode_sum_lm(-1) = 0
      do lm = 0, l_truncation
        istack_mode_sum_l(lm) = istack_mode_sum_l(lm-1)                 &
     &                         + num_mode_sum_l(lm)
        istack_mode_sum_m(lm) = istack_mode_sum_m(lm-1)                 &
     &                         + num_mode_sum_m(lm)
        istack_mode_sum_lm(lm) = istack_mode_sum_lm(lm-1)               &
     &                         + num_mode_sum_lm(lm)
      end do
!
      do lm = 0, l_truncation
        lcou = istack_mode_sum_l(lm-1)
        mcou = istack_mode_sum_m(lm-1)
        icou = istack_mode_sum_lm(lm-1)
        do j = 1, nidx_rj(2)
          lg = idx_gl_1d_rj_j(j,2)
          mg = idx_gl_1d_rj_j(j,3)
          mg = abs(mg)
          if (lg .eq. lm) then
            lcou = lcou + 1
            item_mode_sum_l(lcou) = j
          end if
          if (mg .eq. lm) then
            mcou = mcou + 1
            item_mode_sum_m(mcou) = j
          end if
          if ((lg-mg) .eq. lm) then
            icou = icou + 1
            item_mode_sum_lm(icou) = j
          end if
        end do
      end do
!
      end subroutine set_sum_table_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_layerd_rms
!
      use calypso_mpi
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: num
!
!
      call sum_sph_rms_by_degree(l_truncation, nidx_rj(1), nidx_rj(2),  &
     &    idx_gl_1d_rj_r, istack_mode_sum_l, item_mode_sum_l,           &
     &    ntot_rms_rj, rms_sph_dat(1,1), rms_sph_l_local(1,0,1) )
      call sum_sph_rms_by_degree(l_truncation, nidx_rj(1), nidx_rj(2),  &
     &    idx_gl_1d_rj_r, istack_mode_sum_m, item_mode_sum_m,           &
     &    ntot_rms_rj, rms_sph_dat(1,1), rms_sph_m_local(1,0,1) )
      call sum_sph_rms_by_degree(l_truncation, nidx_rj(1), nidx_rj(2),  &
     &    idx_gl_1d_rj_r, istack_mode_sum_lm, item_mode_sum_lm,         &
     &    ntot_rms_rj, rms_sph_dat(1,1), rms_sph_lm_local(1,0,1) )
!
!
      num = ntot_rms_rj * nidx_rj(1) * (l_truncation + 1)
      call MPI_allREDUCE (rms_sph_l_local(1,0,1), rms_sph_l(1,0,1),     &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (rms_sph_m_local(1,0,1), rms_sph_m(1,0,1),     &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (rms_sph_lm_local(1,0,1), rms_sph_lm(1,0,1),   &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .gt. 0) return
!
      call sum_sph_rms_all_modes(l_truncation, nidx_rj(1), ntot_rms_rj, &
     &    rms_sph_l(0,1,1), rms_sph(1,1) )
!
      end subroutine sum_sph_layerd_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_rms_by_degree(ltr, nidx_r, nidx_j,             &
     &          idx_gl_1d_rj_r, istack_sum, item_mode_4_sum,            &
     &          ntot_rms, rms_sph_dat, rms_sph_lc)
!
      integer(kind = kint), intent(in) :: ltr, nidx_r, nidx_j
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_r(nidx_r)
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
      integer(kind = kint), intent(in) :: ntot_rms
!
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_dat(ntot_rms,nidx_j,nidx_r)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_sph_lc(ntot_rms,0:ltr,nidx_r)
!
      integer(kind = kint) :: lm, k, j, kg, l0
      integer(kind = kint) :: lst, led
!
!
!$omp parallel do private(lm,lst,led,k,j,kg)
      do k = 1, nidx_r
        kg = idx_gl_1d_rj_r(k)
        rms_sph_lc(1:ntot_rms,0:ltr,kg) = 0.0d0
!
        do lm = 0, ltr
          lst = istack_sum(lm-1) + 1
          led = istack_sum(lm)
          do l0 = lst, led
            j = item_mode_4_sum(l0)
!
            rms_sph_lc(1:ntot_rms,lm,kg) = rms_sph_lc(1:ntot_rms,lm,kg) &
     &                                  + rms_sph_dat(1:ntot_rms,j,kg)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_rms_by_degree
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_rms_all_modes(ltr, nidx_r, ntot_rms,           &
     &          rms_sph_l, rms_sph)
!
      integer(kind = kint), intent(in) :: ltr, nidx_r
      integer(kind = kint), intent(in) :: ntot_rms
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_l(ntot_rms,0:ltr,nidx_r)
!
      real(kind = kreal), intent(inout) :: rms_sph(ntot_rms,nidx_r)
!
      integer(kind = kint) :: lm, kg, nd
!
!
!$omp parallel private(lm,kg,nd)
      do kg = 1, nidx_r
!$omp do
        do nd = 1, ntot_rms
          rms_sph(nd,kg) = rms_sph_l(nd,0,kg)
        end do
!$omp end do nowait
!
        do lm = 1, ltr
!$omp do
          do nd = 1, ntot_rms
            rms_sph(nd,kg) = rms_sph(nd,kg) + rms_sph_l(nd,lm,kg)
          end do
!$omp end do nowait
        end do
!
      end do
!$omp end parallel
!
      end subroutine sum_sph_rms_all_modes
!
! -----------------------------------------------------------------------
!
      end module sum_sph_rms_data
