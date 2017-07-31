!>@file   t_sum_sph_rms_data.f90
!!@brief      module t_sum_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine allocate_rms_sph_local_data                          &
!!     &         (ltr, nidx_rj, n_spectr, nri_rms, ntot_comp_sq, WK_pwr)
!!      subroutine deallocate_rms_sph_local_data(WK_pwr)
!!@endverbatim
!
      module t_sum_sph_rms_data
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Structure of work area for mean square data
      type sph_mean_square_work
        real(kind = kreal), allocatable :: shl_rj(:,:,:)
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
        real(kind = kreal), allocatable :: shl_l_local(:,:,:)
        real(kind = kreal), allocatable :: shl_m_local(:,:,:)
        real(kind = kreal), allocatable :: shl_lm_local(:,:,:)
!
        real(kind = kreal), allocatable :: volume_j(:,:)
!
        real(kind = kreal), allocatable :: vol_l_local(:,:,:)
        real(kind = kreal), allocatable :: vol_m_local(:,:,:)
        real(kind = kreal), allocatable :: vol_lm_local(:,:,:)
      end type sph_mean_square_work
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_sph_local_data                            &
     &         (ltr, nidx_rj, n_spectr, nri_rms, ntot_comp_sq, WK_pwr)
!
      integer(kind = kint), intent(in) :: n_spectr
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nri_rms, ntot_comp_sq
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: nri, jmax
!
!
      nri =  nidx_rj(1)
      jmax = nidx_rj(2)
      allocate( WK_pwr%shl_rj(0:nri,jmax,3) )
      WK_pwr%shl_rj = 0.0d0
!
      allocate( WK_pwr%volume_j(jmax,3) )
      WK_pwr%volume_j = 0.0d0
!
      allocate(WK_pwr%shl_l_local(nri_rms,0:ltr,ntot_comp_sq) )
      allocate(WK_pwr%shl_m_local(nri_rms,0:ltr,ntot_comp_sq) )
      allocate(WK_pwr%shl_lm_local(nri_rms,0:ltr,ntot_comp_sq))
      if(ntot_comp_sq .gt. 0) then
        WK_pwr%shl_l_local = 0.0d0
        WK_pwr%shl_m_local = 0.0d0
        WK_pwr%shl_lm_local = 0.0d0
      end if
!
      allocate( WK_pwr%vol_l_local(0:ltr,ntot_comp_sq,n_spectr) )
      allocate( WK_pwr%vol_m_local(0:ltr,ntot_comp_sq,n_spectr) )
      allocate( WK_pwr%vol_lm_local(0:ltr,ntot_comp_sq,n_spectr) )
      if(ntot_comp_sq .gt. 0) then
        WK_pwr%vol_l_local = 0.0d0
        WK_pwr%vol_m_local = 0.0d0
        WK_pwr%vol_lm_local = 0.0d0
      end if
!
      allocate( WK_pwr%num_mode_sum_l(0:ltr) )
      allocate( WK_pwr%num_mode_sum_m(0:ltr) )
      allocate( WK_pwr%num_mode_sum_lm(0:ltr) )
      allocate( WK_pwr%istack_mode_sum_l(-1:ltr) )
      allocate( WK_pwr%istack_mode_sum_m(-1:ltr) )
      allocate( WK_pwr%istack_mode_sum_lm(-1:ltr) )
      allocate( WK_pwr%item_mode_sum_l(jmax) )
      allocate( WK_pwr%item_mode_sum_m(jmax) )
      allocate( WK_pwr%item_mode_sum_lm(jmax) )
!
      WK_pwr%num_mode_sum_l =      0
      WK_pwr%num_mode_sum_m =      0
      WK_pwr%num_mode_sum_lm =     0
      WK_pwr%istack_mode_sum_l =   0
      WK_pwr%istack_mode_sum_m =   0
      WK_pwr%istack_mode_sum_lm =  0
      WK_pwr%item_mode_sum_l =     0
      WK_pwr%item_mode_sum_m =     0
      WK_pwr%item_mode_sum_lm =    0
!
      end subroutine allocate_rms_sph_local_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_sph_local_data(WK_pwr)
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      deallocate(WK_pwr%shl_rj, WK_pwr%volume_j)
      deallocate(WK_pwr%shl_l_local,  WK_pwr%vol_l_local)
      deallocate(WK_pwr%shl_m_local,  WK_pwr%vol_m_local)
      deallocate(WK_pwr%shl_lm_local, WK_pwr%vol_lm_local)
!
      deallocate(WK_pwr%num_mode_sum_l,  WK_pwr%istack_mode_sum_l)
      deallocate(WK_pwr%item_mode_sum_l)
      deallocate(WK_pwr%num_mode_sum_m,  WK_pwr%istack_mode_sum_m)
      deallocate(WK_pwr%item_mode_sum_m)
      deallocate(WK_pwr%num_mode_sum_lm, WK_pwr%istack_mode_sum_lm)
      deallocate(WK_pwr%item_mode_sum_lm)
!
      end subroutine deallocate_rms_sph_local_data
!
! -----------------------------------------------------------------------
!
      end module t_sum_sph_rms_data
