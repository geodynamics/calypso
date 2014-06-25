!
!      module m_rms_4_sph_spectr
!
!     Written by H. Matsui on Feb., 2008
!
!!      subroutine allocate_rms_name_sph_spec
!!      subroutine allocate_rms_4_sph_spectr
!!      subroutine deallocate_rms_4_sph_spectr
!!
!!      subroutine clear_rms_sph_spectr
!
      module m_rms_4_sph_spectr
!
      use m_precision
!
      implicit none
!
!
      integer (kind=kint) :: num_rms_rj
      integer (kind=kint) :: ntot_rms_rj
      integer (kind=kint), allocatable :: ifield_rms_rj(:)
      integer (kind=kint), allocatable :: num_rms_comp_rj(:)
      integer (kind=kint), allocatable :: istack_rms_comp_rj(:)
      character (len=kchara), allocatable :: rms_name_rj(:)
!
      integer (kind=kint) :: nri_rms
      real(kind = kreal), allocatable :: rms_sph_l(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_m(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_lm(:,:,:)
      real(kind = kreal), allocatable :: rms_sph(:,:)
!
      real(kind = kreal), allocatable :: rms_sph_vol_l(:,:)
      real(kind = kreal), allocatable :: rms_sph_vol_m(:,:)
      real(kind = kreal), allocatable :: rms_sph_vol_lm(:,:)
      real(kind = kreal), allocatable :: rms_sph_vol(:)
!
!
      real(kind = kreal), allocatable :: ave_sph_vol(:)
!
!    output flag
!
      integer(kind = kint) :: iflag_layer_rms_spec =  0
      integer(kind = kint) :: iflag_volume_rms_spec = 0
      integer(kind = kint) :: iflag_volume_ave_sph =  0
!
!      data file name
!
      character(len = kchara) :: fhead_ave_vol =    'sph_ave_volume'
      character(len = kchara) :: fhead_rms_vol =    'sph_pwr_volume'
      character(len = kchara) :: fhead_rms_layer =  'sph_pwr_layer'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_name_sph_spec
!
      allocate(ifield_rms_rj(num_rms_rj))
      allocate(num_rms_comp_rj(num_rms_rj))
      allocate(istack_rms_comp_rj(0:num_rms_rj))
      allocate(rms_name_rj(num_rms_rj))
!
      if (num_rms_rj .gt. 0) then
        num_rms_comp_rj = 0
        ifield_rms_rj =   0
      end if
      istack_rms_comp_rj = 0
!
      end subroutine allocate_rms_name_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_4_sph_spectr
!
      use m_spheric_parameter
!
!
      nri_rms =  nidx_rj(1) 
      allocate( rms_sph_l(0:nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_m(0:nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_lm(0:nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph(0:nri_rms,ntot_rms_rj) )
!
      allocate( rms_sph_vol_l(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol_m(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol_lm(0:l_truncation,ntot_rms_rj) )
!
      allocate( rms_sph_vol(ntot_rms_rj) )
      allocate( ave_sph_vol(ntot_rms_rj) )
!
      call clear_rms_sph_spectr
!
      end subroutine allocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_4_sph_spectr
!
!
      deallocate( rms_sph_l, rms_sph_m, rms_sph_lm)
      deallocate( rms_sph )
!
      deallocate( rms_sph_vol_l, rms_sph_vol_m, rms_sph_vol_lm )
      deallocate( rms_sph_vol )
!
      deallocate( ave_sph_vol )
!
      deallocate(num_rms_comp_rj, istack_rms_comp_rj)
      deallocate(rms_name_rj, ifield_rms_rj)
!
      end subroutine deallocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_rms_sph_spectr
!
      rms_sph_l =  0.0d0
      rms_sph_m =  0.0d0
      rms_sph_lm = 0.0d0
      rms_sph = 0.0d0
!
      rms_sph_vol_l = 0.0d0
      rms_sph_vol_m = 0.0d0
      rms_sph_vol_lm = 0.0d0
      rms_sph_vol =    0.0d0
!
      end subroutine clear_rms_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module m_rms_4_sph_spectr
