!>@file   sel_fld_copy_4_sph_trans.f90
!!@brief  module sel_fld_copy_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical harmonics data
!!
!!@verbatim
!!      subroutine sel_scalar_from_trans(nnod, v_rtp, d_sph)
!!      subroutine sel_vector_from_trans(nnod, v_rtp, d_sph)
!!      subroutine sel_tensor_from_trans(nnod, v_rtp, d_sph)
!!
!!      subroutine sel_scalar_to_trans(nnod, d_sph, v_rtp)
!!      subroutine sel_vector_to_trans(nnod, d_sph, v_rtp)
!!      subroutine sel_tensor_to_trans(nnod, d_sph, v_rtp)
!!@endverbatim
!
      module sel_fld_copy_4_sph_trans
!
      use m_precision
!
      use m_FFT_selector
      use m_spheric_parameter
      use m_spheric_param_smp
      use swap_phi_4_sph_trans
      use copy_field_4_sph_trans
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine sel_scalar_from_trans(nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp)
      real(kind = kreal), intent(inout) :: d_sph(nnod)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call swap_phi_scalar_from_trans(nnod, v_rtp, d_sph)
      else
        call copy_scalar_from_trans(nnod_rtp, inod_rtp_smp_stack, nnod, &
     &      v_rtp, d_sph)
      end if
!
      end subroutine sel_scalar_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_vector_from_trans(nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp,3)
      real(kind = kreal), intent(inout) :: d_sph(nnod,3)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call swap_phi_vector_from_trans(nnod, v_rtp, d_sph)
      else
        call copy_vector_from_trans(nnod_rtp, inod_rtp_smp_stack, nnod, &
     &      v_rtp, d_sph)
      end if
!
      end subroutine sel_vector_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_tensor_from_trans(nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp,6)
      real(kind = kreal), intent(inout) :: d_sph(nnod,6)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call swap_phi_tensor_from_trans(nnod, v_rtp, d_sph)
      else
        call copy_tensor_from_trans(nnod_rtp, inod_rtp_smp_stack, nnod, &
     &      v_rtp, d_sph)
      end if
!
      end subroutine sel_tensor_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_scalar_to_trans(nnod, d_sph, v_rtp)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_sph(nnod)
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call swap_phi_scalar_to_trans(nnod, d_sph, v_rtp)
      else
        call copy_scalar_to_trans(nnod_rtp, inod_rtp_smp_stack, nnod,   &
     &      d_sph, v_rtp)
      end if
!
      end subroutine sel_scalar_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_vector_to_trans(nnod, d_sph, v_rtp)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_sph(nnod,3)
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp,3)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call swap_phi_vector_to_trans(nnod, d_sph, v_rtp)
      else
        call copy_vector_to_trans(nnod_rtp, inod_rtp_smp_stack, nnod,   &
     &      d_sph, v_rtp)
      end if
!
      end subroutine sel_vector_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_tensor_to_trans(nnod, d_sph, v_rtp)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_sph(nnod,6)
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp,6)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call swap_phi_tensor_to_trans(nnod, d_sph, v_rtp)
      else
        call copy_tensor_to_trans(nnod_rtp, inod_rtp_smp_stack, nnod,   &
     &      d_sph, v_rtp)
      end if
!
      end subroutine sel_tensor_to_trans
!
!-----------------------------------------------------------------------
!
      end module sel_fld_copy_4_sph_trans
