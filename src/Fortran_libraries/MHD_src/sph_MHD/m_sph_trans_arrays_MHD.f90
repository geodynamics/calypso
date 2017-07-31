!>@file   t_sph_trans_arrays_MHD.f90
!!@brief  module t_sph_trans_arrays_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!!@verbatim
!!      subroutine alloc_sph_trans_address(sph_rtp, WK)
!!      subroutine dealloc_sph_trans_address(WK)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!@endverbatim
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
      module m_sph_trans_arrays_MHD
!
      use t_sph_trans_arrays_MHD
!
!
!>        strucutres for spherical transform dor MHD dynamo
      type(works_4_sph_trans_MHD), save :: trns_WK1
!
      end module m_sph_trans_arrays_MHD
