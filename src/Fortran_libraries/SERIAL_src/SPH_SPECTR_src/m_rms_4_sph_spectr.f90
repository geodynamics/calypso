!>@file   m_rms_4_sph_spectr.f90
!!@brief  module m_rms_4_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
      module m_rms_4_sph_spectr
!
      use m_precision
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      implicit none
!
!>      Structure of mean square data
      type(sph_mean_squares), save :: pwr1
!
      type(sph_mean_square_work), save :: WK_pwr
!
      end module m_rms_4_sph_spectr
