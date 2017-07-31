!>@file   m_cal_max_indices.f90
!!@brief  module m_cal_max_indices
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Aug., 2007
!
!>@brief  Find node positions of maximum values
!!
!!@verbatim
!!      subroutine allocate_phys_range(ncomp_viz)
!!      subroutine deallocate_phys_range
!!      subroutine cal_max_indices(node, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: nod_fld
!!@endverbatim
!
      module m_cal_max_indices
!
      use m_precision
      use t_cal_max_indices
!
      implicit  none
!
      type(maximum_informations), save :: range
!
      end module m_cal_max_indices
