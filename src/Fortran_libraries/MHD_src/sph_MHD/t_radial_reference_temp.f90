!>@file   t_radial_reference_temp.f90
!!@brief  module t_radial_reference_temp
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!        subroutine alloc_reft_rj_data(nri_rj, ref_temp)
!!        subroutine dealloc_reft_rj_data(ref_temp)
!!
!!        ref_temp%t_rj(kr,0) ... T_0
!!        ref_temp%t_rj(kr,1) ... d T_0 / dr
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module t_radial_reference_temp
!
      use m_precision
!
      implicit  none
!
!>      Structure of reference temperature
      type reference_temperature
!>      Number of radial points for reference temperature
        integer(kind = kint) :: nri_reft_rj
!
!>    reference temerature spectr @f$ f(r,j) @f$
        real (kind=kreal), allocatable :: t_rj(:,:)
      end type reference_temperature
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_reft_rj_data(nri_rj, ref_temp)
!
      integer(kind = kint), intent(in) :: nri_rj
      type(reference_temperature), intent(inout) :: ref_temp
!
      ref_temp%nri_reft_rj = nri_rj
      allocate( ref_temp%t_rj(ref_temp%nri_reft_rj,0:2))
      ref_temp%t_rj =  0.0d0
!
      end subroutine alloc_reft_rj_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_reft_rj_data(ref_temp)
!
      type(reference_temperature), intent(inout) :: ref_temp
!
      deallocate( ref_temp%t_rj )
!
      end subroutine dealloc_reft_rj_data
!
!  --------------------------------------------------------------------
!
      end module t_radial_reference_temp
