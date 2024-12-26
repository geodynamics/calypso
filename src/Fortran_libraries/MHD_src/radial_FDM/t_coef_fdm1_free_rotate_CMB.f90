!>@file   t_coef_fdm1_free_rotate_CMB.f90
!!@brief  module t_coef_fdm1_free_rotate_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate 1st order FDM
!!      at CMB with fixed field boundary
!!
!!@verbatim
!!      subroutine check_fdm1_CMB_fixed_field(id_file, fdm1_fix_fld_CMB)
!!        integer(kind = kint), intent(in) :: id_file
!!        real(kind = kreal), intent(in) :: fdm1_fix_fld_CMB(-1:0,2)
!!
!!      subroutine cal_fdm1_coef_fix_fld_CMB(r_from_CMB1,               &
!!     &                                     fdm1_fix_fld_CMB)
!!        real(kind = kreal), intent(in) :: r_from_CMB1(-1:0)
!!        real(kind = kreal), intent(inout) :: fdm1_fix_fld_CMB(-1:0,2)
!!
!!   Matrix for derivatives with fixed field
!!    at outer boundary of the shell
!!      dfdr =      fdm1_fix_fld_CMB( 1,2) * d_rj(CMB-1)
!!                + fdm1_fix_fld_CMB( 0,2) * d_rj(CMB  )
!!@endverbatim
!!
      module t_coef_fdm1_free_rotate_CMB
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm1_CMB_fixed_field(id_file, fdm1_fix_fld_CMB)
!
      integer(kind = kint), intent(in) :: id_file
      real(kind = kreal), intent(in) :: fdm1_fix_fld_CMB(-1:0,2)
!
      write(id_file,*) ' fdm1_fix_fld_CMB'
      write(id_file,*) ' mat_fdm22,  mat_fdm21'
      write(id_file,'(1p9E25.15e3)') fdm1_fix_fld_CMB(-1:0,2)
!
      end subroutine check_fdm1_CMB_fixed_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm1_coef_fix_fld_CMB(r_from_CMB1,                 &
     &                                     fdm1_fix_fld_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB1(-1:0)
      real(kind = kreal), intent(inout) :: fdm1_fix_fld_CMB(-1:0,2)
!
      real(kind = kreal) :: dr_n1
!
!
      dr_n1 = r_from_CMB1(0) - r_from_CMB1(-1)
!
      fdm1_fix_fld_CMB(-1,1) = zero
      fdm1_fix_fld_CMB( 0,1) = one
      fdm1_fix_fld_CMB(-1,2) = - one / dr_n1
      fdm1_fix_fld_CMB( 0,2) =   one / dr_n1
!
      end subroutine cal_fdm1_coef_fix_fld_CMB
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm1_free_rotate_CMB
